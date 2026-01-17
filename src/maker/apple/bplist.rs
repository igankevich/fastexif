use crate::InvalidExif;

use alloc::vec::Vec;

use chrono::DateTime;
use chrono::Utc;

// 2001-01-01
const CORE_DATA_EPOCH: u64 = 978303600;

const TRAILER_LEN: usize = 5 + 3 + 8 * 3;

#[derive(Debug)]
struct Trailer {
    #[allow(unused)]
    sort_version: u8,
    offset_int_size: u8,
    object_ref_size: u8,
    num_objects: u64,
    top_object: u64,
    offset_table_offset: u64,
}

#[derive(Debug)]
pub struct PropertyList<'a> {
    data: &'a [u8],
    remaining_objects: usize,
    trailer: Trailer,
}

impl<'a> PropertyList<'a> {
    pub fn parse(data: &'a [u8]) -> Result<Self, InvalidExif> {
        let rest = data.strip_prefix(b"bplist").ok_or(InvalidExif)?;
        let (_version, rest) = rest.split_at_checked(2).ok_or(InvalidExif)?;
        let trailer_offset = rest.len().checked_sub(TRAILER_LEN).ok_or(InvalidExif)?;
        let (rest, b) = rest.split_at_checked(trailer_offset).ok_or(InvalidExif)?;
        let trailer = Trailer {
            sort_version: b[5],
            offset_int_size: b[6],
            object_ref_size: b[7],
            num_objects: u64::from_be_bytes([b[8], b[9], b[10], b[11], b[12], b[13], b[14], b[15]]),
            top_object: u64::from_be_bytes([
                b[16], b[17], b[18], b[19], b[20], b[21], b[22], b[23],
            ]),
            offset_table_offset: u64::from_be_bytes([
                b[24], b[25], b[26], b[27], b[28], b[29], b[30], b[31],
            ]),
        };
        if trailer.num_objects > i64::MAX as u64 {
            return Err(InvalidExif);
        }
        let offset_table_offset: usize = trailer
            .offset_table_offset
            .try_into()
            .map_err(|_| InvalidExif)?;
        let (_rest, offset_table) = rest
            .split_at_checked(offset_table_offset.checked_sub(6 + 2).ok_or(InvalidExif)?)
            .ok_or(InvalidExif)?;
        let top_object_offset_start = trailer
            .top_object
            .checked_mul(trailer.offset_int_size.into())
            .ok_or(InvalidExif)?;
        let top_object_offset_end = top_object_offset_start
            .checked_add(trailer.offset_int_size.into())
            .ok_or(InvalidExif)?;
        let top_object_offset_start = top_object_offset_start
            .try_into()
            .map_err(|_| InvalidExif)?;
        let top_object_offset_end = top_object_offset_end.try_into().map_err(|_| InvalidExif)?;
        let b = offset_table
            .get(top_object_offset_start..top_object_offset_end)
            .ok_or(InvalidExif)?;
        let top_object_offset = get_int(b)?;
        let top_object_offset = top_object_offset.try_into().map_err(|_| InvalidExif)?;
        let data = data
            .get(top_object_offset..offset_table_offset)
            .ok_or(InvalidExif)?;
        let remaining_objects = trailer.num_objects.try_into().map_err(|_| InvalidExif)?;
        Ok(Self {
            data,
            remaining_objects,
            trailer,
        })
    }
}

impl<'a> Iterator for PropertyList<'a> {
    type Item = Result<ValueRef<'a>, InvalidExif>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.remaining_objects == 0 {
            return None;
        }
        let Some((b, rest)) = self.data.split_at_checked(1) else {
            return Some(Err(InvalidExif));
        };
        let marker = b[0];
        match marker {
            0b0000_1000 => {
                self.data = rest;
                self.remaining_objects -= 1;
                Some(Ok(ValueRef::Bool(false)))
            }
            0b0000_1001 => {
                self.data = rest;
                self.remaining_objects -= 1;
                Some(Ok(ValueRef::Bool(true)))
            }
            _ => match (marker & 0b1111_0000_u8, marker & 0b1111_u8) {
                (0b0001_0000, p) => {
                    let num_bytes = 1_usize << usize::from(p);
                    let Some((b, rest)) = rest.split_at_checked(num_bytes) else {
                        return Some(Err(InvalidExif));
                    };
                    let Ok(number) = get_int(b) else {
                        return Some(Err(InvalidExif));
                    };
                    self.data = rest;
                    self.remaining_objects -= 1;
                    Some(Ok(ValueRef::Int(number)))
                }
                (0b0010_0000, p) => {
                    let len = 1_usize << usize::from(p);
                    let Some((b, rest)) = rest.split_at_checked(len) else {
                        return Some(Err(InvalidExif));
                    };
                    let Ok(number) = get_real(b) else {
                        return Some(Err(InvalidExif));
                    };
                    self.data = rest;
                    self.remaining_objects -= 1;
                    Some(Ok(ValueRef::Real(number)))
                }
                (0b1010_0000, len) => {
                    if len == 0b1111 {
                        unimplemented!();
                    }
                    let elem_len = usize::from(self.trailer.object_ref_size);
                    let Some(dict_end) = usize::from(len).checked_mul(elem_len) else {
                        return Some(Err(InvalidExif));
                    };
                    let Some((arr_bytes, rest)) = rest.split_at_checked(dict_end) else {
                        return Some(Err(InvalidExif));
                    };
                    let mut arr = Vec::with_capacity(usize::from(len));
                    for elem_bytes in arr_bytes.chunks_exact(elem_len) {
                        let Ok(objref) = get_int(elem_bytes) else {
                            return Some(Err(InvalidExif));
                        };
                        if objref >= self.trailer.num_objects as i64 {
                            return Some(Err(InvalidExif));
                        }
                        arr.push(objref);
                    }
                    self.data = rest;
                    self.remaining_objects -= 1;
                    Some(Ok(ValueRef::Array(arr)))
                }
                (0b1101_0000, _) => {
                    let count = marker & 0b1111_u8;
                    if count == 0b1111 {
                        unimplemented!();
                    }
                    let elem_len = usize::from(self.trailer.object_ref_size);
                    let Some(dict_end) = usize::from(count)
                        .checked_mul(elem_len)
                        .and_then(|x| x.checked_mul(2))
                    else {
                        return Some(Err(InvalidExif));
                    };
                    let Some((dict_bytes, rest)) = rest.split_at_checked(dict_end) else {
                        return Some(Err(InvalidExif));
                    };
                    let mut dict = Vec::with_capacity(usize::from(count));
                    for i in 0..count {
                        let key_bytes = &dict_bytes[usize::from(i)..];
                        let Ok(keyref) = get_int(&key_bytes[..elem_len]) else {
                            return Some(Err(InvalidExif));
                        };
                        if keyref >= self.trailer.num_objects as i64 {
                            return Some(Err(InvalidExif));
                        }
                        let obj_bytes = &dict_bytes[usize::from(count) + usize::from(i)..];
                        let Ok(objref) = get_int(&obj_bytes[..elem_len]) else {
                            return Some(Err(InvalidExif));
                        };
                        if objref >= self.trailer.num_objects as i64 {
                            return Some(Err(InvalidExif));
                        }
                        dict.push((keyref, objref));
                    }
                    self.data = rest;
                    self.remaining_objects -= 1;
                    Some(Ok(ValueRef::Dictionary(dict)))
                }
                (0b0011_0000, 0b0011) => {
                    let Some((b, rest)) = rest.split_at_checked(8) else {
                        return Some(Err(InvalidExif));
                    };
                    let secs = f64::from_be_bytes([b[0], b[1], b[2], b[3], b[4], b[5], b[6], b[7]]);
                    let Some(timestamp) = CORE_DATA_EPOCH.checked_add((secs * 1e9) as u64) else {
                        return Some(Err(InvalidExif));
                    };
                    let Ok(timestamp) = timestamp.try_into() else {
                        return Some(Err(InvalidExif));
                    };
                    let date = DateTime::from_timestamp_nanos(timestamp);
                    self.data = rest;
                    self.remaining_objects -= 1;
                    Some(Ok(ValueRef::Date(date)))
                }
                (0b0101_0000, len) => {
                    if len == 0b1111 {
                        unimplemented!();
                    }
                    let Some((str_bytes, rest)) = rest.split_at_checked(usize::from(len)) else {
                        return Some(Err(InvalidExif));
                    };
                    let Ok(s) = core::str::from_utf8(str_bytes) else {
                        return Some(Err(InvalidExif));
                    };
                    self.data = rest;
                    self.remaining_objects -= 1;
                    Some(Ok(ValueRef::Str(s)))
                }
                _ => Some(Err(InvalidExif)),
            },
        }
    }
}

fn get_int(bytes: &[u8]) -> Result<i64, InvalidExif> {
    match bytes {
        [b0] => Ok(i64::from(*b0)),
        [b0, b1] => Ok(i64::from(i16::from_be_bytes([*b0, *b1]))),
        [b0, b1, b2, b3] => Ok(i64::from(i32::from_be_bytes([*b0, *b1, *b2, *b3]))),
        [b0, b1, b2, b3, b4, b5, b6, b7] => {
            Ok(i64::from_be_bytes([*b0, *b1, *b2, *b3, *b4, *b5, *b6, *b7]))
        }
        _ => Err(InvalidExif),
    }
}

fn get_real(bytes: &[u8]) -> Result<f64, InvalidExif> {
    match bytes {
        [b0, b1, b2, b3] => Ok(f64::from(f32::from_be_bytes([*b0, *b1, *b2, *b3]))),
        [b0, b1, b2, b3, b4, b5, b6, b7] => {
            Ok(f64::from_be_bytes([*b0, *b1, *b2, *b3, *b4, *b5, *b6, *b7]))
        }
        _ => Err(InvalidExif),
    }
}

#[derive(Debug, Clone)]
pub enum ValueRef<'a> {
    Bool(bool),
    Int(i64),
    Real(f64),
    Date(DateTime<Utc>),
    Str(&'a str),
    Dictionary(Vec<(i64, i64)>),
    Array(Vec<i64>),
}

impl<'a> ValueRef<'a> {
    pub fn resolve(&self, values: &[Self]) -> Result<ResolvedValueRef<'a>, InvalidExif> {
        match self {
            ValueRef::Bool(x) => Ok(ResolvedValueRef::Bool(*x)),
            ValueRef::Int(x) => Ok(ResolvedValueRef::Int(*x)),
            ValueRef::Real(x) => Ok(ResolvedValueRef::Real(*x)),
            ValueRef::Date(x) => Ok(ResolvedValueRef::Date(*x)),
            ValueRef::Str(x) => Ok(ResolvedValueRef::Str(x)),
            ValueRef::Dictionary(dict) => {
                let mut resolved_dict: Vec<(ResolvedValueRef<'a>, ResolvedValueRef<'a>)> =
                    Vec::with_capacity(dict.len());
                for (key_i, value_i) in dict.iter().copied() {
                    let key_i: usize = key_i.try_into().map_err(|_| InvalidExif)?;
                    let value_i: usize = value_i.try_into().map_err(|_| InvalidExif)?;
                    let key = values.get(key_i).ok_or(InvalidExif)?.resolve(values)?;
                    let value = values.get(value_i).ok_or(InvalidExif)?.resolve(values)?;
                    resolved_dict.push((key, value));
                }
                Ok(ResolvedValueRef::Dictionary(resolved_dict))
            }
            ValueRef::Array(arr) => {
                let mut resolved_arr = Vec::with_capacity(arr.len());
                for value_i in arr.iter().copied() {
                    let value_i: usize = value_i.try_into().map_err(|_| InvalidExif)?;
                    let value = values.get(value_i).ok_or(InvalidExif)?.resolve(values)?;
                    resolved_arr.push(value);
                }
                Ok(ResolvedValueRef::Array(resolved_arr))
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ResolvedValueRef<'a> {
    Bool(bool),
    Int(i64),
    Real(f64),
    Date(DateTime<Utc>),
    Str(&'a str),
    Dictionary(Vec<(Self, Self)>),
    Array(Vec<Self>),
}

impl core::fmt::Display for ResolvedValueRef<'_> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Self::Bool(x) => write!(f, "{x}")?,
            Self::Int(x) => write!(f, "{x}")?,
            Self::Real(x) => write!(f, "{x}")?,
            Self::Date(x) => write!(f, "{x}")?,
            Self::Str(x) => f.write_str(x)?,
            Self::Dictionary(x) => {
                f.write_str("[")?;
                let mut iter = x.iter();
                if let Some((key, value)) = iter.next() {
                    write!(f, "({key}, {value})")?;
                }
                for (key, value) in iter {
                    write!(f, "({key}, {value})")?;
                }
                f.write_str("]")?;
            }
            Self::Array(x) => {
                f.write_str("[")?;
                let mut iter = x.iter();
                if let Some(value) = iter.next() {
                    write!(f, "{value}")?;
                }
                for value in iter {
                    write!(f, ", {value}")?;
                }
                f.write_str("]")?;
            }
        }
        Ok(())
    }
}

#[cfg(feature = "serde")]
impl serde::Serialize for ResolvedValueRef<'_> {
    fn serialize<S>(&self, s: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        use alloc::string::ToString;
        use serde::ser::SerializeMap;
        use serde::ser::SerializeSeq;
        match self {
            Self::Bool(x) => s.serialize_bool(*x),
            Self::Int(x) => s.serialize_i64(*x),
            Self::Real(x) => s.serialize_f64(*x),
            Self::Date(x) => s.serialize_str(&x.to_string()),
            Self::Str(x) => s.serialize_str(x),
            Self::Dictionary(x) => {
                let mut map = s.serialize_map(Some(x.len()))?;
                for (key, value) in x {
                    let s = key.to_string();
                    map.serialize_entry(s.as_str(), value)?;
                }
                map.end()
            }
            Self::Array(x) => {
                let mut seq = s.serialize_seq(Some(x.len()))?;
                for value in x {
                    seq.serialize_element(value)?;
                }
                seq.end()
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn works() {
        //let data = [
        //    98, 112, 108, 105, 115, 116, 48, 48, 212, 1, 2, 3, 4, 5, 6, 6, 7, 81, 51, 81, 49, 81,
        //    50, 81, 48, 16, 0, 34, 0, 0, 0, 0, 16, 1, 8, 17, 19, 21, 23, 25, 27, 32, 0, 0, 0, 0, 0,
        //    0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 8, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 34,
        //];
        let data = [
            98, 112, 108, 105, 115, 116, 48, 48, 210, 1, 2, 3, 4, 81, 49, 81, 50, 16, 3, 162, 5,
            10, 210, 6, 7, 8, 9, 83, 50, 46, 49, 83, 50, 46, 50, 35, 64, 84, 91, 157, 0, 0, 0, 0,
            35, 64, 176, 115, 0, 0, 0, 0, 0, 210, 6, 7, 11, 12, 35, 63, 227, 53, 160, 0, 0, 0, 0,
            35, 64, 86, 192, 0, 0, 0, 0, 0, 8, 13, 15, 17, 19, 22, 27, 31, 35, 44, 53, 58, 67, 0,
            0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 13, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 76,
        ];
        //let data = [
        //    98, 112, 108, 105, 115, 116, 48, 48, 16, 0, 8, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0,
        //    0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 10,
        //];
        let plist = PropertyList::parse(&data).unwrap();
        for prop in plist {
            let prop = prop.unwrap();
            eprintln!("{prop:?}");
        }
    }
}
