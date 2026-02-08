use chrono::DateTime;
use chrono::FixedOffset;
use chrono::NaiveDateTime;
use chrono::TimeDelta;
use chrono::Utc;

use alloc::vec::Vec;

use crate::Endian;
use crate::Format;
use crate::InvalidExif;
use crate::SignedRational;
use crate::UnsignedRational;

/// Exif tags.
pub mod exif;

/// GPS tags.
pub mod gps;

/// Interoperability tags.
pub mod interop;

/// TIFF tags.
pub mod tiff;

#[derive(Clone)]
pub(crate) struct RawEntry<'a> {
    pub(crate) tag: u16,
    pub(crate) format: Format,
    pub(crate) len: usize,
    pub(crate) data: &'a [u8],
    pub(crate) value: &'a [u8],
    pub(crate) endian: Endian,
}

pub(crate) fn trim_nul_terminator(bytes: &mut &[u8]) {
    while bytes.last() == Some(&0) {
        *bytes = &bytes[..bytes.len() - 1];
    }
}

impl<'a> RawEntry<'a> {
    pub fn parse_value(&self) -> Result<Value<'a>, InvalidExif> {
        Value::parse(self.format, self.len, self.value, self.endian, self.data)
    }

    pub(crate) fn get_bytes(&self) -> Option<&'a [u8]> {
        if self.len <= 4 {
            Some(self.value)
        } else {
            let offset = self.endian.get_u32(self.value) as usize;
            self.data.get(offset..offset + self.len)
        }
    }

    fn parse_unsigned_rational_2(&self) -> Result<[UnsignedRational; 2], InvalidExif> {
        if self.format != Format::UnsignedRational {
            return Err(InvalidExif);
        }
        if self.len != 2 {
            return Err(InvalidExif);
        }
        let offset = self.endian.get_u32(self.value) as usize;
        let buf = self
            .data
            .get(offset..offset + self.len * 8)
            .ok_or(InvalidExif)?;
        Ok([
            UnsignedRational(
                self.endian.get_u32(&buf[0..4]),
                self.endian.get_u32(&buf[4..8]),
            ),
            UnsignedRational(
                self.endian.get_u32(&buf[8..12]),
                self.endian.get_u32(&buf[12..16]),
            ),
        ])
    }

    fn parse_unsigned_rational_3(&self) -> Result<[UnsignedRational; 3], InvalidExif> {
        if self.format != Format::UnsignedRational {
            return Err(InvalidExif);
        }
        if self.len != 3 {
            return Err(InvalidExif);
        }
        let offset = self.endian.get_u32(self.value) as usize;
        let buf = self
            .data
            .get(offset..offset + self.len * 8)
            .ok_or(InvalidExif)?;
        Ok([
            UnsignedRational(
                self.endian.get_u32(&buf[0..4]),
                self.endian.get_u32(&buf[4..8]),
            ),
            UnsignedRational(
                self.endian.get_u32(&buf[8..12]),
                self.endian.get_u32(&buf[12..16]),
            ),
            UnsignedRational(
                self.endian.get_u32(&buf[16..20]),
                self.endian.get_u32(&buf[20..24]),
            ),
        ])
    }

    fn parse_unsigned_rational_4(&self) -> Result<[UnsignedRational; 4], InvalidExif> {
        if self.format != Format::UnsignedRational {
            return Err(InvalidExif);
        }
        if self.len != 4 {
            return Err(InvalidExif);
        }
        let offset = self.endian.get_u32(self.value) as usize;
        let buf = self
            .data
            .get(offset..offset + self.len * 8)
            .ok_or(InvalidExif)?;
        Ok([
            UnsignedRational(
                self.endian.get_u32(&buf[0..4]),
                self.endian.get_u32(&buf[4..8]),
            ),
            UnsignedRational(
                self.endian.get_u32(&buf[8..12]),
                self.endian.get_u32(&buf[12..16]),
            ),
            UnsignedRational(
                self.endian.get_u32(&buf[16..20]),
                self.endian.get_u32(&buf[20..24]),
            ),
            UnsignedRational(
                self.endian.get_u32(&buf[24..28]),
                self.endian.get_u32(&buf[28..32]),
            ),
        ])
    }

    fn parse_unsigned_rational_6(&self) -> Result<[UnsignedRational; 6], InvalidExif> {
        if self.len != 6 {
            return Err(InvalidExif);
        }
        let offset = self.endian.get_u32(self.value) as usize;
        let buf = self
            .data
            .get(offset..offset + self.len * 8)
            .ok_or(InvalidExif)?;
        Ok([
            UnsignedRational(
                self.endian.get_u32(&buf[0..4]),
                self.endian.get_u32(&buf[4..8]),
            ),
            UnsignedRational(
                self.endian.get_u32(&buf[8..12]),
                self.endian.get_u32(&buf[12..16]),
            ),
            UnsignedRational(
                self.endian.get_u32(&buf[16..20]),
                self.endian.get_u32(&buf[20..24]),
            ),
            UnsignedRational(
                self.endian.get_u32(&buf[24..28]),
                self.endian.get_u32(&buf[28..32]),
            ),
            UnsignedRational(
                self.endian.get_u32(&buf[32..36]),
                self.endian.get_u32(&buf[36..40]),
            ),
            UnsignedRational(
                self.endian.get_u32(&buf[40..44]),
                self.endian.get_u32(&buf[44..48]),
            ),
        ])
    }

    fn parse_unsigned_rational(&self) -> Result<UnsignedRational, InvalidExif> {
        let Value::UnsignedRational(value) = self.parse_value()? else {
            return Err(InvalidExif);
        };
        Ok(value)
    }

    fn parse_signed_rational(&self) -> Result<SignedRational, InvalidExif> {
        let Value::SignedRational(value) = self.parse_value()? else {
            return Err(InvalidExif);
        };
        Ok(value)
    }

    pub(crate) fn parse_signed_rational_3(&self) -> Result<[SignedRational; 3], InvalidExif> {
        if self.format != Format::SignedRational {
            return Err(InvalidExif);
        }
        if self.len != 3 {
            return Err(InvalidExif);
        }
        let offset = self.endian.get_u32(self.value) as usize;
        let buf = self
            .data
            .get(offset..offset + self.len * 8)
            .ok_or(InvalidExif)?;
        Ok([
            SignedRational(
                self.endian.get_i32(&buf[0..4]),
                self.endian.get_i32(&buf[4..8]),
            ),
            SignedRational(
                self.endian.get_i32(&buf[8..12]),
                self.endian.get_i32(&buf[12..16]),
            ),
            SignedRational(
                self.endian.get_i32(&buf[16..20]),
                self.endian.get_i32(&buf[20..24]),
            ),
        ])
    }

    fn parse_u8(&self) -> Result<u8, InvalidExif> {
        self.parse_value()?.parse_u8()
    }

    fn parse_u16(&self) -> Result<u16, InvalidExif> {
        self.parse_value()?.parse_u16()
    }

    fn parse_u32(&self) -> Result<u32, InvalidExif> {
        self.parse_value()?.parse_u32()
    }

    pub(crate) fn parse_i32(&self) -> Result<i32, InvalidExif> {
        self.parse_value()?.parse_i32()
    }

    fn parse_u16_2(&self) -> Result<[u16; 2], InvalidExif> {
        if self.format != Format::U16 {
            return Err(InvalidExif);
        }
        if self.len != 2 {
            return Err(InvalidExif);
        }
        let buf = self.value;
        Ok([
            self.endian.get_u16(&buf[..2]),
            self.endian.get_u16(&buf[2..]),
        ])
    }

    fn parse_u16_3(&self) -> Result<[u16; 3], InvalidExif> {
        if self.format != Format::U16 {
            return Err(InvalidExif);
        }
        if self.len != 3 {
            return Err(InvalidExif);
        }
        let offset = self.endian.get_u32(self.value) as usize;
        let buf = self
            .data
            .get(offset..offset + self.len * 2)
            .ok_or(InvalidExif)?;
        Ok([
            self.endian.get_u16(&buf[0..2]),
            self.endian.get_u16(&buf[2..4]),
            self.endian.get_u16(&buf[4..6]),
        ])
    }

    fn parse_u16_4(&self) -> Result<[u16; 4], InvalidExif> {
        if self.format != Format::U16 {
            return Err(InvalidExif);
        }
        if self.len != 4 {
            return Err(InvalidExif);
        }
        let offset = self.endian.get_u32(self.value) as usize;
        let buf = self
            .data
            .get(offset..offset + self.len * 2)
            .ok_or(InvalidExif)?;
        Ok([
            self.endian.get_u16(&buf[0..2]),
            self.endian.get_u16(&buf[2..4]),
            self.endian.get_u16(&buf[4..6]),
            self.endian.get_u16(&buf[6..8]),
        ])
    }

    fn parse_vec_u16(&self) -> Result<Vec<u16>, InvalidExif> {
        let Value::Vec(values) = self.parse_value()? else {
            return Err(InvalidExif);
        };
        values.into_iter().map(|x| x.parse_u16()).collect()
    }

    fn parse_vec_u32(&self) -> Result<Vec<u32>, InvalidExif> {
        let Value::Vec(values) = self.parse_value()? else {
            return Err(InvalidExif);
        };
        values.into_iter().map(|x| x.parse_u32()).collect()
    }

    fn parse_bytes(&self) -> Result<&'a [u8], InvalidExif> {
        self.get_bytes().ok_or(InvalidExif)
    }

    fn parse_naive_date_time(&self) -> Result<NaiveDateTime, InvalidExif> {
        let Value::Str(string) = self.parse_value()? else {
            return Err(InvalidExif);
        };
        let t =
            NaiveDateTime::parse_from_str(string, "%Y:%m:%d %H:%M:%S").map_err(|_| InvalidExif)?;
        Ok(t)
    }

    fn parse_str(&self) -> Result<&'a str, InvalidExif> {
        match self.parse_value()? {
            Value::Str(s) => Ok(s),
            _ => Err(InvalidExif),
        }
    }

    fn parse_undefined_as_str(&self) -> Result<&'a str, InvalidExif> {
        match self.parse_value()? {
            Value::Bytes(s) => core::str::from_utf8(s).map_err(|_| InvalidExif),
            _ => Err(InvalidExif),
        }
    }

    pub(crate) fn get_nested_entries(&self) -> Result<&'a [u8], InvalidExif> {
        let offset = self.endian.get_u32(self.value);
        let buf = self.data.get(offset as usize..).ok_or(InvalidExif)?;
        let (b, buf) = buf.split_at_checked(2).ok_or(InvalidExif)?;
        let num_entries = usize::from(self.endian.get_u16(b));
        let directory_len = num_entries * ENTRY_LEN + 4;
        let entries = buf.get(..directory_len).ok_or(InvalidExif)?;
        Ok(entries)
    }
}

impl core::fmt::Debug for RawEntry<'_> {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        let value = self.parse_value();
        f.debug_struct("RawEntry")
            .field("tag", &self.tag)
            .field("value", &value)
            .finish()
    }
}

/// A enumeration of all possible value types and Exif tag can have.
///
/// NOTE: It is more efficient to parse raw entries into `Entry` types
/// rather than into generic `Value` type.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Value<'a> {
    /// Unsigned 8-bit integer.
    U8(u8),
    /// Signed 8-bit integer.
    I8(i8),
    /// Unsigned 16-bit integer.
    U16(u16),
    /// Signed 16-bit integer.
    I16(i16),
    /// Unsigned 32-bit integer.
    U32(u32),
    /// Signed 32-bit integer.
    I32(i32),
    /// String.
    Str(&'a str),
    /// Bytes.
    Bytes(&'a [u8]),
    /// Unsigned rational number.
    UnsignedRational(UnsignedRational),
    /// Signed rational number.
    SignedRational(SignedRational),
    /// A list of values.
    Vec(Vec<Self>),
}

impl<'a> Value<'a> {
    pub(crate) fn parse(
        format: Format,
        len: usize,
        value: &'a [u8],
        endian: Endian,
        data: &'a [u8],
    ) -> Result<Self, InvalidExif> {
        fn get_bytes<'a>(
            len: usize,
            value: &'a [u8],
            endian: Endian,
            data: &'a [u8],
        ) -> Option<&'a [u8]> {
            if len <= 4 {
                Some(&value[..len])
            } else {
                let offset = endian.get_u32(value) as usize;
                data.get(offset..offset + len)
            }
        }

        match format {
            Format::Ascii | Format::Utf8 => {
                let mut bytes = get_bytes(len, value, endian, data).ok_or(InvalidExif)?;
                trim_nul_terminator(&mut bytes);
                let string = core::str::from_utf8(bytes).map_err(|_| InvalidExif)?;
                Ok(Self::Str(string))
            }
            Format::Undefined => {
                let bytes = get_bytes(len, value, endian, data).ok_or(InvalidExif)?;
                match bytes {
                    [b'A', b'S', b'C', b'I', b'I', 0, 0, 0, ..]
                    | [b'U', b'N', b'I', b'C', b'O', b'D', b'E', 0, ..] => {
                        let mut bytes = &bytes[8..];
                        trim_nul_terminator(&mut bytes);
                        let string = core::str::from_utf8(bytes).map_err(|_| InvalidExif)?;
                        Ok(Self::Str(string))
                    }
                    _ => Ok(Self::Bytes(bytes)),
                }
            }
            Format::U8
            | Format::I8
            | Format::U16
            | Format::I16
            | Format::U32
            | Format::I32
            | Format::SignedRational
            | Format::UnsignedRational => {
                let elem_len = match format {
                    Format::U8 => 1,
                    Format::I8 => 1,
                    Format::U16 => 2,
                    Format::I16 => 2,
                    Format::U32 => 4,
                    Format::I32 => 4,
                    Format::SignedRational => 8,
                    Format::UnsignedRational => 8,
                    Format::Ascii | Format::Utf8 | Format::Undefined => unreachable!(),
                };
                let bytes = get_bytes(len * elem_len, value, endian, data).ok_or(InvalidExif)?;
                if bytes.len() == elem_len {
                    Self::parse_single(format, endian, bytes)
                } else {
                    let mut values = Vec::with_capacity(len);
                    let mut slice = bytes;
                    for _ in 0..len {
                        let (buf, rest) = slice.split_at(elem_len);
                        slice = rest;
                        values.push(Self::parse_single(format, endian, buf)?);
                    }
                    Ok(Self::Vec(values))
                }
            }
        }
    }

    fn parse_single(format: Format, endian: Endian, bytes: &[u8]) -> Result<Self, InvalidExif> {
        match format {
            Format::U8 => Ok(Self::U8(bytes[0])),
            Format::I8 => Ok(Self::I8(bytes[0] as i8)),
            Format::U16 => Ok(Self::U16(endian.get_u16(bytes))),
            Format::I16 => Ok(Self::I16(endian.get_i16(bytes))),
            Format::U32 => Ok(Self::U32(endian.get_u32(bytes))),
            Format::I32 => Ok(Self::I32(endian.get_i32(bytes))),
            Format::SignedRational => {
                let numerator = endian.get_i32(&bytes[..4]);
                let denominator = endian.get_i32(&bytes[4..]);
                Ok(Self::SignedRational(SignedRational(numerator, denominator)))
            }
            Format::UnsignedRational => {
                let numerator = endian.get_u32(&bytes[..4]);
                let denominator = endian.get_u32(&bytes[4..]);
                Ok(Self::UnsignedRational(UnsignedRational(
                    numerator,
                    denominator,
                )))
            }
            Format::Ascii | Format::Undefined | Format::Utf8 => unreachable!(),
        }
    }
}

impl Value<'_> {
    fn parse_u8(self) -> Result<u8, InvalidExif> {
        match self {
            Self::U8(x) => Ok(x),
            Self::I8(x) => x.try_into().map_err(|_| InvalidExif),
            Self::U16(x) => x.try_into().map_err(|_| InvalidExif),
            Self::I16(x) => x.try_into().map_err(|_| InvalidExif),
            Self::U32(x) => x.try_into().map_err(|_| InvalidExif),
            Self::I32(x) => x.try_into().map_err(|_| InvalidExif),
            Self::Str(x) => x.parse().map_err(|_| InvalidExif),
            Self::Bytes(..)
            | Self::UnsignedRational(..)
            | Self::SignedRational(..)
            | Self::Vec(..) => Err(InvalidExif),
        }
    }

    fn parse_u16(self) -> Result<u16, InvalidExif> {
        match self {
            Self::U8(x) => Ok(x.into()),
            Self::I8(x) => x.try_into().map_err(|_| InvalidExif),
            Self::U16(x) => Ok(x),
            Self::I16(x) => x.try_into().map_err(|_| InvalidExif),
            Self::U32(x) => x.try_into().map_err(|_| InvalidExif),
            Self::I32(x) => x.try_into().map_err(|_| InvalidExif),
            Self::Str(x) => x.parse().map_err(|_| InvalidExif),
            Self::Bytes(..)
            | Self::UnsignedRational(..)
            | Self::SignedRational(..)
            | Self::Vec(..) => Err(InvalidExif),
        }
    }

    fn parse_u32(self) -> Result<u32, InvalidExif> {
        match self {
            Self::U8(x) => Ok(x.into()),
            Self::I8(x) => x.try_into().map_err(|_| InvalidExif),
            Self::U16(x) => Ok(x.into()),
            Self::I16(x) => x.try_into().map_err(|_| InvalidExif),
            Self::U32(x) => Ok(x),
            Self::I32(x) => x.try_into().map_err(|_| InvalidExif),
            Self::Str(x) => x.parse().map_err(|_| InvalidExif),
            Self::Bytes(..)
            | Self::UnsignedRational(..)
            | Self::SignedRational(..)
            | Self::Vec(..) => Err(InvalidExif),
        }
    }

    fn parse_i32(self) -> Result<i32, InvalidExif> {
        match self {
            Self::U8(x) => Ok(x.into()),
            Self::I8(x) => Ok(x.into()),
            Self::U16(x) => Ok(x.into()),
            Self::I16(x) => Ok(x.into()),
            Self::U32(x) => x.try_into().map_err(|_| InvalidExif),
            Self::I32(x) => Ok(x),
            Self::Str(x) => x.parse().map_err(|_| InvalidExif),
            Self::Bytes(..)
            | Self::UnsignedRational(..)
            | Self::SignedRational(..)
            | Self::Vec(..) => Err(InvalidExif),
        }
    }
}

#[cfg(feature = "serde")]
#[cfg_attr(docsrs, doc(cfg(feature = "serde")))]
impl serde::Serialize for Value<'_> {
    fn serialize<S>(&self, s: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        use serde::ser::SerializeSeq;
        use serde::ser::SerializeTuple;
        match self {
            Self::U8(x) => s.serialize_u8(*x),
            Self::I8(x) => s.serialize_i8(*x),
            Self::U16(x) => s.serialize_u16(*x),
            Self::I16(x) => s.serialize_i16(*x),
            Self::U32(x) => s.serialize_u32(*x),
            Self::I32(x) => s.serialize_i32(*x),
            Self::Str(x) => s.serialize_str(x),
            Self::Bytes(x) => s.serialize_bytes(x),
            Self::UnsignedRational(x) => {
                let mut t = s.serialize_tuple(2)?;
                t.serialize_element(&x.0)?;
                t.serialize_element(&x.1)?;
                t.end()
            }
            Self::SignedRational(x) => {
                let mut t = s.serialize_tuple(2)?;
                t.serialize_element(&x.0)?;
                t.serialize_element(&x.1)?;
                t.end()
            }
            Self::Vec(x) => {
                let mut seq = s.serialize_seq(Some(x.len()))?;
                for elem in x {
                    seq.serialize_element(elem)?;
                }
                seq.end()
            }
        }
    }
}

#[derive(Clone)]
pub(crate) struct Iter<'a> {
    data: &'a [u8],
    entries: &'a [u8],
    endian: Endian,
}

impl<'a> Iter<'a> {
    pub(crate) fn next(&mut self) -> Option<Result<RawEntry<'a>, InvalidExif>> {
        while self.entries.len() <= 4 {
            // Read next directory.
            let Some(buf) = self.entries.get(..4) else {
                return Some(Err(InvalidExif));
            };
            let offset = self.endian.get_u32(buf) as usize;
            let offset: usize = offset;
            if offset == 0 {
                return None;
            }
            // TODO we need to check that we haven't read the file at this offset already to
            // eliminate loops
            let Some(buf) = self.data.get(offset..) else {
                return Some(Err(InvalidExif));
            };
            let Some((b, buf)) = buf.split_at_checked(2) else {
                return Some(Err(InvalidExif));
            };
            let num_entries = usize::from(self.endian.get_u16(b));
            let directory_len = num_entries * ENTRY_LEN + 4;
            let Some(entries) = buf.get(..directory_len) else {
                return Some(Err(InvalidExif));
            };
            self.entries = entries;
        }
        let tag = self.endian.get_u16(&self.entries[0..2]);
        let format = match Format::try_from_u16(self.endian.get_u16(&self.entries[2..4])) {
            Ok(format) => format,
            Err(e) => return Some(Err(e)),
        };
        let len = self.endian.get_u32(&self.entries[4..8]) as usize;
        let value_slice = &self.entries[8..12];
        let entry = RawEntry {
            tag,
            format,
            len,
            data: self.data,
            value: value_slice,
            endian: self.endian,
        };
        self.entries = &self.entries[ENTRY_LEN..];
        Some(Ok(entry))
    }
}

pub(crate) fn to_date_time(
    date_time: NaiveDateTime,
    subsec: Option<TimeDelta>,
    offset: Option<FixedOffset>,
) -> Option<DateTime<Utc>> {
    match (subsec, offset) {
        (Some(subsec), Some(offset)) => date_time
            .checked_add_signed(subsec)
            .and_then(|t| t.checked_sub_offset(offset))
            .map(|t| t.and_utc()),
        (Some(subsec), None) => date_time.checked_add_signed(subsec).map(|t| t.and_utc()),
        (None, Some(offset)) => date_time.checked_sub_offset(offset).map(|t| t.and_utc()),
        (None, None) => Some(date_time.and_utc()),
    }
}

pub(crate) const ENTRY_LEN: usize = 12;

/// Parse Exif metadata from the provided byte slice.
///
/// Returns an iterator over raw entries.
#[inline]
pub fn parse(data: &[u8]) -> Result<tiff::Iter<'_>, InvalidExif> {
    tiff::Iter::parse(data)
}

macro_rules! define_tag_enum {
    (
        $enum: ident $enum_doc: literal
        $entry_enum: ident $entry_enum_doc: literal
        $entry_map: ident $entry_map_doc: literal
        ($($iter: ident)*)
        $(($doc: literal $name: ident $value: literal ($type: ty) $parse: ident $($options: literal)*))*
    ) => {
        #[doc = $enum_doc]
        #[repr(u16)]
        #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
        #[cfg_attr(feature = "serde", derive(serde::Serialize))]
        pub enum $enum {
            $(#[doc = $doc] $name,)*
            /// Unknown tag.
            Other(u16) = u16::MAX,
        }

        impl From<u16> for $enum {
            fn from(other: u16) -> $enum {
                match other {
                    $($value => $enum::$name,)*
                    other => $enum::Other(other),
                }
            }
        }

        paste::paste! {
            impl $enum {
                $(#[doc = $doc] pub const [<$name:snake:upper>]: u16 = $value;)*
            }
        }

        impl core::fmt::Display for $enum {
            fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
                let result = match self {
                    $($enum::$name => Ok(stringify!($name)),)*
                    Self::Other(x) => Err(x),
                };
                match result {
                    Ok(string) => f.write_str(string),
                    Err(number) => write!(f, "{number}"),
                }
            }
        }

        #[doc = $entry_enum_doc]
        #[derive(Debug)]
        #[repr(u16)]
        #[cfg_attr(feature = "serde", derive(serde::Serialize))]
        pub enum $entry_enum<'a> {
            $(#[doc = $doc] $name($type) = $value,)*
            /// Unknown entry.
            Other {
                /// Tag number.
                tag: u16,
                /// Tag value.
                value: Value<'a>
            } = u16::MAX,
        }

        impl $entry_enum<'_> {
            fn tag(&self) -> u16 {
                match self {
                    Self::Other { tag, .. } => *tag,
                    _ => unsafe { *<*const _>::from(self).cast::<u16>() },
                }
            }
        }

        #[doc = $entry_map_doc]
        pub struct $entry_map<'a> {
            entries: alloc::vec::Vec<$entry_enum<'a>>,
        }

        impl<'a> $entry_map<'a> {
            pub(crate) const fn new() -> Self {
                Self {
                    entries: alloc::vec::Vec::new(),
                }
            }

            pub(crate) fn insert(&mut self, entry: $entry_enum<'a>) {
                match self.entries.binary_search_by(|x| x.tag().cmp(&entry.tag())) {
                    Ok(i) => self.entries[i] = entry,
                    Err(i) => self.entries.insert(i, entry),
                }
            }

            pub(crate) fn get(&self, tag: u16) -> Option<&$entry_enum<'a>> {
                match self.entries.binary_search_by(|x| x.tag().cmp(&tag)) {
                    Ok(i) => Some(&self.entries[i]),
                    Err(_i) => None,
                }
            }

            paste::paste! {
                $(
                    #[doc = $doc]
                    pub fn [<get_ $name:snake>](&self) -> Option<&$type> {
                        self.get($enum::[<$name:snake:upper>]).and_then(|entry| match entry {
                            $entry_enum::$name(value) => Some(value),
                            _ => None,
                        })
                    }
                )*
            }
        }

        impl core::fmt::Debug for $entry_map<'_> {
            fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
                self.entries.fmt(f)
            }
        }

        impl<'a> core::iter::Extend<$entry_enum<'a>> for $entry_map<'a> {
            fn extend<I: IntoIterator<Item = $entry_enum<'a>>>(&mut self, iter: I) {
                for entry in iter {
                    self.insert(entry);
                }
            }
        }

        impl<'a> core::iter::FromIterator<$entry_enum<'a>> for $entry_map<'a> {
            fn from_iter<I: IntoIterator<Item = $entry_enum<'a>>>(iter: I) -> Self {
                let mut map = $entry_map::new();
                map.extend(iter);
                map
            }
        }

        #[cfg(feature = "serde")]
        #[cfg_attr(docsrs, doc(cfg(feature = "serde")))]
        impl serde::Serialize for $entry_map<'_> {
            fn serialize<S>(&self, s: S) -> Result<S::Ok, S::Error>
            where
                S: serde::Serializer,
            {
                use serde::ser::SerializeMap;
                use alloc::string::ToString;
                let mut map = s.serialize_map(Some(self.entries.len()))?;
                for entry in self.entries.iter() {
                    match entry {
                        $(
                            $entry_enum::$name(value) => map.serialize_entry(stringify!($name), value)?,
                        )*
                        $entry_enum::Other { tag, value } => {
                            let tag = $enum::from(*tag).to_string();
                            map.serialize_entry(tag.as_str(), value)?
                        }
                    }
                }
                map.end()
            }
        }

        /// Unparsed entry.
        #[derive(Debug, Clone)]
        pub struct RawEntry<'a>(crate::RawEntry<'a>);

        paste::paste! {
            impl<'a> RawEntry<'a> {
                /// Parse entry tag and entry value.
                ///
                /// This is more efficient than [`parse_value`](Self::parse_value).
                pub fn parse(&self, #[allow(unused)] options: $crate::ParseOptions) -> Result<Entry<'a>, InvalidExif> {
                    #[allow(unused)]
                    fn gobble(_a: u8, options: $crate::ParseOptions) -> $crate::ParseOptions {
                        options
                    }

                    match self.0.tag {
                        $(
                            $enum::[<$name:snake:upper>] =>
                                Ok($entry_enum::$name(self.0.$parse($(gobble($options, options))*)?)),
                        )*
                        _ => {
                            let value = self.0.parse_value()?;
                            let tag = self.0.tag;
                            Ok($entry_enum::Other { tag, value })
                        }
                    }
                }

                /// Parse entry tag.
                pub fn parse_tag(&self) -> $enum {
                    $enum::from(self.0.tag)
                }

                /// Parse entry value.
                ///
                /// This is less efficient than [`parse`](Self::parse_value) because of the generic [`Value`] type.
                pub fn parse_value(&self) -> Result<Value<'a>, InvalidExif> {
                    self.0.parse_value()
                }
            }
        }

        $(
            /// Iterator over raw entries.
            #[derive(Clone)]
            pub struct $iter<'a>(pub(crate) crate::Iter<'a>);

            impl<'a> $iter<'a> {
                /// Parse remaining entries and collect results into an entry map.
                ///
                /// Entry map allows one to access entries via convenience wrapper methods rather than sequentially.
                pub fn into_entry_map(self, options: crate::ParseOptions) -> Result<EntryMap<'a>, InvalidExif> {
                    let mut map = EntryMap::new();
                    for raw_entry in self {
                        let raw_entry = raw_entry?;
                        let entry = match raw_entry.parse(options) {
                            Ok(entry) => entry,
                            Err(_) if options.ignore_unparsable_nested_entries => {
                                log::trace!(
                                    "Failed to parse Exif entry: tag={}, format={:?}, count={}, value={:?}",
                                    raw_entry.0.tag,
                                    raw_entry.0.format,
                                    raw_entry.0.len,
                                    raw_entry.0.value,
                                );
                                continue;
                            }
                            Err(e) => return Err(e),
                        };
                        map.insert(entry);
                    }
                    return Ok(map);
                }
            }

            impl<'a> Iterator for $iter<'a> {
                type Item = Result<RawEntry<'a>, InvalidExif>;

                fn next(&mut self) -> Option<Self::Item> {
                    match self.0.next() {
                        Some(Ok(x)) => Some(Ok(RawEntry(x))),
                        Some(Err(e)) => Some(Err(e)),
                        None => None,
                    }
                }
            }
        )*
    };
}

pub(crate) use define_tag_enum;

macro_rules! define_value_enums {
    ($(($enum: ident
     $repr: ident
     $doc: literal
     $(($variant: ident $value: literal $variant_doc: literal))+))+) => {
        $(
            #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
            #[cfg_attr(feature = "serde", derive(serde::Serialize))]
            #[doc = $doc]
            pub enum $enum {
                $( #[doc = $variant_doc] $variant, )+
            }

            paste::paste! {
                impl<'a> $crate::RawEntry<'a> {
                    fn [<parse_ $enum:snake>](&self) -> Result<$enum, InvalidExif> {
                        match self.[<parse_ $repr>]()? {
                            $( $value => Ok($enum::$variant), )+
                            _ => Err(InvalidExif),
                        }
                    }
                }
            }
        )+
    };
}

pub(crate) use define_value_enums;

macro_rules! define_value_str_enums {
    ($(($enum: ident
        $doc: literal
     $(($variant: ident ($($value: literal)+) $variant_doc: literal))+))+) => {
        $(
            #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
            #[cfg_attr(feature = "serde", derive(serde::Serialize))]
            #[doc = $doc]
            pub enum $enum {
                $( #[doc = $variant_doc] $variant, )+
            }

            paste::paste! {
                impl<'a> $crate::RawEntry<'a> {
                    fn [<parse_ $enum:snake>](&self) -> Result<$enum, InvalidExif> {
                        let bytes = self.get_bytes().ok_or(InvalidExif)?;
                        match bytes {
                            $( [$($value,)+ 0, ..] => Ok($enum::$variant), )+
                            _ => Err(InvalidExif),
                        }
                    }
                }
            }
        )+
    };
}

pub(crate) use define_value_str_enums;
