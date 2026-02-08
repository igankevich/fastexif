use crate::ENTRY_LEN;
use crate::Endian;
use crate::Format;
use crate::InvalidExif;
use crate::ParseOptions;
use crate::SignedRational;
use crate::ValueRef;
use crate::make_tag_enum_v2;

use bitflags::bitflags;
use serde::Deserialize;
use serde::Serialize;

make_tag_enum_v2! {
    Tag "Apple tags"
    Entry "Apple entry"
    EntryMap "Apple entries"
    ()
    ("Run time" RunTime 3 (RunTime) parse_run_time)
    ("Acceleration" AccelerationVector 8 ([SignedRational; 3]) parse_signed_rational_3)
    ("Camera type" CameraType 46 (CameraType) parse_camera_type)
    //("Semantic style" SemanticStyle 64 (ResolvedValueRef<'a>) parse_property_list)
    //("Tag 78" Tag78 78 (ResolvedValueRef<'a>) parse_property_list)
    //("Tag 79" Tag79 79 (ResolvedValueRef<'a>) parse_property_list)
}

const MAX_RECURSION: u32 = 10;

/// Camera type.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize)]
pub enum CameraType {
    Back,
    Front,
    Other(i32),
}

bitflags! {
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
    pub struct RunTimeFlags: u32 {
        const VALID = 0b1;
        const HAS_BEEN_ROUNDED = 0b10;
        const POSITIVE_INFINITY = 0b100;
        const NEGATIVE_INFINITY = 0b1000;
        const INDEFINITE = 0b10000;
    }
}

impl<'de> Deserialize<'de> for RunTimeFlags {
    fn deserialize<D>(input: D) -> Result<Self, D::Error>
    where
        D: serde::de::Deserializer<'de>,
    {
        struct U32Visitor;
        impl<'de> serde::de::Visitor<'de> for U32Visitor {
            type Value = RunTimeFlags;

            fn expecting(&self, formatter: &mut core::fmt::Formatter) -> core::fmt::Result {
                formatter.write_str("u32")
            }

            fn visit_u32<E>(self, v: u32) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                Ok(RunTimeFlags::from_bits_retain(v))
            }
        }
        input.deserialize_u32(U32Visitor)
    }
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct RunTime {
    pub flags: RunTimeFlags,
    pub value: i64,
    pub timescale: i32,
    pub epoch: i64,
}

/// Iterator over raw entries.
#[derive(Clone)]
pub struct Iter<'a> {
    data: &'a [u8],
    entries: &'a [u8],
    endian: Endian,
}

impl<'a> Iter<'a> {
    pub fn parse(data: &'a [u8]) -> Result<Self, InvalidExif> {
        let buf = &data[12..];
        let Some((b, buf)) = buf.split_at_checked(2) else {
            return Err(InvalidExif);
        };
        let endian = match b {
            [0x49, 0x49] => Endian::Little,
            [0x4d, 0x4d] => Endian::Big,
            _ => return Err(InvalidExif),
        };
        let Some((b, buf)) = buf.split_at_checked(2) else {
            return Err(InvalidExif);
        };
        let num_entries = usize::from(endian.get_u16(b));
        let directory_len = num_entries * ENTRY_LEN + 4;
        let Some(entries) = buf.get(..directory_len) else {
            return Err(InvalidExif);
        };
        let iter = Self {
            data,
            entries,
            endian,
        };
        Ok(iter)
    }

    pub fn into_entry_map(self, options: ParseOptions) -> Result<EntryMap<'a>, InvalidExif> {
        let mut map = EntryMap::new();
        for entry in self {
            let entry = entry?;
            map.insert(entry.parse(options)?);
        }
        Ok(map)
    }
}

impl<'a> Iterator for Iter<'a> {
    type Item = Result<RawEntry<'a>, InvalidExif>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.entries.len() <= 4 {
            return None;
        }
        let tag = self.endian.get_u16(&self.entries[0..2]);
        let format = match Format::try_from_apple(self.endian.get_u16(&self.entries[2..4])) {
            Ok(format) => format,
            Err(e) => return Some(Err(e)),
        };
        let len = self.endian.get_u32(&self.entries[4..8]) as usize;
        let value_slice = &self.entries[8..12];
        let entry = RawEntry(crate::RawEntry {
            tag,
            format,
            len,
            data: self.data,
            value: value_slice,
            endian: self.endian,
        });
        self.entries = &self.entries[ENTRY_LEN..];
        Some(Ok(entry))
    }
}

impl Format {
    fn try_from_apple(other: u16) -> Result<Self, InvalidExif> {
        match other {
            16 => Ok(Self::I32),
            _ => Self::try_from(other),
        }
    }
}

impl<'a> crate::RawEntry<'a> {
    fn parse_camera_type(&self) -> Result<CameraType, InvalidExif> {
        match self.parse_i32()? {
            1 => Ok(CameraType::Back),
            6 => Ok(CameraType::Front),
            other => Ok(CameraType::Other(other)),
        }
    }

    fn parse_run_time(&self) -> Result<RunTime, InvalidExif> {
        let bytes = self.get_bytes().ok_or(InvalidExif)?;
        let run_time: RunTime =
            bplist::from_slice(bytes, MAX_RECURSION).map_err(|_| InvalidExif)?;
        Ok(run_time)
    }
}
