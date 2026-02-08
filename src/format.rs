use crate::InvalidExif;

/// Exif entry format.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Format {
    /// Unsigned 8-bit integer.
    U8,
    /// NUL-terminated ASCII string.
    Ascii,
    /// Unsigned 16-bit integer.
    U16,
    /// Unsigned 32-bit integer.
    U32,
    /// Unsigned rational (stored as two unsigned 32-bit integers).
    UnsignedRational,
    /// Signed 8-bit integer.
    I8,
    /// Byte array of arbitrary length.
    Undefined,
    /// Signed 16-bit integer.
    I16,
    /// Signed 32-bit integer.
    I32,
    /// Signed rational (stored as two signed 32-bit integers).
    SignedRational,
    /// NUL-terminated UTF-8 string.
    Utf8,
}

impl Format {
    pub fn try_from_u16(other: u16) -> Result<Format, InvalidExif> {
        match other {
            1 => Ok(Self::U8),
            2 => Ok(Self::Ascii),
            3 => Ok(Self::U16),
            4 => Ok(Self::U32),
            5 => Ok(Self::UnsignedRational),
            6 => Ok(Self::I8),
            7 => Ok(Self::Undefined),
            8 => Ok(Self::I16),
            9 => Ok(Self::I32),
            10 => Ok(Self::SignedRational),
            129 => Ok(Self::Utf8),
            _ => Err(InvalidExif),
        }
    }
}
