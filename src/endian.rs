#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Endian {
    Little,
    Big,
}

impl Endian {
    pub fn get_u16(self, b: &[u8]) -> u16 {
        match self {
            Endian::Little => u16::from_le_bytes([b[0], b[1]]),
            Endian::Big => u16::from_be_bytes([b[0], b[1]]),
        }
    }

    pub fn get_u32(self, b: &[u8]) -> u32 {
        match self {
            Endian::Little => u32::from_le_bytes([b[0], b[1], b[2], b[3]]),
            Endian::Big => u32::from_be_bytes([b[0], b[1], b[2], b[3]]),
        }
    }

    pub fn get_i16(self, b: &[u8]) -> i16 {
        match self {
            Endian::Little => i16::from_le_bytes([b[0], b[1]]),
            Endian::Big => i16::from_be_bytes([b[0], b[1]]),
        }
    }
    pub fn get_i32(self, b: &[u8]) -> i32 {
        match self {
            Endian::Little => i32::from_le_bytes([b[0], b[1], b[2], b[3]]),
            Endian::Big => i32::from_be_bytes([b[0], b[1], b[2], b[3]]),
        }
    }
}
