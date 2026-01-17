/// Exif data failed to parse.
#[derive(Debug)]
pub struct InvalidExif;

impl core::fmt::Display for InvalidExif {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        core::fmt::Debug::fmt(self, f)
    }
}

#[cfg(feature = "std")]
impl std::error::Error for InvalidExif {}
