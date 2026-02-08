/// Exif parsing error.
#[derive(Debug)]
pub struct InvalidExif;

impl core::fmt::Display for InvalidExif {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        f.write_str("Invalid Exif data")
    }
}

#[cfg(feature = "std")]
#[cfg_attr(docsrs, doc(cfg(feature = "std")))]
impl std::error::Error for InvalidExif {}
