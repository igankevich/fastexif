use crate::Endian;
use crate::InvalidExif;
use crate::ParseOptions;
use crate::UnsignedRational;
use crate::ValueRef;
use crate::exif;
use crate::gps;
use crate::make_tag_enum_v2;
use crate::to_date_time;

use chrono::DateTime;
use chrono::NaiveDateTime;
use chrono::Utc;

use alloc::vec::Vec;

make_tag_enum_v2! {
    Tag "TIFF tags"
    Entry "TIFF entry"
    EntryMap "TIFF entries"
    (Iter)
    ("Image width" ImageWidth 256 (u32) parse_u32)
    ("Image height" ImageLength 257 (u32) parse_u32)
    ("Number of bits per component" BitsPerSample 258 ([u16; 3]) parse_u16_3)
    ("Compression scheme" Compression 259 (Compression) parse_compression)
    ("Pixel composition" PhotometricInterpretation 262 (PhotometricInterpretation) parse_photometric_interpretation)
    ("Description of Image" ImageDescription 270 (&'a str) parse_str)
    ("Manufacturer of image input equipment" Make 271 (&'a str) parse_str)
    ("Model of image input equipment" Model 272 (&'a str) parse_str)
    ("Offset of strip" StripOffsets 273 (Vec<u32>) parse_vec_u32)
    ("Orientation of image" Orientation 274 (Orientation) parse_orientation)
    ("Number of components per pixel" SamplesPerPixel 277 (u16) parse_u16)
    ("Number of rows per strip" RowsPerStrip 278 (u32) parse_u32)
    ("Bytes per compressed strip" StripByteCounts 279 (Vec<u32>) parse_vec_u32)
    ("Number of pixels per resolution unit in width direction" XResolution 282 (UnsignedRational) parse_unsigned_rational)
    ("Number of pixels per resolution unit in height direction" YResolution 283 (UnsignedRational) parse_unsigned_rational)
    ("Image data arrangement" PlanarConfiguration 284 (PlanarConfiguration) parse_planar_configuration)
    ("Unit of X and Y resolution" ResolutionUnit 296 (ResolutionUnit) parse_resolution_unit)
    ("Transfer function" TransferFunction 301 (Vec<u16>) parse_vec_u16)
    ("Software used" Software 305 (&'a str) parse_str)
    ("File change date and time" DateTime 306 (NaiveDateTime) parse_naive_date_time)
    ("Person who created the image" Artist 315 (&'a str) parse_str)
    ("Host computer used to generate the image" HostComputer 316 (&'a str) parse_str)
    ("White point chromaticity" WhitePoint 318 (Chromaticity) parse_chromaticity)
    ("Chromaticities of primaries" PrimaryChromaticities 319 ([Chromaticity; 3]) parse_chromaticity_3)
    ("Offset to JPEG SOI" JpegInterchangeFormat 513 (u32) parse_u32)
    ("Bytes of JPEG data" JpegInterchangeFormatLength 514 (u32) parse_u32)
    ("Color space transformation matrix coefficients" YCbCrCoefficients 529 ([UnsignedRational; 3]) parse_unsigned_rational_3)
    ("Subsampling ratio of Y to C" YCbCrSubSampling 530 (YCbCrSubSampling) parse_y_cb_cr_sub_sampling)
    ("Y and C positioning" YCbCrPositioning 531 (YCbCrPositioning) parse_y_cb_cr_positionting)
    ("Pair of black and white reference values" ReferenceBlackWhite 532 (ReferenceBlackWhite) parse_reference_black_white)
    ("Copyright holder" Copyright 33432 (&'a str) parse_str)
    ("Exif entries" Exif 34665 (exif::EntryMap<'a>) parse_exif 1)
    ("GPS info entries" GpsInfo 34853 (gps::EntryMap<'a>) parse_gps 1)
}

#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
pub struct Chromaticity {
    pub hue: UnsignedRational,
    pub saturation: UnsignedRational,
}

#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
pub struct ReferenceBlackWhite {
    pub black: [UnsignedRational; 3],
    pub white: [UnsignedRational; 3],
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
pub enum PlanarConfiguration {
    Chunky,
    Planar,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
pub enum ResolutionUnit {
    Centimeters,
    Inches,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
pub enum YCbCrPositioning {
    Centered,
    CoSited,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
pub enum Compression {
    Uncompressed,
    Jpeg,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
pub enum PhotometricInterpretation {
    Rgb,
    YCbCr,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
pub enum Orientation {
    /// No rotation, no mirroring.
    Normal,
    /// Mirror horizontally.
    MirrorHorizontally,
    /// Rotate 180 degrees (mirror both horizontally and vertically).
    MirrorBoth,
    /// Mirror vertically.
    MirrorVertically,
    /// Mirror over main diagonal.
    MirrorMainDiagonal,
    /// Rotate 90 degrees clockwise.
    Rotate90Cw,
    /// Mirror over antidiagonal.
    MirrorAntidiagonal,
    /// Rotate 90 degrees counter-clockwise.
    Rotate90Ccw,
}

/// Chromaticity subsampling.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
pub enum YCbCrSubSampling {
    /// Average Cr and Cb over 2 columns.
    YCbCr422,
    /// Average Cr and Cb over 2 rows and over 2 columns.
    YCbCr420,
}

impl<'a> crate::RawEntry<'a> {
    fn parse_chromaticity(&self) -> Result<Chromaticity, InvalidExif> {
        let [hue, saturation] = self.parse_unsigned_rational_2()?;
        Ok(Chromaticity { hue, saturation })
    }

    fn parse_chromaticity_3(&self) -> Result<[Chromaticity; 3], InvalidExif> {
        let [hue0, saturation0, hue1, saturation1, hue2, saturation2] =
            self.parse_unsigned_rational_6()?;
        Ok([
            Chromaticity {
                hue: hue0,
                saturation: saturation0,
            },
            Chromaticity {
                hue: hue1,
                saturation: saturation1,
            },
            Chromaticity {
                hue: hue2,
                saturation: saturation2,
            },
        ])
    }

    fn parse_reference_black_white(&self) -> Result<ReferenceBlackWhite, InvalidExif> {
        let [black0, white0, black1, white1, black2, white2] = self.parse_unsigned_rational_6()?;
        Ok(ReferenceBlackWhite {
            black: [black0, black1, black2],
            white: [white0, white1, white2],
        })
    }

    fn parse_planar_configuration(&self) -> Result<PlanarConfiguration, InvalidExif> {
        match self.parse_u16()? {
            1 => Ok(PlanarConfiguration::Chunky),
            2 => Ok(PlanarConfiguration::Planar),
            _ => Err(InvalidExif),
        }
    }

    fn parse_resolution_unit(&self) -> Result<ResolutionUnit, InvalidExif> {
        match self.parse_u16()? {
            2 => Ok(ResolutionUnit::Inches),
            3 => Ok(ResolutionUnit::Centimeters),
            _ => Err(InvalidExif),
        }
    }

    fn parse_y_cb_cr_positionting(&self) -> Result<YCbCrPositioning, InvalidExif> {
        match self.parse_u16()? {
            1 => Ok(YCbCrPositioning::Centered),
            2 => Ok(YCbCrPositioning::CoSited),
            _ => Err(InvalidExif),
        }
    }

    fn parse_compression(&self) -> Result<Compression, InvalidExif> {
        match self.parse_u16()? {
            1 => Ok(Compression::Uncompressed),
            6 => Ok(Compression::Jpeg),
            _ => Err(InvalidExif),
        }
    }

    fn parse_photometric_interpretation(&self) -> Result<PhotometricInterpretation, InvalidExif> {
        match self.parse_u16()? {
            2 => Ok(PhotometricInterpretation::Rgb),
            6 => Ok(PhotometricInterpretation::YCbCr),
            _ => Err(InvalidExif),
        }
    }

    fn parse_orientation(&self) -> Result<Orientation, InvalidExif> {
        match self.parse_u16()? {
            1 => Ok(Orientation::Normal),
            2 => Ok(Orientation::MirrorHorizontally),
            3 => Ok(Orientation::MirrorBoth),
            4 => Ok(Orientation::MirrorVertically),
            5 => Ok(Orientation::MirrorMainDiagonal),
            6 => Ok(Orientation::Rotate90Cw),
            7 => Ok(Orientation::MirrorAntidiagonal),
            8 => Ok(Orientation::Rotate90Ccw),
            _ => Err(InvalidExif),
        }
    }

    fn parse_y_cb_cr_sub_sampling(&self) -> Result<YCbCrSubSampling, InvalidExif> {
        match self.parse_u16_2()? {
            [2, 1] => Ok(YCbCrSubSampling::YCbCr422),
            [2, 2] => Ok(YCbCrSubSampling::YCbCr420),
            _ => Err(InvalidExif),
        }
    }

    fn parse_exif(&self, options: ParseOptions) -> Result<exif::EntryMap<'a>, InvalidExif> {
        let iter = exif::Iter(crate::Iter {
            data: self.data,
            endian: self.endian,
            entries: self.get_nested_entries()?,
        });
        iter.into_entry_map(options)
    }

    fn parse_gps(&self, options: ParseOptions) -> Result<gps::EntryMap<'a>, InvalidExif> {
        let iter = gps::Iter(crate::Iter {
            data: self.data,
            endian: self.endian,
            entries: self.get_nested_entries()?,
        });
        iter.into_entry_map(options)
    }
}

impl<'a> Iter<'a> {
    pub fn parse(data: &'a [u8]) -> Result<Self, InvalidExif> {
        let data = match data.split_at_checked(10) {
            Some(([_, _, _, _, b'E', b'x', b'i', b'f', 0, 0], rest)) => rest,
            _ => data,
        };
        let (b, buf) = data.split_at_checked(2).ok_or(InvalidExif)?;
        let endian = match b {
            [0x49, 0x49] => Endian::Little,
            [0x4d, 0x4d] => Endian::Big,
            _ => return Err(InvalidExif),
        };
        let (b, buf) = buf.split_at_checked(2).ok_or(InvalidExif)?;
        let forty_two = endian.get_u16(b);
        if forty_two != 42 {
            return Err(InvalidExif);
        }
        let entries = buf.get(..4).ok_or(InvalidExif)?;
        Ok(Self(crate::Iter {
            data,
            entries,
            endian,
        }))
    }
}

impl<'a> EntryMap<'a> {
    /// File change date and time.
    pub fn modified(&self) -> Option<DateTime<Utc>> {
        let date_time = self.get_date_time().copied()?;
        let (subsec, offset) = match self.get_exif() {
            Some(exif) => {
                let subsec = exif.get_sub_sec_time().copied();
                let offset = exif
                    .get_offset_time()
                    .copied()
                    .map(|exif::FixedOffset(offset)| offset);
                (subsec, offset)
            }
            None => (None, None),
        };
        to_date_time(date_time, subsec, offset)
    }
}
