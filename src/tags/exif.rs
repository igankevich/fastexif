use crate::InvalidExif;
use crate::ParseOptions;
use crate::SignedRational;
use crate::UnsignedRational;
use crate::ValueRef;
use crate::apple;
use crate::interop;
use crate::make_tag_enum_v2;
use crate::to_date_time;

use chrono::DateTime;
use chrono::NaiveDateTime;
use chrono::TimeDelta;
use chrono::Utc;

make_tag_enum_v2! {
    Tag "EXIF tags"
    Entry "EXIF entry"
    EntryMap "EXIF entries"
    (Iter)
    ("Exposure time" ExposureTime 33434 (UnsignedRational) parse_unsigned_rational)
    ("F number" FNumber 33437 (UnsignedRational) parse_unsigned_rational)
    ("Exposure program" ExposureProgram 34850 (u16) parse_u16)
    ("Spectral sensitivity" SpectralSensitivity 34852 (&'a str) parse_str)
    ("Photographic Sensitivity" PhotographicSensitivity 34855 (u16) parse_u16)
    ("Optoelectric coefficient" Oecf 34856 (&'a [u8]) parse_bytes)
    ("Sensitivity Type" SensitivityType 34864 (u16) parse_u16)
    ("Standard Output Sensitivity" StandardOutputSensitivity 34865 (u32) parse_u32)
    ("Recommended Exposure Index" RecommendedExposureIndex 34866 (u32) parse_u32)
    ("ISOSpeed" IsoSpeed 34867 (u32) parse_u32)
    ("ISOSpeed Latitude yyy" IsoSpeedLatitudeyyy 34868 (u32) parse_u32)
    ("ISOSpeed Latitude zzz" IsoSpeedLatitudezzz 34869 (u32) parse_u32)
    ("Exif Version" ExifVersion 36864 (&'a str) parse_undefined_as_str)
    ("Date and time original image was generated" DateTimeOriginal 36867 (NaiveDateTime) parse_naive_date_time)
    ("Date and time image was made digital data" DateTimeDigitized 36868 (NaiveDateTime) parse_naive_date_time)
    ("Offset data of DateTime" OffsetTime 36880 (FixedOffset) parse_time_offset)
    ("Offset data of DateTimeOriginal" OffsetTimeOriginal 36881 (FixedOffset) parse_time_offset)
    ("Offset data of DateTimeDigitized" OffsetTimeDigitized 36882 (FixedOffset) parse_time_offset)
    ("Meaning of each component" ComponentsConfiguration 37121 (&'a [u8]) parse_bytes)
    ("Image compression mode" CompressedBitsPerPixel 37122 (UnsignedRational) parse_unsigned_rational)
    ("Shutter speed" ShutterSpeedValue 37377 (SignedRational) parse_signed_rational)
    ("Aperture" ApertureValue 37378 (UnsignedRational) parse_unsigned_rational)
    ("Brightness" BrightnessValue 37379 (SignedRational) parse_signed_rational)
    ("Exposure bias" ExposureBiasValue 37380 (SignedRational) parse_signed_rational)
    ("Maximum lens aperture" MaxApertureValue 37381 (UnsignedRational) parse_unsigned_rational)
    ("Subject distance" SubjectDistance 37382 (UnsignedRational) parse_unsigned_rational)
    ("Metering mode" MeteringMode 37383 (u16) parse_u16)
    ("Light source" LightSource 37384 (u16) parse_u16)
    ("Flash" Flash 37385 (u16) parse_u16)
    ("Lens focal length" FocalLength 37386 (UnsignedRational) parse_unsigned_rational)
    ("Subject area" SubjectArea 37396 (SubjectArea) parse_subject_area)
    ("Manufacturer notes (vendor-specific entries)" MakerNote 37500 (MakerNote<'a>) parse_maker_note 1)
    ("User comments" UserComment 37510 (&'a [u8]) parse_bytes)
    ("DateTime sub-seconds" SubSecTime 37520 (TimeDelta) parse_subsec_time)
    ("DateTimeOriginal sub-seconds" SubSecTimeOriginal 37521 (TimeDelta) parse_subsec_time)
    ("DateTimeDigitized sub-seconds" SubSecTimeDigitized 37522 (TimeDelta) parse_subsec_time)
    ("Temperature" Temperature 37888 (SignedRational) parse_signed_rational)
    ("Humidity" Humidity 37889 (UnsignedRational) parse_unsigned_rational)
    ("Pressure" Pressure 37890 (UnsignedRational) parse_unsigned_rational)
    ("WaterDepth" WaterDepth 37891 (SignedRational) parse_signed_rational)
    ("Acceleration" Acceleration 37892 (UnsignedRational) parse_unsigned_rational)
    ("Camera elevation angle" CameraElevationAngle 37893 (SignedRational) parse_signed_rational)
    ("Title name of Image" ImageTitle 42038 (&'a str) parse_str)
    ("Photographer name" Photographer 42039 (&'a str) parse_str)
    ("Person who edited the image" ImageEditor 42040 (&'a str) parse_str)
    ("Camera Firmware" CameraFirmware 42041 (&'a str) parse_str)
    ("RAW Developing Software" RawDevelopingSoftware 42042 (&'a str) parse_str)
    ("Image Editing Software" ImageEditingSoftware 42043 (&'a str) parse_str)
    ("Metadata Editing Software" MetadataEditingSoftware 42044 (&'a str) parse_str)
    ("Supported Flashpix version" FlashpixVersion 40960 (&'a str) parse_undefined_as_str)
    ("Color space information" ColorSpace 40961 (u16) parse_u16)
    ("Valid image width" PixelXDimension 40962 (u32) parse_u32)
    ("Valid image height" PixelYDimension 40963 (u32) parse_u32)
    ("Related audio file" RelatedSoundFile 40964 (&'a str) parse_str)
    ("Interoperability entries" InteroperabilityIfdPointer 40965 (interop::EntryMap<'a>) parse_interop 1)
    ("Flash energy" FlashEnergy 41483 (UnsignedRational) parse_unsigned_rational)
    ("Spatial frequency response" SpatialFrequencyResponse 41484 (&'a [u8]) parse_bytes)
    ("Focal plane X resolution" FocalPlaneXResolution 41486 (UnsignedRational) parse_unsigned_rational)
    ("Focal plane Y resolution" FocalPlaneYResolution 41487 (UnsignedRational) parse_unsigned_rational)
    ("Focal plane resolution unit" FocalPlaneResolutionUnit 41488 (u16) parse_u16)
    ("Subject location" SubjectLocation 41492 ([u16; 2]) parse_u16_2)
    ("Exposure index" ExposureIndex 41493 (UnsignedRational) parse_unsigned_rational)
    ("Sensing method" SensingMethod 41495 (u16) parse_u16)
    ("File source" FileSource 41728 (&'a [u8]) parse_bytes)
    ("Scene type" SceneType 41729 (&'a [u8]) parse_bytes)
    ("CFA pattern" CfaPattern 41730 (&'a [u8]) parse_bytes)
    ("Custom image processing" CustomRendered 41985 (u16) parse_u16)
    ("Exposure mode" ExposureMode 41986 (u16) parse_u16)
    ("White balance" WhiteBalance 41987 (u16) parse_u16)
    ("Digital zoom ratio" DigitalZoomRatio 41988 (UnsignedRational) parse_unsigned_rational)
    ("Focal length in 35 mm film" FocalLengthIn35mmFilm 41989 (u16) parse_u16)
    ("Scene capture type" SceneCaptureType 41990 (u16) parse_u16)
    ("Gain control" GainControl 41991 (UnsignedRational) parse_unsigned_rational)
    ("Contrast" Contrast 41992 (u16) parse_u16)
    ("Saturation" Saturation 41993 (u16) parse_u16)
    ("Sharpness" Sharpness 41994 (u16) parse_u16)
    ("Device settings description" DeviceSettingDescription 41995 (&'a [u8]) parse_bytes)
    ("Subject distance range" SubjectDistanceRange 41996 (u16) parse_u16)
    ("Unique image ID" ImageUniqueId 42016 (&'a str) parse_str)
    ("Camera Owner Name" CameraOwnerName 42032 (&'a str) parse_str)
    ("BodySerial Number" BodySerialNumber 42033 (&'a str) parse_str)
    ("Lens Specification" LensSpecification 42034 (LensSpecification) parse_lens_specification)
    ("Lens Make" LensMake 42035 (&'a str) parse_str)
    ("Lens Model" LensModel 42036 (&'a str) parse_str)
    ("Lens Serial Number" LensSerialNumber 42037 (&'a str) parse_str)
    ("Composite image" CompositeImage 42080 (u16) parse_u16)
    ("Source image number of composite image" SourceImageNumberOfCompositeImage 42081 ([u16; 2]) parse_u16_2)
    ("Source exposure times of composite image" SourceExposureTimesOfCompositeImage 42082 (&'a [u8]) parse_bytes)
    ("Gamma" Gamma 42240 (UnsignedRational) parse_unsigned_rational)
}

#[derive(Debug, Clone, Copy)]
pub struct FixedOffset(pub chrono::FixedOffset);

#[cfg(feature = "serde")]
impl serde::Serialize for FixedOffset {
    fn serialize<S>(&self, s: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        use alloc::string::ToString;
        let string = self.0.to_string();
        s.serialize_str(&string)
    }
}

#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
pub enum SubjectArea {
    Point {
        x: u16,
        y: u16,
    },
    Circle {
        center_x: u16,
        center_y: u16,
        diameter: u16,
    },
    Rectangle {
        center_x: u16,
        center_y: u16,
        width: u16,
        height: u16,
    },
}

#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
pub struct LensSpecification {
    pub min_focal_length: UnsignedRational,
    pub max_focal_length: UnsignedRational,
    pub min_f_number: UnsignedRational,
    pub max_f_number: UnsignedRational,
}

#[derive(Debug)]
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
pub enum MakerNote<'a> {
    Apple(apple::EntryMap<'a>),
    Other(&'a [u8]),
}

impl<'a> crate::RawEntry<'a> {
    fn parse_interop(&self, options: ParseOptions) -> Result<interop::EntryMap<'a>, InvalidExif> {
        let iter = interop::Iter(crate::Iter {
            data: self.data,
            endian: self.endian,
            entries: self.get_nested_entries()?,
        });
        iter.into_entry_map(options)
    }

    fn parse_maker_note(&self, options: ParseOptions) -> Result<MakerNote<'a>, InvalidExif> {
        let bytes = self.get_bytes().ok_or(InvalidExif)?;
        match bytes {
            [
                b'A',
                b'p',
                b'p',
                b'l',
                b'e',
                b' ',
                b'i',
                b'O',
                b'S',
                0,
                0,
                1,
                ..,
            ] => {
                let iter = apple::Iter::parse(bytes)?;
                let entries = iter.into_entry_map(options)?;
                Ok(MakerNote::Apple(entries))
            }
            _ => Ok(MakerNote::Other(bytes)),
        }
    }

    fn parse_time_offset(&self) -> Result<FixedOffset, InvalidExif> {
        use chrono::format::Fixed;
        use chrono::format::Item;
        use chrono::format::Parsed;
        let s = self.parse_str()?;
        let mut parsed = Parsed::new();
        let items = [Item::Fixed(Fixed::TimezoneOffsetZ)];
        chrono::format::parse(&mut parsed, s, items.iter()).map_err(|_| InvalidExif)?;
        let offset = parsed.to_fixed_offset().map_err(|_| InvalidExif)?;
        Ok(FixedOffset(offset))
    }

    fn parse_subsec_time(&self) -> Result<TimeDelta, InvalidExif> {
        let mut s = self.parse_str()?.trim();
        let mut n = s.len();
        if n > 9 {
            s = &s[..9];
            n = 9;
        }
        let m = 9 - n;
        let subsec: u64 = s.parse().map_err(|_| InvalidExif)?;
        let nanos = subsec
            .checked_mul(10_u64.pow(m as u32))
            .ok_or(InvalidExif)?
            .try_into()
            .map_err(|_| InvalidExif)?;
        TimeDelta::new(0, nanos).ok_or(InvalidExif)
    }

    fn parse_subject_area(&self) -> Result<SubjectArea, InvalidExif> {
        match self.len {
            2 => {
                let [x, y] = self.parse_u16_2()?;
                Ok(SubjectArea::Point { x, y })
            }
            3 => {
                let [center_x, center_y, diameter] = self.parse_u16_3()?;
                Ok(SubjectArea::Circle {
                    center_x,
                    center_y,
                    diameter,
                })
            }
            4 => {
                let [center_x, center_y, width, height] = self.parse_u16_4()?;
                Ok(SubjectArea::Rectangle {
                    center_x,
                    center_y,
                    width,
                    height,
                })
            }
            _ => Err(InvalidExif),
        }
    }

    fn parse_lens_specification(&self) -> Result<LensSpecification, InvalidExif> {
        let [
            min_focal_length,
            max_focal_length,
            min_f_number,
            max_f_number,
        ] = self.parse_unsigned_rational_4()?;
        Ok(LensSpecification {
            min_focal_length,
            max_focal_length,
            min_f_number,
            max_f_number,
        })
    }
}

impl<'a> EntryMap<'a> {
    pub fn created(&self) -> Option<DateTime<Utc>> {
        let date_time = self.get_date_time_original().copied()?;
        let subsec = self.get_sub_sec_time_original().copied();
        let offset = self
            .get_offset_time_original()
            .copied()
            .map(|FixedOffset(offset)| offset);
        to_date_time(date_time, subsec, offset)
    }

    pub fn digitized(&self) -> Option<DateTime<Utc>> {
        let date_time = self.get_date_time_digitized().copied()?;
        let subsec = self.get_sub_sec_time_digitized().copied();
        let offset = self
            .get_offset_time_digitized()
            .copied()
            .map(|FixedOffset(offset)| offset);
        to_date_time(date_time, subsec, offset)
    }
}
