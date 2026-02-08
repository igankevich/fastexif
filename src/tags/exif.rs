use crate::InvalidExif;
use crate::ParseOptions;
use crate::SignedRational;
use crate::UnsignedRational;
use crate::ValueRef;
use crate::apple;
use crate::interop;
use crate::make_tag_enum_v2;
use crate::to_date_time;
use crate::parse_enum;

use bitflags::bitflags;
use chrono::DateTime;
use chrono::NaiveDateTime;
use chrono::TimeDelta;
use chrono::Utc;

make_tag_enum_v2! {
    Tag "Exif tags"
    Entry "Exif entry"
    EntryMap "Exif entries"
    (Iter)
    ("Exposure time in seconds" ExposureTime 33434 (UnsignedRational) parse_unsigned_rational)
    ("F number (focal ratio)" FNumber 33437 (UnsignedRational) parse_unsigned_rational)
    ("Exposure program" ExposureProgram 34850 (ExposureProgram) parse_exposure_program)
    ("Spectral sensitivity" SpectralSensitivity 34852 (&'a str) parse_str)
    ("Photographic Sensitivity" PhotographicSensitivity 34855 (u16) parse_u16)
    ("Optoelectric coefficient" Oecf 34856 (&'a [u8]) parse_bytes)
    ("Sensitivity Type" SensitivityType 34864 (SensitivityType) parse_sensitivity_type)
    ("Standard Output Sensitivity" StandardOutputSensitivity 34865 (u32) parse_u32)
    ("Recommended Exposure Index" RecommendedExposureIndex 34866 (u32) parse_u32)
    ("ISOSpeed" IsoSpeed 34867 (u32) parse_u32)
    ("ISOSpeed Latitude yyy" IsoSpeedLatitudeYyy 34868 (u32) parse_u32)
    ("ISOSpeed Latitude zzz" IsoSpeedLatitudeZzz 34869 (u32) parse_u32)
    ("Exif Version" ExifVersion 36864 (&'a str) parse_undefined_as_str)
    ("Date and time original image was generated" DateTimeOriginal 36867 (NaiveDateTime) parse_naive_date_time)
    ("Date and time image was made digital data" DateTimeDigitized 36868 (NaiveDateTime) parse_naive_date_time)
    ("Offset data of DateTime" OffsetTime 36880 (FixedOffset) parse_time_offset)
    ("Offset data of DateTimeOriginal" OffsetTimeOriginal 36881 (FixedOffset) parse_time_offset)
    ("Offset data of DateTimeDigitized" OffsetTimeDigitized 36882 (FixedOffset) parse_time_offset)
    ("Meaning of each component" ComponentsConfiguration 37121 ([Component; 4]) parse_components_configuration)
    ("Image compression mode" CompressedBitsPerPixel 37122 (UnsignedRational) parse_unsigned_rational)
    ("Shutter speed" ShutterSpeedValue 37377 (SignedRational) parse_signed_rational)
    ("Aperture" ApertureValue 37378 (UnsignedRational) parse_unsigned_rational)
    ("Brightness" BrightnessValue 37379 (SignedRational) parse_signed_rational)
    ("Exposure bias" ExposureBiasValue 37380 (SignedRational) parse_signed_rational)
    ("Maximum lens aperture" MaxApertureValue 37381 (UnsignedRational) parse_unsigned_rational)
    ("Subject distance in meters" SubjectDistance 37382 (UnsignedRational) parse_unsigned_rational)
    ("Metering mode" MeteringMode 37383 (MeteringMode) parse_metering_mode)
    ("Light source" LightSource 37384 (LightSource) parse_light_source)
    ("Flash" Flash 37385 (Flash) parse_flash)
    ("Lens focal length in millimeters" FocalLength 37386 (UnsignedRational) parse_unsigned_rational)
    ("Subject area" SubjectArea 37396 (SubjectArea) parse_subject_area)
    ("Manufacturer notes (vendor-specific entries)" MakerNote 37500 (MakerNote<'a>) parse_maker_note 1)
    ("User comments" UserComment 37510 (&'a [u8]) parse_bytes)
    ("DateTime sub-seconds" SubSecTime 37520 (TimeDelta) parse_subsec_time)
    ("DateTimeOriginal sub-seconds" SubSecTimeOriginal 37521 (TimeDelta) parse_subsec_time)
    ("DateTimeDigitized sub-seconds" SubSecTimeDigitized 37522 (TimeDelta) parse_subsec_time)
    ("Temperature in degrees of Celsius" Temperature 37888 (SignedRational) parse_signed_rational)
    ("Humidity in percents" Humidity 37889 (UnsignedRational) parse_unsigned_rational)
    ("Pressure in hPa" Pressure 37890 (UnsignedRational) parse_unsigned_rational)
    ("WaterDepth in meters" WaterDepth 37891 (SignedRational) parse_signed_rational)
    ("Acceleration in m/s²" Acceleration 37892 (UnsignedRational) parse_unsigned_rational)
    ("Camera elevation angle in degrees" CameraElevationAngle 37893 (SignedRational) parse_signed_rational)
    ("Title name of Image" ImageTitle 42038 (&'a str) parse_str)
    ("Photographer name" Photographer 42039 (&'a str) parse_str)
    ("Person who edited the image" ImageEditor 42040 (&'a str) parse_str)
    ("Camera Firmware" CameraFirmware 42041 (&'a str) parse_str)
    ("RAW Developing Software" RawDevelopingSoftware 42042 (&'a str) parse_str)
    ("Image Editing Software" ImageEditingSoftware 42043 (&'a str) parse_str)
    ("Metadata Editing Software" MetadataEditingSoftware 42044 (&'a str) parse_str)
    ("Supported Flashpix version" FlashpixVersion 40960 (&'a str) parse_undefined_as_str)
    ("Color space information" ColorSpace 40961 (ColorSpace) parse_color_space)
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
    ("Sensing method" SensingMethod 41495 (SensingMethod) parse_sensing_method)
    ("File source" FileSource 41728 (FileSource) parse_file_source)
    ("Scene type" SceneType 41729 (SceneType) parse_scene_type)
    ("CFA pattern" CfaPattern 41730 (&'a [u8]) parse_bytes)
    ("Custom image processing" CustomRendered 41985 (CustomRendered) parse_custom_rendered)
    ("Exposure mode" ExposureMode 41986 (ExposureMode) parse_exposure_mode)
    ("White balance" WhiteBalance 41987 (WhiteBalance) parse_white_balance)
    ("Digital zoom ratio" DigitalZoomRatio 41988 (UnsignedRational) parse_unsigned_rational)
    ("Focal length in 35 mm film" FocalLengthIn35mmFilm 41989 (u16) parse_u16)
    ("Scene capture type" SceneCaptureType 41990 (SceneCaptureType) parse_scene_capture_type)
    ("Gain control" GainControl 41991 (GainControl) parse_gain_control)
    ("Contrast" Contrast 41992 (Contrast) parse_contrast)
    ("Saturation" Saturation 41993 (Saturation) parse_saturation)
    ("Sharpness" Sharpness 41994 (Sharpness) parse_sharpness)
    ("Device settings description" DeviceSettingDescription 41995 (&'a [u8]) parse_bytes)
    ("Subject distance range" SubjectDistanceRange 41996 (SubjectDistanceRange) parse_subject_distance_range)
    ("Unique image ID" ImageUniqueId 42016 (&'a str) parse_str)
    ("Camera Owner Name" CameraOwnerName 42032 (&'a str) parse_str)
    ("BodySerial Number" BodySerialNumber 42033 (&'a str) parse_str)
    ("Lens Specification" LensSpecification 42034 (LensSpecification) parse_lens_specification)
    ("Lens Make" LensMake 42035 (&'a str) parse_str)
    ("Lens Model" LensModel 42036 (&'a str) parse_str)
    ("Lens Serial Number" LensSerialNumber 42037 (&'a str) parse_str)
    ("Composite image" CompositeImage 42080 (CompositeImage) parse_composite_image)
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
pub enum ColorSpace {
    Srgb,
    Uncalibrated,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
pub enum Component {
    None,
    Y,
    Cb,
    Cr,
    R,
    G,
    B,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
pub enum ExposureProgram {
    Undefined,
    Manual,
    Normal,
    AperturePriority,
    ShutterPriority,
    Creative,
    Action,
    Portrait,
    Landscape,
}

/// Photographic sensitivity parameters.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
pub enum SensitivityType {
    /// Unknown.
    Unknown,
    /// Standard output sensitivity (SOS).
    Sos,
    /// Recommended exposure index (REI).
    Rei,
    /// ISO speed.
    IsoSpeed,
    /// SOS and REI.
    SosRei,
    /// SOS and ISO speed.
    SosIsoSpeed,
    /// REI and ISO speed.
    ReiIsoSpeed,
    /// SOS, REI, ISO speed.
    SosReiIsoSpeed,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
pub enum MeteringMode {
    Unknown,
    Average,
    CenterWeightedAverage,
    Spot,
    MultiSpot,
    Pattern,
    Partial,
    Other,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
pub enum LightSource {
    Unknown,
    Daylight,
    Fluorescent,
    Tungsten,
    Flash,
    FineWeather,
    CloudyWeather,
    Shade,
    DaylightFluorescent,
    DayWhiteFluorescent,
    CoolWhiteFluorescent,
    WhiteFluorescent,
    WarmWhiteFluorescent,
    StandardLightA,
    StandardLightB,
    StandardLightC,
    D55,
    D65,
    D75,
    D50,
    IsoStudioTungsten,
    Other,
}

bitflags! {
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    #[cfg_attr(feature = "serde", derive(serde::Serialize))]
    pub struct Flash: u16 {
        const FIRED                            = 0b000_00_00_1;
        const STROBE_RETURN_LIGHT_NOT_DETECTED = 0b000_00_10_0;
        const STROBE_RETURN_LIGHT_DETECTED     = 0b000_00_11_0;
        const COMPULSORY_FLASH_FIRING          = 0b000_01_00_0;
        const COMPULSORY_FLASH_SUPPRESSION     = 0b000_10_00_0;
        const AUTO_MODE                        = 0b000_11_00_0;
        const NO_FLASH_FUNCTION                = 0b001_00_00_0;
        const RED_EYE_REDUCTION_SUPPORTED      = 0b010_00_00_0;
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
pub enum SensingMethod {
    Undefined,
    OneChipColorArea,
    TwoChipColorArea,
    ThreeChipColorArea,
    ColorSequentialArea,
    Trilinear,
    ColorSequentialTrilinear,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
pub enum FileSource {
    Other,
    TransparentScanner,
    ReflexScanner,
    Dsc,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
pub enum SceneType {
    /// A directly photographed image.
    DirectPhoto,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
pub enum CustomRendered {
    NormalProcess,
    CustomProcess,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
pub enum ExposureMode {
    Auto,
    Manual,
    AutoBracket,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
pub enum WhiteBalance {
    Auto,
    Manual,
}

parse_enum! {
    SceneCaptureType u16
    (Standard 0)
    (Landscape 1)
    (Portrait 2)
    (NightScene 3)
}

parse_enum! {
    GainControl u16
    (None 0)
    (LowGainUp 1)
    (HighGainUp 2)
    (LowGainDown 3)
    (HighGainDown 4)
}

parse_enum! {
    Contrast u16
    (Normal 0)
    (Soft 1)
    (Hard 2)
}

parse_enum! {
    Saturation u16
    (Normal 0)
    (Low 1)
    (High 2)
}

parse_enum! {
    Sharpness u16
    (Normal 0)
    (Soft 1)
    (Hard 2)
}

parse_enum! {
    SubjectDistanceRange u16
    (Unknown 0)
    (Macro 1)
    (CloseView 2)
    (DistantView 3)
}

parse_enum! {
    CompositeImage u16
    (Unknown 0)
    (NonComposite 1)
    (GeneralComposite 2)
    (CompositeWhenShooting 3)
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

    fn parse_color_space(&self) -> Result<ColorSpace, InvalidExif> {
        match self.parse_u16()? {
            1 => Ok(ColorSpace::Srgb),
            u16::MAX => Ok(ColorSpace::Uncalibrated),
            _ => Err(InvalidExif),
        }
    }

    fn parse_components_configuration(&self) -> Result<[Component; 4], InvalidExif> {
        fn parse_component(value: u8) -> Result<Component, InvalidExif> {
            match value {
                0 => Ok(Component::None),
                1 => Ok(Component::Y),
                2 => Ok(Component::Cb),
                3 => Ok(Component::Cr),
                4 => Ok(Component::R),
                5 => Ok(Component::G),
                6 => Ok(Component::B),
                _ => Err(InvalidExif),
            }
        }
        match self.parse_bytes()? {
            [c0, c1, c2, c3] => Ok([
                parse_component(*c0)?,
                parse_component(*c1)?,
                parse_component(*c2)?,
                parse_component(*c3)?,
            ]),
            _ => Err(InvalidExif),
        }
    }

    fn parse_exposure_program(&self) -> Result<ExposureProgram, InvalidExif> {
        match self.parse_u16()? {
            0 => Ok(ExposureProgram::Undefined),
            1 => Ok(ExposureProgram::Manual),
            2 => Ok(ExposureProgram::Normal),
            3 => Ok(ExposureProgram::AperturePriority),
            4 => Ok(ExposureProgram::ShutterPriority),
            5 => Ok(ExposureProgram::Creative),
            6 => Ok(ExposureProgram::Action),
            7 => Ok(ExposureProgram::Portrait),
            8 => Ok(ExposureProgram::Landscape),
            _ => Err(InvalidExif),
        }
    }

    fn parse_sensitivity_type(&self) -> Result<SensitivityType, InvalidExif> {
        match self.parse_u16()? {
            0 => Ok(SensitivityType::Unknown),
            1 => Ok(SensitivityType::Sos),
            2 => Ok(SensitivityType::Rei),
            3 => Ok(SensitivityType::IsoSpeed),
            4 => Ok(SensitivityType::SosRei),
            5 => Ok(SensitivityType::SosIsoSpeed),
            6 => Ok(SensitivityType::ReiIsoSpeed),
            7 => Ok(SensitivityType::SosReiIsoSpeed),
            _ => Err(InvalidExif),
        }
    }

    fn parse_metering_mode(&self) -> Result<MeteringMode, InvalidExif> {
        match self.parse_u16()? {
            0 => Ok(MeteringMode::Unknown),
            1 => Ok(MeteringMode::Average),
            2 => Ok(MeteringMode::CenterWeightedAverage),
            3 => Ok(MeteringMode::Spot),
            4 => Ok(MeteringMode::MultiSpot),
            5 => Ok(MeteringMode::Pattern),
            6 => Ok(MeteringMode::Partial),
            255 => Ok(MeteringMode::Other),
            _ => Err(InvalidExif),
        }
    }

    fn parse_light_source(&self) -> Result<LightSource, InvalidExif> {
        match self.parse_u16()? {
            0 => Ok(LightSource::Unknown),
            1 => Ok(LightSource::Daylight),
            2 => Ok(LightSource::Fluorescent),
            3 => Ok(LightSource::Tungsten),
            4 => Ok(LightSource::Flash),
            9 => Ok(LightSource::FineWeather),
            10 => Ok(LightSource::CloudyWeather),
            11 => Ok(LightSource::Shade),
            12 => Ok(LightSource::DaylightFluorescent),
            13 => Ok(LightSource::DayWhiteFluorescent),
            14 => Ok(LightSource::CoolWhiteFluorescent),
            15 => Ok(LightSource::WhiteFluorescent),
            16 => Ok(LightSource::WarmWhiteFluorescent),
            17 => Ok(LightSource::StandardLightA),
            18 => Ok(LightSource::StandardLightB),
            19 => Ok(LightSource::StandardLightC),
            20 => Ok(LightSource::D55),
            21 => Ok(LightSource::D65),
            22 => Ok(LightSource::D75),
            23 => Ok(LightSource::D50),
            24 => Ok(LightSource::IsoStudioTungsten),
            255 => Ok(LightSource::Other),
            _ => Err(InvalidExif),
        }
    }

    fn parse_flash(&self) -> Result<Flash, InvalidExif> {
        let bits = self.parse_u16()?;
        Ok(Flash::from_bits_retain(bits))
    }

    fn parse_sensing_method(&self) -> Result<SensingMethod, InvalidExif> {
        match self.parse_u16()? {
            1 => Ok(SensingMethod::Undefined),
            2 => Ok(SensingMethod::OneChipColorArea),
            3 => Ok(SensingMethod::TwoChipColorArea),
            4 => Ok(SensingMethod::ThreeChipColorArea),
            5 => Ok(SensingMethod::ColorSequentialArea),
            7 => Ok(SensingMethod::Trilinear),
            8 => Ok(SensingMethod::ColorSequentialTrilinear),
            _ => Err(InvalidExif),
        }
    }

    fn parse_file_source(&self) -> Result<FileSource, InvalidExif> {
        match self.parse_bytes()? {
            [0] => Ok(FileSource::Other),
            [1] => Ok(FileSource::TransparentScanner),
            [2] => Ok(FileSource::ReflexScanner),
            [3] => Ok(FileSource::Dsc),
            _ => Err(InvalidExif),
        }
    }

    fn parse_scene_type(&self) -> Result<SceneType, InvalidExif> {
        match self.parse_bytes()? {
            [1] => Ok(SceneType::DirectPhoto),
            _ => Err(InvalidExif),
        }
    }

    fn parse_custom_rendered(&self) -> Result<CustomRendered, InvalidExif> {
        match self.parse_u16()? {
            0 => Ok(CustomRendered::NormalProcess),
            1 => Ok(CustomRendered::CustomProcess),
            _ => Err(InvalidExif),
        }
    }

    fn parse_exposure_mode(&self) -> Result<ExposureMode, InvalidExif> {
        match self.parse_u16()? {
            0 => Ok(ExposureMode::Auto),
            1 => Ok(ExposureMode::Manual),
            2 => Ok(ExposureMode::AutoBracket),
            _ => Err(InvalidExif),
        }
    }

    fn parse_white_balance(&self) -> Result<WhiteBalance, InvalidExif> {
        match self.parse_u16()? {
            0 => Ok(WhiteBalance::Auto),
            1 => Ok(WhiteBalance::Manual),
            _ => Err(InvalidExif),
        }
    }
}

impl<'a> EntryMap<'a> {
    /// The time when the photo was taken.
    pub fn created(&self) -> Option<DateTime<Utc>> {
        let date_time = self.get_date_time_original().copied()?;
        let subsec = self.get_sub_sec_time_original().copied();
        let offset = self
            .get_offset_time_original()
            .copied()
            .map(|FixedOffset(offset)| offset);
        to_date_time(date_time, subsec, offset)
    }

    /// The time when the photo was digitized.
    pub fn digitized(&self) -> Option<DateTime<Utc>> {
        let date_time = self.get_date_time_digitized().copied()?;
        let subsec = self.get_sub_sec_time_digitized().copied();
        let offset = self
            .get_offset_time_digitized()
            .copied()
            .map(|FixedOffset(offset)| offset);
        to_date_time(date_time, subsec, offset)
    }

    /// Distance to subject in meters.
    ///
    /// Returns `None` if the distance is unknown or the corresponding tag was not found.
    pub fn subject_distance(&self) -> Option<f64> {
        let rational = self.get_subject_distance()?;
        if rational.0 == 0 {
            return None;
        }
        if rational.1 == u32::MAX {
            return Some(f64::INFINITY);
        }
        Some(rational.as_f64())
    }
}
