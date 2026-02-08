#![allow(clippy::unusual_byte_groupings)]

use crate::InvalidExif;
use crate::ParseOptions;
use crate::SignedRational;
use crate::UnsignedRational;
use crate::Value;
use crate::apple;
use crate::define_tag_enum;
use crate::define_value_enums;
use crate::interop;
use crate::to_date_time;

use bitflags::bitflags;
use chrono::DateTime;
use chrono::NaiveDateTime;
use chrono::TimeDelta;
use chrono::Utc;

define_tag_enum! {
    Tag "Exif tag"
    Entry "Exif entry"
    EntryMap "Exif entries"
    (Iter)
    ("Exposure time in seconds" ExposureTime 33434 (UnsignedRational) parse_unsigned_rational)
    ("F number (focal ratio)" FNumber 33437 (UnsignedRational) parse_unsigned_rational)
    ("Exposure program" ExposureProgram 34850 (ExposureProgram) parse_exposure_program)
    ("Spectral sensitivity" SpectralSensitivity 34852 (&'a str) parse_str)
    ("Photographic sensitivity" PhotographicSensitivity 34855 (u16) parse_u16)
    ("Optoelectric coefficient" Oecf 34856 (&'a [u8]) parse_bytes)
    ("Sensitivity type" SensitivityType 34864 (SensitivityType) parse_sensitivity_type)
    ("Standard Output sensitivity" StandardOutputSensitivity 34865 (u32) parse_u32)
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
    ("Interoperability entries" Interoperability 40965 (interop::EntryMap<'a>) parse_interop 1)
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

define_value_enums! {
    (ColorSpace u16 "Color space."
        (Srgb 1 "sRGB color space.")
        (Uncalibrated 0xffff "Other color space."))
    (ExposureProgram u16 "Exposure program class."
        (Undefined 0 "Not defined")
        (Manual 1 "Manual exposure")
        (Normal 2 "Normal program")
        (AperturePriority 3 "Aperture priority")
        (ShutterPriority 4 "Shutter priority")
        (Creative 5 "Creative program (biased toward depth of field)")
        (Action 6 "Action program (biased toward fast shutter speed)")
        (Portrait 7 "Portrait mode (for closeup photos with the background out of focus)")
        (Landscape 8 "Landscape mode (for landscape photos with the background in focus)"))
    (SensitivityType u16 "Photographic sensitivity parameters."
        (Unknown 0 "Unknown type")
        (Sos 1 "Standard output sensitivity (SOS)")
        (Rei 2 "Recommended exposure index (REI)")
        (IsoSpeed 3 "ISO speed")
        (SosRei 4 "SOS and REI")
        (SosIsoSpeed 5 "SOS and ISO speed")
        (ReiIsoSpeed 6 "REI and ISO speed")
        (SosReiIsoSpeed 7 "SOS, REI, ISO speed"))
    (MeteringMode u16 "Metering mode."
        (Unknown 0 "Unknown mode")
        (Average 1 "Average")
        (CenterWeightedAverage 2 "Center-weighted average")
        (Spot 3 "Spot")
        (MultiSpot 4 "Multi-spot")
        (Pattern 5 "Pattern")
        (Partial 6 "Partial")
        (Other 255 "Other"))
    (LightSource u16 "Light source."
        (Unknown 0 "Unknown light source")
        (Daylight 1 "Daylight")
        (Fluorescent 2 "Fluorescent")
        (Tungsten 3 "Tungsten (incandescent light)")
        (Flash 4 "Flash")
        (FineWeather 9 "Fine weather")
        (CloudyWeather 10 "Cloudy weather")
        (Shade 11 "Shade")
        (DaylightFluorescent 12 "Daylight fluorescent (D 5700–7100 K)")
        (DayWhiteFluorescent 13 "Day white fluorescent (N 4600–5500 K)")
        (CoolWhiteFluorescent 14 "Cool white fluorescent (W 3800–4500 K)")
        (WhiteFluorescent 15 "White fluorescent (WW 3250–3800 K)")
        (WarmWhiteFluorescent 16 "Warm white fluorescent (L 2600–3250 K)")
        (StandardLightA 17 "Standard light A")
        (StandardLightB 18 "Standard light B")
        (StandardLightC 19 "Standard light C")
        (D55 20 "D55")
        (D65 21 "D65")
        (D75 22 "D75")
        (D50 23 "D50")
        (IsoStudioTungsten 24 "ISO studio tungsten")
        (Other 255 "Other light source"))
    (SensingMethod u16 "Image sensor type."
        (Undefined 1 "Not defined")
        (OneChipColorArea 2 "One-chip color area sensor")
        (TwoChipColorArea 3 "Two-chip color area sensor")
        (ThreeChipColorArea 4 "Three-chip color area sensor")
        (ColorSequentialArea 5 "Color sequential area sensor")
        (Trilinear 7 "Trilinear sensor")
        (ColorSequentialTrilinear 8 "Color sequential linear sensor"))
    (CustomRendered u16 "Special processing."
        (NormalProcess 0 "Normal process")
        (CustomProcess 1 "Custom process"))
    (ExposureMode u16 "Exposure mode."
        (Auto 0 "Auto exposure")
        (Manual 1 "Manual exposure")
        (AutoBracket 2 "Auto bracket"))
    (WhiteBalance u16 "White balance mode."
        (Auto 0 "Auto")
        (Manual 1 "Manual"))
    (SceneCaptureType u16 "The type of scene"
        (Standard 0 "Standard")
        (Landscape 1 "Landscape")
        (Portrait 2 "Portrait")
        (NightScene 3 "Night scene"))
    (GainControl u16 "Image gain adjustment rate."
        (None 0 "None")
        (LowGainUp 1 "Low gain up")
        (HighGainUp 2 "High gain up")
        (LowGainDown 3 "Low gain down")
        (HighGainDown 4 "High gain down"))
    (Contrast u16 "Contrast adjustment."
        (Normal 0 "Normal")
        (Soft 1 "Soft")
        (Hard 2 "Hard"))
    (Saturation u16 "Saturation adjustment."
        (Normal 0 "Normal")
        (Low 1 "Low saturation")
        (High 2 "High saturation"))
    (Sharpness u16 "Sharpness adjustment."
        (Normal 0 "Normal")
        (Soft 1 "Soft")
        (Hard 2 "Hard"))
    (SubjectDistanceRange u16 "Distance to the subject"
        (Unknown 0 "Unknown")
        (Macro 1 "Macro")
        (CloseView 2 "Close view")
        (DistantView 3 "Distant view"))
    (CompositeImage u16 "Composite image or not."
        (Unknown 0 "Unknown")
        (NonComposite 1 "Non-composite image")
        (GeneralComposite 2 "General composite image")
        (CompositeWhenShooting 3 "Composite image captured when shooting"))
}

/// Image source.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
pub enum FileSource {
    /// Others.
    Other,
    /// Scanner of transparent type.
    TransparentScanner,
    /// Scanner of reflex type.
    ReflexScanner,
    /// DSC.
    Dsc,
}

/// Scene type.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
pub enum SceneType {
    /// A directly photographed image.
    DirectPhoto,
}

/// Timezone offset.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct FixedOffset(pub chrono::FixedOffset);

#[cfg(feature = "serde")]
#[cfg_attr(docsrs, doc(cfg(feature = "serde")))]
impl serde::Serialize for FixedOffset {
    fn serialize<S>(&self, s: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        s.collect_str(&self.0)
    }
}

/// The location and are of the main subject in the scene.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
pub enum SubjectArea {
    /// Single point.
    Point {
        /// X coordinate.
        x: u16,
        /// Y coordinate.
        y: u16,
    },
    /// Circle containing the subject.
    Circle {
        /// X coordinate of the center.
        center_x: u16,
        /// Y coordinate of the center.
        center_y: u16,
        /// Circle diameter.
        diameter: u16,
    },
    /// Rectangle containing the subject.
    Rectangle {
        /// X coordinate of the center.
        center_x: u16,
        /// Y coordinate of the center.
        center_y: u16,
        /// Width.
        width: u16,
        /// Height.
        height: u16,
    },
}

/// Range of camera lens parameters.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
pub struct LensSpecification {
    /// Minimum focal length and associated minimum F number.
    pub min: LensSpecificationComponent,
    /// Maximum focal length and associated minimum F number.
    pub max: LensSpecificationComponent,
}

/// Camera lens parameters.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
pub struct LensSpecificationComponent {
    /// Focal length in millimeters.
    pub focal_length: UnsignedRational,
    /// Minimum F number.
    pub min_f_number: UnsignedRational,
}

/// Manufacturer note.
///
/// This tag is used to store vendor-specific metadata.
///
/// `fastexif` can parse some such metadata when the corresponding feature is enabled.
#[derive(Debug)]
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
pub enum MakerNote<'a> {
    /// Apple-specific metadata.
    #[cfg(feature = "apple")]
    #[cfg_attr(docsrs, doc(cfg(feature = "apple")))]
    Apple(apple::EntryMap<'a>),
    /// Unknown metadata.
    Other(&'a [u8]),
}

/// Color component.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
pub enum Component {
    /// Doesn't exist.
    None,
    /// Luminance.
    Y,
    /// Chrominance (blue-difference).
    Cb,
    /// Chrominance (red-differenceV).
    Cr,
    /// Red.
    R,
    /// Green.
    G,
    /// Blue.
    B,
}

bitflags! {
    /// Flash parameters.
    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    #[cfg_attr(feature = "serde", derive(serde::Serialize))]
    pub struct Flash: u16 {
        /// Flash fired.
        const FIRED                            = 0b000_00_00_1;
        /// Strobe return light not detected.
        const STROBE_RETURN_LIGHT_NOT_DETECTED = 0b000_00_10_0;
        /// Strobe return light detected.
        const STROBE_RETURN_LIGHT_DETECTED     = 0b000_00_11_0;
        /// Compulsory flash firing.
        const COMPULSORY_FLASH_FIRING          = 0b000_01_00_0;
        /// Compulsory flash suppression.
        const COMPULSORY_FLASH_SUPPRESSION     = 0b000_10_00_0;
        /// Auto mode.
        const AUTO_MODE                        = 0b000_11_00_0;
        /// No flash function.
        const NO_FLASH_FUNCTION                = 0b001_00_00_0;
        /// Red-eye reduction supported.
        const RED_EYE_REDUCTION_SUPPORTED      = 0b010_00_00_0;
    }
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
            min_f_number_in_min_focal_length,
            min_f_number_in_max_focal_length,
        ] = self.parse_unsigned_rational_4()?;
        Ok(LensSpecification {
            min: LensSpecificationComponent {
                focal_length: min_focal_length,
                min_f_number: min_f_number_in_min_focal_length,
            },
            max: LensSpecificationComponent {
                focal_length: max_focal_length,
                min_f_number: min_f_number_in_max_focal_length,
            },
        })
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

    fn parse_flash(&self) -> Result<Flash, InvalidExif> {
        let bits = self.parse_u16()?;
        Ok(Flash::from_bits_retain(bits))
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
