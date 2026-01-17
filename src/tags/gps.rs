use crate::InvalidExif;
use crate::UnsignedRational;
use crate::ValueRef;
use crate::make_tag_enum_v2;

use chrono::DateTime;
use chrono::NaiveDate;
use chrono::NaiveTime;
use chrono::Utc;

make_tag_enum_v2! {
    Tag "GPS tags"
    Entry "GPS entry"
    EntryMap "GPS entries"
    (Iter)
    ("GPS tag version" VersionId 0 (&'a str) parse_str)
    ("North or South Latitude" LatitudeRef 1 (LatitudeRef) parse_latitude_ref)
    ("Latitude" Latitude 2 (GpsDegrees) parse_latitude_longitude)
    ("East or West Longitude" LongitudeRef 3 (LongitudeRef) parse_longitude_ref)
    ("Longitude" Longitude 4 (GpsDegrees) parse_latitude_longitude)
    ("Altitude reference" AltitudeRef 5 (AltitudeRef) parse_altitude_ref)
    ("Altitude" Altitude 6 (UnsignedRational) parse_altitude)
    ("GPS time (atomic clock)" TimeStamp 7 (NaiveTime) parse_time_stamp)
    ("GPS satellites used for measurement" Satellites 8 (&'a str) parse_str)
    ("GPS receiver status" Status 9 (&'a str) parse_str)
    ("GPS measurement mode" MeasureMode 10 (&'a str) parse_str)
    ("Measurement precision" Dop 11 (UnsignedRational) parse_unsigned_rational)
    ("Speed unit" SpeedRef 12 (SpeedRef) parse_speed_ref)
    ("Speed of GPS receiver" Speed 13 (UnsignedRational) parse_unsigned_rational)
    ("Reference for direction of movement" TrackRef 14 (Direction) parse_direction)
    ("Direction of movement" Track 15 (UnsignedRational) parse_unsigned_rational)
    ("Reference for direction of image" ImgDirectionRef 16 (Direction) parse_direction)
    ("Direction of image" ImgDirection 17 (UnsignedRational) parse_unsigned_rational)
    ("Geodetic survey data used" MapDatum 18 (&'a str) parse_str)
    ("Reference for latitude of destination" DestLatitudeRef 19 (&'a str) parse_str)
    ("Latitude of destination" DestLatitude 20 (LatitudeRef) parse_latitude_ref)
    ("Reference for longitude of destination" DestLongitudeRef 21 (LongitudeRef) parse_longitude_ref)
    ("Longitude of destination" DestLongitude 22 (UnsignedRational) parse_unsigned_rational)
    ("Reference for bearing of destination" DestBearingRef 23 (Direction) parse_direction)
    ("Bearing of destination" DestBearing 24 (UnsignedRational) parse_unsigned_rational)
    ("Reference for distance to destination" DestDistanceRef 25 (DistanceRef) parse_distance_ref)
    ("Distance to destination" DestDistance 26 (UnsignedRational) parse_unsigned_rational)
    ("Name of GPS processing method" ProcessingMethod 27 (&'a [u8]) parse_bytes)
    ("Name of GPS area" AreaInformation 28 (&'a [u8]) parse_bytes)
    ("GPS date" DateStamp 29 (NaiveDate) parse_date_stamp)
    ("GPS differential correction" Differential 30 (u16) parse_u16)
    ("Horizontal positioning error" HPositioningError 31 (UnsignedRational) parse_unsigned_rational)
}

impl EntryMap<'_> {
    pub fn time(&self) -> Option<DateTime<Utc>> {
        let date = self.get_date_stamp()?;
        let time = self.get_time_stamp()?;
        Some(date.and_time(*time).and_utc())
    }

    pub fn latitude(&self) -> Option<f64> {
        let latitude = self.get_latitude()?;
        let latitude_ref = self.get_latitude_ref()?;
        match latitude_ref {
            LatitudeRef::North => Some(latitude.as_f64()),
            LatitudeRef::South => Some(-latitude.as_f64()),
        }
    }

    pub fn longitude(&self) -> Option<f64> {
        let longitude = self.get_longitude()?;
        let longitude_ref = self.get_longitude_ref()?;
        match longitude_ref {
            LongitudeRef::East => Some(longitude.as_f64()),
            LongitudeRef::West => Some(-longitude.as_f64()),
        }
    }

    pub fn altitude(&self) -> Option<f64> {
        use AltitudeRef::*;
        let altitude = self.get_altitude()?;
        let altitude_ref = self.get_altitude_ref()?;
        match altitude_ref {
            AboveSeaLevel | AboveEllipsoidalSurface => Some(altitude.as_f64()),
            BelowSeaLevel | BelowEllipsoidalSurface => Some(-altitude.as_f64()),
        }
    }

    /// Returns the speed in km/h.
    pub fn speed(&self) -> Option<f64> {
        use SpeedRef::*;
        let speed = self.get_speed()?;
        let speed_ref = self.get_speed_ref()?;
        match speed_ref {
            KilometersPerHour => Some(speed.as_f64()),
            MilesPerHour => Some(speed.as_f64() * 1.609344),
            KnotsPerHour => Some(speed.as_f64() * 1.852),
        }
    }

    /// Returns movement direction with respect to true north.
    pub fn movement_direction(&self) -> Option<f64> {
        let direction = self.get_track_ref()?;
        if *direction != Direction::True {
            return None;
        }
        self.get_track().map(|d| d.as_f64())
    }

    /// Returns image direction with respect to true north.
    pub fn image_direction(&self) -> Option<f64> {
        let direction = self.get_img_direction_ref()?;
        if *direction != Direction::True {
            return None;
        }
        self.get_img_direction().map(|d| d.as_f64())
    }

    /// Returns destination direction with respect to true north.
    pub fn destination_direction(&self) -> Option<f64> {
        let direction = self.get_dest_bearing_ref()?;
        if *direction != Direction::True {
            return None;
        }
        self.get_dest_bearing().map(|d| d.as_f64())
    }
}

#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
pub struct GpsDegrees(UnsignedRational, UnsignedRational, UnsignedRational);

impl GpsDegrees {
    pub const fn degrees(&self) -> &UnsignedRational {
        &self.0
    }

    pub const fn minutes(&self) -> &UnsignedRational {
        &self.1
    }

    pub const fn seconds(&self) -> &UnsignedRational {
        &self.2
    }

    pub fn as_f64(&self) -> f64 {
        self.degrees().as_f64()
            + self.minutes().as_f64() * (1.0 / 60.0)
            + self.seconds().as_f64() * (1.0 / 3600.0)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
pub enum LatitudeRef {
    North,
    South,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
pub enum LongitudeRef {
    East,
    West,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
pub enum AltitudeRef {
    AboveEllipsoidalSurface = 0,
    BelowEllipsoidalSurface = 1,
    AboveSeaLevel = 2,
    BelowSeaLevel = 3,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
pub enum SpeedRef {
    KilometersPerHour,
    MilesPerHour,
    KnotsPerHour,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
pub enum Direction {
    True,
    Magnetic,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
pub enum DistanceRef {
    Kilometers,
    Miles,
    Knots,
}

impl<'a> crate::RawEntry<'a> {
    fn parse_time_stamp(&self) -> Result<NaiveTime, InvalidExif> {
        let [hours, minutes, seconds] = self.parse_unsigned_rational_3()?;
        let common_denominator = u64::from(hours.1)
            .checked_mul(u64::from(minutes.1))
            .ok_or(InvalidExif)?
            .checked_mul(u64::from(seconds.1))
            .ok_or(InvalidExif)?;
        if common_denominator == 0 {
            return Err(InvalidExif);
        }
        let mut nanos: u64 = 0;
        nanos = nanos
            .checked_add(
                u64::from(hours.0)
                    .checked_mul(common_denominator / u64::from(hours.1))
                    .ok_or(InvalidExif)?
                    .checked_mul(3600 * 1_000_000_000)
                    .ok_or(InvalidExif)?,
            )
            .ok_or(InvalidExif)?;
        nanos = nanos
            .checked_add(
                u64::from(minutes.0)
                    .checked_mul(common_denominator / u64::from(minutes.1))
                    .ok_or(InvalidExif)?
                    .checked_mul(60 * 1_000_000_000)
                    .ok_or(InvalidExif)?,
            )
            .ok_or(InvalidExif)?;
        nanos = nanos
            .checked_add(
                u64::from(seconds.0)
                    .checked_mul(common_denominator / u64::from(seconds.1))
                    .ok_or(InvalidExif)?
                    .checked_mul(1_000_000_000)
                    .ok_or(InvalidExif)?,
            )
            .ok_or(InvalidExif)?;
        nanos /= common_denominator;
        let s = nanos
            .checked_div(1_000_000_000)
            .ok_or(InvalidExif)?
            .try_into()
            .map_err(|_| InvalidExif)?;
        let n = nanos
            .checked_rem(1_000_000_000)
            .ok_or(InvalidExif)?
            .try_into()
            .map_err(|_| InvalidExif)?;
        NaiveTime::from_num_seconds_from_midnight_opt(s, n).ok_or(InvalidExif)
    }

    fn parse_latitude_longitude(&self) -> Result<GpsDegrees, InvalidExif> {
        let [degrees, minutes, seconds] = self.parse_unsigned_rational_3()?;
        Ok(GpsDegrees(degrees, minutes, seconds))
    }

    fn parse_altitude(&self) -> Result<UnsignedRational, InvalidExif> {
        let ValueRef::UnsignedRational(altitude) = self.parse_value()? else {
            return Err(InvalidExif);
        };
        Ok(altitude)
    }

    fn parse_latitude_ref(&self) -> Result<LatitudeRef, InvalidExif> {
        let bytes = self.get_bytes().ok_or(InvalidExif)?;
        match bytes {
            [b'N', 0, ..] => Ok(LatitudeRef::North),
            [b'S', 0, ..] => Ok(LatitudeRef::South),
            _ => Err(InvalidExif),
        }
    }

    fn parse_longitude_ref(&self) -> Result<LongitudeRef, InvalidExif> {
        let bytes = self.get_bytes().ok_or(InvalidExif)?;
        match bytes {
            [b'E', 0, ..] => Ok(LongitudeRef::East),
            [b'W', 0, ..] => Ok(LongitudeRef::West),
            _ => Err(InvalidExif),
        }
    }

    fn parse_altitude_ref(&self) -> Result<AltitudeRef, InvalidExif> {
        let ValueRef::U8(byte) = self.parse_value()? else {
            return Err(InvalidExif);
        };
        match byte {
            0 => Ok(AltitudeRef::AboveEllipsoidalSurface),
            1 => Ok(AltitudeRef::BelowEllipsoidalSurface),
            2 => Ok(AltitudeRef::AboveSeaLevel),
            3 => Ok(AltitudeRef::BelowSeaLevel),
            _ => Err(InvalidExif),
        }
    }

    fn parse_speed_ref(&self) -> Result<SpeedRef, InvalidExif> {
        let bytes = self.get_bytes().ok_or(InvalidExif)?;
        match bytes {
            [b'K', 0, ..] => Ok(SpeedRef::KilometersPerHour),
            [b'M', 0, ..] => Ok(SpeedRef::MilesPerHour),
            [b'N', 0, ..] => Ok(SpeedRef::KnotsPerHour),
            _ => Err(InvalidExif),
        }
    }

    fn parse_distance_ref(&self) -> Result<DistanceRef, InvalidExif> {
        let bytes = self.get_bytes().ok_or(InvalidExif)?;
        match bytes {
            [b'K', 0, ..] => Ok(DistanceRef::Kilometers),
            [b'M', 0, ..] => Ok(DistanceRef::Miles),
            [b'N', 0, ..] => Ok(DistanceRef::Knots),
            _ => Err(InvalidExif),
        }
    }

    fn parse_direction(&self) -> Result<Direction, InvalidExif> {
        let bytes = self.get_bytes().ok_or(InvalidExif)?;
        match bytes {
            [b'T', 0, ..] => Ok(Direction::True),
            [b'M', 0, ..] => Ok(Direction::Magnetic),
            _ => Err(InvalidExif),
        }
    }

    fn parse_date_stamp(&self) -> Result<NaiveDate, InvalidExif> {
        let string = self.parse_str()?;
        NaiveDate::parse_from_str(string, "%Y:%m:%d").map_err(|_| InvalidExif)
    }
}
