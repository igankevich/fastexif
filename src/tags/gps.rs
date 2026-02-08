use crate::InvalidExif;
use crate::UnsignedRational;
use crate::Value;
use crate::define_tag_enum;
use crate::define_value_enums;
use crate::define_value_str_enums;

use chrono::DateTime;
use chrono::NaiveDate;
use chrono::NaiveTime;
use chrono::Utc;

define_tag_enum! {
    Tag "GPS tag"
    Entry "GPS entry"
    EntryMap "GPS entries"
    (Iter)
    ("GPS tag version" VersionId 0 (&'a str) parse_str)
    ("North or south latitude" LatitudeRef 1 (LatitudeRef) parse_latitude_ref)
    ("Latitude" Latitude 2 (Degrees) parse_latitude_longitude)
    ("East or west longitude" LongitudeRef 3 (LongitudeRef) parse_longitude_ref)
    ("Longitude" Longitude 4 (Degrees) parse_latitude_longitude)
    ("Altitude reference" AltitudeRef 5 (AltitudeRef) parse_altitude_ref)
    ("Altitude in meters" Altitude 6 (UnsignedRational) parse_altitude)
    ("GPS time (atomic clock)" TimeStamp 7 (NaiveTime) parse_time_stamp)
    ("GPS satellites used for measurement" Satellites 8 (&'a str) parse_str)
    ("GPS receiver status" Status 9 (Status) parse_status)
    ("GPS measurement mode" MeasureMode 10 (MeasureMode) parse_measure_mode)
    ("Measurement precision (data degree of precision)" Dop 11 (UnsignedRational) parse_unsigned_rational)
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
    ("GPS differential correction" Differential 30 (Differential) parse_differential)
    ("Horizontal positioning error in meters" HPositioningError 31 (UnsignedRational) parse_unsigned_rational)
}

define_value_enums! {
    (AltitudeRef u8 "Reference altitude."
        (AboveEllipsoidalSurface 0 "Positive ellipsoidal height (at or above ellipsoidal surface)")
        (BelowEllipsoidalSurface 1 "Negative ellipsoidal height (below ellipsoidal surface)")
        (AboveSeaLevel 2 "Positive sea level value (at or above sea level reference)")
        (BelowSeaLevel 3 "Negative sea level value (below sea level reference)"))
    (Differential u16 "Differential correction"
        (NoCorrection 0 "Measurement without differential correction")
        (CorrectionApplied 1 "Differential correction applied"))
}

define_value_str_enums! {
    (LatitudeRef "Reference latitude."
        (North (b'N') "North")
        (South (b'S') "South"))
    (LongitudeRef "Reference longitude."
        (East (b'E') "East")
        (West (b'W') "West"))
    (Status "GPS receiver status."
        (InProgress (b'A') "Measurement in progress")
        (Interrupted (b'V') "Measurement interrupted"))
    (MeasureMode "Measurement mode."
        (TwoDimensional (b'2') "2-dimensional measurement")
        (ThreeDimensional (b'3') "3-dimensional measurement"))
    (SpeedRef "Speed unit."
        (KilometersPerHour (b'K') "Kilometers per hour")
        (MilesPerHour (b'M') "Miles per hour")
        (KnotsPerHour (b'N') "Knots"))
    (Direction "Direction"
        (True (b'T') "True direction")
        (Magnetic (b'M') "Magnetic direction"))
    (DistanceRef "Distance unit."
        (Kilometers (b'K') "Kilometers")
        (Miles (b'M') "Miles")
        (NauticalMiles (b'N') "Nautical miles"))
}

impl EntryMap<'_> {
    /// Returns atomic time obtained by GPS receiver.
    pub fn time(&self) -> Option<DateTime<Utc>> {
        let date = self.get_date_stamp()?;
        let time = self.get_time_stamp()?;
        Some(date.and_time(*time).and_utc())
    }

    /// Returns longitude in degrees.
    pub fn latitude(&self) -> Option<f64> {
        let latitude = self.get_latitude()?;
        let latitude_ref = self.get_latitude_ref()?;
        match latitude_ref {
            LatitudeRef::North => Some(latitude.as_f64()),
            LatitudeRef::South => Some(-latitude.as_f64()),
        }
    }

    /// Returns longitude in degrees.
    pub fn longitude(&self) -> Option<f64> {
        let longitude = self.get_longitude()?;
        let longitude_ref = self.get_longitude_ref()?;
        match longitude_ref {
            LongitudeRef::East => Some(longitude.as_f64()),
            LongitudeRef::West => Some(-longitude.as_f64()),
        }
    }

    /// Returns altitude in meters.
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

/// GPS location component specified as degrees.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
pub struct Degrees(UnsignedRational, UnsignedRational, UnsignedRational);

impl Degrees {
    /// Returns degrees.
    pub const fn degrees(&self) -> &UnsignedRational {
        &self.0
    }

    /// Returns minutes.
    pub const fn minutes(&self) -> &UnsignedRational {
        &self.1
    }

    /// Returns seconds.
    pub const fn seconds(&self) -> &UnsignedRational {
        &self.2
    }

    /// Returns fractional degress.
    pub fn as_f64(&self) -> f64 {
        self.degrees().as_f64()
            + self.minutes().as_f64() * (1.0 / 60.0)
            + self.seconds().as_f64() * (1.0 / 3600.0)
    }
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

    fn parse_latitude_longitude(&self) -> Result<Degrees, InvalidExif> {
        let [degrees, minutes, seconds] = self.parse_unsigned_rational_3()?;
        Ok(Degrees(degrees, minutes, seconds))
    }

    fn parse_altitude(&self) -> Result<UnsignedRational, InvalidExif> {
        let Value::UnsignedRational(altitude) = self.parse_value()? else {
            return Err(InvalidExif);
        };
        Ok(altitude)
    }

    fn parse_date_stamp(&self) -> Result<NaiveDate, InvalidExif> {
        let string = self.parse_str()?;
        NaiveDate::parse_from_str(string, "%Y:%m:%d").map_err(|_| InvalidExif)
    }
}
