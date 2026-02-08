# fastexif

[![Crates.io Version](https://img.shields.io/crates/v/fastexif)](https://crates.io/crates/fastexif)
[![Docs](https://docs.rs/fastexif/badge.svg)](https://docs.rs/fastexif)
[![dependency status](https://deps.rs/repo/github/igankevich/fastexif/status.svg)](https://deps.rs/repo/github/igankevich/fastexif)

[Exif](https://en.wikipedia.org/wiki/Exif) metadata reader with convenience wrappers for GPS/time tags.

## Examples

### Read GPS location

```no_run,ignore
let data = ...;
let options = Default::default();
let tiff = fastexif::parse(&data)?.into_entry_map(options)?;
if let Some(gps) = tiff.get_gps_info() {
    let location = (gps.longitude(), gps.latitude(), gps.altitude());
    println!("GPS location: {location:?}");
}
```

### Convert Exif tags to JSON

```no_run,ignore
let data = ...;
let options = Default::default();
let tiff = fastexif::parse(&data)?.into_entry_map(options)?;
println!("{}", serde_json::to_string_pretty(&tiff)?);
```

### Read Apple-specific tags (experimental)

```no_run,ignore
use fastexif::exif::MakerNote;

let data = ...;
let options = Default::default();
let tiff = fastexif::parse(&data)?.into_entry_map(options)?;
if let Some(exif) = tiff.get_exif() {
    if let Some(MakerNote::Apple(apple)) = exif.get_maker_note() {
        // `Front` or `Back`.
        println!("Camera type: {:?}", apple.get_camera_type());
    }
}
```

