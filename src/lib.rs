#![no_std]
#![doc = include_str!("../README.md")]
#![cfg_attr(docsrs, feature(doc_cfg))]

//! ## Tags reference
//!
//! Below you can find high-level API for accessing tags.
//! For each tag there is a `get_*` method that returns the associated value wrapped in a type.
//! Tags are stored in several nested tables; the following nested list reflects the overall structure.
//!
//! - [TIFF](tiff::EntryMap)
//!   - [Exif](exif::EntryMap)
//!     - [GPS](gps::EntryMap)
//!     - [Interoperability](interop::EntryMap)
//!     - [Apple](apple::EntryMap)
//!
//! Full description of each tag is in Exif standard that you can download from
//! [cipa.jp](https://cipa.jp/e/std/std-sec.html).

extern crate alloc;

#[cfg(feature = "std")]
extern crate std;

mod endian;
mod error;
mod format;
mod maker;
mod options;
mod rational;
mod tags;

use self::endian::*;
use self::format::*;

pub use self::error::*;
pub use self::maker::*;
pub use self::options::*;
pub use self::rational::*;
pub use self::tags::*;
