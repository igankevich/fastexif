#![no_std]
#![doc = include_str!("../README.md")]
#![cfg_attr(docsrs, feature(doc_cfg))]

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
