/// Unsigned rational number.
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct UnsignedRational(pub u32, pub u32);

impl UnsignedRational {
    /// Convert to [`f64`].
    pub const fn as_f64(self) -> f64 {
        self.0 as f64 / self.1 as f64
    }
}

impl core::fmt::Display for UnsignedRational {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        if self.1 == 1 {
            write!(f, "{}", self.0)
        } else {
            write!(f, "{}/{}", self.0, self.1)
        }
    }
}

impl core::fmt::Debug for UnsignedRational {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        core::fmt::Display::fmt(self, f)
    }
}

/// Signed rational number.
#[cfg_attr(feature = "serde", derive(serde::Serialize))]
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SignedRational(pub i32, pub i32);

impl SignedRational {
    /// Convert to [`f64`].
    pub const fn as_f64(self) -> f64 {
        self.0 as f64 / self.1 as f64
    }
}

impl core::fmt::Display for SignedRational {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        if self.1 == 1 {
            write!(f, "{}", self.0)
        } else {
            write!(f, "{}/{}", self.0, self.1)
        }
    }
}

impl core::fmt::Debug for SignedRational {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        if self.1 == 1 {
            write!(f, "{}", self.0)
        } else {
            write!(f, "{}/{}", self.0, self.1)
        }
    }
}
