/// Parser options.
///
/// Can be set on per-entry basis.
#[derive(Clone, Copy)]
pub struct ParseOptions {
    pub(crate) ignore_unparsable_nested_entries: bool,
}

impl ParseOptions {
    /// Create default options.
    pub const fn new() -> Self {
        Self {
            ignore_unparsable_nested_entries: true,
        }
    }

    /// Ignore any entries that fail to parse. Default is `true`.
    ///
    /// Every time an entry is failed to parse a log message is printed with `Trace` level.
    pub const fn ignore_unparsable_nested_entries(mut self, value: bool) -> Self {
        self.ignore_unparsable_nested_entries = value;
        self
    }
}

impl Default for ParseOptions {
    fn default() -> Self {
        Self::new()
    }
}
