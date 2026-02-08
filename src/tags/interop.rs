use crate::InvalidExif;
use crate::Value;
use crate::define_tag_enum;
use crate::define_value_str_enums;

define_tag_enum! {
    Tag "Interoperability tag"
    Entry "Interoperability entry"
    EntryMap "Interoperability entries"
    (Iter)
    ("Interoperability Identification" InteroperabilityIndex 1 (InteroperabilityIndex) parse_interoperability_index)
}

define_value_str_enums! {
    (InteroperabilityIndex "Interoperability rule"
        (R98 (b'R' b'9' b'8') "Conforms to R98 file")
        (Thm (b'T' b'H' b'M') "Conforms to DCF thumbnail file")
        (R03 (b'R' b'0' b'3') "Conforms to DCF Option File"))
}
