use crate::InvalidExif;
use crate::ValueRef;
use crate::make_tag_enum_v2;
use crate::parse_str_enum;

make_tag_enum_v2! {
    Tag "Interoperability tags"
    Entry "Interoperability entry"
    EntryMap "Interoperability entries"
    (Iter)
    ("Interoperability Identification" InteroperabilityIndex 1 (InteroperabilityIndex) parse_interoperability_index)
}

parse_str_enum! {
    InteroperabilityIndex
    (R98 (b'R' b'9' b'8'))
    (Thm (b'T' b'H' b'M'))
    (R03 (b'R' b'0' b'3'))
}
