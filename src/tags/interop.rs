use crate::InvalidExif;
use crate::ValueRef;
use crate::make_tag_enum_v2;

make_tag_enum_v2! {
    Tag "Interoperability tags"
    Entry "Interoperability entry"
    EntryMap "Interoperability entries"
    (Iter)
    ("Interoperability Identification" InteroperabilityIndex 1 (&'a str) parse_str)
}
