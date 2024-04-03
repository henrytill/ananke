use crate::data::{Description, Entry, Id};

#[derive(Debug, PartialEq, Eq)]
pub enum Target {
    Id(Id),
    Description(Description),
}

impl Target {
    pub fn matches(&self, entry: &Entry) -> bool {
        match self {
            Target::Id(id) if *id == entry.id => true,
            Target::Description(d) if *d == entry.description => true,
            _ => false,
        }
    }
}
