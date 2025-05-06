use std::fmt::{self, Write};

use arcstr::Substr;

use super::types::Type;

#[derive(Clone, Debug)]
pub enum Primitive {
    Int(i128),
    String(Substr),
}

impl Primitive {
    pub fn pretty(&self, f: &mut impl Write, _indent: usize) -> fmt::Result {
        match self {
            Self::Int(i) => write!(f, "{}", i),
            Self::String(s) => write!(f, "{:?}", s),
        }
    }

    pub fn get_type(&self) -> Type {
        match self {
            Self::Int(n) if *n >= 0 => Type::nat(),
            Self::Int(_) => Type::int(),
            Self::String(_) => Type::string(),
        }
    }
}
