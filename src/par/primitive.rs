use std::fmt::{self, Write};

use super::types::{PrimitiveType, Type};

#[derive(Clone, Debug)]
pub enum Primitive {
    Int(i128),
    IntAdd1,
}

impl Primitive {
    pub fn pretty(&self, f: &mut impl Write, _indent: usize) -> fmt::Result {
        match self {
            Self::Int(i) => write!(f, "{}", i),
            Self::IntAdd1 => write!(f, "Int.add1"),
        }
    }

    pub fn get_type(&self) -> Type {
        match self {
            Self::Int(_) => Type::Primitive(Default::default(), PrimitiveType::Int),

            Self::IntAdd1 => Type::Receive(
                Default::default(),
                Box::new(Type::Primitive(Default::default(), PrimitiveType::Int)),
                Box::new(Type::Primitive(Default::default(), PrimitiveType::Int)),
            ),
        }
    }
}
