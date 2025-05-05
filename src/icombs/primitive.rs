use std::sync::Arc;

use super::{Net, Tree};

#[derive(Debug, Clone)]
pub enum PrimitiveComb {
    Int(i128),
    String(Arc<str>),
}

impl PrimitiveComb {
    pub fn interact(self, net: &mut Net, tree: Tree) {
        match (self, tree) {
            (Self::Int(i), Tree::IntRequest(c)) => {
                c.send(i).expect("receiver dropped");
                net.rewrites.resp += 1;
            }
            (Self::String(s), Tree::StringRequest(c)) => {
                c.send(s).expect("receiver dropped");
                net.rewrites.resp += 1;
            }

            (_, Tree::Era) => {
                net.rewrites.era += 1;
            }
            (p, Tree::Dup(a, b)) => {
                net.link(Tree::Primitive(p.clone()), *a);
                net.link(Tree::Primitive(p), *b);
                net.rewrites.commute += 1;
            }

            _ => unreachable!("Invalid interaction with a primitive"),
        }
    }
}
