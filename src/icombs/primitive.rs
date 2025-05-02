use super::{Net, Tree};

#[derive(Debug, Clone)]
pub enum PrimitiveComb {
    Int(i128),
    IntAdd1,
}

impl PrimitiveComb {
    pub fn interact(self, net: &mut Net, tree: Tree) {
        match (self, tree) {
            (Self::Int(i), Tree::IntRequest(c)) => {
                c.send(i).expect("receiver dropped");
                net.rewrites.resp += 1;
            }

            (Self::Int(_), Tree::Era) => {
                net.rewrites.era += 1;
            }
            (Self::Int(i), Tree::Dup(a, b)) => {
                net.link(Tree::Primitive(Self::Int(i)), *a);
                net.link(Tree::Primitive(Self::Int(i)), *b);
                net.rewrites.commute += 1;
            }

            (Self::IntAdd1, Tree::Con(arg, res)) => match arg.as_ref() {
                Tree::Var(_) => {
                    net.link(Tree::Primitive(Self::IntAdd1), Tree::Con(arg, res));
                }
                Tree::Primitive(Self::Int(i)) => {
                    net.link(Tree::Primitive(Self::Int(i + 1)), *res);
                }
                _ => unreachable!("Invalid interaction with a primitive"),
            },

            _ => unreachable!("Invalid interaction with a primitive"),
        }
    }
}
