use std::sync::{Arc, Mutex};

use futures::channel::oneshot;

use crate::par::types::{PrimitiveType, Type, TypeDefs};

use super::{compiler::TypedTree, Net, PrimitiveComb, Tree};

pub struct Handle {
    type_defs: TypeDefs,
    net: Arc<Mutex<Net>>,
    tree: TypedTree,
}

pub enum Readback {
    Int(i128),
    IntRequest(Box<dyn Send + FnOnce(i128)>),

    Times(Handle, Handle),
    Par(Handle, Handle),
    Either(String, Handle),
    Choice(Vec<String>, Box<dyn Send + FnOnce(&str) -> Handle>),

    Break,
    Continue,
}

impl Handle {
    pub fn new(type_defs: TypeDefs, net: Arc<Mutex<Net>>, tree: TypedTree) -> Self {
        Self {
            type_defs,
            net,
            tree,
        }
    }

    pub fn net(&self) -> Arc<Mutex<Net>> {
        Arc::clone(&self.net)
    }

    pub async fn readback(mut self) -> Readback {
        self.prepare_for_readback();

        match &self.tree.ty {
            Type::Primitive(_, PrimitiveType::Int) => Readback::Int(self.int().await),

            typ @ Type::Chan(_, dual) => match dual.as_ref() {
                Type::Primitive(_, PrimitiveType::Int) => {
                    Readback::IntRequest(Box::new(move |value| self.provide_int(value)))
                }

                _ => panic!("Unsupported dual type for readback: {:?}", typ),
            },

            Type::Send(_, _, _) => {
                let (t_handle, u_handle) = self.receive();
                Readback::Times(t_handle, u_handle)
            }

            Type::Receive(_, _, _) => {
                let (t_handle, u_handle) = self.send();
                Readback::Par(t_handle, u_handle)
            }

            Type::Either(_, _) => {
                let (signal, handle) = self.case().await;
                Readback::Either(signal, handle)
            }

            Type::Choice(_, branches) => Readback::Choice(
                branches.keys().map(|k| k.string.clone()).collect(),
                Box::new(move |signal| self.signal(signal)),
            ),

            Type::Break(_) => {
                self.continue_();
                Readback::Break
            }

            Type::Continue(_) => {
                self.break_();
                Readback::Continue
            }

            typ => panic!("Unsupported type for readback: {:?}", typ),
        }
    }

    pub async fn int(mut self) -> i128 {
        self.prepare_for_readback();
        let Type::Primitive(_, PrimitiveType::Int) = self.tree.ty else {
            panic!("Incorrect type for `int`: {:?}", self.tree.ty);
        };

        let rx = {
            let (tx, rx) = oneshot::channel();
            let mut locked = self.net.lock().expect("lock failed");
            locked.link(Tree::IntRequest(tx), self.tree.tree);
            rx
        };

        rx.await.expect("sender dropped")
    }

    pub fn provide_int(mut self, value: i128) {
        self.prepare_for_readback();
        let Type::Chan(span, dual) = self.tree.ty else {
            panic!("Incorrect type for `int`: {:?}", self.tree.ty);
        };
        let Type::Primitive(_, PrimitiveType::Int) = *dual else {
            panic!("Incorrect type for `int`: {:?}", Type::Chan(span, dual));
        };

        let mut locked = self.net.lock().expect("lock failed");
        locked.link(Tree::Primitive(PrimitiveComb::Int(value)), self.tree.tree);
        drop(locked);
    }

    pub fn send(mut self) -> (Self, Self) {
        self.prepare_for_readback();
        let Type::Receive(_, t, u) = self.tree.ty else {
            panic!("Incorrect type for `send`: {:?}", self.tree.ty);
        };

        let mut locked = self.net.lock().expect("lock failed");
        let (t0, t1) = locked.create_wire();
        let (u0, u1) = locked.create_wire();
        locked.link(Tree::c(u1, t1), self.tree.tree);
        drop(locked);

        let t_handle = Self {
            type_defs: self.type_defs.clone(),
            net: Arc::clone(&self.net),
            tree: t0.with_type(t.dual(&self.type_defs).unwrap()),
        };
        let u_handle = Self {
            type_defs: self.type_defs,
            net: self.net,
            tree: u0.with_type(*u),
        };

        (t_handle, u_handle)
    }

    pub fn receive(mut self) -> (Self, Self) {
        self.prepare_for_readback();
        let Type::Send(_, t, u) = self.tree.ty else {
            panic!("Incorrect type for `receive`: {:?}", self.tree.ty);
        };

        let mut locked = self.net.lock().expect("lock failed");
        let (t0, t1) = locked.create_wire();
        let (u0, u1) = locked.create_wire();
        locked.link(Tree::c(u1, t1), self.tree.tree);
        drop(locked);

        let t_handle = Self {
            type_defs: self.type_defs.clone(),
            net: Arc::clone(&self.net),
            tree: t0.with_type(*t),
        };
        let u_handle = Self {
            type_defs: self.type_defs,
            net: self.net,
            tree: u0.with_type(*u),
        };

        (t_handle, u_handle)
    }

    pub fn signal(mut self, chosen: &str) -> Self {
        self.prepare_for_readback();
        let Type::Choice(_, branches) = self.tree.ty else {
            panic!("Incorrect type for `signal`: {:?}", self.tree.ty);
        };
        let size = branches.len() as u16;
        let (index, (_, typ)) = branches
            .into_iter()
            .enumerate()
            .find(|(_, (k, _))| k.string == chosen)
            .unwrap();
        let index = index as u16;

        let mut locked = self.net.lock().expect("lock failed");
        let (a0, a1) = locked.create_wire();
        locked.link(Tree::Signal(index, size, Box::new(a1)), self.tree.tree);
        drop(locked);

        Self {
            type_defs: self.type_defs,
            net: self.net,
            tree: a0.with_type(typ),
        }
    }

    pub async fn case(mut self) -> (String, Self) {
        self.prepare_for_readback();
        let Type::Either(_, branches) = self.tree.ty else {
            panic!("Incorrect type for `case`: {:?}", self.tree.ty);
        };

        let rx = {
            let (tx, rx) = oneshot::channel();
            let mut locked = self.net.lock().expect("lock failed");
            locked.link(Tree::SignalRequest(tx), self.tree.tree);
            rx
        };

        let (index, size, tree) = rx.await.expect("sender dropped");
        assert_eq!(branches.len(), size as usize);
        let (name, typ) = branches.into_iter().skip(index as usize).next().unwrap();

        let handle = Self {
            type_defs: self.type_defs,
            net: self.net,
            tree: tree.with_type(typ),
        };

        (name.string, handle)
    }

    pub fn break_(mut self) {
        self.prepare_for_readback();
        let Type::Continue(_) = self.tree.ty else {
            panic!("Incorrect type for `break`: {:?}", self.tree.ty);
        };

        let mut locked = self.net.lock().expect("lock failed");
        locked.link(Tree::Era, self.tree.tree);
        drop(locked);
    }

    pub fn continue_(mut self) {
        self.prepare_for_readback();
        let Type::Break(_) = self.tree.ty else {
            panic!("Incorrect type for `break`: {:?}", self.tree.ty);
        };

        let mut locked = self.net.lock().expect("lock failed");
        locked.link(Tree::Era, self.tree.tree);
        drop(locked);
    }
}

impl Handle {
    fn prepare_for_readback(&mut self) {
        self.tree.ty = expand_type(
            std::mem::replace(&mut self.tree.ty, Type::Break(Default::default())),
            &self.type_defs,
        );
    }
}

pub fn expand_type(typ: Type, type_defs: &TypeDefs) -> Type {
    let mut typ = typ;
    loop {
        typ = match typ {
            Type::Name(span, name, args) => type_defs.get(&span, &name, &args).unwrap(),
            Type::Recursive {
                span: _,
                asc,
                label,
                body,
            } => Type::expand_recursive(&asc, &label, &body, &type_defs).unwrap(),
            Type::Iterative {
                span: _,
                asc,
                label,
                body,
            } => Type::expand_iterative(&asc, &label, &body, &type_defs).unwrap(),
            typ => break typ,
        };
    }
}
