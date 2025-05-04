use std::sync::Arc;

use super::{
    language::GlobalName,
    process,
    program::{Definition, Module, TypeDef},
    types::{PrimitiveType, Type},
};

pub fn import_builtins(module: &mut Module<Arc<process::Expression<()>>>) {
    module.import("Int", int_module());
}

pub fn int_module() -> Module<Arc<process::Expression<()>>> {
    Module {
        type_defs: vec![TypeDef {
            span: Default::default(),
            name: GlobalName {
                span: Default::default(),
                module: None,
                primary: String::from("Int"),
            },
            params: vec![],
            typ: Type::Primitive(Default::default(), PrimitiveType::Int),
        }],
        declarations: vec![],
        definitions: vec![Definition {
            span: Default::default(),
            name: GlobalName {
                span: Default::default(),
                module: None,
                primary: String::from("Add"),
            },
            expression: Arc::new(process::Expression::External(
                Type::Function(
                    Default::default(),
                    Box::new(Type::Primitive(Default::default(), PrimitiveType::Int)),
                    Box::new(Type::Function(
                        Default::default(),
                        Box::new(Type::Primitive(Default::default(), PrimitiveType::Int)),
                        Box::new(Type::Primitive(Default::default(), PrimitiveType::Int)),
                    )),
                ),
                |handle| {
                    Box::pin(async move {
                        let (x, handle) = handle.receive();
                        let (y, handle) = handle.receive();
                        let (x, y) = (x.int().await, y.int().await);
                        handle.provide_int(x + y);
                    })
                },
                (),
            )),
        }],
    }
}
