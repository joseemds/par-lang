use std::sync::Arc;

use super::{
    process,
    program::{Definition, Module, TypeDef},
    types::Type,
};

pub fn import_builtins(module: &mut Module<Arc<process::Expression<()>>>) {
    module.import("Int", int_module());
}

pub fn int_module() -> Module<Arc<process::Expression<()>>> {
    Module {
        type_defs: vec![TypeDef::external("Int", Type::int())],
        declarations: vec![],
        definitions: vec![Definition::external(
            "Add",
            Type::function(Type::int(), Type::function(Type::int(), Type::int())),
            |handle| {
                Box::pin(async move {
                    let (x, handle) = handle.receive();
                    let (y, handle) = handle.receive();
                    let (x, y) = (x.int().await, y.int().await);
                    handle.provide_int(x + y);
                })
            },
        )],
    }
}
