use std::sync::Arc;

use super::{
    language::GlobalName,
    process,
    program::{Definition, Module, TypeDef},
    types::Type,
};

pub fn import_builtins(module: &mut Module<Arc<process::Expression<()>>>) {
    module.import(
        "Nat",
        Module {
            type_defs: vec![TypeDef::external("Nat", Type::nat())],
            declarations: vec![],
            definitions: vec![
                Definition::external(
                    "Add",
                    Type::function(Type::nat(), Type::function(Type::nat(), Type::nat())),
                    |handle| {
                        Box::pin(async move {
                            let (x, handle) = handle.receive();
                            let (y, handle) = handle.receive();
                            let (x, y) = (x.int().await, y.int().await);
                            handle.provide_int(x + y);
                        })
                    },
                ),
                Definition::external(
                    "Max",
                    Type::function(Type::nat(), Type::function(Type::int(), Type::nat())),
                    |handle| {
                        Box::pin(async move {
                            let (x, handle) = handle.receive();
                            let (y, handle) = handle.receive();
                            let (x, y) = (x.nat().await, y.int().await);
                            handle.provide_int(x.max(y));
                        })
                    },
                ),
                Definition::external(
                    "Repeat",
                    Type::function(
                        Type::nat(),
                        Type::recursive(
                            None,
                            Type::either(vec![
                                ("end", Type::break_()),
                                ("step", Type::self_(None)),
                            ]),
                        ),
                    ),
                    |handle| {
                        Box::pin(async move {
                            let (n, mut handle) = handle.receive();
                            let mut n = n.nat().await;
                            while n > 0 {
                                handle = handle.signal(1, 2); // step
                                n -= 1;
                            }
                            handle = handle.signal(0, 2); // end
                            handle.break_();
                        })
                    },
                ),
            ],
        },
    );

    module.import(
        "Int",
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
        },
    );

    module.import(
        "String",
        Module {
            type_defs: vec![
                TypeDef::external("String", Type::string()),
                TypeDef::external(
                    "Builder",
                    Type::iterative(
                        None,
                        Type::choice(vec![
                            ("add", Type::function(Type::string(), Type::self_(None))),
                            ("build", Type::string()),
                        ]),
                    ),
                ),
            ],
            declarations: vec![],
            definitions: vec![Definition::external(
                "Builder",
                Type::name(GlobalName::external(None, "Builder"), vec![]),
                |mut handle| {
                    Box::pin(async move {
                        let mut buf = String::new();
                        loop {
                            let (i, handle1) = handle.case(2).await;
                            handle = handle1;
                            match i {
                                0 /* add */ => {
                                    let (s, handle1) = handle.receive();
                                    handle = handle1;
                                    buf += &s.string().await;
                                }
                                1 /* build */ => {
                                    handle.provide_string(Arc::from(buf));
                                    break;
                                }
                                _ => unreachable!(),
                            }
                        }
                    })
                },
            )],
        },
    );

    module.import(
        "Console",
        Module {
            type_defs: vec![],
            declarations: vec![],
            definitions: vec![Definition::external(
                "Log",
                Type::function(Type::string(), Type::break_()),
                |handle| {
                    Box::pin(async move {
                        let (s, handle) = handle.receive();
                        println!("{}", s.string().await);
                        handle.break_();
                    })
                },
            )],
        },
    )
}
