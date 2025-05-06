use std::sync::Arc;

use arcstr::Substr;

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
                    |mut handle| {
                        Box::pin(async move {
                            let x = handle.receive().nat().await;
                            let y = handle.receive().nat().await;
                            handle.provide_int(x + y);
                        })
                    },
                ),
                Definition::external(
                    "Max",
                    Type::function(Type::nat(), Type::function(Type::int(), Type::nat())),
                    |mut handle| {
                        Box::pin(async move {
                            let x = handle.receive().nat().await;
                            let y = handle.receive().int().await;
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
                    |mut handle| {
                        Box::pin(async move {
                            let mut n = handle.receive().nat().await;
                            while n > 0 {
                                handle.signal(1, 2); // step
                                n -= 1;
                            }
                            handle.signal(0, 2); // end
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
                |mut handle| {
                    Box::pin(async move {
                        let x = handle.receive().int().await;
                        let y = handle.receive().int().await;
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
                            match handle.case(2).await {
                                0 => {
                                    // add
                                    buf += &handle.receive().string().await;
                                }
                                1 => {
                                    // build
                                    handle.provide_string(Substr::from(buf));
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
                |mut handle| {
                    Box::pin(async move {
                        println!("{}", handle.receive().string().await);
                        handle.break_();
                    })
                },
            )],
        },
    )
}
