use std::{cmp::Ordering, sync::Arc};

use arcstr::Substr;
use num_bigint::BigInt;

use super::{
    process,
    program::{Definition, Module, TypeDef},
    types::Type,
};

pub fn import_builtins(module: &mut Module<Arc<process::Expression<()>>>) {
    module.import(
        "Bool",
        Module {
            type_defs: vec![TypeDef::external(
                "Bool",
                &[],
                Type::either(vec![("false", Type::break_()), ("true", Type::break_())]),
            )],
            declarations: vec![],
            definitions: vec![],
        },
    );

    module.import(
        "List",
        Module {
            type_defs: vec![TypeDef::external(
                "List",
                &["a"],
                Type::recursive(
                    None,
                    Type::either(vec![
                        ("empty", Type::break_()),
                        ("item", Type::pair(Type::var("a"), Type::self_(None))),
                    ]),
                ),
            )],
            declarations: vec![],
            definitions: vec![],
        },
    );

    module.import(
        "Ordering",
        Module {
            type_defs: vec![TypeDef::external(
                "Ordering",
                &[],
                Type::either(vec![
                    ("equal", Type::break_()),
                    ("greater", Type::break_()),
                    ("less", Type::break_()),
                ]),
            )],
            declarations: vec![],
            definitions: vec![],
        },
    );

    module.import(
        "Nat",
        Module {
            type_defs: vec![TypeDef::external("Nat", &[], Type::nat())],
            declarations: vec![],
            definitions: vec![
                Definition::external(
                    "Add",
                    Type::function(Type::nat(), Type::function(Type::nat(), Type::nat())),
                    |mut handle| {
                        Box::pin(async move {
                            let x = handle.receive().nat().await;
                            let y = handle.receive().nat().await;
                            handle.provide_nat(x + y);
                        })
                    },
                ),
                Definition::external(
                    "Mul",
                    Type::function(Type::nat(), Type::function(Type::nat(), Type::nat())),
                    |mut handle| {
                        Box::pin(async move {
                            let x = handle.receive().nat().await;
                            let y = handle.receive().nat().await;
                            handle.provide_nat(x * y);
                        })
                    },
                ),
                Definition::external(
                    "Div",
                    Type::function(Type::nat(), Type::function(Type::nat(), Type::nat())),
                    |mut handle| {
                        Box::pin(async move {
                            let x = handle.receive().nat().await;
                            let y = handle.receive().nat().await;
                            handle.provide_nat(if y == BigInt::ZERO {
                                BigInt::ZERO
                            } else {
                                x / y
                            });
                        })
                    },
                ),
                Definition::external(
                    "Mod",
                    Type::function(Type::nat(), Type::function(Type::nat(), Type::nat())),
                    |mut handle| {
                        Box::pin(async move {
                            let x = handle.receive().nat().await;
                            let y = handle.receive().nat().await;
                            handle.provide_nat(if y == BigInt::ZERO {
                                BigInt::ZERO
                            } else {
                                x % y
                            });
                        })
                    },
                ),
                Definition::external(
                    "Min",
                    Type::function(Type::nat(), Type::function(Type::nat(), Type::nat())),
                    |mut handle| {
                        Box::pin(async move {
                            let x = handle.receive().nat().await;
                            let y = handle.receive().nat().await;
                            handle.provide_nat(x.min(y));
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
                            handle.provide_nat(x.max(y));
                        })
                    },
                ),
                Definition::external(
                    "Equals",
                    Type::function(
                        Type::nat(),
                        Type::function(Type::nat(), Type::name(Some("Bool"), "Bool", vec![])),
                    ),
                    |mut handle| {
                        Box::pin(async move {
                            let x = handle.receive().nat().await;
                            let y = handle.receive().nat().await;
                            if x == y {
                                handle.signal(1, 2); // true
                            } else {
                                handle.signal(0, 2); // false
                            }
                            handle.break_();
                        })
                    },
                ),
                Definition::external(
                    "Compare",
                    Type::function(
                        Type::nat(),
                        Type::function(
                            Type::nat(),
                            Type::name(Some("Ordering"), "Ordering", vec![]),
                        ),
                    ),
                    |mut handle| {
                        Box::pin(async move {
                            let x = handle.receive().nat().await;
                            let y = handle.receive().nat().await;
                            match x.cmp(&y) {
                                std::cmp::Ordering::Equal => handle.signal(0, 3),
                                std::cmp::Ordering::Greater => handle.signal(1, 3),
                                std::cmp::Ordering::Less => handle.signal(2, 3),
                            }
                            handle.break_();
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
                            while n > BigInt::ZERO {
                                handle.signal(1, 2); // step
                                n -= 1;
                            }
                            handle.signal(0, 2); // end
                            handle.break_();
                        })
                    },
                ),
                Definition::external(
                    "Range",
                    Type::function(
                        Type::nat(),
                        Type::function(
                            Type::nat(),
                            Type::name(Some("List"), "List", vec![Type::nat()]),
                        ),
                    ),
                    |mut handle| {
                        Box::pin(async move {
                            let lo = handle.receive().nat().await;
                            let hi = handle.receive().nat().await;

                            let mut i = lo;
                            while i < hi {
                                handle.signal(1, 2); // item
                                handle.send().provide_nat(i.clone());
                                i += 1;
                            }
                            handle.signal(0, 2); // empty
                            handle.break_();
                        })
                    },
                ),
                Definition::external(
                    "ToString",
                    Type::function(Type::nat(), Type::string()),
                    |mut handle| {
                        Box::pin(async move {
                            let x = handle.receive().nat().await;
                            handle.provide_string(Substr::from(x.to_str_radix(10)))
                        })
                    },
                ),
            ],
        },
    );

    module.import(
        "Int",
        Module {
            type_defs: vec![TypeDef::external("Int", &[], Type::int())],
            declarations: vec![],
            definitions: vec![
                Definition::external(
                    "Add",
                    Type::function(Type::int(), Type::function(Type::int(), Type::int())),
                    |mut handle| {
                        Box::pin(async move {
                            let x = handle.receive().int().await;
                            let y = handle.receive().int().await;
                            handle.provide_int(x + y);
                        })
                    },
                ),
                Definition::external(
                    "Sub",
                    Type::function(Type::int(), Type::function(Type::int(), Type::int())),
                    |mut handle| {
                        Box::pin(async move {
                            let x = handle.receive().int().await;
                            let y = handle.receive().int().await;
                            handle.provide_int(x - y);
                        })
                    },
                ),
                Definition::external(
                    "Mul",
                    Type::function(Type::int(), Type::function(Type::int(), Type::int())),
                    |mut handle| {
                        Box::pin(async move {
                            let x = handle.receive().int().await;
                            let y = handle.receive().int().await;
                            handle.provide_int(x * y);
                        })
                    },
                ),
                Definition::external(
                    "Div",
                    Type::function(Type::int(), Type::function(Type::int(), Type::int())),
                    |mut handle| {
                        Box::pin(async move {
                            let x = handle.receive().int().await;
                            let y = handle.receive().int().await;
                            handle.provide_int(if y == BigInt::ZERO {
                                BigInt::ZERO
                            } else {
                                x / y
                            });
                        })
                    },
                ),
                Definition::external(
                    "Mod",
                    Type::function(Type::int(), Type::function(Type::nat(), Type::nat())),
                    |mut handle| {
                        Box::pin(async move {
                            let x = handle.receive().int().await;
                            let y = handle.receive().nat().await;
                            if y == BigInt::ZERO {
                                handle.provide_nat(BigInt::ZERO);
                            } else if x < BigInt::ZERO {
                                let rem = x % y.clone();
                                handle.provide_nat(if rem == BigInt::ZERO {
                                    BigInt::ZERO
                                } else {
                                    y.clone() + rem
                                });
                            } else {
                                handle.provide_nat(x % y);
                            }
                        })
                    },
                ),
                Definition::external(
                    "Min",
                    Type::function(Type::int(), Type::function(Type::int(), Type::int())),
                    |mut handle| {
                        Box::pin(async move {
                            let x = handle.receive().int().await;
                            let y = handle.receive().int().await;
                            handle.provide_int(x.min(y));
                        })
                    },
                ),
                Definition::external(
                    "Max",
                    Type::function(Type::int(), Type::function(Type::int(), Type::int())),
                    |mut handle| {
                        Box::pin(async move {
                            let x = handle.receive().int().await;
                            let y = handle.receive().int().await;
                            handle.provide_int(x.max(y));
                        })
                    },
                ),
                Definition::external(
                    "Equals",
                    Type::function(
                        Type::int(),
                        Type::function(Type::int(), Type::name(Some("Bool"), "Bool", vec![])),
                    ),
                    |mut handle| {
                        Box::pin(async move {
                            let x = handle.receive().int().await;
                            let y = handle.receive().int().await;
                            if x == y {
                                handle.signal(1, 2); // true
                            } else {
                                handle.signal(0, 2); // false
                            }
                            handle.break_();
                        })
                    },
                ),
                Definition::external(
                    "Compare",
                    Type::function(
                        Type::int(),
                        Type::function(
                            Type::int(),
                            Type::name(Some("Ordering"), "Ordering", vec![]),
                        ),
                    ),
                    |mut handle| {
                        Box::pin(async move {
                            let x = handle.receive().int().await;
                            let y = handle.receive().int().await;
                            match x.cmp(&y) {
                                Ordering::Equal => handle.signal(0, 3),
                                Ordering::Greater => handle.signal(1, 3),
                                Ordering::Less => handle.signal(2, 3),
                            }
                            handle.break_();
                        })
                    },
                ),
                Definition::external(
                    "Range",
                    Type::function(
                        Type::int(),
                        Type::function(
                            Type::int(),
                            Type::name(Some("List"), "List", vec![Type::int()]),
                        ),
                    ),
                    |mut handle| {
                        Box::pin(async move {
                            let lo = handle.receive().int().await;
                            let hi = handle.receive().int().await;

                            let mut i = lo;
                            while i < hi {
                                handle.signal(1, 2); // item
                                handle.send().provide_int(i.clone());
                                i += 1;
                            }
                            handle.signal(0, 2); // empty
                            handle.break_();
                        })
                    },
                ),
                Definition::external(
                    "ToString",
                    Type::function(Type::int(), Type::string()),
                    |mut handle| {
                        Box::pin(async move {
                            let x = handle.receive().int().await;
                            handle.provide_string(Substr::from(x.to_str_radix(10)))
                        })
                    },
                ),
            ],
        },
    );

    module.import(
        "String",
        Module {
            type_defs: vec![
                TypeDef::external("String", &[], Type::string()),
                TypeDef::external(
                    "Builder",
                    &[],
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
            definitions: vec![
                Definition::external(
                    "Builder",
                    Type::name(None, "Builder", vec![]),
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
                ),
                Definition::external(
                    "SplitAt",
                    Type::function(
                        Type::string(),
                        Type::function(Type::int(), Type::pair(Type::string(), Type::string())),
                    ),
                    |mut handle| {
                        Box::pin(async move {
                            let string = handle.receive().string().await;
                            let char_index = handle.receive().int().await;

                            if char_index <= BigInt::ZERO {
                                handle.send().provide_string(Substr::from(""));
                                handle.provide_string(string);
                                return;
                            }
                            if char_index > BigInt::from(u32::MAX) {
                                handle.send().provide_string(string);
                                handle.provide_string(Substr::from(""));
                                return;
                            }

                            let char_index = char_index.iter_u32_digits().next().unwrap() as usize;
                            let (left, right) =
                                match string.as_str().char_indices().skip(char_index).next() {
                                    Some((byte_index, _)) => {
                                        (string.substr(..byte_index), string.substr(byte_index..))
                                    }
                                    None => (string, Substr::from("")),
                                };

                            handle.send().provide_string(left);
                            handle.provide_string(right);
                        })
                    },
                ),
            ],
        },
    );

    module.import(
        "Debug",
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
    );

    module.import(
        "Console",
        Module {
            type_defs: vec![TypeDef::external(
                "Console",
                &[],
                Type::iterative(
                    None,
                    Type::choice(vec![
                        ("close", Type::break_()),
                        (
                            "printLine",
                            Type::function(Type::string(), Type::self_(None)),
                        ),
                    ]),
                ),
            )],
            declarations: vec![],
            definitions: vec![Definition::external(
                "Open",
                Type::name(None, "Console", vec![]),
                |mut handle| {
                    Box::pin(async move {
                        loop {
                            match handle.case(2).await {
                                0 => {
                                    // close
                                    handle.break_();
                                    break;
                                }
                                1 => {
                                    // printLine
                                    println!("{}", handle.receive().string().await);
                                }
                                _ => unreachable!(),
                            }
                        }
                    })
                },
            )],
        },
    )
}
