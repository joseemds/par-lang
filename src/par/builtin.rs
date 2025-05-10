use std::{cmp::Ordering, sync::Arc};

use arcstr::Substr;
use num_bigint::BigInt;

use crate::icombs::readback::Handle;

use super::{
    process,
    program::{Definition, Module, TypeDef},
    types::Type,
};

pub fn import_builtins(module: &mut Module<Arc<process::Expression<()>>>) {
    module.import(
        "Bool",
        Module::parse_and_compile(include_str!("./builtin/Bool.par")).unwrap(),
    );
    module.import(
        "List",
        Module::parse_and_compile(include_str!("./builtin/List.par")).unwrap(),
    );
    module.import(
        "Ordering",
        Module::parse_and_compile(include_str!("./builtin/Ordering.par")).unwrap(),
    );
    module.import(
        "String",
        Module::parse_and_compile(include_str!("./builtin/String.par")).unwrap(),
    );
    module.import(
        "Console",
        Module::parse_and_compile(include_str!("./builtin/Console.par")).unwrap(),
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
                    |handle| Box::pin(nat_add(handle)),
                ),
                Definition::external(
                    "Mul",
                    Type::function(Type::nat(), Type::function(Type::nat(), Type::nat())),
                    |handle| Box::pin(nat_mul(handle)),
                ),
                Definition::external(
                    "Div",
                    Type::function(Type::nat(), Type::function(Type::nat(), Type::nat())),
                    |handle| Box::pin(nat_div(handle)),
                ),
                Definition::external(
                    "Mod",
                    Type::function(Type::nat(), Type::function(Type::nat(), Type::nat())),
                    |handle| Box::pin(nat_mod(handle)),
                ),
                Definition::external(
                    "Min",
                    Type::function(Type::nat(), Type::function(Type::nat(), Type::nat())),
                    |handle| Box::pin(nat_min(handle)),
                ),
                Definition::external(
                    "Max",
                    Type::function(Type::nat(), Type::function(Type::int(), Type::nat())),
                    |handle| Box::pin(nat_max(handle)),
                ),
                Definition::external(
                    "Equals",
                    Type::function(
                        Type::nat(),
                        Type::function(Type::nat(), Type::name(Some("Bool"), "Bool", vec![])),
                    ),
                    |handle| Box::pin(nat_equals(handle)),
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
                    |handle| Box::pin(nat_compare(handle)),
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
                    |handle| Box::pin(nat_repeat(handle)),
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
                    |handle| Box::pin(nat_range(handle)),
                ),
                Definition::external(
                    "ToString",
                    Type::function(Type::nat(), Type::string()),
                    |handle| Box::pin(nat_to_string(handle)),
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
                    |handle| Box::pin(int_add(handle)),
                ),
                Definition::external(
                    "Sub",
                    Type::function(Type::int(), Type::function(Type::int(), Type::int())),
                    |handle| Box::pin(int_sub(handle)),
                ),
                Definition::external(
                    "Mul",
                    Type::function(Type::int(), Type::function(Type::int(), Type::int())),
                    |handle| Box::pin(int_mul(handle)),
                ),
                Definition::external(
                    "Div",
                    Type::function(Type::int(), Type::function(Type::int(), Type::int())),
                    |handle| Box::pin(int_div(handle)),
                ),
                Definition::external(
                    "Mod",
                    Type::function(Type::int(), Type::function(Type::nat(), Type::nat())),
                    |handle| Box::pin(int_mod(handle)),
                ),
                Definition::external(
                    "Min",
                    Type::function(Type::int(), Type::function(Type::int(), Type::int())),
                    |handle| Box::pin(int_min(handle)),
                ),
                Definition::external(
                    "Max",
                    Type::function(Type::int(), Type::function(Type::int(), Type::int())),
                    |handle| Box::pin(int_max(handle)),
                ),
                Definition::external(
                    "Equals",
                    Type::function(
                        Type::int(),
                        Type::function(Type::int(), Type::name(Some("Bool"), "Bool", vec![])),
                    ),
                    |handle| Box::pin(int_equals(handle)),
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
                    |handle| Box::pin(int_compare(handle)),
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
                    |handle| Box::pin(int_range(handle)),
                ),
                Definition::external(
                    "ToString",
                    Type::function(Type::int(), Type::string()),
                    |handle| Box::pin(int_to_string(handle)),
                ),
            ],
        },
    );

    module.import(
        "String",
        Module {
            type_defs: vec![TypeDef::external("String", &[], Type::string())],
            declarations: vec![],
            definitions: vec![
                Definition::external("Builder", Type::name(None, "Builder", vec![]), |handle| {
                    Box::pin(string_builder(handle))
                }),
                Definition::external(
                    "SplitAt",
                    Type::function(
                        Type::string(),
                        Type::function(Type::int(), Type::pair(Type::string(), Type::string())),
                    ),
                    |handle| Box::pin(string_split_at(handle)),
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
                |handle| Box::pin(debug_log(handle)),
            )],
        },
    );

    module.import(
        "Console",
        Module {
            type_defs: vec![],
            declarations: vec![],
            definitions: vec![Definition::external(
                "Open",
                Type::name(None, "Console", vec![]),
                |handle| Box::pin(console_open(handle)),
            )],
        },
    )
}

async fn nat_add(mut handle: Handle) {
    let x = handle.receive().nat().await;
    let y = handle.receive().nat().await;
    handle.provide_nat(x + y);
}

async fn nat_mul(mut handle: Handle) {
    let x = handle.receive().nat().await;
    let y = handle.receive().nat().await;
    handle.provide_nat(x * y);
}

async fn nat_div(mut handle: Handle) {
    let x = handle.receive().nat().await;
    let y = handle.receive().nat().await;
    handle.provide_nat(if y == BigInt::ZERO {
        BigInt::ZERO
    } else {
        x / y
    });
}

async fn nat_mod(mut handle: Handle) {
    let x = handle.receive().nat().await;
    let y = handle.receive().nat().await;
    handle.provide_nat(if y == BigInt::ZERO {
        BigInt::ZERO
    } else {
        x % y
    });
}

async fn nat_min(mut handle: Handle) {
    let x = handle.receive().nat().await;
    let y = handle.receive().nat().await;
    handle.provide_nat(x.min(y));
}

async fn nat_max(mut handle: Handle) {
    let x = handle.receive().nat().await;
    let y = handle.receive().int().await;
    handle.provide_nat(x.max(y));
}

async fn nat_equals(mut handle: Handle) {
    let x = handle.receive().nat().await;
    let y = handle.receive().nat().await;
    if x == y {
        handle.signal(1, 2); // true
    } else {
        handle.signal(0, 2); // false
    }
    handle.break_();
}

async fn nat_compare(mut handle: Handle) {
    let x = handle.receive().nat().await;
    let y = handle.receive().nat().await;
    match x.cmp(&y) {
        Ordering::Equal => handle.signal(0, 3),
        Ordering::Greater => handle.signal(1, 3),
        Ordering::Less => handle.signal(2, 3),
    }
    handle.break_();
}

async fn nat_repeat(mut handle: Handle) {
    let mut n = handle.receive().nat().await;
    while n > BigInt::ZERO {
        handle.signal(1, 2); // step
        n -= 1;
    }
    handle.signal(0, 2); // end
    handle.break_();
}

async fn nat_range(mut handle: Handle) {
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
}

async fn nat_to_string(mut handle: Handle) {
    let x = handle.receive().nat().await;
    handle.provide_string(Substr::from(x.to_str_radix(10)))
}

async fn int_add(mut handle: Handle) {
    let x = handle.receive().int().await;
    let y = handle.receive().int().await;
    handle.provide_int(x + y);
}

async fn int_sub(mut handle: Handle) {
    let x = handle.receive().int().await;
    let y = handle.receive().int().await;
    handle.provide_int(x - y);
}

async fn int_mul(mut handle: Handle) {
    let x = handle.receive().int().await;
    let y = handle.receive().int().await;
    handle.provide_int(x * y);
}

async fn int_div(mut handle: Handle) {
    let x = handle.receive().int().await;
    let y = handle.receive().int().await;
    handle.provide_int(if y == BigInt::ZERO {
        BigInt::ZERO
    } else {
        x / y
    });
}

async fn int_mod(mut handle: Handle) {
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
}

async fn int_min(mut handle: Handle) {
    let x = handle.receive().int().await;
    let y = handle.receive().int().await;
    handle.provide_int(x.min(y));
}

async fn int_max(mut handle: Handle) {
    let x = handle.receive().int().await;
    let y = handle.receive().int().await;
    handle.provide_int(x.max(y));
}

async fn int_equals(mut handle: Handle) {
    let x = handle.receive().int().await;
    let y = handle.receive().int().await;
    if x == y {
        handle.signal(1, 2); // true
    } else {
        handle.signal(0, 2); // false
    }
    handle.break_();
}

async fn int_compare(mut handle: Handle) {
    let x = handle.receive().int().await;
    let y = handle.receive().int().await;
    match x.cmp(&y) {
        Ordering::Equal => handle.signal(0, 3),
        Ordering::Greater => handle.signal(1, 3),
        Ordering::Less => handle.signal(2, 3),
    }
    handle.break_();
}

async fn int_range(mut handle: Handle) {
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
}

async fn int_to_string(mut handle: Handle) {
    let x = handle.receive().int().await;
    handle.provide_string(Substr::from(x.to_str_radix(10)))
}

async fn string_builder(mut handle: Handle) {
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
}

async fn string_split_at(mut handle: Handle) {
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
    let (left, right) = match string.as_str().char_indices().skip(char_index).next() {
        Some((byte_index, _)) => (string.substr(..byte_index), string.substr(byte_index..)),
        None => (string, Substr::from("")),
    };

    handle.send().provide_string(left);
    handle.provide_string(right);
}

async fn debug_log(mut handle: Handle) {
    println!("{}", handle.receive().string().await);
    handle.break_();
}

async fn console_open(mut handle: Handle) {
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
}
