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
                    "Clamp",
                    Type::function(
                        Type::int(),
                        Type::function(Type::nat(), Type::function(Type::nat(), Type::nat())),
                    ),
                    |handle| Box::pin(nat_clamp(handle)),
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
                    "Clamp",
                    Type::function(
                        Type::int(),
                        Type::function(Type::int(), Type::function(Type::int(), Type::int())),
                    ),
                    |handle| Box::pin(int_clamp(handle)),
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
        "Char",
        Module {
            type_defs: vec![TypeDef::external("Char", &[], Type::char())],
            declarations: vec![],
            definitions: vec![Definition::external(
                "Code",
                Type::function(Type::char(), Type::nat()),
                |handle| Box::pin(char_code(handle)),
            )],
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
                Definition::external(
                    "Reader",
                    Type::function(Type::string(), Type::name(None, "Reader", vec![])),
                    |handle| Box::pin(string_reader(handle)),
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

async fn nat_clamp(mut handle: Handle) {
    let int = handle.receive().int().await;
    let min = handle.receive().nat().await;
    let max = handle.receive().nat().await;
    handle.provide_nat(int.min(max).max(min));
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

async fn int_clamp(mut handle: Handle) {
    let int = handle.receive().int().await;
    let min = handle.receive().int().await;
    let max = handle.receive().int().await;
    handle.provide_int(int.min(max).max(min));
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

async fn char_code(mut handle: Handle) {
    let c = handle.receive().char().await;
    handle.provide_nat(BigInt::from(c as u32))
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

async fn string_reader(mut handle: Handle) {
    let mut remainder = handle.receive().string().await;

    loop {
        if remainder.is_empty() {
            handle.signal(0, 2); // empty
            handle.break_();
            return;
        }

        handle.signal(1, 2); // some
        loop {
            match handle.case(4).await {
                0 => {
                    // char
                    let c = remainder.chars().next().unwrap();
                    handle.send().provide_char(c);
                    remainder = remainder.substr(c.len_utf8()..);
                    break;
                }
                1 => {
                    // close
                    handle.break_();
                    return;
                }
                2 => {
                    // match
                    let (_, mut cp) = Pattern::readback(handle.receive()).await.compile();
                    let mut last_accepted_index = None;
                    for (i, c) in remainder.char_indices() {
                        match cp(c) {
                            Some(true) => last_accepted_index = Some(i + c.len_utf8()),
                            Some(false) => {}
                            None => break,
                        }
                    }
                    match last_accepted_index {
                        Some(i) => {
                            handle.signal(1, 2); // match
                            handle.send().provide_string(remainder.substr(..i));
                            remainder = remainder.substr(i..);
                            break;
                        }
                        None => {
                            handle.signal(0, 2); // fail
                        }
                    }
                }
                3 => {
                    // remainder
                    handle.provide_string(remainder);
                    return;
                }
                _ => unreachable!(),
            }
        }
    }
}

type CompiledPattern = (bool, Box<dyn FnMut(char) -> Option<bool>>);

enum Pattern {
    Exact(Substr),
    Concat(Arc<Self>, Arc<Self>),
    And(Arc<Self>, Arc<Self>),
    Or(Arc<Self>, Arc<Self>),
    Repeat(Arc<Self>),
    Repeat1(Arc<Self>),
    Times(BigInt, Arc<Self>),
}

impl Pattern {
    async fn readback(mut handle: Handle) -> Self {
        match handle.case(7).await {
            0 => {
                // and
                let left = Box::pin(Self::readback(handle.receive())).await;
                let right = Box::pin(Self::readback(handle)).await;
                Self::And(Arc::new(left), Arc::new(right))
            }
            1 => {
                // concat
                let left = Box::pin(Self::readback(handle.receive())).await;
                let right = Box::pin(Self::readback(handle)).await;
                Self::Concat(Arc::new(left), Arc::new(right))
            }
            2 => {
                // exact
                let string = handle.string().await;
                Self::Exact(string)
            }
            3 => {
                // or
                let left = Box::pin(Self::readback(handle.receive())).await;
                let right = Box::pin(Self::readback(handle)).await;
                Self::Or(Arc::new(left), Arc::new(right))
            }
            4 => {
                // repeat
                let pat = Box::pin(Self::readback(handle)).await;
                Self::Repeat(Arc::new(pat))
            }
            5 => {
                // repeat1
                let pat = Box::pin(Self::readback(handle)).await;
                Self::Repeat1(Arc::new(pat))
            }
            6 => {
                // times
                let number = handle.receive().nat().await;
                let pat = Box::pin(Self::readback(handle)).await;
                Self::Times(number, Arc::new(pat))
            }
            _ => unreachable!(),
        }
    }

    fn compile(&self) -> CompiledPattern {
        match self {
            Self::Exact(string) => {
                let mut remaining = string.clone();
                (
                    remaining.is_empty(),
                    Box::new(move |c| {
                        let Some(d) = remaining.chars().next() else {
                            return None;
                        };
                        remaining = remaining.substr(d.len_utf8()..);
                        if remaining.is_empty() {
                            return Some(c == d);
                        };
                        if c != d {
                            remaining = Substr::from("");
                            return None;
                        }
                        Some(false)
                    }),
                )
            }

            Self::Concat(p1, p2) => {
                let (empty1, mut cp1) = p1.compile();
                let p2 = Arc::clone(p2);
                let mut cp2s = Vec::new();
                if empty1 {
                    cp2s.push(p2.compile());
                }
                (
                    empty1 && cp2s.iter().any(|&(empty2, _)| empty2),
                    Box::new(move |c| {
                        let mut to_remove = Vec::new();
                        for (i, (accept2, cp2)) in cp2s.iter_mut().enumerate() {
                            match cp2(c) {
                                Some(answer2) => *accept2 = answer2,
                                None => to_remove.push(i),
                            }
                        }
                        for i in to_remove.into_iter().rev() {
                            let _ = cp2s.swap_remove(i);
                        }
                        let answer1 = cp1(c);
                        if answer1 == Some(true) {
                            cp2s.push(p2.compile());
                        }
                        if answer1 == None && cp2s.is_empty() {
                            return None;
                        }
                        Some(cp2s.iter().any(|&(accept2, _)| accept2))
                    }),
                )
            }

            Self::And(p1, p2) => {
                let ((empty1, mut cp1), (empty2, mut cp2)) = (p1.compile(), p2.compile());
                (
                    empty1 && empty2,
                    Box::new(move |c| match (cp1(c), cp2(c)) {
                        (Some(accept1), Some(accept2)) => Some(accept1 && accept2),
                        (None, _) | (_, None) => None,
                    }),
                )
            }

            Self::Or(p1, p2) => {
                let ((empty1, mut cp1), (empty2, mut cp2)) = (p1.compile(), p2.compile());
                (
                    empty1 || empty2,
                    Box::new(move |c| match (cp1(c), cp2(c)) {
                        (Some(accept1), Some(accept2)) => Some(accept1 || accept2),
                        (Some(accept), None) | (None, Some(accept)) => Some(accept),
                        (None, None) => None,
                    }),
                )
            }

            Self::Repeat(p) => {
                let (_, cp) = Self::Repeat1(Arc::clone(p)).compile();
                (true, cp)
            }

            Self::Repeat1(p) => {
                let (empty, cp) = p.compile();
                let p = Arc::clone(p);
                let mut cps = vec![cp];
                (
                    empty,
                    Box::new(move |c| {
                        if cps.is_empty() {
                            return None;
                        }
                        let mut accept = false;
                        let mut new_cps = Vec::new();
                        let mut to_remove = Vec::new();
                        for (i, cp) in cps.iter_mut().enumerate() {
                            match cp(c) {
                                Some(true) => {
                                    accept = true;
                                    new_cps.push(p.compile().1);
                                }
                                Some(false) => {}
                                None => to_remove.push(i),
                            }
                        }
                        for i in to_remove.into_iter().rev() {
                            let _ = cps.swap_remove(i);
                        }
                        cps.extend(new_cps);
                        Some(accept)
                    }),
                )
            }

            Self::Times(n, p) => {
                let mut repeated = Self::Exact(Substr::from(""));
                let mut remaining = n.clone();
                while remaining > BigInt::ZERO {
                    repeated = Self::Concat(Arc::clone(p), Arc::new(repeated));
                    remaining -= 1;
                }
                repeated.compile()
            }
        }
    }
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
                // print
                println!("{}", handle.receive().string().await);
            }
            _ => unreachable!(),
        }
    }
}
