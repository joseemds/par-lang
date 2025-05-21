# Construction Expressions

> **<sup>Syntax</sup>**\
> _Construction_ :\
> &nbsp;&nbsp; &nbsp;&nbsp; [_Unit_](#the-unit-expression) \
> &nbsp;&nbsp; | [_PairExpression_](#pair-expressions) \
> &nbsp;&nbsp; | [_FunctionExpression_](#function-expressions) \
> &nbsp;&nbsp; | [_EitherSelection_](#either-selections) \
> &nbsp;&nbsp; | [_ChoiceConstruction_](#choice-constructions) \
> &nbsp;&nbsp; | [_IterativeConstruction_](#iterative-constructions) \
> &nbsp;&nbsp; | [_Loop_](#iterative-constructions) \
> &nbsp;&nbsp; | [_ExistentialConstruction_](#existential-constructions) \
> &nbsp;&nbsp; | [_UniversalConstruction_](#universal-constructions)


## The Unit Expression

> **<sup>Syntax</sup>**\
> _Unit_ : `!`

*<sup>
[Type](../types.md#the-unit-type)
| [Pattern](../patterns.md#the-unit-pattern)
| [Statement](../statements/commands.md#the-break-command)
| [Destructing Statement](../statements/commands.md#the-continue-command)
</sup>*

The unit expression `!` is of the [unit type](../types.md#the-unit-type) `!`.

It's the only value of its type:
```par
def unit: ! = !
```

Unit expressions can be linked via:
```par
dual <> !
// is equivalent to
dual!
```


## Pair Expressions

> **<sup>Syntax</sup>**\
> _PairExpression_ : `(` [_ExpressionList_] `)` [_Expression_]

*<sup>
[Type](../types.md#pair-types)
| [Pattern](../patterns.md#pair-patterns)
| [Statement](../statements/commands.md#send-commands)
| [Destructing Statement](../statements/commands.md#receive-commands)
</sup>*

Having multiple expressions between `(` and `)` is just syntax sugar:
```par
(a, b) c
// is equivalent to
(a) (b) c
```


(Since generic are lowercases, I changed the name of variables, what would be other alternatives?)

If `x` is of type `a` and `y` is of type `b`, the pair expression `(x) y` is of the [pair type](../types.md#pair-types) `(a) b`.

```par
Def Bool_pair: (Bool, Bool)! = (.true!, .false!)!
```

The difference between representing a pair of `a` and `b` as `(a, b)!` or `(a) b` is:
- `(a1, ..., an)!`. is the default for regular tuples.
  ```par
  def Zero_to_two: (Nat, Nat, Nat)! = (
    .zero!,
    .succ.zero!,
    .succ.succ.zero!,
  )!
  ```

- `(a1, ..., an) b` more specifically means "send `A1`, ..., and `An`, then continue as `B`". It's used when all but the last member of a tuple should be received separately and the receiver should continue as the last one. For example:

Does not compile

```par
  def Length: [List] (Nat) List = [l] l.begin.case {
    .empty! => (.zero!) .empty!,
    .item(head) tail => do {
      // tail loop is of type (Nat) List
      tail.loop
      // receive len_pred 
      tail[len_pred]
      // tail is now as before
    } in (.succ len_pred) .item(head) tail
  }
```

Pair expressions can be linked via:
```par
dual <> (a) b
// is equivalent to
dual(a)
dual <> b
```


## Function Expressions

> **<sup>Syntax</sup>**\
> _FunctionExpression_ : `[` [_PatternList_] `]` [_Expression_]

*<sup>
[Type](../types.md#function-types)
| [Destructing Expression](application.md#function-calls)
| [Statement](../statements/commands.md#receive-commands)
| [Destructing Statement](../statements/commands.md#send-commands)
</sup>*

Having multiple patterns between `[` and `]` is just syntax sugar:
```par
[p, q] x
// is equivalent to
[p] [q] x
```

If `p` is an [irrefutable](patterns.md#irrefutable-note) pattern for type `A` and `b` (wich must use the bindings of `p`) is of type `B`, the function expression `[p] b` is of the [function type](../types.md#function-types) `[A] B`.

We've already seen a lot of functions, so here's a simple one:
```par
def Add2: [Nat] Nat = [n] .succ.succ n
```

Note that function expressions are the primary way of defining functions in par. Defining a function looks the same as defining any other value.

Function expressions can be linked via:
```par
dual <> [p] b
// is equivalent to
dual[p]
dual <> b
```

## Either Selections

> **<sup>Syntax</sup>**\
> _EitherSelection_ : [_Label_] [_Expression_]

*<sup>
[Type](../types.md#either-types)
| [Destructing Expression](application.md#either-destructions)
| [Statement](../statements/commands.md#signal-commands)
| [Destructing Statement](../statements/commands.md#match-commands)
</sup>*

The type of an either selection cannot be inferred from itself. \
A selection of the [either type](../types.md#either-types) `either { .a A, .b B }` is either `.a a` if `a` is of type `A` or `.b b` if `b` is of type `B`.

```par
type Bool = either {
  .true!,
  .false!,
}

def true: Bool = .true!
```
[Recursive types](../types.md#recursive-types) have no special construction syntax, instead they are finitely constructed as their underlying type. Most often they're seen as `recursive either` types:
```par
type Nat = recursive either {
  .zero!,
  .succ self,
}

// construct a value of the recursive
// Nat type like any other either type
def Two = .succ.succ.zero!
```

Either selections can be linked via:
```par
dual <> .a a
// is equivalent to
dual.a
dual <> a
```


## Choice Constructions

> **<sup>Syntax</sup>**\
> _ChoiceConstruction_ :\
> &nbsp;&nbsp; &nbsp;&nbsp; `{` ([_Label_] (`(` _ReceivePatterns_ `)`)<sup>\*</sup> `=>` _Expression_ `,`<sup>?</sup>)<sup>\*</sup> `}`
>
> _ReceivePatterns_ :\
> &nbsp;&nbsp; &nbsp;&nbsp; _PatternList_ \
> &nbsp;&nbsp; | `type` [_ID_List_]

*<sup>
[Type](../types.md#choice-types)
| [Destructing Expression](application.md#choice-selections)
| [Statement](../statements/commands.md#match-commands)
| [Destructing Statement](../statements/commands.md#signal-commands)
</sup>*

If `a` is of type `A` and `b` is of type `B`, the choice construction `{ .a => a, .b => b }` is of the [choice type](../types.md#choice-types) `{ .a => A, .b => B }`.

Some patterns can be used on the left side:
- `{ .a(p) => a }` is equivalent to `{ .a => [p] a }`
- `{ .a(type T) => a }` is equivalent to `{ .a => [type T] a }`

Choice constructions look very similar to [match expressions](application.md#match-expressions) (intentionally!).
```par
type BoolChoice = choice {
  .true => Bool,
  .false => Bool,
}

def negate: BoolChoice = case {
  .true => .false!,
  .false => .true!,
}
```

Choice constructions can be linked via:
```par
dual <> { .a => a, .b => b }
// is equivalent to
dual { 
  .a => { dual <> a }
  .b => { dual <> b }
}
```

## Iterative Constructions

> **<sup>Syntax</sup>**\
> _IterativeConstruction_ :\
> &nbsp;&nbsp; &nbsp;&nbsp; `begin` [_LoopLabel_]<sup>?</sup> [_Expression_]
>
> _Loop_ :\
> &nbsp;&nbsp; &nbsp;&nbsp; `loop` [_LoopLabel_]<sup>?</sup>

*<sup>
[Type](../types.md#iterative-types)
| [Statement](../statements/commands.md#recursive-commands)
</sup>*

If --- given every `loop` in `a` is of type `iterative A` --- `a` is of type `A`, the iterative construction `begin a` is of the [iterative type](../types.md#iterative-types) `iterative A`.

A `loop` corresponds to the innermost `begin` with the same loop label. `loop` without a label can only correspond to `begin` without a label.

Iterative types are constructed with `begin`-`loop` and as such, their values are often infinite. This is not a problem, however, as they don't have special syntax for destruction, i.e. they can only be finitely destructed.
```par
type Omega = iterative {
  .close => !,
  .next => self,
}

// This value is infinite:
// .next can be called as often
// as one desires
// (but only finitely many times)
def omega: Omega = begin {
  .close => !,
  .next => loop,
}
```

Iterative constructions can be linked via:
```par
dual <> begin a
// is equivalent to
dual begin
dual <> a // with loop in a replaced by begin a
```


## Existential Constructions

> **<sup>Syntax</sup>**\
> _ExistentialConstruction_ : `(` `type` [_TypeList_] `)` [_Expression_]

*<sup>
[Type](../types.md#existential-types)
| [Pattern](../patterns.md#existential-patterns)
| [Statement](../statements/commands.md#send-type-commands)
| [Destructing Statement](../statements/commands.md#receive-type-commands)
</sup>*

Having multiple types between `(` and `)` is just syntax sugar:
```par
(type a, b) c
// is equivalent to
(type a) (type b) c
```

If `a` is of type `A`, the existential construction `(type T) a` is of the [existential type](../types.md#existential-types) `(type T) A`.

```par
type Any = (type t) t

def Any_bool: Any = (type Bool) .true!
def Any_unit: Any = (type !) !
```

Existential constructions can be linked via:
```par
dual <> (type t) a
// is equivalent to
dual(type t)
dual <> a
```

## Universal Constructions

> **<sup>Syntax</sup>**\
> _UniversalConstruction_ : `[` `type` [_ID_List_] `]` [_Expression_]

*<sup>
[Type](../types.md#universal-types)
| [Destructing Expression](application.md#universal-specializations)
| [Statement](../statements/commands.md#receive-type-commands)
| [Destructing Statement](../statements/commands.md#send-type-commands)
</sup>*

Having multiple names between `[` and `]` is just syntax sugar:
```par
[type t, u] x
// is equivalent to
[type t] [type u] x
```

If `a` is of type `A`, the universal construction `[type T] a` (where `a` can use the type `T`) is of the [universal type](../types.md#universal-types) `[type T] A`.

Universal constructions are moslty used to define "generic functions":
```par
def Empty_list : [type t] List<t> = [type t] .empty!

// called via
let bools: List<Bool> = empty_list(type Bool)
```

Universal constructions can be linked via:
```par
dual <> [type t] a
// is equivalent to
dual[type t]
dual <> a
```

[_Expression_]: ../expressions.md
[_ExpressionList_]: ../expressions.md
[_PatternList_]: ../patterns.md
[_Label_]: ../types.md
[_ID_List_]: ../lexical.md#names
[_LoopLabel_]: ../statements/commands.md#recursive-commands
[_TypeList_]: ../types.md