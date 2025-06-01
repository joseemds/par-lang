# Either

_Either types_ are the well-known [sum types](https://en.wikipedia.org/wiki/Tagged_union),
otherwise known as _tagged unions_.

They defined a finite number of variants, each with a different name and a payload. A value of
an either type is one of its variants.

```par
type StringOrNumber = either {
  .string String,
  .number Int,
}

def Str: StringOrNumber = .string "Hello!"
def Num: StringOrNumber = .number 42,
```

An either type is spelled with the keyword `either`, followed by curly braces enclosing a
comma-separated list of variants.

Each variant has a lower-case name prefixed by a period and followed by a single, obligatory
payload type:

```par
either {
  .variant1 Payload1,
  .variant2 Payload2,
  .variant3 Payload3,
}
```

Since each payload must be a single type, [units](./unit.md), [pairs](./pair.md), and other types
are used to define composite payloads. For example:

```par
type MaybeBoth<a, b> = either {
  .neither!,
  .left a,
  .right b,
  .both(a, b)!,
}
```

Either types are frequently used together with [_recursive_](./recursive.md) types to define finite
tree-like structures.

```par
type BinaryTree<a> = recursive either {
  .empty!,
  .node(a, self, self)!,
}
```

The pre-defined `List<a>` type is a combination of `recursive` and `either`:

```par
type List<a> = recursive either {
  .end!,
  .item(a) self,
}
```

An either type is [`Data`](TODO) if all of its payloads are `Data`. For example, `List<Int>` is `Data`,
and therefore variables of type `List<Int>` can be used multiple times or left unused.

## Construction

