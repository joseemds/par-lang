# Recursive

The previous types had unfamiliar syntax, perhaps, but the concepts were familiar. Now we are entering
the territory of unfamiliar concepts. Brace yourself, we're just getting started!

_Par_ has, among others, these two ambitious design choices:
- **Totality,** meaning preventing infinite loops by type-checking.
- **A structural type system,** where global type definitions are merely aliases.

Totality necessitates distinguishing between:
- _Recursive types_, those are finite.
- _Corecursive types_, potentially infinite. In Par, we call them [_iterative types_](./iterative.md).

The choice of a structural type system has led to avoid defining self-referential types naively,
and instead add a first-class syntax for anonymous self-referential types.

Par is very radical here. If you try the usual way of defining a singly-linked list, it fails:

```par
type IllegalList = either {
  .end!,
  .item(String) IllegalList,  // Error! Cyclic dependency.
}
```

In general, **cyclic dependencies between global definitions are disallowed.** Instead, we have:
- First-class anonymous self-referential types: `recursive` and [`iterative`](./iterative.md).
- A single, universal recursion construct: `begin`/`loop`. It's suitable for recursive destruction,
  iterative construction, and imperative-style loops in [process syntax](../processes.md).

**Let's take a look at `recursive`!**

> **Totality _does not_ mean you can't have a web server, or a game.** While these are often
> implemented using infinite event loops, it doesn't have to be done that way. Instead, we can employ
> corecursion, which Par supports with its [iterative](./iterative.md) types.
>
> To make it clearer, consider this Python program:
>
> ```python
> def __main__():
>     while True:
>         req = next_request()
>         if req is None:
>             break
>         handle_request(req)
> ```
>
> That's a simplified web server, handling requests one by one, using an infinite loop.
>
> Could we switch it around and not have an infinite loop? Absolutely!
>
> ```python
> class WebServer:
>     def close(self):
>         pass
> 
>     def handle(req):
>         handle_request(req)
> 
> def __main__():
>     start_server(WebServer())
> ```
>
> A small re-structuring goes a long way here. [Iterative](./iterative.md) types in Par enable
> precisely this pattern, but with the ergonomics of the infinite loop version.

A recursive type starts with the keyword `recursive` followed by a body that may contain any number
of occurrences of `self`: the self-reference.

```par
type LegalList = recursive either {
  .end!,
  .item(String) self,  // Okay.
}
```

The recursive type can be thought of as being equivalent to its _expansion_. That is, replacing each
`self` inside the body with the `recursive` type itself:

1. The original definition:
   ```par
   recursive either {
     .end!,
     .item(String) self
   }
   ```
2. The first expansion:
   ```par
   either {
     .end!,
     .item(String) recursive either {
       .end!,
       .item(String) self
     }
   }
   ```
3. The second expansion:
   ```par
   either {
     .end!,
     .item(String) either {
       .end!,
       .item(String) recursive either {
         .end!,
         .item(String) self
       }
     }
   }
   ```
4. And so on...

> The body of a `recursive` often starts with an `either`, but doesn't have to. Here's an example of that:
> a non-empty list, which starts with a pair.
>
> ```par
> type NonEmptyList<a> = recursive (a) either {
>   .end!,
>   .item self,
> }
> ```
>
> Another example of a `recursive` type, which doesn't start with an `either` would be a finite stream.
>
> ```par
> type FiniteStream<a> = recursive choice {
>   .close => !,
>   .next => either {
>     .end!,
>     .item(a) self,
>   }
> }
> ```
>
> This one starts with a [choice](./choice.md), which enables polling the elements on demand, or
> cancelling the rest of the stream. However, being recursive, a `FiniteStream<a>` is guaranteed
> to reach the `.end!` eventually, if not cancelled.

The key features of _recursive types_ are that **their values are finite,** and that
**we can perform recursion on them.**

## Construction

Recursive types don't have any special construction syntax. Instead, we directly construct their
bodies, as if they were expanded.

```par
type Tree = recursive either {
  .leaf Int,
  .node(self, self)!,
}

def SmallTree: Tree = .node(
  .node(
    .leaf 1,
    .leaf 2,
  )!,
  .node(
    .leaf 3,
    .leaf 4,
  )!,
)!
```

Already constructed recursive values can be used in the `self`-places of new ones:

```par
def BiggerTree: Tree = .node(SmallTree, SmallTree)!
```

**Lists** are a frequently used recursive type, and so are pre-defined as:

```par
type List<a> = recursive either {
  .end!,
  .item(a) self,
}
```

Constructing them goes like:

```par
dec OneThroughFive  : List<Int>
dec ZeroThroughFive : List<Int>

def OneThroughFive  = .item(1).item(2).item(3).item(4).item(5).end!
def ZeroThroughFive = .item(0) OneThroughFive
```

Because lists are so ubiquitous, there is additionally a **syntax sugar** for constructing them more
concisely:

```par
def OneThroughFive = *(1, 2, 3, 4, 5)
```

However, prepending onto an existing list has no syntax sugar, so `ZeroThroughFive` still has to be
done the same way.

## Destruction

