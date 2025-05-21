# Items

> **<sup>Syntax</sup>**\
> _Item_ :\
> &nbsp;&nbsp; &nbsp;&nbsp; [_TypeDefinition_](#type-definitions) \
> &nbsp;&nbsp; | [_Declaration_](#definitions) \
> &nbsp;&nbsp; | [_Definition_](#definitions)

Items are the primary building block of Par programs.

## Definitions

> **<sup>Syntax</sup>**\
> _Declaration_ :\
> &nbsp;&nbsp; &nbsp;&nbsp; `dec` [ID] `:` [_Type_]
> 
> _Definition_ :\
> &nbsp;&nbsp; &nbsp;&nbsp; `def` [ID] [_Annotation_]<sup>?</sup> `=` [_Expression_]

`def` defines a global definition usable throughout the file in which it was defined.
It can be used as many times as one desires, instantiating itself every time it's used.
```par
// define a static value
dec Unit : !
def Unit = !
// or all-in-one
def Unit: ! = !

// define a function
def Negate: [Bool] Bool = [b] b.case {
  .true! => .false!
  .false! => .true!
}

// define a function receiving types
dec Pop : [type t] [List<t>] (Option<t>) List<t>
def Pop = [type t] [list] list.case {
  .empty! => (.none!) .empty!
  .item(head) tail => (.some head) tail
}
```

## Type Definitions

> **<sup>Syntax</sup>**\
> _TypeDefinition_ :\
> &nbsp;&nbsp; &nbsp;&nbsp; `type` [ID] _TypeParameters_<sup>?</sup> `=` [_Type_]
>
> _TypeParameters_ :\
> &nbsp;&nbsp; &nbsp;&nbsp; `<` _TypeParameter_ (`,` _TypeParamter_)<sup>\*</sup> `,`<sup>?</sup> `>`
>
> _TypeParameter_ :\
> &nbsp;&nbsp; &nbsp;&nbsp; [ID]

A type definition defines a type alias, not a "new type". All types in Par are structural. <!--(Proposal: Automatically add a tag in some cases for either and choice types)-->
```par
// simple type alias
type Boolean = Bool

// the definition of Bool
type Bool = either {
  .true!
  .false!
}

// parameterized type alias
type Option<t> = either {
  .none!
  .some t
}
```


[ID]: ./lexical.md
[_Type_]: ./types.md
[_PatternList_]: ./patterns.md
[_PatternNoTopAlt_]: ./patterns.md
[_Expression_]: ./expressions.md
[_Annotation_]: statements.md#let-statements