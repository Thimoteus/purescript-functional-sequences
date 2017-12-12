# purescript-functional-sequences

An array/list/lazy list/sequence/etc. is completely determined by the function:

```purescript
index :: forall a. sequence a -> Int -> Maybe a
```

This module defines a `Sequence` as a newtype over `Int -> Maybe a`.

A well-formed `s :: Sequence a` is such that there is an interval `[0, n]` of
natural numbers where, given `x : a`, we have `s !! i = Just x` if and only if
`i ∈ [0, n]`.