prettiest
=========

Various experiments modifying Bernardy's `The Prettiest Printer`

* Using a safer representation (rather than a `Seq` under the assumption it is non empty)
* Using a simpler representation (a binary tree with no rebalancing, unlike `Seq` which are implemented as finger trees)
* Adding pragmas (INLINE, SPECIALISE, UNPACK)

So far not much luck
