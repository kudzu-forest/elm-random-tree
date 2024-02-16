# elm-random-tree
This package enables weighted random pick up and update with in _O(log(N))_ time.

You can implement lottery system in which a person is never chosen twice.

You can do the same thing by combining `Random.weighted` function and `List.filter` function in the `elm/random` package, but it takes _O(N)_ time for every picking up and removal.
