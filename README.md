# elm-random-tree

This package is for those who feel somehow unconfortable to apply `Random.uniform` or `Random.weighted` on large list. Both of them are easy-to-use function, but they scans each elements of the list one by one, consuming time proportional to the size of the list.

This package offers new data type `RandomTree.Uniform.Tree` and `RandomTree.Weighted.Tree` to deal with huge set of data within log-scaled time. 

Please hit
`elm install elm/elm-random`,
`elm install kudzu-forest/elm-wide-float` and
`elm install kudzu-forest/elm-random-tree`
in your terminal.
