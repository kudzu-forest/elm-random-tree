# elm-random-tree

This package is for those who feel somehow unconfortable to apply `Random.uniform` or `Random.weighted` on large list. Both of them are easy-to-use function, but they scans each elements of the list one by one, consuming time proportional to the size of the list.

This package offers new data type `RandomTree` to deal with huge set of data within log-scaled time. 
It also anables changing weight as you like.
In order to avoid overflow and underflow of exponentially changable weight, this package uses `WideFloat` from `kudzu-forest/elm-wide-float`.
