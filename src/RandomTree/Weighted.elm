module RandomTree.Weighted exposing
    ( Tree
    , getWeight
    , singleton, fromList
    , get, pick, take, drop
    , map, mapContent, mapWeight, adjustTotalWeightTo
    , insert, insertList, merge
    , remove, filter
    , toList
    )

{-| This module provides a data structure that allows for random selection from a collection of data in amortized _O(log(N))_ time. You can assign a weight to each element as a `Float` value.


## Note

The `Tree` in this module:

  - cannot be empty. Consequently:
      - Functions that reduce the number of elements (`pick`, `take`, `drop`, `remove`, and `filter`) return `Maybe (Tree a)` in the event that all elements are deleted.
      - At least one element must be provided during creation, so `fromList` requires a head element and a tail list (similar to `Random.uniform` or `Random.weighted`).
  - cannot contain elements with negative weights. If such weights are provided, they are automatically converted to their absolute values. While this may lead to errors, wrapping all operations in `Maybe` would be more cumbersome.
  - is not a search tree (the elements are not ordered), resulting in a time complexity of _O(N)_ for `member` and `remove`.


# Type

@docs Tree


# Query

@docs getWeight


# Creation

@docs singleton, fromList


# Random Operations

These functions all return `Random.Generator`.

@docs get, pick, take, drop


# Modification

@docs map, mapContent, mapWeight, adjustTotalWeightTo


# Addition

@docs insert, insertList, merge


# Deletion

These functions return `Maybe` if all elements are removed.

@docs remove, filter


# Destructure

@docs toList

-}

import Random


{-| `RandomTree.Weighted.Tree` represents non-empty collection of randomly chosen values.
-}
type Tree a
    = Branch
        { left : Tree a
        , right : Tree a
        , weight : Float
        }
    | Leaf
        { content : a
        , weight : Float
        }



-- inner functions


{-| Returns the weight of the specified `RandomTree.Weighted.Tree`.
-}
getWeight : Tree a -> Float
getWeight t =
    case t of
        Branch record ->
            record.weight

        Leaf record ->
            record.weight


reduce : ({ content : a, weight : Float } -> b) -> (b -> b -> b) -> Tree a -> b
reduce processLeaf processBranch tree =
    case tree of
        Leaf r ->
            processLeaf r

        Branch { left, right } ->
            processBranch
                (reduce processLeaf processBranch left)
                (reduce processLeaf processBranch right)


{-| Creates `RandomTree.Weighted.Tree` from a single element and its associated weight.
-}
singleton : ( Float, a ) -> Tree a
singleton ( w, c ) =
    Leaf { content = c, weight = abs w }


{-| Inserts a weighted element into the `RandomTree.Weighted.Tree`, adjusting the structure as needed.
-}
insert : ( Float, a ) -> Tree a -> Tree a
insert (( w, c ) as pair) t =
    case t of
        Branch { left, right, weight } ->
            let
                newWeight =
                    weight + abs w
            in
            if getWeight left < getWeight right then
                Branch
                    { left = insert pair left
                    , right = right
                    , weight = newWeight
                    }

            else
                Branch
                    { left = left
                    , right = insert pair right
                    , weight = newWeight
                    }

        Leaf { content, weight } ->
            Branch
                { left = t
                , right =
                    Leaf { content = c, weight = w }
                , weight = weight + abs w
                }


{-| Inserts all weighted elements from the provided list into the `RandomTree.Weighted.Tree`.
-}
insertList : List ( Float, a ) -> Tree a -> Tree a
insertList l t =
    List.foldl insert t l


{-| Creates a `RandomTree.Weighted.Tree` from a single element and a list of additional elements. This ensures that the tree is non-empty.
-}
fromList : ( Float, a ) -> List ( Float, a ) -> Tree a
fromList head tail =
    List.foldl insert (singleton head) tail


{-| Converts a `RandomTree.Weighted.Tree` into a list of tuples containing the weights and their corresponding contents.
-}
toList : Tree a -> List ( Float, a )
toList t =
    toListHelp t [] []


toListHelp : Tree a -> List (Tree a) -> List ( Float, a ) -> List ( Float, a )
toListHelp t l output =
    case t of
        Leaf { content, weight } ->
            let
                newOutput =
                    ( weight, content ) :: output
            in
            case l of
                [] ->
                    newOutput

                head :: tail ->
                    toListHelp head tail newOutput

        Branch { left, right } ->
            toListHelp left (right :: l) output


{-| Merges two `RandomTree.Weighted.Tree` instances into a single tree, preserving weights of each elements.
-}
merge : Tree a -> Tree a -> Tree a
merge t1 t2 =
    insertList (toList t1) t2


{-| Returns a `Random.Generator` that produces a tuple containing a random content from the tree and its associated weight.
-}
get : Tree a -> Random.Generator ( Float, a )
get t =
    Random.float 0 (getWeight t)
        |> Random.map (\x -> pickHelp x t [])
        |> Random.map Tuple.first


{-| Returns a `Random.Generator` that produces a tuple containing a randomly selected weighted content from the tree and a `Maybe` value representing the modified tree with that content removed.
-}
pick : Tree a -> Random.Generator ( ( Float, a ), Maybe (Tree a) )
pick t =
    Random.float 0 (getWeight t)
        |> Random.map (\x -> pickHelp x t [])
        |> Random.map
            (\( c, l ) ->
                case l of
                    [] ->
                        ( c, Nothing )

                    head :: tail ->
                        ( c
                        , Just (reconstruct head tail)
                        )
            )


pickHelp : Float -> Tree a -> List (Tree a) -> ( ( Float, a ), List (Tree a) )
pickHelp x t l =
    case t of
        Leaf { content, weight } ->
            ( ( weight, content ), l )

        Branch { left, right } ->
            let
                wl =
                    getWeight left
            in
            if x > wl then
                pickHelp (x - wl) right (left :: l)

            else
                pickHelp x left (right :: l)


reconstruct : Tree a -> List (Tree a) -> Tree a
reconstruct =
    List.foldl
        (\crr acc ->
            Branch
                { left = acc
                , right = crr
                , weight =
                    getWeight acc
                        + getWeight crr
                }
        )


{-| Returns a `Random.Generator` that produces n weighted contents selected randomly from the tree. The result includes the remaining tree structure.
-}
take : Int -> Tree a -> Random.Generator ( List ( Float, a ), Maybe (Tree a) )
take n t =
    takeHelp n t []


takeHelp :
    Int
    -> Tree a
    -> List ( Float, a )
    -> Random.Generator ( List ( Float, a ), Maybe (Tree a) )
takeHelp n t l =
    if n <= 0 then
        Random.constant ( l, Just t )

    else
        pick t
            |> Random.andThen
                (\( c, mt ) ->
                    case mt of
                        Nothing ->
                            Random.constant ( c :: l, Nothing )

                        Just newT ->
                            takeHelp (n - 1) newT (c :: l)
                )


{-| Returns `Random.Generator` that generates the argument tree whose `n` elements are removed randomly. The returned value is wrapped in `Maybe` just in case all the elements have been removed.
-}
drop : Int -> Tree a -> Random.Generator (Maybe (Tree a))
drop n t =
    take n t
        |> Random.map Tuple.second


{-| Removes all instances of the specified element from the `RandomTree.Weighted.Tree`. The result is wrapped in a `Maybe` type to indicate if the tree is empty after removal.
-}
remove : a -> Tree a -> Maybe (Tree a)
remove a =
    reduce
        (\({ content, weight } as record) ->
            if a == content then
                Nothing

            else
                Just (Leaf record)
        )
        (\mleft mright ->
            case mleft of
                Nothing ->
                    mright

                Just leftProcessed ->
                    case mright of
                        Nothing ->
                            mleft

                        Just rightProcessed ->
                            Just
                                (Branch
                                    { left = leftProcessed
                                    , right = rightProcessed
                                    , weight =
                                        getWeight leftProcessed
                                            + getWeight rightProcessed
                                    }
                                )
        )


{-| Filters the `RandomTree.Weighted.Tree` by removing elements that do not satisfy the provided predicate function. The result is wrapped in a `Maybe` type to indicate if the tree is empty after filtering.
-}
filter : (( Float, a ) -> Bool) -> Tree a -> Maybe (Tree a)
filter predicate =
    reduce
        (\({ content, weight } as record) ->
            if predicate ( weight, content ) then
                Just (Leaf record)

            else
                Nothing
        )
        (\mleft mright ->
            case mleft of
                Nothing ->
                    mright

                Just leftProcessed ->
                    case mright of
                        Nothing ->
                            mleft

                        Just rightProcessed ->
                            Just
                                (Branch
                                    { left = leftProcessed
                                    , right = rightProcessed
                                    , weight =
                                        getWeight leftProcessed
                                            + getWeight rightProcessed
                                    }
                                )
        )


{-| Maps the weighted contents of the tree using the provided function.
-}
map : (( Float, a ) -> ( Float, a )) -> Tree a -> Tree a
map func =
    reduce
        (\{ content, weight } ->
            case func ( weight, content ) of
                ( mappedWeight, mappedContent ) ->
                    Leaf
                        { content = mappedContent
                        , weight = abs mappedWeight
                        }
        )
        (\left right ->
            Branch
                { left = left
                , right = right
                , weight =
                    getWeight left
                        + getWeight right
                }
        )


{-| Maps the weight of each element in the tree using the provided function.
-}
mapWeight : (( Float, a ) -> Float) -> Tree a -> Tree a
mapWeight func =
    map (\(( _, c ) as tuple) -> ( func tuple, c ))


{-| Maps the content of each element in the tree using the provided function.
-}
mapContent : (( Float, a ) -> a) -> Tree a -> Tree a
mapContent func =
    map (\(( w, _ ) as tuple) -> ( w, func tuple ))


{-| Adjusts the total weight of the tree to the specified value.
-}
adjustTotalWeightTo : Float -> Tree a -> Tree a
adjustTotalWeightTo w t =
    adjustTotalWeightToHelp (abs w) t


adjustTotalWeightToHelp : Float -> Tree a -> Tree a
adjustTotalWeightToHelp w t =
    case t of
        Leaf { content } ->
            Leaf
                { content = content
                , weight = w
                }

        Branch { left, right } ->
            let
                wl =
                    getWeight left

                wr =
                    getWeight right

                newWl =
                    w * wl / (wl + wr)

                newWr =
                    w - newWl

                newLeft =
                    adjustTotalWeightTo newWl left

                newRight =
                    adjustTotalWeightTo newWr right
            in
            Branch
                { left = newLeft
                , right = newRight
                , weight =
                    getWeight newLeft
                        + getWeight newRight
                }
