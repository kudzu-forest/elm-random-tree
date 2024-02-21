module RandomTree.Weighted exposing
    ( Tree, Weighted, WideFloat
    , adjustWeight, adjustContent
    , singleton, fromList, fromPairs
    , insert, insertWithRelativeWeight, insertListWithRelativeWeight
    , delete, map, filter
    , get, take, replace
    , count, totalWeight
    )

{-| This module provides data structure that allows random picking up from a collection of data in _O(log(N))_ time. You can set weight of each element with `WideFloat` from `kudzu-forest/elm-wide-float` package.


## Note

`Tree` in this module

  - is not allowed to be empty, so
      - the return value of function that reduce the number of element(`take`, `delete`, and `filter`) all return `Maybe (Tree a)` just in case all the elements are deleted.
      - You have to pass at least one element at the creation, so function like `fromList` takes one heading element and tailing list.(similar with `Random.uniform` or `Random.weighted`.)
  - can not have zero-or-negative-weighted elements. So if you give ones, the weight is automatically converted to `1`. This may be an error prone, but otherwise all things must be wrapped in `Maybe`.
  - is not a search tree(the elements are not ordered), so the time complexity of `member` and `delete` is _O(N)_.
  - may be unbalanced when `delete` or `filter` is called, so when I say "The time complexity is _O(log(N))_", _N_ denotes the maximal size in the history of the tree so far.


# Types

@docs Tree, Weighted, WideFloat


# Operation around weighted datum

@docs adjustWeight, adjustContent


# Creation

@docs singleton, fromList, fromPairs


# Insertion

@docs insert, insertWithRelativeWeight, insertListWithRelativeWeight


# Modification

@docs delete, map, filter


# Random Pick Up

@docs get, take, replace


# Status Checking

@docs count, totalWeight

-}

import Bitwise
import Random
import WideFloat exposing (add, proportionOf)


{-| Type that represent floating point number with wider range than `Float` in core package. This is introduced for preventing overflow of exponentially changing value.
Please hit `elm install kudzu-forest/elm-wide-float` in your terminal.
-}
type alias WideFloat =
    WideFloat.WideFloat


type Node a
    = Branch
        { l : Tree a
        , r : Tree a
        }
    | Leaf a


type alias Content a =
    { c : Int
    , w : WideFloat
    , n : Node a
    }


{-| Type that represents binary tree with weighted elements subjected to random picking up.
-}
type Tree a
    = Tree (Content a)


{-| Type alias that represents each weighted contents in `Tree`. The probability of being chosen is proportional to its weight.
The weight is represented with `WideFloat.WideFloat` type in `kudzu-forest/elm-wide-float` package.
-}
type alias Weighted a =
    { weight : WideFloat.WideFloat
    , content : a
    }


{-| Returns a `Tree` that has only one element. Never give this function a negetive-or-zero-weighted element.(If the weight is negative or zero, the weight is automatically converted to 1.)
-}
singleton : Weighted a -> Tree a
singleton e =
    if WideFloat.isLargerThan WideFloat.zero e.weight then
        Tree
            { c = 1
            , w = e.weight
            , n = Leaf e.content
            }

    else
        Tree
            { c = 1
            , w = WideFloat.one
            , n = Leaf e.content
            }


{-| Returns a `Tree` that has all element in given list.Never give this function a negetive-or-zero-weighted element.(If the weight is negative or zero, the weight is automatically converted to 1.)
-}
fromList : Weighted a -> List (Weighted a) -> Tree a
fromList e t =
    let
        head =
            Tree
                { c = 1
                , w = e.weight
                , n = Leaf e.content
                }

        tail =
            List.map singleton t
    in
    fromList_ head tail []


fromList_ :
    Tree a
    -> List (Tree a)
    -> List (Tree a)
    -> Tree a
fromList_ current before after =
    case before of
        head1 :: head2 :: tail ->
            case current of
                Tree c ->
                    case head1 of
                        Tree h1 ->
                            let
                                newBranch =
                                    Branch
                                        { l = current
                                        , r = head1
                                        }

                                processed =
                                    Tree
                                        { c = c.c + h1.c
                                        , w = add c.w h1.w
                                        , n = newBranch
                                        }
                            in
                            fromList_ head2 tail (processed :: after)

        head1 :: [] ->
            case current of
                Tree c ->
                    case head1 of
                        Tree h1 ->
                            let
                                newBranch =
                                    Branch
                                        { l = current
                                        , r = head1
                                        }

                                processed =
                                    Tree
                                        { c = c.c + h1.c
                                        , w = add c.w h1.w
                                        , n = newBranch
                                        }
                            in
                            fromList_ processed after []

        [] ->
            case after of
                _ :: _ ->
                    fromList_ current after []

                [] ->
                    current


{-| Creates `Tree` from list of tuples of a `Float` value corresponding to its weight as the first component, and a datum as the second component. Never give this function a negetive-or-zero-weighted element.(If the weight is negative or zero, the weight is automatically converted to 1.)
-}
fromPairs : ( Float, a ) -> List ( Float, a ) -> Tree a
fromPairs ( hf, ha ) tail =
    let
        current =
            singleton
                { weight = WideFloat.fromFloat hf
                , content = ha
                }

        before =
            tail
                |> List.map
                    (\( f, a ) ->
                        { weight = WideFloat.fromFloat f
                        , content = a
                        }
                    )
                |> List.map singleton
    in
    fromList_ current before []


{-| Inserts a Weighted data to an already-existing `Tree`. Never give this function a negetive-or-zero-weighted element.(If the weight is negative or zero, the weight is automatically converted to 1.)
-}
insert : Weighted a -> Tree a -> Tree a
insert e t =
    insert_ (singleton e) t


insert_ : Tree a -> Tree a -> Tree a
insert_ (Tree a) (Tree t) =
    case t.n of
        Branch b ->
            case b.l of
                Tree l ->
                    case b.r of
                        Tree r ->
                            if l.c <= r.c then
                                Tree
                                    { c = t.c + 1
                                    , w = add t.w a.w
                                    , n =
                                        Branch
                                            { l = insert_ (Tree a) b.l
                                            , r = b.r
                                            }
                                    }

                            else
                                Tree
                                    { c = t.c + 1
                                    , w = add t.w a.w
                                    , n =
                                        Branch
                                            { l = b.l
                                            , r = insert_ (Tree a) b.r
                                            }
                                    }

        Leaf _ ->
            Tree
                { c = 2
                , w = add t.w a.w
                , n =
                    Branch
                        { l = Tree a
                        , r = Tree t
                        }
                }


{-| Random generator that generates one of the elements conteined in `Tree` given as parameter. The probability for each elements to be chosen is proportional to its weight. The time complexity is _O(log(N))_.
-}
get : Tree a -> Random.Generator a
get (Tree t) =
    Random.float 0 1
        |> Random.map (\x -> get_ x t.n)


get_ : Float -> Node a -> a
get_ x n =
    case n of
        Branch b ->
            case b.l of
                Tree l ->
                    case b.r of
                        Tree r ->
                            let
                                p =
                                    proportionOf
                                        l.w
                                        r.w
                            in
                            if x < p then
                                get_ (x / p) l.n

                            else
                                get_ ((x - p) / (1 - p)) r.n

        Leaf leaf ->
            leaf


{-| Random generator that generates one of the elements conteined in `Tree`, paired with the rest part of the tree.
If the `Tree` has only one element, then `Nothing` is returned as the second component.
The time complexity is _O(log(N))_.
-}
take : Tree a -> Random.Generator ( Weighted a, Maybe (Tree a) )
take (Tree t) =
    Random.float 0 1
        |> Random.map
            (\x -> take_ x t [])


take_ : Float -> Content a -> List (Content a) -> ( Weighted a, Maybe (Tree a) )
take_ x t list =
    case t.n of
        Branch b ->
            case b.l of
                Tree l ->
                    case b.r of
                        Tree r ->
                            let
                                p =
                                    proportionOf
                                        l.w
                                        r.w
                            in
                            if x < p then
                                take_ (x / p) l (r :: list)

                            else
                                take_ ((x - p) / (1 - p)) r (l :: list)

        Leaf leaf ->
            let
                e =
                    { weight = t.w
                    , content = leaf
                    }
            in
            case list of
                head :: tail ->
                    let
                        tree =
                            reconstruct head tail
                    in
                    ( e, Just tree )

                [] ->
                    ( e, Nothing )


{-| Random generator that replaces one weighted data from the tree and generates a pair consisting of the removed data and resultant tree. The time complexity is _O(log(N))_
-}
replace : Weighted a -> Tree a -> Random.Generator ( Weighted a, Tree a )
replace e (Tree c) =
    Random.float 0 1
        |> Random.map (\x -> replace_ x e c [])


replace_ : Float -> Weighted a -> Content a -> List (Content a) -> ( Weighted a, Tree a )
replace_ x e c list =
    case c.n of
        Branch b ->
            case b.l of
                Tree l ->
                    case b.r of
                        Tree r ->
                            let
                                p =
                                    proportionOf
                                        l.w
                                        r.w
                            in
                            if x < p then
                                replace_ (x / p) e l (r :: list)

                            else
                                replace_ ((x - p) / (1 - p)) e r (l :: list)

        Leaf leaf ->
            let
                returnedWeighted =
                    { weight = c.w
                    , content = leaf
                    }

                returnedTree =
                    reconstruct
                        { c = 1
                        , w = e.weight
                        , n = Leaf e.content
                        }
                        list
            in
            ( returnedWeighted, returnedTree )


{-| Inserts an element into the tree. The first parameter denotes the relative weight(`1` means the whole weight of the original tree). The time complexity is _O(log(N))_.
-}
insertWithRelativeWeight : Float -> a -> Tree a -> Tree a
insertWithRelativeWeight f c (Tree t) =
    insert
        { weight = WideFloat.multiplyFloat f t.w
        , content = c
        }
        (Tree t)


{-| Inserts all elements of given list into the tree of second parameter. The first components of the tuple denotes the relative weight(`1` means the whole weight of the original tree).
-}
insertListWithRelativeWeight : List ( Float, a ) -> Tree a -> Tree a
insertListWithRelativeWeight list (Tree t) =
    insertListWithRelativeWeight_ t.w list (Tree t)


insertListWithRelativeWeight_ : WideFloat -> List ( Float, a ) -> Tree a -> Tree a
insertListWithRelativeWeight_ w list t =
    case list of
        ( f, c ) :: tail ->
            insertListWithRelativeWeight_ w
                tail
                (insert
                    { weight = WideFloat.multiplyFloat f w
                    , content = c
                    }
                    t
                )

        [] ->
            t


{-| Returns how many elements the tree has. The time complexity is _O(1)_
-}
count : Tree a -> Int
count (Tree t) =
    t.c


{-| Returns the sum of weight of all the elements in the tree. The time complexity is _O(1)_
-}
totalWeight : Tree a -> WideFloat
totalWeight (Tree t) =
    t.w


{-| Checks whether the tree of second parameter has the first element. The time complexity is _O(N)_.
-}
member : a -> Tree a -> Bool
member a (Tree r) =
    case r.n of
        Leaf leaf ->
            a == leaf

        Branch b ->
            member a b.l || member a b.r


{-| Maps all the content of the tree, with the weights unchanged.
-}
map : (a -> b) -> Tree a -> Tree b
map f (Tree r) =
    case r.n of
        Leaf leaf ->
            Tree
                { c = 1
                , w = r.w
                , n =
                    Leaf (f leaf)
                }

        Branch b ->
            Tree
                { c = r.c
                , w = r.w
                , n =
                    Branch
                        { l = map f b.l
                        , r = map f b.r
                        }
                }


{-| Removes any number of elements which passes the test function given as the first parameter from the second parameter. The returned value is wrapped in `Maybe`.
-}
filter : (WideFloat -> a -> Bool) -> Tree a -> Maybe (Tree a)
filter f (Tree r) =
    case r.n of
        Leaf leaf ->
            if f r.w leaf then
                Just (Tree r)

            else
                Nothing

        Branch b ->
            case filter f b.l of
                Nothing ->
                    filter f b.r

                (Just (Tree filteredLeft)) as fl ->
                    case filter f b.r of
                        Nothing ->
                            fl

                        Just (Tree filteredRight) ->
                            Just
                                (Tree
                                    { c =
                                        filteredLeft.c
                                            + filteredRight.c
                                    , w =
                                        add filteredLeft.w
                                            filteredRight.w
                                    , n =
                                        Branch
                                            { l =
                                                Tree filteredLeft
                                            , r =
                                                Tree filteredRight
                                            }
                                    }
                                )


{-| Removes any number of elements which is the same as the first parameter from the second parameter. The returned value is wrapped in `Maybe`. The time complexity is _O(N)_.
-}
delete : a -> Tree a -> Maybe (Tree a)
delete a (Tree r) =
    case r.n of
        Leaf leaf ->
            if a == leaf then
                Nothing

            else
                Just (Tree r)

        Branch b ->
            let
                mLeft =
                    delete a b.l

                mRight =
                    delete a b.r
            in
            case mLeft of
                Just (Tree left) ->
                    case mRight of
                        Just (Tree right) ->
                            Just
                                (Tree
                                    { c = left.c + right.c
                                    , w = WideFloat.add left.w right.w
                                    , n =
                                        Branch
                                            { l = Tree left
                                            , r = Tree right
                                            }
                                    }
                                )

                        Nothing ->
                            mLeft

                Nothing ->
                    mRight


{-| Returns weighted datum whose weight is multiplied with the given `Float`.
-}
adjustWeight : Float -> Weighted a -> Weighted a
adjustWeight f e =
    { weight =
        WideFloat.multiplyFloat f e.weight
    , content = e.content
    }


{-| Maps content in `Element`.
-}
adjustContent : (a -> b) -> Weighted a -> Weighted b
adjustContent f e =
    { weight = e.weight
    , content = f e.content
    }



-- inner functions


reconstruct : Content a -> List (Content a) -> Tree a
reconstruct current list =
    case list of
        head :: tail ->
            if Bitwise.shiftLeftBy 1 current.c - head.c > 0 then
                let
                    newC =
                        { c = current.c + head.c
                        , w = add current.w head.w
                        , n =
                            Branch
                                { l = Tree current
                                , r = Tree head
                                }
                        }
                in
                reconstruct newC tail

            else
                case head.n of
                    Branch b ->
                        case b.l of
                            Tree l ->
                                case b.r of
                                    Tree r ->
                                        let
                                            richer =
                                                if l.c >= r.c then
                                                    l

                                                else
                                                    r

                                            poorer =
                                                if l.c >= r.c then
                                                    r

                                                else
                                                    l

                                            newLeftContent =
                                                { c = current.c + poorer.c
                                                , w =
                                                    add current.w poorer.w
                                                , n =
                                                    Branch
                                                        { l = Tree current
                                                        , r = Tree poorer
                                                        }
                                                }

                                            newBranch =
                                                Branch
                                                    { l = Tree newLeftContent
                                                    , r = Tree richer
                                                    }

                                            newC =
                                                { c = current.c + head.c
                                                , w = add newLeftContent.w richer.w
                                                , n = newBranch
                                                }
                                        in
                                        reconstruct newC tail

                    Leaf _ ->
                        --impossible
                        Tree current

        [] ->
            Tree current
