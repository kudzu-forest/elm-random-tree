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


# Query

@docs count, totalWeight

-}

import Bitwise
import Random
import WideFloat exposing (add, proportionOf)


{-| Type that represent floating point number with wider range than `Float` in core package. This is introduced for preventing overflow of exponentially changing value.
If you need to directly manipulate values of `WideFloat`, please hit `elm install kudzu-forest/elm-wide-float` in your terminal.
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


{-| Type that represents binary tree with weighted elements for random picking up.
-}
type Tree a
    = Tree (Content a)


{-| Type alias that represents each weighted contents in `Tree`.
-}
type alias Weighted a =
    { weight : WideFloat
    , content : a
    }


{-| Returns a `Tree` that has only one element.
-}
singleton : Weighted a -> Tree a
singleton e =
    Tree
        { c = 1
        , w = e.weight
        , n = Leaf e.content
        }


{-| Returns a `Tree` that has all element in given list.
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


{-| Creates `Tree` from list of tuples of a `Float` value corresponding to its weight as the first component, and a datum as the second component.
the weighter it is, the higher probability it is chosen in.
-}
fromPairs : ( Float, a ) -> List ( Float, a ) -> Tree a
fromPairs ( hf, ha ) tail =
    let
        current =
            Tree
                { c = 1
                , w =
                    WideFloat.create
                        { base2toThe1024exponent = 0
                        , significand = hf
                        }
                , n = Leaf ha
                }

        before =
            List.map
                (\( f, a ) ->
                    Tree
                        { c = 1
                        , w =
                            WideFloat.create
                                { base2toThe1024exponent = 0
                                , significand = f
                                }
                        , n = Leaf a
                        }
                )
                tail
    in
    fromList_ current before []


{-| Inserts a Weighted data to an already-existing `Tree`.
-}
insert : Weighted a -> Tree a -> Tree a
insert e (Tree t) =
    case t.n of
        Branch b ->
            case b.l of
                Tree l ->
                    case b.r of
                        Tree r ->
                            if l.c <= r.c then
                                Tree
                                    { c = t.c + 1
                                    , w = add t.w e.weight
                                    , n =
                                        Branch
                                            { l = insert e b.l
                                            , r = b.r
                                            }
                                    }

                            else
                                Tree
                                    { c = t.c + 1
                                    , w = add t.w e.weight
                                    , n =
                                        Branch
                                            { l = b.l
                                            , r = insert e b.r
                                            }
                                    }

        Leaf _ ->
            Tree
                { c = 2
                , w = add t.w e.weight
                , n =
                    Branch
                        { l =
                            Tree
                                { c = 1
                                , w = e.weight
                                , n = Leaf e.content
                                }
                        , r = Tree t
                        }
                }


{-| Random generator that generates one of the elements conteined in `Tree` given as parameter.
The time complexity is _O(log(N))_ where _N_ denotes the size of the tree.
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
The time complexity is _O(log(N))_ where _N_ denotes the size of the tree.
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


{-| Random generator that replaces one weighted data from the tree and generates a pair consisting of the removed data and resultant tree.
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


{-| Inserts an element into the tree. The first parameter denotes the relative weight(`1` means the whole weight of the original tree).
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


{-| Returns how many elements the tree has.
-}
count : Tree a -> Int
count (Tree t) =
    t.c


{-| Returns the sum of weight of all the elements in the tree.
-}
totalWeight : Tree a -> WideFloat
totalWeight (Tree t) =
    t.w


{-| Checks whether the tree of second parameter has the first element.
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


{-| Removes any number of elements which is the same as the first parameter from the second parameter. The returned value is wrapped in `Maybe`.
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
