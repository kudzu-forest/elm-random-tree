module RandomTree.Uniform exposing
    ( Tree
    , singleton, fromList
    , insert, insertList
    , map, filter, delete
    , get, take, replace
    , count
    )

{-| This module provides data structure subjected to random choice of elements in uniform probability.


# Types

@docs Tree


# Creation

@docs singleton, fromList


# Insertion

@docs insert, insertList


# Modification

@docs map, filter, delete


# Random Picking Up

@docs get, take, replace


# Status Checking

@docs count

-}

import Bitwise
import Random


type Node a
    = Branch
        { l : Tree a
        , r : Tree a
        }
    | Leaf a


type alias Content a =
    { c : Int
    , n : Node a
    }


{-| Type that represents binary tree with weighted elements for random picking up.
-}
type Tree a
    = Tree (Content a)


{-| Returns a `Tree` that has only one element.
-}
singleton : a -> Tree a
singleton e =
    Tree
        { c = 1
        , n = Leaf e
        }


{-| Returns a `Tree` that has all elements in given list.
-}
fromList : a -> List a -> Tree a
fromList e t =
    let
        head =
            singleton e

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


{-| Inserts a data to an already-existing `Tree`.
-}
insert : a -> Tree a -> Tree a
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
                                    , n =
                                        Branch
                                            { l = insert e b.l
                                            , r = b.r
                                            }
                                    }

                            else
                                Tree
                                    { c = t.c + 1
                                    , n =
                                        Branch
                                            { l = b.l
                                            , r = insert e b.r
                                            }
                                    }

        Leaf _ ->
            Tree
                { c = 2
                , n =
                    Branch
                        { l =
                            Tree
                                { c = 1
                                , n = Leaf e
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
                                    toFloat l.c
                                        / (toFloat l.c + toFloat r.c)
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
take : Tree a -> Random.Generator ( a, Maybe (Tree a) )
take (Tree t) =
    Random.float 0 1
        |> Random.map
            (\x -> take_ x t [])


take_ : Float -> Content a -> List (Content a) -> ( a, Maybe (Tree a) )
take_ x t list =
    case t.n of
        Branch b ->
            case b.l of
                Tree l ->
                    case b.r of
                        Tree r ->
                            let
                                p =
                                    toFloat l.c
                                        / (toFloat l.c
                                            + toFloat r.c
                                          )
                            in
                            if x < p then
                                take_ (x / p) l (r :: list)

                            else
                                take_ ((x - p) / (1 - p)) r (l :: list)

        Leaf e ->
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
replace : a -> Tree a -> Random.Generator ( a, Tree a )
replace e (Tree c) =
    Random.float 0 1
        |> Random.map (\x -> replace_ x e c [])


replace_ : Float -> a -> Content a -> List (Content a) -> ( a, Tree a )
replace_ x e c list =
    case c.n of
        Branch b ->
            case b.l of
                Tree l ->
                    case b.r of
                        Tree r ->
                            let
                                p =
                                    toFloat l.c
                                        / (toFloat l.c
                                            + toFloat r.c
                                          )
                            in
                            if x < p then
                                replace_ (x / p) e l (r :: list)

                            else
                                replace_ ((x - p) / (1 - p)) e r (l :: list)

        Leaf leaf ->
            let
                returnedTree =
                    reconstruct c list
            in
            ( leaf, returnedTree )


{-| Inserts all elements of given list into the tree of second parameter. The first components of the tuple denotes the relative weight(`1` means the whole weight of the original tree).
-}
insertList : List a -> Tree a -> Tree a
insertList list t =
    List.foldl insert t list


{-| Returns how many elements the tree has.
-}
count : Tree a -> Int
count (Tree t) =
    t.c


{-| Checks whether the tree of second parameter has the first element.
-}
member : a -> Tree a -> Bool
member a (Tree r) =
    case r.n of
        Leaf leaf ->
            a == leaf

        Branch b ->
            member a b.l || member a b.r


{-| Maps all the content of the tree.
-}
map : (a -> b) -> Tree a -> Tree b
map f (Tree r) =
    case r.n of
        Leaf leaf ->
            Tree
                { c = 1
                , n =
                    Leaf (f leaf)
                }

        Branch b ->
            Tree
                { c = r.c
                , n =
                    Branch
                        { l = map f b.l
                        , r = map f b.r
                        }
                }


{-| Removes any number of elements which fails to passes the test function given as the first parameter. The returned value is wrapped in `Maybe`.
-}
filter : (a -> Bool) -> Tree a -> Maybe (Tree a)
filter f (Tree r) =
    case r.n of
        Leaf leaf ->
            if f leaf then
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
delete a =
    filter ((/=) a)



-- inner functions


reconstruct : Content a -> List (Content a) -> Tree a
reconstruct current list =
    case list of
        head :: tail ->
            if 2 * current.c - head.c > 0 then
                let
                    newC =
                        { c = current.c + head.c
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
                                                , n = newBranch
                                                }
                                        in
                                        reconstruct newC tail

                    Leaf _ ->
                        --impossible
                        Tree current

        [] ->
            Tree current
