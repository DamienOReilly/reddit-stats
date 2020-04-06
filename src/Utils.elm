module Utils exposing (ascending, descending, sortByWith)

{-| These four helper functions are from:
<https://gist.github.com/wilsongp/860d6fd9f799c3d5dc775e8fa1d3a993>
-}


sortByWith : (a -> comparable) -> (comparable -> comparable -> Order) -> List a -> List a
sortByWith accessor sortFunc list =
    List.sortWith (orderBy accessor sortFunc) list


orderBy : (a -> comparable) -> (comparable -> comparable -> Order) -> a -> a -> Order
orderBy accessor orderFunc a b =
    orderFunc (accessor a) (accessor b)


ascending : comparable -> comparable -> Order
ascending a b =
    case compare a b of
        LT ->
            LT

        EQ ->
            EQ

        GT ->
            GT


descending : comparable -> comparable -> Order
descending a b =
    case compare a b of
        LT ->
            GT

        EQ ->
            EQ

        GT ->
            LT
