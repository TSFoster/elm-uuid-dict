module UUID.Dict exposing (Dict, diff, empty, filter, foldl, foldr, fromList, get, insert, intersect, isEmpty, keys, map, member, merge, partition, remove, singleton, size, toList, union, update, values)

import Dict
import UUID exposing (UUID, fromString, toString)


type Dict v
    = Dict (Dict.Dict String v)


empty : Dict v
empty =
    Dict Dict.empty


singleton : UUID -> v -> Dict v
singleton uuid =
    Dict.singleton (toString uuid) >> Dict


insert : UUID -> v -> Dict v -> Dict v
insert uuid value (Dict dict) =
    Dict (Dict.insert (toString uuid) value dict)


update : UUID -> (Maybe v -> Maybe v) -> Dict v -> Dict v
update uuid fn (Dict dict) =
    Dict (Dict.update (toString uuid) fn dict)


remove : UUID -> Dict v -> Dict v
remove uuid (Dict dict) =
    Dict (Dict.remove (toString uuid) dict)


isEmpty : Dict v -> Bool
isEmpty (Dict dict) =
    Dict.isEmpty dict


member : UUID -> Dict v -> Bool
member uuid (Dict dict) =
    Dict.member (toString uuid) dict


get : UUID -> Dict v -> Maybe v
get uuid (Dict dict) =
    Dict.get (toString uuid) dict


size : Dict v -> Int
size (Dict dict) =
    Dict.size dict


keys : Dict v -> List UUID
keys (Dict dict) =
    List.filterMap (fromString >> Result.toMaybe) (Dict.keys dict)


values : Dict v -> List v
values (Dict dict) =
    Dict.values dict


toList : Dict v -> List ( UUID, v )
toList (Dict dict) =
    List.filterMap toListMap (Dict.toList dict)


toListMap : ( String, v ) -> Maybe ( UUID, v )
toListMap ( canonical, value ) =
    case fromString canonical of
        Ok uuid ->
            Just ( uuid, value )

        Err _ ->
            Nothing


fromList : List ( UUID, v ) -> Dict v
fromList =
    List.map (Tuple.mapFirst toString) >> Dict.fromList >> Dict


map : (UUID -> a -> b) -> Dict a -> Dict b
map fn (Dict dict) =
    Dict (Dict.foldl (mapFold fn) Dict.empty dict)


mapFold : (UUID -> a -> b) -> String -> a -> Dict.Dict String b -> Dict.Dict String b
mapFold fn canonical value accumulator =
    fromString canonical
        |> Result.map (\uuid -> Dict.insert canonical (fn uuid value) accumulator)
        |> Result.withDefault accumulator


foldl : (UUID -> v -> b -> b) -> b -> Dict v -> b
foldl fn accumulator (Dict dict) =
    Dict.foldl (foldFold fn) accumulator dict


foldr : (UUID -> v -> b -> b) -> b -> Dict v -> b
foldr fn accumulator (Dict dict) =
    Dict.foldr (foldFold fn) accumulator dict


foldFold : (UUID -> v -> b -> b) -> String -> v -> b -> b
foldFold fn canonical value accumulator =
    fromString canonical
        |> Result.map (\uuid -> fn uuid value accumulator)
        |> Result.withDefault accumulator


filter : (UUID -> v -> Bool) -> Dict v -> Dict v
filter fn (Dict dict) =
    Dict (Dict.filter (filterFn fn) dict)


partition : (UUID -> v -> Bool) -> Dict v -> ( Dict v, Dict v )
partition fn (Dict dict) =
    Dict.partition (filterFn fn) dict
        |> Tuple.mapBoth Dict Dict


filterFn : (UUID -> v -> Bool) -> String -> v -> Bool
filterFn fn canonical =
    fromString canonical
        |> Result.map fn
        |> Result.withDefault (always False)


union : Dict v -> Dict v -> Dict v
union (Dict dict1) (Dict dict2) =
    Dict (Dict.union dict1 dict2)


intersect : Dict v -> Dict v -> Dict v
intersect (Dict dict1) (Dict dict2) =
    Dict (Dict.intersect dict1 dict2)


diff : Dict a -> Dict b -> Dict a
diff (Dict dict1) (Dict dict2) =
    Dict (Dict.diff dict1 dict2)


merge :
    (UUID -> a -> result -> result)
    -> (UUID -> a -> b -> result -> result)
    -> (UUID -> b -> result -> result)
    -> Dict a
    -> Dict b
    -> result
    -> result
merge onlyLeft inBoth onlyRight (Dict dict1) (Dict dict2) =
    Dict.merge (foldFold onlyLeft) (inBothFn inBoth) (foldFold onlyRight) dict1 dict2


inBothFn : (UUID -> a -> b -> result -> result) -> String -> a -> b -> result -> result
inBothFn fn canonical value1 value2 accumulator =
    case fromString canonical of
        Ok uuid ->
            fn uuid value1 value2 accumulator

        Err _ ->
            accumulator
