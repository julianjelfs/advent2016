module Day14 exposing (..)

import MD5 exposing (hex)
import Dict
import Set

input = "yjdafjpo"

getHash hashes inp =
    case Dict.get inp hashes of
        Nothing ->
            let
                h = hex inp
            in
                (h, Dict.insert inp h hashes)
        Just h ->
            (h, hashes)

stretchedHash hashes key =
    List.foldl (\i (h, hs) ->
        getHash hs h
    ) (getHash hashes key) (List.range 0 2015)

generateHash hashes index =
    let
        key = (input ++ (toString index))
    in
        case Dict.get key hashes of
            Nothing ->
                let
                    (h, updated) = stretchedHash hashes key
                in
                    (h, Dict.insert key h updated)
            Just h ->
                (h, hashes)

hashContainsTriple hash =
    let
        lookForTriples h =
            case h of
                a :: b :: c :: _ ->
                    if a == b && b == c then
                        Just a
                    else
                        lookForTriples (List.tail h |> Maybe.withDefault [])
                _ -> Nothing
    in
        lookForTriples (String.toList hash)

hashContainsFiveOfAKind c hash =
    let
        str =
            List.repeat 5 c
                |> List.foldl String.cons ""
    in
        String.contains str hash

futureHashesContainFiveOfAKind hashes index c =
    let
        (next1000, updated) =
            List.range index (index + 999)
                |> List.foldl (\i (hs, prev) ->
                    let
                        (h, updated) = generateHash prev i
                    in
                        (h::hs, updated)
                ) ([], hashes)
    in
        (next1000
            |> List.filter (hashContainsFiveOfAKind c)
            |> List.isEmpty
            |> not, updated)

getKeys index hashes keys =
    case Set.size keys of
        64 -> index - 1
        _ ->
            let
                (h, updated) = generateHash hashes index
            in
                case hashContainsTriple h of
                    Nothing ->
                        getKeys (index + 1) updated keys
                    Just t ->
                        case futureHashesContainFiveOfAKind updated (index + 1) t of
                            (True, updatedAgain) ->
                                getKeys (index + 1) updatedAgain (Set.insert h keys)
                            (_, updatedAgain) ->
                                getKeys (index + 1) updatedAgain keys

solution () =
    getKeys 0 Dict.empty Set.empty




