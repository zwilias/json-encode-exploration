module Json.Encode.Exploration
    exposing
        ( Encoded
        , Obj
        , Seq
        , Value
        , addField
        , concatObj
        , concatSeq
        , cons
        , emptyObj
        , emptySeq
        , float
        , force
        , int
        , list
        , listOf
        , maybe
        , null
        , object
        , objectOf
        , string
        , toValue
        , value
        )

{-| Examples assume the following imports:

    import Json.Encode

-}

import Json.Encode as Encode


type alias Value =
    Encode.Value


type Encoded t
    = Encoded (t -> Value) t


type Seq
    = Seq (List Value)


type Obj
    = Obj (List ( String, Value ))


toValue : Encoded a -> Value
toValue (Encoded encoder value) =
    encoder value



-- Primitives


{-| The equivalent of `Json.Encode.int`

    toValue <| int 5
    --> Json.Encode.int 5

-}
int : Int -> Encoded Value
int =
    value << Encode.int


{-| The equivalent of `Json.Encode.float`

    toValue <| float 4.5
    --> Json.Encode.float 4.5

-}
float : Float -> Encoded Value
float =
    value << Encode.float


{-| The equivalent of `Json.Encode.string`

    toValue <| string "hello"
    --> Json.Encode.string "hello"

-}
string : String -> Encoded Value
string =
    value << Encode.string


{-| The equivalent of `Json.Encode.null`

    toValue null
    --> Json.Encode.null

-}
null : Encoded Value
null =
    value Encode.null


{-| Wrap a value in `Encoded`.

    Json.Encode.string "hello world"
        |> value
        |> toValue
    --> Json.Encode.string "hello world"

-}
value : Value -> Encoded Value
value =
    Encoded identity


{-| Force any `Encoded x` into an `Encoded Value`

    listOf string [ "hello", "world" ] -- Encoded Seq
        |> force -- Encoded Value
        |> toValue -- Value
    --> Json.Encode.list
        [ Json.Encode.string "hello", Json.Encode.string "world" ]

-}
force : Encoded a -> Encoded Value
force =
    toValue >> value


{-| The type of thing having a `force` is useful for.

    maybe string Nothing
    --> null

    maybe string (Just "foo")
    --> string "foo"

-}
maybe : (a -> Encoded b) -> Maybe a -> Encoded Value
maybe encoder =
    Maybe.map (force << encoder) >> Maybe.withDefault null



-- Structural


{-| An empty sequence. Useful for consing thing onto.

    emptySeq
    --> list []

    cons (string "hello") <| cons (int 5) emptySeq
    --> list [ string "hello", int 5 ]

-}
emptySeq : Encoded Seq
emptySeq =
    seq []


{-| Combine a `List` of encoded values into a single `Seq`uence.

Note that making a sequence involving both `Encoded Value` and `Encoded Seq`
will involve "downcasting" the `Encoded Seq` to an `Encoded Value` using `force`.

    toValue <| list [ string "hello", string "world" ]
    --> Json.Encode.list
        [ Json.Encode.string "hello", Json.Encode.string "world" ]


    toValue <| list [ string "hello", null, int 5 ]
    --> Json.Encode.list
        [ Json.Encode.string "hello", Json.Encode.null, Json.Encode.int 5 ]


    toValue <| list [ force emptySeq, force emptyObj, null ]
    --> Json.Encode.list
        [ Json.Encode.list []
        , Json.Encode.object []
        , Json.Encode.null
        ]

-}
list : List (Encoded a) -> Encoded Seq
list =
    listOf identity


{-| An even more constrained way of creating a sequence, yet often useful.

    toValue <| listOf string [ "hello", "world" ]
    --> Json.Encode.list
        [ Json.Encode.string "hello", Json.Encode.string "world" ]


    toValue <| listOf (always null) [ 1, 2, 3 ]
    --> Json.Encode.list
        [ Json.Encode.null, Json.Encode.null, Json.Encode.null ]


    listOf identity [ null ]
    --> list [ null ]

-}
listOf : (a -> Encoded b) -> List a -> Encoded Seq
listOf encoder =
    List.map (encoder >> toValue) >> seq


{-| Create an empty JSON object, so you can add things to it as you see fit

    emptyObj
    --> object []


    emptyObj
        |> addField "hello" (string "world")
    --> object [ ( "hello", string "world" ) ]

-}
emptyObj : Encoded Obj
emptyObj =
    obj []


{-| Like `sequence` creates an `Encoded Seq`, this creates an `Encoded Obj`.

    toValue <| object [ ( "hello", string "world" ), ( "nope", null ) ]
    --> Json.Encode.object
        [ ( "hello", Json.Encode.string "world" )
        , ( "nope", Json.Encode.null )
        ]

-}
object : List ( String, Encoded a ) -> Encoded Obj
object =
    objectOf identity


{-| Create an object from a list of key-value pairs where the values all happen
to be the same type.

Since this results in an `Encoded Obj`, you can still add information of a
different type afterwards.

    objectOf string [ ( "foo", "bar" ), ( "hello", "world" ) ]
    --> object [ ( "foo", string "bar" ), ( "hello", string "world" ) ]

-}
objectOf : (a -> Encoded b) -> List ( String, a ) -> Encoded Obj
objectOf encoder =
    List.map (Tuple.mapSecond <| encoder >> toValue) >> obj



-- Structural manipulation


{-| Concatenate two sequences.

    left : Encoded Seq
    left =
        listOf string [ "a", "b", "c" ]

    right : Encoded Seq
    right =
        listOf int [ 1, 2, 3 ]

    left |> concatSeq right
    --> list
        [ string "a"
        , string "b"
        , string "c"
        , int 1
        , int 2
        , int 3
        ]

-}
concatSeq : Encoded Seq -> Encoded Seq -> Encoded Seq
concatSeq (Encoded _ (Seq right)) (Encoded _ (Seq left)) =
    seq <| left ++ right


{-| Prepend a single value to a sequence.

    listOf string [ "a", "b" ]
        |> cons (int 0)
        |> cons null
    --> list
        [ null
        , int 0
        , string "a"
        , string "b"
        ]

-}
cons : Encoded a -> Encoded Seq -> Encoded Seq
cons (Encoded valCoder x) (Encoded _ (Seq xs)) =
    seq <| valCoder x :: xs


{-| Concatenates two objects. Order doesn't really matter, but is preserved.

    left : Encoded Obj
    left =
        objectOf string [ ( "hello", "world" ) ]

    right : Encoded Obj
    right =
        objectOf int [ ( "foo", 5 ) ]

    left |> concatObj right
    --> object
        [ ( "hello", string "world" )
        , ( "foo", int 5 )
        ]

-}
concatObj : Encoded Obj -> Encoded Obj -> Encoded Obj
concatObj (Encoded _ (Obj right)) (Encoded _ (Obj left)) =
    obj <| left ++ right


{-| Add a single field to an encoded object.

    objectOf string [ ( "hello", "world" ) ]
        |> addField "foo" (string "bar")
    --> object
        [ ( "foo", string "bar" )
        , ( "hello", string "world" )
        ]

-}
addField : String -> Encoded a -> Encoded Obj -> Encoded Obj
addField key (Encoded valCoder val) (Encoded _ (Obj kvPairs)) =
    obj <| ( key, valCoder val ) :: kvPairs



-- Helpers


seq : List Value -> Encoded Seq
seq =
    Encoded seqToValue << Seq


obj : List ( String, Value ) -> Encoded Obj
obj =
    Encoded objToValue << Obj


seqToValue : Seq -> Value
seqToValue (Seq values) =
    Encode.list values


objToValue : Obj -> Value
objToValue (Obj kvPairs) =
    Encode.object kvPairs
