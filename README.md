# FSharpLu F# library

This library provides F# helper utilities for string manipulations, 
logging, collection data structures, file operations, text processing, 
security, async, parsing, diagnostics, configuration files and Json serialization.

This library is used in serveral projects internally at Microsoft and made available 
to the F# community 


## License

Apache 2.0

## Packages

- FSharpLu: Core utilities
- FSharpLu.Json: Provides JSon.Net converters for improved serialization of F# data types. 
In particular it provides more succinct serialization for option types and discriminate unions.
- FSharpLu.Tests: Unit tests for the solution

## Contact
 william.blum@microsoft.com
 
 
 
## FSharp.Json

Newtonsoft's Json.Net is the library of choice for Json serialization. Unfortunately
the built-in converters generate rather verbose and ugly Json for certain F# data types like
discriminated unions and option types. The module `Microsoft.FSharpLu.Json` provides more succinct 
serialization for those types.

To use our serializer open the module with

    open Microsoft.FSharpLu.Json

Take the following simple value of type `(int option) list`

    [Some 5; None; Some 6]
 
It gets serialized by JSON.Net with command `Default.serialize [Some 5; None; Some 6]` as
 
    ``Json
    [
        {
            "Case": "Some",
            "Fields": [ 5 ]
        },
        null,
        {
            "Case": "Some",
            "Fields": [ 6 ]
        }
    ]
    ``

In comparison our seriliazer generates the following Json one-liner:

    ``Json 
    [ 5, null, 6 ]"
    ``
    
Now let's take a look at simple field-less discriminated unions. The value `Foo` of type `type SimpleDu = Foo | Bar`
gets serialized by Json.Net as `{  "Case": "Foo" }` instead of just `"Foo"` by our serializer.

Our serializer also supports generic discrimnated unions with fields for instance take the following
binary Tree example:

    ``FSharp
    type 'a Tree = Leaf of 'a | Node of 'a Tree * 'a Tree
    let x = Node (Node((Leaf 1), (Leaf 4)), Leaf 6)
    Default.serialize x
    Union.serialize x
    ``
    
The value `x` gets serialized by Json.net as

    ``Json
    {
        "Case": "Node",
        "Fields": [
            {
                "Case": "Node",
                "Fields": [
                        {
                            "Case": "Leaf",
                            "Fields": [ 1 ]
                        },
                        {
                            "Case": "Leaf",
                            "Fields": [ 4 ]
                        }
                    ]
            },
            {
                "Case": "Leaf",
                "Fields": [ 6 ]
            } ]
    }``

where FSharpLu.Json produces the more succinct:
 
    ``Json
    {
        "Node": [
            {
                "Node": [
                    { "Leaf": 1 },
                    { "Leaf": 4 }
                ]
            },
            { "Leaf": 6 }
        ]
    }``