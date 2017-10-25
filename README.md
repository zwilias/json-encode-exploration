# json-encode-exploration [![Build Status](https://travis-ci.org/zwilias/json-encode-exploration.svg)](https://travis-ci.org/zwilias/json-encode-exploration)
> Retain highlevel structural information while encoding to JSON

So this is a bit of an experiment. Rather than encoding straight into a
`Json.Encode.Value`, this encodes to one of three things:

- `Encoded Value`
- `Encoded Seq`
- `Encoded Obj`

Especially the latter 2 are interesting, since it means we can now write and use
functions to, for example `addField : String -> Encoded a -> Encoded Obj -> Encoded Obj`,
or 2 concatenate two `Encoded Seq`uences.

There is not destructuring, so once a value is _inside_ an `Encoded a`, you can
no longer reach or manipulate it.

At the end of everything, you can `toValue` your `Encoded thing` to get a simple
`Value`.

Made with ❤️ and released under BSD-3.
