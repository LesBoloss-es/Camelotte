type 'a cast = 'a -> string

let int = string_of_int
let bit = function true -> "1" | _ -> "0"
let float = Format.sprintf "%g"
let char = String.make 1
let string = Fun.id

let list c l = List.map c l |> String.concat " "

let array c s = list c (Array.to_list s)

let tuple2g c1 c2 (v1, v2) =
  c1 v1 ^ " " ^ c2 v2

let tuple2 c t = tuple2g c c t
let pairg = tuple2g
let pair = tuple2

let tuple3g c1 c2 c3 (v1, v2, v3) =
  c1 v1 ^
  " " ^
  c2 v2 ^
  " " ^ c3 v3

let tuple3 c t = tuple3g c c c t

let tuple4g c1 c2 c3 c4 (v1, v2, v3, v4) =
  c1 v1 ^
  " " ^
  c2 v2 ^
  " " ^
  c3 v3 ^
  " " ^ c4 v4

let tuple4 c t = tuple4g c c c c t

let tuple5g c1 c2 c3 c4 c5 (v1, v2, v3, v4, v5) =
  c1 v1 ^
  " " ^
  c2 v2 ^
  " " ^
  c3 v3 ^
  " " ^
  c4 v4 ^
  " " ^ c5 v5

let tuple5 c t = tuple5g c c c c c t

let to_string cast v = cast v

let line_to_chan ochan cast v =
  string cast v |> output_string ochan;
  output_char ochan '\n'

let line cast value = line_to_chan stdout cast value
let line_to_err cast value = line_to_chan stderr cast value
