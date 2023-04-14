(** {1 SpacedOut Write} *)

(** {2 Casts} *)

type 'a cast

(** {3 Simple} *)

val int : int cast

val bit : bool cast

val float : float cast
(** Cast a float using the %g specification (see Printf). *)

val char : char cast

val string : string cast

val list : 'a cast -> 'a list cast

val array : 'a cast -> 'a array cast

(** {3 Tuples}

   For each tuple size n (up to 5), we provide a function tuplen and a function
   tupleng, the former taking one cast and the latter taking n casts ("g" stands
   for "generic"). *)

val tuple2g : 'a cast -> 'b cast -> ('a * 'b) cast
(** [tuple2g c1 c2] is a cast that takes a pair, applies [c1] on the first part
   and [c2] on the second part and glues the result with a space character. *)

val tuple2 : 'a cast -> ('a * 'a) cast
(** [tuple2 c t = tuple2g c c t]. *)

val pairg : 'a cast -> 'b cast -> ('a * 'b) cast
(** Alias for [tuple2g]. *)

val pair : 'a cast -> ('a * 'a) cast
(** Alias for [tuple2]. *)

val tuple3g : 'a cast -> 'b cast -> 'c cast -> ('a * 'b * 'c) cast

val tuple3 : 'a cast -> ('a * 'a * 'a) cast
(** [tuple3 c s = tuple3g c c c s]. *)

val tuple4g : 'a cast -> 'b cast -> 'c cast -> 'd cast -> ('a * 'b * 'c * 'd) cast

val tuple4 : 'a cast -> ('a * 'a * 'a * 'a) cast
(** [tuple4 c s = tuple4g c c c c s]. *)

val tuple5g :
  'a cast ->
  'b cast ->
  'c cast ->
  'd cast ->
  'e cast ->
  ('a * 'b * 'c * 'd * 'e) cast

val tuple5 : 'a cast -> ('a * 'a * 'a * 'a * 'a) cast
(** [tuple5 c s = tuple5g c c c c c s]. *)

(** {2 Reader} *)

val to_string : 'a cast -> 'a -> string
(** [to_string c v] casts [v] using [c] and returns the result as a string. *)

val line : 'a cast -> 'a -> unit
(** [line c v] casts [v] using [c] and writes the result to standard output,
   adding a newline character. *)

val line_to_err : 'a cast -> 'a -> unit
(** [line_to_err c v] casts [v] using [c] and writes the result to standard
   error, adding a newline character. *)

val line_to_chan : out_channel -> 'a cast -> 'a -> unit
(** [line_to_chan ochan c v] casts [v] using [c] and writes the result to
   [ochan], adding a newline character. *)
