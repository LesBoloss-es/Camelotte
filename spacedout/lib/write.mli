(** {1 SpacedOut Write} *)

type 'a cast

(** {2 Writers} *)

val string_as : 'a cast -> 'a -> string
(** [to_string c v] casts [v] using [c] and returns the result as a string. *)

val line_as : 'a cast -> out_channel -> 'a -> unit
(** [line c ochan v] casts [v] using [c] and writes the result [ochan], adding a
    newline character. *)

(** {2 Casts} *)

(** {3 Simple} *)

val int : int cast

val bit : bool cast

val float : float cast
(** Cast a float using the %g specification (see Printf). *)

val char : char cast

val string : string cast

val seq : ?sep: string -> 'a cast -> 'a Seq.t cast
val list : ?sep: string -> 'a cast -> 'a list cast
val array : ?sep: string -> 'a cast -> 'a array cast

(** {3 Tuples}

   For each tuple size [n] (up to 9), we provide a function [tuple_<n>] as well
   as some common aliases (eg. [pair] for [tuple_2] or [pentuple] for
   [tuple_5]). [tuple_<n>] takes [n] cast functions and returns a cast for the
   corresponding [n]-tuples.

   [tuple_<n> ~sep c1 ... c<n>] is a cast that applies [c1] ... [c<n>] on its
   [n] inputs and catenates them using [sep]s. *)

val tuple_2 : ?sep: string -> 'a cast -> 'b cast -> ('a * 'b) cast
val pair : ?sep: string -> 'a cast -> 'b cast -> ('a * 'b) cast
val couple : ?sep: string -> 'a cast -> 'b cast -> ('a * 'b) cast

val tuple_3 : ?sep: string -> 'a cast -> 'b cast -> 'c cast -> ('a * 'b * 'c) cast
val triple : ?sep: string -> 'a cast -> 'b cast -> 'c cast -> ('a * 'b * 'c) cast

val tuple_4 : ?sep: string -> 'a cast -> 'b cast -> 'c cast -> 'd cast -> ('a * 'b * 'c * 'd) cast
val quadruple : ?sep: string -> 'a cast -> 'b cast -> 'c cast -> 'd cast -> ('a * 'b * 'c * 'd) cast

val tuple_5 : ?sep: string -> 'a cast -> 'b cast -> 'c cast -> 'd cast -> 'e cast -> ('a * 'b * 'c * 'd * 'e) cast
val pentuple : ?sep: string -> 'a cast -> 'b cast -> 'c cast -> 'd cast -> 'e cast -> ('a * 'b * 'c * 'd * 'e) cast
val quintuple : ?sep: string -> 'a cast -> 'b cast -> 'c cast -> 'd cast -> 'e cast -> ('a * 'b * 'c * 'd * 'e) cast

val tuple_6 : ?sep: string -> 'a cast -> 'b cast -> 'c cast -> 'd cast -> 'e cast -> 'f cast -> ('a * 'b * 'c * 'd * 'e * 'f) cast
val sextuple : ?sep: string -> 'a cast -> 'b cast -> 'c cast -> 'd cast -> 'e cast -> 'f cast -> ('a * 'b * 'c * 'd * 'e * 'f) cast
val hextuple : ?sep: string -> 'a cast -> 'b cast -> 'c cast -> 'd cast -> 'e cast -> 'f cast -> ('a * 'b * 'c * 'd * 'e * 'f) cast

val tuple_7 : ?sep: string -> 'a cast -> 'b cast -> 'c cast -> 'd cast -> 'e cast -> 'f cast -> 'g cast -> ('a * 'b * 'c * 'd * 'e * 'f * 'g) cast
val septuple : ?sep: string -> 'a cast -> 'b cast -> 'c cast -> 'd cast -> 'e cast -> 'f cast -> 'g cast -> ('a * 'b * 'c * 'd * 'e * 'f * 'g) cast
val heptuple : ?sep: string -> 'a cast -> 'b cast -> 'c cast -> 'd cast -> 'e cast -> 'f cast -> 'g cast -> ('a * 'b * 'c * 'd * 'e * 'f * 'g) cast

val tuple_8 : ?sep: string -> 'a cast -> 'b cast -> 'c cast -> 'd cast -> 'e cast -> 'f cast -> 'g cast -> 'h cast -> ('a * 'b * 'c * 'd * 'e * 'f * 'g * 'h) cast
val octuple : ?sep: string -> 'a cast -> 'b cast -> 'c cast -> 'd cast -> 'e cast -> 'f cast -> 'g cast -> 'h cast -> ('a * 'b * 'c * 'd * 'e * 'f * 'g * 'h) cast

val tuple_9 : ?sep: string -> 'a cast -> 'b cast -> 'c cast -> 'd cast -> 'e cast -> 'f cast -> 'g cast -> 'h cast -> 'i cast -> ('a * 'b * 'c * 'd * 'e * 'f * 'g * 'h * 'i) cast
val nonuple : ?sep: string -> 'a cast -> 'b cast -> 'c cast -> 'd cast -> 'e cast -> 'f cast -> 'g cast -> 'h cast -> 'i cast -> ('a * 'b * 'c * 'd * 'e * 'f * 'g * 'h * 'i) cast
