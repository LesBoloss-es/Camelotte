(** {1 SpacedOut Read} *)

type 'a cast

(** {2 Readers} *)

val string_as : 'a cast -> string -> 'a
(** [string_as c] reads a string and casts it using [c]. *)

val line_as : 'a cast -> in_channel -> 'a
(** [line_as c] reads one line from a channel and casts it using [c]. *)

val lines_until_empty_as : 'a cast -> in_channel -> 'a list
(** [lines_until_empty_as c] reads lines from a channel and casts them using
    [c]. It stops at the first empty line it meets. *)

(** {2 Casts} *)

(** {3 Simple} *)

val int : int cast
val bit : bool cast
val float : float cast
val char : char cast
val string : string cast

val no_space_string : string cast
(** Cast that checks that its input does not contain a space and returns it
   right away. In [seq], [list] and [array] below, it is no different to
   [string]. In the [tupleng] functions below, it is only different to [string]
   in last position. *)

val seq : ?sep: Str.regexp -> 'a cast -> 'a Seq.t cast
val list : ?sep: Str.regexp -> 'a cast -> 'a list cast
val array : ?sep: Str.regexp -> 'a cast -> 'a array cast

val non_empty_seq : ?sep: Str.regexp -> 'a cast -> 'a Seq.t cast
val non_empty_list : ?sep: Str.regexp -> 'a cast -> 'a list cast
val non_empty_array : ?sep: Str.regexp -> 'a cast -> 'a array cast

(** {3 Tuples}

   For each tuple size [n] (up to 9), we provide a function [tuple_<n>] as well
   as some common aliases (eg. [pair] for [tuple_2] or [pentuple] for
   [tuple_5]). [tuple_<n>] takes [n] cast functions and returns an [n]-tuple.

   [tuple_<n> ~sep c1 ... c<n>] is a cast that cuts its input at the [n - 1]
   first [sep]s (which default to blank characters) and applies [c1] on the
   first part, [c2] on the second part, ... and [c<n>] on the [n]th part,
   returning the [n]-tuple of the results. *)

val tuple_2 : ?sep: Str.regexp -> 'a cast -> 'b cast -> ('a * 'b) cast
val pair : ?sep: Str.regexp -> 'a cast -> 'b cast -> ('a * 'b) cast
val couple : ?sep: Str.regexp -> 'a cast -> 'b cast -> ('a * 'b) cast

val tuple_3 : ?sep: Str.regexp -> 'a cast -> 'b cast -> 'c cast -> ('a * 'b * 'c) cast
val triple : ?sep: Str.regexp -> 'a cast -> 'b cast -> 'c cast -> ('a * 'b * 'c) cast

val tuple_4 : ?sep: Str.regexp -> 'a cast -> 'b cast -> 'c cast -> 'd cast -> ('a * 'b * 'c * 'd) cast
val quadruple : ?sep: Str.regexp -> 'a cast -> 'b cast -> 'c cast -> 'd cast -> ('a * 'b * 'c * 'd) cast

val tuple_5 : ?sep: Str.regexp -> 'a cast -> 'b cast -> 'c cast -> 'd cast -> 'e cast -> ('a * 'b * 'c * 'd * 'e) cast
val pentuple : ?sep: Str.regexp -> 'a cast -> 'b cast -> 'c cast -> 'd cast -> 'e cast -> ('a * 'b * 'c * 'd * 'e) cast
val quintuple : ?sep: Str.regexp -> 'a cast -> 'b cast -> 'c cast -> 'd cast -> 'e cast -> ('a * 'b * 'c * 'd * 'e) cast

val tuple_6 : ?sep: Str.regexp -> 'a cast -> 'b cast -> 'c cast -> 'd cast -> 'e cast -> 'f cast -> ('a * 'b * 'c * 'd * 'e * 'f) cast
val sextuple : ?sep: Str.regexp -> 'a cast -> 'b cast -> 'c cast -> 'd cast -> 'e cast -> 'f cast -> ('a * 'b * 'c * 'd * 'e * 'f) cast
val hextuple : ?sep: Str.regexp -> 'a cast -> 'b cast -> 'c cast -> 'd cast -> 'e cast -> 'f cast -> ('a * 'b * 'c * 'd * 'e * 'f) cast

val tuple_7 : ?sep: Str.regexp -> 'a cast -> 'b cast -> 'c cast -> 'd cast -> 'e cast -> 'f cast -> 'g cast -> ('a * 'b * 'c * 'd * 'e * 'f * 'g) cast
val septuple : ?sep: Str.regexp -> 'a cast -> 'b cast -> 'c cast -> 'd cast -> 'e cast -> 'f cast -> 'g cast -> ('a * 'b * 'c * 'd * 'e * 'f * 'g) cast
val heptuple : ?sep: Str.regexp -> 'a cast -> 'b cast -> 'c cast -> 'd cast -> 'e cast -> 'f cast -> 'g cast -> ('a * 'b * 'c * 'd * 'e * 'f * 'g) cast

val tuple_8 : ?sep: Str.regexp -> 'a cast -> 'b cast -> 'c cast -> 'd cast -> 'e cast -> 'f cast -> 'g cast -> 'h cast -> ('a * 'b * 'c * 'd * 'e * 'f * 'g * 'h) cast
val octuple : ?sep: Str.regexp -> 'a cast -> 'b cast -> 'c cast -> 'd cast -> 'e cast -> 'f cast -> 'g cast -> 'h cast -> ('a * 'b * 'c * 'd * 'e * 'f * 'g * 'h) cast

val tuple_9 : ?sep: Str.regexp -> 'a cast -> 'b cast -> 'c cast -> 'd cast -> 'e cast -> 'f cast -> 'g cast -> 'h cast -> 'i cast -> ('a * 'b * 'c * 'd * 'e * 'f * 'g * 'h * 'i) cast
val nonuple : ?sep: Str.regexp -> 'a cast -> 'b cast -> 'c cast -> 'd cast -> 'e cast -> 'f cast -> 'g cast -> 'h cast -> 'i cast -> ('a * 'b * 'c * 'd * 'e * 'f * 'g * 'h * 'i) cast

(** {3 Custom} *)

val cast : (string -> 'a) -> 'a cast
(** Create a cast from a [string -> 'a] function. *)
