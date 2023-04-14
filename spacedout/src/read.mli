(** {1 SpacedOut Read} *)

(** {2 Casts} *)

type 'a cast

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

val list : ?sep:Str.regexp -> 'a cast -> 'a list cast
val array : ?sep:Str.regexp -> 'a cast -> 'a array cast

val non_empty_list : ?sep:Str.regexp -> 'a cast -> 'a list cast
val non_empty_array : ?sep:Str.regexp -> 'a cast -> 'a array cast

(** {3 Tuples}

   For each tuple size n (up to 5), we provide a function [tuple<n>] taking [n]
   cast functions and returning a tuple. *)

val tuple2 : ?sep:Str.regexp -> 'a cast -> 'b cast -> ('a * 'b) cast
(** [tuple2 ~sep cast1 cast2] is a cast that cuts its input at the first
    [sep] (which defaults to blank characters) and applies [cast1] on the
    first part and [cast2] on the second part, returning the 2-tuple (pair) of
    the results. *)

val pair : ?sep:Str.regexp -> 'a cast -> 'b cast -> ('a * 'b) cast
(** Alias for {!tuple2}. *)

val tuple3 : ?sep:Str.regexp -> 'a cast -> 'b cast -> 'c cast -> ('a * 'b * 'c) cast
(** [tuple3 ~sep cast1 cast2 cast3] is a cast that cuts its input at the
    two first [sep]s (which default to blank characters) and applies
    [cast1] on the first part, [cast2] on the second part and [cast3] on the
    third part, returning the 3-tuple of the results. *)

val triple : ?sep:Str.regexp -> 'a cast -> 'b cast -> 'c cast -> ('a * 'b * 'c) cast
(** Alias for {!tuple3}. *)

val tuple4 : ?sep:Str.regexp -> 'a cast -> 'b cast -> 'c cast -> 'd cast -> ('a * 'b * 'c * 'd) cast
(** [tuple4 ~sep cast1 cast2 cast3 cast4] is a cast that cuts its input at
    the three first [sep]s (which default to blank characters) and applies
    [cast1] on the first part, [cast2] on the second part, [cast3] on the third
    part and [cast4] on the fourth part, returning the 4-tuple of the
    results. *)

val tuple5 : ?sep:Str.regexp -> 'a cast -> 'b cast -> 'c cast -> 'd cast -> 'e cast -> ('a * 'b * 'c * 'd * 'e) cast
(** [tuple5 ~sep cast1 cast2 cast3 cast4 cast5] is a cast that cuts its
    input at the first four [sep]s (which default to blank characters) and
    applies [cast1] on the first part, [cast2] on the second part, [cast3] on
    the third part, [cast4] on the fourth part and [cast5] on the fifth part,
    returning the 5-tuple of the results. *)

val tuple6 : ?sep:Str.regexp -> 'a cast -> 'b cast -> 'c cast -> 'd cast -> 'e cast -> 'f cast -> ('a * 'b * 'c * 'd * 'e * 'f) cast
(** [tuple6 ~sep cast1 cast2 cast3 cast4 cast5 cast6] is a cast that cuts its
    input at the first five [sep]s (which default to blank characters) and
    applies [cast1] on the first part, [cast2] on the second part, [cast3] on
    the third part, [cast4] on the fourth part, [cast5] on the fifth part and
    [cast6] on the sixth part, returning the 6-tuple of the results. *)

(** {3 Custom} *)

val cast : (string -> 'a) -> 'a cast
(** Create a cast from a [string -> 'a] function. *)

(** {2 Reader} *)

val of_string : 'a cast -> string -> 'a
(** [string c s] reads [s] and casts it using [c]. *)

val line : 'a cast -> 'a
(** [line c] reads one line from standard input and casts it using [c]. *)

val line_of_chan : in_channel -> 'a cast -> 'a
(** [line_from_chan ichan c] reads one line from [ichan] and casts it using [c]. *)

val lines_until_empty : 'a cast -> 'a list
(** [lines_until_empty c] reads lines from standard input and casts them using
    [c]. It stops at the first empty line it meets. *)

val lines_of_chan_until_empty : in_channel -> 'a cast -> 'a list
(** [lines_of_chan_until_empty ichan c] reads lines from [ichan] and casts them
    using [c]. It stops at the first empty line it meets. *)
