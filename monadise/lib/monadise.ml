(** {1 Monadise}

    Make any direct-style function monadic without effort, but with effects! *)

module type Monad = sig
  type 'a t
  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
end

module type S = sig
  type 'a m

  (** {2 One-function variant} *)

  val monadise :
    (('a -> 'b) -> 'c) ->
    (('a -> 'b m) -> 'c m)
  (** The core function of this library. Takes a higher-order function consuming
      a direct-style action, and makes it into a function consuming a monadic
      action for the given monad. *)

  (** {2 Two-functions variant}

      Say you want to map a function [f] of type ['a -> 'b m] on an
      ['a list array]. With {!monadise}, you would need to do
      {[
        monadise_1_1 Array.map (fun xs -> monadise_1_1 List.map (fun x -> f x)) xss
      ]}
      or the shorter
      {[
        monadise_1_1 Array.map monadise_1_1 List.map f
      ]}
      which, either way, means calling {!monadise} twice.

      With the two-functions variant, one only needs to install the
      handler once with {!run}, and then one can {!yield} anywhere in
      the context. For instance, the above code becomes:
      {[
        run @@ fun () ->
        Array.map
          (fun xs ->
            List.map (fun x -> yield (f x)))
          xss
      ]}
      This also avoids having to think of the number of arguments. *)

  val run : (unit -> 'a) -> 'a m
  (** Set up the context in which to call {!yield}. It is safe,
      although useless, to nest this function. *)

  val yield : 'a m -> 'a
  (** Transforms a monadic value into a direct value. This only works
      in the context of {!run}, without which you will get a runtime
      exception [Stdlib.Effect.Unhandled(Yield(_))] *)

  (** {2 Variations over [monadise]}

      Everything can be derived from {!monadise}. However, to save the user some
      gymnasics, we provide a bunch of helpers for functions with different
      numbers of arguments. *)

  (** {3 Action arguments}

      These helpers come in the form [monadise_<n>] where [<n>] is the number of
      arguments of the action. They are provided for [<n>] up to [5]. For
      instance, {!monadise} would be [monadise_1]. *)

  val monadise_2 :
    (('a1 -> 'a2 -> 'b) -> 'c) ->
    (('a1 -> 'a2 -> 'b m) -> 'c m)

  val monadise_3 :
    (('a1 -> 'a2 -> 'a3 -> 'b) -> 'c) ->
    (('a1 -> 'a2 -> 'a3 -> 'b m) -> 'c m)

  val monadise_4 :
    (('a1 -> 'a2 -> 'a3 -> 'a4 -> 'b) -> 'c) ->
    (('a1 -> 'a2 -> 'a3 -> 'a4 -> 'b m) -> 'c m)

  val monadise_5 :
    (('a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'b) -> 'c) ->
    (('a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'b m) -> 'c m)

  (** {3 Additional arguments}

      These helpers come in the form [monadise_<n>_<m>] where [<n>] is the
      number of arguments of the action, and [<m>] is the number of additional
      arguments to the function. They are provided for [<n>] and [<m>] up to
      [5]. For instance, you would use {!monadise_1_1} on {!List.map} and
      {!monadise_2_2} on {!List.fold_left}. *)

  val monadise_1_1 :
    (('a -> 'b) -> 'c -> 'd) ->
    (('a -> 'b m) -> 'c -> 'd m)

  val monadise_1_2 :
    (('a -> 'b) -> 'c1 -> 'c2 -> 'd) ->
    (('a -> 'b m) -> 'c1 -> 'c2 -> 'd m)

  val monadise_1_3 :
    (('a -> 'b) -> 'c1 -> 'c2 -> 'c3 -> 'd) ->
    (('a -> 'b m) -> 'c1 -> 'c2 -> 'c3 -> 'd m)

  val monadise_1_4 :
    (('a -> 'b) -> 'c1 -> 'c2 -> 'c3 -> 'c4 -> 'd) ->
    (('a -> 'b m) -> 'c1 -> 'c2 -> 'c3 -> 'c4 -> 'd m)

  val monadise_1_5 :
    (('a -> 'b) -> 'c1 -> 'c2 -> 'c3 -> 'c4 -> 'c5 -> 'd) ->
    (('a -> 'b m) -> 'c1 -> 'c2 -> 'c3 -> 'c4 -> 'c5 -> 'd m)

  val monadise_2_1 :
    (('a1 -> 'a2 -> 'b) -> 'c -> 'd) ->
    (('a1 -> 'a2 -> 'b m) -> 'c -> 'd m)

  val monadise_2_2 :
    (('a1 -> 'a2 -> 'b) -> 'c1 -> 'c2 -> 'd) ->
    (('a1 -> 'a2 -> 'b m) -> 'c1 -> 'c2 -> 'd m)

  val monadise_2_3 :
    (('a1 -> 'a2 -> 'b) -> 'c1 -> 'c2 -> 'c3 -> 'd) ->
    (('a1 -> 'a2 -> 'b m) -> 'c1 -> 'c2 -> 'c3 -> 'd m)

  val monadise_2_4 :
    (('a1 -> 'a2 -> 'b) -> 'c1 -> 'c2 -> 'c3 -> 'c4 -> 'd) ->
    (('a1 -> 'a2 -> 'b m) -> 'c1 -> 'c2 -> 'c3 -> 'c4 -> 'd m)

  val monadise_2_5 :
    (('a1 -> 'a2 -> 'b) -> 'c1 -> 'c2 -> 'c3 -> 'c4 -> 'c5 -> 'd) ->
    (('a1 -> 'a2 -> 'b m) -> 'c1 -> 'c2 -> 'c3 -> 'c4 -> 'c5 -> 'd m)

  val monadise_3_1 :
    (('a1 -> 'a2 -> 'a3 -> 'b) -> 'c -> 'd) ->
    (('a1 -> 'a2 -> 'a3 -> 'b m) -> 'c -> 'd m)

  val monadise_3_2 :
    (('a1 -> 'a2 -> 'a3 -> 'b) -> 'c1 -> 'c2 -> 'd) ->
    (('a1 -> 'a2 -> 'a3 -> 'b m) -> 'c1 -> 'c2 -> 'd m)

  val monadise_3_3 :
    (('a1 -> 'a2 -> 'a3 -> 'b) -> 'c1 -> 'c2 -> 'c3 -> 'd) ->
    (('a1 -> 'a2 -> 'a3 -> 'b m) -> 'c1 -> 'c2 -> 'c3 -> 'd m)

  val monadise_3_4 :
    (('a1 -> 'a2 -> 'a3 -> 'b) -> 'c1 -> 'c2 -> 'c3 -> 'c4 -> 'd) ->
    (('a1 -> 'a2 -> 'a3 -> 'b m) -> 'c1 -> 'c2 -> 'c3 -> 'c4 -> 'd m)

  val monadise_3_5 :
    (('a1 -> 'a2 -> 'a3 -> 'b) -> 'c1 -> 'c2 -> 'c3 -> 'c4 -> 'c5 -> 'd) ->
    (('a1 -> 'a2 -> 'a3 -> 'b m) -> 'c1 -> 'c2 -> 'c3 -> 'c4 -> 'c5 -> 'd m)

  val monadise_4_1 :
    (('a1 -> 'a2 -> 'a3 -> 'a4 -> 'b) -> 'c -> 'd) ->
    (('a1 -> 'a2 -> 'a3 -> 'a4 -> 'b m) -> 'c -> 'd m)

  val monadise_4_2 :
    (('a1 -> 'a2 -> 'a3 -> 'a4 -> 'b) -> 'c1 -> 'c2 -> 'd) ->
    (('a1 -> 'a2 -> 'a3 -> 'a4 -> 'b m) -> 'c1 -> 'c2 -> 'd m)

  val monadise_4_3 :
    (('a1 -> 'a2 -> 'a3 -> 'a4 -> 'b) -> 'c1 -> 'c2 -> 'c3 -> 'd) ->
    (('a1 -> 'a2 -> 'a3 -> 'a4 -> 'b m) -> 'c1 -> 'c2 -> 'c3 -> 'd m)

  val monadise_4_4 :
    (('a1 -> 'a2 -> 'a3 -> 'a4 -> 'b) -> 'c1 -> 'c2 -> 'c3 -> 'c4 -> 'd) ->
    (('a1 -> 'a2 -> 'a3 -> 'a4 -> 'b m) -> 'c1 -> 'c2 -> 'c3 -> 'c4 -> 'd m)

  val monadise_4_5 :
    (('a1 -> 'a2 -> 'a3 -> 'a4 -> 'b) -> 'c1 -> 'c2 -> 'c3 -> 'c4 -> 'c5 -> 'd) ->
    (('a1 -> 'a2 -> 'a3 -> 'a4 -> 'b m) -> 'c1 -> 'c2 -> 'c3 -> 'c4 -> 'c5 -> 'd m)

  val monadise_5_1 :
    (('a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'b) -> 'c -> 'd) ->
    (('a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'b m) -> 'c -> 'd m)

  val monadise_5_2 :
    (('a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'b) -> 'c1 -> 'c2 -> 'd) ->
    (('a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'b m) -> 'c1 -> 'c2 -> 'd m)

  val monadise_5_3 :
    (('a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'b) -> 'c1 -> 'c2 -> 'c3 -> 'd) ->
    (('a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'b m) -> 'c1 -> 'c2 -> 'c3 -> 'd m)

  val monadise_5_4 :
    (('a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'b) -> 'c1 -> 'c2 -> 'c3 -> 'c4 -> 'd) ->
    (('a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'b m) -> 'c1 -> 'c2 -> 'c3 -> 'c4 -> 'd m)

  val monadise_5_5 :
    (('a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'b) -> 'c1 -> 'c2 -> 'c3 -> 'c4 -> 'c5 -> 'd) ->
    (('a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'b m) -> 'c1 -> 'c2 -> 'c3 -> 'c4 -> 'c5 -> 'd m)
end

module Make (M : Monad) : S with type 'a m = 'a M.t = struct
  type 'a m = 'a M.t

  type _ Effect.t += Yield : 'a m -> 'a Effect.t

  let run (f : unit -> 'a) : 'a m =
    match f () with
    | v -> M.return v
    | effect (Yield x), k -> M.bind x (Effect.Deep.continue k)

  let yield (x : 'a m) : 'a =
    Effect.perform (Yield x)

  let monadise f = fun a ->
    run (fun () -> f (fun x -> yield (a x)))

  let monadise_2 f = fun a -> monadise (fun a -> f (fun x y -> a (x, y))) (fun (x, y) -> a x y)
  let monadise_3 f = fun a -> monadise (fun a -> f (fun x y z -> a (x, y, z))) (fun (x, y, z) -> a x y z)
  let monadise_4 f = fun a -> monadise (fun a -> f (fun x y z u -> a (x, y, z, u))) (fun (x, y, z, u) -> a x y z u)
  let monadise_5 f = fun a -> monadise (fun a -> f (fun x y z u v -> a (x, y, z, u, v))) (fun (x, y, z, u, v) -> a x y z u v)

  let monadise_1_1 f = fun a x -> monadise (fun a -> f a x) a
  let monadise_1_2 f = fun a x y -> monadise (fun a -> f a x y) a
  let monadise_1_3 f = fun a x y z -> monadise (fun a -> f a x y z) a
  let monadise_1_4 f = fun a x y z u -> monadise (fun a -> f a x y z u) a
  let monadise_1_5 f = fun a x y z u v -> monadise (fun a -> f a x y z u v) a

  let monadise_2_1 f = fun a x -> monadise_2 (fun a -> f a x) a
  let monadise_2_2 f = fun a x y -> monadise_2 (fun a -> f a x y) a
  let monadise_2_3 f = fun a x y z -> monadise_2 (fun a -> f a x y z) a
  let monadise_2_4 f = fun a x y z u -> monadise_2 (fun a -> f a x y z u) a
  let monadise_2_5 f = fun a x y z u v -> monadise_2 (fun a -> f a x y z u v) a

  let monadise_3_1 f = fun a x -> monadise_3 (fun a -> f a x) a
  let monadise_3_2 f = fun a x y -> monadise_3 (fun a -> f a x y) a
  let monadise_3_3 f = fun a x y z -> monadise_3 (fun a -> f a x y z) a
  let monadise_3_4 f = fun a x y z u -> monadise_3 (fun a -> f a x y z u) a
  let monadise_3_5 f = fun a x y z u v -> monadise_3 (fun a -> f a x y z u v) a

  let monadise_4_1 f = fun a x -> monadise_4 (fun a -> f a x) a
  let monadise_4_2 f = fun a x y -> monadise_4 (fun a -> f a x y) a
  let monadise_4_3 f = fun a x y z -> monadise_4 (fun a -> f a x y z) a
  let monadise_4_4 f = fun a x y z u -> monadise_4 (fun a -> f a x y z u) a
  let monadise_4_5 f = fun a x y z u v -> monadise_4 (fun a -> f a x y z u v) a

  let monadise_5_1 f = fun a x -> monadise_5 (fun a -> f a x) a
  let monadise_5_2 f = fun a x y -> monadise_5 (fun a -> f a x y) a
  let monadise_5_3 f = fun a x y z -> monadise_5 (fun a -> f a x y z) a
  let monadise_5_4 f = fun a x y z u -> monadise_5 (fun a -> f a x y z u) a
  let monadise_5_5 f = fun a x y z u v -> monadise_5 (fun a -> f a x y z u v) a
end

(** {2 Monadisation for some standard monads} *)

module Option = Make(struct
  type 'a t = 'a option
  let return = Option.some
  let bind = Option.bind
end)
