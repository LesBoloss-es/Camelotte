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

  val lift :
    (('a -> 'b) -> 'c) ->
    (('a -> 'b m) -> 'c m)
  (** The core function of this library. Takes a higher-order function consuming
      a direct-style action, and makes it into a function consuming a monadic
      action for the given monad. *)

  (** {2 Two-functions variant}

      Say you want to map a function [f] of type ['a -> 'b m] on an
      ['a list array]. With {!lift}, you would need to do
      {[
        lift_1_1 Array.map (fun xs -> lift_1_1 List.map (fun x -> f x)) xss
      ]}
      or the shorter
      {[
        lift_1_1 Array.map lift_1_1 List.map f
      ]}
      which, either way, means calling {!lift} twice.

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

  (** {2 Variations over {!lift}}

      Everything can be derived from {!lift}. However, to save the user some
      gymnasics, we provide a bunch of helpers for functions with different
      numbers of arguments. *)

  (** {3 Action arguments}

      These helpers come in the form [lift_<n>] where [<n>] is the number of
      arguments of the action. They are provided for [<n>] up to [5]. For
      instance, {!lift} would be [lift_1]. *)

  val lift_2 :
    (('a1 -> 'a2 -> 'b) -> 'c) ->
    (('a1 -> 'a2 -> 'b m) -> 'c m)

  val lift_3 :
    (('a1 -> 'a2 -> 'a3 -> 'b) -> 'c) ->
    (('a1 -> 'a2 -> 'a3 -> 'b m) -> 'c m)

  val lift_4 :
    (('a1 -> 'a2 -> 'a3 -> 'a4 -> 'b) -> 'c) ->
    (('a1 -> 'a2 -> 'a3 -> 'a4 -> 'b m) -> 'c m)

  val lift_5 :
    (('a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'b) -> 'c) ->
    (('a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'b m) -> 'c m)

  (** {3 Additional arguments}

      These helpers come in the form [lift_<n>_<m>] where [<n>] is the
      number of arguments of the action, and [<m>] is the number of additional
      arguments to the function. They are provided for [<n>] and [<m>] up to
      [5]. For instance, you would use {!lift_1_1} on {!List.map} and
      {!lift_2_2} on {!List.fold_left}. *)

  val lift_1_1 :
    (('a -> 'b) -> 'c -> 'd) ->
    (('a -> 'b m) -> 'c -> 'd m)

  val lift_1_2 :
    (('a -> 'b) -> 'c1 -> 'c2 -> 'd) ->
    (('a -> 'b m) -> 'c1 -> 'c2 -> 'd m)

  val lift_1_3 :
    (('a -> 'b) -> 'c1 -> 'c2 -> 'c3 -> 'd) ->
    (('a -> 'b m) -> 'c1 -> 'c2 -> 'c3 -> 'd m)

  val lift_1_4 :
    (('a -> 'b) -> 'c1 -> 'c2 -> 'c3 -> 'c4 -> 'd) ->
    (('a -> 'b m) -> 'c1 -> 'c2 -> 'c3 -> 'c4 -> 'd m)

  val lift_1_5 :
    (('a -> 'b) -> 'c1 -> 'c2 -> 'c3 -> 'c4 -> 'c5 -> 'd) ->
    (('a -> 'b m) -> 'c1 -> 'c2 -> 'c3 -> 'c4 -> 'c5 -> 'd m)

  val lift_2_1 :
    (('a1 -> 'a2 -> 'b) -> 'c -> 'd) ->
    (('a1 -> 'a2 -> 'b m) -> 'c -> 'd m)

  val lift_2_2 :
    (('a1 -> 'a2 -> 'b) -> 'c1 -> 'c2 -> 'd) ->
    (('a1 -> 'a2 -> 'b m) -> 'c1 -> 'c2 -> 'd m)

  val lift_2_3 :
    (('a1 -> 'a2 -> 'b) -> 'c1 -> 'c2 -> 'c3 -> 'd) ->
    (('a1 -> 'a2 -> 'b m) -> 'c1 -> 'c2 -> 'c3 -> 'd m)

  val lift_2_4 :
    (('a1 -> 'a2 -> 'b) -> 'c1 -> 'c2 -> 'c3 -> 'c4 -> 'd) ->
    (('a1 -> 'a2 -> 'b m) -> 'c1 -> 'c2 -> 'c3 -> 'c4 -> 'd m)

  val lift_2_5 :
    (('a1 -> 'a2 -> 'b) -> 'c1 -> 'c2 -> 'c3 -> 'c4 -> 'c5 -> 'd) ->
    (('a1 -> 'a2 -> 'b m) -> 'c1 -> 'c2 -> 'c3 -> 'c4 -> 'c5 -> 'd m)

  val lift_3_1 :
    (('a1 -> 'a2 -> 'a3 -> 'b) -> 'c -> 'd) ->
    (('a1 -> 'a2 -> 'a3 -> 'b m) -> 'c -> 'd m)

  val lift_3_2 :
    (('a1 -> 'a2 -> 'a3 -> 'b) -> 'c1 -> 'c2 -> 'd) ->
    (('a1 -> 'a2 -> 'a3 -> 'b m) -> 'c1 -> 'c2 -> 'd m)

  val lift_3_3 :
    (('a1 -> 'a2 -> 'a3 -> 'b) -> 'c1 -> 'c2 -> 'c3 -> 'd) ->
    (('a1 -> 'a2 -> 'a3 -> 'b m) -> 'c1 -> 'c2 -> 'c3 -> 'd m)

  val lift_3_4 :
    (('a1 -> 'a2 -> 'a3 -> 'b) -> 'c1 -> 'c2 -> 'c3 -> 'c4 -> 'd) ->
    (('a1 -> 'a2 -> 'a3 -> 'b m) -> 'c1 -> 'c2 -> 'c3 -> 'c4 -> 'd m)

  val lift_3_5 :
    (('a1 -> 'a2 -> 'a3 -> 'b) -> 'c1 -> 'c2 -> 'c3 -> 'c4 -> 'c5 -> 'd) ->
    (('a1 -> 'a2 -> 'a3 -> 'b m) -> 'c1 -> 'c2 -> 'c3 -> 'c4 -> 'c5 -> 'd m)

  val lift_4_1 :
    (('a1 -> 'a2 -> 'a3 -> 'a4 -> 'b) -> 'c -> 'd) ->
    (('a1 -> 'a2 -> 'a3 -> 'a4 -> 'b m) -> 'c -> 'd m)

  val lift_4_2 :
    (('a1 -> 'a2 -> 'a3 -> 'a4 -> 'b) -> 'c1 -> 'c2 -> 'd) ->
    (('a1 -> 'a2 -> 'a3 -> 'a4 -> 'b m) -> 'c1 -> 'c2 -> 'd m)

  val lift_4_3 :
    (('a1 -> 'a2 -> 'a3 -> 'a4 -> 'b) -> 'c1 -> 'c2 -> 'c3 -> 'd) ->
    (('a1 -> 'a2 -> 'a3 -> 'a4 -> 'b m) -> 'c1 -> 'c2 -> 'c3 -> 'd m)

  val lift_4_4 :
    (('a1 -> 'a2 -> 'a3 -> 'a4 -> 'b) -> 'c1 -> 'c2 -> 'c3 -> 'c4 -> 'd) ->
    (('a1 -> 'a2 -> 'a3 -> 'a4 -> 'b m) -> 'c1 -> 'c2 -> 'c3 -> 'c4 -> 'd m)

  val lift_4_5 :
    (('a1 -> 'a2 -> 'a3 -> 'a4 -> 'b) -> 'c1 -> 'c2 -> 'c3 -> 'c4 -> 'c5 -> 'd) ->
    (('a1 -> 'a2 -> 'a3 -> 'a4 -> 'b m) -> 'c1 -> 'c2 -> 'c3 -> 'c4 -> 'c5 -> 'd m)

  val lift_5_1 :
    (('a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'b) -> 'c -> 'd) ->
    (('a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'b m) -> 'c -> 'd m)

  val lift_5_2 :
    (('a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'b) -> 'c1 -> 'c2 -> 'd) ->
    (('a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'b m) -> 'c1 -> 'c2 -> 'd m)

  val lift_5_3 :
    (('a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'b) -> 'c1 -> 'c2 -> 'c3 -> 'd) ->
    (('a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'b m) -> 'c1 -> 'c2 -> 'c3 -> 'd m)

  val lift_5_4 :
    (('a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'b) -> 'c1 -> 'c2 -> 'c3 -> 'c4 -> 'd) ->
    (('a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'b m) -> 'c1 -> 'c2 -> 'c3 -> 'c4 -> 'd m)

  val lift_5_5 :
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

  let lift f = fun a ->
    run (fun () -> f (fun x -> yield (a x)))

  let lift_2 f = fun a -> lift (fun a -> f (fun x y -> a (x, y))) (fun (x, y) -> a x y)
  let lift_3 f = fun a -> lift (fun a -> f (fun x y z -> a (x, y, z))) (fun (x, y, z) -> a x y z)
  let lift_4 f = fun a -> lift (fun a -> f (fun x y z u -> a (x, y, z, u))) (fun (x, y, z, u) -> a x y z u)
  let lift_5 f = fun a -> lift (fun a -> f (fun x y z u v -> a (x, y, z, u, v))) (fun (x, y, z, u, v) -> a x y z u v)

  let lift_1_1 f = fun a x -> lift (fun a -> f a x) a
  let lift_1_2 f = fun a x y -> lift (fun a -> f a x y) a
  let lift_1_3 f = fun a x y z -> lift (fun a -> f a x y z) a
  let lift_1_4 f = fun a x y z u -> lift (fun a -> f a x y z u) a
  let lift_1_5 f = fun a x y z u v -> lift (fun a -> f a x y z u v) a

  let lift_2_1 f = fun a x -> lift_2 (fun a -> f a x) a
  let lift_2_2 f = fun a x y -> lift_2 (fun a -> f a x y) a
  let lift_2_3 f = fun a x y z -> lift_2 (fun a -> f a x y z) a
  let lift_2_4 f = fun a x y z u -> lift_2 (fun a -> f a x y z u) a
  let lift_2_5 f = fun a x y z u v -> lift_2 (fun a -> f a x y z u v) a

  let lift_3_1 f = fun a x -> lift_3 (fun a -> f a x) a
  let lift_3_2 f = fun a x y -> lift_3 (fun a -> f a x y) a
  let lift_3_3 f = fun a x y z -> lift_3 (fun a -> f a x y z) a
  let lift_3_4 f = fun a x y z u -> lift_3 (fun a -> f a x y z u) a
  let lift_3_5 f = fun a x y z u v -> lift_3 (fun a -> f a x y z u v) a

  let lift_4_1 f = fun a x -> lift_4 (fun a -> f a x) a
  let lift_4_2 f = fun a x y -> lift_4 (fun a -> f a x y) a
  let lift_4_3 f = fun a x y z -> lift_4 (fun a -> f a x y z) a
  let lift_4_4 f = fun a x y z u -> lift_4 (fun a -> f a x y z u) a
  let lift_4_5 f = fun a x y z u v -> lift_4 (fun a -> f a x y z u v) a

  let lift_5_1 f = fun a x -> lift_5 (fun a -> f a x) a
  let lift_5_2 f = fun a x y -> lift_5 (fun a -> f a x y) a
  let lift_5_3 f = fun a x y z -> lift_5 (fun a -> f a x y z) a
  let lift_5_4 f = fun a x y z u -> lift_5 (fun a -> f a x y z u) a
  let lift_5_5 f = fun a x y z u v -> lift_5 (fun a -> f a x y z u v) a
end

(** {2 Monadisation for some standard monads} *)

module Option = Make(struct
  type 'a t = 'a option
  let return = Option.some
  let bind = Option.bind
end)
