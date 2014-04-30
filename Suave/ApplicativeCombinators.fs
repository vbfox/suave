[<AutoOpen>]
module Suave.ApplicativeCombinators

let inline succeed x = Some x

let fail = None

let inline never _ = None

let bind = Option.bind

let inline delay f = f()

let inline (>>=) a b = fun x -> Option.bind b (a x)

let inline (>=>) a b = fun x ->
  match a x with
  | None   -> b x
  | r      -> r

let rec choose options arg =
  match options with
  | []        -> None
  | p :: tail ->
    match p arg with
    | Some x -> Some x
    | None   -> choose tail arg

let inline warbler f a = f a a //which bird? A Warbler!

let inline (>>==) a b = a >>= warbler (fun r -> b r)

let inline cnst x = fun _ -> x

let cond d f g a =
  match d with
  | Some x -> f x a
  | None   -> g a
