module Suave

module ApplicativeCombinators =

  /// Return success with some value
  val inline succeed : item:'a -> 'a option

  /// Return failure without any value
  val fail : 'a option

  /// Return failure with a value that is ignored
  val inline never : x:'a -> 'b option

  /// bind f inp evaluates to match inp with None -> None | Some x -> f x
  /// The same as Option.bind.
  val bind : (('a -> 'b option) -> 'a option -> 'b option)

  /// Delay the computation of f
  val inline delay : f:(unit -> 'a) -> 'a

  /// Compose (bind) two arguments, 'a' and 'b', so that the result of
  /// the composition can be applied to an argument of 'a' and then passed
  /// to 'b', if 'a' yields a value.
  val inline (>>=) : first:('a -> 'b option) -> second:('b -> 'c option) -> input:'a -> 'c option

  /// Left-to-right Kleisli composition of monads.
  val inline (>=>) : first:('a -> 'b option) -> second:('a -> 'b option) -> input:'a -> 'b option

  /// Entry-point for composing the applicative routes of the http application,
  /// by iterating the options, applying the context, arg, to the predicate
  /// from the list of options, until there's a match/a Some(x) which can be
  /// run.
  val choose : options:('a -> 'b option) list -> input:'a -> 'b option

  /// Pipe the request through to a bird that can peck at it.
  val inline warbler : f:('a -> 'a -> 'b) -> 'a -> 'b

  /// Compose an applicative value with a 'warble' function and then returns
  /// a new function that is the composition of 'app' and 'warble'.
  val inline (>>==) : app:('a -> 'b option) -> warble:('b -> 'b -> 'c option) -> ('a -> 'c option)

  /// The constant function, which returns its constant, no matter
  /// its input.
  /// - theorem: identity = (cnst |> warbler)
  /// (warbler cnst) x = cnst x x = fun _ -> x
  val inline cnst : x:'a -> 'b -> 'a

  /// The conditional function that applies f x a if there's a value in d,
  /// or otherwise, applies g a, if there is no value in d.
  val cond : item:'a option -> f:('a -> 'b -> 'c) -> g:('b -> 'c) -> 'b -> 'c
