module type MONOID = sig
	type element
	val zero1: element
	val zero2: element
	val mul: element -> element -> element
	val add: element -> element -> element
	val div: element -> element -> element
	val sub: element -> element -> element
end

module INT : MONOID with type element = int = struct
	type element = int
	let zero1 = 0
	let zero2 = 1
	let mul = ( * )
	let add = ( + )
	let div = ( / )
	let sub = ( - )
end

module FLOAT : MONOID with type element = float = struct
	type element = float
	let zero1 = 0.0
	let zero2 = 1.0
	let mul = ( *. )
	let add = ( +. )
	let div = ( /. )
	let sub = ( -. )
end

module type CALC = functor (M : MONOID) -> sig
	val mul : M.element -> M.element -> M.element
	val add : M.element -> M.element -> M.element
	val div : M.element -> M.element -> M.element
	val sub : M.element -> M.element -> M.element
	val power : M.element -> int -> M.element
	val fact : M.element -> M.element
end

module Calc : CALC = functor (M : MONOID) -> struct
	let mul = M.mul
	let add = M.add
	let div = M.div
	let sub = M.sub
	let rec power x exposant =
		match exposant with
		| 0 -> M.zero2
		| 1 -> x
		| _ -> M.mul x (power x (exposant - 1))
	let rec fact (x : M.element) =
		if x = M.zero1 || x = M.zero2 then M.zero2
		else M.mul x (fact (M.sub x M.zero2))
end
