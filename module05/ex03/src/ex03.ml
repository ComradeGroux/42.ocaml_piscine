module type FIXED = sig
	type t
	val of_float : float -> t
	val of_int : int -> t
	val to_float : t -> float
	val to_int : t -> int
	val to_string : t -> string
	val zero : t
	val one : t
	val succ : t -> t
	val pred : t -> t
	val min : t -> t -> t
	val max : t -> t -> t
	val gth : t -> t -> bool
	val lth : t -> t -> bool
	val gte : t -> t -> bool
	val lte : t -> t -> bool
	val eqp : t -> t -> bool (** physical equality *)
	val eqs : t -> t -> bool (** structural equality *)
	val add : t -> t -> t
	val sub : t -> t -> t
	val mul : t -> t -> t
	val div : t -> t -> t
	val foreach : t -> t -> (t -> unit) -> unit
end

module type FRACTION_BITS = sig
	val bits : int
end

module type MAKE =
	functor (Fraction : FRACTION_BITS) -> FIXED

module Make : MAKE =
	functor (Fraction : FRACTION_BITS) -> struct
	  	type t = int
		let of_int x = x lsl Fraction.bits
		let of_float x = int_of_float (floor (0.5 +. x *. (float_of_int (of_int 1))))
		let to_int x = x lsr Fraction.bits
		let to_float x = (float_of_int x) /. (2.0 ** float_of_int Fraction.bits)
		let to_string x = string_of_float (to_float x)
		let zero = of_int 0
		let one = of_int 1
		let succ t = t + 1
		let pred t = t - 1
		let min = Stdlib.(min)
		let max = Stdlib.(max)
		let gth = Stdlib.(>)
		let lth = Stdlib.(<)
		let gte = Stdlib.(>=)
		let lte = Stdlib.(<=)
		let eqp = Stdlib.(==)
		let eqs = Stdlib.(=)
		let add = Stdlib.(+)
		let sub = Stdlib.(-)
		let mul = Stdlib.( * )
		let div = Stdlib.(/)
		let rec foreach a b c =
			if a > b then ()
			else begin
				c a;
				foreach (a + 1) b c
			end
	end










module Fixed2 : FIXED = Make (struct let bits = 2 end)
module Fixed4 : FIXED = Make (struct let bits = 4 end)
module Fixed8 : FIXED = Make (struct let bits = 8 end)
module Fixed10 : FIXED = Make (struct let bits = 10 end)

let () =
	let x8 = Fixed8.of_float 21.10 in
	let y8 = Fixed8.of_float 21.32 in
	let r8 = Fixed8.add x8 y8 in
	print_endline (Fixed8.to_string r8);
	Fixed4.foreach (Fixed4.zero) (Fixed4.one) (fun f -> print_endline (Fixed4.to_string f)); print_newline () ;
	Fixed2.foreach (Fixed2.zero) (Fixed2.one) (fun f -> print_endline (Fixed2.to_string f)); print_newline () ;
	Fixed10.foreach (Fixed10.zero) (Fixed10.one) (fun f -> print_endline (Fixed10.to_string f));
