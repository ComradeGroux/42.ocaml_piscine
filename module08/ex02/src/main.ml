module Calc_int = Calc.Calc(Calc.INT)
module Calc_float = Calc.Calc(Calc.FLOAT)

let () =
	print_endline "=== INT: add / sub / mul / div ===" ;
	Printf.printf "5 + 3 = %d\n" (Calc_int.add 5 3) ;
	Printf.printf "5 - 3 = %d\n" (Calc_int.sub 5 3) ;
	Printf.printf "5 * 3 = %d\n" (Calc_int.mul 5 3) ;
	Printf.printf "6 / 3 = %d\n" (Calc_int.div 6 3) ;

	print_endline "\n=== FLOAT: add / sub / mul / div ===" ;
	Printf.printf "5.0 +. 3.0 = %f\n" (Calc_float.add 5.0 3.0) ;
	Printf.printf "5.0 -. 3.0 = %f\n" (Calc_float.sub 5.0 3.0) ;
	Printf.printf "5.0 *. 3.0 = %f\n" (Calc_float.mul 5.0 3.0) ;
	Printf.printf "6.0 /. 3.0 = %f\n" (Calc_float.div 6.0 3.0) ;

	print_endline "\n=== INT: power ===" ;
	Printf.printf "2 ^ 0 = %d\n" (Calc_int.power 2 0) ;
	Printf.printf "2 ^ 1 = %d\n" (Calc_int.power 2 1) ;
	Printf.printf "2 ^ 10 = %d\n" (Calc_int.power 2 10) ;
	Printf.printf "3 ^ 3 = %d\n" (Calc_int.power 3 3) ;

	print_endline "\n=== FLOAT: power ===" ;
	Printf.printf "2.0 ^ 0 = %f\n" (Calc_float.power 2.0 0) ;
	Printf.printf "2.0 ^ 1 = %f\n" (Calc_float.power 2.0 1) ;
	Printf.printf "2.0 ^ 10 = %f\n" (Calc_float.power 2.0 10) ;
	Printf.printf "3.0 ^ 3 = %f\n" (Calc_float.power 3.0 3) ;

	print_endline "\n=== INT: fact ===" ;
	Printf.printf "0! = %d\n" (Calc_int.fact 0) ;
	Printf.printf "1! = %d\n" (Calc_int.fact 1) ;
	Printf.printf "5! = %d\n" (Calc_int.fact 5) ;
	Printf.printf "7! = %d\n" (Calc_int.fact 7) ;

	print_endline "\n=== FLOAT: fact ===" ;
	Printf.printf "0.0! = %f\n" (Calc_float.fact 0.0) ;
	Printf.printf "1.0! = %f\n" (Calc_float.fact 1.0) ;
	Printf.printf "5.0! = %f\n" (Calc_float.fact 5.0) ;

	print_endline "\n=== INT: identite de zero1 (add) ===" ;
	let n = 42 in
	Printf.printf "add zero1 %d = %d -> %s\n" n (Calc_int.add Calc.INT.zero1 n)
		(if Calc_int.add Calc.INT.zero1 n = n then "OK" else "ECHEC") ;

	print_endline "\n=== INT: identite de zero2 (mul) ===" ;
	Printf.printf "mul zero2 %d = %d -> %s\n" n (Calc_int.mul Calc.INT.zero2 n)
		(if Calc_int.mul Calc.INT.zero2 n = n then "OK" else "ECHEC") ;

	print_endline "\n=== FLOAT: identite de zero1 (add) ===" ;
	let f = 42.0 in
	Printf.printf "add zero1 %f = %f -> %s\n" f (Calc_float.add Calc.FLOAT.zero1 f)
		(if Calc_float.add Calc.FLOAT.zero1 f = f then "OK" else "ECHEC") ;

	print_endline "\n=== FLOAT: identite de zero2 (mul) ===" ;
	Printf.printf "mul zero2 %f = %f -> %s\n" f (Calc_float.mul Calc.FLOAT.zero2 f)
		(if Calc_float.mul Calc.FLOAT.zero2 f = f then "OK" else "ECHEC")
