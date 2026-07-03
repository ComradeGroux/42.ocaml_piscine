let sum a b = a +. b

let () =
	print_string " 21.0 +  21.0 = " ;	print_float (sum 21.0 21.0) ; print_newline ();
	print_string "-21.0 +  21.0 = " ;	print_float (sum (-21.0) 21.0) ; print_newline ();
	print_string " 21.0 + -21.0 = " ;	print_float (sum 21.0 (-21.0)) ; print_newline ();
	print_string " 42.0 +   0.0 = " ;	print_float (sum 21.0 0.0) ; print_newline ();
