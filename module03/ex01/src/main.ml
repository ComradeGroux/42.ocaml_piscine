let rec print_lst lst p f =
	match lst with
	| head::tail -> p (f head) ; print_char ' ' ; print_lst tail p f
	| [] -> print_newline

let () =
	let lst = Value.all () in
	print_lst lst print_string Value.toString () ;
	print_lst lst print_string Value.toStringVerbose () ;
	print_lst lst print_int Value.toInt () ;

	let t = Value.T10 in
	print_int (Value.toInt t) ; print_newline () ;
	print_int (Value.toInt (Value.prev t)) ; print_newline () ;
	print_int (Value.toInt (Value.next t)) ; print_newline () ;

	(* ERROR TESTING *)
	print_string (Value.toStringVerbose (Value.prev Value.T2)) ;
	print_string (Value.toStringVerbose (Value.next Value.As)) ;
