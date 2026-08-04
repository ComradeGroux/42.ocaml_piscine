let print_int_try (label: string) (t: int Try.t) : unit =
	match t with
	| Try.Success v -> Printf.printf "%s -> Success: %d\n" label v
	| Try.Failure e -> Printf.printf "%s -> Failure: %s\n" label (Printexc.to_string e)

let () =

	print_endline "=== return ===" ;
	print_int_try "return 42" (Try.return 42) ;

	print_endline "\n=== bind: cas Success, pas d'exception ===" ;
	print_int_try "bind (Success 10) (+1)"
		(Try.bind (Try.return 10) (fun x -> Try.return (x + 1))) ;

	print_endline "\n=== bind: la fonction leve une exception ===" ;
	print_int_try "bind (Success 10) (/0)"
		(Try.bind (Try.return 10) (fun x -> Try.return (x / 0))) ;

	print_endline "\n=== bind: deja en Failure, f n'est jamais appelee ===" ;
	print_int_try "bind (Failure ..) (never called)"
		(Try.bind (Try.Failure Not_found) (fun x -> Try.return (x + 999))) ;

	print_endline "\n=== recover: sur un Failure ===" ;
	print_int_try "recover (Failure ..) (fun _ -> Success 0)"
		(Try.recover (Try.Failure Not_found) (fun _ -> Try.return 0)) ;

	print_endline "\n=== recover: sur un Success (ne fait rien) ===" ;
	print_int_try "recover (Success 7) (never called)"
		(Try.recover (Try.return 7) (fun _ -> Try.return 0)) ;

	print_endline "\n=== filter: predicat satisfait ===" ;
	print_int_try "filter (Success 10) (>0)"
		(Try.filter (Try.return 10) (fun x -> x > 0)) ;

	print_endline "\n=== filter: predicat non satisfait ===" ;
	print_int_try "filter (Success (-5)) (>0)"
		(Try.filter (Try.return (-5)) (fun x -> x > 0)) ;

	print_endline "\n=== filter: deja en Failure ===" ;
	print_int_try "filter (Failure ..) (never called)"
		(Try.filter (Try.Failure Not_found) (fun x -> x > 0)) ;

	print_endline "\n=== flatten: Success (Success x) ===" ;
	print_int_try "flatten (Success (Success 5))"
		(Try.flatten (Try.Success (Try.return 5))) ;

	print_endline "\n=== flatten: Success (Failure e) -> doit devenir Failure ===" ;
	print_int_try "flatten (Success (Failure ..))"
		(Try.flatten (Try.Success (Try.Failure Not_found))) ;

	print_endline "\n=== flatten: Failure e directement ===" ;
	print_int_try "flatten (Failure ..)"
		(Try.flatten (Try.Failure Not_found))
