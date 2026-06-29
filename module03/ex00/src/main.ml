let () =
	let allT = Color.all () in
	let rec print_all lst =
		match lst with
		| [] -> ()
		| head::tail -> print_string ((Color.toString head) ^ " : " ^ (Color.toStringVerbose head) ^ "\n") ; print_all tail
	in print_all allT
