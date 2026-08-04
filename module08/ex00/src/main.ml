let () =
	let wt = Watchtower.add 4 3
	in print_endline ("4 + 3 = " ^ string_of_int wt);
	let wt = Watchtower.add 11 2
	in print_endline ("11 + 2 = " ^ string_of_int wt);
	let wt = Watchtower.add 16 2
	in print_endline ("16 + 2 = " ^ string_of_int wt);

	print_newline ();

	let wt = Watchtower.sub 8 5
	in print_endline ("8 - 5 = " ^ string_of_int wt);
	let wt = Watchtower.sub 14 2
	in print_endline ("14 - 2 = " ^ string_of_int wt);
	let wt = Watchtower.sub 1 1
	in print_endline ("1 - 1 = " ^ string_of_int wt);
