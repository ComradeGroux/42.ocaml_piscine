let () =
	let water = new Molecule.water in
	let h2 = new Molecule.dihydrogen in
	let o2 = new Molecule.dioxygen in
	let create_water = new Reaction.simple_reaction [(h2, 2) ; (o2, 1)] [(water, 2)] in
	print_endline create_water#to_string;
	print_endline (string_of_bool create_water#is_balanced);
	print_newline ();
	let create_wrong_water = new Reaction.simple_reaction [(h2, 1) ; (o2, 1)] [(water, 1)] in
	print_endline create_wrong_water#to_string;
	print_endline (string_of_bool create_wrong_water#is_balanced);
