let () =
	let silver = new Atom.silver in
	print_endline silver#to_string ;
	let oxygen = new Atom.oxygen in
	print_endline oxygen#to_string ;
	if silver#equals oxygen then
		print_endline "true"
	else
		print_endline "false";
	let newsilver = new Atom.silver in
	if silver#equals newsilver then
		print_endline "true"
	else
		print_endline "false"
