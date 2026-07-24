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
		print_endline "false";
	
	let water = new Molecule.water in
	print_endline water#to_string;
	let tnt = new Molecule.trinitrotoluene in
	print_endline tnt#to_string;
	if water#equals (new Molecule.water) then
		print_endline "true"
	else
		print_endline "false";
	if water = tnt then
		print_endline "true"
	else
		print_endline "false";

	let octane = new Alkane.octane in
	print_endline (octane#name ^ " " ^ octane#formula)
