let print_incomplete_results alk_react =
	let print_molecule mol n =
		if n > 1 then
			print_string (string_of_int n ^ " " ^ mol)
		else if n = 1 then
			print_string mol
		else
			()
	in let rec print_mol_lst mol_lst = begin
		match mol_lst with
		| [] -> print_newline ()
		| (mol, n)::t ->
			print_molecule mol#formula n ;
			match t with
			| [] -> ()
			| ta -> if n > 0 then print_string " + " else () ; print_mol_lst ta
	end
	in let rec print_incomplete results =
		match results with
		| [] -> ()
		| (o2, lst)::tail -> begin
			print_mol_lst alk_react#get_super_start ;
			print_string " + " ;
			print_molecule "O2" o2 ;
			print_string " -> " ;
			print_mol_lst lst ; print_newline () ;
			print_incomplete tail
		end
	in print_incomplete alk_react#get_incomplete_results

let () =
	let alk_react = new Alkane_combustion.alkane_combustion [new Alkane.methane ; new Alkane.methane]
	in let new_alk_comb = alk_react#balance
	in print_endline new_alk_comb#to_string ;
	print_endline (string_of_bool new_alk_comb#is_balanced);
	print_newline ();

	let alk_react = (new Alkane_combustion.alkane_combustion [new Alkane.ethane])
	in print_incomplete_results alk_react ; print_newline () ;

	let alk_react = (new Alkane_combustion.alkane_combustion [new Alkane.propane])
	in print_incomplete_results alk_react ; print_newline () ;

	let alk_react = (new Alkane_combustion.alkane_combustion [new Alkane.propane ; new Alkane.methane])
	in print_incomplete_results alk_react
