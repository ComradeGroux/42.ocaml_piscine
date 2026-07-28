let () =
	let alk_react = new Reaction.alkane_combustion [new Alkane.methane ; new Alkane.methane]
	in let new_alk_comb = alk_react#balance
	in print_endline new_alk_comb#to_string ;
	print_string (string_of_bool new_alk_comb#is_balanced)
