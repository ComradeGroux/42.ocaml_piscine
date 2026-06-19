let crossover l m =
	let rec cross_l l m res =
		let rec cross_m lvalue m = 
			match m with
			| mvalue::mtail ->
				if lvalue = mvalue then
					true
				else
					cross_m lvalue mtail
			| [] -> false
		in match l with
		| lvalue::ltail -> begin
			if (cross_m lvalue m) then
				cross_l ltail m (res @ [lvalue])
			else
				cross_l ltail m res
		end
		| [] -> res
	in cross_l l m []


let () =
	let print_list l =
		print_string "cross: [";
		let rec plist l =
			match l with
			| first::second::tail -> print_int first; print_string "; "; plist (second::tail)
			| first::tail -> print_int first
			| _ -> ()
		in plist l;
		print_endline "]\n"
	in
	print_endline "list1: [1; 2; 3; 4]";
	print_endline "list2: [2; 4; 5; 6]";
	print_list (crossover [1; 2; 3; 4] [2; 4; 5; 6]);

	print_endline "list1: [1; 2; 3; 4]";
	print_endline "list2: []";
	print_list (crossover [1; 2; 3; 4] []);

	print_endline "list1: [1; 2; 3; 4]";
	print_endline "list2: [2]";
	print_list (crossover [1; 2; 3; 4] [2]);

	print_endline "list1: [1; 2; 3; 4]";
	print_endline "list2: [6; 9; 30; 3]";
	print_list (crossover [1; 2; 3; 4] [6; 9; 30; 3]);

	print_endline "list1: []";
	print_endline "list2: [6; 9; 30; 3]";
	print_list (crossover [] [6; 9; 30; 3]);
