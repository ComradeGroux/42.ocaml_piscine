let ft_print_comb = 
	let rec print_combo x y z = 
		print_int x;
		print_int y;
		print_int z;

		if x < 7 then begin
			print_string ", ";
			if z < 9 then
				print_combo x y (z + 1)
			else if y < 8 then
				print_combo x (y + 1) (y + 2)
			else
				print_combo (x + 1) (x + 2) (x + 3)
		end else
			print_string "\n"
		
	in print_combo 0 1 2

let () = ft_print_comb
