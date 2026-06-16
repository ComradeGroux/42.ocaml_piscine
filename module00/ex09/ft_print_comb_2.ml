let ft_print_comb_2 =
	let rec print_combo a b c d =
		print_int a;
		print_int b;
		print_char ' ';
		print_int c;
		print_int d;

		if a <= 9 && not (a == 9 && b == 8) then begin
			print_char ',';
			print_char ' ';

			if d < 9 then
				print_combo a b c (d + 1)
			else if c < 9 then
				print_combo a b (c + 1) 0
			else if b < 9 then
				if b == 8 then
					print_combo a (b + 1) (a + 1) 0
				else
					print_combo a (b + 1) a (b + 2)
			else if a != 9 then
				print_combo (a + 1) 0 (a + 1) 1
		end else begin
			print_char '\n'
		end;
	
	in print_combo 0 0 0 0

let () = ft_print_comb_2
