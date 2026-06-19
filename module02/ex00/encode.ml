let encode l =
	let rec count_letter l count res =
		match l with
		| first::second::tail ->
			if first = second then
				count_letter (second::tail) (count + 1) res
			else
				count_letter (second::tail) 0 (res @ [((count + 1), first)])
		| first::tail -> res @ [((count + 1), first)]
		| [] -> res
	in count_letter l 0 []

let () =
	let rec print_tuples l = match l with
		| (count, letter)::tail -> print_int count; print_char letter; print_tuples tail
		| [] -> print_char '\n'
	in
	print_tuples (encode ['a'; 'a'; 'a'; 'b'; 'b'; 'b']);
	print_tuples (encode ['a'; 'a'; 'b'; 'b'; 'b'; 'a'; 'a']);
	print_tuples (encode ['a'; 'b'; 'a'; 'b'; 'a'; 'b'; 'a']);
	print_tuples (encode ['a']);
	print_tuples (encode [])
