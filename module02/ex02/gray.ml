let gray n =
	let rec reverse_list ?(acc=[]) list =
		match list with
		| [] -> acc
		| value::tail -> reverse_list tail ~acc:(value::acc)
	in
	let rec prefix_list ?(acc=[]) c list =
		match list with
		| [] -> reverse_list acc
		| value::tail -> prefix_list c tail ~acc:((c ^ value)::acc)
	in
	let concat_list list1 list2 =
		let list1 = reverse_list list1 in
		let rec concat list1 list2 =
			match list1 with
			| [] -> list2
			| value::tail -> concat tail (value::list2)
		in concat list1 list2
	in
	let rec create_gray n =
		if n = 0 then
			[""]
		else
			let sub = create_gray (n - 1) in
			let sub0 = prefix_list "0" sub in
			let sub1 = reverse_list sub in
			let sub1 = prefix_list "1" sub1 in
			concat_list sub0 sub1
	in
	let print_list l =
		let rec plist l =
			match l with
			| first::second::tail -> print_string first; print_string " "; plist (second::tail)
			| [first] -> print_string first
			| _ -> ()
		in plist l;
		print_newline ()
	in print_list (create_gray n)

let () = 
	gray 1;
	gray 2;
	gray 3;
	gray 4;
