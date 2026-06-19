let leibniz_pi delta =
	let rec leibniz_intern ?(i=0) ?(value=0.0) delta =
		let ref_pi = (atan 1.0) *. 4.0 in
		if delta < 0.0 then
			-1
		else if 4.0 *. value > (ref_pi -. delta) && 4.0 *. value < (ref_pi +. delta) then
			i
		else
			let sign = if i mod 2 = 0 then 1.0 else -1.0 in
			let next = value +. sign /. float_of_int (2 * i + 1) in
			leibniz_intern ~i:(i + 1) ~value:next delta
	in leibniz_intern delta

let () =
	print_int (leibniz_pi (-1.));
	print_newline ();

	print_int (leibniz_pi (1.));
	print_newline ();

	print_int (leibniz_pi (2.));
	print_newline ();

	print_int (leibniz_pi (1e-8));
	print_newline ()
