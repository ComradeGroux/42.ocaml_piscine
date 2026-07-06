let eu_dist p1 p2 =
	let d = ref 0.0 in
	let n = ref 0 in
	begin
		if Array.length p1 <= Array.length p2 then
			n := Array.length p1 - 1
		else
			n := Array.length p2 - 1
	end;
	for i = 0 to !n do
		d := !d +. ((p1.(i) -. p2.(i)) ** 2.0)
	done;
	Float.sqrt !d

let () =
	print_float (eu_dist [| 1.0 ; 3.0 |] [| 2.0 ; 1.0 |]) ; print_newline () ;
	print_float (eu_dist [| 2.0 ; 1.0 |] [| 1.0 ; 3.0 |]) ; print_newline () ;
	print_float (eu_dist [| 1.0 ; 3.0 |] [| -2.0 ; -1.0 |]) ; print_newline ()
