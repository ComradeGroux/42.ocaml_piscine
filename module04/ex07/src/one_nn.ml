type radar = float array * string

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

let one_nn radars  r =
	let nearest_type = ref "" in
	let nearest_dist = ref Float.max_float in
	let loop = ref true in
	let current = ref radars in
	while !loop = true do
		match !current with
		| [] -> begin
			if !nearest_type = "" then invalid_arg "Empty list" else
			loop := false
		end
		| head::tail -> begin
			match head with
			| (fa, str) -> begin
				match r with
				| (fa_r, _) -> begin
					let dist = eu_dist fa fa_r in
					if !nearest_dist > dist then begin
						nearest_type := str;
						nearest_dist := dist;
					end
				end
			end;
			current := tail
		end
	done;
	!nearest_type






let examples_of_file filePath =
	if Sys.file_exists filePath = false then invalid_arg "File does not exist" else ();
	let ifd = open_in filePath in
	let file = ref [] in
	begin
		try
			while true do
				let line = input_line ifd in
				let splittedLine = String.split_on_char ',' line in
				let acc = ref [] in
				let lineLength = List.length splittedLine in
				for i = 0 to lineLength - 2 do
					ignore (acc := (float_of_string (List.nth splittedLine i))::!acc);
				done;
				file := (Array.of_list (List.rev !acc), (List.nth splittedLine ((List.length splittedLine) - 1)))::!file
			done
		with
		| End_of_file -> ()
	end;
	close_in ifd ;
	List.rev !file

let () =
	print_endline "=== Test 1 : radars fabriqués à la main ===";
	let manual_training = [
		([| 0.0; 0.0 |], "close");
		([| 10.0; 10.0 |], "far");
	] in
	let query_close = ([| 0.5; 0.5 |], "") in
	let query_far = ([| 9.5; 9.5 |], "") in
	Printf.printf "Point proche de (0,0)   -> prediction: %s (attendu: close)\n"
		(one_nn manual_training query_close);
	Printf.printf "Point proche de (10,10) -> prediction: %s (attendu: far)\n\n"
		(one_nn manual_training query_far);

	print_endline "=== Test 2 : radar reel extrait du fichier CSV ===";
	let radars = examples_of_file "ionosphere.test.csv" in
	begin match radars with
	| [] -> print_endline "Fichier vide, impossible de tester.\n"
	| (test_vec, test_class) :: _ ->
		let training_set = List.filteri (fun j _ -> j <> 0) radars in
		let prediction = one_nn training_set (test_vec, "") in
		Printf.printf "Classe reelle: %s / Classe predite: %s\n\n" test_class prediction
	end;

	print_endline "=== Test 3 : precision sur tout le fichier (leave-one-out) ===";
	let n = List.length radars in
	let correct = ref 0 in
	for i = 0 to n - 1 do
		let (test_vec, test_class) = List.nth radars i in
		let training_set = List.filteri (fun j _ -> j <> i) radars in
		let prediction = one_nn training_set (test_vec, "") in
		if prediction = test_class then incr correct
	done;
	Printf.printf "Precision: %d / %d (%.2f%%)\n"
		!correct n (100.0 *. float_of_int !correct /. float_of_int n)
