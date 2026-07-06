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

let most_represented result =
	let count = ref [] in
	for i = 0 to Array.length result - 1 do
		if List.exists (fun (_, str) -> result.(i) = str) !count then begin
			count := List.map (fun (counter, str) -> if str = result.(i) then (counter + 1, str) else (counter, str)) !count
		end else
			count := (1, result.(i))::!count
	done;
	count := List.sort (fun (c1, _) (c2, _) -> if c1 < c2 then 1 else if c1 > c2 then -1 else 0) !count ;
	match List.hd !count with
	| (_, t) -> t
	
exception EndOfLoop
let find_where_insert dist nearest_dist k =
	let insert = ref (-1) in
	begin try
		for i = 0 to k - 1 do
			if (!nearest_dist).(i) > dist then begin
				insert := i ;
				raise EndOfLoop
			end
		done
	with
	| EndOfLoop -> () end;
	!insert

let k_nn radars k r =
	let nearest_type = ref (Array.init k (fun i -> "")) in
	let nearest_dist = ref (Array.init k (fun i -> Float.max_float)) in
	let current = ref radars in
	let loop = ref true in
	while !loop = true do
		match !current with
		| [] -> begin
			if (!nearest_type).(0) = "" then invalid_arg "Empty list" else
			loop := false
		end
		| head::tail -> begin
			match head with
			| (fa, str) -> begin
				match r with
				| (fa_r, _) ->  let dist = eu_dist fa fa_r in
								let i = find_where_insert dist nearest_dist k in
								if i >= 0 then begin
									Array.blit !nearest_dist i !nearest_dist (i + 1) (k - 1 - i);
									Array.blit !nearest_type i !nearest_type (i + 1) (k - 1 - i);
									(!nearest_dist).(i) <- dist;
									(!nearest_type).(i) <- str
								end else ()
			end;
			current := tail
		end
	done;
	most_represented !nearest_type











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
		(k_nn manual_training 2 query_close);
	Printf.printf "Point proche de (10,10) -> prediction: %s (attendu: far)\n\n"
		(k_nn manual_training 2 query_far);

	print_endline "=== Test 2 : radar reel extrait du fichier CSV ===";
	let radars = examples_of_file "ionosphere.train.csv" in
	begin match radars with
	| [] -> print_endline "Fichier vide, impossible de tester.\n"
	| (test_vec, test_class) :: _ ->
		let training_set = List.filteri (fun j _ -> j <> 0) radars in
		let prediction = k_nn training_set 3 (test_vec, "") in
		Printf.printf "Classe reelle: %s / Classe predite: %s\n\n" test_class prediction
	end;

	print_endline "=== Test 3 : precision sur tout le fichier (leave-one-out) ===";
	let n = List.length radars in
	let correct = ref 0 in
	for i = 0 to n - 1 do
		let (test_vec, test_class) = List.nth radars i in
		let training_set = List.filteri (fun j _ -> j <> i) radars in
		let prediction = k_nn training_set 3 (test_vec, "") in
		if prediction = test_class then incr correct
	done;
	Printf.printf "Precision: %d / %d (%.2f%%)\n"
		!correct n (100.0 *. float_of_int !correct /. float_of_int n)
