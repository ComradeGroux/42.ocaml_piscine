let rec converges (f: 'a -> 'a) x n =
	if x = (f x) then
		true
	else if n = 0 then
		false
	else
		converges f (f x) (n - 1)

let () =
	print_endline ("converges (( * ) 2) 2 5       : " ^ if converges (( * ) 2) 2 5 then "True" else "False");
	print_endline ("converges (fun x -> x / 2) 2 3: " ^ if converges (fun x -> x / 2) 2 3 then "True" else "False");
	print_endline ("converges (fun x -> x / 2) 2 2: " ^ if converges (fun x -> x / 2) 2 2 then "True" else "False")
