let my_sleep () = Unix.sleep 1

let main argc argv =
	match int_of_string_opt (argv.(1)) with
	| Some v -> for i = 0 to v do my_sleep () done
	| None -> ()

let () =
	main (Array.length Sys.argv) Sys.argv
