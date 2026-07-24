(* Pas dans initializer parce que OCaml evalue D'ABORD les val PUIS initializer *)
let () = Random.self_init ()

class dalek = object
	val _name : string =
		let rec randChar n acc =
			match n with
			| 0 -> acc
			| x -> randChar (x - 1) (acc ^ String.make 1 (String.get "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" (Random.bits () mod 52)))
		in "Dalek" ^ (randChar 3 "")
	val _hp : int = 100
	val mutable _shield : bool = true

	method to_string =  "My name is " ^ _name ^
						". I'm a Dalek, I have " ^ string_of_int _hp ^
						" HP and my current shield status is " ^ match _shield with | true -> "activated" | false -> "desactivated"
	method talk =
		match (Random.bits () mod 4) with
		| 0 -> print_endline "Explain! Explain!"
		| 1 -> print_endline "Exterminate! Exterminate!"
		| 2 -> print_endline "I obey!"
		| _ -> print_endline "You are the Doctor! You are the enemy of the Daleks!"
	method exterminate (people: People.people) = people#die ; _shield <- not _shield
	method die = print_endline "Emergency Temporal Shift!"
end
