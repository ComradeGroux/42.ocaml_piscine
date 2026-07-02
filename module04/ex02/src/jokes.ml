let jokes = [|
				"L'égalité des sexes" ;
				"Qu'est-ce qui est pire qu'un bébé dans une poubelle ? Un bébé dans 2 poubelles" ;
				"Quel est le légume officiel de l'Allemagne ? Michael Schumacher" ;
				"Le racisme, c'est comme les noirs, ça devrait pas exister" ;
				"Quel est pire que de faire l'amour dans un cimetière ? Avoir oublier la pelle"
			|]

let () =
	Random.self_init ();
	print_endline jokes.((Random.int (Array.length jokes)))
