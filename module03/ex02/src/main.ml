let rec print_lst lst p f =
	match lst with
	| head::tail -> p (f head) ; print_char ' ' ; print_lst tail p f
	| [] -> print_newline


let () =
	let test = Card.allSpades in 
	print_lst test print_string Card.toString () ; print_newline () ;
	print_lst test print_endline Card.toStringVerbose () ;

	let test = Card.allDiamonds in 
	print_lst test print_string Card.toString () ; print_newline () ;
	print_lst test print_endline Card.toStringVerbose () ;

	let test = Card.allClubs in 
	print_lst test print_string Card.toString () ; print_newline () ;
	print_lst test print_endline Card.toStringVerbose () ;

	let test = Card.allHearts in 
	print_lst test print_string Card.toString () ; print_newline () ;
	print_lst test print_endline Card.toStringVerbose () ;

	let test = Card.all in
	print_lst test print_string Card.toString () ; print_newline () ;
	print_lst test print_endline Card.toStringVerbose () ;

	print_string "Min between { As, Spade }, { T9, Spade } = " ;
	print_endline (Card.toStringVerbose (Card.min (Card.newCard Card.Value.As Card.Color.Spade) (Card.newCard Card.Value.T9 Card.Color.Spade)));
	print_string "Min between { T9, Heart }, { T9, Spade } = " ;
	print_endline (Card.toStringVerbose (Card.min (Card.newCard Card.Value.T9 Card.Color.Heart) (Card.newCard Card.Value.T9 Card.Color.Spade)));
	print_newline ();
	print_string "Max between { As, Spade }, { T9, Spade } = " ;
	print_endline (Card.toStringVerbose (Card.max (Card.newCard Card.Value.As Card.Color.Spade) (Card.newCard Card.Value.T9 Card.Color.Spade)));
	print_string "Max between { T9, Heart }, { T9, Spade } = " ;
	print_endline (Card.toStringVerbose (Card.max (Card.newCard Card.Value.T9 Card.Color.Heart) (Card.newCard Card.Value.T9 Card.Color.Spade)));
	print_newline ();

	print_string "Compare between { T10, Diamond }, { Queen, Heart } = ";
	print_int (Card.compare (Card.newCard Card.Value.T10 Card.Color.Diamond) (Card.newCard Card.Value.Queen Card.Color.Heart));
	print_newline ();
	print_string "Compare between { Queen, Heart }, { T10, Diamond } = ";
	print_int (Card.compare (Card.newCard Card.Value.Queen Card.Color.Heart) (Card.newCard Card.Value.T10 Card.Color.Diamond));
	print_newline ();
	print_string "Compare between { Queen, Heart }, { Queen, Diamond } = ";
	print_int (Card.compare (Card.newCard Card.Value.Queen Card.Color.Heart) (Card.newCard Card.Value.Queen Card.Color.Diamond));
	print_endline "\n";

	print_string "Best card in allClub = " ;
	print_endline (Card.toStringVerbose (Card.best Card.allClubs));

	print_string "Best card in all = " ;
	print_endline (Card.toStringVerbose (Card.best Card.all));
	print_newline ();

	print_endline ("Is { As, Spade } a Diamond card ? " ^ (string_of_bool (Card.isDiamond (Card.newCard Card.Value.As Card.Color.Spade))));
	print_endline ("Is { As, Spade } a Club card ? " ^ (string_of_bool (Card.isOf (Card.newCard Card.Value.As Card.Color.Spade) Card.Color.Club)));
	print_endline ("Is { As, Spade } a Spade card ? " ^ (string_of_bool (Card.isSpade (Card.newCard Card.Value.As Card.Color.Spade))));

	print_newline ();
	print_endline "The next step must print an error (best with an empty list)" ;
	ignore(Card.best []);
