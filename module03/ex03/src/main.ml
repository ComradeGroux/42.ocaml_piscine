let rec print_lst lst =
	match lst with
	| head::tail -> print_string head ; print_char ' ' ; print_lst tail
	| [] -> print_newline ()

let () =
	print_lst (Deck.toStringList (Deck.newDeck ()));

	print_endline "\n";

	let d = Deck.newDeck () in
	print_lst (Deck.toStringList d);
	print_newline ();

	let (card, d) = Deck.drawCard d in
	print_endline ("First card of the deck was: " ^ Deck.Card.toStringVerbose card ^ " and now the deck contain: ");
	print_lst (Deck.toStringList d)
