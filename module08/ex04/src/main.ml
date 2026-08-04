(* Petit utilitaire pour visualiser le contenu d'un Set, via foreach uniquement *)
let print_set (label: string) (s: int Set.t) : unit =
	Printf.printf "%s : [" label ;
	Set.foreach s (fun x -> Printf.printf "%d; " x) ;
	print_endline "]"

(* Construit un set vide en detournant filter : aucun element ne passe le predicat *)
let empty : int Set.t = Set.filter (Set.return 0) (fun _ -> false)

(* Construit un set a partir d'une liste, uniquement via union + return *)
let of_list (lst: int list) : int Set.t =
	List.fold_left (fun acc x -> Set.union acc (Set.return x)) empty lst

let () =

	print_endline "=== return ===" ;
	print_set "return 5" (Set.return 5) ;

	print_endline "\n=== union ===" ;
	let s1 = of_list [1; 2; 3] in
	let s2 = of_list [3; 4; 5] in
	print_set "s1" s1 ;
	print_set "s2" s2 ;
	print_set "union s1 s2" (Set.union s1 s2) ;

	print_endline "\n=== inter ===" ;
	print_set "inter s1 s2" (Set.inter s1 s2) ;

	print_endline "\n=== diff ===" ;
	print_set "diff s1 s2 (dans s1 mais pas s2)" (Set.diff s1 s2) ;
	print_set "diff s2 s1 (dans s2 mais pas s1)" (Set.diff s2 s1) ;

	print_endline "\n=== filter ===" ;
	let s3 = of_list [1; 2; 3; 4; 5; 6] in
	print_set "s3" s3 ;
	print_set "filter s3 (pair)" (Set.filter s3 (fun x -> x mod 2 = 0)) ;

	print_endline "\n=== bind ===" ;
	(* double chaque element *)
	print_set "bind s1 (x -> {x*2})" (Set.bind s1 (fun x -> Set.return (x * 2))) ;
	(* chaque element se transforme en 2 elements -> teste le "flatten" implicite de bind *)
	print_set "bind s1 (x -> {x; x+10})"
		(Set.bind s1 (fun x -> Set.union (Set.return x) (Set.return (x + 10)))) ;

	print_endline "\n=== foreach ===" ;
	let total = ref 0 in
	Set.foreach s1 (fun x -> total := !total + x) ;
	Printf.printf "somme des elements de s1 via foreach = %d\n" !total ;

	print_endline "\n=== for_all ===" ;
	Printf.printf "for_all s1 (>0) = %b\n" (Set.for_all s1 (fun x -> x > 0)) ;
	Printf.printf "for_all s1 (>2) = %b\n" (Set.for_all s1 (fun x -> x > 2)) ;

	print_endline "\n=== exists ===" ;
	Printf.printf "exists s1 (=2) = %b\n" (Set.exists s1 (fun x -> x = 2)) ;
	Printf.printf "exists s1 (=99) = %b\n" (Set.exists s1 (fun x -> x = 99)) ;

	print_endline "\n=== empty set (edge cases) ===" ;
	print_set "empty" empty ;
	Printf.printf "for_all empty (>0) = %b (doit etre true, vacuite)\n" (Set.for_all empty (fun x -> x > 0)) ;
	Printf.printf "exists empty (>0) = %b (doit etre false)\n" (Set.exists empty (fun x -> x > 0))