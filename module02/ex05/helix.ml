type phosphate = string
type deoxyribose = string
type nucleobase = A | T | C | G | None

type nucleotide = {
	phosphate: phosphate;
	deoxyribose: deoxyribose;
	nucleobase: nucleobase
}

type helix = nucleotide list

let string_of_nucleobase nucleobase =
	match nucleobase with
	| A -> "A"
	| T -> "T"
	| C -> "C"
	| G -> "G"
	| _ -> "None"

let nucleobase_of_char char =
	match char with
	| 'A' -> A
	| 'T' -> T
	| 'C' -> C
	| 'G' -> G
	| _ -> None

let generate_nucleotide base =
	{
		phosphate = "phosphate";
		deoxyribose = "deoxyribose";
		nucleobase = (nucleobase_of_char base)
	}

let generate_helix n =
	if n < 0 then
		[]
	else
		let () = Random.self_init () in
		let generate_random_nucleotide () =
			match Random.int 5 with
			| 0 -> generate_nucleotide 'A'
			| 1 -> generate_nucleotide 'T'
			| 2 -> generate_nucleotide 'C'
			| 3 -> generate_nucleotide 'G'
			| _ -> generate_nucleotide '_'
		in let rec gen n acc =
			if n = 0 then
				acc
			else
				gen (n - 1) (generate_random_nucleotide ()::acc)
		in gen n []

let complementary_helix helix =
	let generate_complementary_nucleobase base =
		match base with
		| A -> T
		| T -> A
		| C -> G
		| G -> C
		| _ -> None
	in let extract_nucleobase nucleotide =
		match nucleotide with
		| { phosphate: _ ; deoxyribose: _ ; nucleobase: _ } -> nucleobase
	in let generate_complementary_nucleotide base =
		{
			phosphate = "phosphate";
			deoxyribose = "deoxyribose";
			nucleobase = generate_complementary_nucleobase (extract_nucleobase base)
		}
	in let rec generate_complementary_helix helix acc =
		match helix with
		| [] -> acc
		| head::tail -> generate_complementary_helix tail (acc @ [generate_complementary_nucleotide head])
	in generate_complementary_helix helix []

let helix_to_string helix =
	let extract_nucleobase nucleotide =
		match nucleotide with
		| { phosphate: _ ; deoxyribose: _ ; nucleobase: _ } -> nucleobase
	in let rec string_of_helixes helix acc =
		match helix with
		| [] -> acc
		| head::tail -> string_of_helixes tail (acc ^ string_of_nucleobase (extract_nucleobase head))
	in string_of_helixes helix ""




let () =
	let print_helix helix =
		print_endline (helix_to_string helix)
	in let helix10 = generate_helix 10
	in print_helix helix10 ;
	print_helix (complementary_helix helix10)
