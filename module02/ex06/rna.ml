type phosphate = string
type deoxyribose = string
type nucleobase = A | T | C | G | U | None

type nucleotide = {
	phosphate: phosphate;
	deoxyribose: deoxyribose;
	nucleobase: nucleobase
}
	
type rna = nucleobase list
type helix = nucleotide list

let string_of_nucleobase nucleobase =
	match nucleobase with
	| A -> "A"
	| T -> "T"
	| C -> "C"
	| G -> "G"
	| U -> "U"
	| _ -> "None"

let nucleobase_of_char char =
	match char with
	| 'A' -> A
	| 'T' -> T
	| 'C' -> C
	| 'G' -> G
	| 'U' -> U
	| _ -> None

let extract_nucleobase nucleotide =
	match nucleotide with
	| { phosphate: _ ; deoxyribose: _ ; nucleobase: _ } -> nucleobase

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
			match Random.int 4 with
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
	let rec string_of_helixes helix acc =
		match helix with
		| [] -> acc
		| head::tail -> string_of_helixes tail (acc ^ string_of_nucleobase (extract_nucleobase head))
	in string_of_helixes helix ""

let generate_rna helix =
	let generate_complementary_nucleobase base =
		match base with
		| A -> U
		| T -> A
		| C -> G
		| G -> C
		| _ -> None
	in let rec generate helix acc =
		match helix with
		| [] -> acc
		| head::tail -> generate tail (acc @ [generate_complementary_nucleobase (extract_nucleobase head)])
	in generate helix []





let print_helix helix =
	print_endline (helix_to_string helix)

let print_rna rna =
	let rec print r acc =
		match r with
		| [] -> print_endline acc
		| head::tail -> print tail (acc ^ (string_of_nucleobase head))
	in print rna ""

let () =
	let helix10 = generate_helix 10
	in print_helix helix10 ;

	let rna_helix10 = generate_rna helix10
	in print_rna rna_helix10
