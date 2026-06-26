type phosphate = string
type deoxyribose = string
type nucleobase = A | T | C | G | U | None
type aminoacid = Stop | Ala | Arg | Asn | Asp | Cys | Gln | Glu
				| Gly | His | Ile | Leu | Lys | Met | Phe | Pro
				| Ser | Thr | Trp | Tyr | Val

type nucleotide = {
	phosphate: phosphate;
	deoxyribose: deoxyribose;
	nucleobase: nucleobase
}
	
type rna = nucleobase list
type helix = nucleotide list
type protein = aminoacid list

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

let generate_bases_triplets rna =
	let rec generate_triplets r acc =
		match r with
		| first::second::third::tail -> generate_triplets tail (acc @ [(first, second, third)])
		| _ -> acc
	in generate_triplets rna []

let string_of_protein protein =
	let string_of_aminoacid aminoacid =
		match aminoacid with
		| Ala -> "Alanine"
		| Arg -> "Arginine"
		| Asn -> "Asparagine"
		| Asp -> "Asparatique"
		| Cys -> "Cysteine"
		| Gln -> "Glutamine"
		| Glu -> "Glutamique"
		| Gly -> "Glycine"
		| His -> "Histidine"
		| Ile -> "Isoleucine"
		| Leu -> "Leucine"
		| Lys -> "Lysine"
		| Met -> "Methionine"
		| Phe -> "Phenylalanine"
		| Pro -> "Proline"
		| Ser -> "Serine"
		| Thr -> "Threonine"
		| Trp -> "Tryptophane"
		| Tyr -> "Tyrosine"
		| Val -> "Valine"
		| _ -> ""
	in let rec create_string_of_protein protein acc =
		match protein with
		| head::tail when head <> Stop -> acc ^ (string_of_aminoacid head) ^ "-"
		| _ -> acc
	in create_string_of_protein protein ""

let decode_arn rna =
	let rec decode_triplets t acc =
		match t with
		| (U, A, A)::tail
		| (U, A, G)::tail
		| (U, G, A)::tail -> acc
		| (G, C, A)::tail
		| (G, C, C)::tail
		| (G, C, G)::tail
		| (G, C, U)::tail -> decode_triplets tail (acc @ [Ala])
		| (A, G, A)::tail
		| (A, G, G)::tail
		| (C, G, A)::tail
		| (C, G, C)::tail
		| (C, G, G)::tail
		| (C, G, U)::tail -> decode_triplets tail (acc @ [Arg])
		| (A, A, C)::tail
		| (A, A, U)::tail -> decode_triplets tail (acc @ [Asn])
		| (G, A, C)::tail
		| (G, A, U)::tail -> decode_triplets tail (acc @ [Asp])
		| (U, G, C)::tail
		| (U, G, U)::tail -> decode_triplets tail (acc @ [Cys])
		| (C, A, A)::tail
		| (C, A, G)::tail -> decode_triplets tail (acc @ [Gln])
		| (G, A, A)::tail
		| (G, A, G)::tail -> decode_triplets tail (acc @ [Glu])
		| (G, G, A)::tail
		| (G, G, C)::tail
		| (G, G, G)::tail
		| (G, G, U)::tail -> decode_triplets tail (acc @ [Gly])
		| (C, A, C)::tail
		| (C, A, U)::tail -> decode_triplets tail (acc @ [His])
		| (A, U, A)::tail
		| (A, U, C)::tail
		| (A, U, U)::tail -> decode_triplets tail (acc @ [Ile])
		| (C, U, A)::tail
		| (C, U, C)::tail
		| (C, U, G)::tail
		| (C, U, U)::tail
		| (U, U, A)::tail
		| (U, U, G)::tail -> decode_triplets tail (acc @ [Leu])
		| (A, A, A)::tail
		| (A, A, G)::tail -> decode_triplets tail (acc @ [Lys])
		| (A, U, G)::tail -> decode_triplets tail (acc @ [Met])
		| (U, U, C)::tail
		| (U, U, U)::tail -> decode_triplets tail (acc @ [Phe])
		| (C, C, C)::tail
		| (C, C, A)::tail
		| (C, C, G)::tail
		| (C, C, U)::tail -> decode_triplets tail (acc @ [Pro])
		| (U, C, A)::tail
		| (U, C, C)::tail
		| (U, C, G)::tail
		| (U, C, U)::tail
		| (A, G, U)::tail
		| (A, G, C)::tail -> decode_triplets tail (acc @ [Ser])
		| (A, C, A)::tail
		| (A, C, C)::tail
		| (A, C, G)::tail
		| (A, C, U)::tail -> decode_triplets tail (acc @ [Thr])
		| (U, G, G)::tail -> decode_triplets tail (acc @ [Trp])
		| (U, A, C)::tail
		| (U, A, U)::tail -> decode_triplets tail (acc @ [Tyr])
		| (G, U, A)::tail
		| (G, U, C)::tail
		| (G, U, G)::tail
		| (G, U, U)::tail -> decode_triplets tail (acc @ [Val])
		| _ -> acc
	in decode_triplets (generate_bases_triplets rna) []













let print_helix helix =
	print_endline (helix_to_string helix)

let print_rna rna =
	let rec print r acc =
		match r with
		| [] -> print_endline acc
		| head::tail -> print tail (acc ^ (string_of_nucleobase head))
	in print rna ""

let print_protein protein =
	print_endline (string_of_protein protein)

let () =
	let helix10 = generate_helix 10
	in print_helix helix10 ;

	let rna_helix10 = generate_rna helix10
	in print_rna rna_helix10 ;

	let protein_helix10 = decode_arn rna_helix10
	in print_protein protein_helix10
