type phosphate = string
type deoxyribose = string
type nucleobase = A | T | C | G | None

type nucleotide = {
	phosphate: phosphate;
	deoxyribose: deoxyribose;
	nucleobase: nucleobase
}

let generate_nucleotide base =
	let nucleobase_of_char char =
		match char with
		| 'A' -> A
		| 'T' -> T
		| 'C' -> C
		| 'G' -> G
		| _ -> None
	in
	{
		phosphate = "phosphate";
		deoxyribose = "deoxyribose";
		nucleobase = (nucleobase_of_char base)
	}

let print_nucleotide nuc =
	let string_of_nucleobase nucleobase =
		match nucleobase with
		| A -> "A"
		| T -> "T"
		| C -> "C"
		| G -> "G"
		| _ -> "None"
	in match nuc with
	| { phosphate: _ ; deoxyribose: _ ; nucleobase: _ } -> print_string ("Phosphate= " ^ phosphate ^ "; Deoxyribose= " ^ deoxyribose ^ "; Nucleobase= " ^ (string_of_nucleobase nucleobase))

let () =
	let a = generate_nucleotide 'A' in
	let t = generate_nucleotide 'T' in
	let c = generate_nucleotide 'C' in
	let g = generate_nucleotide 'G' in
	let none = generate_nucleotide 'N' in

	print_nucleotide a ; print_newline ();
	print_nucleotide t ; print_newline ();
	print_nucleotide c ; print_newline ();
	print_nucleotide g ; print_newline ();
	print_nucleotide none ; print_newline ();
	