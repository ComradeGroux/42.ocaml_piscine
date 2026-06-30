module Value = struct
	type t = T2 | T3 | T4 | T5 | T6 | T7 | T8 | T9 | T10 | Jack | Queen | King | As

	let all () = [ T2; T3; T4; T5; T6; T7; T8; T9; T10; Jack; Queen; King; As ]

	let toInt value =
		match value with
		| T2 -> 1
		| T3 -> 2
		| T4 -> 3
		| T5 -> 4
		| T6 -> 5
		| T7 -> 6
		| T8 -> 7
		| T9 -> 8
		| T10 -> 9
		| Jack -> 10
		| Queen -> 11
		| King -> 12
		| As -> 13

	let toString value =
		match value with
		| T2 -> "2"
		| T3 -> "3"
		| T4 -> "4"
		| T5 -> "5"
		| T6 -> "6"
		| T7 -> "7"
		| T8 -> "8"
		| T9 -> "9"
		| T10 -> "10"
		| Jack -> "J"
		| Queen -> "Q"
		| King -> "K"
		| As-> "A"

	let toStringVerbose value =
		match value with
		| T2 -> "2"
		| T3 -> "3"
		| T4 -> "4"
		| T5 -> "5"
		| T6 -> "6"
		| T7 -> "7"
		| T8 -> "8"
		| T9 -> "9"
		| T10 -> "10"
		| Jack -> "Jack"
		| Queen -> "Queen"
		| King -> "King"
		| As -> "As"

	let next value =
		match value with
		| T2 -> T3
		| T3 -> T4
		| T4 -> T5
		| T5 -> T6
		| T6 -> T7
		| T7 -> T8
		| T8 -> T9
		| T9 -> T10
		| T10 -> Jack
		| Jack -> Queen
		| Queen -> King
		| King -> As
		| As -> invalid_arg "Nothing after As"

	let prev value =
		match value with
		| T2 -> invalid_arg "Nothing before T2"
		| T3 -> T2
		| T4 -> T3
		| T5 -> T4
		| T6 -> T5
		| T7 -> T6
		| T8 -> T7
		| T9 -> T8
		| T10 -> T9
		| Jack -> T10
		| Queen -> Jack
		| King -> Queen
		| As -> King
end

module Color = struct
	type t = Spade | Heart | Diamond | Club

	let all () =
		[ Spade; Heart; Diamond; Club ]

	let toString t =
		match t with
		| Spade -> "S"
		| Heart -> "H"
		| Diamond -> "D"
		| Club -> "C"

	let toStringVerbose t =
		match t with
		| Spade -> "Spade"
		| Heart -> "Heart"
		| Diamond -> "Diamond"
		| Club -> "Club"
end

type t = { value: Value.t ; color: Color.t }

let newCard value color = { value ; color }

let allSpades =
	let allValue = Value.all () in
	let rec iterValue lst acc =
		match lst with
		| head::tail -> iterValue tail (acc @ [newCard head Color.Spade])
		| [] -> acc
	in iterValue allValue []

let allHearts =
	let allValue = Value.all () in
	let rec iterValue lst acc =
		match lst with
		| head::tail -> iterValue tail (acc @ [newCard head Color.Heart])
		| [] -> acc
	in iterValue allValue []

let allDiamonds =
	let allValue = Value.all () in
	let rec iterValue lst acc =
		match lst with
		| head::tail -> iterValue tail (acc @ [newCard head Color.Diamond])
		| [] -> acc
	in iterValue allValue []

let allClubs =
	let allValue = Value.all () in
	let rec iterValue lst acc =
		match lst with
		| head::tail -> iterValue tail (acc @ [newCard head Color.Club])
		| [] -> acc
	in iterValue allValue []

let all = allSpades @ allHearts @ allDiamonds @ allClubs

let getValue card = card.value
let getColor card = card.color

let toString card = Value.toString (getValue card) ^ Color.toString (getColor card)
let toStringVerbose card = Printf.sprintf "Card(%s, %s)" (Value.toStringVerbose (getValue card)) (Color.toStringVerbose (getColor card))

let compare c1 c2 =
	if getValue c1 = getValue c2 then
		0
	else if min c1 c2 = c2 then
		1
	else
		-1

let min c1 c2 =
	if getValue c1 > getValue c2 then
		c2
	else
		c1

let max c1 c2 =
	if getValue c1 < getValue c2 then
		c2
	else
		c1

let best lst =
	match lst with
	| [] -> invalid_arg "Empty list"
	| _ -> List.fold_left max (List.hd lst) lst

let isOf card color = getColor card = color
let isSpade card = isOf card Color.Spade
let isHeart card = isOf card Color.Heart
let isDiamond card = isOf card Color.Diamond
let isClub card = isOf card Color.Club
