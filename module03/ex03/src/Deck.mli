module Card:
sig
	module Value:
	sig
		type t

		val toInt: t -> int
		val toString: t -> string
		val toStringVerbose: t -> string

		val next: t -> t
		val prev: t -> t
	end

	module Color:
	sig
		type t

		val toString: t -> string
		val toStringVerbose: t -> string
	end

	type t

	val newCard: Value.t -> Color.t -> t

	val getValue: t -> Value.t
	val getColor: t -> Color.t

	val toString: t -> string
	val toStringVerbose: t -> string

	val compare: t -> t -> int
	val min: t -> t -> t
	val max: t -> t -> t
	val best: t list -> t

	val isOf: t -> Color.t -> bool
	val isSpade: t -> bool
	val isHeart: t -> bool
	val isDiamond: t -> bool
	val isClub: t -> bool
end

type t

val newDeck: unit -> t

val toStringList: t -> string list
val toStringListVerbose: t -> string list

val drawCard: t -> Card.t * t
