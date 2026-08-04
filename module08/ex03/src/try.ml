type 'a t = | Success of 'a | Failure of exn

let return x = Success (x)

let bind x f =
	match x with
	| Failure a -> Failure a
	| Success a ->
		try
			f a
		with e -> Failure e

let recover x f =
	match x with
	| Failure a -> f a
	| Success _ -> x

let filter x f =
	match x with
	| Failure _ -> x
	| Success a ->
		try
			if f a then
				Success a
			else
				Failure (Stdlib.Failure "Filter not satisfied")
		with e -> Failure e

let rec flatten x =
	match x with
	| Failure a -> Failure a
	| Success a -> a
