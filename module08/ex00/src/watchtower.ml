type hour = int
let zero = 12

let add h1 h2 =
	let res = (h1 + h2) mod zero
	in if res = 0 then zero
	else res

let sub h1 h2 =
	let res = (h1 - h2) mod zero
	in if res <= 0 then res + zero
	else res
