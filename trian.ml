type polygone = points list

let fusion_poly = fun polygone1 polygone2 ->
	let rotate_poly = fun poly ->
		match poly with 
		[] -> []
		| a::[] -> [a]
		| a::b::ps -> (b::ps)@[a] in

	let fusion_poly_rec = fun polya polyb ->
		match polya with
		[] -> []
		| pa1::pa2::pas -> match polyb with
							[] -> []
							| pb1::pb2::pbs when pa1 = pb1 and pa2 = pb2 ->
								(pa2::pas::pa1)@(List.rev pbs)
							| pb1::pb2::pbs when pa1 = pb2 and pa2 = pb1 ->
								pa2::pas::pa1::pbs
							| pb1::pb2::pbs -> 
