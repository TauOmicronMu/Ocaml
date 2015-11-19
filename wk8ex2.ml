let split xs =
	let rec create_segment y acc = function
	| []                  -> acc
	| x :: xs when x != y -> acc
	| x :: xs when x = y  -> create_segment y (x :: acc) xs
	in
	let rec remove_segment y = function
	| []                  -> []
	| x :: xs when x != y -> x :: xs
	| x :: xs when x = y  -> remove_segment y xs 
	in
	let rec split' acc = function
	| []      -> acc
	| x :: xs -> split' (acc @ [create_segment x [] (x :: xs)]) (remove_segment x (x :: xs))
	in split' [] xs;;

