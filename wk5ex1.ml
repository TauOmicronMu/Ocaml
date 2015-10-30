let rec zip xs ys = match (xs, ys) with
|[],[] -> []
|x :: xs, [] -> x :: xs
|[], y :: ys -> y :: ys
|x :: xs, y :: ys -> x :: y :: (zip xs ys);;

