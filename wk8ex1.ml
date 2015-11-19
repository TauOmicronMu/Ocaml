replace a b acc = function
| []                 -> acc 
| x :: xs when x = a -> replace a b (b :: acc) xs
| x :: xs            -> replace a b (x :: acc) xs;;
