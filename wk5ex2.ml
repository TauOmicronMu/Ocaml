let rec shiftrotate xs n =  match xs with
|[] -> []
|x :: xs -> if n = 0 then x :: xs 
            else shiftrotate (xs @ [x]) (n-1);;

