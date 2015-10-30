let rec exclude xs y = match xs with
  | [] -> []
  | x::xs -> if x = y then exclude xs y else [x] @ exclude xs y;;
  
let rec count xs m n = match xs with
  | [] -> n
  | x::xs -> if x = m then count xs m (n+1) else count xs m n;;
 
let rec histo xs = match xs with
  | [] -> []
  | x::xs -> histo (exclude xs x) @ [(x, count xs x 1)];;
