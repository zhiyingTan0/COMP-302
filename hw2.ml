(* Question 1. *)

let rec pairlists (l1, l2) =
  if l1=[] then []
  else (List.hd l1,List.hd l2):: pairlists(List.tl l1 , List.tl l2)
;;

let wmean weights data =
  let x = pairlists(weights,data) in
  let y = List.map(fun n-> fst n *. snd n) x in 
  
  ( sumlist y)/. (sumlist weights)
  
;;

(* Question 2. *)

let rec memberof (n, l) =
  match l with
  |[]->false
  |x::xs -> 
      if x=n then true
      else memberof(n,xs)
;;

let rec remove (item, lst) =
  match lst with
  |[] -> []
  |x::xs ->
      if x != item then (x:: remove(item,xs))
      else remove(item,xs)
;;

(* Question 3. *)

let find_max l = 
  let rec helper(lst , max)= 
    match lst with
    |[] -> max
    |x::xs ->
        if x > max then helper(xs,x)
        else   helper(xs,max)
  in
  helper(l,List.hd l)
;;

(* Question 4. *)

let rec selsort l =
  match l with
  |[]->[]
  |x::xs -> find_max(x::xs)::selsort(remove(find_max(x::xs) , x::xs))
;;
