
(* question1 *)
let mysqrt (x:float) = 
  let rec helper (g)=
    if close(x,square(g)) = true then g 
    else helper((g+.(x/.g))/.2.0 )
            
  in 
  helper(x/.2.0)
;;

(*question 2*)
let cube_root (x:float) = 
  let rec helper(g)=
    if close(x,cube(g)) = true then g
    else helper((2.0 *. g +.(x/.square(g)))/.3.0)
  in 
  helper(x/.2.0)
;;

(*question 3*)
let fast_exp (base, power) = 
  if base = 0 then 0
  else if power = 0 then 1
  else
    let rec helper(a,b) = 
      if b = 1 then a 
      else helper((a * base), (b-1))
    in 
    helper(base, power)
;;
      
  
                                         

                           
