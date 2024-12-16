(* Question 3 *)


(* ----- Fonction qui inverse l'ordre d'une liste. *)

let rec rev liste =
  match liste with
  | [] -> []
  | [_] -> liste
  | elt1::reste -> (rev reste) @ [elt1] ;;


(* ----- Fonction recursive terminale qui inverse l'ordre d'une liste. *)

let rec rev_rt liste acc =
  match liste with
  | [] -> acc
  | elt1::reste -> rev_rt reste (elt1::acc) ;;

(* Oui c'est bien une fonction récursive terminale : 
  - acc en entrée qui va recevoir à chaque appel récursif le résultat intermédiaire et retourné lors de l'appel du cas trivial
  - appel récursif en dernier élément à être exécuté
*)

(* -------------------- SUITE DE FIBONACCI -------------------- *)

(* La suite de Fibonacci est une fonction qui prend en entrée un entier n > 0, appelé rang, tel que : 
   Fibo(0) = 0 
   Fibo(1) = 1 
   Fibo(n) = Fibo(n-1) + Fibo(n-2)
*)

(* Question 4 : récursive classique *) 
let rec fibonacci n = 
  if n = 0 then 0 
  else if n = 1 then 1 
  else fibonacci(n-1) + fibonacci(n-2);;

fibonacci 35 ;;

(* Question 5 : récursive terminale *)
let fibona n = 
  let rec fibo n acc1 acc2 = 
    if n = 0 then acc2
    else fibo (n-1) acc2 (acc1+acc2)
  in fibo n 1 0;;

fibona 35
  
  
(* Question 6 *)
(* ----- Fonction qui calcule la valeur de Fibonacci au rang n par memoisation. *) 

let f n =
  let ht_dyn = Hashtbl.create n in
  let rec fibonacci_aux n =
    try Hashtbl.find ht_dyn n  
    with Not_found -> 
      let fn = match n with
        | 0 -> 0
        | 1 | 2 -> 1
        | _ -> fibonacci_aux (n-1) + fibonacci_aux (n-2) 
      in Hashtbl.add ht_dyn n fn; fn
  in fibonacci_aux n ;;

f 35
  
  
(* Question 7 *)
  
(* ----- Fibonacci avec des produits de matrices *)

(* ----- Definition du type matrice et de fonctions de multiplication. *)

(* hg = haut-gauche, hd = haut-droite, bg = bas-gauche, bd = bas-droite. *)
type matrice = { hg: int; hd: int; bg: int; bd: int } ;;

let mult_matrices { hg = hg1; hd = hd1; bg = bg1; bd = bd1 } { hg = hg2; hd = hd2; bg = bg2; bd = bd2 } =
  { 
    hg = hg1 * hg2 + hd1 * bg2 ;
    hd = hg1 * hd2 + hd1 * bd2 ;
    bg = bg1 * hg2 + bd1 * bg2 ;
    bd = bg1 * hd2 + bd1 * bd2    
  } ;;

let puissance_2 matrice = 
  mult_matrices matrice matrice ;;

let rec puissance_n matrice n =
  if 0 = n then { hg = 1; hd = 0; bg = 0; bd = 1 } (* Matrice identite. *)
  else if 0 = n mod 2 then puissance_2 (puissance_n matrice (n/2))
  else mult_matrices matrice (puissance_n matrice (n-1)) ;;

let rec fibon n =
  let base = {hg = 1; hd = 1; bg = 1; bd = 0} in let res = puissance_n base (n-1) in 
  res.hg;;

fibon 35
      


