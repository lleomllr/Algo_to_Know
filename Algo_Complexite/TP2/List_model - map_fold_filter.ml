(* Question 1 *)

(* Modèle List : 

   - map : associe à tout élément d'une liste de type a un élément de type b. 
           Le premier paramètre est une fonction qui fait cette association
           Le second est la liste de départ et la valeur retournée est la liste obtenue. 
   
   - fold_left : prend tous les éléments de la liste et les compacte en un seul (parcourt du début vers la fin)
   
   - fold_right : prend tous les éléments de la liste et les compacte en un seul (parcourt de la fin vers le debut)
   
   - filter : filtre une liste en fonction d'une condition. 
              Le premier paramètre est une fonction et le deuxième une liste 
              La première fonction va être appliquée à tous les éléments de la liste et si true élément gardé. 
*)

(* Question 2 *)

 (* --- Fonction map --- *)
let rec map f = function
  | [] -> [] 
  | a :: b -> f a :: map f b;;

 (* --- Fonction fold_left --- *)
let rec fold_l f int liste = 
  match liste with 
  | [] -> int 
  | a :: liste -> fold_l f (f int a) liste ;;

 (* --- Fonction fold_right --- *)
let rec fold_r f liste int = 
  match liste with
  | [] -> int 
  | a :: liste -> f a (fold_r f liste int) ;;

(* Test *)
let addition x y = x + y 
let somme = fold_l (+) 0 ;;
somme [3; 4; 10]
    

