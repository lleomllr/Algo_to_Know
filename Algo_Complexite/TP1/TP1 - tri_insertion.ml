(*Question 1 :*)

(* ------ Tri par insertion sans fonction de comparaison. *)

let rec insere_dans_liste_triee element liste = (* déclaration d'une fonction récursive avec deux paramètres 'element' et 'liste'  *)
  match liste with (*  *)
  | []->[element] (* dans le cas d'une liste vide *)
  | elt1::reste-> (* dans le cas où la liste n'est pas vide. 'elt1' correspond au 1er élément de la liste. 'reste' correspond au reste de la liste *)
      if element < elt1 then element::liste (* Si il y a un élément choisi est inférieure au premier élément, on l'ajoute dans la liste avant 'elt1' *)
      else elt1::(insere_dans_liste_triee element reste);; (* Sinon appel récursif : continue de chercher ou placer 'element' *)

let rec tri_insertion liste = (* déclaration d'une fonction récursive avec un paramètre 'liste' *)
  match liste with 
  | []->[] (* Si liste vide, on renvoie une liste vide *)
  | elt1::reste->insere_dans_liste_triee elt1(tri_insertion reste);; (* *)

(* Question 2 *)

(* ----- Tri par insertion avec fonction de comparaison. *)

let rec insere_dans_liste_triee element liste fct_comparaison =
  match liste with 
  | [] -> [element]
  | elt1::reste -> 
      if fct_comparaison element elt1 
      then element::liste 
      else elt1::(insere_dans_liste_triee element reste fct_comparaison) ;;

let rec tri_insertion liste fct_comparaison =
  match liste with
  | [] -> []
  | elt1::reste -> insere_dans_liste_triee elt1 (tri_insertion reste fct_comparaison) fct_comparaison ;;

let cmp_croissant el1 el2 = 
  el1 < el2 ;;
  
let cmp_decroissant el1 el2 = 
  el1 > el2 ;;
  
let cmp_coo_croissant (x1, y1) (x2, y2) = 
  x1 < x2 
          
(* ----- Tests du tri par insertion. *)

let liste_a_trier = [9;0;5;9;6] ;;
let liste_coo_a_trier = [(9,1);(0,2);(5,3);(9,4);(6,5)] ;;

tri_insertion liste_a_trier cmp_croissant ;;
tri_insertion liste_a_trier cmp_decroissant ;;
tri_insertion liste_coo_a_trier cmp_coo_croissant ;;