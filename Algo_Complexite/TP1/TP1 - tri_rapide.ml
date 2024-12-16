(* Tri Rapide : 
réorganiser récursivement une liste par rapport à un élément donné = pivot 

Procédure : 
- Si la liste est vide, elle est déjà triée 
- Si la liste contient un seul élément, elle est déjà triée 
- Sinon : choisir un pivot + id tous les elems < pivot + id tous les elems = pivot + id tous les elems > pivot
          trier les listes des elems + petits et + grands 
          concaténer 
*)


let rec tri_rapide fct_comparaison liste = 
  match liste with 
  | [] -> []
  | pivot :: reste -> 
      let gauche, droit = List.partition (fun x -> fct_comparaison x pivot) reste in 
      tri_rapide fct_comparaison gauche @ [pivot] @ tri_rapide fct_comparaison droit;;


let cmp_croissant el1 el2 = 
  el1 < el2 ;;
  
let cmp_decroissant el1 el2 = 
  el1 > el2 ;;
  
let cmp_coo_croissant (x1, y1) (x2, y2) = 
  x1 < x2 
          

let liste_non_triee = [5; 2; 3; 8; 1];;
let liste_coo_non_triee = [(9,1);(0,2);(5,3);(9,4);(6,5)] ;;

let liste_triee = tri_rapide cmp_croissant liste_non_triee;; 
let liste_triee = tri_rapide cmp_decroissant liste_non_triee;;
let liste_triee = tri_rapide cmp_coo_croissant liste_coo_non_triee;;
        
