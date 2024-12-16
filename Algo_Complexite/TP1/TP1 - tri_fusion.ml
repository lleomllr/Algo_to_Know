(* ----- Fonction coupe_en_2 du tri fusion. *) 

let rec coupe_en_2 liste = 
  match liste with 
  | [] -> [], []
  | [_] -> liste, []
  | elt1::elt2::reste -> 
      let partie1, partie2 = coupe_en_2 reste in 
      elt1::partie1, elt2::partie2 ;;
        
(* ----- Fonction fusionne du tri fusion. *) 

let rec fusionne liste1 liste2 fct_comparaison = 
  match liste1, liste2 with 
  | [], _ -> liste2
  | _, [] -> liste1
  | tete1::queue1, tete2::queue2 -> 
      if fct_comparaison liste1 tete1 
      then tete1::(fusionne queue1 liste2 fct_comparaison) 
      else tete2::(fusionne liste1 queue2 fct_comparaison) ;;