(* arbre binaire de recherche = structure récursive composée de noeuds 
- Soit un noeud est vide 
- Soit il est associé à une valeur et possède exactement deux fils droit et gauche (eux-mêmes des noeuds) 

Un noeud dont les deux fils sont vides = feuille de l'arbre 

Tri par arbre binaire transforme une liste en arbre binaire de recherche puis parcourt la structure de gauche à droite pour récuperer les 
éléments triés
*)

(* ----- Definition du type arbre et fonction principale du tri par arbre. *)

type 't arbre = Vide | Noeud of 't arbre * 't * 't arbre ;; (* Déclaration d'un nv type appelé 'arbre' qui contient des val de type t *)

(* ---- Question 4 *)
let rec inserer fct_comparaison arbre element = 
  match arbre with
  | Vide -> Noeud(Vide, element, Vide)
  | Noeud(fils_gauche, valeur, fils_droit) ->
      if fct_comparaison element valeur then 
        Noeud(inserer fct_comparaison fils_gauche element, valeur, fils_droit)
      else
        Noeud(fils_gauche, valeur, inserer fct_comparaison fils_droit element);;

                                                                               
(* ---- Question 5 *) 
let rec lire_prefixe arbre_binaire acc = 
  match arbre_binaire with 
  | Vide -> acc
  | Noeud(fils_gauche, valeur, fils_droit) ->
      let acc_gauche = lire_prefixe fils_gauche acc in 
      let acc_avec_val = acc_gauche @ [valeur] in 
      lire_prefixe fils_droit acc_avec_val
  
                                                            
let tri_arbre liste fct_comparaison = 
  let arbre_binaire = List.fold_left (inserer fct_comparaison) Vide liste in (* applique une fonction 'inserer' sur chaque element de la liste en la parcourant de gauche à droite *)
  lire_prefixe arbre_binaire [] ;;

(* ----- Modele de fonction qui etudie la decomposition d'un arbre. *) 

let fonction_qui_utilise_un arbre = 
  match arbre with
  | Vide -> Noeud(Vide, Vide, Vide) (* Les instructions si l'arbre est un noeud vide. *)
  | Noeud(fils_gauche, valeur, fils_droit) -> (* Les instructions si l'arbre n'est pas un noeud vide. *) 
      (fonction_qui_utilise_un fils_gauche) + (fonction_qui_utilise_un fils_droit);;
                                                                                   
(* ----- Exemples d'instances d'arbres. *)

(* Arbre vide. *)
let arbre_vide = Vide ;;
(* Arbre d'entiers compose d'un seul element (= feuille). *)
let arbre_feuille = Noeud(Vide, 1, Vide) ;;
(* Arbre d'entiers compose d'un seul noeud avec un fils. *)
let arbre_un_fils = Noeud(Vide, 1, Noeud(Vide, 2, Vide)) ;;
(* Arbre d'entiers compose d'un seul noeud avec deux fils. *)
let arbre_deux_fils = Noeud(Noeud(Vide, 0, Vide), 1, Noeud(Vide, 2, Vide)) ;;

(* ---- Question 6 *)

let cmp_croissant el1 el2 = 
  el1 < el2 ;;
  
let cmp_decroissant el1 el2 = 
  el1 > el2 ;;
  
let cmp_coo_croissant (x1, y1) (x2, y2) = 
  x1 < x2 ;;

tri_arbre arbre_vide cmp_croissant;;
tri_arbre arbre_vide cmp_decroissant;;
tri_arbre arbre_feuille cmp_croissant;;