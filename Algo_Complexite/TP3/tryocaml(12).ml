(*====================================================================================*)
(* Code mis à disposition pour le TP3 d'Algorithmique et Complexite. *)
(*====================================================================================*)

(* Pour lancer une session interactive d'OCaml dans un terminal : rlwrap ocaml. *)

(* Pour exécuter du code OCaml situe dans un fichier .ml depuis la 
session interactive : #use "nom_du_fichier.ml" ;; 

PS: Bien sur il faut changer nom_du_fichier par le vrai nom de votre fichier ... :) *)

(* Pour quitter la session interactive : #quit ;; *)

(* ----- Definition du type graphe. *)

type 'a graphe = { 
  sommets: 'a array; 
  aretes: ((int * float) list) array 
} ;;

let g = {
  sommets = [|"A"; "B"; "C"; "D"; "E"; "F"; "G"; "H"; "I"; "J" |]; 
  aretes = [|
    [ (1, 85.0); (2, 217.0); (4, 173.0);];
    [ (0, 85.0); (5, 80.0)]; 
    [ (0, 217.0); (6, 186.0); (7, 103.0)]; 
    [ (7, 183.0)]; 
    [ (0, 173.0); (9, 502.0)]; 
    [ (1, 80.0); (8, 250.0)]; 
    [ (2, 186.0)]; 
    [ (2, 103.0); (3, 183.0)]; 
    [ (4, 250.0); (9, 84.0)]; 
    [ (7, 167.0); (8, 84.0); (4, 502.0)]; 
  |] 
};;

let () = 
  Array.iteri(fun i s -> Printf.printf "Sommet %d : %s\n" i s) g.sommets; 
  Array.iteri(fun i lst ->
      Printf.printf "Aretes du sommet %s :\n" g.sommets.(i); 
      List.iter(fun(dest, poids) -> 
          Printf.printf " vers %s avec un poids de %.1f\n" g.sommets.(dest) poids 
        ) lst
    ) g.aretes; 
;;

(* ----- Algorithme de Dijkstra. *)
type graphe = (int * int) list array
    
let dijkstra graphe pt_depart = 
  let n = Array.length graphe in
  let dist = Array.make n infinity in 
  let pred = Array.make n (-1) in 
  dist.(pt_depart) <- 0.0; 
  pred.(pt_depart) <- pt_depart; 
  
  let rec plus_court_chemin non_visit = 
    if non_visit = [] then (dist, pred)
    else
      let dist_min, u = List.fold_left (fun (min, idx_min) i ->
          if dist.(i) < min then (dist.(i), i) else (min, idx_min) 
        ) (infinity, -1) non_visit in 
      
      if dist_min = infinity then (dist, pred) 
      else
        let voisins = graphe.(u) in 
        List.iter (fun (v, poids) -> 
            if List.mem v non_visit then 
              let new_dist = dist.(u) +. float_of_int poids in 
              if new_dist < dist.(v) then begin 
                dist.(v) <- new_dist; 
                pred.(v) <- u
              end
          ) voisins; 
        plus_court_chemin (List.filter ((<>) u) non_visit ) 
  in 
  
  let non_visit = List.init n (fun  x -> x) in 
  plus_court_chemin non_visit
;;

let g = [|
  [ (1, 85); (2, 217); (4, 173);];
  [ (0, 85); (5, 80)]; 
  [ (0, 217); (6, 186); (7, 103)]; 
  [ (7, 183)]; 
  [ (0, 173); (9, 502)]; 
  [ (1, 80); (8, 250)]; 
  [ (2, 186)]; 
  [ (2, 103); (3, 183)]; 
  [ (4, 250); (9, 84)]; 
  [ (7, 167); (8, 84); (4, 502)]; 
|] 

let dist, pred = dijkstra g 0;;
  
  

(* ----- Definition du type tas. *)

type 't tas = Vide | Noeud of 't tas * 't * 't tas ;;

let rec ajouter element tas = 
  match tas with
  | Vide -> Noeud (Vide, element, Vide)
  | Noeud (fils_gauche, valeur, fils_droit) -> 
      Noeud (fils_droit, min valeur element, ajouter (max valeur element) fils_gauche) ;;

let rec ajouter_plusieurs liste tas = 
  match liste with
  | [] -> tas
  | elt1::reste -> ajouter elt1 (ajouter_plusieurs reste tas) ;;

let rec supprimer_premier_noeud tas = 
  match tas with
  | Vide -> Vide
  | Noeud (Vide, _, fils_droit) -> fils_droit
  | Noeud (fils_gauche, _, Vide) -> fils_gauche
  | Noeud ((Noeud (fg_fg, fg_val, fg_fd) as fg), valeur, (Noeud (fd_fg, fd_val, fd_fd) as fd)) -> 
      if fg_val < fd_val
      then Noeud (supprimer_premier_noeud fg, fg_val, fd)
      else Noeud (fg, fd_val, supprimer_premier_noeud fd) ;;

let vider tas = 
  let rec vider_rt tas acc =
    match tas with
    | Vide -> acc
    | Noeud (_, valeur, _) as noeud -> vider_rt (supprimer_premier_noeud noeud) (acc @ [valeur])
  in vider_rt tas [] ;; 

let creer_tas liste_elements = ajouter_plusieurs liste_elements Vide ;;


