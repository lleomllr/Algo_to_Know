(*====================================================================================*)
(* Code mis à disposition pour le TP4 d'Algorithmique et Complexite. *)
(*====================================================================================*)

(* Pour lancer une session interactive d'OCaml dans un terminal : rlwrap ocaml. *)

(* Pour exécuter du code OCaml situe dans un fichier .ml depuis la 
session interactive : #use "nom_du_fichier.ml" ;; 

PS: Bien sur il faut changer nom_du_fichier par le vrai nom de votre fichier ... :) *)

(* Pour quitter la session interactive : #quit ;; *)



(* ----- Fonction qui remplit le tableau dynamique pour le probleme des sous-sequences communes. *)

let tab_sous_sequence s1 s2 =
  let taille_s1 = Array.length s1 in
  let taille_s2 = Array.length s2 in
  let tab_dynamique = Array.init (taille_s1 + 1) (function i -> Array.make (taille_s2 + 1) 0) in

  let rec tab_sous_sequence_aux i j =
    if i > taille_s1 then ()
    else if j > taille_s2 then 
      tab_sous_sequence_aux (i+1) 1
    else 
      begin
        if s1.(i-1) = s2.(j-1) then 
          tab_dynamique.(i).(j) <- tab_dynamique.(i-1).(j-1) + 1
        else 
          tab_dynamique.(i).(j) <- max tab_dynamique.(i-1).(j) tab_dynamique.(i).(j-1);
        tab_sous_sequence_aux i (j+1)
      end
  in tab_sous_sequence_aux 1 1 ;
    
  tab_dynamique ;;

let s1 = [|'A'; 'L'; 'G'; 'O' |] ;;
let s2 = [|'F'; 'A'; 'C'; 'I'; 'L'; 'E'|] ;;
let res = tab_sous_sequence s1 s2 ;; 
Array.iter(fun row -> Array.iter (Printf.printf "%d ") row; print_newline()) res ;;
         

(* ----- Fonction qui transforme un string en tableau de caracteres. *)

let string_to_array chaine = Array.of_list (List.init (String.length chaine) (String.get chaine)) ;;

(* ----- Fonction qui cherche la plus longue sous-sequence commune entre deux chaines de caracteres. *)

let sous_sequence sequence_1 sequence_2 =
  let taille_seq_1 = Array.length sequence_1 in
  let taille_seq_2 = Array.length sequence_2 in
  let tab_dynamique = tab_sous_sequence sequence_1 sequence_2 in
  let rec parcourir_tableau i j chaine_resultat =
    match i, j with
    | 0, 0 -> String.of_seq (List.to_seq chaine_resultat)
    | 0, _ -> parcourir_tableau i (j-1) chaine_resultat
    | _, 0 -> parcourir_tableau (i-1) j chaine_resultat
    | _, _ -> if sequence_1.(i-1) = sequence_2.(j-1)
        then parcourir_tableau (i-1) (j-1) (sequence_1.(i-1)::chaine_resultat)
        else if (tab_dynamique.(i).(j) = tab_dynamique.(i-1).(j) && tab_dynamique.(i).(j) > tab_dynamique.(i).(j-1))
        then parcourir_tableau (i-1) j chaine_resultat
        else parcourir_tableau i (j-1) chaine_resultat
  in (tab_dynamique.(taille_seq_1).(taille_seq_2),
      parcourir_tableau (taille_seq_1) (taille_seq_2) [],
      tab_dynamique) ;;

(* ----- TEST ----- *)
let sequence_1 = string_to_array "algo" ;;
let sequence_2 = string_to_array "facile"  ;;
let res = sous_sequence sequence_1 sequence_2 ;;

(* ----- Algorithm du sac à dos ----- *)
let tab_sac_a_dos max_sac tab_poids valeurs = 
  let taille_poids = Array.length tab_poids in 
  let matrix = Array.make_matrix (taille_poids + 1) (max_sac + 1) 0 in 
  
  let rec remplir i w = 
    if i > taille_poids then () 
    else if w > max_sac then 
      remplir (i+1) 0
    else
      begin 
        if i = 0 || w = 0 then 
          matrix.(i).(w) <- 0
        else if tab_poids.(i-1) <= w then 
          matrix.(i).(w) <- max matrix.(i-1).(w) (matrix.(i-1).(w - tab_poids.(i-1)) + valeurs.(i-1))
        else
          matrix.(i).(w) <- matrix.(i-1).(w); 
        remplir i (w+1)
      end
  in remplir 0 0; 
  matrix;;

let max_sac = 7 ;;
let poids = [|1; 3; 5; 3; 1 |];; 
let valeurs = [|1; 5; 2; 1; 3 |];;

let mat_tab = tab_sac_a_dos max_sac poids valeurs ;;

let print_tab matrix = 
  Array.iteri (fun i row -> 
      Printf.printf "Objet %d:" i; 
      Array.iter (Printf.printf "%3d") row; 
      print_newline()
    ) matrix ;;
print_tab mat_tab;;
  

(* ----- Fonction qui identifie les objets a prendre en fonction du contenu du tableau dynamique (sac). *)

let sac_a_dos umax tab_poids =
  let nb_objets = (Array.length umax) - 1 in
  let capacite = (Array.length umax.(0)) - 1 in
  let rec parcourir_tableau i c contenu_sac =
    match i, c with
    | 0, 0 -> contenu_sac
    | 0, _ -> parcourir_tableau i (c-1) contenu_sac
    | _, 0 -> parcourir_tableau (i-1) c contenu_sac
    | _, _ -> if umax.(i).(c) > umax.(i-1).(c)
        then parcourir_tableau (i-1) (c - tab_poids.(i-1)) (i::contenu_sac)
        else parcourir_tableau (i-1) c contenu_sac
  in (umax.(nb_objets).(capacite),
      parcourir_tableau nb_objets capacite []) ;;

let max_sac = 7 ;;
let poids = [|1; 3; 5; 3; 1 |];; 
let valeurs = [|1; 5; 2; 1; 3 |];;

let mat_tab = tab_sac_a_dos max_sac poids valeurs ;;
let res = sac_a_dos mat_tab poids;;

(* ----- Fonction generique pour memoiser une fonction. *)

let memoiser fonction =
  let hashtable = Hashtbl.create 123 in
  let rec fonction_aux x param =
    try Hashtbl.find hashtable x 
    with Not_found -> 
      let resultat = fonction fonction_aux x param
      in Hashtbl.add hashtable x resultat ; 
      resultat
  in (fonction_aux, hashtable) ;;    



(* ----- Fonction de fibonacci qui peut etre memoisee avec la fonction precedente. *)
let rec fibo fibo_aux x _ =
  if x < 2 then x 
  else (fibo_aux (x-1) ()) + (fibo_aux (x-2) ()) ;;

let (memo, _) = memoiser fibo;; 
let res = fibo memo 50 () ;; 


(* ----- Modelisation du diagramme de Pert de l'exemple. *)

let sommets = [ 1; 2; 3; 4; 5; 6 ] ;;
let arcs = [ (1, 2, 100); (1, 3, 200); (2, 4, 50); (2, 5, 60); (3, 4, 70);
             (3, 5, 80); (4, 6, 10); (5, 6, 20) ] ;;

let source = 1 ;;
let terminal = 6 ;;
let duree_defaut = 0 ;;
let date_fin = 7;;

(* ----- Fonction qui calcule les dates au plus tot et qui peut etre memoisee. *)

let rec plus_tot plus_tot_aux sommet (arcs, arcs_restants, sommet_source, duree_defaut, duree_max) = 
  if sommet = sommet_source then 0
  else (match arcs_restants with 
      |[] -> duree_max
      |(source, destination, duree)::reste -> 
          if destination <> sommet 
          then plus_tot plus_tot_aux sommet (arcs, reste, sommet_source, duree_defaut, duree_max)
          else plus_tot plus_tot_aux sommet (arcs, reste, sommet_source, duree_defaut, 
                                             (max duree_max ((plus_tot_aux source (arcs, arcs , sommet_source, duree_defaut, duree_defaut)) + duree ))));;


let rec plus_tard plus_tard_aux sommet (arcs, arcs_restants, sommet_puits, duree_defaut, duree_min) = 
  if sommet = sommet_puits then 0
  else (match arcs_restants with 
      |[] -> duree_min
      |(source, destination, duree)::reste -> 
          if source <> sommet 
          then plus_tard plus_tard_aux sommet (arcs, reste, sommet_puits, duree_defaut, duree_min)
          else plus_tard plus_tard_aux sommet (arcs, reste, sommet_puits, duree_defaut, 
                                               (min duree_min ((plus_tard_aux destination (arcs, arcs, sommet_puits, duree_defaut, duree_defaut)) - duree))));;

let (plus_tot_memo, plus_tot_hash) = memoiser plus_tot ;;
let (plus_tard_memo, plus_tard_hash) = memoiser plus_tard ;;

(* Calcul des dates au plus tôt *)
let date_plus_tot_sommet_4 = plus_tot_memo 6 (arcs, arcs, source, duree_defaut, duree_defaut)

(* Calcul des dates au plus tard -> Renvoie la différence entre Date de 'F' et le sommet choisi *)
let date_au_plus_tard_sommet_1 = plus_tard_memo 4 (arcs, arcs, terminal, duree_defaut, date_fin)

(* ----- Fonction qui affiche le contenu des hashtables de l'exercice sur Pert. *)

let print_ht_pert ht = Hashtbl.iter (fun x y -> Printf.printf "%c -> %d\n" x y) ht ;;

(* ----- Fonction qui prend en entrée les tables de hachage des dates au plus tôt et au plus tard et qui affiche les chemins critiques ----- *)



