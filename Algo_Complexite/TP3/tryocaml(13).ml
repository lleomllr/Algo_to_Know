(* 
Groupe : MEILLIER Léo - TP4 
Enseignant : DIARRA Ibrahim 
*)


(* ---- TP3 NOTE - Tri par dénombrement ---- *)




let rec tri_denombrement e n res =
  if (n < Array.length e)
  then
    tri_denombrement e (n + 1) (Array.append res (Array.make e.(n) n))
  else
    res
;;

let l = [3; 1; 2; 3; 0; 1; 2; 0];;
let max_val = List.fold_left max 0 l;;
let e = Array.make (max_val+1) 0;;

let rec denombrement liste e = 
  match liste with 
  |tete::queue -> begin let a = e.(tete) in e.(tete) <- a+1; denombrement queue e end 
  |[]->e;; 
let e = denombrement l e;; 
tri_denombrement e 0 [||];;

(*
  let freq = Array.make (max_val + 1) 0;;

  Array.iter (fun x -> freq.(x) <- freq.(x) + 1) e;;

  let res = tri_denombrement freq 0 [||];;

  Array.iter (fun x -> Printf.printf "%d " x) res;;*)


