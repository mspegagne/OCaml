(********************)
(*  TP7 - Caml      *)
(********************)

open List
(* Ne pas utiliser les fonction récursives pour ce TP *)
(*=== 1.Ensembles ===*)

(* question 1 *)
let union l1 l2 = 
fold_left (fun r x -> if (mem x l2) then r else x::r) l2 l1;; (* x : element courant; r : resultat courant; resultat a l2 au depart; l1 : liste parcourue *) 

let inter l1 l2 = 
fold_left (fun r x -> if (mem x l2) then x::r else r)  [] l1;;



(* --------
TESTS :
union [1;4;5;6][3;4;7;9];; [6; 5; 1; 3; 4; 7; 9]
inter [1;4;5;6][3;4;7;6;9];;  [6; 4]
   -------- *)


(* question 2 *)
let touscouples l1 l2 =
flatten( map (fun x -> ( map (fun y -> (x,y)) l2) ) l1);;

(* --------
TESTS :
touscouples [1;4;7] [8;3];; [(1, 8); (1, 3); (4, 8); (4, 3); (7, 8); (7, 3)]
   -------- *)


(* question 3 *)
let partition l =
fold_left (fun r x -> r @ (map (fun e -> x::e) r )) [[]] l;;

(* --------
TESTS :
partition [1;2;3];; [[]; [1]; [2]; [2; 1]; [3]; [3; 1]; [3; 2]; [3; 2; 1]]
   -------- *)

(*=== 2.Booleens et predicats ===*)

(* question 1 *)
let conj p1 p2 =
function x -> (p1 x) && (p2 x);;

(* --------
TESTS :
conj (fun x -> x>2) (fun y -> y>4) 5;; true
conj (fun x -> x>2) (fun y -> y>4) 3;; false
   -------- *)

(* question 2 *)
let conjlist lp =
fold_left (fun r p -> conj p r) (fun x -> true) lp;;

(* --------
TESTS :
let listpred = [(fun x -> x>2); (fun y -> y>4); (fun x -> x>6); (fun y -> y>8)];;
conjlist listpred 9;; true
conjlist listpred 5;; false
   -------- *)


(* question 3 *)
let filtrer lp le =
filter (conjlist lp) le;;

(* --------
TESTS :
let listpred = [(fun x -> x>2); (fun y -> y>4); (fun x -> x>6); (fun y -> y>8)];;
filtrer listpred [2;10;16;4;12];; [10; 16; 12]
   -------- *)
