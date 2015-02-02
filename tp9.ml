(********************)
(*  TP9 - Caml      *)
(********************)

open List

(* question 1 *)
type 'a narbr = 
    F of 'a
  | N of 'a * 'a narbr list;;


(* question 2 *)
let rec compter = function
  | F(_) -> 1
  | N(e,l) -> match l with 
      | [] -> 0
      | a::reste -> (compter a) + (compter (N(e,reste)) );; 

let arbretest = N(5, [N(3, [F(6); N(8, [F(3); F(4)]); F(18)]); F(2) ]);;

(* --------
TESTS :
compter arbretest;; 5
   -------- *)


(* question 3 *)
let rec pluslongue = function
  | F(_) -> 1
  | N(e,l) -> 1 + (fold_left (fun r e -> max r (pluslongue e))) 0 l;;

(* --------
TESTS :
pluslongue arbretest;; 4
   -------- *)

(* question 4 *)
let rec listsa = function
  | F(a) -> [F(a)]
  | N(e,l) -> fold_left (fun r x -> r@(listsa x)) [N(e,l)] l ;;
	
	
 let a = N(5,[N(3,[F 4;N(7,[F 10;F 12;F 13]);F 20]);F 21]);;
 let a2 = N(5,[N(2,[F 4;N(6,[F 10;F 12;F 13]);F 20]);F 21]);;
 
 (* --------
TESTS :
 listsa a;; 
 [N (5, [N (3, [F 4; N (7, [F 10; F 12; F 13]); F 20]); F 21]);
 N (3, [F 4; N (7, [F 10; F 12; F 13]); F 20]); F 4;
 N (7, [F 10; F 12; F 13]); F 10; F 12; F 13; F 20; F 21]
    -------- *)


(* question 5 *)
let rec insertion e = function
  | [] -> []
  | a::reste -> (e::a)::(insertion e reste);; 

let rec listbr = function
  | F(a) -> [[a]]
  | N(e,l) -> insertion e (fold_left (fun r x -> r@(listbr x) ) [] l) ;;
 
 (* --------
TESTS :
listbr a;; [[5; 3; 4]; [5; 3; 7; 10]; [5; 3; 7; 12]; [5; 3; 7; 13]; [5; 3; 20]; [5; 21]]
    -------- *)


(* question 6 *)
let rec egal arb1 arb2 = 
match (arb1,arb2) with
  | (F(a1),F(a2)) -> a1=a2
  | (F(_),N(_,_)) -> false
  | (N(_,_),F(_)) -> false
  | (N(a1,l1),N(a2,l2)) -> if a1=a2 then fold_left2 (fun r e1 e2 -> r&&(egal e1 e2) ) true l1 l2 else false;;

 (* --------
TESTS :
egal a a;; true
egal a arbretest;; false
egal a a2;; false
    -------- *)

(* question 7 *)
let rec remplace a1 a2 = function
  | N(e,l) -> if (egal (N(e,l)) a1) then a2 else N(e, map (remplace a1 a2) l)
  | F(e) -> F(e);;

 (* --------
TESTS :
remplace (N(7,[F 10;F 12;F 13])) (N(5,[F 3;F 2]))  a;;  N (5, [N (3, [F 4; N (5, [F 3; F 2]); F 20]); F 21])
    -------- *)
