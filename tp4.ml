(********************)
(*  TP4 - Caml      *)
(********************)


(* question 1 *)
let rec longueur = function
  | [] -> 0
  | a::reste -> 1 + longueur reste;;

(* --------
TESTS :
longueur [1;2;3];; 3
longueur [];; 0
   -------- *)


(* question 2 *)
let rec appartient = function e -> function
  | [] -> false
  | a::reste -> a==e || appartient e reste;;

(* --------
TESTS :
appartient 1 [1;2;3;4;5];; true
appartient 3 [1;2;3;4;5];; true
appartient 6 [1;2;3;4;5];; false
   -------- *)


(* question 3 *)
let rec rang = function e -> function l ->
match l with 
  | [] -> 0
  | a::reste ->
if (appartient e l) then if a==e then 1 else 1+rang e reste else 0;;

(* --------
TESTS :
rang 1 [1;2;3;4;5];; 1
rang 3 [1;2;3;4;5];; 3
rang 6 [1;2;3;4;5];; 0
   -------- *)


(* question 4 *)
let rec debliste = function l -> function n ->
if n > longueur l then l
else match l with
  | [] -> l
  | a::reste ->
      match n with
	| 0 -> [] 
	| n -> a::(debliste reste (n-1));; 

(* --------
TESTS :
debliste [1;2;3;4;5] 3;; [1;2;3]
debliste [1;2;3;4;5] 0;; []
debliste [1;2;3;4;5] 6;; [1;2;3;4;5]
   -------- *)


(* question 5 *)
let rec finliste = function l -> function n ->
if n > longueur l then l
else match l with
  | [] -> l
  | a::reste ->
      match n with
	| 0 -> []
	| n -> if (longueur l)==n then a::(finliste reste (n-1)) else finliste reste n;;

(* --------
TESTS :
finliste [1;2;3;4;5] 3;; [3;4;5]
finliste [1;2;3;4;5] 0;; []
finliste [1;2;3;4;5] 6;; [1;2;3;4;5]
   -------- *)


(* question 6 *)
let rec remplace = function x -> function y -> function l ->
match l with
  | [] -> []
  | a::reste ->
if x==a then y::(remplace x y reste)
else a::(remplace x y reste);;

(* --------
TESTS :
remplace 2 42 [1;2;3;4;2];; [1;42;3;4;42]
   -------- *)


(* question 7 *)
let rec entete = function l -> function l1 ->
match l with
  | [] -> true
  | a::reste ->
      match l1 with 
	| a1::reste1 -> if a==a1 then entete reste reste1 else false
	| [] -> l==[];;

(* --------
TESTS :
entete [1;2;3] [1;2;3;2;5];; true
entete [] [1;2;3;2;5];; true
entete [1;2;3;2;5] [];; false
entete [1;2;3;2;5] [1;2;3];; false
   -------- *)


(* question 8 *)
let rec sousliste = function l -> function l1 ->
match l with
  | [] -> true
  | a::reste ->
      match l1 with 
	| a1::reste1 -> if a==a1 then entete reste reste1 else sousliste l reste1
	| [] -> l==[];;

(* --------
TESTS :
sousliste [1;2;3] [1;2;3;2;5];; true
sousliste [2;3;2] [1;2;3;2;5];; true
sousliste [2;3;5] [1;2;3;2;5];; false
   -------- *)


(* question 9 *)
let rec oter = function l -> function l1 ->
match l with
  | [] -> l1
  | a::reste ->
      match l1 with 
	| a1::reste1 -> if entete l l1 then oter reste reste1 else l1
	| [] -> l1;;

(* --------
TESTS :
oter [1;2;3] [1;2;3;2;5];; [2; 5]
oter [2;3;2] [1;2;3;2;5];; [1; 2; 3; 2; 5]
oter [2;3;5] [1;2;3;2;5];; [1; 2; 3; 2; 5]
   -------- *)


(* question 10 *)
let rec remplacel = function l1 -> function l2 -> function l ->
match l with
  | [] -> l
  | a::reste ->
      if entete l1 l then l2@(remplacel l1 l2 (oter l1 l)) else a::(remplacel l1 l2 reste);;

(* --------
TESTS :
remplacel [1;2;3] [5;6] [1;2;3;2;1;2;3;5];; [5; 6; 2; 5; 6; 5]
   -------- *)


(* question 11 *)
let rec supprimel = function l1 -> function l ->
remplacel l1 [] l;;

(* --------
TESTS :
supprimel [1;2] [1;2;3;1;2;4];;
   -------- *)
	
