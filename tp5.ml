(********************)
(*  TP5 - Caml      *)
(********************)


(*=== 2.QUELQUES FONCTIONS SUR LES INTERVALLES ===*)
(* question 1 *)

let dans = function e -> function (binf, bsup) ->
(e >= binf && e <= bsup);;

(* --------
TESTS :
dans 5. (2.,6.);; true
dans 5. (6.,8.);; false
   -------- *)


(* question 2 *)

let vide = function (binf,bsup) ->
binf>=bsup;;

(* --------
TESTS :
vide (2., 5.);; false
vide (5., 5.);; true
vide (6., 5.);; true
   -------- *)


(* question 3 *)
let disjoint = function (binf1,bsup1) -> function (binf2,bsup2) ->
if vide (binf1,bsup1) || vide (binf2,bsup2)
then true
else bsup1<binf2 || bsup2<binf1;;

(* --------
TESTS :
disjoint (2.,3.) (4.,5.);; true
disjoint (2.,4.) (3.,5.);; false
disjoint (2.,7.) (4.,5.);; false
disjoint (4.,5.) (2.,3.);; true
   -------- *)


(* question 4 *)
let recouvre = function (binf1,bsup1) -> function (binf2,bsup2) ->
if vide (binf1,bsup1)
then if vide (binf2,bsup2)
     then (0,0)               (* cas i1 vide, i2 vide *)
     else (binf2,bsup2)         (* cas i1 vide, i2 non vide *)
else if vide (binf2,bsup2)     
     then (binf1,bsup1)         (* cas i1 non vide, i2 vide *)
     else ((if binf1<binf2 then binf1 else binf2), (if bsup1>bsup2 then bsup1 else bsup2));; (* cas i1 non vide, i2 non vide *)

(* --------
TESTS :
recouvre (2,3) (4,5);; (2, 5)
recouvre (2,4) (3,5);; (2, 5)
recouvre (3,3) (4,5);; (4, 5)
recouvre (2,3) (4,4);; (2, 3)
recouvre (3,3) (4,4);; (0, 0)
   -------- *)


(*=== 3.FONCTIONS RECURSIVES SUR DES LISTES D'INTERVALLES ===*)
(* question 1 *)
let rec enveloppe = function l ->
match l with
  | [] -> (0,0)
  | a::reste -> recouvre a (enveloppe reste);;


(* --------
TESTS :
enveloppe [(2,3);(5,7);(6,9);(12,14)];; (2, 14)
   -------- *)


(* question 2 *)
(* dansl verifie si un element e appartient à un des intervalles de la liste l *)
let rec dansl = function e -> function l ->
match l with
  | [] -> false
  | (a,b)::reste -> if dans e (a,b) then true else dansl e reste;;

(* listeEntier permet de creer la liste d'entier comprise dans un intervalle *)
let rec listeEntier = function (a,b)->
if a=b then [a]
else a::(listeEntier (a+1,b));;

(* --------
TESTS :
dansl 4 [(1,2);(3,5)];; true
dansl 4 [(1,2);(5,6)];; false
listeEntier (3,7);; [3; 4; 5; 6; 7]
   -------- *)

(* endehors2listes rend la liste des element de l2 qui ne sont pas presents dans la liste des intervalles de l1 *)
let rec endehors2listes = function l -> function l2 ->
match l2 with
  | [] -> []
  | a2::reste2 -> if dansl a2 l then endehors2listes l reste2 else a2::(endehors2listes l reste2);;


let endehors = function l ->
endehors2listes l (listeEntier (enveloppe l));;

(* --------
TESTS :
endehors [(2,3);(5,7);(6,9);(12,14)];; [4; 10; 11]
   -------- *)


(* question 3 *)
(* creaListe et crealiste2 permettent de creer à partir d'une lsite d'entier une liste d'intervalle *)
let rec creaListe = function l ->
match l with
  | [] -> []
  | a::[] -> [(a,a)]
  | a::b::reste -> if b==a+1 then (a,b)::(creaListe (b::reste)) else (a,a)::(creaListe (b::reste));;

   
let rec creaListe2 = function l ->
match l with
  | [] -> []
  | a::[] -> [a]
  | (a,b)::(c,d)::reste -> if b==c then (creaListe2 ((a,d)::reste)) else (a,b)::(creaListe2 ((c,d)::reste));;


(* --------
TESTS :
creaListe [4; 10; 11; 12];; [(4, 4); (10, 11); (11, 12); (12, 12)]
creaListe2 [(4, 4); (10, 11); (11, 12); (12, 12); (13,14); (14,16)];; [(4, 4); (10, 12); (13, 16)]
   -------- *)

let endehorsli = function l ->
creaListe2 (creaListe (endehors l));;

(* --------
TESTS :
endehorsli [(2,3);(5,7);(6,9);(12,14)];; [(4, 4); (10, 11)]
   -------- *)


(* question 4 *)
let rec supprvide = function l -> 
match l with 
  | [] -> []
  | a::reste -> if vide a then supprvide reste else a::(supprvide reste);;

(* --------
TESTS :
supprvide [(2,3);(3,4);(3,-2);(5,6)];; [(2, 3); (3, 4); (5, 6)]
   -------- *)

let rec ordrecroissant = function l ->
match l with
  | [] -> true
  | a::[] -> true
  | (a,b)::(c,d)::reste -> if c<a then false else ordrecroissant ((c,d)::reste);;

(* --------
TESTS :
ordrecroissant [(3,5);(4,6)];; true
ordrecroissant [(4,6);(3,5)];; false
   -------- *)

let rec tricroissant = function l -> 
match l with 
  | [] -> []
  | a::[] -> [a]
  | (a,b)::(c,d)::reste -> if c<a then (c,d)::(tricroissant ((a,b)::reste)) else (a,b)::(tricroissant ((c,d)::reste));;

(* --------
TESTS :
tricroissant [(4,6);(3,5);(2,7);(3,8)];; [(3, 5); (2, 7); (3, 8); (4, 6)]
   -------- *)

let rec simplifier = function l ->
if not (ordrecroissant l) then simplifier (tricroissant (supprvide l)) else
match l with 
  | [] -> []
  | a::[] -> [a]
  | a::b::reste -> if(disjoint a b) then a::(simplifier (b::reste)) else (simplifier ((recouvre a b)::reste));;

(* --------
TESTS :
simplifier [(3,6);(2,4);(1,-3);(7,9);(1,2);(10,-42);(3,5)];; [(1, 6); (7, 9)]
   -------- *)
