(********************)
(*  TP6 - Caml      *)
(********************)

open List

(*=== 1.Le tri rapide ===*)
let rec create2lists l = 
match l with
  | [] -> ([],[])
  | [a] -> ([a],[])
  | a::b::reste -> let (l1,l2) = 
      create2lists (a::reste) in
      if a<b then (l1, b::l2) else (b::l1, l2);;

(* --------
TESTS :
create2lists [6;4;5;6;7;8];; ([4; 5; 6; 6], [7; 8])
   -------- *)

let rec qs l =
  let (l1,l2) = create2lists l in 
    ((qs l1) @ (qs l2));;
     
(* --------
TESTS :
qs [42;3;5;7;6;2;1];; [1; 2; 3; 5; 6; 7; 42]
   -------- *)


(*=== 2.Kieme plus petit element ===*)
let rec kieme k l = 
  let (l1,l2) = create2lists l in
    match l with
      | []-> raise (Failure "La liste est incompatible")
      | a::reste ->
	  if k=0 then a else
	    if length l1 <=k then kieme (k - (length l1)) l2 else kieme k l1;;

(* --------
TESTS :
kieme 3 [4;5;0;1;2];; 4
kieme 3 [0;1];; Failure "La liste est incompatible".
   -------- *)


(*=== 3.Le tri a bulle ===*)
let rec jqastable x f =
  if not (f x = x) then jqastable (f x) f else x;;

let rec echange = function
      | [] -> []
      | [a] -> [a]
      | a::b::reste -> if a>b then b::(echange (a::reste)) else a::(echange (b::reste)) ;; 

let tribulle l = 
  jqastable l echange;;

(* --------
TESTS :
tribulle [4;12;27;-12;7;8;1;3;6;12;42];;[-12; 1; 3; 4; 6; 7; 8; 12; 12; 27; 42]
   -------- *)
  

(*=== 4.Partition d'un entier n ===*)
let rec insert k l = 
match l with
  | [] -> []
  | a::reste -> ([k] @ a)::(insert k reste);;

(* --------
TESTS :
insert 1 [[3;5];[7;3;9];[];[6]];; [[1; 3; 5]; [1; 7; 3; 9]; [1]; [1; 6]]
   -------- *)

let rec partition2  = function
  | (0,0) -> [[]]
  | (n,0) -> []
  | (n,k) -> if k>n then partition2 (n,n)
else (insert k (partition2 ((n-k),k) )) @ (partition2 (n,(k-1)) );;

let partition n =
  partition2 (n,n);;

(* --------
TESTS :
partition 6;; [[6]; [5; 1]; [4; 2]; [4; 1; 1]; [3; 3]; [3; 2; 1]; [3; 1; 1; 1]; [2; 2; 2]; [2; 2; 1; 1]; [2; 1; 1; 1; 1]; [1; 1; 1; 1; 1; 1]]
   -------- *)
