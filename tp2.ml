(********************)
(*  TP2 - Caml      *)
(********************)



(*=== 1.PAIR ET IMPAIR ===*)

let rec pair = function
  | 0 -> true
  | 1 -> false
  | x -> impair(pred x)
and impair = function (* fonctions mutuellement recursives : on ecrit "and" *)
  | 0 -> false
  | 1 -> true
  | x -> pair(pred x);;

(* --------
TESTS :
pair 4;; true
pair 5;; false
impair 4;; false
impair 5;; true
   -------- *)



(*=== 2.VARIATIONS SUR SIGMA ===*)


(* question 1 *)
let rec sigma = function (x,y) ->
  if x>y then 0 else x + sigma(succ x, y);;

(* --------
TESTS :
sigma (1,4);; 10
sigma (4,7);; 22
sigma (12,11);; 11 (cas x>y)
   -------- *)


(* question 2 *)
let rec sigma2 = function f -> function (x,y) ->
  if x>y then 0 else f(x) + sigma2 f (succ x, y);;

(* --------
TESTS :
sigma2 (function x -> 2*x) (1,4);; 20
   -------- *)


(* question 3 *)
let rec sigma3 = function f -> function (a, i, b) -> function (fc,vi) ->
  if a>b then vi else fc (f a) (sigma3 f (a+i,i,b) (fc,vi));;

(* --------
TESTS :
sigma3 (function x -> 2*x) (1,1,4) ((+),0);; 20
sigma3 (function x -> 2*x) (1,2,6) ((-),2);; 4 (= 2*1-(2*3-(2*5-2)))
   -------- *)


(* question 4 *)
let rec sigma4 = function f -> function (predarret, fctincr) -> function (fc,vi) -> function a -> 
  if predarret a then vi else fc (f a) (sigma4 f (predarret, fctincr) (fc,vi) (fctincr a));;

(* --------
TESTS :
sigma4 (function x -> 2*x) ((function x -> x>4), (function x -> succ x)) ((+),0) 1;; 20
  -------- *)


(* question 5 *)
let cum = function f -> function (a,b,dx) -> function (fc,vi) -> sigma4 f ((function x -> x>b), (function x -> x+.dx)) (fc,vi) a;;


(* question 6 *)
let integre = function f -> function (a,b,dx) -> (cum (function x -> (f x)*.dx) (a, b, dx) ((+.),0.));; 


(* question 7 *)
integre (function x -> 1./.x) (1.,2.,0.005);;


(*=== 3.RECHERCHE DU MAXIMUM D'UNE FONCTION ===*)

let rec maxi = function f -> function (a,b) -> 
if b-.a < 0.00000001 then f b
else if (f (a+.((b-.a)/.3.))) > (f (b-.((b-.a)/.3.))) then maxi f (a , b-.((b-.a)/.3.))
else maxi f (a+.((b-.a)/.3.) , b);;
(*Correction : Utiliser les varibales locales ici*)

(* --------
TESTS :
maxi (fun x -> x*.x) (-.1., 3.);; 9.
   -------- *)
