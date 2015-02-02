(********************)
(*  TP3 - Caml      *)
(********************)

#load "graphics.cma";;

(*=== 2.DEFINITION DES DONNEES ===*)

(* cf: partie 3 question 1 *)


(*=== 3.LA CONSTRUCTION DES TRANSFORMATIONS AFFINES ===*)

(* question 1 *)
type vector = {x : float; y : float}
type matrix = {ab : vector; cd : vector};;


let m1 = { ab = {x = 0.000 ; y = 0.0000} ; cd = {x = 0.172 ; y = 0.496}};;
let m2 = { ab = {x = 0.076 ; y = 0.3122} ; cd = {x = 0.257 ; y = 0.204}};;
let m3 = { ab = {x = 0.821 ; y = -0.028} ; cd = {x = 0.030 ; y = 0.845}};;
let m4 = { ab = {x = 0.024 ; y = -0.356} ; cd = {x = 0.323 ; y = 0.074}};;

let v1 = {x = 0.496 ; y = -0.091};;
let v2 = {x = 0.494 ; y = 0.133};;
let v3 = {x = 0.088 ; y = 0.176};;
let v4 = {x = 0.470 ; y = 0.260};;


(* question 2 *)
let mt = fun mat -> fun vec ->
{x = mat.ab.x*.vec.x +. mat.ab.y*.vec.y  ; y = mat.cd.x*.vec.x +. mat.cd.y*.vec.y};; 

(* --------
TESTS :
mt m1 v1;; - : vector = {x = 0.; y = 0.040176}
mt m2 v3;; - : vector = {x = 0.0616351999999999942; y = 0.05852}
   -------- *)


(* question 3 *)
let sv = fun v1 -> fun v2 ->
{x = v1.x +. v2.x ; y = v1.y +. v2.y};;

(* --------
TESTS :
sv v1 v2;; - : vector = {x = 0.99; y = 0.0420000000000000095}
sv v2 v3;; - : vector = {x = 0.582; y = 0.309}
   -------- *)


(* question 4 *)
let genapplin = fun mat ->
function vec -> mt mat vec;; 
(* function vec -> {x = mat.ab.x*.vec.x +. mat.ab.y*.vec.y ; y = mat.cd.x*.vec.x +. mat.cd.y*.vec.y};; *)

(* --------
TESTS :
genapplin m1 v1;; - : vector = {x = 0.; y = 0.040176}
genapplin m2 v3;; - : vector = {x = 0.0616351999999999942; y = 0.05852}
  -------- *)


(* question 5 *)
let gentraffine = fun applin -> fun vector ->
fun vec -> sv (applin vec) vector;;  

(* --------
TESTS :
gentraffine (genapplin m1) v2 v1;;  - : vector = {x = 0.494; y = 0.173176}
  -------- *)


(* question 6 *)
let applin1 = genapplin m1;;
let applin2 = genapplin m2;;
let applin3 = genapplin m3;;
let applin4 = genapplin m4;;


(* question 7 *)
let let4tr = (gentraffine applin1 v1, gentraffine applin2 v2, gentraffine applin3 v3, gentraffine applin4 v4);;



(*=== 4.LE CHOIX D'UNE TRANSFORMATION PARMI 4 ===*)

(* question 1 *)
let elemrang = fun (a,b,c,d) -> function
| 0 -> a
| 1 -> b
| 2 -> c
| 3 -> d
| _ -> a;; (* Cas d'erreur, on renvoie le premier element *)

(* --------
TESTS :
elemrang (2,4,6,8) 3;; 8
elemrang (2,4,6,8) 4;; 2
  -------- *)


(* question 2 *)
let traff ()  = elemrang let4tr (Random.int 4) ;;

(* Affichage des points *)
let aff = function v -> Graphics.plot(int_of_float(v.x*.500.))(int_of_float(v.y*.500.)); v;;
(* Integrer cette fonction aff dans la fonction suite *)

(*=== 5.LA SUITE DES POINTS TRANSFORMES ===*)

let rec suite = function
 0 -> {x = 0.5 ; y = 0.0}
| n -> traff () (aff (suite (n-1)));;

(* --------
TESTS :
suite 3;;
  -------- *)


(*=== 6.L'AFFICHAGE DE LA SUITE DE POINTS ===*)

let tout n =
Graphics.open_graph " "; (* ouvre l'ecran graphique *)
suite n;                 (* affiche la suite de points *)
read_int();              (* fait une pause (attend la lecture d'un entier) *)
Graphics.close_graph();;  (* ferme l'ecran graphique *)



