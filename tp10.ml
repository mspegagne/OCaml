(********************)
(*  TP10 - Caml     *)
(********************)

#load "graphics.cma";;
#load "carte.cmo";;
open Graphics;;
open Carte;;

open List
(*=== 1.Definition de la nature des objets ===*)

(*type coul = 
    Trefle | Carreau | Coeur | Pique;;

type haut = 
    Sept | Huit | Neuf | Dix | Valet | Dame | Roi | As;;

type carte = C of haut * coul;;*)

(* accesseurs *)

(*let haut (C(h,c)) = h;;
let coul (C(h,c)) = c;;*)
(*=== 2.Construction d'un paquet battu de 32 cartes ===*)
(*let random_couleur n =
match (Random.int n) with
  | 0 -> Trefle
  | 1 -> Carreau
  | 2 -> Coeur
  | _ -> Pique;;

let random_hauteur n =
match (Random.int n) with
  | 0 -> Sept
  | 1 -> Huit
  | 2 -> Neuf
  | 3 -> Dix
  | 4 -> Valet
  | 5 -> Dame
  | 6 -> Roi
  | _ -> As;;

let random_carte () = C(random_hauteur 8,random_couleur 4);;*)

let rec ajtcarte lc =
  let a = (random_carte ()) in
    if (exists (fun x -> ((haut a) = (haut x))&&((coul a) = (coul x))) lc) 
    then ajtcarte lc 
    else a::lc;;

(* --------
TESTS :
ajtcarte [random_carte (); random_carte ()];; [C (Dix, Trefle); C (Roi, Carreau); C (Neuf, Pique)]
   -------- *)


let rec faitjeu = function
  | 0 -> []
  | n -> ajtcarte (faitjeu (n-1));;

(* --------
TESTS :
faitjeu 32;; 
[C (As, Pique); C (Neuf, Trefle); C (Neuf, Pique); C (Valet, Coeur);
 C (Valet, Trefle); C (Valet, Carreau); C (Dix, Coeur); C (Neuf, Carreau);
 C (As, Trefle); C (Dix, Pique); C (Sept, Trefle); C (Huit, Pique);
 C (Roi, Carreau); C (Roi, Trefle); C (Dame, Pique); C (As, Carreau);
 C (Dix, Trefle); C (Huit, Coeur); C (As, Coeur); C (Dame, Coeur);
 C (Huit, Trefle); C (Sept, Pique); C (Sept, Coeur); C (Valet, Pique);
 C (Dix, Carreau); C (Sept, Carreau); C (Roi, Coeur); C (Dame, Trefle);
 C (Dame, Carreau); C (Huit, Carreau); C (Neuf, Coeur); C (Roi, Pique)]
   -------- *)


(*=== 3.On programme une reussite ===*)
let similaire c1 c2 =
if ((haut c1) = (haut c2)) || ((coul c1) = (coul c2)) then true else false;;

(* --------
TESTS :
similaire (C(As, Pique)) (C(Neuf, Trefle));; false 
similaire (C(As, Pique)) (C(Neuf, Pique));; true
   -------- *)


let rec reduc lp = match lp with 
  | ((c1::r1)::(c2::r2)::(c3::r3)::r) -> if (similaire c1 c3) then (((c2::r2)@(c1::r1))::(c3::r3)::r) else ((c1::r1)::(reduc ((c2::r2)::(c3::r3)::r)))
  | p -> p;;

(* --------
TESTS :
let p1 = [(C(As, Pique)); (C(Neuf, Trefle))];;
let p2= [(C(Valet, Coeur)); (C(Dix, Trefle))];;
let p3 = [(C(Dame, Pique)); (C(Neuf, Trefle))];;
let p4 = [(C(Sept, Pique)); (C(Sept, Carreau))];;
reduc [p1;p2;p3;p4]
   -------- *)

let rec reussite lp = let reduit = (reduc lp) in 
if (reduit  = lp) then lp else reussite reduit;;

(* genere des paquets d'une carte à partir d'un liste de carte *)
let rec gen_paquet  = function
  | [] -> [[]]
  | a::reste -> [a]::(gen_paquet reste);; 

(* --------
TESTS :
reussite (gen_paquet(faitjeu 32));;
[[C (Roi, Pique); C (Valet, Trefle)];
 [C (Valet, Carreau); C (Valet, Pique); C (Neuf, Trefle); C (Dix, Carreau);
  C (Valet, Coeur); C (Huit, Carreau); C (As, Carreau)];
 [C (Dame, Trefle); C (As, Pique); C (Roi, Trefle); C (Dix, Trefle)];
 [C (Huit, Pique); C (Dix, Pique); C (Sept, Pique)];
 [C (Dix, Coeur); C (Sept, Trefle); C (Neuf, Carreau); C (Dame, Coeur)];
 [C (Dame, Carreau); C (Neuf, Coeur); C (Roi, Carreau); C (As, Trefle);
  C (Sept, Carreau); C (Roi, Coeur); C (Neuf, Pique); C (Dame, Pique)];
 [C (Huit, Trefle); C (Sept, Coeur); C (Huit, Coeur)]; [C (As, Coeur)]; 
 []]
   -------- *)
let rec draw_pile = function
  | [] -> (moveto (current_x()+30) 5)
  | a::reste -> (draw_carte a); draw_pile reste;;


(* --------
TESTS :
draw_pile (faitjeu 32);;
   -------- *)

let rec draw_jeu llc =
clear_graph ();
moveto 5 5;
draw_jeu2 llc;;

let rec draw_jeu2 = function
  | [] -> ()
  | a::reste -> draw_pile a; draw_jeu2 reste;;


(* --------
TESTS :
draw_jeu (gen_paquet (faitjeu 32));;
   -------- *)

(*let rec draw_reussite2 lpc =
    if (key_pressed ())
    then (draw_jeu (reduc lpc); draw_reussite2 (reduc lpc)) else ();;

let draw_reussite () =
  let lpc = (gen_paquet (faitjeu 32)) in
    draw_jeu lpc;
   while (not (read_key () = 'q'))
      do
	draw_reussite2 lpc
    done
;;*)

let rec draw_reussite2 lpc =
  draw_jeu lpc;
  match read_key () with
    | 'q' -> close_graph ();
    | _ -> draw_reussite2 (reduc lpc);;

let draw_reussite () =
  draw_reussite2 (gen_paquet (faitjeu 32));;



let graph () =
 open_graph " 960x480";;
