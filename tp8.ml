(********************)
(*  TP8 - Caml      *)
(********************)


(*=== 1.Compter les feuilles d'un arbre ===*)

type 'a arbin = 
    Feuille of 'a
  | Noeud of 'a arbin * 'a * 'a arbin;;
 
let arbretest = Noeud (Noeud (Feuille 5, 2, Feuille 3), 4, Noeud(Feuille 10, 6, Noeud(Feuille 1,9, Feuille 7)));;

let rec compter = function 
  | Feuille (_) -> 1
  | Noeud(fg,_,fd) -> (compter fg) + (compter fd);;


(* --------
TESTS :
compter arbretest;; 5
   -------- *)


(*=== 2. Arbre trié ===*)
let rec placer va arb =
match arb with  
  | Feuille ("Nil") -> Feuille va
  | Feuille (v) -> if va<v then Noeud(Feuille va,v,Feuille "Nil") else Noeud(Feuille "Nil",v,Feuille va)
  | Noeud(fg,v,fd) -> if va<v then Noeud (placer va fg, v,fd) else Noeud(fg,v,placer va fd);;
 
(* --------
TESTS :
placer "celeri" (Noeud(Noeud(Feuille "Nil","ble", Feuille "mais"),"soja",Noeud(Feuille "Nil","tomate", Feuille "Nil")));;
Noeud
 (Noeud (Feuille "Nil", "ble",
   Noeud (Feuille "celeri", "mais", Feuille "Nil")),
 "soja", Noeud (Feuille "Nil", "tomate", Feuille "Nil"))
   -------- *)


let rec constr = function
  | [] -> Feuille "Nil"
  | a::reste -> placer a (constr reste);;

(* --------
TESTS :
constr ["celeri";"orge";"mais";"ble";"tomate";"soja"];;
Noeud
 (Noeud (Feuille "Nil", "ble",
   Noeud (Feuille "celeri", "mais", Feuille "orge")),
 "soja", Feuille "tomate")
   -------- *)

(*=== 3. Mise en page de l'arbre de Huffman ===*)

type coord = int * int;; (* couple d'entiers *)
type huffp = Fp of coord * char | Np of coord * huffp * huffp;;
type huff = F of char | N of huff * huff;;
let d = 5;;
let e = 4;;


let rec createhuff x y = function
  | F(char) -> Fp((x,y),char)
  | N(h1,h2) -> Np((x+,y),(createhuff (x-e) (y+d) h1),(createhuff (x+e) (y+d) h2));;


let  placerhuff h = 
createhuff e d h;;

placerhuff (N (F 'j', N (N (N (F 'w', F 'k'), F 'z'), F 'y')));;

(* Nous n'avons pas trouvé la méthode pour obtenir les bonnes coordonnées des Noeuds de l'arbre *)
