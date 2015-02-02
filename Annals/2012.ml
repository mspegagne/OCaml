Exo 1

1)
val cmp : ('a->'b)->('c->'a)->'c->'b=<fun>
val f : ('a->'b)*'c->'a->'b=<fun>

2)
let rec f g = function
|[]->[]
|(a::r)->(g a)::(f g r);;

let g = let (l1, l2) = ([],[]) in function
|[]->(l1,l2)
|(a::r)->match a with 
			|(a,b)->(a::l1, b::l2)
			|_ -> (l1, l2);;

let h g b = function
|[] -> b
|(a::r)->(g a b)+(h g b r);;

let vide = ([]: 'a ensemble);;

let rec appartient a (b : 'a ensemble) = match b with 
|[]->false
|(b::r)->if (a=b) then true else (if b>a then false) else (appartient a r);;

let rec cardinal (b: 'a ensemble) = match b with 
|[]-> 0
|(a::r)-> 1+(cardinal r);;

let rec ajout a (ens: 'a ensemble) = match ens with
|[]->([a]: 'a ensemble)
|(tete::reste)-> if tete>a then (a::tete::reste) else tete::(ajout a reste);;

let rec union (ensa: 'a ensemble) (ensb: 'a ensemble) = match ensa with
|[]->([]: 'a ensemble)
|(tete::reste)->union reste (ajout tete ensb);;

let rec difference (ensa: 'a ensemble) (ensb: 'a ensemble) = match ensa with
|[]->([]: 'a ensemble)
|(tete::reste)-> if (appartient tete ensb) then (difference reste ensb) else tete::(difference reste ensb);;

type 'a arbre = Vide | Noeud of 'a arbre * 'a * 'a arbre;;

let rec appartient a = function
|Vide->false
|Noeud(g,i,d)-> if i=a then true else (appartient a g)||(appartient a d);;

let rec insertion = fun a arbre -> if (appartient a arbre) then arbre else match arbre with
|Vide -> Noeud(Vide, a, Vide)
|Noeud(g, i, d) -> if (a>i) then Noeud(g, i, (insertion a d)) else Noeud((insertion a g), i, d);;

let rec suppression a arbre = if (appartient a arbre) then match arbre with 
|Vide->Vide
|Noeud(g,i,Vide)->if i=a then g else (suppression a g)
|Noeud(Vide,i,d)->if i=a then d else (suppression a d)
|Noeud(Vide,i,Vide) -> if i=a then Vide
|Noeud(g,i,d)->if i=a then match d with |(gn,in,dn)->Noeud(g,in,(suppresion in d));;
else arbre;
