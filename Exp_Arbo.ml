(** Strucutre de données pour manipuler des polynomes (coef, puissance de x) *)
type monome = int * int;;
type polynome = monome list;;

(** tri par insertion O(N2) complexité *)
let rec insere (elem : monome) (liste : polynome) : polynome  = 
    match liste with
    |  [] -> elem::[]
    |  tete::queue ->
        if snd elem > snd tete then elem :: liste
        else if snd elem = snd tete then (fst tete + fst elem, snd elem):: queue
        else tete :: insere elem queue
;;

let rec canonique (l : polynome) : polynome = 
    match l with 
    | [] -> []
    | h::t -> if (fst h == 0) then canonique t else 
        insere h (canonique t)
;;

(** val poly_add : polynome -> polynome -> polynome *)
let rec poly_add (l1 : polynome) (l2 : polynome) : polynome  = 
    match l1, l2 with
    | [], _ -> l2
    | _,[] -> l1
    | h1::q1, h2::q2 ->
        if snd h1 > snd h2 then (h1 :: poly_add q1 l2) else 
        if snd h1 = snd h2 then (fst h1 + fst h2, snd h1):: poly_add q1 q2
        else (h2 :: poly_add l1 q2)
;;


(** val poly_prod : polynome -> polynome -> polynome *)
let rec poly_prod (l1 : polynome) (l2 : polynome) : polynome  =
    let rec produit (e : monome) (lp : polynome) : polynome = 
        match lp with
        | [] -> []
        | h1::q1 -> (fst h1 * fst e, snd h1 + snd e)::produit e q1
    in match l1, l2 with
        | l1, [] -> l1 
        | h1::[], l2 -> produit h1 l2
        | [], l2 -> l2
        | h1::q1, h2::q2 -> poly_add (produit h1 l2) (poly_prod q1 l2) 
;;


(** 
    E = int | E^ | E+ | E*
    E^ = x^int+
    E+ = (E\E+) + (E\E+) + ...
    E* = (E\E*') * (E\E*') * ...
*)

type arbre = 
    | NodeInt of int
    | NodePower of int 
    | NodePlus of arbreP list
    | NodeMulti of arbreM list
and arbreP = 
    | NodeIntP of int (* int est positif ou negatif *)
    | NodePowerP of int (**x^int+, int est positif *)
    | NodeMultiP of arbreM list
    (* jamais deux * successif parce que le noeud d'addition contient l'arbreM où ne contient pas le noeud addition *)
and arbreM = 
    | NodeIntM of int (* int est positif ou negatif *)
    | NodePowerM of int (**x^int+, int est positif *)
    | NodePlusM of arbreP list
    (* jamais deux + successif parce que le noeud de multiplication contient l'arbreP où ne contient pas le noeud multiplication *)
;;


(* 
             +
         /   |   \
        *   42    ^
       / \       / \
    123   ^     x   3
         / \
        x   1
*)
(** Implementation arbre représentant '123 * x + 42 + x^3 *)
let contruireArbre = NodePlus([
                NodeMultiP([ NodeIntM 123; NodePowerM 1 ]);
                NodeIntP 42;
                NodePowerP 3
                ])

    ;;


let rec arbM2poly (a: arbreM list) : polynome = 
    match a with 
    | [] -> []
    | NodeIntM x :: t-> poly_prod [(x,0)] (arbM2poly t)
    | NodePowerM x :: t-> poly_prod [(1, x)] (arbM2poly t)
    | NodePlusM l :: t -> (arbP2poly l) @ (arbM2poly t)
and arbP2poly  (a: arbreP list) : polynome =
     match a with 
    | [] -> []
    | NodeIntP x :: t -> poly_add [(x,0)] (arbP2poly t)
    | NodePowerP x :: t -> poly_add [(1, x)] (arbP2poly t)
    | NodeMultiP l :: t -> (arbM2poly l) @ (arbP2poly t)
;;

let arb2poly (a: arbre) : polynome = 
    match a with 
    | NodeInt x -> [(x,0)]
    | NodePower x -> [(1, x)]
    | NodeMulti [] -> [] 
    | NodePlus [] -> [] 
    | NodeMulti l -> canonique (arbM2poly l) 
    | NodePlus l -> canonique (arbP2poly l) 
;;
arb2poly contruireArbre ;;


let contruireArbre2 = NodePlus([
                NodeMultiP([ NodePlusM([NodeIntP 3 ; NodeIntP 3; NodeIntP (-1)]);  NodePowerM 15 ]);
                NodeIntP 20;
                NodeMultiP([ NodeIntM 20 ; NodePowerM 4])
                ])

    ;;

arb2poly contruireArbre2;;

