(** Strucutre de données pour manipuler des polynomes (coef, puissance de x) *)
type monome = int * int;;
type polynome = monome list;;

(** tri par insertion O(N2) complexité *)
let rec insere (elem : monome) (liste : polynome) : polynome  = 
    match liste with
    |  [] -> elem::[]
    |  tete::queue ->
        if snd elem < snd tete then elem :: liste
        else if snd elem = snd tete then (fst tete + fst elem, snd elem):: queue
        else tete :: insere elem queue
;;

let rec canonique (l : polynome) : polynome = 
    match l with 
    | [] -> []
    | h::t -> if (fst h == 0) then canonique t else 
        insere h (canonique t)
;;

(** des tests pour tester la fonction canonique *)
let listetest : polynome = [(120,3); (-2,2); (10,14); (-5,2); (0,2)];;
canonique(listetest);;

(** val poly_add : polynome -> polynome -> polynome *)
let rec poly_add (l1 : polynome) (l2 : polynome) : polynome  = 
    match l1, l2 with
    | [], _ -> l2
    | _,[] -> l1
    | h1::q1, h2::q2 ->
        if snd h1 > snd h2 then canonique (h1 :: poly_add q1 l2)
        else canonique (h2 :: poly_add l1 q2)
;;

(** des tests pour tester la fonction poly_add *)
let ltest1 : polynome = [(3,1); (5,2); (3,3)];;
let ltest2 : polynome = [(1,1); (2,2); (2,3); (10,4)];;
(** polynome = [(4, 1); (7, 2); (5, 3); (10, 4)] *)
poly_add ltest1 ltest2;;

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
(** des tests pour tester la fonction poly_prod *)
let lprod1 : polynome = [(3,1); (5,2); (3,3)];;
let lprod2 : polynome = [(4,0);(1,1); (2,2); (2,3); (10,4)];;
(** polynome = [(3, 1); (10, 2); (6, 3); (10, 4)] *)
poly_prod lprod1 lprod2;;

let lprod3 : polynome = [(1,1); (2,2)];;
let lprod4 : polynome = [(5,0);(3,1); (-4,2)];;
poly_prod lprod3 lprod4;;
poly_prod [(123,0)] [(1, 1)];;
poly_prod [] [(2, 10)];;
poly_prod lprod1 [];;
poly_prod [(1, 13)] [];;
poly_prod [] [];;
