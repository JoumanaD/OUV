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
    | h::[] -> h::l
    | h::m::t -> if (fst h == 0) then canonique t else 
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
        if snd h1 > snd h2 then (h1 :: poly_add q1 l2) else 
        if snd h1 = snd h2 then (fst h1 + fst h2, snd h1):: poly_add q1 q2
        else (h2 :: poly_add l1 q2)
;;

(** des tests pour tester la fonction poly_add *)
let ltest1 : polynome = [(3,3); (5,2); (3,1)];;
let ltest2 : polynome = [(10,4); (2,3); (2,2); (1,1)];;
(** polynome = [(10, 4); (5, 3); (7, 2); (4, 1)] *)
poly_add ltest1 ltest2;;
poly_add [] ltest2;;
poly_add ltest1 [];;


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
poly_prod ltest1 ltest2;;

let ltest3 : polynome = [(1,1); (2,2)];;
let ltest4 : polynome = [(5,0);(3,1); (-4,2)];;
poly_prod ltest3  ltest4;;
poly_prod [(123,0)] [(1, 1)];;
poly_prod [] [(2, 10)];;
poly_prod ltest1 [];;
poly_prod [(1, 13)] [];;
poly_prod [] [];;

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

(*1.3*)
let rec remove (x: int) (i: int) (l: int list) : int list  * int  =
  match l with
  | [] -> failwith "liste vide"
  | h :: t -> if x=i then (t,h) else h::(fst (remove x (i+1) t)), snd (remove x (i+1) t)

let c = remove 2 0 [0;1;2;3;4];;

let extraction_alea (m: int list) (p: int list) : int list * int list = 
    let l = List.length m in let _ = Random.self_init () and r = Random.int l in 
    match m with
    | [] -> ([],p)
    | h::t -> let c = (remove r 0 m) in 
                fst c, snd c :: p
;;

let x = (extraction_alea [0;1;2;3;4] [5;6;7;8;9]);;

let rec insere_liste (elem : int) (liste : int list) : int list  = 
    match liste with
    |  [] -> elem::[]
    |  tete::queue ->
        if  elem <= tete then elem :: liste
        else tete :: insere_liste elem queue
;;

let rec create_liste n = 
    if n!=0 then (insere_liste n (create_liste (n-1))) else []
;;  

create_liste 5;;

let gen_permutation n = 
    let rec permutation (m: int list) (p: int list) : int list = 
        match m with 
        | [] -> p
        | h::t -> let (a, b) = (extraction_alea m p) in permutation a b
    in 
    permutation (create_liste n) [];;

gen_permutation 5;;


(* Définition du type pour construire un ABR *)
type abr = 
  | Feuille
  | Noeud of int *  abr *  abr
;;

let rec insert l a = 
  match a with
    | Feuille -> Noeud(l,Feuille,Feuille)
    | Noeud(k, fg, fd) -> if l < k  then 
                        Noeud(k, (insert l fg), fd ) 
                      else Noeud(k,fg, (insert l fd))
;;

let rec construireARB l a = 
  match l with
    | [] -> a
    | h::t -> construireARB t (insert h a)
;;	

let abr1 = construireARB [4;2;3;8;1;9;6;7;5] Feuille;;

(**
  * = 42 
  + = 43
  ^ = 94 
  x = 120
*)
let rec etiquetage a = 
  match a with 
  | Feuille -> if Random.bool() then Noeud(int_of_char 'x', Feuille, Feuille) else Noeud(Random.int (201+201)-201, Feuille, Feuille)
  | Noeud(l, fg, fd) -> match fd, fg with 
                          | Feuille, Feuille -> if (l mod 2 == 1) then let _ = Random.self_init () and r = Random.int (201+201)-201 in 
                                                    Noeud(int_of_char '*', Noeud(r, Feuille, Feuille), Noeud(int_of_char 'x', Feuille, Feuille))
                                                else let _ = Random.self_init () and r = Random.int 100 in 
                                                    Noeud(int_of_char '^', Noeud(int_of_char 'x', Feuille, Feuille), Noeud(r, Feuille, Feuille)) 
                          | _, _ -> if (Random.bool() && Random.bool()) then Noeud(int_of_char '*', etiquetage fg, etiquetage fd)
                                    else Noeud(int_of_char '+', etiquetage fg, etiquetage fd)
                                    (*Random.bool donne un true avec probabilité 0.5 alors on veut que la probabilité soit 0.25 qlors 0.5*0.5=0.25
                                      en bool 0.5&&0.5=0.25 alors Random.bool() && Random.bool() = 0.25 de probabilité*)      
;;

let a1 = etiquetage (construireARB [4;2;3;8;1;9;6;7;5] Feuille);;
let a2 = etiquetage (construireARB [4;2;3;8;1;9;6;7;4] Feuille);;

let rec abr2poly (a: abr) : polynome = 
    match a with 
    | Feuille -> []   
    | Noeud(l, Noeud(g, Feuille, Feuille), Noeud(d, Feuille, Feuille)) ->
        if l = 42 then [(g,1)] else if l = 94 then [(1,d)] else []
    | Noeud(l, fg,fd) -> if l = 42 then poly_prod (canonique (abr2poly fg)) (canonique (abr2poly fd)) else 
                        if l = 43 then poly_add (canonique (abr2poly fg)) (canonique (abr2poly fd)) else []
;;

abr2poly (a1);;
canonique (abr2poly (a2));;


(** Partie Experimentations *)

let rec long (a: abr) : int = 
    match a with
    | Feuille -> 0
    | Noeud(_, g, d) -> 1 + max (long g) (long d);;

long abr1;;

(**2.13*)
let ln = [100; 200; 300; 400; 500; 600; 700; 800; 900; 1000];;
let abr200 = construireARB [1; 2; 3; 4; 5;6;7;8;9;10;11;12;13;14;15;16;17;18;19;20] Feuille;;
let abr201 = construireARB [4;23;60;17;59;40;39;58;46;2;57;22;61;30;45;56;38;16;62;3;69;70;71;72;55;29;44;63;54;68;37;24;8;13;73;74;75;
76;64;43;10;28;1;65;36;66;21;50;47;9;77;78;79;80;81;18;67;49;6;82;11;26;41;83;31;51;42;52;19;20;48;53;7;14;35;32;25;21;12;33;15;34;5] Feuille;;
long abr201;;

let time f =
  let t = Sys.time () in
  let res = f () in
  Printf.printf ("Temps d'éxécution: %f secondes \n")
                (Sys.time () -. t);
  res
;;

let n = 100;;
let g =  gen_permutation 40;;
let cons = construireARB g Feuille;;
time (fun () -> (g));;
time (fun () -> (cons));;