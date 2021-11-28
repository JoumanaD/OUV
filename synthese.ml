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

construireARB [4;2;3;8;1;9;6;7;5] Feuille;;

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

etiquetage (construireARB [4;2;3;8;1;9;6;7;5] Feuille);;

