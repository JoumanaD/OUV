let rec insere (elem : int) (liste : int list) : int list  = 
    match liste with
    |  [] -> elem::[]
    |  tete::queue ->
        if  elem <= tete then elem :: liste
        else tete :: insere elem queue
;;

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

(*

let swap arr i j =
  let temp = arr.(i) in
  arr.(i) <- arr.(j);
  arr.(j) <- temp

let fisher_yates_shuffle arr =
  let l = Array.length arr in
  for i = (l-1) downto 1 do
    let r = Random.int (i+1) in
    swap arr i r;
  done;
*)


let rec create_liste n = 
    if n!=0 then (insere n (create_liste (n-1))) else []
;;  

create_liste 5;;

let gen_permutation n =  

let rec fisher_yates l = 
    match l with 
;;  