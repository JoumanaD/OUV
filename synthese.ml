type arbreP = 
    | NodeInt of int (* int est positif ou negatif *)
    | NodePower of char * char * int (**la premiere char est '^', la deuxième est 'x' et int est positif *)
    | NodePlus of char * (arbreM list)
    (* jamais deux * successif parce que le noeud d'addition contient l'arbreM où ne contient pas le noeud addition *)
and arbreM = 
    | NodeInt of int (* int est positif ou negatif *)
    | NodePower of char * char * int (**la premiere char est '^', la deuxième est 'x' et int est positif *)
    | NodeMulti of char * (arbreP list) 
    (* jamais deux + successif parce que le noeud de multiplication contient l'arbreP où ne contient pas le noeud multiplicqtion *)
;;
 
let extraction_alea (l: int list) (p: int list) : int list * int list = 
    let long = List.length l in let r = Random.int long in 
    match l,p with
    | [], p -> ([],p)
    | l,

