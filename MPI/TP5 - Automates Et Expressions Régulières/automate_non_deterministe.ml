type automate = {
  taille : int ;
initial : int list;
transitions : (char * int) list array;
final : bool array
};;
(* question 11 *)
let rec assocn cle l = match l with
| [] -> []
| (c,elt)::t when c = cle -> elt::(assocn cle t)
| (c,elt)::t -> assocn cle t ;;

let delta aut q c = assocn c aut.transitions.(q);;

let rec liste_etats_contient_acceptants aut l = match l with
| [] -> false
| e::t -> aut.final.(e) || liste_etats_contient_acceptants aut t;;

let rec rech_prof aut mot l = match mot with
| [] -> liste_etats_contient_acceptants aut l
| c::t -> (* on lit c et ensuite on appelle la recherche avec la liste générée *)
  let rec aux et = match et with
  | [] -> false
  | e2::t2 -> (rech_prof aut t (delta aut e2 c)) || aux t2 in
  aux l;;


let an = { taille = 5 ; 
  initial = [0] ;
  transitions = [| 
    [('a',1) ; ('a',2) ; ('b',2)] ; 
    [('a',1)] ; 
    [( 'a',3) ; ('b',4)] ; 
    [('b',4)] ; 
    [('a',3)] |] ; 
  final = [|false; true; false; true; false|]} ;;

let reconnait_prof aut mot = rech_prof aut (List.init (String.length mot) (String.get mot)) aut.initial;;

reconnait_prof an "aa";; (* true *)
reconnait_prof an "aba";; (* true *)
reconnait_prof an "bab";; (* false *)

(* 3.2 Recherche en largeur *)

let rec contains l x = match l with | [] -> false | e::t when e =x -> true | e::t -> contains t x;;
let etape aut x le = 
  let rec aux l = match l with | [] -> []
  | e::t -> (delta aut e x)@(aux t) 
  in
  aux le;;

let reconnait_larg aut s =
  (* on utilise la question 14 *)
  let ei = ref aut.initial in
  let n = String.length s in
  let elt_list = List.init n (String.get s) in
  let rec aux l = match l with | [] ->  () | e::t -> ei := etape aut e !ei in
  aux elt_list;
  liste_etats_contient_acceptants aut !ei;;
reconnait_larg an "aa";; (* true *)
reconnait_larg an "aba";; (* true *)
reconnait_larg an "bab";; (* false *)

