(* question 1 *)
let rec assoc cle l = match l with
| [] -> raise Not_found
| (c,elt)::t when c = cle -> elt
| (c,elt)::t -> assoc cle t;;

(* question 2 *)
type automate = {
  taille : int ;
initial : int;
transitions : (char * int) list array;
final : bool array
};;
(* un automate accepte un mot si on n'est pas bloqué et que l'on finie sur un état acceptant.*)
exception Blocage;;
let calcul_det mot aut =
  let act = ref aut.initial in
  let rec aux m = match m with
  | [] -> aut.final.(!act) 
  | ch::t -> try
      act := assoc ch (aut.transitions.(!act));
      aux t;
    with Not_found -> raise Blocage in
  aux (List.init (String.length mot) (String.get mot));;

  let a1 = { 
    taille = 5 ; 
    initial = 0 ;
    transitions = [| 
      [('a',1);('b',2)] ;
      [('a',1)] ;
      [('a',3);('b',4)] ;
      [('b',4)] ;
      [('a',3)] |] ; 
    final = [|false; true; false; true; false|]} ;;
  
calcul_det "bab" a1;; (* true, Blocage,*)

let parcours_profondeur aut =
  let vus = Array.make (aut.taille) false in
  let rec aux s = 
    if vus.(s) = false then begin 
    print_string "\n visite ";print_int s;
    vus.(s) <- true;
    let voisins = aut.transitions.(s) in
    let rec cast_voisins l = match l with
    | [] -> ();
    | (a,b)::t -> aux b; cast_voisins t in
    cast_voisins voisins 
    end in

  aux aut.initial;
  for i = 0 to aut.taille-1 do
    if vus.(i) = false then aux i;
  done;;
parcours_profondeur a1;;

let accessible aut =
  (*
    Un sommet est dit accessible si il existe un chemin du sommet initial à ce sommet
  *)
  let vus = Array.make (aut.taille) false in
  let indice_act = ref 0 in
  let tab_conversion = Array.make (aut.taille) (-1) in
  let tab_inversion = Array.make (aut.taille) (-1) in
  let rec aux s = 
    if vus.(s) = false then begin 
    vus.(s) <- true;
    tab_inversion.(!indice_act) <- s;
    tab_conversion.(s) <- !indice_act;
    indice_act := !indice_act +1;
    let voisins = aut.transitions.(s) in
    let rec cast_voisins l = match l with
    | [] -> ();
    | (a,b)::t -> aux b; cast_voisins t in
    cast_voisins voisins 
    end in
  aux aut.initial;
  (* Désormais vus contient tous les sommets accessibles depuis le sommet initial. *)
  let taille = !indice_act in
  let initial = tab_conversion.(aut.initial) in
  let final = Array.make taille false in
  for i = 0 to aut.taille -1 do
    if aut.final.(i) && tab_conversion.(i) <> -1 then 
      final.(tab_conversion.(i)) <- true;
  done;
  let transitions = Array.make taille [] in
  for i = 0 to aut.taille-1 do
    if tab_inversion.(i) <> -1 then begin
      let new_i = tab_conversion.(i) in
      let past_trans = aut.transitions.(i) in
      let rec parc l = match l with
      | [] -> ()
      | (c,e)::t -> transitions.(new_i)<- (c,tab_conversion.(e))::transitions.(new_i); parc t;
    in parc past_trans;
    end
  done;
  {taille = taille; initial = initial; transitions = transitions; final = final};;


(* Partie 2 : Le plus petit mot *)
let est_vide_auto aut =
  let newaut = accessible aut in
  let vide = ref true in
  for i = 0 to aut.taille -1 do 
    if newaut.final.(i) then vide:= false;
  done;
  !vide;;
exception Auto_vide;;
let longueur_mot_min_auto auto =
  if est_vide_auto auto then raise Auto_vide
  else begin
    let file = Queue.create () in
    let added = Array.make (auto.taille) false in
    Queue.add (auto.initial,0) file;
    let continue = ref true in
    let res = ref (-1) in
    while not (Queue.is_empty file) && !continue do
      let s,p = Queue.take file in
      if not added.(s) then begin
        added.(s) <- true;
        if auto.final.(s) then 
          begin
            res:= p;
            continue := false;
          end
        else begin
          let voisins = auto.transitions.(s) in
          let rec aux l = match l with | [] -> ()
          | (a,b)::t -> Queue.add (b,p+1) file; aux t;
          in
          aux voisins; 
          end;
      end;
    done;
    if !res = -1 then raise Not_found
    else !res;
  end;;

longueur_mot_min_auto a1;;

let langage_auto auto =
  let lmin = longueur_mot_min_auto auto in
  if est_vide_auto auto then raise Auto_vide
  else begin
    let file = Queue.create () in
    let added = Array.make (auto.taille) false in
    Queue.add (auto.initial,"",0) file;
    let continue = ref true in
    let mots = ref [] in 
    while not (Queue.is_empty file) && !continue do
      let s,mot,p = Queue.take file in
      if not added.(s) then begin
        added.(s) <- true;
        if p=lmin then 
          begin
            if auto.final.(s) then
              begin
              (* On a fini, on ajoute le mot et on drop la branche *)
              mots := mot::(!mots);
              end
          end
        else begin
          let voisins = auto.transitions.(s) in
          let rec aux l = match l with | [] -> ()
          | (a,b)::t -> Queue.add (b,mot^(String.make 1 a),p+1) file; aux t;
          in
          aux voisins; 
          end;
      end;
    done;
    !mots;
  end;;

(* Expressions rationnelles *)

type expr = 
Vide 
| Epsilon 
| Lettre of char 
| Union of expr * expr 
| Concat of expr * expr 
| Etoile of expr;;


(* q.7 & 8 *)
let rec est_vide exp = match exp with
| Vide -> true
| Epsilon -> false
| Lettre(a) -> false
| Union(a,b) -> est_vide a && est_vide b
| Concat(a,b) -> est_vide a || est_vide b (* On ne peut pas concaténer le vide avec quelque chose, ce n'est pas epsilon mais bien le vide.*)
| Etoile(a) -> false;; 

let rec mot_min exp = match exp with
| Vide -> None
| Epsilon -> Some 0
| Lettre(a) -> Some 1
| Union(a,b) -> let ma = mot_min a in let mb = mot_min b in (match ma,mb with | None, _ -> mb | _,None -> ma | Some(mav), Some(mbv) -> Some (min mav mbv));
| Concat(a,b) -> let ma, mb = mot_min a, mot_min b in (match ma,mb with | None,_ -> None | _, None -> None | Some(mav),Some(mbv) -> Some (mav+mbv) );
| Etoile(a) -> Some 0;;

(* Automates non-déterministes *)

