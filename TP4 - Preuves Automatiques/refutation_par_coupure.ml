exception NOT_POSSIBLE;;
type clause = int list;;
let abs a = if a < -a then -a else a;;
let rec simplify l x b= match l with
| [] when b -> [x]
| [] -> []
| e::t when (e+x = 0) -> simplify t x false
| e::t when (e = x) -> simplify t x b
| e::t -> e::(simplify t x b);;
let coupure c1 c2 n = 
	let rec aux res l1 l2 = match l1,l2 with
	| [],[] -> res
	| e::t,[] when e<>n -> aux (simplify res e true) t l2
	| e::t, [] -> aux res t l2
	| [],e::t when e<>n -> aux (simplify res e true) l1 t
	| [],e::t -> aux res l1 t
	| e1::t1,e2::t2 when (e1<e2 && e1<>n && (e1+n) <> 0) -> aux (simplify res e1 true) t1 l2
	| e1::t1,e2::t2 when  (e1<e2) -> aux res t1 l2
	| e1::t1,e2::t2 when (e1>=e2 && e2<>n && (e2+n)<>0) -> aux (simplify res e2 true) l1 t2
	| e1::t1,e2::t2 -> aux res l1 t2 in
	aux [] c1 c2;;

let rec variables_a_couper c1 c2 =
	match c1 with
	| [] -> []
	| e::t when (List.mem (-e) c2) -> e::(variables_a_couper t c2)
	| e::t -> variables_a_couper t c2;;


let nouvelles_clauses c1 c2 = let vs = variables_a_couper c1 c2 in
	let rec aux l = match l with
	| [] -> []
	| e::t -> (coupure c1 c2 e)::(aux t) in
	aux vs;;

let rec exists_clause_vide l = match l with
| [] -> false
|e::t when e=[] -> true
| e::t -> exists_clause_vide t;;

let rec print_l l = match l with | [] -> print_string "\n"; | e::t -> print_int e; print_string " "; print_l t;;
let rec lrm l c = match l with | [] -> [] | e::t when e = c -> lrm t c | e::t -> e::(lrm t c);;
let derive_clause_vide clist =
	let ct = ref clist in
	let cp = ref [] in
	let i = ref 0 in
	while (!ct <> []) do
		i:=(!i)+1;
		let c = List.hd (!ct) in
		ct:=List.tl (!ct);
		let cp_copie = ref (!cp) in
		while (!cp_copie) <> [] do
			let c' = (List.hd !cp_copie) in
			let all_cuts = ref (nouvelles_clauses c c') in
			while (!all_cuts <> []) do
				let act = List.hd (!all_cuts) in
				all_cuts := List.tl (!all_cuts);
				if (not (List.mem act (!cp) || List.mem act (!ct)) || act = c) then ct := act::(!ct);
			done;
			cp_copie := List.tl (!cp_copie);
		done;
		cp := c::(!cp);
		
		if (3< !i && !i < 7) then (print_string "\n Starting : ";print_int (!i);print_newline ();List.iter print_l !ct;print_string "\n cp :\n";List.iter print_l !cp;)
	done;
	
	List.mem [] !cp;;


(*
Chaussette : 1
Kilt : 2
Marie : 3
Ecossais : 4
Sortir Dimanche : 5	 
*)
let club_ecossais = [
	[1;4];
	[-1;2];
	[-5;-3];
	[-5;4];
	[-4;5];
	[-2;4];
	[-2;3];
	[-4;2]
];;
derive_clause_vide club_ecossais;;