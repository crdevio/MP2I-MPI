type 'a prop = 
| Top
| Bot
| V of 'a
| Not of 'a prop
| And of 'a prop * 'a prop
| Or of 'a prop * 'a prop
| Impl of 'a prop * 'a prop
;;
type 'a sequent = {
  gamma :'a prop list;
  delta : 'a prop list;
  gamma_var : 'a prop list;
  delta_var : 'a prop list
};;
exception Wrong_rule of string;;
let create_sequent gamma delta =
  { gamma = gamma ; delta = delta ; gamma_var = [] ; delta_var = []};;
let rec member x l = match l with | [] -> false | e::t when e=x -> true | e::t -> member x t;;

let bot seq =
  member Bot seq.gamma || member Bot seq.gamma_var;;

let top seq =
  member Top seq.delta || member Top seq.delta_var;;

let axiom seq =
  let rec aux l1 l2 = match l1 with
  | [] -> false
  | e::t when member e l2 -> true
  | e::t -> aux t l2 in
  aux (seq.gamma @ seq.gamma_var) (seq.delta @ seq.delta_var);;

let and_gamma seq =
  match seq.gamma with
  | And (a,b)::t -> {gamma= a::(b::t); delta_var = seq.delta_var; gamma_var = seq.gamma_var; delta = seq.delta}
  | _ -> raise (Wrong_rule "And Gamma");;

let or_gamma seq =
  match seq.gamma with
  | Or (a,b)::t ->
    ({gamma = a::t; delta_var = seq.delta_var; gamma_var = seq.gamma_var; delta = seq.delta},
     {gamma = b::t; delta_var = seq.delta_var; gamma_var = seq.gamma_var; delta = seq.delta})
  | _ -> raise (Wrong_rule "Or Gamma");;

let impl_gamma seq =
  match seq.gamma with
  | Impl (a,b)::t ->
    ({gamma = t; delta_var = seq.delta_var; gamma_var = seq.gamma_var; delta = a::seq.delta},
    {gamma = b::t; delta_var = seq.delta_var; gamma_var = seq.gamma_var; delta = seq.delta})
  | _ -> raise (Wrong_rule "Impl Gamma");;

let not_gamma seq =
    match seq.gamma with
    | Not (a)::t ->
      {gamma = t; delta_var = seq.delta_var; gamma_var = seq.gamma_var; delta = a::seq.delta}
    | _ -> raise (Wrong_rule "Not Gamma");;

let and_delta seq =
  match seq.delta with
  | And (a,b)::t -> (
    {delta= a::t; delta_var = seq.delta_var; gamma_var = seq.gamma_var; gamma = seq.gamma},
    {delta= b::t; delta_var = seq.delta_var; gamma_var = seq.gamma_var; gamma = seq.gamma}
    )
  | _ -> raise (Wrong_rule "And Delta");;

let or_delta seq =
  match seq.delta with
  | Or (a,b)::t ->
    {delta= a::(b::t); delta_var = seq.delta_var; gamma_var = seq.gamma_var; gamma = seq.gamma}
  | _ -> raise (Wrong_rule "Or Delta");;
let impl_delta seq =
  match seq.delta with
  | Impl (a,b)::t ->
    {delta= b::t; delta_var = seq.delta_var; gamma_var = seq.gamma_var; gamma = a::seq.gamma}
  | _ -> raise (Wrong_rule "Impl Delta");;

let not_delta seq =
  match seq.delta with
  | Not (a)::t ->
    {delta= t; delta_var = seq.delta_var; gamma_var = seq.gamma_var; gamma = a::seq.gamma}
  | _ -> raise (Wrong_rule "Not Delta");;

let rec proof_search seq =
  if (bot seq || top seq || axiom seq) then true
  else match seq.gamma with
  | Top::t -> proof_search {gamma = t; gamma_var = Top::seq.gamma_var; delta = seq.delta; delta_var = seq.delta_var;}
  | Bot::t -> proof_search {gamma = t; gamma_var = Bot::seq.gamma_var; delta = seq.delta; delta_var = seq.delta_var;}
  | V(a)::t -> proof_search {gamma = t; gamma_var = V(a)::seq.gamma_var; delta = seq.delta; delta_var = seq.delta_var;}
  | And(a,b)::t -> proof_search (and_gamma seq)
  | Or(a,b)::t -> let r1,r2 = or_gamma seq in (proof_search r1) && (proof_search r2)
  | Impl(a,b)::t -> let r1,r2 = impl_gamma seq in (proof_search r1) && (proof_search r2)
  | Not(a)::t -> proof_search (not_gamma seq)
  | [] ->(
    match seq.delta with
    | Top::t -> proof_search {gamma = seq.gamma; gamma_var = seq.gamma_var; delta = t; delta_var = Top::seq.delta_var;}
    | Bot::t -> proof_search {gamma = seq.gamma; gamma_var = seq.gamma_var; delta = t; delta_var = Bot::seq.delta_var;}
    | V(a)::t -> proof_search {gamma = seq.gamma; gamma_var = seq.gamma_var; delta = t; delta_var = V(a)::seq.delta_var;}
    | And(a,b)::t -> let r1,r2 = and_delta seq in (proof_search r1 && proof_search r2)
    | Or(a,b)::t -> proof_search (or_delta seq)
    | Impl(a,b)::t -> proof_search (impl_delta seq)
    | Not(a)::t -> proof_search (not_delta seq)
    | [] -> false
  );;

proof_search (create_sequent [] [Or(V 1, V 1)]) ;;
proof_search (create_sequent [] [Impl(Impl(Impl(V 1, V 2),V 1),V 2)]) ;;

proof_search (create_sequent [] [Impl(Impl(Impl(V 1, V 2),V 1),V 1)]) ;;
