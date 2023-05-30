type image = int array;;

type instance = image * int;;
type entrainement = instance array;;

let high = "■";;
let mid = "▧";;
let low = "□";;

let lire_lignes_fichiers ic =
  (*
  let rec aux i =
    try
      (input_line i)::(aux i)
    with End_of_file -> [] in
    *)
  let r = ref [] in
  let ok = ref true in
  while !ok do
    try
      r:= (input_line ic)::!r
    with End_of_file ->  ok:=false;
  done;
  List.rev !r;;

let parser_ligne_image s =
  let entiers_l = String.split_on_char ',' s in
  Array.of_list (List.map (fun c -> int_of_string c) entiers_l);;

let lire_images s : image array= 
  let f = open_in s in
  let images = lire_lignes_fichiers f in
  let image_list = List.map (fun s -> parser_ligne_image s) images in
  close_in f;
  Array.of_list image_list;;

let rec nb_lines f =
  try
    let _ = input_line f in
    1 + nb_lines f
  with End_of_file -> 0;;
let lire_etiquettes s =
  let f1 = open_in s in
  let n = nb_lines f1 in
  close_in f1;
  let f2 = open_in s in
  let res = Array.make n (-1) in
  for i=0 to (n-1) do
    res.(i) <- (int_of_string (input_line f2));
  done;
  close_in f2;
  res;;

let jeu_donnees im_arr v_arr =
  let n = Array.length im_arr in (* on suppose que c'est de bonne taille ...*)
  Array.init n (fun i -> (im_arr.(i),v_arr.(i)));;


let affiche_image im =
  print_string "\n NOUVELLE IMAGE \n";
  for i=0 to 783 do
    if (i mod 28)=0 then print_newline ();
    if im.(i)> 200 then print_string high
    else if im.(i) > 100 then print_string mid
    else print_string low;
  done;;




let test_affichage () =
  let ims = lire_images "x_test.csv" in
  let arr = Array.init 5 (fun i -> ims.(i)) in
  for i = 0 to 4 do

    affiche_image arr.(i);

  done;;

(*test_affichage();;*)


type modele = {
  distance : image -> image -> float ;
  k : int;
  donnees : (image * int) array;
};;

let norme_1 i1 i2 =
  let res = ref 0 in
  for k=0 to (Array.length i1) - 1 do
    res:= !res + abs (i1.(k) - i2.(k));
  done;
  (float)!res;;

let nb_elements li =
  let res = Array.make 10 0 in
  let rec aux l = 
    match l with
      | [] -> ()
      | e::t -> 
        let _ = res.(e)<- res.(e)+1 in
        aux t;
  in
  aux li;
  res;;

let classe_plus_frequente li =
  let rec aux l max max_classe i =
    match l with
      | [] -> max_classe
      | e::t when e < max -> aux t max max_classe (i+1)
      | e::t -> aux t e i (i+1)
  in
  aux li (-1) (-1) 0;;

let construire_modele dist k jeu =
  {distance = dist; k = k; donnees = jeu;};;


let image_from_donnee d = let im,_ = d in im;;


type dis_point = int*int;;
let get_fst e = match e with | d,_ -> d;;
let get_snd e = match e with | _,d -> d;;
let tri_insertion arr =
  for i = 1 to Array.length arr - 1 do
    let act = arr.(i) in let j = ref i in
    let valeur,_ = act in
    while 0 < !j && valeur < (get_fst arr.(!j-1)) do
      arr.(!j) <- arr.(!j -1);
      decr j;
    done;
    arr.(!j) <- act;
  done;;

(* 1 si i < j *)
let compare a b =
int_of_float ((get_fst a) -. (get_fst b));;
let determiner_classe m im =
  let dist_ind = Array.init  (Array.length m.donnees) (fun i -> (norme_1 im (image_from_donnee m.donnees.(i)),get_snd m.donnees.(i))) in
  
  (* On récupère les k images les plus proches *)
  Array.sort compare dist_ind;
 
  let k_nearest = Array.sub dist_ind 0 m.k in
  (* On rentre leurs classes dans un tableau *)
  
  let classes = Array.make 10 0 in
  for j = 0 to m.k -1 do
    let c = get_snd (k_nearest.(j)) in
    classes.(c) <- (classes.(c)+1);
  done;
  let rec construire_liste i = match i with
  | 9 -> [classes.(i)]
  | _ -> classes.(i)::(construire_liste (i+1))
  in
  (* On renvoie la classe la plus fréquente *)
  classe_plus_frequente (construire_liste 0);;

let modele_tests () =
  let ims = lire_images "x_train.csv" in
  let ets = lire_etiquettes "y_train.csv" in
  let arr = Array.init (Array.length ims) (fun i -> (ims.(i),ets.(i))) in
  construire_modele norme_1 2 arr;;

let tester_modele ()=
let m =modele_tests() in

let ims = lire_images "x_train.csv" in
let ets = lire_etiquettes "y_train.csv" in
let bon = ref 0 in
let tot = ref 0 in 
for i = 0 to 100 do
  incr tot;
  let predit = determiner_classe m ims.(i) in
  if predit = ets.(i) then incr bon;
  Format.printf "a déterminé %d sur l'entrée n° %i, réponse attendue : %d\n" predit i ets.(i);
done;
Format.printf "Réussite : %f\n Total : %d, Réussies : %d" ((float)(!bon) /. (float)(!tot)) !tot !bon;;
;;
(*tester_modele ();;*)

let pourcentage_erreur m test_datas =
  let bon = ref 0 in
  let tot = ref 0 in 
  let confusion_arr = Array.make_matrix 10 10 0 in
  for i = 0 to Array.length test_datas - 1 do
    let im,et = test_datas.(i) in
    incr tot;
    let predit = determiner_classe m im in
    confusion_arr.(et).(predit) <- confusion_arr.(et).(predit)+1;
    if predit = et then incr bon;
  done;
  ((float)(!tot - !bon))/. (float)(!tot), confusion_arr;;


let matrice_confusion m test_datas =
  let confusion_arr = Array.make_matrix 10 10 0 in
  for i = 0 to Array.length test_datas - 1 do
    if (i mod 100 = 0) then print_int i;
    let im,et = test_datas.(i) in
    let predit = determiner_classe m im in
    confusion_arr.(et).(predit) <- confusion_arr.(et).(predit)+1;
  done;
  confusion_arr;;


let test_sur_tout () =
  let m = modele_tests () in
  let ims = lire_images "x_test.csv" in
  let ets = lire_etiquettes "y_test.csv" in
  let arr = Array.init (Array.length ims) (fun i -> (ims.(i),ets.(i))) in
  let err, conf = pourcentage_erreur m arr in
  Format.printf "\n Erreur : %f\n" err;
  conf;;

let arr = test_sur_tout();;
print_string "Confusion :\n";;
for i = 0 to (Array.length arr)-1 do
  for j = 0 to (Array.length arr) -1 do
    print_int arr.(i).(j);
    print_string " ";
  done;
  print_string "\n";
done;;