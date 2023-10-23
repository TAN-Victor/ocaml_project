
(** 
    Cette partie du programme NE DOIT EN AUCUN CAS être modifiée sous peine
    de voir votre code ne pas passer les tests 
*)
type instruction = (* représentation d'une instruction reçu *)
  | Left (* Déplacement du curseur vers la gauche *)
  | Right (* Déplacement du curseur vers la droite *)
  | Write of char (* Écriture du caractère sur le ruban *)
  | Repeat of (int * instruction list) (* Repeter n fois la liste d'instructions *)
  | Caesar of int (* Caesar n à partir de la phase 3Aiens *)
  | Delete of char (* Delete(a) suppression du caractère a du message à partir de la phase 3Aiens *)
  | Invert (* Invert : retournement du message  à partir de la phase 3Aiens *)
  
type program = instruction list (* Un programme est simplement une liste d'instruction *)

(**
   VOUS N'AVEZ PAS BESOIN DE LIRE OU DE COMPRENDRE CETTE PARTIE DU CODE (LES SIX FONCTIONS SUIVANTES). 
   ELLE EST VOLONTAIREMENT NON COMMENTÉE ET RÉDIGÉE DE MANIÈRE PEU CLAIRE 
 *)
let rec analyse_comment fd =
  let c = Scanf.bscanf fd "%c" (fun c -> c) in
  if c = '\n'
  then ()
  else analyse_comment fd
             
let rec analyse_program_aux =
  fun fd lvl  ->
    try 
      let c = Scanf.bscanf fd "%c" (fun a  -> a) in
      if c = '#'
      then
        let _ = analyse_comment fd in
        analyse_program_aux fd lvl
      else if c = ';' || c = '\n' || c = ' '
      then analyse_program_aux fd lvl
      else if c = ']'
      then
        if lvl > 0
        then []
        else raise (Invalid_argument "Error on char ]")
      else if c = 'W'
      then
        let i= Write(Scanf.bscanf fd "(%c)" (fun a -> a)) in
        let li = analyse_program_aux fd lvl in
        i::li
      else if c = 'C'
      then
        let i= Caesar(Scanf.bscanf fd "(%d)" (fun a -> a)) in
        let li = analyse_program_aux fd lvl in
        i::li
      else if c = 'D'
      then
        let a = Scanf.bscanf fd "(%c)" (fun a -> a) in 
        let i= Delete(a) in
        let li = analyse_program_aux fd lvl in
        i::li
      else if c = 'R'
      then let li = analyse_program_aux fd lvl in
        Right::li
      else if c = 'I'
      then Invert::analyse_program_aux fd lvl
      else if c = 'L'
      then Left::analyse_program_aux fd lvl
      else if c = 'F'
      then
        let n = Scanf.bscanf fd "(%d,[" (fun n -> n) in
        let l = analyse_program_aux fd (lvl + 1) in
        let c = Scanf.bscanf fd "%c" (fun a -> a) in
        if c <> ')' then raise (Invalid_argument ("Error found '"^String.make 1 c^"' instead of ')'"))
        else
          let li = analyse_program_aux fd lvl in
          Repeat(n,l)::li
      else
        let _ = Format.printf  "ERROR '%c' (%d)@." c (Char.code c) in
        assert false
    with End_of_file -> []

let rec read_file_aux =
  fun acc fd ->
    try
      let c = Scanf.bscanf fd "%c" (fun x -> x) in
      read_file_aux (c::acc) fd
    with End_of_file -> acc

let read_file file =
  try
    if Sys.is_directory file
    then raise (Invalid_argument "is a directory")
    else
      let fd = Scanf.Scanning.open_in file in 
      List.rev (read_file_aux [] fd)
  with exc ->
    Format.fprintf Format.err_formatter "Problème à l'ouverture du fichier %s (exception %s)@." file (Printexc.to_string exc);
    exit 127

                    
let rec fprintf_instruction fmt i =
  match i with
  | Write c -> Format.fprintf fmt "W(%c)" c
  | Right -> Format.fprintf fmt "R"
  | Left -> Format.fprintf fmt "L"
  | Repeat(n,li) ->
      Format.fprintf fmt "F(%d,[%a])" n (fun fmt li -> List.iter (fun i -> Format.fprintf fmt "%a;" fprintf_instruction i) li) li
  | Caesar n -> Format.fprintf fmt "C(%d)" n
  | Delete(a) -> Format.fprintf fmt "D(%c)" a
  | Invert -> Format.fprintf fmt "I"
            
let fprintf_program fmt l =
  List.iter (fun i -> Format.fprintf fmt "%a;" fprintf_instruction i) l

(*** 
     Retour au côté clair
*)


(* 
   [print_program] : program -> unit 
   affiche un [program] sur la sortie standard de l'executable
 *)  
let print_program p = Format.printf "%a" fprintf_program p

(*
  [analyse_program] : unit -> program
  Lit un programme 1Aien ou 2Aien, l'analyse et le retourne sous forme d'une valeur de type [program]
 *)                    
let analyse_program file =
  try
    if Sys.is_directory file
    then raise (Invalid_argument "is a directory")
    else
      let fd = Scanf.Scanning.open_in file in 
      analyse_program_aux fd 0
  with exc ->
    Format.fprintf Format.err_formatter "Problème à l'ouverture du fichier %s (exception %s)@." file (Printexc.to_string exc);
    exit 127









    
(** Votre code doit commencer à partir de ce point.

    NE MODIFIER RIEN AU DESSUS DE CE POINT 
*)


(**
  * Dans la suite, dans les exemples / tests, on utilisera la syntaxe "==" pour donner le résultat attendu d'une fonction
*)

type ruban = {
  gauche: char list;
  droite: char list; (* Le curseur est toujours à l'emplacement du premier élément de droite *)
}

(**
  *@type: val func_gauche : ruban -> ruban = <fun>
  *@requires: Nothing
  *@ensures: Retourne le ruban en se déplaçant vers la gauche, c'est à dire que le premier caractère de la liste de gauche
  passe à droite
  Exemples: * func_gauche {gauche=['c';'b';'a']; droite=['d';'e';'f']} == {gauche=['b';'a']; droite=['c';'d';'e';'f']}
            * func_gauche {gauche=[]; droite=['d';'e';'f']} == {gauche=[]; droite=[' ';'d';'e';'f']} // Non précisé dans l'énoncé
            * func_gauche {gauche=['c';'b';'a']; droite=[]} == {gauche=['b';'a']; droite=['c']}
            * func_gauche {gauche=[]; droite=[]} == {gauche=[]; droite=[' ']} // Non précisé dans l'énoncé
  *@raises: Nothing
*)
let func_gauche rub = match rub.gauche with
| [] -> {gauche=[]; droite=' '::rub.droite}
| gaucheh::gauchet -> {gauche=gauchet; droite=gaucheh::rub.droite}


(**
  *@type: val func_droite : ruban -> ruban = <fun>
  *@requires: Nothing
  *@ensures: Retourne le ruban en se déplaçant vers la droite, c'est à dire que le premier caractère de la liste de droite
  passe à gauche
  Exemples: * func_droite {gauche=['c';'b';'a']; droite=['d';'e';'f']} [] == {gauche=['d';'c';'b';'a']; droite=['e';'f']}
            * func_droite {gauche=['c';'b';'a']; droite=[]} [] == {gauche=[' ';'c';'b';'a']; droite=[]} // Non précisé dans l'énoncé
            * func_droite {gauche=[]; droite=['d';'e';'f']} [] == {gauche=['d']; droite=['e';'f']}
            * func_droite {gauche=[]; droite=[]} [] == {gauche=[' ']; droite=[]} // Non précisé dans l'énoncé
  *@raises: Nothing
*)
let func_droite rub = match rub.droite with
| [] -> {gauche=' '::rub.gauche; droite=[]} 
| droiteh::droitet -> {gauche=droiteh::rub.gauche; droite=droitet}


(**
  *@type: val func_ecrire : ruban -> char -> ruban = <fun>
  *@requires: Un caractère `cara` affichable de l'ASCII non étendu
  *@ensures: Retourne le ruban en écrivant le caractère `cara` sur le premier élément de la liste de droite du ruban
  Exemples: * func_ecrire {gauche=['c';'b';'a']; droite=['d';'e';'f']} 'z' == {gauche=['c';'b';'a']; droite=['z';'e';'f']}
            * func_ecrire {gauche=['c';'b';'a']; droite=[]} 'z' == {gauche=['c';'b';'a']; droite=['z']}
            * func_ecrire {gauche=[]; droite=['d';'e';'f']} 'z' == {gauche=[]; droite=['z';'e';'f']}
            * func_ecrire {gauche=[]; droite=[]} 'z' == {gauche=[]; droite=['z']}
  *@raises: Nothing
*)
let func_ecrire rub cara = match rub.droite with
| [] -> {gauche=rub.gauche; droite=cara::[]}
| droiteh::droitet -> {gauche=rub.gauche; droite=cara::droitet}


(**
  *@type: val func_repeter : 'a -> 'b -> int -> ('a -> 'b -> 'a) -> 'a = <fun>
  *@requires: Un entier `n` strictement positif et une liste d'instruction `instructions` non vide
  *@ensures: Retourne le ruban en exécutant `n` fois les instructions `instructions`
    S'arrête car l'entier `n` est strictement positif et décroit à chaque itération jusqu'à 0
  Exemples: * func_repeter {gauche=['c';'b';'a']; droite=['d';'e';'f']} 1 [Write 'z'] == {gauche=['c';'b';'a']; droite=['z';'e';'f']}
            * func_repeter {gauche=['c';'b';'a']; droite=['d';'e';'f']} 2 [Write 'z'] == {gauche=['c';'b';'a']; droite=['z';'e';'f']}
            * func_repeter {gauche=['c';'b';'a']; droite=['d';'e';'f']} 2 [Write 'z'; Right] == {gauche=['z';'z';'c';'b';'a']; droite=['f']}
  *@raises: Nothing
*)
let rec func_repeter rub instructions n aux_function = match n with
  | 0 -> rub
  | n -> func_repeter (aux_function rub instructions) instructions (n-1) aux_function


(**
  *@type: val func_caesar : char -> int -> char = <fun>
  *@requires: Un entier `n` qui peut être positif ou négatif et un caractère `caractere` affichable de l'ASCII non étendu
  *@ensures: Retourne le caractère en décalant le code ASCII du caractère de `n` positions, avec un décalage circulaire
  Exemples: * func_caesar 'a' 1 == 'b'
            * func_caesar 'a' 2 == 'c'
            * func_caesar 'a' (-1) == 'z'
            * func_caesar 'a' 0 == 'a'
            * func_caesar 'A' 1 == 'B'
            * func_caesar 'A' (-1) == 'Z'
            * func_caesar 'A' 27 == 'B'
            * func_caesar '!' 1 == '!'
            * func_caesar '@' 1 == '@' 
  *@raises: Nothing
*)
let func_caesar caractere n = let m = n mod 26 in (* On prend le modulo 26 pour avoir un décalage circulaire, décaler de 27 revient à décaler de 1 *)
  if Char.code caractere > 96 && Char.code caractere < 123 && m > 0 then (* Minuscules et César positif *)
    if Char.code caractere + m > 122 (* Si le décalage dépasse la valeur ASCII de la lettre z *)
    then Char.chr (Char.code caractere + m - 26)
    else Char.chr (Char.code caractere + m)
  else if Char.code caractere > 64 && Char.code caractere < 91 && m > 0 then (* Majuscules et César positif *)
    if Char.code caractere + m > 90 (* Si le décalage dépasse la valeur ASCII de la lettre Z *)
    then Char.chr (Char.code caractere + m - 26)
    else Char.chr (Char.code caractere + m)
  else if Char.code caractere > 96 && Char.code caractere < 123 && m < 0 then (* Minuscules et César négatif *)
    if Char.code caractere + m < 97 (* Si le décalage dépasse la valeur ASCII de la lettre a *)
    then Char.chr (Char.code caractere + m + 26)
    else Char.chr (Char.code caractere + m)
  else if Char.code caractere > 64 && Char.code caractere < 91 && m < 0 then (* Majuscules et César négatif *)
    if Char.code caractere + m < 65 (* Si le décalage dépasse la valeur ASCII de la lettre A *)
    then Char.chr (Char.code caractere + m + 26)
    else Char.chr (Char.code caractere + m)
  else caractere  (* Si le caractère n'est pas une lettre, on ne le décale pas *)


(**
  *@type: val func_supprimer : 'a -> 'a -> 'a list -> 'a list = <fun>
  *@requires: Deux caractères `caracteres` et `a` affichables de l'ASCII non étendu et une liste de char `liste_sans_a` ne contenant pas a
  *@ensures: Renvoie la liste en ajoutant `caractere` au début de la liste ssi ce n'est pas `a`
  Exemples: * func_supprimer 'a' 'a' ['b'] == ['b']
            * func_supprimer 'b' 'a' ['b'] == ['b';'b']
            * func_supprimer 'a' 'a' [] == []
            * func_supprimer 'b' 'a' [] == ['b']
  *@raises: Nothing
*)
let func_supprimer caractere a liste_sans_a  = 
  if a = caractere
  then liste_sans_a
  else caractere::liste_sans_a


(**
  *@type: val func_inverser : ruban -> ruban = <fun>
  *@requires: Nothing
  *@ensures: Inverse les listes de gauche et droite en replaçant le curseur sur la même lettre
  Exemples: * func_inverser {gauche=['c';'b';'a']; droite=['d';'e';'f']} == {gauche=['e';'f']; droite=['d';'c';'b';'a']}]
            * func_inverser {gauche=[]; droite=['a']} == {gauche=[]; droite=['a']}
            * func_inverser {gauche=[]; droite=['a';'b']} == {gauche=['b']; droite=['a']}
            * func_inverser {gauche=['a']; droite=[]} n'est pas défini et renvoie une erreur
            * func_inverser {gauche=[]; droite=[]} == {gauche=[]; droite=[]}
  *@raises: Si la liste de droite est vide et celle de gauche non-vide, failwith "Impossible d'utiliser la fonction inverser avec un char vide..."
*)
let func_inverser rub = match (rub.gauche, rub.droite) with
  | ([], []) -> {gauche=[]; droite=[]}
  | ([], droiteh::droitet) -> {gauche=droitet; droite=[droiteh]}
  | (gaucheh::gauchet, []) -> failwith "Impossible d'utiliser la fonction inverser avec un char vide dans la liste droite"
  | (gaucheh::gauchet, droiteh::droitet) -> {gauche=droitet; droite=droiteh::(rub.gauche)}


(**
  *@type: val execute_program : instruction list -> ruban = <fun>
  *@requires: Nothing
  *@ensures: Retourne le ruban après avoir exécuté toutes les instructions de la liste `prog` à partir d'un ruban vierge infini
    S'arrête car le programme est une liste d'instructions finie qui rétrécit à chaque itération, et finit par être vide
    En particulier, pour Caesar et Delete, on applique List.map et List.fold_right pour appliquer la fonction sur chaque élément de la liste
    d'où l'absence de la récursivité explicite dans ces fonctions
  Exemples: * execute_program [Write 'a'] == {gauche=[]; droite=['a']}
            * execute_program [Write 'a'; Write 'b'] == {gauche=[]; droite=['b']}
            * execute_program [Write 'a'; Right; Write 'b'] == {gauche=['a']; droite=['b']}
            * execute_program [Write 'a'; Left; Write 'b'] == {gauche=[]; droite=['b';'a']}
            * execute_program [Write 'a'; Right; Repeat (2, [Write 'l'; Right])] == {gauche=['l';'l';'a']; droite=[]}
            * execute_program [Write 'a'; Right; Write 'b'; Right; Write 'c'; Caesar 1] == {gauche=['c';'b']; droite=['d']}
            * Les autres tests importants sont normalement dans les fonctions précédentes
  *@raises: Nothing
*)
let execute_program p = let rec aux rub prog  = match prog with
  | [] -> rub
  | h::t -> match h with
    | Left -> aux (func_gauche rub) t
    | Right -> aux (func_droite rub) t
    | Write cara-> aux (func_ecrire rub cara) t
    | Repeat (nombre, instructions) -> aux (func_repeter rub instructions nombre aux) t
    | Caesar n -> let f caractere = func_caesar caractere n in
      aux {gauche=List.map f rub.gauche; droite=List.map f rub.droite} t
    | Delete a -> let f caractere liste_sans_a = func_supprimer caractere a liste_sans_a
        in aux {gauche=List.fold_right f rub.gauche []; droite=List.fold_right f rub.droite []} t
    | Invert -> aux (func_inverser rub) t
in aux {gauche=[]; droite=[]} p;;


(**
  *@type: val fold_ruban : ('a -> char -> 'a) -> 'a -> ruban -> 'a = <fun>
  *@requires: Nothing
  *@ensures: Transforme le ruban en une liste (en allant tout à gauche du ruban) puis effectue un List.fold_left sur cette liste
  *@raises: Nothing
*)  
let fold_ruban f v0 r = let rec aux rub = match rub.gauche with
  | [] -> List.fold_left f v0 rub.droite
  | gaucheh::gauchet -> aux {gauche = gauchet; droite=gaucheh::rub.droite}
in aux r;;






(** Troisième partie **)


(**
  *@type: val compteur_repetition : 'a list -> int -> 'a -> 'a list * int = <fun>
  *@requires: Un entier `compteur` strictement supérieur positif, et devrait être initialisé à 1 pour le but recherché
              Un caractere `lettre` affichable de l'ASCII non étendu, et devrait être la première lettre de l'accumulateur (char list) pour le but recherché
  *@ensures: Renvoie un couple (liste_char * increment) tel que la fonction supprime les caractères consécutifs (donc s'arrête à la première différence)
   et identiques à `lettre` tout en comptant le nombre de fois où ce premier caractère est apparu en tête de la liste dans `increment`
  Exemples: * compteur_repetition ['a';'a';'b'] 1 'a' == (['a';'b'], 2)
            * compteur_repetition ['a';'b';'a'] 1 'a' == (['a';'b';'a'], 1)
            * compteur_repetition ['a';'a';'a;'] 1 'a' == (['a'], 3)
  *@raises: Nothing
*)
let rec compteur_repetition acc compteur lettre = match acc with
  | [] -> ([], compteur)
  | lettre2::reste2 -> (if lettre = lettre2
                      then compteur_repetition reste2 (compteur+1) lettre
                      else (acc, compteur))


(**
  *@type: val generate_program : char list -> instruction list = <fun>
  *@requires: Une chaine de caractères `msg` composée de caractères affichables de l'ASCII non étendu
  *@ensures: Renvoie une liste d'instructions permettant d'écrire `msg` sur un ruban vierge
              Les commentaires sont pris en compte car ils seront ignorés par le programme lors de la lecture du programme
              Le retour chariot est pris en compte par un retour à la ligne et non pas par '\n'
              Ici, la fonction détecte la présence de plusieurs caractères identiques consécutifs et les remplace par une instruction Repeat
  Exemples: * generate_program 'Hello' == [Write 'H'; Right; Write 'e'; Right; Repeat (2, [Write 'l'; Right]); Write 'o'; Right] // Le Right final pourrait être modifié
*)
let generate_program msg = let rec aux3 program fin_du_msg = match fin_du_msg with
  | [] -> program
  | lettre::reste -> let (reste_ins, compteur_ins) = compteur_repetition reste 1 lettre in
                    if compteur_ins = 1
                    then Write lettre::Right::aux3 program reste_ins
                    else (Repeat (compteur_ins, Write lettre::[Right]))::aux3 program reste_ins
in aux3 [] msg;;
                      



  
(** Votre code doit s'arreter à partir de ce point.

    NE MODIFIER RIEN EN DESSOUS DE CE POINT 
*)











                         
                         
let die i = 
  let _ = Format.fprintf Format.err_formatter "Usage: %s <1|2|3> <file>@." Sys.argv.(0) in
  exit i
  
let main phase file =
  if phase = "1" || phase = "2"
  then
    let li = analyse_program file in
    let rub = execute_program li in
    let _ = fold_ruban (fun _ c -> Format.printf "%c" c) () rub in
    Format.printf "@."
  else if phase = "3"
  then
    let msg = read_file file in
    let p = generate_program msg in
    print_program p
  else die 1


let _ =
  if Array.length Sys.argv = 3 
  then
    main Sys.argv.(1) Sys.argv.(2)
  else die 2
