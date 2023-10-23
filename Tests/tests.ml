(** 
  * Ce fichier contient l'ensemble des tests qui peuvent être exécuter
  * Il contiendra un main qui pourra donc être compilé
  * Mes connaissances étant lacunaires, je n'ai pas trouvé mieux
  * Il reprendra beaucoup de code du fichier ocaml originel
*)

(** COPIE DES LIGNES DU FICHIER OCAML ORIGINEL - VOIR LIGNE 231 POUR LE DEBUT DES TESTS**)

type instruction = (* représentation d'une instruction reçu *)
  | Left (* Déplacement du curseur vers la gauche *)
  | Right (* Déplacement du curseur vers la droite *)
  | Write of char (* Écriture du caractère sur le ruban *)
  | Repeat of (int * instruction list) (* Repeter n fois la liste d'instructions *)
  | Caesar of int (* Caesar n à partir de la phase 3Aiens *)
  | Delete of char (* Delete(a) suppression du caractère a du message à partir de la phase 3Aiens *)
  | Invert (* Invert : retournement du message  à partir de la phase 3Aiens *)
  
type program = instruction list (* Un programme est simplement une liste d'instruction *)

type ruban = {
  gauche: char list;
  droite: char list; (* Le curseur est toujours à l'emplacement du premier élément de droite *)
}

(**
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
  *@requires: Nothing
  *@ensures: Retourne le ruban en se déplaçant vers la droite, c'est à dire que le premier caractère de la liste de droite
  passe à gauche
  Exemples: * func_droite {gauche=['c';'b';'a']; droite=['d';'e';'f']} == {gauche=['d';'c';'b';'a']; droite=['e';'f']}
            * func_droite {gauche=['c';'b';'a']; droite=[]} == {gauche=[' ';'c';'b';'a']; droite=[]} // Non précisé dans l'énoncé
            * func_droite {gauche=[]; droite=['d';'e';'f']} == {gauche=['d']; droite=['e';'f']}
            * func_droite {gauche=[]; droite=[]} == {gauche=[' ']; droite=[]} // Non précisé dans l'énoncé
  *@raises: Nothing
*)
let func_droite rub = match rub.droite with
| [] -> {gauche=' '::rub.gauche; droite=[]} 
| droiteh::droitet -> {gauche=droiteh::rub.gauche; droite=droitet}


(**
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
  *@requires: Un entier `n` strictement positif, une liste d'instruction `instructions` non vide et `aux_function` qui est la fonction
    dans execute_program et qui permet de lire les instructions
  *@ensures: Retourne le ruban en exécutant `n` fois les instructions `instructions`
            Sachant qu'il nous faut avoir `aux_function`, on ne peut pas tester cette fonction directement
  *@raises: Nothing
*)
let rec func_repeter rub instructions n aux_function = match n with
  | 0 -> rub
  | n -> func_repeter (aux_function rub instructions) instructions (n-1) aux_function


(**
  *@requires: Un entier `n` qui peut être positif ou négatif et un caractère `caractere` affichable de l'ASCII non étendu
  *@ensures: Retourne le caractère en décalant le code ASCII du caractère de `n` positions, avec un décalage circulaire
  Exemples: * func_caesar 'a' 1 == 'b'
            * func_caesar 'a' 2 == 'c'
            * func_caesar 'a' -1 == 'z'
            * func_caesar 'a' 0 == 'a'
            * func_caesar 'A' 1 == 'B'
            * func_caesar 'A' -1 == 'Z'
            * func_caesar 'A' 27 == 'B'
            * func_caesar '!' 1 == '!'
            * func_caesar '@' 1 == '@' 
  *@raises: Nothing
*)
let func_caesar caractere n = let m = n mod 26 in
  if Char.code caractere > 96 && Char.code caractere < 123 && m > 0 then
    if Char.code caractere + m > 122
    then Char.chr (Char.code caractere + m - 26)
    else Char.chr (Char.code caractere + m)
  else if Char.code caractere > 64 && Char.code caractere < 91 && m > 0 then
    if Char.code caractere + m > 90
    then Char.chr (Char.code caractere + m - 26)
    else Char.chr (Char.code caractere + m)
  else if Char.code caractere > 96 && Char.code caractere < 123 && m < 0 then
    if Char.code caractere + m < 97
    then Char.chr (Char.code caractere + m + 26)
    else Char.chr (Char.code caractere + m)
  else if Char.code caractere > 64 && Char.code caractere < 91 && m < 0 then
    if Char.code caractere + m < 65
    then Char.chr (Char.code caractere + m + 26)
    else Char.chr (Char.code caractere + m)
  else caractere


(**
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
  *@requires: Nothing
  *@ensures: Inverse les listes de gauche et droite en replaçant le curseur sur la même lettre
  Exemples: * func_inverser {gauche=['c';'b';'a']; droite=['d';'e';'f']} == {gauche=['e';'f']; droite=['d';'c';'b';'a']}
            * func_inverser {gauche=[]; droite=['a']} == {gauche=[]; droite=['a']}
            * func_inverser {gauche=[]; droite=['a';'b']} == {gauche=['b']; droite=['a']}
            * func_inverser {gauche=['a']; droite=[]} n'est pas défini et renvoie une erreur
            * func_inverser {gauche=[]: droite=[]} == {gauche=[]: droite=[]}
  *@raises: Si la liste de droite est vide et celle de gauche non-vide, failwith "Impossible d'utiliser la fonction inverser avec un char vide..."
*)
let func_inverser rub = match (rub.gauche, rub.droite) with
  | ([], []) -> {gauche=[]; droite=[]}
  | ([], droiteh::droitet) -> {gauche=droitet; droite=[droiteh]}
  | (gaucheh::gauchet, []) -> failwith "Impossible d'utiliser la fonction inverser avec un char vide dans la liste droite"
  | (gaucheh::gauchet, droiteh::droitet) -> {gauche=droitet; droite=droiteh::(rub.gauche)}


(**
  *@requires: Nothing
  *@ensures: Retourne le ruban après avoir exécuté toutes les instructions de la liste `prog` à partir d'un ruban vierge infini
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
  *@requires: Un entier `compteur` strictement supérieur positif, et devrait être initialisé à 1 pour le but recherché
              Un caractere `lettre` affichable de l'ASCII non étendu, et devrait être la première lettre de l'accumulateur (char list) pour le but recherché
  *@ensures: Renvoie un couple (liste_char * increment) tel que la fonction supprime les caractères consécutifs (donc s'arrête à la première différence)
   et identiques à `lettre` tout en comptant le nombre de fois où ce premier caractère est apparu en tête de la liste dans `increment`
  Exemples: * compteur_repetition ['a';'a';'b'] 1 'a' == (['a';'b'], 2)
            * compteur_repetition ['a';'b';'a'] 1 'a' == (['a';'b';'a'], 2)
            * compteur_repetition ['a';'a';'a;'] 1 'a' == (['a'], 3)
  *@raises: Nothing
*)
let rec compteur_repetition acc compteur lettre = match acc with
  | [] -> ([], compteur)
  | lettre2::reste2 -> (if lettre = lettre2
                      then compteur_repetition reste2 (compteur+1) lettre
                      else (acc, compteur))


(**
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





(** DEBUT DES TESTS **)

let test_1_1 = func_gauche {gauche=['c';'b';'a']; droite=['d';'e';'f']}
let test_1_2 = func_gauche {gauche=[]; droite=['d';'e';'f']}
let test_1_3 = func_gauche {gauche=['c';'b';'a']; droite=[]}
let test_1_4 = func_gauche {gauche=[]; droite=[]}

let test_2_1 = func_droite {gauche=['c';'b';'a']; droite=['d';'e';'f']}
let test_2_2 = func_droite {gauche=['c';'b';'a']; droite=[]}
let test_2_3 = func_droite {gauche=[]; droite=['d';'e';'f']}
let test_2_4 = func_droite {gauche=[]; droite=[]}

let test_3_1 = func_ecrire {gauche=['c';'b';'a']; droite=['d';'e';'f']} 'z'
let test_3_2 = func_ecrire {gauche=['c';'b';'a']; droite=[]} 'z'
let test_3_3 = func_ecrire {gauche=[]; droite=['d';'e';'f']} 'z'
let test_3_4 = func_ecrire {gauche=[]; droite=[]} 'z'
let test_3_5 = func_ecrire {gauche=[]; droite=[]} ';'

let test_4_1 = func_caesar 'a' 1
let test_4_2 = func_caesar 'a' 2
let test_4_3 = func_caesar 'a' (-1)
let test_4_4 = func_caesar 'a' 0
let test_4_5 = func_caesar 'A' 1
let test_4_6 = func_caesar 'A' (-1)
let test_4_7 = func_caesar 'A' 27
let test_4_8 = func_caesar '!' 1
let test_4_9 = func_caesar '@' 1 

let test_5_1 = func_supprimer 'a' 'a' ['b']
let test_5_2 = func_supprimer 'b' 'a' ['b']
let test_5_3 = func_supprimer 'a' 'a' []
let test_5_4 = func_supprimer 'b' 'a' []

let test_6_1 = func_inverser {gauche=['c';'b';'a']; droite=['d';'e';'f']}
let test_6_2 = func_inverser {gauche=[]; droite=['a']}
let test_6_3 = func_inverser {gauche=[]; droite=['a';'b']}
(*let test_6_4 = func_inverser {gauche=['a']; droite=[]} n'est pas défini et renvoie une erreur... Mais je ne sais pas tester une Exception*)
let test_6_5 = func_inverser {gauche=[]; droite=[]}

let test_7_1 = execute_program [Write 'a']
let test_7_2 = execute_program [Write 'a'; Write 'b'] 
let test_7_3 = execute_program [Write 'a'; Right; Write 'b'] 
let test_7_4 = execute_program [Write 'a'; Left; Write 'b']
let test_7_5 = execute_program [Write 'a'; Right; Repeat (2, [Write 'l'; Right])]
let test_7_6 = execute_program [Write 'a'; Right; Write 'b'; Right; Write 'c'; Caesar 1]



let die i = 
  let _ = Format.fprintf Format.err_formatter "Usage: %s" Sys.argv.(0) in
  exit i
  
let main = 
  assert( test_1_1 = {gauche=['b';'a']; droite=['c';'d';'e';'f']} );
  print_endline "Le test_1_1 est validé";
  assert( test_1_2 = {gauche=[]; droite=[' ';'d';'e';'f']}  );
  print_endline "Le test_1_2 est validé";
  assert( test_1_3 = {gauche=['b';'a']; droite=['c']} );
  print_endline "Le test_1_3 est validé";
  assert( test_1_4 = {gauche=[]; droite=[' ']} );
  print_endline "Le test_1_4 est validé";

  assert( test_2_1 = {gauche=['d';'c';'b';'a']; droite=['e';'f']} );
  print_endline "Le test_2_1 est validé";
  assert( test_2_2 = {gauche=[' ';'c';'b';'a']; droite=[]} );
  print_endline "Le test_2_2 est validé";
  assert( test_2_3 = {gauche=['d']; droite=['e';'f']} );
  print_endline "Le test_2_3 est validé";
  assert( test_2_4 = {gauche=[' ']; droite=[]} );
  print_endline "Le test_2_4 est validé";;

  assert( test_3_1 = {gauche=['c';'b';'a']; droite=['z';'e';'f']} );
  print_endline "Le test_3_1 est validé";
  assert( test_3_2 = {gauche=['c';'b';'a']; droite=['z']} );
  print_endline "Le test_3_2 est validé";
  assert( test_3_3 = {gauche=[]; droite=['z';'e';'f']} );
  print_endline "Le test_3_3 est validé";
  assert( test_3_4 = {gauche=[]; droite=['z']} );
  print_endline "Le test_3_4 est validé";
  assert( test_3_5 = {gauche=[]; droite=[';']} );
  print_endline "Le test_3_5 est validé";

  assert( test_4_1 = 'b' );
  print_endline "Le test_4_1 est validé";
  assert( test_4_2 = 'c' );
  print_endline "Le test_4_2 est validé";
  assert( test_4_3 = 'z' );
  print_endline "Le test_4_3 est validé";
  assert( test_4_4 = 'a' );
  print_endline "Le test_4_4 est validé";
  assert( test_4_5 = 'B' );
  print_endline "Le test_4_5 est validé";
  assert( test_4_6 = 'Z' );
  print_endline "Le test_4_6 est validé";  
  assert( test_4_7 = 'B' );
  print_endline "Le test_4_7 est validé";
  assert( test_4_8 = '!' );
  print_endline "Le test_4_8 est validé";
  assert( test_4_9 = '@'  );
  print_endline "Le test_4_9 est validé";

  assert( test_5_1 = ['b'] );
  print_endline "Le test_5_1 est validé";
  assert( test_5_2 = ['b';'b'] );
  print_endline "Le test_5_2 est validé";
  assert( test_5_3 = [] );
  print_endline "Le test_5_3 est validé";
  assert( test_5_4 = ['b'] );
  print_endline "Le test_5_4 est validé";

  assert( test_6_1 = {gauche=['e';'f']; droite=['d';'c';'b';'a']} );
  print_endline "Le test_6_1 est validé";
  assert( test_6_2 = {gauche=[]; droite=['a']} );
  print_endline "Le test_6_2 est validé";
  assert( test_6_3 = {gauche=['b']; droite=['a']} );
  print_endline "Le test_6_3 est validé";
  assert( test_6_5 = {gauche=[]; droite=[]} );
  print_endline "Le test_6_5 est validé";

  assert( test_7_1 = {gauche=[]; droite=['a']} );
  print_endline "Le test_7_1 est validé";
  assert( test_7_2 = {gauche=[]; droite=['b']} );
  print_endline "Le test_7_2 est validé";
  assert( test_7_3 = {gauche=['a']; droite=['b']} );
  print_endline "Le test_7_3 est validé";
  assert( test_7_4 = {gauche=[]; droite=['b';'a']} );
  print_endline "Le test_7_4 est validé";
  assert( test_7_5 = {gauche=['l';'l';'a']; droite=[]} );
  print_endline "Le test_7_5 est validé";
  assert( test_7_6 = {gauche=['c';'b']; droite=['d']} );
  print_endline "Le test_7_6 est validé";;



let _ =
  if Array.length Sys.argv = 1
  then
    main
  else die 2