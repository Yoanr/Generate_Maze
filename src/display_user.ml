(* Question 5.2 version interactive *)
open Common ;;
open Labysquare ;;
open Display ;;

(* fonction auxiliaire qui, étant donnée la case courante et la touche
saisie par l’utilisateur, retourne la case d’arrivée (qu’elle soit valide ou non).

(x,y) : case courante
input : touche saisie *)
  let get_next_by_user = fun (x,y) input ->
  match  input with
  | 'q' -> (x-1,y)
  | 'z' -> (x,y+1)
  |  'd' -> (x+1,y)
  | 's' -> (x,y-1)
  | _ -> (x,y)
  ;;

(* méthode auxiliaire qui permet de tracer le morceau de chemin entre v et succ (succ est la case ou l'utilisateur veut aller) 

f : fonction pour gerer le zoom
g : grille
v : sommet courant
succ : voisins du sommet courant
v_out : sortie 
*)
let rec draw_path_user_aux = fun f -> fun g -> fun v -> fun succ -> fun v_out  ->
let ev = Graphics.wait_next_event [Graphics.Key_pressed] in
let key = ev.Graphics.key in
let v' = get_next_by_user v key in
if v' = v_out || Char.code key = 27
 then 
   Graphics.close_graph ()
else
    if CellSet.mem v' succ then
      try
        let succ' = CellMap.find v' g in
        let x, y  = f (get_center_coord v) in
        let x',y' = f (get_center_coord v') in
        let _ = Graphics.moveto x y in
        let _ = Graphics.lineto x' y' in
        draw_path_user_aux f g v' succ' v_out
      with _ ->
        draw_path_user_aux f g v succ v_out
    else
      draw_path_user_aux f g v succ v_out
;;

(* cette méthode utilise la méthode auxiliaire pour tracer un morceau de chemin donné en argument, similaire à celle dans le fichier display.ml mais sans chemin donné completement, car on n'utilise pas de clear *)
let draw_path_user = fun f -> fun g  -> fun v_in -> fun v_out ->
       try
      let succ = CellMap.find v_in g in
      let _ = Graphics.set_color Graphics.blue in
      draw_path_user_aux f g v_in succ v_out
    with _ ->
      let (i,j) = v_in in Printf.printf "(%d,%d) is outside of the maze\n" i j
;;

(* cette méthode represente la boucle de jeu pour la partie intéractive, similaire à celle dans le fichier display.ml mais sans chemin *)
let test_interactif = fun (w,h) -> fun (z,sw,sh) -> fun g -> fun v_in -> fun v_out ->
   try
  let _ = init w h in
  let f = fun (x,y) -> (z*x+sw, z*y+sh) in
  let _ = draw_maze f g v_in v_out in
  let _ = draw_path_user f g v_in v_out in
  wait_and_close ()
   with _ ->
     Printf.printf "Program is finished\n"
;;
