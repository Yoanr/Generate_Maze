open Common ;;
(* distribution(int a, int list b) : (int * int) list      *)
(*                                                         *)
(* ajoute l'element a dans chaque element de la list de b  *)
let distribution = fun v  l ->
  List.map (fun a -> (v,a)) l;;
(* Question 2: range2(int a, int list b) : (int * int) list *)
(*                                                          *)
(* ajoute l'element a dans chaque element de la list de b   *)
let range2 = fun m n ->
  List.fold_left (fun acc a -> acc@(distribution a (range 0 (n-1)))) [] (range 0 (m-1))

(* Question 8: is_valid(int la, int lo, Cell.t c ) : bool                                 *)
(*                                                                                        *)
(* verifie que la case c est dans le rectangle de largeur la et longueur lo               *)
let is_valid = fun la lo noeud ->
  match noeud with
  | (x,y) -> x < la && y < lo && 0 <= x && 0 <= y;;
  
(* Question 9: get_neighbours(int la, int lo, Cell.t c ) : list Cell.t                          *)
(*                                                                                              *)
(* retourne les voisins de la case (x,y) pour une grille de taille la*lo                        *)
let get_neighbours = fun la lo (x,y) ->
  let neighbours = [(x-1,y);(x,y-1);(x+1,y);(x,y+1)] in
  List.filter (fun x -> is_valid la lo x) neighbours;;


  
(* Question 10: create_grid(int la, int lo ) : list Cell.t                        *)
(*                                                                                        *)
(* creer la grille grid de taille la lo                                                   *)
let create_grid = fun la lo ->
  let lc = range2 lo la in
  let grid  = List.fold_left (fun acc sommet -> add_vertex sommet acc) CellMap.empty lc in
  List.fold_left (fun acc c1 -> List.fold_left (fun acc c2 -> add_edges c1 c2 acc) acc (get_neighbours lo la c1)) grid lc;; 

  
(*********** Partie 3 ***********)

(* Question 11: get_center_cord(Cell.t) : int * int                                       *)
(*                                                                                        *)
(* recuperer les coordonnées du centre de la case                                         *)
  
let get_center_coord = fun (x,y) ->
  ((1+2*x),(1+2*y));;


(* Question 12: get_contour(Cell.t) : int * int list                                             *)
(*                                                                                               *)
(* recuperer la liste des coordonnées des quatres coins                                          *)
let get_contour = fun (x,y) ->
  match get_center_coord (x,y) with
  | ( x', y' ) -> [(x'-1,y'-1);(x'-1,y'+1);(x'+1,y'+1);(x'+1,y'-1)];;
  

   
(* Methodes permettant en finalité de retourner l'intersection de 2 elements*)

let rec remove x = function
  | [] -> []
  | h::t -> if h = x then t else h::(remove x t);;
  
let rec intersect a b = match a with
  | [] -> if b = [] then [] else intersect b a
  | h::t ->
     if List.mem h b then
       let b' = remove h b in
       h::(intersect t b')
     else
       intersect t b;;

   
(* Question 13: get_wall(Cell.t,Cell.t) : int * int list                                         *)
(*                                                                                               *)
(* recupere la liste des deux coordonnées formant un mur                                         *)
let get_wall = fun (x,y) (x',y') ->
  intersect (get_contour (x,y)) (get_contour (x',y'));;



  (* Question 14: Rapport *)

    
let direction = [(1,0);(0,1);(-1,0);(0,-1)] ;; (*represente la direction prise par l'algorithme*)
  
let rec left_hand = fun g (x,y) out path dir ->
  let neighbours = (CellSet.elements(CellMap.find (x,y) g)) in
  if (x,y) = out (*si x == sortie, je retourne le chemin*)
  then (x,y)::path
  else
    match List.nth direction ((dir+1) mod 4) with
    | (x',y') -> if List.mem (x+x', y+y') neighbours (*si pas de mur a gauche *)
                 then left_hand g (x+x',y+y') out ((x,y)::path) ((dir+1) mod 4) (*je tourne a 90° CCW et j'avance*)
                 else match List.nth direction ((dir) mod 4) with
                      | (x'',y'') -> if List.mem (x+x'', y+y'') neighbours (*si pas de mur en face*)
                                     then left_hand g (x+x'',y+y'') out ((x,y)::path) (dir mod 4) (*j'avance*)
                                         else left_hand g (x,y) out path ((dir+3) mod 4) (*je tourne a 90° CW*)
;;

(* Question 17: solve_maze(Grid g,Cell.t in, Cell.t out) : int * int list                        *)
(*                                                                                               *)
(* méthode qui resout le labyrinthe g d'entrée in et de sortie out                               *)
let solve_maze = fun g inn out ->
  left_hand g inn out [] 1;;

