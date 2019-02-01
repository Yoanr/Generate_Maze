
(*********** Partie 1 ***********)

(* Question 1: range(int a, int b) : int list                  *)
(*                                                             *)
(* construit la liste de tous les entiers entre a et b inclus  *)
let rec range = fun a b ->
  if a > b
  then []
  else a :: range (a+1) b;;



(* Question 3: remove_nth(int list l, int i) : int list    *)
(*                                                         *)
(* Retire l'element à l'indice i de la liste l             *)
let rec remove_nth = fun l i ->  
  match l with
  | [] -> []
  | h::t -> let x = if i=0 then [] else h::[] in
            x@remove_nth t (i-1);;

(* extract_random(int list l) : (int list, int r)          *)
(*                                                         *)
(* Retire un element aléatoire de la list l                *)
let rec extract_random = fun l ->
  let i = Random.int (List.length l) in
  (remove_nth l i, List.nth l i);;

(* shuffle(int list l) : int list                          *)
(*                                                         *)
(* Melanger une liste l                                    *)
let rec shuffle = fun l ->
  let n = List.length l in
  if n=0
  then []
  else
    let (li, x) =  extract_random l in 
    x::shuffle li
;;

(*********** Partie 2 ***********)

module Cell =
  struct
    type t = int * int
    let compare =  Pervasives.compare
  end
;;

module CellMap = Map.Make(Cell) ;;
module CellSet = Set.Make(Cell) ;;
type grid = CellSet.t CellMap.t ;;

(* Question 6: add_vertex( Cell.t c, grid g ) : grid            *)
(*                                                              *)
(* ajoute un sommet sans successeur au graphe grid              *)
let add_vertex = fun noeud grid ->
  if (CellMap.mem noeud grid)
  then grid
  else
    CellMap.add noeud CellSet.empty grid;;

(* Question 7: add_edges( Cell.t c1, Cell.t c2, grid g ) : grid      *)
(*                                                                   *)
(* ajoute une arrete aux sommets c1 et c2 graphe grid                *)
let add_edges = fun noeud1 noeud2 map ->
  let map_tmp = CellMap.add (noeud1) (CellSet.add (noeud2) (CellMap.find noeud1 map)) (map) in 
  CellMap.add (noeud2) (CellSet.add noeud1 (CellMap.find noeud2 map)) (map_tmp);;

(* je considere que dans la map grid nous avont toujours les arretes dans les deux sens, 
donc je verifie seulement la présence d'un coté dans la fonction ci dessus *)

(*********** Partie 4 ***********)
  
(* Question 15: generate_maze_aux(Grid g, Grid m,Cell.t v, Cell.t,Cell.t list l) : int * int list    *)
(*                                                                                                   *)
(* methode auxiliaire qui genere un labyrinthe                                                       *)
let rec generate_maze_aux = fun g m v l ->
  if l = []
  then m
  else
    let v' = List.hd l in
    let t = List.tl l in
    if (CellMap.mem v' m)
    then
      generate_maze_aux g m v t
    else
      let m1 = add_vertex v' (add_vertex v m) in
      let m'= add_edges v v' m1 in
      let l' = CellSet.elements (CellMap.find v' g) in
      let m'' = generate_maze_aux g m' v' (shuffle l') in
      generate_maze_aux g m'' v t;;
  
  

(* Question 16: generate_maze(Grid g, Grid m,Cell.t v, Cell.t,Cell.t list l) : int * int list                                         *)
(*                                                                                               *)
(* méthode qui genere un labyrinthe                                          *)
let generate_maze = fun g ->
  match CellMap.choose g with (x,y) ->
    generate_maze_aux g CellMap.empty x (CellSet.elements y);;
  

let get_good_neighbour = fun g x l ->
  List.hd (List.filter (fun x1 -> not (List.mem x1 l)) (CellSet.elements(CellMap.find x g)));;





  
(* seek(Grid g,Cell.t in, Cell.t out,Cell.t list chemin) : int * int list                                         *)
(*                                                                                               *)
(* méthode recursive auxiliaire qui resout le labyrinthe g d'entrée inn et de sortie out   *)
(* 

ne fonctionne pas très bien !!!! :'(

exception ListeVide of ((int*int) list);;
 
let rec seek = fun g inn out path ->
  match inn with
  | (x,y) -> let _ = Printf.printf "(%d,%d)\n" x y in
  if (List.mem inn path)
  then path
  else
    let npath = inn::path in
    if (inn = out)
    then npath
    else
      let neighbours = (CellSet.elements(CellMap.find inn g)) in                   
      if (neighbours = [])
      then raise (ListeVide path)
      else
        try 
          List.fold_left (fun acc a -> seek g a out ((x,y)::path)) [] neighbours
        with _  -> seek g inn out path
   
 *)
