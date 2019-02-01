(*Version hexagonale : Question 18*)

open Common ;;
  
(* is_valid(int r, Cell.t (x,y) ) : bool                                            *)
(*                                                                                  *)
(* verifie que la case (c,y) est dans la grille de nbr de couronnes r               *)
let is_valid = fun r (x,y) ->
  ((x>=0 && x<r) && (y>=0 && y<(r*6)))
;;

(*range2(int r) : (int * int) list            *)
(*                                            *)
(* fonction range2 pour une grille hexagonale *)
let range2 = fun r ->
  (0,0)::List.fold_left (fun acc a -> (List.fold_left (fun acc2 b -> (a,b)::acc2 )) acc (range 0 ((a*6)-1))) [] (range 0 (r-1));;

(* fonctions auxiliaire à get_center_coord                                                                    *)
(* permettant  de tourner dans la couronne courante et donc trouver recursivement le sommet recherché         *)
 let shift_up = fun x ->
   (0,x*8) ;;
   
 let rec shift_down_right = fun (x,y) x' i ->
   if(i = 0 || x' = 0)
   then ((x,y),i)
   else shift_down_right (x+7,y-4) (x'-1) (i-1);;

 let rec shift_down = fun (x,y) x' i ->
   if(i = 0 || x' = 0)
   then ((x,y),i)
   else shift_down (x,y-8) (x'-1) (i-1);;


 let rec shift_down_left = fun (x,y) x' i ->
   if(i = 0 || x' = 0)
   then ((x,y),i)
   else shift_down_left (x-7,y-4) (x'-1) (i-1);;

 let rec shift_up_left = fun (x,y) x' i ->
   if(i = 0 || x' = 0)
    then ((x,y),i)
   else shift_up_left (x-7,y+4) (x'-1) (i-1);;
   
 let rec shift_up2 = fun (x,y) x' i ->
   if(i = 0 || x' = 0)
   then ((x,y),i)
   else shift_up2 (x,y+8) (x'-1) (i-1);;

 let rec shift_up_right = fun (x,y) x' i ->
   if(i = 0 || x' = 0)
   then ((x,y),i)
   else shift_up_right (x+7,y+4) (x'-1) (i-1);;
   
(* get_center_cord(Cell.t) : int * int                                                                 *)
(*                                                                                                     *)
(* recuperer les coordonnées du centre de la case (x,y) en utilisant des fonctions auxiliaires         *)
 let get_center_coord = fun (x,y) ->
   if (x,y) = (0,0)
   then (0,0)
   else
     let su = shift_up x in
     if(y = 0)
     then su
     else        
       let (sdr,i) = shift_down_right su x y in
       if(i = 0)
       then sdr
       else
         let (sd,i') = shift_down sdr x i in
         if(i' = 0)
         then sd
         else
           let (sdl,i'') = shift_down_left sd x i' in
           if(i'' = 0)
           then sdl
           else
             let (sul,i''') = shift_up_left sdl x i'' in
             if(i''' = 0)
             then sul
             else
               let (su2,i'''') = shift_up2 sul x i''' in
               if(i'''' = 0)
               then su2
               else
                 let (sup,i''''') = shift_up_right su2 x i'''' in
                 sup
 ;;
  

(*get_contour(Cell.t) : int * int list                                             *)
(*                                                                                               *)
(* recuperer la liste des coordonnées des six coins                                          *)

 let get_contour = fun (x,y) -> match get_center_coord (x,y) with
                                | (x',y') -> [(x'-2,y'+4); (x'-5,y'); (x'-2,y'-4); (x'+2,y'-4); (x'+5,y'); (x'+2,y'+4)]
;;







(* fonctions auxiliaire à get_coord_name  *)
(* permettant  de tourner dans la couronne courante pour trouver le nom de la case correspondant a la coordonnée donnée en parametre (x,y)   *)

 let shift_up_2 = fun (x',y') (a,b) (x,y) ->
   ((0,x'+8),(a+1,0)) ;;
   
 let rec shift_down_right_2 = fun (x',y') (a,b) i (x,y)->
   if ((x,y) = (x',y') || i = 0)
   then ((x',y'),(a,b))
   else shift_down_right_2 (x'+7,y'-4) (a,b+1) (i-1) (x,y);;

 let rec shift_down_2 = fun (x',y') (a,b) i (x,y)->
   if ((x,y) = (x',y') || i = 0)
   then ((x',y'),(a,b))
   else shift_down_2 (x',y'-8) (a,b+1) (i-1) (x,y);;
     
 let rec shift_down_left_2 = fun (x',y') (a,b) i (x,y)->
   if ((x,y) = (x',y') || i = 0)
   then ((x',y'),(a,b))
   else shift_down_left_2 (x'-7,y'-4) (a,b+1) (i-1) (x,y);;

 let rec shift_up_left_2 = fun (x',y') (a,b) i (x,y)->
   if ((x,y) = (x',y') || i = 0)
   then ((x',y'),(a,b))
   else shift_up_left_2 (x'-7,y'+4) (a,b+1) (i-1) (x,y);;

 let rec shift_up2_2 = fun (x',y') (a,b) i (x,y)->
   if ((x,y) = (x',y') || i = 0)
   then ((x',y'),(a,b))
   else shift_up2_2 (x',y'+8) (a,b+1) (i-1) (x,y);;
  
 let rec shift_up_right_2 = fun (x',y') (a,b) i (x,y)->
   if ((x,y) = (x',y') || i = 0)
   then ((x',y'),(a,b))
   else shift_up_right_2 (x'+7,y'+4) (a,b+1) (i-1) (x,y);;

   
(* get_coord_name prend en entré la coordonnée du centre d'une case (x,y)   
 et retourne le nom de celle-ci ("o" est la variable qui permet de monter en couronne, on mets 1, car 0 est déjà 
testé pour que ça fonctionne bien)  
 x et y doivent être le centre d'une case obligatoirement pour que ça fonctionne *)
 let rec get_coord_name = fun (x,y) o ->
   if (x,y) = (0,0)
   then (0,0)
   else
     let i = o in
     let (x',y') = (0,i*8) in
     let (a,b ) = (i,0) in
     if((x',y') = (x,y))
     then (a,b)
     else        
       let (sdr,(a1,b1)) = shift_down_right_2 (x',y') (a,b) i (x,y) in
       if(sdr = (x,y))
       then (a1,b1)
       else
         let (sd,(a2,b2)) = shift_down_2 (sdr) (a1,b1) i (x,y) in
         if(sd = (x,y))
         then (a2,b2)
         else
           let (sdl,(a3,b3)) = shift_down_left_2 (sd) (a2,b2) i (x,y) in
           if(sdl = (x,y))
           then (a3,b3)
           else
             let (sul,(a4,b4)) = shift_up_left_2 (sdl) (a3,b3) i (x,y) in
             if(sul = (x,y))
             then (a4,b4)
             else
               let (su2,(a5,b5)) = shift_up2_2 (sul) (a4,b4) i (x,y) in
               if(su2 = (x,y))
               then (a5,b5)
               else
                 let (sup,(a6,b6)) = shift_up_right_2 (su2) (a5,b5) (i-1) (x,y) in
                 if(sup = (x,y))
                 then (a6,b6)
                 else
                   get_coord_name (x,y) (o+1)
 ;;

(* get_neighbours(int r, Cell.t c ) : list Cell.t                                                    *)
(*                                                                                                   *)
(* retourne les voisins de la case (x,y) pour une grille hexagonale de taille de couronne r          *)
 let get_neighbours = fun r (x,y) ->
   match get_center_coord (x,y) with
   | (x',y') -> 
      let coord = [(x'+7,y'-4);(x',y'-8);(x'-7,y'-4);(x'-7,y'+4);(x',y'+8);(x'+7,y'+4)];
      in
      let coord_name = List.fold_left (fun acc a -> ( get_coord_name a 1 )::acc  ) [] coord in
      List.filter (fun x -> is_valid r x) coord_name;;



     
(* create_grid(r int ) : list Cell.t                                                      *)
(*                                                                                        *)
(* creer la grille grid de taille la lo                                                   *)
  let create_grid = fun r ->
  let lc = range2 r in
  let grid  = List.fold_left (fun acc sommet -> add_vertex sommet acc) CellMap.empty lc in
  List.fold_left (fun acc c1 -> List.fold_left (fun acc c2 -> add_edges c1 c2 acc) acc (get_neighbours r c1)) grid lc;;


      
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


(* Ne fonctionne pas pour une grille hexagonale :'( *)
    
let direction = [(1,0);(0,1);(-1,0);(0,-1)] ;; (* il faudrait 6 directions pour une grille hexagonale *)
  
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
