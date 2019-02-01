(*Version hexagonale : question 18*)
open Common ;;
open Labyhexa ;;
open Display_hexa ;;
    
let _ = Random.self_init () ;;
  let r = int_of_string Sys.argv.(1) ;;
  let inn = (r-1,0) ;;
  let out = (r-1,(r-1)*3) ;;
  let zoom =  int_of_string Sys.argv.(2) ;;
  let g = create_grid (r) ;;
  let m = generate_maze g ;;
    
  (*let s1 = solve_maze m inn out;; 

"left_hand" Ne fonctionne pas avec le labyrinthe hexagonale*)
  let _ = test (800,600) (zoom,400,300) m inn out [] ;;
  
