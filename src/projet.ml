(*Version basique (carré)*)
open Common ;;
open Labysquare ;;
open Display ;;
    
  let _ = Random.self_init () ;;
  let inn = int_of_string Sys.argv.(2)-1 ;;
  let out = int_of_string Sys.argv.(1)-1 ;;
  let zoom =  int_of_string Sys.argv.(3) ;;
  let g = create_grid (int_of_string Sys.argv.(1)) (int_of_string Sys.argv.(2)) ;;
  let m = generate_maze g ;;
  let s1 = solve_maze m (0,0) (inn,out) ;;
  let _ = test (800,600) (zoom,1,1) m (0,0) (inn,out) s1 ;;
  

