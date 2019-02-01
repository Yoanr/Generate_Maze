(* Question 5.2 version interactive *)
open Common ;;
open Labysquare ;;
open Display_user ;;
    
  let _ = Random.self_init () ;;
  let inn = int_of_string Sys.argv.(2)-1 ;;
  let out = int_of_string Sys.argv.(1)-1 ;;
  let zoom = int_of_string Sys.argv.(3) ;;
  let g = create_grid (int_of_string Sys.argv.(1)) (int_of_string Sys.argv.(2)) ;;
  let m = generate_maze g ;;
  let _ = test_interactif (800,600) (zoom,1,1) m (0,0) (inn,out) ;;

