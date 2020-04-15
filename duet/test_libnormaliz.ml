open OUnit
open Normaliz

let k z = Mpz.of_int z
let l z = Mpz.to_string z

let cone_to_matrix cone = Libnormaliz.cone_to_matrix cone

let matrix_to_cone mat = Libnormaliz.matrix_to_cone mat

let integer_hull mat = cone_to_matrix (Libnormaliz.integer_hull (matrix_to_cone mat))

let matrix_to_matrix mat = cone_to_matrix (matrix_to_cone mat)

let constraints1 = [| [| k 1; k 2; k 3|]; 
                     [| k 3; k 4; k (-6)|] |]

let int_hull1 = [|  [|k 0; k 0; k 1|];
                    [|k 1; k 2; k 3|];
                    [|k 2; k 3; k (-2)|] |]

let constraints2 = [| [|k 2; k 1|] |]

let int_hull2 = [| [|k 0; k 1 |]; [|k 1; k 0|] |]

let constraints3 = [| [|k 0; k 2; k 1|]; |]

let int_hull3 = [| [|k 0; k 0; k 1 |]; [|k 0; k 1; k 0|] |]

let roundtrip1 () = 
  assert ((matrix_to_matrix constraints1) = constraints1)

let roundtrip2 () = 
  assert ((matrix_to_matrix int_hull1) = int_hull1)

let roundtrip3 () = 
  assert ((matrix_to_matrix constraints2) = constraints2)

let roundtrip4 () = 
  assert ((matrix_to_matrix int_hull2) = int_hull2)

let roundtrip5 () = 
  assert ((matrix_to_matrix constraints3) = constraints3)

let roundtrip6 () = 
  assert ((matrix_to_matrix int_hull3) = int_hull3)

let integer_hull1 () = 
  assert (int_hull1 = (integer_hull constraints1))

let integer_hull2 () = 
  assert (int_hull2 = (integer_hull constraints2))

let integer_hull3 () = 
  assert (int_hull3 = (integer_hull constraints3))

let suite = "Libnormaliz" >::: [
    "roundtrip1" >:: roundtrip1;
    "roundtrip2" >:: roundtrip2;
    "roundtrip3" >:: roundtrip3;
    "roundtrip4" >:: roundtrip4;
    "roundtrip5" >:: roundtrip5;
    "roundtrip6" >:: roundtrip6;
    "integer_hull1" >:: integer_hull1;
    "integer_hull2" >:: integer_hull2;
    "integer_hull3" >:: integer_hull3;
  ]
