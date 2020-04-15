open OUnit
open Srk.Polyhedron
open Srk.ZZ

let k z = Mpz.of_int z
let l z = Mpz.to_string z

let print_array zzarrays = 
	let () = Array.iter (Array.iter (fun zz -> prerr_string (show zz))) zzarrays in
	prerr_newline ()

let normaliz_to_normaliz constraint_array = 
  convert_to_libnormaliz_fmt (convert_from_libnormaliz_fmt constraint_array)

let constraint_array1 = [|  [|k 0; k 0; k 1|];
                            [|k 1; k 2; k 3|];
                            [|k 2; k 3; k (-2)|] |]

let constraint_array2 = [| [| k 1; k 2; k 3|]; 
                           [| k 3; k 4; k (-6)|] |]

let roundtrip1 () = 
  let res = (normaliz_to_normaliz constraint_array1) in
  let () = print_array res in
  assert (constraint_array1 = res)


let roundtrip2 () = 
  assert (constraint_array2 = (normaliz_to_normaliz constraint_array2))

let suite = "srk polyhedron" >::: [
    "roundtrip1" >:: roundtrip1;
    "roundtrip2" >:: roundtrip2;
]