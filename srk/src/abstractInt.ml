open Syntax
open Normaliz
open BatPervasives

include Log.Make(struct let name = "srk.abstractInt" end)

module V = Linear.QQVector
module CS = CoordinateSystem

let opt_abstract_limit = ref (-1)

(* let print_array zzarrays = 
  let () = Array.iter (
    fun zzarray -> 
      for i=0 to (Array.length zzarray) - 1 do
      print_string (ZZ.show zzarray.(i));
      if i <> (Array.length zzarray) - 1 then print_string " " else print_newline ()
    done) zzarrays in ()
 *)
let perform_integer_hull polyhedron =
  (* print_string "in perform_integer_hull\n"; *)
  (* let zzarrays = Polyhedron.convert_to_libnormaliz_fmt polyhedron in *)
  (* let () = print_array zzarrays in *)
  polyhedron
  |> Polyhedron.convert_to_libnormaliz_fmt
  |> Libnormaliz.matrix_to_cone
  |> Libnormaliz.integer_hull
  |> Libnormaliz.cone_to_matrix
  |> Polyhedron.convert_from_libnormaliz_fmt

let abstractInt ?exists:(p=fun _ -> true) srk man phi =
  let solver = Smt.mk_solver srk in
  let phi_symbols = symbols phi in
  let symbol_list = Symbol.Set.elements phi_symbols in
  let env_proj = SrkApron.Env.of_set srk (Symbol.Set.filter p phi_symbols) in
  let cs = CoordinateSystem.mk_empty srk in

  let disjuncts = ref 0 in
  let rec go prop =
    (* Smt.Solver.push solver; *)
    (* Smt.Solver.add solver [mk_not srk (SrkApron.formula_of_property prop)]; *)
    let result =
      Log.time "lazy_dnf/sat" (Smt.Solver.get_concrete_model solver) symbol_list
    in
    match result with
    | `Unsat ->
      (* Smt.Solver.pop solver 1; *)
      prop
    | `Unknown ->
      begin
        logf ~level:`warn "abstraction timed out (%d disjuncts); returning top"
          (!disjuncts);
        (* Smt.Solver.pop solver 1; *)
        SrkApron.top man env_proj
      end
    | `Sat interp -> begin
        (* Smt.Solver.pop solver 1; *)
        incr disjuncts;
        logf "[%d] abstract lazy_dnf" (!disjuncts);
        if (!disjuncts) = (!opt_abstract_limit) then begin
          logf ~level:`warn "Met symbolic abstraction limit; returning top";
          SrkApron.top man env_proj
        end else begin
          let disjunct =
            match Interpretation.select_implicant interp phi with
            | Some d -> Polyhedron.of_implicant ~admit:true cs d
            | None -> assert false
          in
          let lia_disjunct = perform_integer_hull disjunct in
          let projected_coordinates =
            BatEnum.filter (fun i ->
                match CS.destruct_coordinate cs i with
                | `App (sym, _) -> not (p sym)
                | _ -> true)
              (0 -- (CS.dim cs - 1))
            |> BatList.of_enum
          in
          (* perform Fourier-Motzkin elimination *)
          let projected_disjunct =
            Polyhedron.project projected_coordinates lia_disjunct
            |> Polyhedron.to_apron cs env_proj man
          in
          Smt.Solver.add solver [mk_not srk (Polyhedron.to_formula cs disjunct)];
          go (SrkApron.join prop projected_disjunct)
        end
      end
  in
  Smt.Solver.add solver [phi];
  Log.time "Abstraction" go (SrkApron.bottom man env_proj)

