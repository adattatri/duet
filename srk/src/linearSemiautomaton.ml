open Syntax
open BatPervasives

module QQMatrix = Linear.QQMatrix
module QQVector = Linear.QQVector
module VS = Linear.QQVectorSpace

type t =
  { transitions : QQMatrix.t list;

    (* Affine simulation is defined over the pre-state  *)
    simulation : QQMatrix.t  }

type lts = QQMatrix.t * QQMatrix.t

type tdlts_abstraction = QQMatrix.t * QQMatrix.t

let dimension aut = QQMatrix.nb_rows aut.simulation

let postify srk tr_symbols =
  let post_map =
    List.fold_left
      (fun map (sym, sym') -> Symbol.Map.add sym (mk_const srk sym') map)
      Symbol.Map.empty
      tr_symbols
  in
  substitute_map srk post_map

let join aut1 aut2 =
  (* coordinate the simulations of a system of (simulation, transition) pairs *)
  let rec fix trs =
    let sim = (* intersection of rowspaces of all simulations *)
      match trs with
      | [] -> assert false
      | ((sim,_)::xs) ->
        List.fold_left (fun sim (s, _) ->
            (VS.intersect sim (VS.of_matrix s)))
          (VS.of_matrix sim)
          xs
        |> VS.matrix_of
    in
    let trs' =
      trs |> List.map (fun (s, m) ->
          let r = match Linear.divide_right sim s with
            | Some r -> r
            | None -> assert false
          in
          let (mT, mM) =
            Linear.max_lds sim (QQMatrix.mul (QQMatrix.mul r m) s)
          in
          (QQMatrix.mul mT sim, mM))
    in
    let sim_dim = QQMatrix.nb_rows sim in
    if List.for_all (fun (s, _) -> sim_dim = QQMatrix.nb_rows s) trs' then
      { simulation = sim;
        transitions = List.map snd trs' }
    else
      fix trs'
  in
  let lower aut =
    List.map (fun m -> (aut.simulation, m)) aut.transitions
  in
  fix ((lower aut1)@(lower aut2))

let bottom tr_symbols const_symbols =
  { transitions = [];
    simulation =
      (List.map (fun (s,_) -> Linear.dim_of_sym s) tr_symbols)
      @ (Linear.const_dim
         ::(List.map Linear.dim_of_sym const_symbols))
      |> List.map (fun d -> QQVector.of_list [QQ.one, d])
      |> QQMatrix.of_rows }

let to_transition_formula srk aut tr_symbols =
  let pre_sim =
    QQMatrix.rowsi aut.simulation
    /@ (fun (_, row) -> Linear.of_linterm srk row)
    |> BatArray.of_enum
  in
  let postify = postify srk tr_symbols in
  let map_tf m =
    BatArray.enum pre_sim
    |> BatEnum.mapi (fun i row ->
        let lhs = postify row in
        let rhs =
          Linear.term_of_vec srk (fun i -> pre_sim.(i)) (QQMatrix.row i m)
        in
        mk_eq srk lhs rhs)
    |> BatList.of_enum
    |> mk_and srk
  in
  List.map map_tf aut.transitions
  |> mk_or srk

let lts_simulates (mA, mB) (mA', mB') =
  let ann =
    QQMatrix.interlace_columns mA mB
    |> VS.of_matrix
  in
  let ann' =
    QQMatrix.interlace_columns mA' mB'
    |> VS.of_matrix
  in
  VS.subspace ann' ann

(* Find an linear abstraction of a formula (i.e., matrices A,B,C such
   that F(x,x',y) |= Ax' = Bx + Cy, where x,x' are the transition
   symbols and y are constant symbols (including 1). *)
let abstract_lts srk phi tr_symbols const_symbols =
  let pre_syms =
    List.fold_left
      (fun syms (s, _) -> Symbol.Set.add s syms)
      Symbol.Set.empty
      tr_symbols
  in
  let pre_of_post =
    List.fold_left
      (fun map (sym, sym') -> Symbol.Map.add sym' sym map)
      Symbol.Map.empty
      tr_symbols
  in
  let aff_hull =
    let symbols =
      List.fold_left (fun syms (s,s') -> s::s'::syms) const_symbols tr_symbols
    in
    Abstract.affine_hull srk phi symbols
  in
  (* write aff_hull as Ax' = Bx + Cy, where y is a set of constant symbols *)
  BatList.fold_lefti
    (fun (mA, mB, mC) i eq ->
       let (a, b, c) =
         BatEnum.fold
           (fun (a, b, c) (coeff, dim) ->
              match Linear.sym_of_dim dim with
              | None -> (a, b, QQVector.add_term coeff dim c)
              | Some sym ->
                if Symbol.Set.mem sym pre_syms then
                  (a, QQVector.add_term coeff dim b, c)
                else if Symbol.Map.mem sym pre_of_post then
                  let pre_dim =
                    Linear.dim_of_sym (Symbol.Map.find sym pre_of_post)
                  in
                  (QQVector.add_term (QQ.negate coeff) pre_dim a, b, c)
                else
                  (a, b, QQVector.add_term coeff dim c))
           (QQVector.zero, QQVector.zero, QQVector.zero)
           (QQVector.enum (Linear.linterm_of srk eq))
       in
       (QQMatrix.add_row i a mA,
        QQMatrix.add_row i b mB,
        QQMatrix.add_row i c mC))
    (QQMatrix.zero, QQMatrix.zero, QQMatrix.zero)
    aff_hull

let abstract_generalized_eigenspace (mA, mB) lambda rank =
  (* Ax' = Bx |= ux' = lambda*ux iff there is some v such that vA = u /\ vB = lambda*u *)
  let delta = (* (lambda*B) *)
    QQMatrix.add mB (QQMatrix.scalar_mul (QQ.negate lambda) mA)
  in
  let dims =
    SrkUtil.Int.Set.union (QQMatrix.row_set mA) (QQMatrix.row_set mA)
    |> SrkUtil.Int.Set.elements
  in
  (* Compute a triple (last, simulation, dynamics), where last is a
     matrix of eigenvectors of rank r, simulation is a matrix of
     eigenvectors of rank <= r, and dynamics is a dynamics matrix for
     the simulation *)
  let rec go rank =
    if rank == 0 then
      let eigenvectors = Linear.nullspace (QQMatrix.transpose delta) dims in
      let dynamics =
        QQMatrix.identity (List.mapi (fun i _ -> i) eigenvectors)
        |> QQMatrix.scalar_mul lambda
      in
      let simulation = QQMatrix.mul (QQMatrix.of_rows eigenvectors) mA in
      (simulation, simulation, dynamics)
    else
      let (last, simulation, dynamics) = go (rank - 1) in
      let (mC, mD) = Linear.pushout delta last in
      let sim_rows = QQMatrix.nb_rows simulation in
      let (simulation', dynamics') =
        BatEnum.fold (fun (simulation, dynamics) (i, v) ->
            let dyn_row =
              QQVector.add_term lambda (sim_rows + i) (QQMatrix.row i mD)
            in
            (QQMatrix.add_row (sim_rows + i) v simulation,
             QQMatrix.add_row (sim_rows + i) dyn_row dynamics))
          (simulation, dynamics)
          (QQMatrix.rowsi (QQMatrix.mul mC mA))
      in
      (QQMatrix.sub simulation' simulation,
       simulation',
       dynamics')
  in
  if rank < 0 then
    invalid_arg "rank must be non-negative"
  else
    let (_, simulation, dynamics) = go rank in
    (simulation, dynamics)

(* Find an total deterministic linear abstraction of a formula (i.e.,
   a linear semiautomaton with a single transition) *)
let abstract_tdlts srk phi tr_symbols const_symbols =
  let (mA, mB, mC) = abstract_lts srk phi tr_symbols const_symbols in
  (* TB = MTA *)
  let (mT, mM) = Linear.max_lds mA mB in
  (* Ax' = Bx + Cy, so TAx' = TBx + TCy.
     Since TB = MTA, we have (TA)x' = M(TA)x + TCy.
     Rewrite as [ TA 0 ][x']  = [ M  TC ][ TA  0  ][x]
                [ 0  I ][y']    [ 0  I  ][ 0   I  ][y]
  *)
  let mTA = QQMatrix.mul mT mA in
  let mTA_size = QQMatrix.nb_rows mTA in
  let const_dims =
    Linear.const_dim::(List.map Linear.dim_of_sym const_symbols)
  in
  let mTC =
    let mConst =
      BatList.fold_lefti (fun mConst i const ->
          QQMatrix.add_entry const (i + mTA_size) QQ.one mConst)
        QQMatrix.zero
        const_dims
    in
    QQMatrix.mul (QQMatrix.mul mT mC) mConst
  in
  let sim =
    BatList.fold_lefti
      (fun sim i j ->
         QQMatrix.add_entry (i + mTA_size) j QQ.one sim)
      mTA
      const_dims
  in
  let transform =
    BatList.fold_lefti
      (fun tr i _ ->
         let j = i + mTA_size in
         QQMatrix.add_entry j j QQ.one tr)
      (QQMatrix.add mM mTC)
      const_dims
  in

  (sim, transform)

(* Find best abstraction of a TDLTS as a TDLTS with rational spectrum *)
let abstract_tdlts_rational transform dim =
  let dimensions = BatList.of_enum (0 -- (dim - 1)) in
  let sim =
    Linear.rational_spectral_decomposition transform dimensions
    |> List.map snd
    |> Linear.QQVectorSpace.matrix_of
  in
  (* simulation * transform = abstract_transform * simulation *)
  let abstract_transform =
    match Linear.divide_right (QQMatrix.mul sim transform) sim with
    | Some m -> m
    | None -> assert false
  in
  (sim, abstract_transform)

let abstract ?(exists=fun x -> true) srk phi tr_symbols =
  let phi =
    rewrite srk ~down:(nnf_rewriter srk) phi
    |> Nonlinear.linearize srk
  in
  let const_symbols =
    List.fold_left (fun consts (s,s') ->
        Symbol.Set.remove s (Symbol.Set.remove s' consts))
      (Symbol.Set.filter exists (symbols phi))
      tr_symbols
    |> Symbol.Set.elements
  in
  let solver = Smt.mk_solver srk in
  let rec go aut =
    Smt.Solver.add solver [mk_not srk (to_transition_formula srk aut tr_symbols)];
    match Smt.Solver.get_model solver with
    | `Unsat -> aut
    | `Unknown -> assert false
    | `Sat m ->
      match Interpretation.select_implicant m phi with
      | None -> assert false
      | Some implicant ->
        let (sim, tr) =
          abstract_tdlts srk (mk_and srk implicant) tr_symbols const_symbols
        in
        let (qq_sim, tr) =
          abstract_tdlts_rational tr (QQMatrix.nb_rows sim)
        in
        let implicant_aut =
          { simulation = QQMatrix.mul qq_sim sim;
            transitions = [tr] }
        in
        go (join aut implicant_aut)
  in
  Smt.Solver.add solver [phi];
  go (bottom tr_symbols const_symbols)

let transitions aut = aut.transitions

let simulation aut = aut.simulation

let lts_of_tdlts (sim, dyn) = (sim, QQMatrix.mul dyn sim)

let tdlts_equiv tdlts1 tdlts2 =
  let lts1 = lts_of_tdlts tdlts1 in
  let lts2 = lts_of_tdlts tdlts2 in
  lts_simulates lts1 lts2 && lts_simulates lts2 lts1

let pp_tdlts formatter (sim, dyn) =
  Format.fprintf formatter "Simulation:@\n @[%a@]@\n Dynamics:@\n @[%a@]"
    QQMatrix.pp sim
    QQMatrix.pp dyn

let pp_lts formatter (mA, mB) =
  Format.fprintf formatter "A:@\n @[%a@]@\n B:@\n @[%a@]"
    QQMatrix.pp mA
    QQMatrix.pp mB

let mk_lts mA mB = (mA, mB)
let mk_tdlts sim dyn = (sim, dyn)
