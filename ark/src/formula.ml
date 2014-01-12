(*pp camlp4find deriving.syntax *)

open Apak
open Term
open ArkPervasives
open BatPervasives

exception Nonlinear

module type S = sig
  type t
  include Putil.Hashed.S with type t := t
  include Putil.OrderedMix with type t := t

  module T : Term.S

  val top : t
  val bottom : t
  val conj : t -> t -> t
  val disj : t -> t -> t
  val leqz : T.t -> t
  val eqz : T.t -> t
  val ltz : T.t -> t
  val atom : T.t atom -> t
  val eq : T.t -> T.t -> t
  val leq : T.t -> T.t -> t
  val geq : T.t -> T.t -> t
  val lt : T.t -> T.t -> t
  val gt : T.t -> T.t -> t
  val negate : t -> t

  val big_conj : t BatEnum.t -> t
  val big_disj : t BatEnum.t -> t

  val eval : ('a, T.t atom) formula_algebra -> t -> 'a
  val view : t -> (t, T.t atom) open_formula

  val of_abstract : 'a T.D.t -> t
  val abstract : ?exists:(T.V.t -> bool) option ->
                 'a Apron.Manager.t ->
                 t ->
                 'a T.D.t
  val abstract_assign : 'a Apron.Manager.t ->
                        'a T.D.t ->
                        T.V.t ->
                        T.t ->
                        'a T.D.t
  val abstract_assume : 'a Apron.Manager.t -> 'a T.D.t -> t -> 'a T.D.t

  val exists : (T.V.t -> bool) -> t -> t
  val exists_list : T.V.t list -> t -> t
  val opt_qe_strategy : ((T.V.t -> bool) -> t -> t) ref
  val qe_lme : (T.V.t -> bool) -> t -> t
  val qe_z3 : (T.V.t -> bool) -> t -> t
  val qe_cover : (T.V.t -> bool) -> t -> t

  val simplify : (T.V.t -> bool) -> t -> t
  val simplify_z3 : (T.V.t -> bool) -> t -> t
  val simplify_dillig : (T.V.t -> bool) -> t -> t
  val opt_simplify_strategy : ((T.V.t -> bool) -> t -> t) list ref

  val of_smt : Smt.ast -> t
  val to_smt : t -> Smt.ast
  val implies : t -> t -> bool
  val equiv : t -> t -> bool
  val map : (T.t atom -> t) -> t -> t
  val subst : (T.V.t -> T.t) -> t -> t
  val select_disjunct : (T.V.t -> QQ.t) -> t -> t option
  val symbolic_bounds : (T.V.t -> bool) -> t -> T.t -> (pred * T.t) list
  val linearize : (unit -> T.V.t) -> t -> t
  val symbolic_abstract : (T.t list) -> t -> (QQ.t option * QQ.t option) list
  val disj_optimize : (T.t list) -> t -> (QQ.t option * QQ.t option) list list

  val dnf_size : t -> int
  val nb_atoms : t -> int
  val size : t -> int

  val log_stats : unit -> unit

  module Syntax : sig
    val ( && ) : t -> t -> t
    val ( || ) : t -> t -> t
    val ( == ) : T.t -> T.t -> t
    val ( < ) : T.t -> T.t -> t
    val ( > ) : T.t -> T.t -> t
    val ( <= ) : T.t -> T.t -> t
    val ( >= ) : T.t -> T.t -> t
  end
end

module Make (T : Term.S) = struct
  open Hashcons

  module F = struct
    type t =
    | Or of t Hset.t
    | And of t Hset.t
    | Atom of T.t atom

    let hash = function
      | Or xs         -> Hashtbl.hash (Hset.hash xs, 0)
      | And xs        -> Hashtbl.hash (Hset.hash xs, 1)
      | Atom (LeqZ t) -> Hashtbl.hash (T.hash t, 2)
      | Atom (EqZ t)  -> Hashtbl.hash (T.hash t, 3)
      | Atom (LtZ t)  -> Hashtbl.hash (T.hash t, 4)

    let atom_equal x y = match x,y with
      | LeqZ s, LeqZ t
      | EqZ s, EqZ t
      | LtZ s, LtZ t -> T.equal s t
      | _, _ -> false

    let equal x y = match x,y with
      | Or xs, Or ys   -> Hset.equal xs ys
      | And xs, And ys -> Hset.equal xs ys
      | Atom a, Atom b -> atom_equal a b
      | _, _ -> false
  end

  open F
  module HC = Hashcons.Make(F)
  let formula_tbl = HC.create 200003
  let hashcons x = Log.time "formula:hashcons" (HC.hashcons formula_tbl) x

  type t = F.t hash_consed
  module T = T

  include Putil.MakeFmt(struct
    type a = t
    let rec format_in formatter phi = match phi with
      | Or xs -> Format.fprintf formatter "Or %a" (Hset.format format_in) xs
      | And xs -> Format.fprintf formatter "And %a" (Hset.format format_in) xs
      | Atom (LeqZ t) -> Format.fprintf formatter "%a <= 0" T.format t
      | Atom (EqZ t) -> Format.fprintf formatter "%a == 0" T.format t
      | Atom (LtZ t) -> Format.fprintf formatter "%a < 0" T.format t
    let rec format formatter phi = format_in formatter phi.node
  end)
  module Compare_t = struct
    type a = t
    let compare x y = Pervasives.compare x.tag y.tag
  end
  let compare = Compare_t.compare
  let equal x y = x.tag == y.tag
  let hash x = x.hkey

  let top = hashcons (Atom (LeqZ T.zero))
  let bottom = hashcons (Atom (LeqZ T.one))

  let mk_and xs =
    if Hset.is_empty xs then top
    else if Hset.mem bottom xs then bottom
    else hashcons (And xs)

  let mk_or xs =
    if Hset.is_empty xs then bottom
    else if Hset.mem top xs then top
    else hashcons (Or xs)

  let conj phi psi =
    if phi.tag == top.tag then psi
    else if psi.tag == top.tag then phi
    else if phi.tag == bottom.tag || psi.tag == bottom.tag then bottom
    else begin match phi.node, psi.node with
    | (And xs, And ys) -> hashcons (And (Hset.union xs ys))
    | (And xs, _) -> hashcons (And (Hset.add psi xs))
    | (_, And xs) -> hashcons (And (Hset.add phi xs))
    | (Or xs, Or ys) ->
      let common = Hset.inter xs ys in
      if Hset.is_empty common
      then hashcons (And (Hset.add phi (Hset.singleton psi)))
      else begin
	let phi = mk_or (Hset.diff xs common) in
	let psi = mk_or (Hset.diff ys common) in
	mk_and (Hset.add (mk_or (Hset.add phi (Hset.singleton psi))) common)
      end
    | (_, _) -> hashcons (And (Hset.add phi (Hset.singleton psi)))
    end

  let disj phi psi =
    if phi.tag == bottom.tag then psi
    else if psi.tag == bottom.tag then phi
    else if phi.tag == top.tag || psi.tag == top.tag then top
    else begin match phi.node, psi.node with
    | (Or xs, Or ys) -> hashcons (Or (Hset.union xs ys))
    | (Or xs, _) -> hashcons (Or (Hset.add psi xs))
    | (_, Or xs) -> hashcons (Or (Hset.add phi xs))
    | (And xs, And ys) ->
      let common = Hset.inter xs ys in
      if Hset.is_empty common
      then hashcons (Or (Hset.add phi (Hset.singleton psi)))
      else begin
	let phi = mk_and (Hset.diff xs common) in
	let psi = mk_and (Hset.diff ys common) in
	mk_and (Hset.add (mk_or (Hset.add phi (Hset.singleton psi))) common)
      end
    | (_, _) -> hashcons (Or (Hset.add phi (Hset.singleton psi)))
    end

  let leqz term =
    match T.get_const term with
    | Some k -> if QQ.leq k QQ.zero then top else bottom
    | _ -> hashcons (Atom (LeqZ term))
  let eqz term =
    match T.get_const term with
    | Some k -> if QQ.equal k QQ.zero then top else bottom
    | _ -> hashcons (Atom (EqZ term))
  let ltz term =
    match T.get_const term with
    | Some k -> if QQ.lt k QQ.zero then top else bottom
    | _ -> hashcons (Atom (LtZ term))
  let atom = function
    | LeqZ t -> leqz t
    | EqZ t  -> eqz t
    | LtZ t  -> ltz t

  let eq s t = eqz (T.sub s t)
  let leq s t = leqz (T.sub s t)
  let geq s t = leqz (T.sub t s)
  let lt s t = ltz (T.sub s t)
  let gt s t = ltz (T.sub t s)

  module M = Memo.Make(struct
    type t = F.t hash_consed
    let hash t = t.hkey
    let equal s t = s.tag == t.tag
  end)

  let eval alg =
    M.memo_recursive ~size:991 (fun eval x ->
      match x.node with
      | Or xs ->
	let e = Hset.enum xs in
	begin match BatEnum.get e with
	| Some elt ->
	  BatEnum.fold (fun x y -> alg (OOr (x, eval y))) (eval elt) e
	| None -> alg (OAtom (LeqZ T.one))
	end
      | And xs ->
	let e = Hset.enum xs in
	begin match BatEnum.get e with
	| Some elt ->
	  BatEnum.fold (fun x y -> alg (OAnd (x, eval y))) (eval elt) e
	| None -> alg (OAtom (LeqZ T.zero))
	end
      | Atom atom -> alg (OAtom atom))

  let view t = match t.node with
    | Atom atom -> OAtom atom
    | And xs ->
      (* todo: skip enumerations; write functions in hset to make this more
	 efficient *)
      let e = Hset.enum xs in
      begin match BatEnum.count e with
      | 0 -> OAtom (LeqZ T.zero)
      | 1 -> assert false (* this should have been promoted! *)
      | 2 -> OAnd (BatEnum.get_exn e, BatEnum.get_exn e)
      | _ -> OAnd (BatEnum.get_exn e, hashcons (And (Hset.of_enum e)))
      end
    | Or xs ->
      let e = Hset.enum xs in
      begin match BatEnum.count e with
      | 0 -> OAtom (LeqZ T.one)
      | 1 -> assert false (* this should have been promoted! *)
      | 2 -> OOr (BatEnum.get_exn e, BatEnum.get_exn e)
      | _ -> OOr (BatEnum.get_exn e, hashcons (Or (Hset.of_enum e)))
      end

  let to_smt =
    let alg = function
      | OAtom (LeqZ t) -> Smt.mk_le (T.to_smt t) (Smt.const_int 0)
      | OAtom (LtZ t) -> Smt.mk_lt (T.to_smt t) (Smt.const_int 0)
      | OAtom (EqZ t) -> Smt.mk_eq (T.to_smt t) (Smt.const_int 0)
      | OAnd (x, y) -> Smt.conj x y
      | OOr (x, y) -> Smt.disj x y
    in
    eval alg

  let log_stats () =
    let (length, count, total, min, median, max) = HC.stats formula_tbl in
    Log.log 0 "----------------------------\n Formula stats";
    Log.logf 0 "Length:\t\t%d" length;
    Log.logf 0 "Count:\t\t%d" count;
    Log.logf 0 "Total size:\t%d" total;
    Log.logf 0 "Min bucket:\t%d" min;
    Log.logf 0 "Median bucket:\t%d" median;
    Log.logf 0 "Max bucket:\t%d" max

  let negate_atom = function
    | LeqZ t -> ltz (T.neg t)
    | LtZ t -> leqz (T.neg t)
    | EqZ t -> disj (ltz t) (ltz (T.neg t))

  let negate =
    let alg = function
    | OOr (x, y) -> conj x y
    | OAnd (x, y) -> disj x y
    | OAtom atom -> negate_atom atom
    in
    eval alg

  let map f =
    let alg = function
      | OOr (phi, psi) -> disj phi psi
      | OAnd (phi, psi) -> conj phi psi
      | OAtom atom -> f atom
    in
    eval alg

  let subst sigma =
    let term_subst = T.subst sigma in (* Build memo table here *)
    let subst_atom = function
      | LtZ t -> ltz (term_subst t)
      | LeqZ t -> leqz (term_subst t)
      | EqZ t -> eqz (term_subst t)
    in
    map subst_atom

  let of_smt ast =
    let open Z3 in
    let ctx = Smt.get_context () in
    let smt_term vs = T.of_smt (List.nth vs) in
    let rec of_smt vars ast =
      match get_ast_kind ctx ast with
      | APP_AST -> begin
	let app = to_app ctx ast in
	let decl = get_app_decl ctx app in
	let args =
	  (0 -- (get_app_num_args ctx app - 1)) /@ (get_app_arg ctx app)
	in
	match get_decl_kind ctx decl with
	| OP_TRUE -> top
	| OP_FALSE -> bottom
	| OP_AND ->
	  BatEnum.fold conj top (BatEnum.map (of_smt vars) args)
	| OP_OR ->
	  BatEnum.fold disj bottom (BatEnum.map (of_smt vars) args)
	| OP_NOT -> begin
	  match BatList.of_enum (BatEnum.map (of_smt vars) args) with
	  | [phi] -> negate phi
	  | _ -> assert false
	end
	| OP_EQ -> begin
	  match BatList.of_enum (BatEnum.map (smt_term vars) args) with
	  | [x;y] -> eq x y
	  | _ -> assert false
	end
	| OP_LE -> begin
	  match BatList.of_enum (BatEnum.map (smt_term vars) args) with
	  | [x;y] -> leq x y
	  | _ -> assert false
	end
	| OP_GE -> begin
	  match BatList.of_enum (BatEnum.map (smt_term vars) args) with
	  | [x;y] -> geq x y
	  | _ -> assert false
	end
	| OP_LT -> begin
	  match BatList.of_enum (BatEnum.map (smt_term vars) args) with
	  | [x;y] -> lt x y
	  | _ -> assert false
	end
	| OP_GT -> begin
	  match BatList.of_enum (BatEnum.map (smt_term vars) args) with
	  | [x;y] -> gt x y
	  | _ -> assert false
	end
	| _ -> assert false
      end
      | QUANTIFIER_AST ->
	let num_vars = Z3.get_quantifier_num_bound ctx ast in
	let new_vars =
	  BatEnum.map
	    (T.V.of_smt % Z3.get_quantifier_bound_name ctx ast)
	    (0--(num_vars-1))
	in
	let vars = List.append (BatList.of_enum new_vars) vars in
	of_smt vars (Z3.get_quantifier_body ctx ast)
      | NUMERAL_AST
      | VAR_AST
      | FUNC_DECL_AST
      | SORT_AST
      | UNKNOWN_AST -> assert false
    in
    of_smt [] ast

  let of_goal g =
    let ctx = Smt.get_context() in
    let subgoal_formula n = of_smt (Z3.goal_formula ctx g n) in
    let formulae =
      BatEnum.map subgoal_formula (0 -- (Z3.goal_size ctx g - 1))
    in
    BatEnum.fold conj top formulae

  let of_apply_result result =
    let ctx = Smt.get_context() in
    let goal_formula n = of_goal (Z3.apply_result_get_subgoal ctx result n) in
    let formulae =
      BatEnum.map
	goal_formula
	(0 -- (Z3.apply_result_get_num_subgoals ctx result - 1))
    in
    BatEnum.fold conj top formulae

  let implies phi psi =
    let s = new Smt.solver in
    s#assrt (to_smt phi);
    s#assrt (Smt.mk_not (to_smt psi));
    match s#check () with
    | Smt.Sat   -> false
    | Smt.Unsat -> true
    | Smt.Undef -> assert false

  let equiv phi psi =
    let s = new Smt.solver in
    let phi = to_smt phi in
    let psi = to_smt psi in
    s#assrt (Smt.disj
	       (Smt.conj phi (Smt.mk_not psi))
	       (Smt.conj (Smt.mk_not phi) psi));
    match s#check () with
    | Smt.Sat   -> false
    | Smt.Unsat -> true
    | Smt.Undef -> assert false

  let big_disj = BatEnum.fold disj bottom 
  let big_conj = BatEnum.fold conj top

  (* A strange term evaluates to different things according to T.eval and
     m#eval_qq.  For debugging purposes only: strange terms should not
     exist *)
  let strange_term m t = 
    let x = T.evaluate (m#eval_qq % T.V.to_smt) t in
    let y = m#eval_qq (T.to_smt t) in
    if not (QQ.equal x y) then begin
      Log.errorf "Found a strange term: %a" T.format t;
      assert false
    end

  (* A strange formula is one that contains a strange term.  For debugging
     purposes only: strange formulae should not exist *)
  let strange_formula m phi =
    let f = function
      | OAtom (LeqZ t) | OAtom (LtZ t) | OAtom (EqZ t) -> strange_term m t
      | _ -> ()
    in
    eval f phi

  (** [select_disjunct m phi] selects a clause [psi] in the disjunctive normal
      form of [psi] such that [m |= psi] *)
  let select_disjunct m phi =
    let f = function
      | OAtom (LeqZ t) ->
	if QQ.leq (T.evaluate m t) QQ.zero then Some (leqz t) else None
      | OAtom (LtZ t) ->
	if QQ.lt (T.evaluate m t) QQ.zero then Some (ltz t) else None
      | OAtom (EqZ t) ->
	if QQ.equal (T.evaluate m t) QQ.zero then Some (eqz t) else None
      | OAnd (Some phi, Some psi) -> Some (conj phi psi)
      | OAnd (_, _) -> None
      | OOr (x, None) | OOr (_, x) -> x
    in
    eval f phi

  (** [unsat_residual m phi] replaces every atom in [phi] which the model [m]
      satisfies with [true] (and leaves unsatisfied atoms unchanged). *)
  let unsat_residual m phi =
    let f = function
      | OAtom (LeqZ t) ->
	if QQ.leq (T.evaluate m t) QQ.zero then top else leqz t
      | OAtom (LtZ t) ->
	if QQ.lt (T.evaluate m t) QQ.zero then top else ltz t
      | OAtom (EqZ t) ->
	if QQ.equal (T.evaluate m t) QQ.zero then top else eqz t
      | OAnd (phi, psi) -> conj phi psi
      | OOr (phi, psi) -> disj phi psi
    in
    eval f phi

  module VarSet = Putil.Set.Make(T.V)
  module D = T.D
  open Apron

  let term_free_vars term =
    let f = function
      | OVar v -> VarSet.singleton v
      | OConst _ -> VarSet.empty
      | OAdd (x,y) | OMul (x,y) | ODiv (x,y) -> VarSet.union x y
      | OFloor x -> x
    in
    T.eval f term

  let formula_free_vars phi =
    let f = function
      | OOr (x,y) | OAnd (x,y) -> VarSet.union x y
      | OAtom (LeqZ t) | OAtom (EqZ t) | OAtom (LtZ t) -> term_free_vars t
    in
    eval f phi

  let of_tcons env tcons =
    let open Tcons0 in
    let t = T.of_apron env tcons.texpr0 in
    match tcons.typ with
    | EQ      -> eqz t
    | SUPEQ   -> leqz (T.neg t)
    | SUP     -> ltz (T.neg t)
    | DISEQ   -> negate (eqz t)
    | EQMOD _ -> assert false (* todo *)

  let mk_env phi = D.Env.of_enum (VarSet.enum (formula_free_vars phi))

  let dnf_size phi =
    let alg = function
      | OOr (x, y) -> x + y
      | OAnd (x, y) -> x * y
      | _ -> 1
    in
    eval alg phi

  let nb_atoms phi =
    let alg = function
      | OOr (x, y) | OAnd (x, y) -> x + y
      | _ -> 1
    in
    eval alg phi

  let size phi =
    let alg = function
      | OOr (x, y) | OAnd (x, y) -> x + y + 1
      | _ -> 1
    in
    eval alg phi

  let of_abstract x =
    let open D in
    let man = Abstract0.manager x.prop in
    let tcons = BatArray.enum (Abstract0.to_tcons_array man x.prop) in
    big_conj (BatEnum.map (of_tcons x.env) tcons)

  (* [lazy_dnf join bottom top prop_to_smt phi] computes an abstraction of phi
     by lazily computing its disjunctive normal form.
     [join] : [abstract -> purely conjunctive formula -> abstract]
         [join abstract psi] computes an abstract property which
         overapproximates both abstract and psi.
     [bottom] : [abstract]
         Represents false
     [top] : [abstract]
         Represents true
     [prop_to_smt] : [abstract -> smt formula]
         Convert an abstract property to an SMT formula
     [phi] : [formula]
         The formula to abstract
  *)
  let lazy_dnf ~join ~bottom ~top prop_to_smt phi =
    let s = new Smt.solver in
    let disjuncts = ref 0 in
    let rec go prop =
      s#push ();
      s#assrt (Smt.mk_not (prop_to_smt prop));
      match s#check () with
      | Smt.Unsat -> prop
      | Smt.Undef ->
	begin
	  Log.errorf "lazy_dnf failed (%d disjuncts)" (!disjuncts);
	  assert false
	end
      | Smt.Sat -> begin
	let m = s#get_model () in
	s#pop ();
	incr disjuncts;
	Log.logf Log.info "[%d] lazy_dnf" (!disjuncts);
	let disjunct = match select_disjunct (m#eval_qq % T.V.to_smt) phi with
	  | Some d -> d
	  | None -> begin (* This should be impossible. *)
	    let phi = unsat_residual (m#eval_qq % T.V.to_smt) phi in
	    Log.errorf
	      "Couldn't select disjunct for formula:\n%a\nwith model:\n%s"
	      format phi
	      (m#to_string ());
	    Log.errorf "Smt:\n%s\n" (Smt.ast_to_string (to_smt phi));
	    assert false
	  end
	in
	go (join prop disjunct)
      end
    in
    s#assrt (to_smt phi);
    go bottom

  (* Convert a *conjunctive* formula into a list of apron tree constraints *)
  let to_apron man env phi =
    let open D in
    let to_apron = T.to_apron env in
    let alg = function
    | OAtom (LeqZ t) -> [Tcons0.make (to_apron (T.neg t)) Tcons0.SUPEQ]
    | OAtom (LtZ t) -> [Tcons0.make (to_apron (T.neg t)) Tcons0.SUP]
    | OAtom (EqZ t) -> [Tcons0.make (to_apron t) Tcons0.EQ]
    | OAnd (phi, psi) -> phi @ psi
    | OOr (_, _) -> assert false
    in
    { prop = 
	Abstract0.of_tcons_array
	  man
	  (Env.int_dim env)
	  (Env.real_dim env)
	  (Array.of_list (eval alg phi));
      env = env }

  let abstract ?exists:(p=None) man phi =
    let env = mk_env phi in
    let env_proj = match p with
      | Some p -> D.Env.filter p env
      | None   -> env
    in
    let join prop disjunct =
      let new_prop = match p with
	| Some p -> D.exists man p (to_apron man env disjunct)
	| None   -> to_apron man env disjunct
      in
      D.join prop new_prop
    in
    let (top, bottom) = (D.top man env_proj, D.bottom man env_proj) in
    lazy_dnf ~join:join ~bottom:bottom ~top:top (to_smt % of_abstract) phi

  let abstract_assign man x v t =
    let open Apron in
    let open D in
    let vars = VarSet.add v (term_free_vars t) in
    let x = D.add_vars (VarSet.enum vars) x in
    { prop =
	Abstract0.assign_texpr_array
	  man
	  x.prop
	  [| Env.dim_of_var x.env v|]
	  [| T.to_apron x.env t |]
	  None;
      env = x.env }

  let abstract_assume man x phi = abstract man (conj (of_abstract x) phi)

  (** [symbolic_bounds p phi t] computes a set of bounds for the real term [t]
      which are implied by property [phi], and where each variable in each
      bound satisfies [p]. *)
  let symbolic_bounds p phi t =
    let open Apron in
    let open NumDomain in
    let open D in
    let open Lincons0 in
    let man = Polka.manager_alloc_strict () in
    let vars = term_free_vars t in
    let x = D.add_vars (VarSet.enum vars) (abstract man phi) in
    let tdim = Env.dimension x.env in (* unused real dimension to store t *)
    let change =
      { Dim.dim = [| Env.dimension x.env |];
	Dim.intdim = 0;
	Dim.realdim = 1 }
    in
    let prop = Abstract0.add_dimensions man x.prop change false in
    let prop =
      Abstract0.assign_texpr_array
	man
	prop
	[| tdim |]
	[| T.to_apron x.env t |]
	None
    in
    let x = D.exists man p { prop = prop; env = x.env } in
    let tdim = Env.dimension x.env in (* tdim gets shifted by projection *)

    let show v =
      if v == tdim then "$" else T.V.show (Env.var_of_dim x.env v)
    in
    Log.logf Log.info "Symbound projection: %a"
      (Abstract0.print show) x.prop;

    let lincons = Abstract0.to_lincons_array man x.prop in
    let symbounds = ref [] in
    for i = 0 to Array.length lincons - 1 do
      let linexpr = lincons.(i).linexpr0 in
      match qq_of_coeff (Linexpr0.get_coeff linexpr tdim) with
      | Some tcoeff ->
	(* constraints that do not involve tdim are irrelevant *)
	if not (QQ.equal tcoeff QQ.zero) then begin
	  let pred = match lincons.(i).typ, QQ.lt tcoeff QQ.zero with
	    | (SUPEQ, true)  -> Pleq
	    | (SUPEQ, false) -> Pgeq
	    | (SUP, true)    -> Plt
	    | (SUP, false)   -> Pgt
	    | (EQ, true)     -> Peq
	    | (EQ, false)    -> Peq
	    | (_, _)         -> assert false
	  in
	  let symbound =
	    let t = match qq_of_coeff (Linexpr0.get_cst linexpr) with
	      | Some k -> ref (T.const (QQ.negate (QQ.div k tcoeff)))
	      | None -> assert false
	    in
	    let f coeff dim =
	      if dim != tdim then begin match qq_of_coeff coeff with
	      | Some c ->
		let coeff = T.const (QQ.negate (QQ.div c tcoeff)) in
		let term = T.mul (T.var (Env.var_of_dim x.env dim)) coeff in
		t := T.add (!t) term
	      | None -> ()
	      end
	    in
	    Linexpr0.iter f linexpr;
	    !t
	  in
	  symbounds := (pred, symbound)::(!symbounds)
	end
      | None -> ()
    done;
    !symbounds

  (****************************************************************************)
  (* Quantifier elimination                                                   *)
  (****************************************************************************)

  let qe_lme p phi =
    let man = Polka.manager_alloc_strict () in
    let env = mk_env phi in
    Log.logf Log.info "Quantifier elimination [dim: %d, target: %d]"
      (D.Env.dimension env)
      (D.Env.dimension (D.Env.filter p env));
    let join psi disjunct =
      Log.logf Log.info "Polytope projection [sides: %d]"
	(nb_atoms disjunct);
      let projection =
	Log.time "Polytope projection"
	  (fun () -> of_abstract (D.exists man p (to_apron man env disjunct)))
	  ()
      in
      Log.logf Log.info "Projected polytope sides: %d"
	(nb_atoms projection);
      disj psi projection
    in
    lazy_dnf ~join:join ~bottom:bottom ~top:top to_smt phi

  let qe_z3 p phi =
    let fv = formula_free_vars phi in
    let vars = BatList.of_enum (BatEnum.filter (not % p) (VarSet.enum fv)) in
    let ctx = Smt.get_context () in
    let solve = Z3.mk_tactic ctx "qe" in
    let simpl = Z3.mk_tactic ctx "simplify" in
    let qe = Z3.tactic_and_then ctx solve simpl in
    let g = Z3.mk_goal ctx false false false in
    Z3.goal_assert ctx g
      (Z3.mk_exists_const
	 ctx
	 (List.length vars)
	 (Array.of_list (List.map (Z3.to_app ctx % T.V.to_smt) vars))
	 [||]
	 (to_smt phi));
    of_apply_result (Z3.tactic_apply ctx qe g)

  let opt_qe_strategy = ref qe_lme
  let exists p phi = (!opt_qe_strategy) p phi
  let exists_list vars phi =
    let vars = List.fold_left (flip VarSet.add) VarSet.empty vars in
    exists (not % (flip VarSet.mem vars)) phi

  module V = T.V
  module A = Linear.AffineVar(V)
  module AMap = BatMap.Make(A)
  module AffineTerm = Linear.Expr.Make(A)(QQ)

  let linterm_smt = AffineTerm.to_smt A.to_smt Smt.const_qq

  (** Extract a basis for the smallest linear manifold which contains [phi]
      and is defined over the variables [vars]. *)
  let extract_equalities_impl s vars =
    let zero = Smt.const_int 0 in
    let const_var = A.to_smt AConst in
    let space = new Smt.solver in (* solution space for implied equalities *)
    let extract_linterm m =
      let f term v =
	AffineTerm.add_term
	  (AVar v)
	  (m#eval_qq (V.to_smt v))
	  term
      in
      let const_term =
	AffineTerm.add_term
	  AConst
	  (m#eval_qq const_var)
	  AffineTerm.zero
      in
      List.fold_left f const_term vars
    in
    let rec go equalities =
      match space#check () with
      | Smt.Unsat | Smt.Undef -> equalities
      | Smt.Sat ->
	let candidate = extract_linterm (space#get_model ()) in
	let candidate_formula =
	  Smt.mk_eq (linterm_smt candidate) zero
	in
	s#push ();
	s#assrt (Smt.mk_not candidate_formula);
	match s#check () with
	| Smt.Undef -> (* give up; return the equalities we have so far *)
	  equalities
	| Smt.Unsat -> (* candidate equality is implied by phi *)
	  s#pop ();
	  let leading =
	    let e =
	      let f = function
		| (AConst, _) -> None
		| (AVar x, y) -> Some (x,y)
	      in
	      BatEnum.filter_map f (AffineTerm.enum candidate)
	    in
	    match BatEnum.get e with
	    | Some (dim, coeff) ->
	      assert (not (QQ.equal coeff QQ.zero));
	      dim
	    | None -> assert false
	  in
	  space#assrt (Smt.mk_eq (V.to_smt leading) zero);
	  go (candidate::equalities)
	| Smt.Sat -> (* candidate equality is not implied by phi *)
	  let point = extract_linterm (s#get_model ()) in
	  s#pop ();
	  space#assrt (Smt.mk_eq (linterm_smt point) zero);
	  go equalities
    in
    let nonzero v = Smt.mk_not (Smt.mk_eq (V.to_smt v) zero) in
    s#assrt (Smt.mk_eq const_var (Smt.const_int 1));
    space#assrt (Smt.big_disj (BatEnum.map nonzero (BatList.enum vars)));
    go []

  module TMap = Putil.Map.Make(T)
  let nonlinear_equalities map phi vars =
    let s = new Smt.solver in

    let ctx = Smt.get_context () in
    let uninterp_mul_sym = Smt.mk_string_symbol "uninterp_mul" in
    let uninterp_div_sym = Smt.mk_string_symbol "uninterp_div" in
    let real_sort = Smt.mk_real_sort () in
    let uninterp_mul_decl =
      Z3.mk_func_decl ctx uninterp_mul_sym [| real_sort; real_sort |] real_sort
    in
    let uninterp_div_decl = 
      Z3.mk_func_decl ctx uninterp_div_sym [| real_sort; real_sort |] real_sort
    in
    let mk_mul x y = Z3.mk_app ctx uninterp_mul_decl [| x; y |] in
    let mk_div x y = Z3.mk_app ctx uninterp_div_decl [| x; y |] in
    let talg = function
      | OVar v -> (V.to_smt v, V.typ v)
      | OConst k ->
	begin match QQ.to_zz k with
	| Some z -> (Smt.const_zz z, TyInt)
	| None   -> (Smt.const_qq k, TyReal)
	end
      | OAdd ((x,x_typ),(y,y_typ)) -> (Smt.add x y, join_typ x_typ y_typ)
      | OMul ((x,x_typ),(y,y_typ)) -> (mk_mul x y, TyReal)
      | ODiv ((x,x_typ),(y,y_typ)) ->
	let x = match x_typ with
	  | TyReal -> x
	  | TyInt  -> (Smt.mk_int2real x)
	in
	let y = match y_typ with
	  | TyReal -> y
	  | TyInt  -> (Smt.mk_int2real y)
	in
	(mk_div x y, TyReal)
      | OFloor (x, _)   -> (Smt.mk_real2int x, TyInt)
    in
    let assert_nl_eq nl_term var =
      s#assrt (Smt.mk_eq (fst (T.eval talg nl_term)) (V.to_smt var))
    in
    s#assrt (to_smt phi);
    TMap.iter assert_nl_eq map;
    extract_equalities_impl s (VarSet.elements vars)

  (* Split a formula into a linear formula and a set of nonlinear equations by
     introducing a variable (and equation) for every nonlinear term. *)
  let split_linear mk_tmp phi =
    let nonlinear = ref TMap.empty in
    let replace_term t =
      if T.is_linear t then t else begin
	let (lin, nonlin) = T.split_linear t in
	let mk_nl_term (t, coeff) =
	  let var =
	    try TMap.find t (!nonlinear)
	    with Not_found ->
	      let v = mk_tmp () in
	      nonlinear := TMap.add t v (!nonlinear);
	      v
	  in
	  T.mul (T.var var) (T.const coeff)
	in
	BatEnum.fold T.add lin (BatList.enum nonlin /@ mk_nl_term)
      end
    in
    let alg = function
      | OOr (phi, psi) -> disj phi psi
      | OAnd (phi, psi) -> conj phi psi
      | OAtom (LeqZ t) -> leqz (replace_term t)
      | OAtom (LtZ t) -> ltz (replace_term t)
      | OAtom (EqZ t) -> eqz (replace_term t)
    in
    (eval alg phi, !nonlinear)

  (* Linearize, but avoid converting to DNF.  Not necessarily faster than
     converting to DNF, because this linearization strategy requires
     abstracting the formula by a convex polytope (which may be just as bad -
     or worse - than converting to DNF). *)
  let linearize_nodnf mk_tmp phi =
    let open D in
    let (lin_phi, nonlinear) = split_linear mk_tmp phi in
    let vars =
      BatEnum.fold
	(fun set (_,v) -> VarSet.add v set)
	(formula_free_vars phi)
	(TMap.enum nonlinear)
    in
    let lin_phi =
      let f phi eq =
	let g (v, coeff) =
	  match v with
	  | AVar v -> T.mul (T.var v) (T.const coeff)
	  | AConst -> T.const coeff
	in
	conj phi (eqz (BatEnum.reduce T.add (AffineTerm.enum eq /@ g)))
      in
      let eqs = nonlinear_equalities nonlinear lin_phi vars in
      List.fold_left f lin_phi eqs
    in
    let man = Polka.manager_alloc_loose () in
    let approx = D.add_vars (VarSet.enum vars) (abstract man lin_phi) in
    let mk_nl_equation (term, var) =
      Tcons0.make (T.to_apron approx.env (T.sub term (T.var var))) Tcons0.EQ
    in
    let nl_eq =
      BatArray.of_enum (TMap.enum nonlinear /@ mk_nl_equation)
    in
    let nl_approx =
      { prop = Abstract0.meet_tcons_array man approx.prop nl_eq;
	env = approx.env }
    in
    conj lin_phi (of_abstract nl_approx)

  (* Linearize & convert to DNF.  More accurate than [linearize_nodnf], since
     linearization is done in each disjunct separately.  *)
  let linearize mk_tmp phi =
    let (lin_phi, nonlinear) = split_linear mk_tmp phi in
    if TMap.is_empty nonlinear then phi else begin
      let vars =
	BatEnum.fold
	  (fun set (_,v) -> VarSet.add v set)
	  (formula_free_vars phi)
	  (TMap.enum nonlinear)
      in
      let env = D.Env.of_enum (VarSet.enum vars) in
      Log.logf Log.info "Linearize formula (%d dimensions, %d nonlinear)"
	(D.Env.dimension env)
	(TMap.cardinal nonlinear);
      let lin_phi =
	let f phi eq =
	  let g (v, coeff) =
	    match v with
	    | AVar v -> T.mul (T.var v) (T.const coeff)
	    | AConst -> T.const coeff
	  in
	  conj phi (eqz (BatEnum.reduce T.add (AffineTerm.enum eq /@ g)))
	in
	let eqs = nonlinear_equalities nonlinear lin_phi vars in
	List.fold_left f lin_phi eqs
      in
      let man = Polka.manager_alloc_loose () in

      let join psi disjunct =
	(* todo: compute & strengthen w/ nl equalities here *)
	disjunct::psi
      in
      let to_smt = to_smt % (BatList.fold_left disj bottom) in

      let dnf = lazy_dnf ~join:join ~top:[top] ~bottom:[] to_smt lin_phi in
      let mk_nl_equation (term, var) =
	Tcons0.make (T.to_apron env (T.sub term (T.var var))) Tcons0.EQ
      in
      let nl_eq =
	BatArray.of_enum (TMap.enum nonlinear /@ mk_nl_equation)
      in
      let add_nl psi =
	let approx = to_apron man env psi in
	let open D in
	let nl_approx =
	  { prop = Abstract0.meet_tcons_array man approx.prop nl_eq;
	    env = approx.env }
	in
	of_abstract nl_approx
      in
      List.fold_left disj bottom (List.map add_nl dnf)
    end

  let disj_optimize terms phi =
    let man = Polka.manager_alloc_loose () in
    let vars =
      let f vars t = VarSet.union (term_free_vars t) vars in
      List.fold_left f (formula_free_vars phi) terms
    in
    let env = D.Env.of_enum (VarSet.enum vars) in
    Log.logf
      Log.info
      "Disjunctive symbolic optimization [objectives: %d, dimensions: %d]"
      (List.length terms)
      (D.Env.dimension env);
    let get_bounds prop t =
      let open D in
      let ivl =
	Log.time "bound_texpr"
	  (Abstract0.bound_texpr man prop.prop) (T.to_apron prop.env t)
      in
      let cvt scalar =
	if Scalar.is_infty scalar == 0 then Some (NumDomain.qq_of_scalar scalar)
	else None
      in
      (cvt ivl.Interval.inf, cvt ivl.Interval.sup)
    in
    let join bounds disjunct =
      let prop = to_apron man env disjunct in
      let new_bounds = List.map (get_bounds prop) terms in
      new_bounds::bounds
    in
    let prop_to_smt bounds =
      let to_formula (t, (lower, upper)) =
	let lo = match lower with
	  | Some b -> leq (T.const b) t
	  | None -> top
	in
	let hi = match upper with
	  | Some b -> leq t (T.const b)
	  | None -> top
	in
	conj lo hi
      in
      let disjunct_smt disjunct =
	let e =
	  BatEnum.combine (BatList.enum terms, BatList.enum disjunct)
	in
	to_smt (BatEnum.fold conj top (e /@ to_formula))
      in
      Smt.big_disj ((BatList.enum bounds) /@ disjunct_smt)
    in
    let top = [List.map (fun _ -> (None, None)) terms] in
    let bottom = [] in
    lazy_dnf ~join:join ~top:top ~bottom:bottom prop_to_smt phi

  (* Given a list of (linear) terms [terms] and a formula [phi], find lower
     and upper bounds for each term within the feasible region of [phi]. *)
  let symbolic_abstract terms phi =
    let man = Polka.manager_alloc_loose () in
    let vars =
      let f vars t = VarSet.union (term_free_vars t) vars in
      List.fold_left f (formula_free_vars phi) terms
    in
    let env = D.Env.of_enum (VarSet.enum vars) in
    Log.logf Log.info "Symbolic optimization [objectives: %d, dimensions: %d]"
      (List.length terms)
      (D.Env.dimension env);
    let get_bounds prop t =
      let open D in
      let ivl =
	Log.time "bound_texpr"
	  (Abstract0.bound_texpr man prop.prop) (T.to_apron prop.env t)
      in
      let cvt scalar =
	if Scalar.is_infty scalar == 0 then Some (NumDomain.qq_of_scalar scalar)
	else None
      in
      (cvt ivl.Interval.inf, cvt ivl.Interval.sup)
    in
    let join bounds disjunct =
      let prop = to_apron man env disjunct in
      let new_bounds = List.map (get_bounds prop) terms in
      let f (lo0, hi0) (lo1, hi1) =
	let lo = match lo0, lo1 with
	  | (Some lo0, Some lo1) -> Some (if QQ.leq lo0 lo1 then lo0 else lo1)
	  | (None, _) | (_, None) -> None
	in
	let hi = match hi0, hi1 with
	  | (Some hi0, Some hi1) -> Some (if QQ.geq hi0 hi1 then hi0 else hi1)
	  | (None, _) | (_, None) -> None
	in
	(lo, hi)
      in
      BatList.map2 f bounds new_bounds
    in
    let prop_to_smt bounds =
      let to_formula (t, (lower, upper)) =
	let lo = match lower with
	  | Some b -> leq (T.const b) t
	  | None -> top
	in
	let hi = match upper with
	  | Some b -> leq t (T.const b)
	  | None -> top
	in
	conj lo hi
      in
      let e =
	BatEnum.combine (BatList.enum terms, BatList.enum bounds)
      in
      to_smt (BatEnum.fold conj top (e /@ to_formula))
    in
    let top = List.map (fun _ -> (None, None)) terms in
    let bottom = List.map (fun _ -> (Some QQ.one, Some QQ.zero)) terms in
    lazy_dnf ~join:join ~top:top ~bottom:bottom prop_to_smt phi

  let symbolic_abstract ts phi =
    Log.time "symbolic_abstract" (symbolic_abstract ts) phi

  let linearize mk_tmp phi =
    Log.time "linearize" (linearize mk_tmp) phi

  let symbolic_bounds p phi term =
    Log.time "symbolic_bounds" (symbolic_bounds p phi) term

  exception Unbounded (* Internal to qe_cover *)
  let qe_cover p phi =
    let fv = formula_free_vars phi in
    let vars = BatList.of_enum (BatEnum.filter (not % p) (VarSet.enum fv)) in
    let to_linterm t =
      match T.to_linterm t with
      | Some lt -> lt
      | None -> raise Nonlinear
    in
    let box = symbolic_abstract (List.map T.var vars) phi in
    let bounds =
      List.fold_left2
	(fun m v box -> T.V.Map.add v box m)
	T.V.Map.empty
	vars
	box
    in
    let lower = function
      | (None, _) -> raise Unbounded
      | (Some x, _) -> x
    in
    let upper = function
      | (_, None) -> raise Unbounded
      | (_, Some x) -> x
    in
    let f (var, coeff) =
      try
	let k = 
	  if QQ.lt coeff QQ.zero
	  then upper (T.V.Map.find var bounds)
	  else lower (T.V.Map.find var bounds)
	in
	(AConst, QQ.mul coeff k)
      with Not_found -> (AVar var, coeff)
    in
    let rec replace = function
      | EqZ t -> conj (replace (LeqZ t)) (replace (LeqZ (T.neg t)))
      | LtZ t ->
	let lt = to_linterm t in
	begin try
	  ltz
	    (T.of_linterm
	       (T.Linterm.add
		  (T.Linterm.of_enum ((T.Linterm.var_bindings lt) /@ f))
		  (T.Linterm.const (T.Linterm.const_coeff lt))))
	with Unbounded -> top end
      | LeqZ t ->
	let lt = to_linterm t in
	begin try
	  leqz
	    (T.of_linterm
	       (T.Linterm.add
		  (T.Linterm.of_enum ((T.Linterm.var_bindings lt) /@ f))
		  (T.Linterm.const (T.Linterm.const_coeff lt))))
	with Unbounded -> top end
    in
    map replace phi

  let atom_smt = function
    | LeqZ t -> Smt.mk_le (T.to_smt t) (Smt.const_int 0)
    | LtZ t -> Smt.mk_lt (T.to_smt t) (Smt.const_int 0)
    | EqZ t -> Smt.mk_eq (T.to_smt t) (Smt.const_int 0)

  let rec simplify_children star s children =
    let f psi = s#assrt (star (to_smt psi)) in
    let changed = ref false in
    let rec go simplified = function
      | [] -> simplified
      | (phi::phis) ->
	s#push ();
	List.iter f simplified;
	List.iter f phis;
	let simple_phi = simplify_dillig_impl phi s in
	s#pop ();
	if not (equal phi simple_phi) then changed := true;
	go (simple_phi::simplified) phis
    in
    let rec fix children =
      let simplified = go [] children in
      if !changed then begin
	changed := false;
	fix simplified
      end else simplified
    in
    fix (BatList.of_enum (Hset.enum children))

  and simplify_dillig_impl phi s =
    match phi.node with
    | Atom at ->
      s#push ();
      s#assrt (atom_smt at);
      let simplified =
	match s#check () with
	| Smt.Undef -> atom at
	| Smt.Unsat -> bottom
	| Smt.Sat -> 
	  s#pop ();
	  s#push ();
	  s#assrt (Smt.mk_not (atom_smt at));
	  match s#check () with
	  | Smt.Undef -> atom at
	  | Smt.Unsat -> top
	  | Smt.Sat -> atom at
      in
      s#pop ();
      simplified
    | Or xs -> big_disj (BatList.enum (simplify_children Smt.mk_not s xs))
    | And xs -> big_conj (BatList.enum (simplify_children (fun x -> x) s xs))

  let simplify_dillig _ phi = simplify_dillig_impl phi (new Smt.solver)

  let simplify_z3 p phi =
    let fv = formula_free_vars phi in
    let vars = BatList.of_enum (BatEnum.filter (not % p) (VarSet.enum fv)) in
    let ctx = Smt.get_context () in
    let simplify = Z3.mk_tactic ctx "simplify" in
    let solve_eqs = Z3.mk_tactic ctx "solve-eqs" in
    let simplify = Z3.tactic_and_then ctx simplify solve_eqs in
    let g = Z3.mk_goal ctx false false false in

    Z3.goal_assert ctx g
      (Z3.mk_exists_const
	 ctx
	 (List.length vars)
	 (Array.of_list (List.map (Z3.to_app ctx % T.V.to_smt) vars))
	 [||]
	 (to_smt phi));
    of_apply_result (Z3.tactic_apply ctx simplify g)


  let opt_simplify_strategy = ref [qe_cover; simplify_dillig]
  let simplify p phi =
    List.fold_left (fun phi simpl -> simpl p phi) phi (!opt_simplify_strategy)

  module Syntax = struct
    let ( && ) x y = conj x y
    let ( || ) x y = disj x y
    let ( == ) x y = eq x y
    let ( < ) x y = lt x y
    let ( > ) x y = gt x y
    let ( <= ) x y = leq x y
    let ( >= ) x y = geq x y
  end
end

module MakeEq (F : S) = struct
  open F
  module V = T.V
  module A = Linear.AffineVar(V)
  module AMap = BatMap.Make(A)
  module AffineTerm = T.Linterm

  let linterm_smt = AffineTerm.to_smt A.to_smt Smt.const_qq

  (** Extract a basis for the smallest linear manifold which contains [phi]
      and is defined over the variables [vars]. *)
  let extract_equalities phi vars =
    (* todo: this should use extract_equalities_impl from Defaults, but we
       don't want to extract_equalities_impl or AffineTerm to should up in the
       signature for Formula *)
    let zero = Smt.const_int 0 in
    let const_var = A.to_smt AConst in
    let s = new Smt.solver in
    let space = new Smt.solver in (* solution space for implied equalities *)
    let extract_linterm m =
      let f term v =
	AffineTerm.add_term
	  (AVar v)
	  (m#eval_qq (V.to_smt v))
	  term
      in
      let const_term =
	AffineTerm.add_term
	  AConst
	  (m#eval_qq const_var)
	  AffineTerm.zero
      in
      List.fold_left f const_term vars
    in
    let rec go equalities =
      match space#check () with
      | Smt.Unsat | Smt.Undef -> equalities
      | Smt.Sat ->
	let candidate = extract_linterm (space#get_model ()) in
	let candidate_formula =
	  Smt.mk_eq (linterm_smt candidate) zero
	in
	s#push ();
	s#assrt (Smt.mk_not candidate_formula);
	match s#check () with
	| Smt.Undef -> (* give up; return the equalities we have so far *)
	  equalities
	| Smt.Unsat -> (* candidate equality is implied by phi *)
	  s#pop ();
	  let leading =
	    let e =
	      let f = function
		| (AConst, _) -> None
		| (AVar x, y) -> Some (x,y)
	      in
	      BatEnum.filter_map f (AffineTerm.enum candidate)
	    in
	    match BatEnum.get e with
	    | Some (dim, coeff) ->
	      assert (not (QQ.equal coeff QQ.zero));
	      dim
	    | None -> assert false
	  in
	  space#assrt (Smt.mk_eq (V.to_smt leading) zero);
	  go (candidate::equalities)
	| Smt.Sat -> (* candidate equality is not implied by phi *)
	  let point = extract_linterm (s#get_model ()) in
	  s#pop ();
	  space#assrt (Smt.mk_eq (linterm_smt point) zero);
	  go equalities
    in
    let nonzero v = Smt.mk_not (Smt.mk_eq (V.to_smt v) zero) in
    s#assrt (to_smt phi);
    s#assrt (Smt.mk_eq const_var (Smt.const_int 1));
    space#assrt (Smt.big_disj (BatEnum.map nonzero (BatList.enum vars)));
    go []
end