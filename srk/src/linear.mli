open Syntax

(** Various operations for the vector space [int -> QQ] *)

(** Raised for unsolvable systems of linear equations *)
exception No_solution

exception Nonlinear

module ZZVector : sig
  include Ring.Vector with type dim = int and type scalar = ZZ.t
  val compare : t -> t -> int
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val hash : t -> int
end

module QQVector : sig
  include Ring.Vector with type dim = int and type scalar = QQ.t
  val compare : t -> t -> int
  val pp : Format.formatter -> t -> unit
  val show : t -> string
  val hash : t -> int
end

module QQMatrix : sig
  include Ring.Matrix with type scalar = QQ.t
                       and type vector = QQVector.t

  val pp : Format.formatter -> t -> unit
  val show : t -> string

  (** Compute a list rational eigenvalues of a matrix, along with their
      algebraic multiplicities. *)
  val rational_eigenvalues : t -> (QQ.t * int) list
end

(** [nullspace mat dimensions] computes a basis for the vector space [{ x :
    max*x = 0}], projected on to the set of dimensions [dimensions].  (Note
    that the nullspace is not finitely generated in [int -> QQ], hence the
    projection). *)
val nullspace : QQMatrix.t -> int list -> QQVector.t list

(** [solve_exn mat b] computes a rational vector [x] such that [mat*x =
    b]. Raises [No_solution] if there is no solution. *)
val solve_exn : QQMatrix.t -> QQVector.t -> QQVector.t

(** [solve mat b] computes a rational vector [x] such that [mat*x = b], if
    such a vector exists.  Otherwise, return [None]. *)
val solve : QQMatrix.t -> QQVector.t -> QQVector.t option

(** Given a predicate on dimensions and a list of terms (all implicitly equal
    to zero), orient the equations as rewrite rules that eliminate dimensions
    that don't satisfy the predicate. *)
val orient : (int -> bool) -> QQVector.t list -> (int * QQVector.t) list

(** [vector_right_mul m v] computes [m*v] *)
val vector_right_mul : QQMatrix.t -> QQVector.t -> QQVector.t

(** [vector_left_mul v m] computes [(v^t)*m] *)
val vector_left_mul : QQVector.t -> QQMatrix.t -> QQVector.t

(** Given two matrices [A] and [B], compute matrices [C] and [D] such that [CA
    = DB] is a basis for the intersection of the rowspaces of [A] and [B]. *)
val intersect_rowspace : QQMatrix.t -> QQMatrix.t -> (QQMatrix.t * QQMatrix.t)

(** Given two matrices A and B, compute a matrix C such that CB = A (if one
    exists).  C exists when the rowspace of B is contained in the rowspace of
    A.  If A and B are invertible, then C is exactly AB{^-1}. *)
val divide_right : QQMatrix.t -> QQMatrix.t -> QQMatrix.t option

(** Given matrices [A] and [B], find a matrix [C] whose rows constitute a
    basis for the vector space [{ v : exists u. uA = vB }] *)
val max_rowspace_projection : QQMatrix.t -> QQMatrix.t -> QQMatrix.t

(** Given matrices [A] and [B] representing a system of equations [Ax' = Bx],
    find a matrix [T] and a square matrix [M] such that [y' = My] is the
    greatest linear dynamical system that approximates [Ax' = Bx], and [T] is
    the linear transformation into the linear dynamical system.  That is, [TB
    = MTA], and the rowspace of [TA] is maximal. *)
val max_lds : QQMatrix.t -> QQMatrix.t -> QQMatrix.t * QQMatrix.t

(** Given a matrix [A], find a pair of matrices [(M,T)] such that [MA = TM],
    [T] is lower-triangular, and the rowspace of [MA] is maximal. *)
val rational_triangulation : QQMatrix.t -> (QQMatrix.t * QQMatrix.t)

val rational_spectral_decomposition : QQMatrix.t -> (QQ.t * QQVector.t) list
val periodic_rational_spectral_decomposition : QQMatrix.t -> (int * QQ.t * QQVector.t) list

val evaluate_affine : (int -> QQ.t) -> QQVector.t -> QQ.t

(** {2 Affine terms} *)

(** Various operations for manipulating affine terms over symbols, represented
    as rational vectors *)

(** Map a symbol to a dimension.  The following equations hold:
    - [sym_of_dim (dim_of_sym sym) = Some sym]
    - [sym_of_dim const_dim = None] *)
val sym_of_dim : int -> symbol option

(** Map a dimension to symbol.  The following equations hold:
    - [sym_of_dim (dim_of_sym sym) = Some sym]
    - [sym_of_dim const_dim = None] *)
val dim_of_sym : symbol -> int

(** Dimension for representing the coefficient of the constant 1. *)
val const_dim : int

(** Representation of a rational number as an affine term.  The equation
    [const_of_linterm (const_linterm qq) = Some qq] must hold. *)
val const_linterm : QQ.t -> QQVector.t

(** Convert an affine term to a rational number, if possible.  The equation
    [const_of_linterm (const_linterm qq) = Some qq] must hold. *)
val const_of_linterm : QQVector.t -> QQ.t option

(** Convert a rational vector representing an affine term.  Raises [Nonlinear]
    if the input term is non-linear. *)
val linterm_of : 'a context -> 'a term -> QQVector.t

(** Convert a rational vector to an affine term.  The equation [of_linterm srk
    (linterm_of srk t) = t] must hold. *)
val of_linterm : 'a context -> QQVector.t -> 'a term

(** Pretty-print an affine term *)
val pp_linterm : 'a context -> Format.formatter -> QQVector.t -> unit

(** [evaluate_linterm env t] evaluates the affine term t in the environment
    [env] *)
val evaluate_linterm : (symbol -> QQ.t) -> QQVector.t -> QQ.t

(** Count the number of dimensions with non-zero coefficients *)
val linterm_size : QQVector.t -> int
