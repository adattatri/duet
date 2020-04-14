open Syntax

(* compute integer hull of a polyhedron *)
val perform_integer_hull : Polyhedron.t -> Polyhedron.t

val abstractInt : ?exists:(symbol -> bool) ->
  'a context ->
  'abs Apron.Manager.t ->
  'a formula ->
  ('a,'abs) SrkApron.property