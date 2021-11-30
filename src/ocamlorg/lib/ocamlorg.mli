(** Backend for the OCaml.org website. *)

module Opam_user = Opam_user
module Package = Package
module Std = Std

val toplevels_path : Fpath.t
(** The path where the toplevel scripts are located. Delete when they are served
    from a CDN. *)
