module Package = Ocamlorg.Package

type package_info =
  { name : string
  ; constraints : string option
  }

type packages_result =
  { total_packages : int
  ; packages : Package.t list
  }

val starts_with : string -> string -> bool
(** This function compares two strings and returns true if they are equal up to
    capitalization *)

val is_package : string -> string -> bool
(** This function compares two strings and returns true if the first string is
    equal to the beginning of the second string up to capitalization *)

val get_packages_result
  :  int
  -> int
  -> int
  -> string option
  -> Package.t list
  -> packages_result

val get_info : (Package.Name.t * string option) list -> package_info list
(** This function returns the list part Package.Info such as
    Package.Info.dependencies *)

val schema : Dream.request Graphql_lwt.Schema.schema
(** This schema allows to query for opam packages from the opam-repository as
    graphql api data *)