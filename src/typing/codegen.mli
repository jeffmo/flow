(* TODO: Hide this type *)
type t = {
  applied_tparams: Type.t list;
  buf: Buffer.t;
  class_names: string IMap.t;
  mutable next_class_name: int;
  flow_cx: Context.t;
  tparams: Type.typeparam list;
  type_names: string Type.TypeMap.t;
}

(* Utils *)
val add_applied_tparams: Type.t list -> t -> t
val add_class_name: int -> string -> t -> t
val add_str: string -> t -> t
val add_tparams: Type.typeparam list -> t -> t
val add_type_name: Type.t -> string -> t -> t
val find_props: int -> t -> Type.t SMap.t
val find_recursive_types:
  ?seen:Type.TypeSet.t ->
  ?rec_types:Type.TypeSet.t ->
  Type.t ->
  t ->
  (Type.TypeSet.t * Type.TypeSet.t)
val has_class_name: int -> t -> bool
val mk_env: Context.t -> t
val next_class_name: t -> string
val resolve_type: Type.t -> t -> Type.t
val to_string: t -> string

(* Code generators *)
val gen_func_params: string list option -> Type.t list -> t -> t
val gen_if: bool -> (t -> t) -> t -> t
val gen_intersection_list: Type.InterRep.t -> t -> t
val gen_named_type: Type.t -> t -> t
val gen_separated_list: 'a list -> string -> ('a -> t -> t) -> t -> t
val gen_tparams_list: t -> t
val gen_type: Type.t -> t -> t
val gen_union_list: Type.UnionRep.t -> t -> t
