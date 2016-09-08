(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

open Utils_js
let _ = print_endlinef

(**
 * This file is a general-purpose utility for generating code. It is
 * Context.t-aware, which allows it to resolve and codegen type syntax from
 * types themselves.
 *
 * Example usage:
 *
 *   let code_str =
 *     Codegen.mk_env cx
 *       |> Codegen.add_str "/* Before printed type */\n"
 *       |> Codegen.gen_type NullT.t
 *       |> Codegen.add_str "\n/* After printed type */\n"
 *       |> Codegen.to_string
 *   in
 *   print_endline code_str
 *)

let spf = Printf.sprintf

type t = {
  applied_tparams: Type.t list;
  buf: Buffer.t;
  class_names: string IMap.t;
  mutable next_class_name: int;
  flow_cx: Context.t;
  tparams: Type.typeparam list;
  type_names: string Type.TypeMap.t;
}

let add_applied_tparams applied_tparams env = {env with applied_tparams;}
let add_class_name class_id name env =
  {env with class_names = IMap.add class_id name env.class_names;}
let add_str str env = Buffer.add_string env.buf str; env
let add_tparams tparams env = {env with tparams;}
let add_type_name t name env =
  let type_names = Type.TypeMap.add t name env.type_names in
  {env with type_names;}
let find_props tmap_id env = Flow_js.find_props env.flow_cx tmap_id
let has_class_name class_id env = IMap.mem class_id env.class_names
let next_class_name env =
  let id = env.next_class_name in
  env.next_class_name <- id + 1;
  spf "Class%d" id
let resolve_type t env = Flow_js.resolve_type env.flow_cx t
let to_string env = Buffer.contents env.buf

class recursive_type_finder = object(self)
  inherit [Type.TypeSet.t * Type.TypeSet.t] Type_visitor.t as super

  method! tvar cx (seen, rec_types) r id =
    let t = Type.OpenT (r, id) in
    self#type_ cx (seen, rec_types) (Flow_js.resolve_type cx t)

  method! type_ cx (seen, rec_types) t = Type.(
    let resolve_type = Flow_js.resolve_type cx in
    let t = match t with OpenT _ -> resolve_type t | t -> t in
    let seen_t = TypeSet.mem t seen in
    let seen = TypeSet.add t seen in
    match t with
    | ObjT _ ->
      if seen_t
      then (seen, TypeSet.add t rec_types)
      else super#type_ cx (seen, rec_types) t
    | _ ->
      if seen_t
      then (seen, rec_types)
      else super#type_ cx (seen, rec_types) t
  )
end
let find_recursive_types =
  let visitor = new recursive_type_finder in
  (fun ?seen ?rec_types t env ->
    let empty_set = Type.TypeSet.empty in
    let seen = match seen with None -> empty_set | Some s -> s in
    let rec_types = match rec_types with None -> empty_set | Some s -> s in
    visitor#type_ env.flow_cx (seen, rec_types) t
  )

let mk_env merged_flow_cx = {
  applied_tparams = [];
  buf = Buffer.create 320;
  class_names = IMap.empty;
  flow_cx = merged_flow_cx;
  next_class_name = 0;
  tparams = [];
  type_names = Type.TypeMap.empty;
}

(**
 * Just a helper function to simplify this:
 *
 *   let env =
 *     add_str "first" env
 *       |> add_str "second"
 *       |> add_str "third"
 *   in
 *   let env =
 *     if conditional
 *     then add_str "maybe fourth" env
 *     else env
 *   in
 *   add_str "fifth" env
 *     |> add_str "sixth"
 *     |> add_str "seventh"
 *
 * into this:
 *
 *   add_str "first" env
 *     |> add_str "second"
 *     |> add_str "third"
 *     |> gen_if conditional (add_str "maybe fourth")
 *     |> add_str "fifth"
 *     |> add_str "sixth"
 *     |> add_str "seventh"
 *)
let gen_if conditional gen_fn env = if conditional then gen_fn env else env

(**
 * Given a type which must be a built-in class instance type, trace out the
 * built-in's name and codegen it.
 *
 * NOTE: It would be good to come back to this and find a more general
 *       (less fragile) way of preserving class names alongside instance types.
 *)
let gen_builtin_class_type t env = Type.(
  (* AVERT YOUR EYES *)
  let reason = reason_of_t t in
  let builtin_name = Reason.desc_of_reason reason in

  (**
   * Assert that the builtin name we found does match with the class_id we're
   * backtracking. This is super defensive just because our method of getting
   * a builtin's name is so hacky. Once we make that better, we can be less
   * defensive here.
   *)
  let classid =
    match t with
    | InstanceT (_, _, _, {class_id; _;}) -> class_id
    | t -> failwith (
      spf
        ("Internal error: Expected an InstanceT while looking up a builtin " ^^
         "class name, but got a %s!")
        (string_of_ctor t)
    )
  in

  let builtin_t = Flow_js.get_builtin env.flow_cx builtin_name reason in
  let builtin_classid =
    match resolve_type builtin_t env with
    | ThisClassT(InstanceT(_, _, _, {class_id; _;})) ->
      class_id
    | PolyT(_, ThisClassT(InstanceT(_, _, _, {class_id; _;}))) ->
      class_id
    | builtin_t -> failwith (spf "Unexpected global type: %s" (string_of_ctor builtin_t))
  in

  if builtin_classid = classid
  then add_str builtin_name env
  else failwith (
    "Internal error: Encountered an instance type for a class that " ^
    "has not been defined!"
  )
)

(* Helper to generate a list of items with some separator between. *)
let gen_separated_list list sep gen_fn env =
  let count = List.length list in
  let (env, _) = List.fold_left (fun (env, idx) item ->
    let idx = idx + 1 in
    let env = gen_fn item env in
    ((if idx < count then add_str sep env else env), idx)
  ) (env, 0) list in
  env

(* Generate type syntax for a given type *)
let rec gen_type_rec ?print_named_type ~seen t env = Type.(
  let t = match t with OpenT _ -> resolve_type t env | t -> t in
  match (TypeMap.get t env.type_names, print_named_type) with
  | (Some name, Some t_to_print) when t_to_print <> t ->
    add_str name env |> gen_tparams_list_rec ~seen
  | (Some name, None) ->
    add_str name env |> gen_tparams_list_rec ~seen
  | (Some _, Some _)
  | (None, _) ->
    (*
      if TypeSet.mem t seen then failwith (
        "Attempted to codegen a recursive type without a name!"
      ) else
    *)
  let seen = TypeSet.add t seen in
  match t with
  | AbstractT t ->
      add_str "$Abstract<" env |> gen_type_rec ~seen t |> add_str ">"
  | AnnotT (_, t) -> gen_type_rec ~seen t env
  | AnyFunT _ -> add_str "Function" env
  | AnyObjT _ -> add_str "Object" env
  | AnyT _
  | AnyWithLowerBoundT _
  | AnyWithUpperBoundT _
    -> add_str "any" env
  | ArrT (_, tparam, ts) ->
    (match ts with
    | [] ->
      add_str "Array<" env
        |> gen_type_rec ~seen tparam
        |> add_str ">"
    | _ ->
      let t_count = List.length ts in
      let env = add_str "[" env in
      let (env, _) = List.fold_left (fun (env, idx) t ->
        let env = gen_type_rec ~seen t env in
        let idx = idx + 1 in
        let env =
          if idx < t_count then add_str ", " env else env
        in
        (env, idx)
      ) (env, 0) ts in
      add_str "]" env
    )
  | BoolT (_, Some _) ->
    (* TODO: Consider polarity and print the literal type when appropriate *)
    add_str "boolean" env
  | BoolT (_, None) ->
    add_str "boolean" env
  | BoundT {name; _;} -> add_str name env
  | ClassT t ->
    add_str "Class<" env
      |> gen_type_rec ~seen t
      |> add_str ">"
  | CustomFunT (_, ObjectAssign) -> add_str "Object$Assign" env
  | CustomFunT (_, ObjectGetPrototypeOf) -> add_str "Object$GetPrototypeOf" env
  | CustomFunT (_, PromiseAll) -> add_str "Promise$All" env
  | CustomFunT (_, ReactCreateElement) -> add_str "React$CreateElement" env
  | CustomFunT (_, Merge) -> add_str "$Facebookism$Merge" env
  | CustomFunT (_, MergeDeepInto) -> add_str "$Facebookism$MergeDeepInto" env
  | CustomFunT (_, MergeInto) -> add_str "$Facebookism$MergeInto" env
  | CustomFunT (_, Mixin) -> add_str "$Facebookism$Mixin" env
  | CustomFunT (_, Idx) -> add_str "$Facebookism$Idx" env
  (* TODO: Once predicate types are a little more fleshed out, fill out this
   *       codegen.
   *)
  | OpenPredT (_, _, _, _) -> add_str "mixed /* TODO: OpenPredT */" env
  | DiffT (t1, t2) ->
    add_str "$Diff<" env
      |> gen_type_rec ~seen t1
      |> add_str ", "
      |> gen_type_rec ~seen t2
      |> add_str ">"
  | ExactT (_, t) ->
    add_str "$Exact<" env |> gen_type_rec ~seen t |> add_str ">"
  | FunProtoT _ -> add_str "typeof Function.prototype" env
  | FunProtoApplyT _ -> add_str "typeof Function.prototype.apply" env
  | FunProtoBindT _ -> add_str "typeof Function.prototype.bind" env
  | FunProtoCallT _ -> add_str "typeof Function.prototype.call" env
  | FunT (_, _static, _prototype, {params_tlist; params_names; return_t; _;}) ->
    gen_tparams_list_rec ~seen env
      |> add_str "("
      |> gen_func_params_rec ~seen params_names params_tlist
      |> add_str ") => "
      |> gen_type_rec ~seen return_t
  | InstanceT (_, _static, _super, {class_id; _;}) -> (
    (* TODO: See if we can preserve class names *)
    let env =
      match IMap.get class_id env.class_names with
      | Some name -> add_str name env
      | None -> gen_builtin_class_type t env
    in
    gen_tparams_list_rec ~seen env
  )
  | IntersectionT (_, intersection) -> gen_intersection_list_rec ~seen intersection env
  | KeysT (_, t) -> add_str "$Keys<" env |> gen_type_rec ~seen t |> add_str ">"
  | MaybeT t -> add_str "?" env |> gen_type_rec ~seen t
  | MixedT _ -> add_str "mixed" env
  | NumT (_, Literal _) ->
    (* TODO: Consider polarity and print the literal type when appropriate *)
    add_str "number" env
  | NumT (_, (Truthy|AnyLiteral)) -> add_str "number" env
  | NullT _ -> add_str "null" env
  | ObjT (_, {flags = _; dict_t; props_tmap; proto_t = _;}) -> (
    let env = add_str "{" env in

    (* Generate prop entries *)
    let props = find_props props_tmap env in
    let props = SMap.elements props |> List.sort (fun (k1, _) (k2, _) ->
      Pervasives.compare k1 k2
    ) in
    let env = gen_separated_list props ", " (fun (k, t) env ->
      let (sep, t) =
        match resolve_type t env with
        | OptionalT t -> ("?: ", resolve_type t env)
        | t -> (": ", t)
      in
      add_str k env |> add_str sep |> gen_type_rec ~seen t
    ) env in

    (* Generate potential dict entry *)
    let env =
      match dict_t with
      | Some {dict_name; key; value;} ->
        let key_name = (
          match dict_name with
          | Some n -> n
          | None -> "_"
        ) in
        let key = resolve_type key env in
        let value = resolve_type value env in
        gen_if (List.length props > 0) (add_str ", ") env
          |> add_str "["
          |> add_str key_name
          |> add_str ": "
          |> gen_type_rec ~seen key
          |> add_str "]: "
          |> gen_type_rec ~seen value
      | None -> env
    in

    add_str "}" env
  )
  | OptionalT t -> add_str "void | " env |> gen_type_rec ~seen t
  | OpenT _ -> gen_type_rec ~seen (resolve_type t env) env
  | PolyT (tparams, t) -> gen_type_rec ~seen t (add_tparams tparams env)
  | RestT rest -> gen_type_rec ~seen rest env
  | ShapeT t -> add_str "$Shape<" env |> gen_type_rec ~seen t |> add_str ">"
  | SingletonBoolT (_, v) -> add_str (spf "%b" v) env
  | SingletonNumT (_, (_, v)) -> add_str (spf "%s" v) env
  | SingletonStrT (_, v) -> add_str (spf "%S" v) env
  | StrT (_, Literal _) ->
    (* TODO: Consider polarity and print the literal type when appropriate *)
    add_str "string" env
  | StrT (_, (Truthy|AnyLiteral)) -> add_str "string" env
  | ThisClassT t -> gen_type_rec ~seen t env
  | ThisTypeAppT (t, _, ts) -> add_applied_tparams ts env |> gen_type_rec ~seen t
  | TypeAppT (t, ts) -> add_applied_tparams ts env |> gen_type_rec ~seen t
  | TypeT (_, t) -> gen_type_rec ~seen t env
  | UnionT (_, union) -> gen_union_list_rec ~seen union env
  | VoidT _ -> add_str "void" env

  (**
   * These types can't be expressed in code well so we fail back to `mixed`.
   *
   * TODO: This handling is a little low-fidelity which may not work for all
   *       cases. It works for current needs (best-effort codegen of shadow
   *       files), but at some point it might make sense to offer other kinds of
   *       handling for these types depening on the needs of the API user
   *       (i.e. raise, etc).
   *)
  | ChoiceKitT _
  | EmptyT _
  | EvalT _
  | ExistsT _
  | ExtendsT _
  | IdxWrapper _
  | ModuleT _
  | TaintT _
    -> add_str (spf "mixed /* UNEXPECTED TYPE: %s */" (string_of_ctor t)) env
)

and gen_func_params_rec ~seen params_names params_tlist env =
  let params =
    match params_names with
    | Some params_names ->
      List.rev (List.fold_left2 (fun params name t ->
        (name, t):: params
      ) [] params_names params_tlist)
    | None ->
      List.mapi (fun idx t -> (spf "p%d" idx, t)) params_tlist
  in
  gen_separated_list params ", " Type.(fun (name, t) env ->
    match t with
    | RestT t ->
      add_str "..." env
      |> add_str name
      |> add_str ": Array<"
      |> gen_type_rec ~seen t
      |> add_str ">"
    | OptionalT t ->
      add_str name env
      |> add_str "?: "
      |> gen_type_rec ~seen t
    | t ->
      add_str name env
      |> add_str ": "
      |> gen_type_rec ~seen t
  ) env

and gen_intersection_list_rec ~seen intersection env =
  let members = Type.InterRep.members intersection in
  gen_separated_list members " & " (gen_type_rec ~seen) env

and gen_tparams_list_rec = Type.(
  let gen_tparam ~seen {reason = _; name; bound; polarity; default;} env =
    let bound = resolve_type bound env in
    let env = (
      match polarity with
      | Negative -> add_str "-" env
      | Neutral -> env
      | Positive -> add_str "+" env
    ) in
    let env = add_str name env in
    let env = (
      match bound with
      | MixedT _ -> env
      | bound -> add_str ": " env |> gen_type_rec ~seen bound
    ) in
    let env = (
      match default with
      | Some default -> add_str " = " env |> gen_type_rec ~seen default
      | None -> env
    ) in
    env
  in

  fun ~seen env ->
    let tparams = env.tparams in
    let params_count = List.length tparams in
    let applied_tparams = env.applied_tparams in
    let applied_tparams_count = List.length applied_tparams in
    match (params_count, applied_tparams_count) with
    | (0, 0) -> env
    | (_, 0) ->
      {env with tparams = []; }
        |> add_str "<"
        |> gen_separated_list tparams ", " (gen_tparam ~seen)
        |> add_str ">"
    | _ ->
      {env with tparams = []; applied_tparams = []; }
        |> add_str "<"
        |> gen_separated_list applied_tparams ", " (gen_type_rec ~seen)
        |> add_str ">"
)

and gen_union_list_rec ~seen union env =
  let members = Type.UnionRep.members union in
  gen_separated_list members " | " (gen_type_rec ~seen) env

let empty_seen = Type.TypeSet.empty
let gen_named_type t = gen_type_rec ~seen:empty_seen ~print_named_type:t t
let gen_type t = gen_type_rec ~seen:empty_seen t
let gen_func_params p = gen_func_params_rec ~seen:empty_seen p
let gen_intersection_list i = gen_intersection_list_rec ~seen:empty_seen i
let gen_tparams_list env = gen_tparams_list_rec ~seen:empty_seen env
let gen_union_list u = gen_union_list_rec u ~seen:empty_seen
