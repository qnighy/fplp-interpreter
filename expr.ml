(* expr.ml : value and expression *)
open Misc
open Loc

type pattern =
  | PConstInt of int
  | PConstBool of bool
  | PVar of identifier
  | PTuple of pattern list
  | PNil
  | PCons of pattern * pattern

(* expression has its position data *)
type expr = expr_t loc
and expr_t =
  | EConstInt of int
  | EConstBool of bool
  | EVar of identifier
  | EApp of expr * expr
  | EFun of identifier * expr
  | ELet of identifier * expr * expr
  | ERLet of (identifier * expr) list * expr
  | EMatch of expr * (pattern * expr) list
  | ETuple of expr list
  | ENil
  | ECons of expr * expr
  | EAdd of expr * expr
  | ESub of expr * expr
  | EMul of expr * expr
  | EDiv of expr * expr
  | EEq of expr * expr
  | ELt of expr * expr
  | EIf of expr * expr * expr

type command =
  | CExp of expr
  | CLet of identifier * expr
  | CRLet of (identifier * expr) list
  | CEnd

let rec is_list_literal_pattern = function
  | PNil -> true
  | PCons (_, p1) -> is_list_literal_pattern p1
  | _ -> false


let pp_pattern_getlevel p =
  match p with
  | PConstInt _ -> 0
  | PConstBool _ -> 0
  | PVar _ -> 0
  | PTuple _ -> 0
  | PNil -> 0
  | PCons _ -> if is_list_literal_pattern p then 0 else 1

let rec pp_pattern_lvl lvl pf p =
  if pp_pattern_getlevel p > lvl then
    Format.fprintf pf "@[("
  else ();
  begin match p with
  | PConstInt i -> Format.fprintf pf "@[%d@]" i
  | PConstBool b -> Format.fprintf pf "@[%B@]" b
  | PVar x -> Format.fprintf pf "@[%s@]" x
  | PTuple pl ->
      Format.fprintf pf "@[(";
      let b = ref true in
      List.iter (fun p ->
        begin if !b then
          Format.fprintf pf "%a" (pp_pattern_lvl 10) p
        else
          Format.fprintf pf ",@ %a" (pp_pattern_lvl 10) p
        end;
        b := false) pl;
      Format.fprintf pf ")@]"
  | PNil -> Format.fprintf pf "@[[]@]"
  | PCons (p0, p1) ->
      if is_list_literal_pattern p then
        Format.fprintf pf "@[[%a%a]@]"
          (pp_pattern_lvl 10) p0
          pp_pattern_list p1
      else
        Format.fprintf pf "@[%a@ ::@ %a@]"
          (pp_pattern_lvl 0) p0
          (pp_pattern_lvl 1) p1
  end;
  if pp_pattern_getlevel p > lvl then
    Format.fprintf pf ")@]"
  else ()
and pp_pattern_list pf p =
  match p with
  | PNil -> ()
  | PCons (p0, p1) ->
      Format.fprintf pf ";@ %a%a"
        (pp_pattern_lvl 10) p0
        pp_pattern_list p1
  | _ -> assert false

let pp_pattern = pp_pattern_lvl 10
