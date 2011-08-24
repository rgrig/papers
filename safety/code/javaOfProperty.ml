(* Reads a TOPL properties and generates Java code that instantiates the
  corresponding ASTs. *)

open Format

module A = Ast
module PA = Ast.PropertyAst
module U = Util

(* Things that are integers in Java (for efficiency). *)
let vertex = Hashtbl.create 13
let variable = Hashtbl.create 13
let tag = Hashtbl.create 13

let to_ints h xs =
  let c = ref (-1) in
  Hashtbl.clear h;
  let f x = if not (Hashtbl.mem h x) then (incr c; Hashtbl.add h x !c) in
  List.iter f xs

let get_vertices p =
  let f acc {PA.edge_source=s;PA.edge_target=t;PA.edge_labels=_} =
    s :: t :: acc in
  "start" :: "error" :: List.fold_left f [] p.PA.edges

let rec atomics_of_guard = function
  | PA.Atomic a -> [a]
  | PA.Not g -> atomics_of_guard g
  | PA.And gs | PA.Or gs -> List.concat (List.map atomics_of_guard gs)

let get_atomics p =
  let gs = PA.guards_of_automaton p in
  List.concat (List.map atomics_of_guard gs)

let get_variables p =
  let f = function PA.Var (v, _) -> Some v | _ -> None in
  U.map_option f (get_atomics p)

let get_tags p =
  let f = function PA.Event t -> Some t | _ -> None in
  U.map_option f (get_atomics p)

type adjacency_list = ((PA.label list * int) list) list

let transform_graph : PA.edge list -> adjacency_list = fun es ->
  let a = Array.make (Hashtbl.length vertex) [] in
  let f {PA.edge_source=s;PA.edge_target=t;PA.edge_labels=ls} =
    let s = Hashtbl.find vertex s in
    let t = Hashtbl.find vertex t in
    a.(s) <- (ls, t) :: a.(s) in
  List.iter f es;
  Array.to_list a

let transform_guard g =
  let g = PA.dnf g in
  let split (t, gs) = function
    | PA.Not (PA.Atomic (PA.Event _)) -> failwith "Can't express in Java (1)"
    | PA.Atomic (PA.Event t') -> assert (t = None); Some t', gs
    | g -> t, g :: gs in
  let g = List.map (List.fold_left split (None, [])) g in
  let t, gs = List.split g in
  let t = U.map_option (fun x -> x) t in
  let gs = PA.simplify gs in
  match gs with
    | _ :: _ :: _ -> failwith "Can't express in Java (2)"
    | [] -> [], PA.Not (PA.Atomic PA.Any)
    | [gs] -> List.map (Hashtbl.find tag) t, PA.And gs

let id = U.fresh_id ()

let rec pp_list pe ppf = function
  | [] -> ()
  | [x] -> fprintf ppf "@\n%a" pe x
  | x :: xs -> fprintf ppf "@\n%a,%a" pe x (pp_list pe) xs

let rec guard ppf = function
  | PA.Atomic (PA.Var (av, ev)) -> fprintf ppf "new Checker.StoreEqualityGuard(%d, %d)" ev (Hashtbl.find variable av)
  | PA.Atomic (PA.Ct (ct, ev)) -> fprintf ppf "new Checker.ConstantEqualityGuard(%d, %d)" ev ct
  | PA.Atomic (PA.Any) -> fprintf ppf "new Checker.TrueGuard()"
  | PA.Atomic (PA.Event _) -> failwith "INTERNAL: should be pruned by transform_guard"
  | PA.Not g -> fprintf ppf "new Checker.NotGuard(%a)" guard g
  | PA.And [] -> guard ppf (PA.Atomic PA.Any)
  | PA.Or [] -> guard ppf (PA.Not (PA.Atomic PA.Any))
  | PA.And [g] | PA.Or [g] -> guard ppf g
  | PA.And gs -> fprintf ppf "@[<2>new Checker.AndGuard(new Checker.Guard[]{%a})@]" (pp_list guard) gs
  | PA.Or gs -> fprintf ppf "@[<2>new Checker.OrGuard(new Checker.Guard[]{%a})@]" (pp_list guard) gs

let assignment ppf (av, ev) =
  fprintf ppf "new Checker.Action.Assignment(%d, %d)"
    (Hashtbl.find variable av) ev

let action ppf xs =
  fprintf ppf "@[<2>new Checker.Action(new Checker.Action.Assignment[]{%a})@]" (pp_list assignment) xs

let label ppf {PA.label_guard=g; PA.label_action=a} =
  let gt, gr = transform_guard g in
  fprintf ppf "@[<2>new Checker.TransitionStep(";
  fprintf ppf   "@\n@[<2>new int[]{%a}@],"
    (pp_list (fun ppf x -> fprintf ppf "%d" x)) gt;
  fprintf ppf   "@\n%a," guard gr;
  fprintf ppf   "@\n%a)@]" action a

let one_outgoing ppf (ls, tgt) =
  fprintf ppf "@[<2>new Checker.Transition(@\n@[<2>new Checker.TransitionStep[]{%a}@],@\n%d)@]"
    (pp_list label) ls
    tgt

let all_outgoing ppf ts =
  fprintf ppf "@[<2>new Checker.Transition[]{%a}@]" (pp_list one_outgoing) ts

let property ppf p =
  to_ints vertex (get_vertices p);
  to_ints variable (get_variables p);
  to_ints tag (get_tags p);
  fprintf ppf "@[<2>";
  fprintf ppf "Checker %s = new Checker(" (id ());
  fprintf ppf   "@\n\"%s\"," p.PA.message;
  fprintf ppf   "@\n@[<2>new Checker.Automaton(%d, %d, new Checker.Transition[][]{%a})@]"
    (Hashtbl.find vertex "start")
    (Hashtbl.find vertex "error")
    (pp_list all_outgoing) (transform_graph p.PA.edges);
  fprintf ppf ");";
  fprintf ppf "@]@\n"
  (* TODO: print [vertex], [variable], [tag] (in comments). *)

let file f =
  try
    printf "// from file %s@\n" f;
    let f = Helper.parse f in
    let ps = List.map (fun x -> x.A.ast) f.A.program_properties in
    List.iter (property std_formatter) ps
  with Helper.Parsing_failed m ->
    eprintf "@[%s@." m

let () =
  printf "@[";
  printf "package topl;@\n";
  printf "@[<2>public class CheckerTests {@\n";
  for i = 1 to Array.length Sys.argv - 1 do
    file Sys.argv.(i)
  done;
  printf "@[<2>public static void main(String[] args) {@\n";
  printf "System.out.println(\"TODO\");@]@\n}";
  printf "@]@\n}@."

(*
vim:tw=0:
*)
