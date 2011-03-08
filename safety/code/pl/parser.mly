%{
  open Ast
  open Format

  type ('a, 'b) either = Left of 'a | Right of 'b
  let either a b = function Left x -> a x | Right x -> b x

  let from_option a = function None -> a | Some a -> a
%}

(* tokens and precedences {{{ *)
%token <string> ID
%token AND
%token ASGN
%token CLASS
%token COMMA
%token DOT
%token ELSE
%token EQ
%token EOF
%token IF
%token LB
%token LP
%token MAIN
%token NE
%token NEW
%token NOT
%token OR
%token RB
%token RETURN
%token RP
%token THIS
%token VAR
%token WHILE

%left EQ NE
%left OR AND
%nonassoc NOT
%left DOT
(* }}} *)

%start <Ast.program> program

%%

program: 
    a=global* m=main b=global* EOF
      { let f x = [x] in
        let g x = [] in
        let get f g x = List.concat (List.map (either f g) x) in
        { program_classes = get f g a @ get f g b
        ; program_globals = get g f a @ get g f b
        ; program_main = m } }

global:
    CLASS c=ID LB m=member* RB 
      { Left(c, m) }
  | VAR d=type_id 
      { Right d }

main: 
    MAIN b=body 
      { b }

member:
    VAR d=type_id 
      { Field d }
  | d=type_id LP a=separated_list(COMMA, type_id) RP b=body? 
      { Method
        { method_return_type = d.declaration_type
        ; method_name = d.declaration_variable
        ; method_formals = a
        ; method_body = b } }

type_id: 
    t=ID v=ID 
      { { declaration_type = t; declaration_variable = v } }

body: 
    LB b=statement* RB
      { let ds = ref [] in (* ugly *)
        let ss = ref [] in
        let add r x = r := x :: !r in
        List.iter(List.iter(either (add ds) (add ss))) b;
        Body (List.rev !ds, List.rev !ss) }

statement:
    RETURN r=expression
      { [Right(Return r)] }
  | VAR d=type_id
      { [Left d] }
  | l=lhs ASGN NEW
      { fst l @ [Right(Allocate (snd l))] }
  | l=lhs ASGN e=expression
      { fst l @ [Right(Assignment(snd l, e))] }
  | l=lhs ASGN r=expression DOT m=ID a=args
      { fst l @ [Right(Call 
        { call_lhs = Some(snd l)
        ; call_receiver = r
        ; call_method = m
        ; call_arguments = a })] }
  (* TODO allow expr on the left: use indentation info to introduce SEMI *)
  | r=ref_ DOT m=ID a=args (* if lhs may start with (, then grammar would be ambiguous *)
      { [ Right(Call
        { call_lhs = None
        ; call_receiver = Ref r
        ; call_method = m
        ; call_arguments = a })] }
  | WHILE pre=body? c=expression post=body?
      { [Right(While{while_pre_body=from_option empty_body pre; while_condition=c; while_post_body=from_option empty_body post})] }
  | IF c=expression i=body e=else_?
      { [Right(If (c,i))] @ (match e with None -> [] | Some e -> [Right(If (Not c, e))]) }

lhs:
  VAR d=type_id { ([Left d], d.declaration_variable) } (* sugar *)
  | r=ref_ { ([], r) }

expression:
    r=atom 
      { r }
  | l=expression op=binop r=expression
      { Bin (l, op, r) }
  | l=expression op=acop r=expression
      { Ac (op, [l; r]) }
  | r=expression DOT f=ID
      { Deref (r, f) }
  | NOT r=expression
      { Not r }

atom:
    LP r=expression RP
      { r }
  | r=ref_ 
      { Ref r }

%inline binop:
    EQ
      { Eq }
  | NE
      { Ne }

%inline acop: 
    OR 
      { Or }
  | AND
      { And }

args: 
    LP r=separated_list(COMMA, expression) RP 
      { r }

ref_: 
    s=ID
      { s }
  | THIS 
      { "this" }

else_:
    ELSE b=body 
      { b } (* sugar *)

%%
