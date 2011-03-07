%{
  open Ast
  open Format

  type body_member = S of statement | D of declaration

  let from_option a = function None -> a | Some a -> a
%}

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

%start <unit> program

%%

program: 
    global* main global* EOF
      { failwith "TODO" }

global:
    CLASS ID LB member* RB 
  | VAR type_id 
      { failwith "todo" }

main: 
    MAIN body 
      { failwith "todo" }

member:
    VAR d=type_id 
      { Field d }
  | type_id LP separated_list(COMMA, type_id) RP body? 
      { failwith "todo" }

type_id: 
    t=ID v=ID 
      { { declaration_type = t; declaration_variable = v } }

body: 
    LB b=statement* RB
      { let ds = ref [] in (* ugly *)
        let ss = ref [] in
        List.iter(List.iter(function S x->ss:=x::!ss | D x->ds:=x::!ds)) b;
        Body (List.rev !ds, List.rev !ss) }

statement:
    RETURN r=ref_
      { [S(Return r)] }
  | VAR d=type_id
      { [D d] }
  | l=lhs ASGN e=expression
      { fst l @ [S(Assignment (snd l, e))] }
  | l=lhs ASGN r=expression DOT m=ID a=args
      { fst l @ [S(Call {call_lhs=snd l; call_receiver=r; call_method=m; call_arguments=a})] }
  | l=lhs ASGN NEW
      { fst l @ [S(Allocate (snd l))] }
  | lhs DOT ID args (* sugar *)
      { failwith "todo" }
  | WHILE pre=body? c=expression post=body?
      { [S(While{while_pre_body=from_option empty_body pre; while_condition=c; while_post_body=from_option empty_body post})] }
  | IF c=expression i=body e=else_?
      { [S(If (c,i))] @ (match e with None -> [] | Some e -> [S(If (Not c, e))]) }

lhs:
  VAR d=type_id { ([D d], d.declaration_variable) } (* sugar *)
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
