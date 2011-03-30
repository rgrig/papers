%{
  open Ast
  open Format
  open Util

  let type_of_string = function
    | "Bool" -> Bool
    | "Unit" -> Unit
    | x -> Class x
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
%token STAR
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

with_line(X):
    x=X { { ast = x; line = $startpos.Lexing.pos_lnum } }

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
        ; method_body = 
            match b with 
              | Some b -> b 
              | None -> default_body $endpos.Lexing.pos_lnum } }

type_id: 
    t=ID v=ID 
      { { declaration_type = type_of_string t; declaration_variable = v } }

body: 
    LB b=with_line(statement)* RB
      { let ds = ref [] in (* ugly *)
        let ss = ref [] in
        let add_to_ds x = ds := x :: !ds in
        let add_to_ss line x = ss := { ast = x; line = line } :: !ss in
        let process_line { ast = xs; line = line } =
          List.iter (either add_to_ds (add_to_ss line)) xs in
        List.iter process_line b;
        Body (List.rev !ds, List.rev !ss) }

statement:
    RETURN r=expression
      { [Right(Return r)] }
  | VAR d=type_id
      { [Left d] }
  | l=lhs ASGN NEW
      { fst l @ [Right(mk_allocate (snd l))] }
  | l=lhs ASGN e=expression
      { fst l @ 
        [Right(Assignment(snd l, e))] }
  | l=lhs ASGN r=expression DOT m=ID a=args
      { fst l @ [Right(mk_call (Some(snd l)) r m a)] }
  | r=ref_ DOT m=ID a=args (* if lhs may start with (, then grammar would be ambiguous *)
      { [ Right(mk_call None (Ref r) m a) ] }
  | WHILE pre=body? c=expression post=body?
      { [Right(While
        { while_pre_body=from_option empty_body pre
        ; while_condition=c
        ; while_post_body=from_option empty_body post})] }
  | IF c=expression i=body e=else_?
      { [Right(If(c,i))] 
        @ (match e with None -> [] | Some e -> [Right(If(Not c, e))]) }

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
  | STAR
      { Literal None }

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
