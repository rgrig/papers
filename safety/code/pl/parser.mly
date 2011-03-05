(* TODO
  - globals
 *)

%{
  open Format
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
%token NE
%token NEW
%token NOT
%token OR
%token RB
%token RETURN
%token RP
%token THIS
%token WHILE

%left EQ NE
%left OR AND
%nonassoc NOT
%left DOT

%start <unit> program

%%

program: global* EOF {}
global:
    CLASS ID LB member* RB {}
  | type_id {}
member:
    type_id {}
  | type_id LP separated_list(COMMA, type_id) RP body? {}
type_id: ID ID {}
body: LB statement* RB {}
statement:
    RETURN ref_ {}
  | ref_ ASGN expression {}
  | ref_ ASGN expression DOT ID args {}
  | ref_ ASGN NEW ID {}
  | ref_ DOT ID args {} (* sugar *)
  | WHILE body? expression body? {}
  | IF expression body else_? {}
expression:
    atom {}
  | expression op expression {}
  | expression DOT ID {}
  | NOT expression {}
atom:
    LP expression RP {}
  | ref_ {}
%inline op: EQ {} | NE {} | OR {} | AND {}
args: LP separated_list(COMMA, ref_) RP {}
ref_: ID {} | THIS {}
else_: ELSE body {} (* sugar *)

%%
