%{
  open Format
%}

%token <string> ID
%token ASGN
%token COMMA
%token DOT
%token ELSE
%token EOF
%token IF
%token LB
%token LP
%token NEW
%token RB
%token RETURN
%token RP
%token THIS
%token WHILE

%start <unit> program

%%

program: class_* EOF {}
class_: ID LB member* RB {}
member:
    type_id {}
  | type_id LP separated_list(COMMA, type_id) RP body? {}
type_id: ID ID {}
body: LB statement* RB {}
statement:
    RETURN ref_ {}
  | ref_ ASGN ref_ {}
  | ref_ ASGN ref_ DOT ID {}
  | ref_ ASGN ref_ DOT ID args {}
  | ref_ ASGN NEW ID {}
  | ref_ DOT ID args {} (* sugar *)
  | WHILE body? condition body? {}
  | IF condition body else_? {}
args: LP separated_list(COMMA, ref_) RP {}
ref_: ID {} | THIS {}
condition: LP ref_ RP {}
else_: ELSE body {}

%%
