%{
  open Ast
  open Format
  open Util
%}

%token <int> NUMBER
%token <string> ID
%token <string> STRING
%token AND
%token ARROW
%token ASGN
%token CLASS
%token COLON
%token COMMA
%token DO
%token DOT
%token ELSE
%token EOF
%token EQ
%token IF
%token LB
%token LP
%token MAIN
%token NE
%token NEW
%token NOT
%token OR
%token PROPERTY
%token RB
%token RETURN
%token RP
%token STAR
%token VAR
%token WHILE

%left OR AND
%left EQ NE
%nonassoc NOT
%left DOT

%start <Ast.program> program

%%

program:
    h=class_ t=program
      { { t with program_classes = h :: t.program_classes } }
  | h=global t=program
      { { t with program_globals = h :: t.program_globals } }
  | h=main t=program
      { if t.program_main <> None then 
          eprintf "WARNING: Only the last main matters.";
        { t with program_main = Some h } }
  | property t=program
      { eprintf "DBG: skip a property"; t } 
  | EOF
      { { program_classes = []
        ; program_globals = []
        ; program_main = None
        ; program_properties = []  } }

%public with_line(X):
    x=X { { ast = x; line = $startpos.Lexing.pos_lnum } }

%%

