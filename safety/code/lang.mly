%{
  let type_of_string = function
    | "Bool" -> Bool
    | "Unit" -> Unit
    | x -> Class x
%}

%%

%public class_:
    CLASS c=ID LB m=member* RB
      { (c, m) }

%public global:
    VAR d=type_id 
      { d }

%public main:
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
  | r=ID DOT m=ID a=args (* if lhs may start with (, then grammar would be ambiguous *)
      { [ Right(mk_call None (Ref r) m a) ] }
  | pre=do_part? WHILE c=expression post=body?
      { [Right(While
        { while_pre_body=from_option empty_body pre
        ; while_condition=c
        ; while_post_body=from_option empty_body post})] }
  | IF c=expression i=body e=else_?
      { [Right(If(c,i))] 
        @ (match e with None -> [] | Some e -> [Right(If(Not c, e))]) }

do_part:
    DO b=body
      { b }

lhs:
  VAR d=type_id { ([Left d], d.declaration_variable) } (* sugar *)
  | r=ID { ([], r) }

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
  | r=ID
      { Ref r }
  | n=NUMBER
      { Literal(Some n) }
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

%public args:
    LP r=separated_list(COMMA, expression) RP
      { r }

else_:
    ELSE b=body 
      { b } (* sugar *)

%%
