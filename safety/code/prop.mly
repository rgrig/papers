(* Rules for the properties. *)

%%

%public property: PROPERTY STRING LB transition* RB { }

transition: edge COLON separated_nonempty_list(COMMA, label) { }

edge: ID ARROW ID { }

label: atom receiver? DOT ID args { }

receiver: ASGN atom {}

%%

