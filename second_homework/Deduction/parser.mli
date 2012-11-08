type expression =
    PropositionalVariable of char
  | Implication of expression * expression
  | Conjunction of expression * expression
  | Disjunction of expression * expression
  | Negation of expression
type parse_tuple = { end_pos : int; result : expression; }
val parse_expr : string -> expression
