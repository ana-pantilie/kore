module PATH
  import DOMAINS
  syntax S ::= "a" | "b" | "c" 
  syntax Cmd ::= "select"

  configuration <k> $PGM:K </k> <state> SetItem(a) SetItem(b) SetItem(c) </state>

  rule <k> select => X ...</k> <state>... SetItem(X:S) ...</state>
endmodule
