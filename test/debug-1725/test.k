module TEST
  imports COLLECTIONS
  imports STRING

  configuration <k> $PGM:KItem </k>
                <theSet> .Set </theSet>

    syntax KItem ::= #checkArg ( String )
                   | "#included"
                   | "#notIncluded"

    rule <k> #checkArg(ARG) => #included ...</k>
         <theSet> S </theSet>
      requires ARG in S

    // rule <k> #checkArg(ARG) => #notIncluded ...</k>
    //      <theSet> S </theSet>
    //   requires notBool ARG in S

    rule [simplRule]: X in ((SetItem(A) REST:Set) #as S) => X ==K A orBool X in REST                    [simplification, symbolic(S)]

    syntax String ::= id(String) [function, functional]

    rule id(X) => X

endmodule
