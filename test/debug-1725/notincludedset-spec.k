module VERIFICATION
  imports TEST

endmodule

module NOTINCLUDEDSET-SPEC
  imports VERIFICATION

  rule <k> #checkArg("foo") => #notIncluded </k>
       <theSet> SetItem("c") SetItem(A) </theSet>

endmodule
