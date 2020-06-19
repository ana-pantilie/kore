// Copyright (c) 2020 K Team. All Rights Reserved.

module VERIFICATION
    imports VERIF
endmodule

module TEST-SPEC
    imports VERIFICATION

    rule <k> step foo(_) => . ...</k>
endmodule
