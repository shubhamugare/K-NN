require "substitution.md"

module KNN-SYNTAX
  imports DOMAINS-SYNTAX
  imports KVAR-SYNTAX

  syntax Exp ::= Int
               | Id                
               | "(" Exp ")"           [bracket]
               > Exp "*" Exp           [strict, left]
               > Exp "+" Exp           [strict, left]
               > "let" Id "=" Exp "in" Exp [strict(2)] 
               
                
endmodule


module KNN
  imports KNN-SYNTAX
  imports DOMAINS
  imports SUBSTITUTION

  syntax KResult ::= Int


  rule let X:Id = E1:Exp in E2:Exp => E2[E1 / X] 

  rule I1 * I2 => I1 *Int I2
  rule I1 + I2 => I1 +Int I2
  
endmodule
