require "substitution.md"

module KNN-SYNTAX
  imports DOMAINS-SYNTAX
  imports KVAR-SYNTAX
  imports FLOAT-SYNTAX

  syntax Id ::= "Name" [token] | "tensor" [token]

  syntax Exp ::= Float
               | Id                
               | "(" Exp ")"           [bracket]
               > Exp "*" Exp           [strict, left]
               > Exp "+" Exp           [strict, left]
               | "tensor" "(" Int "," FloatList ")"     
               > "let" Id "=" Exp "in" Exp [strict(2)] 
               
  syntax FloatList ::= "[" Floats "]"
  syntax Floats ::= Float 
                  | Float "," Floats

  
           
endmodule


module KNN
  imports KNN-SYNTAX
  imports DOMAINS
  imports SUBSTITUTION
  imports FLOAT

  configuration <T color="red">
                  <k color="green"> $PGM:Exp </k>
                  <tensors color="green">        
                     <tensorData multiplicity="*" type="Map" color="green">
                        <tensorName color="green"> Name </tensorName>
                        <size color="green"> .List </size>
                        <val color="green"> .List </val>
                     </tensorData>
                  </tensors>
                </T>

  syntax KResult ::= Float | Int 


  rule <k> tensor ( Si:Int , [Fl:Floats] )  => . ...</k> 
        <tensors>...
            (.Bag =>         
             <tensorData>
                <tensorName> Name </tensorName>
                <size> ListItem(Si) </size>
                <val> toList(Fl) </val>
             </tensorData>
             )
        ...</tensors>
        
  rule let X:Id = E1:Exp in E2:Exp => E2[E1 / X] 

  rule I1 * I2 => I1 *Float I2
  rule I1 + I2 => I1 +Float I2
  
  rule toList(F1:Float, FL:Floats) => F1 toList(FL)  

endmodule
