require "substitution.md"

module KNN-SYNTAX
  imports DOMAINS-SYNTAX
  imports KVAR-SYNTAX
  imports FLOAT-SYNTAX

  syntax Id ::= "Name" [token] | "tensor" [token]

  syntax Exp ::= Float
               | Int
               | Id                
               | "(" Exp ")"           [bracket]
               > Exp "*" Exp           [strict, left]
               > Exp "+" Exp           [strict, left]
               | "tensor" "(" Ints "," FloatList ")"     
               | "relu" "(" Exp ")"     
               > "let" Id "=" Exp "in" Exp // [strict] 
               
  syntax FloatList ::= "[" Floats "]"
  syntax Floats ::= Float 
                  | Float "," Floats
  syntax Ints ::= Int 
                  | Int "," Ints

  
           
endmodule


module KNN
  imports KNN-SYNTAX
  imports DOMAINS
  imports SUBSTITUTION
  imports FLOAT

  configuration <T color="red">
                  <k color="white"> $PGM:Exp </k>
                  <tensors color="green">        
                     <tensorData multiplicity="*" type="Map" color="green">
                        <tensorName color="green"> Name </tensorName>
                        <size color="green"> .List </size>
                        <val color="green"> .List </val>
                        <tempval color="olive"> .List </tempval>
                     </tensorData>
                  </tensors>
                </T>

  syntax KResult ::= Float | Int 

  // Scalar in let
  rule let X:Id = E1:Float in E2:Exp => E2[E1 / X] 
  rule let X:Id = E1:Int in E2:Exp => E2[E1 / X] 

  rule I1 * I2 => I1 *Float I2
  rule I1 + I2 => I1 +Float I2

  // Tensor in let
  syntax KItem ::= toList(Id, Floats) // add Id in toList to track which tensor to modify

  rule <k> ( let X:Id = tensor ( Si:Ints , [Fl:Floats] ) in E2:Exp ) => toList(X, Fl) ~> E2 ...</k> 
        <tensors>...
            (.Bag =>         
             <tensorData>
                <tensorName> X </tensorName>
                <size> ListItem(Si) </size>
                <val> .List </val>
                ...
             </tensorData>
             )
        ...
       </tensors>
        
  rule <k> toList(X, F1:Float) => . ... </k> <tensorData> <tensorName> X </tensorName> <val> ... .List => ListItem(F1)  </val> ... </tensorData> 
  rule <k> toList(X, (F1:Float, FL:Floats)) => toList(X, FL) ... </k> <tensorData> <tensorName> X </tensorName> <val> ... .List => ListItem(F1) </val> ... </tensorData>  // append at the end

  // Relu in let
  syntax KItem ::= reluHelper(Id) 
  rule <k> let X:Id = relu(E:Id) in E2 => ( reluHelper(X) ~> E2 ) ... </k> 
        <tensors>...
             <tensorData>
                <tensorName> E </tensorName>
                <size> S </size>
                <val> L </val>
                ...
             </tensorData>
            (.Bag =>         
             <tensorData>
                <tensorName> X </tensorName>
                <size> S </size>
                <val> .List </val>
                <tempval> L </tempval>
                ...
             </tensorData>
             )
        ...
       </tensors>

  rule <k> reluHelper(X) ... </k> <tensorData> <tensorName> X </tensorName> <size> _ </size> <val> ... .List => ListItem(F1)  </val> <tempval> ListItem(F1) => .List ... </tempval>... </tensorData> requires F1 >=Float 0.0
  rule <k> reluHelper(X) ... </k> <tensorData> <tensorName> X </tensorName> <size> _ </size> <val> ... .List => ListItem(0.0) </val> <tempval> ListItem(F1) => .List ... </tempval>... </tensorData> requires F1 <Float 0.0
  rule <k> reluHelper(X) => . ... </k> <tensorData> <tensorName> X </tensorName> <size> _ </size>  <val> _ </val> <tempval> .List </tempval>... </tensorData>


  // Result, TODO: fix this
  rule <k> E1:Id => V </k> <tensorData> <tensorName> E1 </tensorName> <size> _ </size> <val> V </val> <tempval> .List </tempval>... </tensorData>

endmodule
