## Authors
Zixin Huang (UIUC)
Shubham Ugare (UIUC)

## Syntax

```k
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
               | "initArray" "(" Ints ")" 
               | Exp "[" Ints "]"      [strict(1)]
               | "relu" "(" Exp ")"     
               > "let" Exp "=" Exp "in" Exp  
               
  syntax FloatList ::= "[" Floats "]"
  syntax Floats ::= Float 
                  | Float "," Floats
  syntax Ints ::= Int 
                  | Int "," Ints
endmodule
```

## Semantics

```k
module KNN
  imports KNN-SYNTAX
  imports DOMAINS
  imports SUBSTITUTION
  imports FLOAT
```

## Configuration

```k
  configuration <T color="red">
                  <k color="white"> $PGM:Exp </k>
                  <env> .Map </env>
                  <store> .Map </store>
                  <nextLoc> 0 </nextLoc>
                  <tensors color="green">        
                     <tensorData multiplicity="*" type="Map" color="green">
                        <tensorName color="green"> Name </tensorName>
                        <size color="green"> .List </size>
                        <val color="green"> .List </val>
                        <tempval color="olive"> .List </tempval>
                     </tensorData>
                  </tensors>
                </T>
```

## Basic operations
```k
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
```

## Array (to be tensor)
```k
  
  syntax Val ::= array(Int, Int) | Float | Int 
  syntax Exp ::= Val
  syntax KResult ::= Val

  rule <k> let X = initArray(N:Int) in E:Exp  => E ...</k>
       <env> Env => Env[X <- L] </env>
       <store>... .Map => L |-> array(L +Int 1, N)
                          (L +Int 1) ... (L +Int N) |-> 0 ...</store>
       <nextLoc> L:Int => L +Int 1 +Int N </nextLoc>
      when N >=Int 0

  rule <k> X:Id => V ...</k>
       <env>... X |-> L ...</env>
       <store>... L |-> V:Val ...</store>  [lookup] 
  
  syntax Exp ::= lookup(Int)

  rule array(L,_)[N:Int] => lookup(L +Int N)      [structural, anywhere]                     
  rule <k> lookup(L) => V ...</k> <store>... L |-> V:Val ...</store>  [lookup]

  syntax Exp ::= storeVal(Exp, Val) 

  rule <k> let X:Id [I:Int] = V:Val in E:Exp => storeVal(X[I], V) ~> E ...</k>

  rule <k> storeVal(X[I], V) => . ...</k>
       <env>... X |-> L ...</env>
       <store>... (L +Int I +Int 1) |-> (_ => V) ...</store>    

         
```

## Relu

```k
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
  rule <k> E1:Id => . </k> <tensorData> <tensorName> E1 </tensorName> <size> _ </size> <val> _ </val> <tempval> .List </tempval>... </tensorData>
```

## Utility functions

```k

  syntax Map ::= Int "..." Int "|->" K [function]

  rule N...M |-> _ => .Map  requires N >Int M
  rule N...M |-> K => N |-> K (N +Int 1)...M |-> K  requires N <=Int M


endmodule
```
