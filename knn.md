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
               | Exp "[" Ints "]"      [strict(1)]      
               | "(" Exp ")"           [bracket]
               > Exp "*" Exp           [strict, left]
               > Exp "+" Exp           [strict, left]
               | "tensor" "(" Ints "," FloatList ")"
               | "initArray" "(" Ints ")" 
               | "relu" "(" Exp ")"  
               | "reluArray" "(" Exp ")"    
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
                  <shape> .Map </shape>
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
  
  syntax Val ::= array(Int, Int) | Float | Ints 
  syntax Exp ::= Val
  syntax KResult ::= Val

  syntax Exp ::= defineArray(Exp, Ints) 
  syntax Exp ::= "defineArrayHelper" "(" Id "," Int "..." Int "," Ints ")"

  rule <k> let X = initArray(Is:Ints) in E:Exp  => defineArray(X, Is) ~> E ...</k>
       <shape>... .Map => X |-> (Is) ...</shape> 

  rule <k> defineArray(X:Id,N:Int) => . ...</k>
       <env> Env => Env[X <- L] </env>
       <store>... .Map => L |-> array(L +Int 1, N)
                          (L +Int 1) ... (L +Int N) |-> 0 ...</store>
       <nextLoc> L:Int => L +Int 1 +Int N </nextLoc>
      when N >=Int 0

  rule <k> defineArray(X:Id, I:Int, Rest:Ints) => defineArray(X, I) ~> defineArrayHelper(X, 0 ...I, Rest:Ints) ...</k> 
  
  syntax Id ::= "$1"
  rule <k> defineArrayHelper(X, I...J, Rest) => defineArray($1, Rest) ~> storeVal(X[I], $1) ~> defineArrayHelper(X, (I +Int 1)...J, Rest) ...</k>
      when I <Int J 

  rule <k> defineArrayHelper(X, I...I, Rest) => defineArray($1, Rest) ~> storeVal(X[I], $1) ...</k>


  rule <k> X:Id => V ...</k>
       <env>... X |-> L ...</env>
       <store>... L |-> V:Val ...</store>  [lookup] 
  
  syntax Exp ::= lookup(Int)

  rule array(L,_)[N:Int] => lookup(L +Int N)      [structural, anywhere]
  rule array(L,I)[N:Int, Rest:Ints] => array(L,I)[N:Int][Rest:Ints]    [structural, anywhere]                  
  rule <k> lookup(L) => V ...</k> <store>... L |-> V:Val ...</store>  [lookup]

  syntax Exp ::= storeVal(Exp, Exp) 

  rule <k> let X:Id [I:Ints] = V:Val in E:Exp => storeVal(X[I], V) ~> E ...</k>

  context storeVal(_, HOLE)
  context storeVal(HOLE[_:Int, _:Ints],_)

  rule storeVal(X[I:Int, Rest:Ints], V:Val) => storeVal(X[I][Rest], V)

  rule <k> storeVal(X[I:Int], V) => . ...</k>
        <env>... X |-> L ...</env>
        <store>... (L +Int I +Int 1) |-> (_ => V) ...</store>

  rule <k> storeVal(lookup(L)[I:Int], V) => storeAtVal(T +Int I, V) ...</k>
       <store>... L |-> array(T,_) ...</store> 

  syntax Exp ::= storeAtVal(Int, Val)
    
  rule <k> storeAtVal(L:Int, V:Val) => . ...</k>
       <store>... L |-> (_ => V) ...</store> 

         
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

## Relu (For arrays)
```k
  rule <k> let X:Id = reluArray(Y:Id) in E => defineArray(X,Is) ~> reluArrayHelper(Y, X, Is) ~> E ... </k> 
       <shape>... Y |-> Is ...</shape> 

  syntax Exp ::= reluArrayHelper(Exp, Exp)     
  syntax Exp ::= reluArrayHelper(Exp, Exp, Ints)
  syntax Exp ::= "reluArrayHelper" "(" Exp "," Exp "," Int "..." Int "," Ints ")"
  syntax Exp ::= "reluArrayHelper" "(" Exp "," Exp "," Int "..." Int ")"

  rule reluArrayHelper(Y, X, I:Int, Is:Ints) => reluArrayHelper(Y, X, 0 ...I, Is) 
  rule reluArrayHelper(Y, X, I:Int) => reluArrayHelper(Y, X, 0 ...I)

  rule <k> reluArrayHelper(Y, X, I...J:Int, Is:Ints) => reluArrayHelper(Y[I], X[I], Is:Ints) ~> reluArrayHelper(Y, X, (I +Int 1)...J:Int, Is:Ints) ...</k>     
    when I <Int J
  
  rule <k> reluArrayHelper(Y, X, I...J:Int) => reluArrayHelper(Y[I], X[I]) ~> reluArrayHelper(Y, X, (I +Int 1)...J:Int) ...</k>     
    when I <Int J

  rule <k> reluArrayHelper(_, _, I...I:Int, _:Ints) => . ...</k>
  rule <k> reluArrayHelper(_, _, I...I:Int) => . ...</k>

  rule reluArrayHelper(E1:Exp, E2:Exp) => let E1 = E2 in 0

```


## Utility functions

```k

  syntax Map ::= Int "..." Int "|->" K [function]

  rule N...M |-> _ => .Map  requires N >Int M
  rule N...M |-> K => N |-> K (N +Int 1)...M |-> K  requires N <=Int M


endmodule
```