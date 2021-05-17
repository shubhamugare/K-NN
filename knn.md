## Authors

Shubham Ugare (UIUC)

Zixin Huang (UIUC)

## Abstract

This is the **K** semantic definition of K-NN, a language 
for neural networks. We defined several common used operations
in the ML domain. Future works could extend the semantics 
and tailor them for verifying different properties of 
neural networks.

Specifically, KNN supports the following features:

*   Tensors. Tensors are essentially multi-dimensional arrays with additional shape information. 
    We store tensor references  in <env>, and we look up the tensor values in <store>. The references are values indicating 
    the locations where the tensor elements start in <store>. Similar to multi-dimensional arrays in simple language, the tensor
    elements can also be references, indicating
    the locations for sub-tensors. Besides the values, we store the shape information in <shape>, 
    for convenient 
    slicing, reshaping or other dimension dependent operations on tensors.
*   Built-in Tensor Transformers. We implemented several commonly used transformers, which takes tensor(s) as input
    and output another tensor. The functions include the definition and assignment of a tensor, 
    the definition of a linear layer in the neural network,
    and activation functions Relu, Sigmoid, and Tanh.
    

## Syntax

We support the simple language constructs that could define a typical neural network: 
constants, variables, tensors, declarations (let-in), arithmetic operations, and functions.

First we use the existing DOMAINS, KVAR and FLOAT syntax. 

```k
require "substitution.md"


module KNN-SYNTAX
  imports DOMAINS-SYNTAX
  imports KVAR-SYNTAX
  imports FLOAT-SYNTAX

```

Here we define the expressions in the program. Integers (comma-separated integers) are used for the shape and index of the tensors, and Floats (comma-separated floats) are used as the elements in tensors
and also the arithmetic on them.  We let each tensor to be associated with an `Id` stored in the enviroment,
and we employ the let-in clause for scoping. 
During the complication, we use a special identifier `tensor` to denote that an element is a tensor.

We also define accessing an element in the tensor in the same way as accessing multi-dimensional arrays.


```k
  syntax Id ::= "tensor" [token]

  syntax Exp ::= Float
               | Int
               | Id          
               | Exp "[" Ints "]"      [strict(1)]      
               | "(" Exp ")"           [bracket]
               | "max" "(" Exp "," Exp ")"  [strict]
               | "min" "(" Exp "," Exp ")"  [strict]
               | "ArgMax" "(" Exp ")"       // expects a vector
               > Exp "*" Exp           [strict, left]
               > Exp "+" Exp           [strict, left]
               > "let" Exp "=" Exp "in" Exp  [strict(2)]
               >  "let" Exp "=" Func "in" Exp 

  syntax FloatList ::= "[" Floats "]"
  syntax Floats ::= Float 
                  | Float "," Floats
  syntax Ints ::= Int 
                  | Int "," Ints
```
  
### Functions
We define the common used functions in defining neural networks.

```k
  syntax Func ::=  "initTensor" "(" Ints ")" 
                   | "relu" "(" Exp ")"  
                   | "reluTensor" "(" Exp ")"
                   | "tanh" "(" Exp ")"  
                   | "tanhTensor" "(" Exp ")"  
                   | "sigmoid" "(" Exp ")"  
                   | "sigmoidTensor" "(" Exp ")"
                   | "linear" "(" Id "," Id ")"  

                   

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
To store additional shape information tensors, we add \<shape\> in the configuration, 
which is a map from tensor IDs to shape (comma separated integers). The other configurations
are typical to sequential languages. For tensors, we implement them similar to multidimensional arrays.
We store their references in \<env\>, and lookup their raw values in \<store\>.

```k
  configuration <T color="red">
                  <k color="white"> $PGM:Exp </k>
                  <env> .Map </env>
                  <store> .Map </store>
                  <nextLoc> 0 </nextLoc>
                  <shape> .Map </shape>
                </T>
```

## Basic operations

For simple float/int scalars, we substitute their value in the final expression.
For arithmetic on floats, we use the built-in functions on floats.



```k
  syntax KResult ::= Float | Int 

  // Scalar in let
  rule let X:Id = E1:Float in E2:Exp => E2[E1 / X] 
  rule let X:Id = E1:Int in E2:Exp => E2[E1 / X] 
  // rule <k> let X:Id = E:Exp in E1:Exp => storeVal(X, E) ~> E1...</k>

  rule I1 * I2 => I1 *Float I2
  rule I1 + I2 => I1 +Float I2

```

## Tensor 

Here we initialize a tensors by storing a reference (a value indicating the starting location 
of the elements in \<store\>)
 of the tensor in \<env\>. Since the tensor can be multi-dimensional, each elements following 
the referece can be references to sub-tensors.
We allow initialize the tensor with a given shape and later assign value to some elements in the tensor.
By default, `initTensor` initialize all the elements to `0.0`.
This will be very useful for defining sparse tensors, where only a few elements are non-zero.

```k
  
  syntax Val ::= tensor(Int, Int) | Float | Ints 
  syntax Exp ::= Val
  syntax KResult ::= Val

  syntax Exp ::= defineTensor(Exp, Ints) 
  syntax Exp ::= "defineTensorHelper" "(" Id "," Int "..." Int "," Ints ")"

  rule <k> let X = initTensor(Is:Ints) in E:Exp  => defineTensor(X, Is) ~> E ...</k>
       <shape>... .Map => X |-> (Is) ...</shape> 

  rule <k> defineTensor(X:Id,N:Int) => . ...</k>
       <env> Env => Env[X <- L] </env>
       <store>... .Map => L |-> tensor(L +Int 1, N)
                          (L +Int 1) ... (L +Int N) |-> 0.0 ...</store>
       <nextLoc> L:Int => L +Int 1 +Int N </nextLoc>
      when N >=Int 0

  rule <k> defineTensor(X:Id, I:Int, Rest:Ints) => defineTensor(X, I) ~> defineTensorHelper(X, 0 ...I, Rest:Ints) ...</k> 
  
  syntax Id ::= "$1"
  rule <k> defineTensorHelper(X, I...J, Rest) => defineTensor($1, Rest) ~> storeVal(X[I], $1) ~> defineTensorHelper(X, (I +Int 1)...J, Rest) ...</k>
      when I <Int J 

  rule <k> defineTensorHelper(X, I...I, Rest) => defineTensor($1, Rest) ~> storeVal(X[I], $1) ...</k>


  rule <k> X:Id => V ...</k>
       <env>... X |-> L ...</env>
       <store>... L |-> V:Val ...</store>  [lookup] 
  
  syntax Exp ::= lookup(Int)

  rule tensor(L,_)[N:Int] => lookup(L +Int N)      [structural, anywhere]
  rule tensor(L,I)[N:Int, Rest:Ints] => tensor(L,I)[N:Int][Rest:Ints]    [structural, anywhere]                  
  rule <k> lookup(L) => V ...</k> <store>... L |-> V:Val ...</store>  [lookup]

  syntax Exp ::= storeVal(Exp, Exp) 

  rule <k> let X:Id [Is:Ints] = E:Exp in E1:Exp => storeVal(X[Is], E) ~> E1...</k>

  context storeVal(_, HOLE)
  context storeVal(HOLE[_:Int, _:Ints],_)

  rule storeVal(X[I:Int, Rest:Ints], V:Val) => storeVal(X[I][Rest], V)

  rule <k> storeVal(X[I:Int], V) => . ...</k>
        <env>... X |-> L ...</env>
        <store>... (L +Int I +Int 1) |-> (_ => V) ...</store>

  rule <k> storeVal(lookup(L)[I:Int], V) => storeAtVal(T +Int I, V) ...</k>
       <store>... L |-> tensor(T,_) ...</store> 

  syntax Exp ::= storeAtVal(Int, Val)
    
  rule <k> storeAtVal(L:Int, V:Val) => . ...</k>
       <store>... L |-> (_ => V) ...</store> 

         
```

## Relu 

We define relu by processing every element from the input tensor and assign to the
output tensor. First we initialize the output tensor, and then
 assign to every element in the new tensor.
We apply the `reluTensorHelper` function
recursively until we go through all the elements. 

In short, the following code lets each element X[I]=max(Y[I],0), where 
X is the output tensor  and Y is the input tensor. The shape of X and Y will be the same.



```k
  rule <k> let X:Id = reluTensor(Y:Id) in E => defineTensor(X,Is) ~> reluTensorHelper(Y, X, Is) ~> E ... </k> 
       <shape>... Y |-> Is ...</shape> 

  syntax Exp ::= reluTensorHelper(Exp, Exp)     
  syntax Exp ::= reluTensorHelper(Exp, Exp, Ints)
  syntax Exp ::= "reluTensorHelper" "(" Exp "," Exp "," Int "..." Int "," Ints ")"
  syntax Exp ::= "reluTensorHelper" "(" Exp "," Exp "," Int "..." Int ")"

  rule reluTensorHelper(Y, X, I:Int, Is:Ints) => reluTensorHelper(Y, X, 0 ...I, Is) 
  rule reluTensorHelper(Y, X, I:Int) => reluTensorHelper(Y, X, 0 ...I)

  rule <k> reluTensorHelper(Y, X, I...J:Int, Is:Ints) => reluTensorHelper(Y[I], X[I], Is:Ints) ~> reluTensorHelper(Y, X, (I +Int 1)...J:Int, Is:Ints) ...</k>     
    when I <Int J
  
  rule <k> reluTensorHelper(Y, X, I...J:Int) => reluTensorHelper(Y[I], X[I]) ~> reluTensorHelper(Y, X, (I +Int 1)...J:Int) ...</k>     
    when I <Int J

  rule <k> reluTensorHelper(_, _, I...I:Int, _:Ints) => . ...</k>
  rule <k> reluTensorHelper(_, _, I...I:Int) => . ...</k>

  //rule reluTensorHelper(E1:Exp[I:Int], E2:Exp[J:Int]) => reluTensorHelper(fun(E1:Exp[I:Int]), fun(E2:Exp[J:Int]))
  //context reluTensorHelper(HOLE, HOLE)
  rule reluTensorHelper(E1:Exp[I:Int], E2:Exp[J:Int]) => let E2[J] = E1[I] in 0

  rule let E:Exp[I:Int][J:Int] = E2:Exp in E3:Exp => let E:Exp[I,J] = max(E2:Exp, 0.0) in E3:Exp
//  rule E:Exp[I:Int][Is:Ints]) => E[I, Is]) 
//  rule X:Id[Is:Ints]) => X:Id[Is:Ints] 

 // syntax Exp ::= "something" [token]

  rule <k> 0 => . ...</k>

  //rule reluTensorHelper()
```
## Linear Layer

The following code computes the output of a linear layer in the neural network.
It follows the matrix-vector product--given  as input a weights tensor W of shape (I,J), 
and a 1-dimensional tensor 
X of shape (J), it output the output tensor WX of shape (I)

The implementation is similar to Relu: we first initialize the tensor to shape (I),
and then assign to each element in the tensor.
```k
  
  syntax Exp ::= linearHelper(Id, Int, Int, Id, Id)
  syntax Exp ::= "linearHelper" "(" Id "," Int "..." Int "," Int "," Id "," Id ")"
  syntax Exp ::= "linearHelper" "(" Id "[" Int "]" "," Int "..." Int "," Id "[" Int "]" "," Id ")" 
  syntax Exp ::= linearHelper(Exp, Exp, Exp)

  rule <k> let Y:Id = linear(W, X) in E => defineTensor(Y, I) ~> linearHelper(Y, I, J, W, X) ~> E ...</k> 
       <shape>... W |-> I:Int,J:Int  ...</shape>

  rule linearHelper(Y, I:Int, J:Int, W, X) => linearHelper(Y, 0 ...I, J, W, X) 
       
  rule linearHelper(Y, I1:Int...I2:Int, J:Int, W, X) => linearHelper(Y[I1], 0 ...J, W[I1], X) ~> linearHelper(Y, (I1 +Int 1)...I2:Int, J:Int, W, X) 
        when I1 <Int I2

  rule linearHelper(_, I:Int...I:Int, _:Int, _:Id, _:Id) => .

  rule linearHelper(Y[I1], J1...J2, W[I1], X) => linearHelper(Y[I1], W[I1][J1], X[J1]) ~> linearHelper(Y[I1], (J1 +Int 1)...J2, W[I1], X)
      when J1 <Int J2

  rule linearHelper(_:Id[_:Int], J...J, _:Id[_:Int], _:Id) => .

  rule linearHelper(E1, E2, E3) => let E1 = E1 + (E2*E3) in 0

```


## Tanh

Tanh is similar to Relu. We proceed each element
by X[I]=(exp(Y[I])-exp(-Y[I]))/(exp(Y[I])+exp(-Y[I])), where 
X is the output tensor and Y is the input tensor. The shape of X and Y will be the same.

```k
  rule <k> let X:Id = tanhTensor(Y:Id) in E => defineTensor(X,Is) ~> tanhTensorHelper(Y, X, Is) ~> E ... </k> 
       <shape>... Y |-> Is ...</shape> 

  syntax Exp ::= tanhTensorHelper(Exp, Exp)     
  syntax Exp ::= tanhTensorHelper(Exp, Exp, Ints)
  syntax Exp ::= "tanhTensorHelper" "(" Exp "," Exp "," Int "..." Int "," Ints ")"
  syntax Exp ::= "tanhTensorHelper" "(" Exp "," Exp "," Int "..." Int ")"
  syntax Exp ::= tanhItem(Exp)     

  rule tanhTensorHelper(Y, X, I:Int, Is:Ints) => tanhTensorHelper(Y, X, 0 ...I, Is) 
  rule tanhTensorHelper(Y, X, I:Int) => tanhTensorHelper(Y, X, 0 ...I)

  rule <k> tanhTensorHelper(Y, X, I...J:Int, Is:Ints) => tanhTensorHelper(Y[I], X[I], Is:Ints) ~> tanhTensorHelper(Y, X, (I +Int 1)...J:Int, Is:Ints) ...</k>     
    when I <Int J
  
  rule <k> tanhTensorHelper(Y, X, I...J:Int) => tanhTensorHelper(Y[I], X[I]) ~> tanhTensorHelper(Y, X, (I +Int 1)...J:Int) ...</k>     
    when I <Int J

  rule <k> tanhTensorHelper(_, _, I...I:Int, _:Ints) => . ...</k>
  rule <k> tanhTensorHelper(_, _, I...I:Int) => . ...</k>

  rule tanhTensorHelper(E1:Exp[I:Int], E2:Exp[J:Int]) => let E2[J] = tanhItem(E1[I]) in 0

  context tanhItem(HOLE)
  rule tanhItem(E:Exp[I:Int][J:Int]) => tanhItem(E:Exp[I:Int,J:Int]) 
  rule let E:Exp[I:Int][J:Int] = tanhItem(E2:Exp) in E3:Exp => let E:Exp[I,J] = tanhItem(E2:Exp) in E3:Exp

```

## Sigmoid

Sigmoid is similar to Relu. We proceed each element
by X[I]=1/(1-exp(-Y[I])), where 
X is the output tensor and Y is the input tensor. The shape of X and Y will be the same.
```k
  rule <k> let X:Id = sigmoidTensor(Y:Id) in E => defineTensor(X,Is) ~> sigmoidTensorHelper(Y, X, Is) ~> E ... </k> 
       <shape>... Y |-> Is ...</shape> 

  syntax Exp ::= sigmoidTensorHelper(Exp, Exp)     
  syntax Exp ::= sigmoidTensorHelper(Exp, Exp, Ints)
  syntax Exp ::= "sigmoidTensorHelper" "(" Exp "," Exp "," Int "..." Int "," Ints ")"
  syntax Exp ::= "sigmoidTensorHelper" "(" Exp "," Exp "," Int "..." Int ")"
  syntax Exp ::= sigmoidItem(Exp)     

  rule sigmoidTensorHelper(Y, X, I:Int, Is:Ints) => sigmoidTensorHelper(Y, X, 0 ...I, Is) 
  rule sigmoidTensorHelper(Y, X, I:Int) => sigmoidTensorHelper(Y, X, 0 ...I)

  rule <k> sigmoidTensorHelper(Y, X, I...J:Int, Is:Ints) => sigmoidTensorHelper(Y[I], X[I], Is:Ints) ~> sigmoidTensorHelper(Y, X, (I +Int 1)...J:Int, Is:Ints) ...</k>     
    when I <Int J
  
  rule <k> sigmoidTensorHelper(Y, X, I...J:Int) => sigmoidTensorHelper(Y[I], X[I]) ~> sigmoidTensorHelper(Y, X, (I +Int 1)...J:Int) ...</k>     
    when I <Int J

  rule <k> sigmoidTensorHelper(_, _, I...I:Int, _:Ints) => . ...</k>
  rule <k> sigmoidTensorHelper(_, _, I...I:Int) => . ...</k>

  rule sigmoidTensorHelper(E1:Exp[I:Int], E2:Exp[J:Int]) => let E2[J] = sigmoidItem(E1[I]) in 0

  context sigmoidItem(HOLE)
  rule sigmoidItem(E:Exp[I:Int][J:Int]) => sigmoidItem(E:Exp[I:Int,J:Int]) 
  rule let E:Exp[I:Int][J:Int] = sigmoidItem(E2:Exp) in E3:Exp => let E:Exp[I,J] = sigmoidItem(E2:Exp) in E3:Exp

```

## Linear Layer
```k
  
  syntax Exp ::= linearHelper(Id, Int, Int, Id, Id)
  syntax Exp ::= "linearHelper" "(" Id "," Int "..." Int "," Int "," Id "," Id ")"
  syntax Exp ::= "linearHelper" "(" Id "[" Int "]" "," Int "..." Int "," Id "[" Int "]" "," Id ")" 
  syntax Exp ::= linearHelper(Exp, Exp, Exp)

  rule <k> let Y:Id = linear(W, X) in E => defineTensor(Y, I) ~> linearHelper(Y, I, J, W, X) ~> E ...</k> 
       <shape>... W |-> I:Int,J:Int  ...</shape>

  rule linearHelper(Y, I:Int, J:Int, W, X) => linearHelper(Y, 0 ...I, J, W, X) 
       
  rule linearHelper(Y, I1:Int...I2:Int, J:Int, W, X) => linearHelper(Y[I1], 0 ...J, W[I1], X) ~> linearHelper(Y, (I1 +Int 1)...I2:Int, J:Int, W, X) 
        when I1 <Int I2

  rule linearHelper(_, I:Int...I:Int, _:Int, _:Id, _:Id) => .

  rule linearHelper(Y[I1], J1...J2, W[I1], X) => linearHelper(Y[I1], W[I1][J1], X[J1]) ~> linearHelper(Y[I1], (J1 +Int 1)...J2, W[I1], X)
      when J1 <Int J2

  rule linearHelper(_:Id[_:Int], J...J, _:Id[_:Int], _:Id) => .

  rule linearHelper(E1, E2, E3) => let E1 = E1 + (E2*E3) in 0

```

## Min and Max

The following are useful auxiliary functions for min, max, tanh, sigmoid on a scalar float.

```k

rule min(A:Float, B:Float) => A 
      when A <Float B

rule min(A:Float, B:Float) => B 
      when A >=Float B

rule max(A:Float, B:Float) => B 
      when A <Float B

rule max(A:Float, B:Float) => A 
      when A >=Float B

rule tanhItem(X:Float) => (expFloat(2.0 *Float X) -Float 1.0) /Float (expFloat(2.0 *Float X) +Float 1.0)

rule sigmoidItem(X:Float) => (1.0 /Float (1.0 +Float expFloat(0.0 -Float X)))

```

## Argmax
It takes a vector as an argument and returns the index of largest element.

```k

rule <k> ArgMax(X:Id) => ArgMaxHelper(X, 0 ...I, 0.0, 0) ...</k>
     <shape>... X |-> I:Int ...</shape>

syntax Exp ::= "ArgMaxHelper" "(" Id "," Int "..." Int "," Float "," Int ")"

rule <k> ArgMaxHelper(X, I...J, F:Float, _:Int) => ArgMaxHelper(X, (I +Int 1)...J, F2, I)  ...</k>
     <env>... X|->L ...</env>
     <store>... (L +Int I +Int 1)|-> F2 ...</store>
    when F2 >Float F andBool J >Int I

rule <k> ArgMaxHelper(X, I...J, F:Float, II:Int) => ArgMaxHelper(X, (I +Int 1)...J, F, II)  ...</k>
     <env>... X|->L ...</env>
     <store>... (L +Int I +Int 1)|-> F2 ...</store>
    when F2 <Float F andBool J >Int I

  rule <k> ArgMaxHelper(_, I...I, _:Float, II:Int) => II ...</k>

```

## Utility functions

```k

  syntax Map ::= Int "..." Int "|->" K [function]

  rule N...M |-> _ => .Map  requires N >Int M
  rule N...M |-> K => N |-> K (N +Int 1)...M |-> K  requires N <=Int M

  syntax Exp::= fun(Exp)
  rule fun(E:Exp[I:Int][J:Int]) => fun(E[I,J])   [function]
  rule fun(E:Exp[I:Int][Is:Ints]) => fun(E[I, Is]) [function]
  rule fun(X:Id[Is:Ints]) => X:Id[Is:Ints] [function]

endmodule
```
