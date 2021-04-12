# K-NN

In this project, we plan to develop a programming language for neural network inference using the K framework. We plan to explore the verification capabilities of the K framework to prove properties over the neural network. Moreover, we also explore the possibility of performing abstract interpretation over the neural network modifying the existing semantics to take abstract values such as intervals. 

NN models can often be represented with several syntactic constructs. We take inspiration from an existing NN inference language defined in [1] called SeeDot. The basic syntax for SeeDot can be summarized using the following rules. 


This syntax with some additional operators such as the Relu function is sufficient to define fully connected neural networks. Our project will focus on networks with fully-connected layers. If time permits we will extend it to convolutional layers (but we note potential difficulties when representing convolutions).

We will use K to formally define the semantics of these commonly used operations in the ML domain. Further, we will explore the verification capabilities of K to prove the properties of the ML model.

Our project will help open up new research directions for neural network inference and verification. Future works could extend our semantics in K and tailor them to specific research questions. We list some of these possibilities here ( Not in the scope of the course project):  
The original SeeDot paper compiles the NNs to fixed point integer code in C. It is interesting to explore whether using K-framework low bit-width integers could produce similar results.

We can prove adversarial robustness properties of neural networks using advanced abstract interpretation using zonotopes or deeppoly techniques in [2, 3].
The semantics of NN inference could augment the verification possibilities in Koord [4] and many other domain-specific languages. Koord is a programming language implemented using the K framework for robotics verification. In these semantics, Neural networks are treated as black-box, so our work might endow it with full verification power.

## References:

[1] Sridhar Gopinath, Nikhil Ghanathe, Vivek Seshadri, Rahul Sharma, “Compiling  
     KB-sized machine learning models to tiny IoT devices”, PLDI 2019
     
[2] Gagandeep Singh, Timon Gehr, Markus Püschel, Martin Vechev, “An Abstract  
     domain  for Certifying Neural Networks”, POPL 2019
     
[3] Gagandeep Singh, Timon Gehr, Matthew Mirman, Markus Püschel, Martin 
     Vechev, “Fast and Effective Robustness Certification”, NIPS 2018
     
[4] Ritwika Ghosh, Chiao Hsieh, Sasa Misailovic, Sayan Mitra, “Koord: A Language 
