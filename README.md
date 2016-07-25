# RHPackage
A Mathematica package for solving Riemann–Hilbert problems numerically

![Painlevé II RH Problem](sixrays.jpg)


Examples include computing Cauchy and Hilbert transforms, homogeneous Painlevé II 
(such as the Hastings–McLeod solution and Ablowitz–Stegun solutions for large x), Painlevé III and Painlevé IV. 

Includes a routine for evaluating Painlevé II:
```mathematica
	PainleveII[{s1,s2,s3},x]
```	
evaluates the solution to Painlevé II with Stokes' constants `s1`, `s2` and `s3` at the point `x`. 
The computation is reliable for all real x. 

The software was developed by Sheehan Olver with contributions from Georg Wechslberger, 
based on the framework described in _Found. Comput. Maths_, **11**: 153–179 and  _Numer. Math._ **122**: 305–340


# Installation instructions

Download the repository and follow the installation instructions in the file INSTALL.nb.

# License

The file
```
    RiemannHilbert/Data/McLeodSolution.txt 
```	
is included with permission from Michael Prahofer and Herbert Spohn, and is available online. All other files are © Sheehan Olver, 2010, subject to a BSD license. 

The package includes contributions from Georg Wechslberger.
