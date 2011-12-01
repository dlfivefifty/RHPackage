(* Mathematica Test File *)







(* ::Section:: *)
(* Unit Circle *)


f = Sech[#] Sin[1/#] &;
lf = LFun[f, UnitCircle];


Test[
	Cauchy[lf, 0.1] - CircleNIntegrate[f[z]/(z - 0.1), z]/(2 \[Pi] I)
	,
	0
	,
	TestID->"RHTest-Circle-Cauchy"
	,
	EquivalenceFunction -> NEqual
]

Test[
	Cauchy[-1, lf, 10. I + 0.1] -  CircleNIntegrate[f[z]/(z - (10 I + 1/10)), z, 
		WorkingPrecision -> 20]/(2 \[Pi] I)
	,
	0
	,
	EquivalenceFunction -> NEqual
	,
	TestID->"RHTest-Circle-Cauchy-1"
]

Test[
	Cauchy[+1, lf, Exp[I 0.1]] - Cauchy[-1, lf, Exp[I 0.1]] - 
 lf[Exp[I 0.1]]
	,
	0
	,
	EquivalenceFunction -> NEqual
	,
	TestID->"RHTest-Circle-Cauchy-JumpCondition"
]

Test[
	Cauchy[+1, lf, {1,Exp[I 0.1]}] - Cauchy[-1, lf, {1,Exp[I 0.1]}] - 
 (lf[{1,Exp[I 0.1]}])//Norm
	,
	0
	,
	EquivalenceFunction -> NEqual
	,
	TestID->"RHTest-Circle-Cauchy-List"
]




(* ::Section:: *)
(* Real Line *)



f = Sech[#] &;
lf = LFun[f, RealLine];


Test[
	Cauchy[lf, 0.1 I] - NIntegrate[
   f[z]/(z - 0.1 I), {z, -\[Infinity], \[Infinity]}]/(2 \[Pi] I)
	,
	0
	,
	TestID->"RHTest-RealLine-Cauchy"
	,
	EquivalenceFunction -> NEqual
]

Test[
	(Cauchy[+1, lf, 0.1] - Cauchy[-1, lf, 0.1] - f[0.1])/Length[lf]
	,
	0
	,
	EquivalenceFunction -> NEqual
	,
	TestID->"RHTest-RealLine-Cauchy-Jump"
]

Test[
	(Hilbert[lf, 0.1] - 
 NIntegrate[f[z]/(z - 0.1), {z, -\[Infinity], 0.1, \[Infinity]}, 
   PrincipalValue -> True]/\[Pi])/Length[lf]
	,
	0
	,
	EquivalenceFunction -> NEqual
	,
	TestID->"RHTest-RealLine-Hilbert"
]

Test[
	(lf // Hilbert)[0.1] - Hilbert[lf, 0.1] 
	,
	0
	,
	EquivalenceFunction -> NEqual
	,
	TestID->"RHTest-RealLine-HilbertVsHilbertFun"
]


Test[
	(lf // HilbertInverse)[0.1] - HilbertInverse[lf, 0.1]
	,
	0
	,
	EquivalenceFunction -> NEqual
	,
	TestID->"RHTest-RealLine-HilbertInverseVsHilbertInverseFun"
]


Test[
	{CauchyInverse[+1, lf, 10000000000. I], 
   CauchyInverse[-1, lf, -10000000000. I]} // Chop // Norm
	,
	0
	,
	EquivalenceFunction -> NEqual
	,
	TestID->"RHTest-RealLine-DecayAtInfinity"
]


Test[
	CauchyInverse[+1, lf, 0.1] + CauchyInverse[-1, lf, 0.1] - lf[0.1]
	,
	0
	,
	EquivalenceFunction -> NEqual
	,
	TestID->"RHTest-RealLine-CauchyInverse-Jump"
]

Test[
	CauchyInverse[+1, lf] + CauchyInverse[-1, lf] - lf // Values // Norm
	,
	0
	,
	EquivalenceFunction -> NEqual
	,
	TestID->"RHTest-RealLine-CauchyInverse-Fun-Jump"
]

Test[
	(lf // HilbertInverse // Hilbert) - lf // Values // Norm
	,
	0
	,
	EquivalenceFunction -> NEqual
	,
	TestID->"RHTest-RealLine-HilbertInverseThenHilbert"
]

Test[
	(lf // Hilbert // HilbertInverse) - lf // Values // Norm
	,
	0
	,
	EquivalenceFunction -> NEqual
	,
	TestID->"RHTest-RealLine-HilbertThenHilbertInverse"
]

Test[
	HilbertInverse[lf, 0.1] - HilbertInverse[lf][0.1]
	,
	0
	,
	EquivalenceFunction -> NEqual
	,
	TestID->"RHTest-RealLine-HilbertInverseVsHilbertInverseFun"
]




(* ::Section:: *)
(* Interval *)



f = ChebyshevT[2, #] &;
if = IFun[{0, 0, 1, 0, 0, 0, 0, 0} // InverseDCT, UnitInterval];


Test[
	if[3.] - f[3.]//Chop
	,
	0
	,
	TestID->"RHTest-Interval-BaryCentricVsPolynomial"
	,
	EquivalenceFunction -> NEqual
]

Test[
	CauchyBasis[if, 3, 1. I] - 
 NIntegrate[f[x]/(x - 1. I), {x, -1, 1}]/(2 \[Pi] I)
	,
	0
	,
	TestID->"RHTest-Interval-CauchyBasis"
	,
	EquivalenceFunction -> NEqual
]

Test[
	CauchyBasis[if, 
   3, {1. I, 1. I + 1., 
    1. I + 2.}] - (NIntegrate[f[x]/(x - #), {x, -1, 1}] & /@ {1. I, 
      1. I + 1., 1. I + 2.})/(2 \[Pi] I) // Norm
	,
	0
	,
	TestID->"RHTest-Interval-CauchyBasisList"
	,
	EquivalenceFunction -> NEqual
]

Test[
	CauchyBasis[+1, if, 3, 0.1] - CauchyBasis[-1, if, 3, 0.1] - if[0.1]
	,
	0
	,
	EquivalenceFunction -> NEqual
	,
	TestID->"RHTest-Interval-CauchyBasis-Jump"
]

f = Exp;
if = IFun[Exp, UnitInterval, 20];

Test[
	Cauchy[if, {1. I, 1. I + 1., 
    1. I + 2.}] - (NIntegrate[f[x]/(x - #), {x, -1, 1}] & /@ {1. I, 
      1. I + 1., 1. I + 2.})/(2 \[Pi] I) // Norm
	,
	0
	,
	TestID->"RHTest-Interval-CauchyList"
	,
	EquivalenceFunction -> NEqual
]

Test[
	Cauchy[{if}, {1. I, 1. I + 1., 
    1. I + 2.}] - (NIntegrate[f[x]/(x - #), {x, -1, 1}] & /@ {1. I, 
      1. I + 1., 1. I + 2.})/(2 \[Pi] I) // Norm
	,
	0
	,
	TestID->"RHTest-Interval-List-CauchyList"
	,
	EquivalenceFunction -> NEqual
]



Test[
	Hilbert[if, 0.1] - 
 NIntegrate[f[z]/(z - 0.1), {z, -1., 0.1, 1.}, 
   PrincipalValue -> True]/\[Pi]
	,
	0
	,
	EquivalenceFunction -> NEqual
	,
	TestID->"RHTest-Interval-Hilbert"
]


(* ::Section:: *)
(* Ray *)

hf = IFun[Sech, Line[{0, \[Infinity]}]];

Test[
	CauchyBasis[hf, 2, 5. I] - 
 NIntegrate[
   1/(z - 5. I ) (ChebyshevT[1, MapToInterval[hf, z]] - 
      ChebyshevT[0, MapToInterval[hf, z]]), {z, 
    0, \[Infinity]}]/(2 \[Pi] I)
	,
	0
	,
	TestID->"RHTest-Ray-CauchyBasis"
	,
	EquivalenceFunction -> NEqual
]

Test[
	Cauchy[hf, 5. I] - 
 NIntegrate[Sech[z]/(z - 5 I), {z, 0, \[Infinity]}, 
   WorkingPrecision -> 20]/(2 \[Pi] I)
	,
	0
	,
	TestID->"RHTest-Ray-Cauchy"
	,
	EquivalenceFunction -> NEqual
]

Test[
	Cauchy[+1, hf, 2.] - Cauchy[-1, hf, 2.] - hf[2.]
	,
	0
	,
	TestID->"RHTest-Ray-Jump"
	,
	EquivalenceFunction -> NEqual
]

Test[
	Hilbert[hf, 2.] - 
 NIntegrate[Sech[z]/(z - 2), {z, 0, 2, \[Infinity]}, 
   PrincipalValue -> True, WorkingPrecision -> 20]/( \[Pi] )
	,
	0
	,
	TestID->"RHTest-Ray-Hilbert"
	,
	EquivalenceFunction -> NEqual
]



(* ::Section:: *)
(* Singularity Data *)


(* ::Subsection:: *)
(* Interval *)

if = IFun[Exp[-5 (# - 0.1)^2] &, UnitInterval, 100];

Test[
	Chop[z = -1. + 
  0.0000001 I; k = 6; (LeftSingularityDataBasis[if, 
    k] // #[[1]] + #[[2]] Log[#[[3]] (z + 1)] &) - 
 CauchyBasis[if, k, z],10^(-5)]
	,
	0
	,
	TestID->"RHTest-SingularityDataBasis-Interval--1"
	,
	EquivalenceFunction -> NEqual
]

Test[
	Chop[z = 1. + 0.0000001 I; (RightSingularityDataBasis[if, 
    k] // #[[1]] + #[[2]] Log[#[[3]] (z - 1)] &) - 
 CauchyBasis[if, k, z],10^(-5)]
	,
	0
	,
	TestID->"RHTest-SingularityDataBasis-Interval-+1"
	,
	EquivalenceFunction -> NEqual
]

Test[
	Chop[z = -1. + 
  0.0000001 I; (if // 
    LeftSingularityData // #[[1]] + #[[2]] Log[#[[3]] (z + 1)] &) - 
 Cauchy[if, z],10^(-5)]
	,
	0
	,
	TestID->"RHTest-SingularityData-Interval--1"
	,
	EquivalenceFunction -> NEqual
]

Test[
	Chop[z = 1. + 0.0000001 I; (if // 
    RightSingularityData // #[[1]] + #[[2]] Log[#[[3]] (z - 1)] &) - 
 Cauchy[if, z],10^(-5)]
	,
	0
	,
	TestID->"RHTest-SingularityData-Interval-+1"
	,
	EquivalenceFunction -> NEqual
]

if = IFun[Exp, Line[{1. + 0.1 I, -2. - 3. I}]];


Test[
	Chop[z = LeftEndpoint[if] + 
  0.0000001 I; k = 6; (LeftSingularityDataBasis[if, 
    k] // #[[1]] + #[[2]] Log[#[[3]] (z - LeftEndpoint[if])] &) - 
 CauchyBasis[if, k, z],10^(-5)]
	,
	0
	,
	TestID->"RHTest-SingularityDataBasis-OtherInterval--1"
	,
	EquivalenceFunction -> NEqual
]

Test[
	Chop[z = RightEndpoint[if] + 
  0.0000001 I; k = 6; (RightSingularityDataBasis[if, 
    k] // #[[1]] + #[[2]] Log[#[[3]] (z - RightEndpoint[if])] &) - 
 CauchyBasis[if, k, z],10^(-5)]
	,
	0
	,
	TestID->"RHTest-SingularityDataBasis-OtherInterval-+1"
	,
	EquivalenceFunction -> NEqual
]

Test[
	Chop[z = LeftEndpoint[if] + 
  0.0000001 I; k = 6; (LeftSingularityData[
    if] // #[[1]] + #[[2]] Log[#[[3]] (z - LeftEndpoint[if])] &) - 
 Cauchy[if, z],10^(-5)]
	,
	0
	,
	TestID->"RHTest-SingularityData-OtherInterval--1"
	,
	EquivalenceFunction -> NEqual
]

Test[
	Chop[z = RightEndpoint[if] + 
  0.0000001 I; k = 6; (RightSingularityData[
    if] // #[[1]] + #[[2]] Log[#[[3]] (z - RightEndpoint[if])] &) - 
 Cauchy[if, z],10^(-5)]
	,
	0
	,
	TestID->"RHTest-SingularityData-OtherInterval-+1"
	,
	EquivalenceFunction -> NEqual
]


(* ::Subsection:: *)
(* Ray *)


hf = IFun[Sech, Line[{0, \[Infinity]}]];


Test[
	Chop[z = LeftEndpoint[hf] + 
  0.000001 I; (hf // 
    LeftSingularityData // (#[[
       1]] + #[[2]] Log[#[[3]] (z - LeftEndpoint[hf])] &)) - 
 Cauchy[hf, z],10^(-5)]
	,
	0
	,
	TestID->"RHTest-SingularityData-Ray--1"
	,
	EquivalenceFunction -> NEqual
]

hfR = (hf // ReverseOrientation);

Test[
	Chop[(hfR // RightSingularityData // (#[[
       1]] + #[[2]] Log[#[[3]] (z - RightEndpoint[hfR])] &)) - 
 Cauchy[hfR, z],10^(-5)]
	,
	0
	,
	TestID->"RHTest-SingularityData-ReverseRay--1"
	,
	EquivalenceFunction -> NEqual
]

Test[
	Chop[z = 0.00001 Exp[-0.9 I]; (LeftSingularityData[
    hf, -0.9] // (#[[1]] + #[[2]] Log[Abs[z]] &)) - Cauchy[hf, z],10^(-5)]
	,
	0
	,
	TestID->"RHTest-SingularityData-Ray-0"
	,
	EquivalenceFunction -> NEqual
]

Test[
	Chop[z = 0.00001 Exp[-0.00001 I]; (RightSingularityData[hfR, 
    z // Arg] // (#[[1]] + #[[2]] Log[Abs[z]] &)) - Cauchy[hfR, z],10^(-5)]
	,
	0
	,
	TestID->"RHTest-SingularityData-ReverseRay-0"
	,
	EquivalenceFunction -> NEqual
]

Test[
	Chop[z = 0.00001; (LeftSingularityData[+1, 
    hf] // (#[[1]] + #[[2]] Log[Abs[z]] &)) - Cauchy[+1, hf, z],10^(-5)]
	,
	0
	,
	TestID->"RHTest-SingularityData-Ray-0-+Limit"
	,
	EquivalenceFunction -> NEqual
]

Test[
	Chop[z = 0.00001; (LeftSingularityData[-1, 
    hf] // (#[[1]] + #[[2]] Log[Abs[z]] &)) - Cauchy[-1, hf, z],10^(-5)]
	,
	0
	,
	TestID->"RHTest-SingularityData-Ray-0--Limit"
	,
	EquivalenceFunction -> NEqual
]

Test[
	Chop[z = 0.00001; (RightSingularityData[+1, 
    hfR] // (#[[1]] + #[[2]] Log[Abs[z]] &)) - Cauchy[+1, hfR, z],10^(-5)]
	,
	0
	,
	TestID->"RHTest-SingularityData-ReverseRay-0-+Limit"
	,
	EquivalenceFunction -> NEqual
]

Test[
	Chop[z = 0.00001; (RightSingularityData[-1, 
    hfR] // (#[[1]] + #[[2]] Log[Abs[z]] &)) - Cauchy[-1, hfR, z],10^(-5)]
	,
	0
	,
	TestID->"RHTest-SingularityData-ReverseRay-0--Limit"
	,
	EquivalenceFunction -> NEqual
]



(* ::Section:: *)
(* Finite Part *)



(* ::Section:: *)
(* Cauchy Matrix *)


(* ::Section:: *)
(* RHSolve Sine Kernel *)


G[x_] := IFun[( {
      {0, Exp[I 2 # x]},
      {-Exp[-I 2 # x], 2}
     } ) &, UnitInterval, 20];
     

Test[
	RHSolve[G[0.]]
	,
	RHSolve[{G[0.]}][[1]]
	,
	TestID->"RHTest-RHSolve-SineKernel-ListAndNoList"
	,
	EquivalenceFunction -> NEqual
]

Test[
	RHSolver[G[0.]][G[0.]]
	,
	RHSolve[G[0.]]
	,
	TestID->"RHTest-RHSolve-SineKernel-RHSolver"
	,
	EquivalenceFunction -> NEqual
]

Test[
	RHSolverTop[G[0.]][G[0.]]
	,
	RHSolve[G[0.]][[1]]
	,
	TestID->"RHTest-RHSolve-SineKernel-RHSolverTop"
	,
	EquivalenceFunction -> NEqual
]


Test[
	RHSolveTop[G[0.]]
	,
	RHSolve[G[0.]][[1]]
	,
	TestID->"RHTest-RHSolve-SineKernel-RHSolveTop"
	,
	EquivalenceFunction -> NEqual
]

