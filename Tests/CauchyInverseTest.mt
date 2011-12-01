(* Mathematica Test File *)


(* ::Section:: *)
(* Real interval *)




(* ::Subsection:: *)
(*  Cauchy Inverse *)


eps = 10.^(-7);
a=0.1;
b=0.2 ;

cf = Fun[1/# &, Line[{a,b}]];
cfaeps = Fun[1/# &, Line[{a+eps,b}]];
cfbeps = Fun[1/# &, Line[{a,b+eps}]];

cfa = SetDomain[cf,cfaeps];
cfb = SetDomain[cf,cfbeps];


Test[
	Chop[
CauchyInverse[cf, 10000000.] 
,10.^(-5)]
	,
	0
	,
	TestID->"CauchyInverse-Decay"
]

Test[
	Chop[
(CauchyInverse[cfa, 1. I] - CauchyInverse[cf, 1. I])/eps - 
 CauchyInverseDomainGrad[1, 0][cf, 1. I] 
,10.^(-5)]
	,
	0
	,
	TestID->"CauchyInverse-DomainGrad-a"
]

Test[
	Chop[
(CauchyInverse[cfb, 1. I] - CauchyInverse[cf, 1. I])/eps - 
 CauchyInverseDomainGrad[0, 1][cf, 1. I] 
,10.^(-5)]
	,
	0
	,
	TestID->"CauchyInverse-DomainGrad-b"
]


Test[
	Chop[
(CauchyInverse[cfaeps,1.I]-CauchyInverse[cf,1.I])/eps-CauchyInverseDomainD[1,0][cf, 1. I] 
,10.^(-5)]
	,
	0
	,
	TestID->"CauchyInverse-DomainD-a"
]

Test[
	Chop[
(CauchyInverse[cfbeps,1.I]-CauchyInverse[cf,1.I])/eps-CauchyInverseDomainD[0,1][cf, 1. I] 
,10.^(-5)]
	,
	0
	,
	TestID->"CauchyInverse-DomainD-b"
]






(* ::Subsection:: *)
(*  Cauchy Inverse Integral *)


g[z_] = CauchyInverseIntegral[cf,z];

Test[
	Chop[
CauchyInverseIntegral[cf, 1000000.]-DCT[cf][[2]] ((Log[1/4 (RightEndpoint[cf] - LeftEndpoint[cf])] - 
     Log[IntervalToInnerCircle[
       MapToInterval[cf, 1000000.]]]))/(4 MapToIntervalD[cf, 0.`]) 
,10.^(-5)]
	,
	0
	,
	TestID->"CauchyInverseIntegral-Decay"
]

Test[
	Chop[
g'[1. I]-CauchyInverse[cf,1. I]
,10.^(-5)]
	,
	0
	,
	TestID->"CauchyInverseIntegral-D"
]

Test[
	Chop[
CauchyInverseIntegral[cf, a + eps^2 I] - CauchyInverseIntegral[+1, cf, a] 
,10.^(-5)]
	,
	0
	,
	TestID->"CauchyInverseIntegral+1-Jump-a"
]

Test[
	Chop[
CauchyInverseIntegral[cf, a - eps^2 I] - CauchyInverseIntegral[-1, cf, a] 
,10.^(-5)]
	,
	0
	,
	TestID->"CauchyInverseIntegral-1-Jump-a"
]

Test[
	Chop[
CauchyInverseIntegral[cf, b + eps^2 I] - CauchyInverseIntegral[+1, cf, b] 
,10.^(-5)]
	,
	0
	,
	TestID->"CauchyInverseIntegral+1-Jump-b"
]

Test[
	Chop[
CauchyInverseIntegral[cf, b - eps^2 I] - CauchyInverseIntegral[-1, cf, b] 
,10.^(-5)]
	,
	0
	,
	TestID->"CauchyInverseIntegral-1-Jump-b"
]

Test[
	Chop[
CauchyInverseIntegral[cf, .15 + eps^2 I] - CauchyInverseIntegral[+1, cf, .15] 
,10.^(-5)]
	,
	0
	,
	TestID->"CauchyInverseIntegral+1-Jump-mid"
]

Test[
	Chop[
CauchyInverseIntegral[cf, .15 - eps^2 I] - CauchyInverseIntegral[-1, cf, .15] 
,10.^(-5)]
	,
	0
	,
	TestID->"CauchyInverseIntegral-1-Jump-mid"
]

Test[
	Chop[
CauchyInverseIntegral[cf, eps^2 I] - CauchyInverseIntegral[+1, cf, 0.] 
,10.^(-5)]
	,
	0
	,
	TestID->"CauchyInverseIntegral+1-Jump-left"
]

Test[
	Chop[
CauchyInverseIntegral[cf, -eps^2 I] - CauchyInverseIntegral[-1, cf, 0.] 
,10.^(-5)]
	,
	0
	,
	TestID->"CauchyInverseIntegral-1-Jump-left"
]
  
  
  Test[
	Chop[
CauchyInverseIntegral[cf,1.+ eps^2 I] - CauchyInverseIntegral[+1, cf, 1.] 
,10.^(-5)]
	,
	0
	,
	TestID->"CauchyInverseIntegral+1-Jump-right"
]

Test[
	Chop[
CauchyInverseIntegral[cf,1. -eps^2 I] - CauchyInverseIntegral[-1, cf, 1.] 
,10.^(-5)]
	,
	0
	,
	TestID->"CauchyInverseIntegral-1-Jump-right"
]
  
 
(* ::Subsection:: *)
(* SPCauchy Inverse Integral D *)

Test[
Chop[(SPCauchyInverseIntegral[cfa] - SPCauchyInverseIntegral[cf])/eps - 
  SPCauchyInverseIntegralDomainGrad[1, 0][cf] // Norm,10^(-6)]
  ,
  0
  ,
  TestID->"SPCauchyInverseIntegralDomaainGrad-a"
]
  
Test[
	Chop[
		(SPCauchyInverseIntegral[cfaeps] - SPCauchyInverseIntegral[cf])/eps - 
 SPCauchyInverseIntegralDomainD[1, 0][cf],10^(-5)]//Norm
 ,
 0
 ,
 TestID->"SPCauchyInverseIntegralDomainD-a"
]

Test[
	Chop[
		(SPCauchyInverseIntegral[cfbeps] - SPCauchyInverseIntegral[cf])/eps - 
 SPCauchyInverseIntegralDomainD[0, 1][cf],10^(-5)]//Norm
 ,
 0
 ,
 TestID->"SPCauchyInverseIntegralDomainD-b"
]


Test[
	Chop[
		1/eps (CauchyInverse[SPCauchyInverseIntegral[cfa], 1. I] - 
    CauchyInverse[SPCauchyInverseIntegral[cf], 
     1. I])- (
     CauchyInverse[SPCauchyInverseIntegralDomainGrad[1, 0][cf],1. I] + 
     CauchyInverseDomainGrad[1, 0][SPCauchyInverseIntegral[cf], 1. I]),10^(-5)]
 ,
 0
 ,
 TestID->"CauchySPCauchyInverseIntegralDomainGrad-a"
]

Test[
	Chop[
		1/eps (CauchyInverse[SPCauchyInverseIntegral[cfaeps], 1. I] - 
    CauchyInverse[SPCauchyInverseIntegral[cf], 
     1. I])- (
     CauchyInverse[SPCauchyInverseIntegralDomainD[1, 0][cf],1. I] + 
     CauchyInverseDomainGrad[1, 0][SPCauchyInverseIntegral[cf], 1. I]),10^(-5)]
 ,
 0
 ,
 TestID->"CauchySPCauchyInverseIntegralDomainD-a"
]
Test[
	Chop[
		1/eps (CauchyInverse[SPCauchyInverseIntegral[cfbeps], 1. I] - 
    CauchyInverse[SPCauchyInverseIntegral[cf], 
     1. I])- (CauchyInverse[SPCauchyInverseIntegralDomainD[0, 1][cf],
     1. I] + 
   CauchyInverseDomainGrad[0, 1][SPCauchyInverseIntegral[cf], 1. I]),10^(-5)]
 ,
 0
 ,
 TestID->"CauchySPCauchyInverseIntegralDomainD-b"
]

Test[
	Chop[
		1/eps (CauchyInverse[+1,SPCauchyInverseIntegral[cfaeps], .15] - 
    CauchyInverse[+1,SPCauchyInverseIntegral[cf], 
     .15])- (
     CauchyInverse[+1,SPCauchyInverseIntegralDomainD[1, 0][cf],.15] + 
     CauchyInverseDomainGrad[1, 0][+1,SPCauchyInverseIntegral[cf], .15]),10^(-5)]
 ,
 0
 ,
 TestID->"CauchySPCauchyInverseIntegralDomainD-a-mid+1"
]

Test[
	Chop[
		1/eps (CauchyInverse[+1,SPCauchyInverseIntegral[cfaeps],a+eps] - 
    CauchyInverse[+1,SPCauchyInverseIntegral[cf], 
     a])- (
     CauchyInverse[+1, SPCauchyInverseIntegralDomainD[1, 0][cf], a]),10^(-5)]
 ,
 0
 ,
 TestID->"CauchySPCauchyInverseIntegralDomainD-a+1"
]


Test[
	Chop[
	(CauchyInverseIntegral[cfa, 1. I] - 
  CauchyInverseIntegral[cf, 1. I])/eps - 
 CauchyInverseIntegralDomainGrad[1, 0][cf, 1. I]
   ,10^(-5)]
 ,
 0
 ,
 TestID->"CauchyInverseIntegralDomainGrad-a"
]

Test[
	Chop[
	(CauchyInverseIntegral[cfb, 1. I] - 
  CauchyInverseIntegral[cf, 1. I])/eps - 
 CauchyInverseIntegralDomainGrad[0, 1][cf, 1. I]
   ,10^(-5)]
 ,
 0
 ,
 TestID->"CauchyInverseIntegralDomainGrad-b"
]

Test[
	Chop[
	(CauchyInverseIntegral[+1,cfa, .15] - 
  CauchyInverseIntegral[+1,cf, .15])/eps - 
 CauchyInverseIntegralDomainGrad[1, 0][+1,cf, .15]
   ,10^(-5)]
 ,
 0
 ,
 TestID->"CauchyInverseIntegralDomainGrad-a-mid+1"
]

Test[
	Chop[
	(CauchyInverseIntegral[+1,cfa, 0.] - 
  CauchyInverseIntegral[+1,cf, 0.])/eps - 
 CauchyInverseIntegralDomainGrad[1, 0][+1,cf, 0.]
   ,10^(-5)]
 ,
 0
 ,
 TestID->"CauchyInverseIntegralDomainGrad-a-left+1"
]

Test[
	Chop[
	(CauchyInverseIntegral[+1,cfa, a+eps] - 
  CauchyInverseIntegral[+1,cf, a])/eps - 
 CauchyInverseIntegralDomainGradBoundary[1, 0][+1,cf, a]
   ,10^(-5)]
 ,
 0
 ,
 TestID->"CauchyInverseIntegralDomainGradBoundary-a-a+1"
]


Test[
	Chop[
	(CauchyInverseIntegral[+1,cfaeps, .15] - 
  CauchyInverseIntegral[+1,cf, .15])/eps - 
 CauchyInverseIntegralDomainD[1, 0][+1,cf, .15]
   ,10^(-5)]
 ,
 0
 ,
 TestID->"CauchyInverseIntegralDomainD-a-mid+1"
]


   
Test[
	Chop[(CauchyInverseIntegral[cfaeps, 1. I] - 
  CauchyInverseIntegral[cf, 1. I])/eps - 
 CauchyInverseIntegralDomainD[1, 0][cf, 1. I],10.^(-5)]
	,
	0
	,
	TestID->"CauchyInverseIntegralD-Complex-a"
] 

Test[
	Chop[(CauchyInverseIntegral[cfbeps, 1. I] - 
  CauchyInverseIntegral[cf, 1. I])/eps - 
 CauchyInverseIntegralDomainD[0, 1][cf, 1. I],10.^(-5)]
	,
	0
	,
	TestID->"CauchyInverseIntegralD-Complex-b"
] 



Test[
	Chop[(CauchyInverseIntegral[+1,cfaeps, a+eps] - 
  CauchyInverseIntegral[+1,cf, a])/eps - 
 CauchyInverseIntegralDomainDBoundary[1, 0][+1,cf, a],10.^(-5)]
	,
	0
	,
	TestID->"+CauchyInverseIntegralDBoundary-Domain-a-a"
] 


Test[
	Chop[(CauchyInverseIntegral[-1,cfaeps, a+eps] - 
  CauchyInverseIntegral[-1,cf, a])/eps - 
 CauchyInverseIntegralDomainDBoundary[1, 0][-1,cf, a],10.^(-5)]
	,
	0
	,
	TestID->"-CauchyInverseIntegralDBoundary-Domain-a-a"
] 


Test[
	Chop[(CauchyInverseIntegral[-1,cfbeps, b+eps] - 
  CauchyInverseIntegral[-1,cf, b])/eps - 
 CauchyInverseIntegralDomainDBoundary[0,1][-1,cf, b],10.^(-5)]
	,
	0
	,
	TestID->"-CauchyInverseIntegralDBoundary-Domain-b-b"
] 




(* ::Section:: *)
(* Complex interval *)


(* ::Subsection:: *)
(*  Cauchy Inverse *)



eps = 10.^(-7);
a=0.1;
b=0.2 I;

cf = Fun[1/# &, Line[{a,b}]];
cfaeps = Fun[1/# &, Line[{a+eps,b}]];
cfbeps = Fun[1/# &, Line[{a,b+eps}]];
cfa = SetDomain[cf, Line[{a + eps, b}]];
cfb = SetDomain[cf, Line[{a, b + eps}]];


cfMZ = 

Test[
	Chop[
CauchyInverse[cf, 10000000.] 
,10.^(-5)]
	,
	0
	,
	TestID->"CauchyInverse-Decay-Complex"
]

Test[
	Chop[
(CauchyInverse[cfaeps, 1. I] -CauchyInverse[cf, 1. I])/eps - 
CauchyInverseDomainD[1,0][cf,1. I]
,10.^(-5)]
	,
	0
	,
	TestID->"CauchyInverse-DomainD-a-z"
]

Test[
	Chop[
(CauchyInverse[cfbeps, 1. I] -CauchyInverse[cf, 1. I])/eps - 
CauchyInverseDomainD[0,1][cf,1. I]
,10.^(-5)]
	,
	0
	,
	TestID->"CauchyInverse-DomainD-b-z"
]



(* ::Subsection:: *)
(* Cauchy Inverse Integral *)

g[z_] = CauchyInverseIntegral[cf,z];

Test[
	Chop[
CauchyInverseIntegral[cf, 1000000.] -DCT[cf][[2]] ((Log[1/4 (RightEndpoint[cf] - LeftEndpoint[cf])] - 
     Log[IntervalToInnerCircle[
       MapToInterval[cf, 1000000.]]]))/(4 MapToIntervalD[cf, 0.`]) 
,10.^(-5)]
	,
	0
	,
	TestID->"CauchyInverseIntegral-Decay-Complex"
]

Test[
	Chop[
g'[1. I]-CauchyInverse[cf,1. I]
,10.^(-5)]
	,
	0
	,
	TestID->"CauchyInverseIntegral-D-Complex"
]

Test[
	Chop[
CauchyInverseIntegral[cf, a - eps^2] - CauchyInverseIntegral[+1, cf, a] 
,10.^(-5)]
	,
	0
	,
	TestID->"CauchyInverseIntegral-1-Jump-Complex"
]
  
   
Test[
	Chop[(CauchyInverseIntegral[cfaeps, 1. I] - 
  CauchyInverseIntegral[cf, 1. I])/eps - 
 CauchyInverseIntegralDomainD[1, 0][cf, 1. I],10.^(-5)]
	,
	0
	,
	TestID->"CauchyInverseIntegralD-Complex-a-Complex"
] 

Test[
	Chop[(CauchyInverseIntegral[cfbeps, 1. I] - 
  CauchyInverseIntegral[cf, 1. I])/eps - 
 CauchyInverseIntegralDomainD[0, 1][cf, 1. I],10.^(-5)]
	,
	0
	,
	TestID->"CauchyInverseIntegralD-Complex-b-Complex"
] 

Test[
	Chop[
(CauchyInverseIntegral[+1, cfb, a] - 
  CauchyInverseIntegral[+1, cf, a])/eps - 
 CauchyInverseIntegralDomainGrad[0, 1][+1, cf, a]
  ,10.^(-5)]
	,
	0
	,
	TestID->"+CauchyInverseIntegralD-Domain-b-a-Complex"
] 

Test[
	Chop[
(CauchyInverseIntegral[+1, cfa, b] - 
  CauchyInverseIntegral[+1, cf, b])/eps - 
 CauchyInverseIntegralDomainGrad[1, 0][+1, cf, b]
  ,10.^(-5)]
	,
	0
	,
	TestID->"+CauchyInverseIntegralD-Domain-a-b-Complex"
] 



Test[
	Chop[
		(CauchyInverseIntegral[+1,cfaeps, a+eps] - 
  CauchyInverseIntegral[+1,cf, a])/eps - 
 CauchyInverseIntegralDomainDBoundary[1, 0][+1,cf, a]
 ,10.^(-5)]
	,
	0
	,
	TestID->"+CauchyInverseIntegralD-Domain-a-a-Complex"
] 


Test[
	Chop[(CauchyInverseIntegral[-1,cfaeps, a+eps] - 
  CauchyInverseIntegral[-1,cf, a])/eps - 
 CauchyInverseIntegralDomainDBoundary[1, 0][-1,cf, a],10.^(-5)]
	,
	0
	,
	TestID->"-CauchyInverseIntegralD-Domain-a-a-Complex"
] 





(* ::Section:: *)
(* List *)


eps = 10.^(-7);
a=0.1;
b=0.2 I;
c=0.3;
d=-0.5 I + 0.4;
cf = Fun[1/# &, {Line[{a,b}], Line[{c,d}]}];
cfaeps = Fun[
   1/# &, {Line[{0.1 + eps, 0.2 I}], Line[{0.3, -0.5 I + 0.4}]}];
cfbeps = Fun[
   1/# &, {Line[{0.1 , 0.2 I+ eps}], Line[{0.3, -0.5 I + 0.4}]}];
cfceps = Fun[
   1/# &, {Line[{0.1 , 0.2 I}], Line[{0.3+ eps, -0.5 I + 0.4}]}];
cfdeps = Fun[
   1/# &, {Line[{0.1 , 0.2 I}], Line[{0.3, -0.5 I + 0.4+ eps}]}];  


Test[
	Chop[
CauchyInverseIntegral[cf[[1]], a - eps^2] - 
 CauchyInverseIntegral[+1, cf[[1]], a]
 ,10.^(-5)]
	,
	0
	,
	TestID->"CauchyInverseIntegral-List-1-Jump"
] 
  
   
Test[
	Chop[(CauchyInverseIntegral[cfaeps, 1. I] - 
  CauchyInverseIntegral[cf, 1. I])/eps - 
 CauchyInverseIntegralDomainD[{1, 0}, {0, 0}][cf, 1. I],10.^(-5)]
	,
	0
	,
	TestID->"CauchyInverseIntegralD-List-Complex-a"
] 

Test[
	Chop[(CauchyInverseIntegral[cfbeps, 1. I] - 
  CauchyInverseIntegral[cf, 1. I])/eps - 
 CauchyInverseIntegralDomainD[{0, 1}, {0, 0}][cf, 1. I],10.^(-5)]
	,
	0
	,
	TestID->"CauchyInverseIntegralD-List-Complex-b"
] 

Test[
	Chop[(CauchyInverseIntegral[cfceps, 1. I] - 
  CauchyInverseIntegral[cf, 1. I])/eps - 
 CauchyInverseIntegralDomainD[{0, 0}, {1, 0}][cf, 1. I],10.^(-5)]
	,
	0
	,
	TestID->"CauchyInverseIntegralD-List-Complex-c"
] 

Test[
	Chop[(CauchyInverseIntegral[cfdeps, 1. I] - 
  CauchyInverseIntegral[cf, 1. I])/eps - 
 CauchyInverseIntegralDomainD[{0, 0}, {0, 1}][cf, 1. I],10.^(-5)]
	,
	0
	,
	TestID->"CauchyInverseIntegralD-List-Complex-d"
] 

Test[
	Chop[(CauchyInverseIntegral[+1,cfceps, a] - 
  CauchyInverseIntegral[+1,cf, a])/eps - 
 CauchyInverseIntegralDomainD[{0, 0}, {1, 0}][+1,cf, a],10.^(-5)]
	,
	0
	,
	TestID->"+CauchyInverseIntegralD-List-Domain-c-a"
] 


Test[
	Chop[(CauchyInverseIntegral[-1,cfceps, a] - 
  CauchyInverseIntegral[-1,cf, a])/eps - 
 CauchyInverseIntegralDomainD[{0, 0}, {1, 0}][-1,cf, a],10.^(-5)]
	,
	0
	,
	TestID->"-CauchyInverseIntegralD-List-Domain-c-a"
] 

Test[
	Chop[(CauchyInverseIntegral[+1,cfdeps, a] - 
  CauchyInverseIntegral[+1,cf, a])/eps - 
 CauchyInverseIntegralDomainD[{0, 0}, {0, 1}][+1,cf, a],10.^(-5)]
	,
	0
	,
	TestID->"+CauchyInverseIntegralD-List-Domain-d-a"
] 


Test[
	Chop[(CauchyInverseIntegral[-1,cfdeps, a] - 
  CauchyInverseIntegral[-1,cf, a])/eps - 
 CauchyInverseIntegralDomainD[{0, 0}, {0, 1}][-1,cf, a],10.^(-5)]
	,
	0
	,
	TestID->"-CauchyInverseIntegralD-List-Domain-d-a"
] 


Test[
Chop[
	(CauchyInverseIntegral[CauchyInverseCurves[cfbeps][[2]], a] - 
  CauchyInverseIntegral[CauchyInverseCurves[cf][[2]], a])/eps - 
 CauchyInverseIntegral[CauchyInverseCurvesD[{0,1},{0,0}][cf][[2]], a],
 10^(-5)]
 	,
	0
	,
	TestID->"-CauchyInverseIntegralD-List-Domain-b-a-second"
] 



Test[
	Chop[(CauchyInverseIntegral[-1,cfbeps, a] - 
  CauchyInverseIntegral[-1,cf, a])/eps - 
 CauchyInverseIntegralDomainD[{0, 1}, {0, 0}][-1,cf, a],10.^(-5)]
	,
	0
	,
	TestID->"-CauchyInverseIntegralD-List-Domain-b-a"
] 



Test[
	Chop[(CauchyInverseIntegral[+1,cfaeps, a+eps] - 
  CauchyInverseIntegral[+1,cf, a])/eps - 
 CauchyInverseIntegralDomainDBoundary[{1, 0}, {0, 0}][+1,cf, a],10.^(-5)]
	,
	0
	,
	TestID->"+CauchyInverseIntegralDBoundary-List-Domain-a-a"
] 





Test[
	Chop[(CauchyInverseIntegral[-1,cfaeps, a+eps] - 
  CauchyInverseIntegral[-1,cf, a])/eps - 
 CauchyInverseIntegralDomainDBoundary[{1, 0}, {0, 0}][-1,cf, a],10.^(-5)]
	,
	0
	,
	TestID->"-CauchyInverseIntegralDBoundary-List-Domain-a-a"
] 

Test[
	Chop[(CauchyInverseIntegral[-1,cfbeps, b+eps] - 
  CauchyInverseIntegral[-1,cf, b])/eps - 
 CauchyInverseIntegralDomainDBoundary[{0, 1}, {0, 0}][-1,cf, b],10.^(-5)]
	,
	0
	,
	TestID->"-CauchyInverseIntegralDBoundary-List-Domain-b-b"
] 

Test[
	Chop[(CauchyInverseIntegral[-1,cfceps, c+eps] - 
  CauchyInverseIntegral[-1,cf, c])/eps - 
 CauchyInverseIntegralDomainDBoundary[{0, 0}, {1, 0}][-1,cf, c],10.^(-5)]
	,
	0
	,
	TestID->"-CauchyInverseIntegralDBoundary-List-Domain-c-c"
] 

Test[
	Chop[(CauchyInverseIntegral[-1,cfdeps, d+eps] - 
  CauchyInverseIntegral[-1,cf, d])/eps - 
 CauchyInverseIntegralDomainDBoundary[{0, 0}, {0, 1}][-1,cf, d],10.^(-5)]
	,
	0
	,
	TestID->"-CauchyInverseIntegralDBoundary-List-Domain-d-d"
] 



(* ::Subsection:: *)
(* Real List *)


eps = 10.^(-7);
a=-.3;
b=-.2 ;
c=0.3;
d=0.4;
cf = Fun[1/# &, {Line[{a,b}], Line[{c,d}]}];
cfaeps = Fun[
   1/# &, {Line[{a + eps, b}], Line[{c,d}]}];
cfbeps = Fun[
   1/# &, {Line[{a , b+ eps}], Line[{c,d}]}];
cfceps = Fun[
   1/# &, {Line[{a , b}], Line[{c+ eps, d}]}];
cfdeps = Fun[
   1/# &, {Line[{a , b}], Line[{c, d+eps}]}];  

Test[
	Chop[(CauchyInverseIntegral[+1,cfaeps, 0] - 
  CauchyInverseIntegral[+1,cf, 0])/eps - 
 CauchyInverseIntegralDomainD[{1, 0}, {0, 0}][+1,cf, 0],10.^(-5)]
	,
	0
	,
	TestID->"+CauchyInverseIntegralDBoundary-List-Domain-a-0-Bug"
] 

Test[
	Chop[(CauchyInverseIntegral[+1,cfaeps, eps I] - 
  CauchyInverseIntegral[+1,cf, eps I])/eps - 
 CauchyInverseIntegralDomainD[{1, 0}, {0, 0}][+1,cf, eps I],10.^(-5)]
	,
	0
	,
	TestID->"+CauchyInverseIntegralDBoundary-List-Domain-a-epsI-Bug"
] 

Test[
CauchyInverseIntegralDomainD[{0, 0}, {1, 0}][+1, cf, 0] - 
  CauchyInverseIntegralDomainD[{0, 0}, {1, 0}][cf, eps I] // Chop
	,
	0
	,
	TestID->"+CauchyInverseIntegralDBoundary-List-Domain-c-0-Bug-pert"
] 



Test[
	Chop[(CauchyInverseIntegral[+1,cfceps, 0] - 
  CauchyInverseIntegral[+1,cf, 0])/eps - 
 CauchyInverseIntegralDomainD[{0, 0}, {1, 0}][+1,cf, 0],10.^(-5)]
	,
	0
	,
	TestID->"+CauchyInverseIntegralDBoundary-List-Domain-c-0-Bug"
] 

Test[
	Chop[(CauchyInverseIntegral[cfceps, eps I] - 
  CauchyInverseIntegral[cf, eps I])/eps - 
 CauchyInverseIntegralDomainD[{0, 0}, {1, 0}][cf, eps I],10.^(-5)]
	,
	0
	,
	TestID->"+CauchyInverseIntegralDBoundary-List-Domain-c-epsI-Bug"
] 


