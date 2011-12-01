(* ::Package:: *)

(************************************************************************)
(* This file was generated automatically by the Mathematica front end.  *)
(* It contains Initialization cells from a Notebook file, which         *)
(* typically will have the same name as this file except ending in      *)
(* ".nb" instead of ".m".                                               *)
(*                                                                      *)
(* This file is intended to be loaded into the Mathematica kernel using *)
(* the package loading commands Get or Needs.  Doing so is equivalent   *)
(* to using the Evaluate Initialization Cells menu command in the front *)
(* end.                                                                 *)
(*                                                                      *)
(* DO NOT EDIT THIS FILE.  This entire file is regenerated              *)
(* automatically each time the parent Notebook file is saved in the     *)
(* Mathematica front end.  Any changes you make to this file will be    *)
(* overwritten.                                                         *)
(************************************************************************)



BeginPackage["RiemannHilbert`",{"RiemannHilbert`Common`"}];



RHSolve::usage="RHSolve[ifunlist] returns u such that IdentityMatrix[2] + CauchyTransform[u,z] has the jumps specified by ifunlist.";
RHSolver::usage="RHSolver[ifunlist] constructs an anonymous function R that precomputes the matrices used in RHSolve, so that if the domain and points of ifunlist are the same as ifunlist2, then R[ifunlist2] == RHSolve[ifunlist2].";
RHSolveTop::usage="RHSolveTop[ifunlist] returns the top row of RHSolve.";
RHSolverTop::usage="RHSolverTop[ifunlist] constructs an anonymous function R that precomputes the matrices used in RHSolveTop, so that if the domain and points of ifunlist are the same as ifunlist2, then R[ifunlist2] == RHSolveTop[ifunlist2].";
FredholmDet;
DiagonalMatrixQ;
Parametrix;
ParametrixBranch;
RHWellPosed;
ZeroSumCondition;
ScaledRHSolver;
ScaledCauchyOperator;
ConstructCurve;
RHSolverMatrix;
CauchySeriesAtInfinity;
SowCondition::usage="Option for RHSolve";
SparseSolve::usage="Option for RHSolve";
Begin["Private`"];


ScalarToVectorMatrix:=BlockDiagonalMatrix[{#,#}]&;
ScalarToMatrixMatrix:=BlockDiagonalMatrix[{#,#,#,#}]&;

CauchyMatrix[s_?SignQ,f_?VectorFunQ,g_?FunQ]:=CauchyMatrix[s,f[[1]],g]//ScalarToVectorMatrix;
CauchyMatrix[s_?SignQ,f_?MatrixFunQ,g_?FunQ]:=CauchyMatrix[s,f[[1,1]],g]//ScalarToMatrixMatrix;

CauchyMatrix[s_?SignQ,l:{__?MatrixFunQ},g_?FunQ]:=CauchyMatrix[s,#[[1,1]]&/@l,g]//ScalarToMatrixMatrix;
CauchyMatrix[s_?SignQ,l:{__?MatrixFunQ},l2_List]:=CauchyMatrix[s,#[[1,1]]&/@l,l2]//ScalarToMatrixMatrix;

CauchyMatrix[s_?SignQ,l:{__?VectorFunQ},g_?FunQ]:=CauchyMatrix[s,#[[1]]&/@l,g]//ScalarToVectorMatrix;
CauchyMatrix[s_?SignQ,l:{__?VectorFunQ},l2_List]:=CauchyMatrix[s,#[[1]]&/@l,l2]//ScalarToVectorMatrix;
CauchyMatrix[s_?SignQ,l:{__?FunQ},g_?FunQ]:=RightJoin@@(CauchyMatrix[s,#,g]&/@l);
CauchyMatrix[s_?SignQ,l:{__?FunQ},l2:{__?FunQ}]:=Join@@(CauchyMatrix[s,l,#]&/@l2);

CauchyMatrix[s_?SignQ,f_?FunQ]:=CauchyMatrix[s,f,f];
CauchyMatrix[s_?SignQ,f:{__?FunQ}]:=CauchyMatrix[s,f,f];

CauchySeriesAtInfinity[U_]:=-(U//DomainIntegrate)/(2 \[Pi]\[NonBreakingSpace]I);



MakeMachineNumber[x_]:=Chop[x,$MinMachineNumber]//N;
RHSolverTop[GGIn:{__?FunQ},opts:OptionsPattern[] ]:=
RHSolverTop[CauchyMatrix[-1,#[[1,1]]&/@GGIn]//MakeMachineNumber,opts];


RHSolverMatrix[R_RHSolverTop,GG_List]:=Module[{matm},
matm=R[[1]]//ScalarToVectorMatrix;
SparseIdentityMatrix[Length[matm]]+(SparseIdentityMatrix[Length[matm]]-MakeMachineNumber[RightMatrixMultVectorFun[GG]]).matm//MakeMachineNumber];

RHSolverMatrix[R_RHSolverTop,GG_?FunQ]:=RHSolverMatrix[R,{GG}];

RHSolverMatrix[GG_]:=RHSolverMatrix[GG//RHSolverTop,GG];


RHSolverTop[matmS_,opts:OptionsPattern[{SowCondition->False,SparseSolve->False}]][GG_List,GR:{__List}]:=Module[{matt,solv,sol,cond,matm},
matm=matmS//ScalarToVectorMatrix;
matt =RHSolverMatrix[RHSolverTop[matmS,opts],GG];
If[OptionValue[SowCondition],
cond=LinearAlgebra`MatrixConditionNumber[matt];
Sow[cond];
];
solv=LinearSolve[If[OptionValue[SparseSolve],matt,matt//Normal]];
sol=FromValueList[#,solv[#//ToValueList//MakeMachineNumber]]&/@GR
];

RHSolverTop[matm_,opts:OptionsPattern[]][GG_List] := RHSolverTop[matm,opts][GG,#[[1,All]]&/@SubtractIdentityMatrix[GG]];

RHSolverTop[matm_,opts:OptionsPattern[]][GG_List,GR_List]:=
RHSolverTop[matm,opts][GG,{GR}]//First;


RHSolveTop[GG_List,GI_List,opts:OptionsPattern[]]:=Module[{matp,matm,GITop},
RHSolverTop[GG,opts][GG,GI]];

RHSolveTop[GG_List,opts:OptionsPattern[]]:=
RHSolveTop[GG,#[[1,All]]&/@(#-IdentityMatrix[2]&/@#&/@GG),opts];


RHSolver[GGIn_List,opts:OptionsPattern[]]:=Module[{rsolvt},
rsolvt=RHSolverTop[GGIn,opts];
RHSolver[rsolvt]
];

RHSolver[rsolvt_RHSolverTop][GG_List,GR_List] :=ToArrayFun[Join[{ToArrayOfFuns[#[[1]]]},{ToArrayOfFuns[#[[2]]]}]]&/@Thread[rsolvt[GG,{#[[1,All]]&/@GR,#[[2,All]]&/@GR}]];
RHSolver[rsolvt_RHSolverTop][GG_List]:=RHSolver[rsolvt][GG,(#-IdentityMatrix[2]&/@#&/@GG)];

RHSolve[GG_List,GI_List,opts:OptionsPattern[]]:=
RHSolver[GG,opts][GG,GI];
RHSolve[GG_List,opts:OptionsPattern[]]:=
RHSolve[GG,#-IdentityMatrix[2]&/@#&/@GG,opts];


RHSolve[GG_?FunQ]:=RHSolve[{GG}]//First;
RHSolveTop[GG_?FunQ]:=RHSolveTop[{GG}]//First;
RHSolver[GG_?FunQ]:=RHSolver[{GG}];
RHSolverTop[GG_?FunQ]:=RHSolverTop[{GG}];
RHSolverTop[matm_,opts:OptionsPattern[]][GG_?FunQ] := RHSolverTop[matm,opts][{GG}]//First;
RHSolverTop[matm_,opts:OptionsPattern[]][GG_?FunQ,GR_?FunQ]:=
RHSolverTop[matm,opts][{GG},{GR}]//First;
RHSolver[matm_,opts:OptionsPattern[]][GG_?FunQ] := RHSolver[matm,opts][{GG}]//First;
RHSolver[matm_,opts:OptionsPattern[]][GG_?FunQ,GR_?FunQ]:=
RHSolver[matm,opts][{GG},{GR}]//First;



FunValueListOperator[mat_,g_][f:{__?ScalarFunQ}]:=FromValueList[g,mat.ToValueList[f]];
FunValueListOperator[mat_,g_][f:{__?VectorFunQ}]:=FromValueList[{0,0}&/@#&/@g,ScalarToVectorMatrix[mat].ToValueList[f]];
FunValueListOperator[mat_,g_][f:{__?MatrixFunQ}]:=FromValueList[({
 {0, 0},
 {0, 0}
})&/@#&/@g,ScalarToMatrixMatrix[mat].ToValueList[f]];

FunValueListOperator[mat_][f_]:=FunValueListOperator[mat,f][f];

SetDomain[op_FunValueListOperator,d_]:=FunValueListOperator[op[[1]],SetDomain[op[[2]],d]];

CauchyOperator[1,R_RHSolverTop]:=
FunValueListOperator[R[[1]]+SparseIdentityMatrix[Length[R[[1]]]]];
CauchyOperator[-1,R_RHSolverTop]:=
FunValueListOperator[R[[1]]];
CauchyOperator[s_?SignQ,R_RHSolver]:=
CauchyOperator[s,R[[1]]];


FredholmDet[K_,a_,b_,m_]:=
Module[{w,x},
{x,w}=GaussianQuadratureWeights[m,a,b]//Thread;
w=Sqrt[w];
Det[IdentityMatrix[m]-(Transpose[{w}].{w})Outer[K,x,x]]];
FredholmDet[K_,a_,\[Infinity],m_]:=Module[{Ks,x,w,\[Phi]},
\[Phi][s_][x_]:=s+10 Tan[\[Pi] x/2];
Ks[s_][x_,y_]:=Sqrt[\[Phi][s]'[x]\[Phi][s]'[y]] K[\[Phi][s][x],\[Phi][s][y]];
{x,w}=GaussianQuadratureWeights[m,0,1]//Thread;
w=Sqrt[w];
IdentityMatrix[m]-(Transpose[{w}].{w})Outer[Ks[a],x,x]//Det
];


RHWellPosed[GG_,Gg_]:=Dot@@(If[LeftEndpoint[#]~NEqual~(Gg),
First[#],
Inverse[Last[#]]]&/@
SelectWithPoint[GG,Gg])//Chop;
RHWellPosed[GG_]:=RHWellPosed[GG,#]&/@Endpoints[GG];
ZeroSumCondition[GG_,Gg_]:=
Plus@@(If[LeftEndpoint[#]~NEqual~(Gg),
First[#],
-Last[#]]&/@
SelectWithPoint[GG,Gg])//Chop;
ZeroSumCondition[GG_]:=ZeroSumCondition[GG,#]&/@Endpoints[GG];



IteratedRHSolver[{{{z0_,sc_},lsGs_},Grest___},{},R_,gms_]:=Module[{Cus,uz0,usc,us,gl},
gl=Fun[Function[z,#[[1]][z0+z/sc]],Sequence@@#[[2]]]&/@Thread[{lsGs,gms}];
IteratedRHSolver[{Grest},{{z0,sc,R[gl]}},R,gms]
];
IteratedRHSolver[{},Uls_,_,_]:=Uls;
IteratedRHSolver[{{{z0_,sc_},lsGs_},Grest___},Uls_,R_,gms_]:=Module[{Cus,uz0,usc,us,gl},
gl=Fun[Function[z,#[[1]][z0+z/sc]],Sequence@@#[[2]]]&/@Thread[{lsGs,gms}];
Cus=(Dot@@#)&/@Thread[
Function[uls,
{uz0,usc,us}=uls;
FromValueList[gl,Cauchy[us,((z0+Points[gl]/sc)-uz0)usc]//ToMatrixOfLists//Flatten]//AddIdentityMatrix
]/@Uls
];
IteratedRHSolver[{Grest},Join[{{z0,sc,R[#[[1]].#[[2]].Inverse[#[[1]]]&/@Thread[{Cus,gl}]]}},Uls],R,gms]
];


OuterIteratedRHSolver[{},Uls_,_]:=Uls;
OuterIteratedRHSolver[{{{scale_,domains_},jumps_,R_},rest___},Uls_,x_]:=OuterIteratedRHSolver[{rest},IteratedRHSolver[Thread[{scale[x]//Transpose,jumps//Transpose}],Uls,R,domains],x];


ConvertIteratedToStandardFunList[ifl_]:=(Function[sfl,Fun[Values[#],((#//Domain)/sfl[[2]]+sfl[[1]])]&/@Last[sfl]]/@ifl);


ScaledRHSolver[l:{{_,_}..}]:=Module[{scale,domains},
ScaledRHSolver[l,
Function[Gl,
{scale,domains}=Gl;
RHSolver[Function[domain,Fun[IdentityMatrix[2]&,Sequence@@domain]]/@domains]
]/@l
]
];
ScaledRHSolver[l:{{_,_}..},Rs_][x_,Gf_]:=OuterIteratedRHSolver[Thread[{l,Gf,Rs}],{},x]//ConvertIteratedToStandardFunList;





ScaledRHSolver[{scs_,gms_}]:=
ScaledRHSolver[{scs,gms},RHSolver[Fun[IdentityMatrix[2]&,Sequence@@##]&/@gms]];
ScaledRHSolver[{scs_,gms_},R_RHSolver][x_,gs_]:=IteratedRHSolver[Thread[{scs[x]//Transpose,gs//Transpose}],{},R,gms]//ConvertIteratedToStandardFunList;


CauchyMatrix[s_?SignQ,l1:{{_?IntervalDomainQ,_Integer}..},l2:{{_?IntervalDomainQ,_Integer}..}]:=CauchyMatrix[s,IFun[Array[0&,#[[2]]],#[[1]]]&/@l1,IFun[Array[0&,#[[2]]],#[[1]]]&/@l2];
CauchyOperator[s_?SignQ,l1:{{_?IntervalDomainQ,_Integer}..},l2:{{_?IntervalDomainQ,_Integer}..}]:=FunValueListOperator[CauchyMatrix[s,l1,l2],IFun[Array[0&,#[[2]]],#[[1]]]&/@l2];

(** We want to be able to change the domain after the fact below **)

CauchyOperator[s_?SignQ,l1:{{_?DomainQ,_Integer}..},l2:{{_?DomainQ,_Integer}..},l3:{{_?DomainQ,_Integer}..}]:=FunValueListOperator[CauchyMatrix[s,l1,l2],IFun[Array[0&,#[[2]]],#[[1]]]&/@l3];


IteratedScaledCauchy[{{z0_,sc_},rest___},{},gms_,i_]:=IteratedScaledCauchy[{rest},{{z0,sc,i,{}}},gms,i];
IteratedScaledCauchy[{{z0_,sc_},rest___},Uls_,gms_,i_]:=Module[{Cmats,uz0,usc,us,ugms,uCmats,uR,uz0scs,ui},
Cmats=Function[ulscs,
{uz0,usc,ui,uCmats}=ulscs;
CauchyOperator[+1,gms[[ui]],{usc (z0+#[[1]]/sc-uz0),#[[2]]}&/@gms[[i]],gms[[ui]]]
]/@Uls;
IteratedScaledCauchy[{rest},Join[Uls,{{z0,sc,i,Cmats}}],gms,i]];
IteratedScaledCauchy[{},Uls_,_,_]:=Uls;
OuterIteratedScaledCauchy[{},Uls_,_,_,_]:=Uls;
OuterIteratedScaledCauchy[{scales_,rest___},Uls_,gms_,i_,x_]:=
OuterIteratedScaledCauchy[{rest},IteratedScaledCauchy[scales[x]//Transpose,Uls,gms,i],gms,i+1,x];



IteratedRHSolver[{},Uls_,_,_]:=Uls;IteratedRHSolver[{{{z0_,sc_,i_,Cmats_},Gf_},Grest___},{},Rs_,gms_]:=Module[{gl},
gl=Fun[Function[z,#[[1]][z0+z/sc]],Sequence@@#[[2]]]&/@Thread[{Gf,gms[[i]]}];
IteratedRHSolver[{Grest},{{z0,sc,Rs[[i]][gl]}},Rs,gms]
];
IteratedRHSolver[{{{z0_,sc_,i_,Cmats_},Gf_},Grest___},Uls_,Rs_,gms_]:=Module[{Cus,uz0,usc,us,gl,Cs},
gl=Fun[Function[z,#[[1]][z0+z/sc]],Sequence@@#[[2]]]&/@Thread[{Gf,gms[[i]]}];
Cus=(Dot@@#)&/@Thread[
Function[uls,
{{uz0,usc,us},Cs}=uls;
Cs[us]//AddIdentityMatrix
]/@Reverse[Thread[{Uls,Cmats}]]
];

IteratedRHSolver[{Grest},Join[Uls,{{z0,sc,Rs[[i]][#[[1]].#[[2]].Inverse[#[[1]]]&/@Thread[{Cus,gl}]]}}],Rs,gms]
];


ScaledRHSolver[l:{{_,_}..},Rs_][x_]:=
ScaledRHSolver[OuterIteratedScaledCauchy[First/@l,{},Last/@l,1,x],Rs,Last/@l];
ScaledRHSolver[l:{{_,_,_,_}..},Rs:{__RHSolver},gms:{{{_?DomainQ,_}..}..}][Gl_]:=
IteratedRHSolver[Thread[{l,Flatten[Transpose/@Gl,1]}],{},Rs,gms]//ConvertIteratedToStandardFunList;
ScaledRHSolver[l:{{_,_,_,_}..},R_RHSolver,gms:{{_?DomainQ,_}..}][Gl_]:=
ScaledRHSolver[l,{R},{gms}][{Gl}];
ScaledRHSolver[{scs_,gms_},R_][x_]:=
ScaledRHSolver[OuterIteratedScaledCauchy[{scs},{},{gms},1,x],R,gms];


(**TODO: ScaledCauchyOperator currently only works with two symmetric scaled graphs **)
(**TODO: ScaledCauchyOperator currently returns C[u][z] + IdentityMatrix[2] **)

ScaledCauchyOperator[CmR_,Cmat_List][Ucx_]:=Join[AddIdentityMatrix[Cmat[[1]][Ucx[[2]]]]~FunListDot~AddIdentityMatrix[CmR[Ucx[[1]]]],AddIdentityMatrix[CmR[Ucx[[2]]]]~FunListDot~AddIdentityMatrix[Cmat[[2]][Ucx[[1]]]]
];
ScaledCauchyOperator[s_?SignQ,slvrx_ScaledRHSolver]:=ScaledCauchyOperator[CauchyOperator[s,slvrx[[2]]],SetDomain[slvrx[[1,2,-1,1]],#]&/@(Function[scs,Function[gm,scs[[1]]+scs[[2]]gm[[1]]]/@Last[slvrx]]/@First[slvrx])];

CauchyOperator[ScaledCauchyOperator[CmR_,Cmat_List]]:=FunValueListOperator[
Join[
RightJoin[CmR[[1]],Cmat[[1,1]]],
RightJoin[Cmat[[2,1]],CmR[[1]]]
],Join@@scCm[[2,All,-1]]];

ConstructCurve[crvs:{{_,_}..},gls_List,x_]:=Join@@(ConstructCurve[Sequence@@#,x]&/@Thread[{crvs,gls}]);ConstructCurve[{scs_,domain_},gfs_,x_]:=Join@@(ConstructCurve[#,domain]&/@Thread[{scs[x]//Transpose,gfs//Transpose}]);
ConstructCurve[{{z0_,sc_},gs_},domain_]:=Fun[#[[1]],#[[2,1]]/sc +z0,#[[2,2]]]&/@Thread[{gs,domain}]; 


End[];
EndPackage[];
