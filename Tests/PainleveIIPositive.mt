(* Mathematica Test File *)

maxMemory = 256*1024*1024;

(** The test value is from PainleveIISmall. This is unstable, therefore very few digits are accurate **)

Test[
	MemoryConstrained[
	Chop[PainleveII[{1, 2, 1/3}, 10.] - (-0.14802788933835595` + 0.3307431712872643` I ), 10^-5],
  maxMemory]
	,
	0
	,
	TestID->"PainleveII-Positive-ComparedToSmall"
	,
	EquivalenceFunction -> NEqual
]

Test[
	MemoryConstrained[
	PainleveII[{1, 2, 1/3}, 10.] + PainleveII[-{1, 2, 1/3}, 10.] ,
  maxMemory]
	,
	0
	,
	TestID->"PainleveII-Positive-Stokes"
	,
	EquivalenceFunction -> NEqual
]