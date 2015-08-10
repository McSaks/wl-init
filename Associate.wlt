BeginTestSection["Associate.Test"]

VerificationTest[(* 1 *)
	CompoundExpression[Needs["Associate`"], Set[associate, Associate`Associate], associate[Association[Rule["a", 1], RuleDelayed["b", Plus[2, 2]]], Rule["c", 3]]]
	,
	Association[Rule["a", 1], RuleDelayed["b", Plus[2, 2]], Rule["c", 3]]	
]

VerificationTest[(* 2 *)
	associate[Association[Rule["a", 1], RuleDelayed["b", Plus[2, 2]]], RuleDelayed["c", Plus[3, 3]]]
	,
	Association[Rule["a", 1], RuleDelayed["b", Plus[2, 2]], RuleDelayed["c", Plus[3, 3]]]	
]

VerificationTest[(* 3 *)
	associate[Association[Rule["a", 1], RuleDelayed["b", Plus[2, 2]]], RuleDelayed["a", Plus[3, 3]]]
	,
	Association[RuleDelayed["a", Plus[3, 3]], RuleDelayed["b", Plus[2, 2]]]	
]

VerificationTest[(* 4 *)
	associate[Association[Rule["a", 1], RuleDelayed["b", Plus[2, 2]]], Rule["b", 3]]
	,
	Association[Rule["a", 1], Rule["b", 3]]	
]

VerificationTest[(* 5 *)
	associate[List[Rule["a", 1], RuleDelayed["b", Plus[2, 2]]], Rule["b", 3]]
	,
	Association[Rule["a", 1], Rule["b", 3]]	
]

VerificationTest[(* 6 *)
	associate[List[Rule["a", 1], RuleDelayed["b", Plus[2, 2]]], RuleDelayed["c", Plus[3, 3]]]
	,
	Association[Rule["a", 1], RuleDelayed["b", Plus[2, 2]], RuleDelayed["c", Plus[3, 3]]]	
]

VerificationTest[(* 7 *)
	associate[List[Rule["a", 1], RuleDelayed["b", Plus[2, 2]]], Association[RuleDelayed["c", Plus[3, 3]], Rule["a", 4]]]
	,
	Association[Rule["a", 4], RuleDelayed["b", Plus[2, 2]], RuleDelayed["c", Plus[3, 3]]]	
]

VerificationTest[(* 8 *)
	associate[Association[Rule["a", 1], RuleDelayed["b", Plus[2, 2]]], List[RuleDelayed["c", Plus[3, 3]], Rule["a", 4]]]
	,
	Association[Rule["a", 4], RuleDelayed["b", Plus[2, 2]], RuleDelayed["c", Plus[3, 3]]]	
]

VerificationTest[(* 9 *)
	associate[Association[Rule["a", 1], RuleDelayed["b", Plus[2, 2]]], Association[RuleDelayed["c", Plus[3, 3]], Rule["a", 4]]]
	,
	Association[Rule["a", 4], RuleDelayed["b", Plus[2, 2]], RuleDelayed["c", Plus[3, 3]]]	
]

VerificationTest[(* 10 *)
	associate[List[Rule["a", 1], RuleDelayed["b", Plus[2, 2]]], List[RuleDelayed["c", Plus[3, 3]], Rule["a", 4]]]
	,
	Association[Rule["a", 4], RuleDelayed["b", Plus[2, 2]], RuleDelayed["c", Plus[3, 3]]]	
]

EndTestSection[]
