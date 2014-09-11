{-# OPTIONS_GHC -w #-}
module Parser.Bcparser where

import Char
import ComputationGraph.Program
import ComputationGraph.Instructions
import ComputationGraph.State

-- parser produced by Happy Version 1.18.6

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7
	| HappyAbsSyn8 t8
	| HappyAbsSyn9 t9
	| HappyAbsSyn10 t10
	| HappyAbsSyn11 t11
	| HappyAbsSyn12 t12
	| HappyAbsSyn13 t13
	| HappyAbsSyn14 t14
	| HappyAbsSyn15 t15

action_0 (16) = happyShift action_3
action_0 (4) = happyGoto action_4
action_0 (5) = happyGoto action_2
action_0 _ = happyReduce_2

action_1 (16) = happyShift action_3
action_1 (5) = happyGoto action_2
action_1 _ = happyFail

action_2 (16) = happyShift action_3
action_2 (4) = happyGoto action_6
action_2 (5) = happyGoto action_2
action_2 _ = happyReduce_2

action_3 (55) = happyShift action_5
action_3 _ = happyFail

action_4 (59) = happyAccept
action_4 _ = happyFail

action_5 (17) = happyShift action_7
action_5 _ = happyFail

action_6 _ = happyReduce_1

action_7 (55) = happyShift action_8
action_7 _ = happyFail

action_8 (53) = happyShift action_9
action_8 _ = happyFail

action_9 (18) = happyShift action_11
action_9 (6) = happyGoto action_10
action_9 _ = happyFail

action_10 _ = happyReduce_3

action_11 (55) = happyShift action_12
action_11 _ = happyFail

action_12 (19) = happyShift action_13
action_12 _ = happyFail

action_13 (55) = happyShift action_14
action_13 _ = happyFail

action_14 (21) = happyShift action_15
action_14 (53) = happyShift action_16
action_14 _ = happyFail

action_15 (20) = happyShift action_18
action_15 _ = happyFail

action_16 (25) = happyShift action_17
action_16 _ = happyFail

action_17 (55) = happyShift action_20
action_17 _ = happyFail

action_18 (22) = happyShift action_19
action_18 _ = happyFail

action_19 (25) = happyShift action_24
action_19 _ = happyFail

action_20 (53) = happyShift action_23
action_20 (7) = happyGoto action_21
action_20 (8) = happyGoto action_22
action_20 _ = happyReduce_7

action_21 (24) = happyShift action_28
action_21 _ = happyFail

action_22 (53) = happyShift action_23
action_22 (7) = happyGoto action_27
action_22 (8) = happyGoto action_22
action_22 _ = happyReduce_7

action_23 (53) = happyShift action_26
action_23 _ = happyFail

action_24 (55) = happyShift action_25
action_24 _ = happyFail

action_25 (53) = happyShift action_23
action_25 (7) = happyGoto action_30
action_25 (8) = happyGoto action_22
action_25 _ = happyReduce_7

action_26 _ = happyReduce_8

action_27 _ = happyReduce_6

action_28 (55) = happyShift action_29
action_28 _ = happyFail

action_29 (23) = happyShift action_34
action_29 (9) = happyGoto action_32
action_29 (10) = happyGoto action_33
action_29 _ = happyReduce_10

action_30 (24) = happyShift action_31
action_30 _ = happyFail

action_31 (55) = happyShift action_37
action_31 _ = happyFail

action_32 _ = happyReduce_5

action_33 (23) = happyShift action_34
action_33 (9) = happyGoto action_36
action_33 (10) = happyGoto action_33
action_33 _ = happyReduce_10

action_34 (55) = happyShift action_35
action_34 _ = happyFail

action_35 (53) = happyShift action_39
action_35 _ = happyFail

action_36 _ = happyReduce_9

action_37 (23) = happyShift action_34
action_37 (9) = happyGoto action_38
action_37 (10) = happyGoto action_33
action_37 _ = happyReduce_10

action_38 _ = happyReduce_4

action_39 (53) = happyShift action_40
action_39 _ = happyFail

action_40 (26) = happyShift action_41
action_40 _ = happyFail

action_41 (55) = happyShift action_42
action_41 _ = happyFail

action_42 (53) = happyShift action_45
action_42 (11) = happyGoto action_43
action_42 (12) = happyGoto action_44
action_42 _ = happyReduce_13

action_43 (32) = happyShift action_49
action_43 _ = happyFail

action_44 (53) = happyShift action_45
action_44 (11) = happyGoto action_48
action_44 (12) = happyGoto action_44
action_44 _ = happyReduce_13

action_45 (27) = happyShift action_46
action_45 (53) = happyShift action_47
action_45 _ = happyFail

action_46 (28) = happyShift action_51
action_46 _ = happyFail

action_47 _ = happyReduce_14

action_48 _ = happyReduce_12

action_49 (55) = happyShift action_50
action_49 _ = happyFail

action_50 (29) = happyShift action_54
action_50 (13) = happyGoto action_53
action_50 _ = happyFail

action_51 (53) = happyShift action_52
action_51 _ = happyFail

action_52 _ = happyReduce_15

action_53 _ = happyReduce_11

action_54 (55) = happyShift action_55
action_54 _ = happyFail

action_55 (54) = happyShift action_56
action_55 _ = happyFail

action_56 (30) = happyShift action_57
action_56 _ = happyFail

action_57 (55) = happyShift action_58
action_57 _ = happyFail

action_58 (54) = happyShift action_59
action_58 _ = happyFail

action_59 (31) = happyShift action_60
action_59 _ = happyFail

action_60 (55) = happyShift action_61
action_60 _ = happyFail

action_61 (54) = happyShift action_64
action_61 (14) = happyGoto action_62
action_61 (15) = happyGoto action_63
action_61 _ = happyReduce_18

action_62 _ = happyReduce_16

action_63 (54) = happyShift action_64
action_63 (14) = happyGoto action_66
action_63 (15) = happyGoto action_63
action_63 _ = happyReduce_18

action_64 (55) = happyShift action_65
action_64 _ = happyFail

action_65 (33) = happyShift action_67
action_65 (34) = happyShift action_68
action_65 (35) = happyShift action_69
action_65 (36) = happyShift action_70
action_65 (37) = happyShift action_71
action_65 (38) = happyShift action_72
action_65 (39) = happyShift action_73
action_65 (40) = happyShift action_74
action_65 (41) = happyShift action_75
action_65 (42) = happyShift action_76
action_65 (43) = happyShift action_77
action_65 (44) = happyShift action_78
action_65 (45) = happyShift action_79
action_65 (46) = happyShift action_80
action_65 (47) = happyShift action_81
action_65 (48) = happyShift action_82
action_65 (49) = happyShift action_83
action_65 (50) = happyShift action_84
action_65 (51) = happyShift action_85
action_65 (52) = happyShift action_86
action_65 _ = happyFail

action_66 _ = happyReduce_17

action_67 (54) = happyShift action_99
action_67 _ = happyFail

action_68 (54) = happyShift action_98
action_68 _ = happyFail

action_69 (54) = happyShift action_95
action_69 (57) = happyShift action_96
action_69 (58) = happyShift action_97
action_69 _ = happyFail

action_70 (53) = happyShift action_94
action_70 _ = happyFail

action_71 (53) = happyShift action_93
action_71 _ = happyFail

action_72 (53) = happyShift action_92
action_72 _ = happyFail

action_73 (53) = happyShift action_91
action_73 _ = happyFail

action_74 (53) = happyShift action_90
action_74 _ = happyFail

action_75 _ = happyReduce_29

action_76 _ = happyReduce_30

action_77 _ = happyReduce_31

action_78 _ = happyReduce_32

action_79 (54) = happyShift action_88
action_79 (56) = happyShift action_89
action_79 _ = happyFail

action_80 _ = happyReduce_35

action_81 _ = happyReduce_36

action_82 _ = happyReduce_37

action_83 _ = happyReduce_38

action_84 _ = happyReduce_39

action_85 _ = happyReduce_40

action_86 (54) = happyShift action_87
action_86 _ = happyFail

action_87 _ = happyReduce_41

action_88 _ = happyReduce_33

action_89 (54) = happyShift action_103
action_89 _ = happyFail

action_90 (54) = happyShift action_102
action_90 _ = happyFail

action_91 _ = happyReduce_27

action_92 (53) = happyShift action_101
action_92 _ = happyFail

action_93 (53) = happyShift action_100
action_93 _ = happyFail

action_94 _ = happyReduce_24

action_95 _ = happyReduce_23

action_96 _ = happyReduce_22

action_97 _ = happyReduce_21

action_98 _ = happyReduce_20

action_99 _ = happyReduce_19

action_100 _ = happyReduce_25

action_101 _ = happyReduce_26

action_102 _ = happyReduce_28

action_103 _ = happyReduce_34

happyReduce_1 = happySpecReduce_2  4 happyReduction_1
happyReduction_1 (HappyAbsSyn4  happy_var_2)
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1 : happy_var_2
	)
happyReduction_1 _ _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_0  4 happyReduction_2
happyReduction_2  =  HappyAbsSyn4
		 ([]
	)

happyReduce_3 = happyReduce 6 5 happyReduction_3
happyReduction_3 ((HappyAbsSyn6  happy_var_6) `HappyStk`
	(HappyTerminal (TokVar happy_var_5)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn5
		 (ClassDecl (happy_var_5,happy_var_6)
	) `HappyStk` happyRest

happyReduce_4 = happyReduce 13 6 happyReduction_4
happyReduction_4 ((HappyAbsSyn9  happy_var_13) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_10) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (ClassBody "" happy_var_10 happy_var_13
	) `HappyStk` happyRest

happyReduce_5 = happyReduce 11 6 happyReduction_5
happyReduction_5 ((HappyAbsSyn9  happy_var_11) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn7  happy_var_8) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokVar happy_var_5)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (ClassBody happy_var_5 happy_var_8 happy_var_11
	) `HappyStk` happyRest

happyReduce_6 = happySpecReduce_2  7 happyReduction_6
happyReduction_6 (HappyAbsSyn7  happy_var_2)
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_1 : happy_var_2
	)
happyReduction_6 _ _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_0  7 happyReduction_7
happyReduction_7  =  HappyAbsSyn7
		 ([]
	)

happyReduce_8 = happySpecReduce_2  8 happyReduction_8
happyReduction_8 (HappyTerminal (TokVar happy_var_2))
	(HappyTerminal (TokVar happy_var_1))
	 =  HappyAbsSyn8
		 (FDecl (happy_var_1,happy_var_2)
	)
happyReduction_8 _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_2  9 happyReduction_9
happyReduction_9 (HappyAbsSyn9  happy_var_2)
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_1 : happy_var_2
	)
happyReduction_9 _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_0  9 happyReduction_10
happyReduction_10  =  HappyAbsSyn9
		 ([]
	)

happyReduce_11 = happyReduce 10 10 happyReduction_11
happyReduction_11 ((HappyAbsSyn13  happy_var_10) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn11  happy_var_7) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokVar happy_var_4)) `HappyStk`
	(HappyTerminal (TokVar happy_var_3)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 (MDecl (happy_var_4,happy_var_7,happy_var_3,happy_var_10)
	) `HappyStk` happyRest

happyReduce_12 = happySpecReduce_2  11 happyReduction_12
happyReduction_12 (HappyAbsSyn11  happy_var_2)
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1 : happy_var_2
	)
happyReduction_12 _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_0  11 happyReduction_13
happyReduction_13  =  HappyAbsSyn11
		 ([]
	)

happyReduce_14 = happySpecReduce_2  12 happyReduction_14
happyReduction_14 (HappyTerminal (TokVar happy_var_2))
	(HappyTerminal (TokVar happy_var_1))
	 =  HappyAbsSyn12
		 (ParMeth (happy_var_1,happy_var_2)
	)
happyReduction_14 _ _  = notHappyAtAll 

happyReduce_15 = happyReduce 4 12 happyReduction_15
happyReduction_15 ((HappyTerminal (TokVar happy_var_4)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokVar happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn12
		 (ParMeth (happy_var_1,happy_var_4)
	) `HappyStk` happyRest

happyReduce_16 = happyReduce 9 13 happyReduction_16
happyReduction_16 ((HappyAbsSyn14  happy_var_9) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokInt happy_var_6)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TokInt happy_var_3)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn13
		 (MBody (happy_var_3,happy_var_6,happy_var_9)
	) `HappyStk` happyRest

happyReduce_17 = happySpecReduce_2  14 happyReduction_17
happyReduction_17 (HappyAbsSyn14  happy_var_2)
	(HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn14
		 (happy_var_1 : happy_var_2
	)
happyReduction_17 _ _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_0  14 happyReduction_18
happyReduction_18  =  HappyAbsSyn14
		 ([]
	)

happyReduce_19 = happyReduce 4 15 happyReduction_19
happyReduction_19 ((HappyTerminal (TokInt happy_var_4)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn15
		 (Load happy_var_4
	) `HappyStk` happyRest

happyReduce_20 = happyReduce 4 15 happyReduction_20
happyReduction_20 ((HappyTerminal (TokInt happy_var_4)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn15
		 (Store happy_var_4
	) `HappyStk` happyRest

happyReduce_21 = happyReduce 4 15 happyReduction_21
happyReduction_21 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn15
		 (Push Null
	) `HappyStk` happyRest

happyReduce_22 = happyReduce 4 15 happyReduction_22
happyReduction_22 (_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn15
		 (Push Unit
	) `HappyStk` happyRest

happyReduce_23 = happyReduce 4 15 happyReduction_23
happyReduction_23 ((HappyTerminal (TokInt happy_var_4)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn15
		 (Push (IntVal happy_var_4)
	) `HappyStk` happyRest

happyReduce_24 = happyReduce 4 15 happyReduction_24
happyReduction_24 ((HappyTerminal (TokVar happy_var_4)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn15
		 (New happy_var_4
	) `HappyStk` happyRest

happyReduce_25 = happyReduce 5 15 happyReduction_25
happyReduction_25 ((HappyTerminal (TokVar happy_var_5)) `HappyStk`
	(HappyTerminal (TokVar happy_var_4)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn15
		 (Getfield happy_var_4 happy_var_5
	) `HappyStk` happyRest

happyReduce_26 = happyReduce 5 15 happyReduction_26
happyReduction_26 ((HappyTerminal (TokVar happy_var_5)) `HappyStk`
	(HappyTerminal (TokVar happy_var_4)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn15
		 (Putfield happy_var_4 happy_var_5
	) `HappyStk` happyRest

happyReduce_27 = happyReduce 4 15 happyReduction_27
happyReduction_27 ((HappyTerminal (TokVar happy_var_4)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn15
		 (Checkcast happy_var_4
	) `HappyStk` happyRest

happyReduce_28 = happyReduce 5 15 happyReduction_28
happyReduction_28 ((HappyTerminal (TokInt happy_var_5)) `HappyStk`
	(HappyTerminal (TokVar happy_var_4)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn15
		 (Invoke happy_var_4 happy_var_5
	) `HappyStk` happyRest

happyReduce_29 = happySpecReduce_3  15 happyReduction_29
happyReduction_29 _
	_
	_
	 =  HappyAbsSyn15
		 (Return
	)

happyReduce_30 = happySpecReduce_3  15 happyReduction_30
happyReduction_30 _
	_
	_
	 =  HappyAbsSyn15
		 (Pop
	)

happyReduce_31 = happySpecReduce_3  15 happyReduction_31
happyReduction_31 _
	_
	_
	 =  HappyAbsSyn15
		 (IAdd
	)

happyReduce_32 = happySpecReduce_3  15 happyReduction_32
happyReduction_32 _
	_
	_
	 =  HappyAbsSyn15
		 (ISub
	)

happyReduce_33 = happyReduce 4 15 happyReduction_33
happyReduction_33 ((HappyTerminal (TokInt happy_var_4)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn15
		 (Goto happy_var_4
	) `HappyStk` happyRest

happyReduce_34 = happyReduce 5 15 happyReduction_34
happyReduction_34 ((HappyTerminal (TokInt happy_var_5)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn15
		 (Goto (negate happy_var_5)
	) `HappyStk` happyRest

happyReduce_35 = happySpecReduce_3  15 happyReduction_35
happyReduction_35 _
	_
	_
	 =  HappyAbsSyn15
		 (CmpEq
	)

happyReduce_36 = happySpecReduce_3  15 happyReduction_36
happyReduction_36 _
	_
	_
	 =  HappyAbsSyn15
		 (CmpNeq
	)

happyReduce_37 = happySpecReduce_3  15 happyReduction_37
happyReduction_37 _
	_
	_
	 =  HappyAbsSyn15
		 (CmpGeq
	)

happyReduce_38 = happySpecReduce_3  15 happyReduction_38
happyReduction_38 _
	_
	_
	 =  HappyAbsSyn15
		 (BNot
	)

happyReduce_39 = happySpecReduce_3  15 happyReduction_39
happyReduction_39 _
	_
	_
	 =  HappyAbsSyn15
		 (BAnd
	)

happyReduce_40 = happySpecReduce_3  15 happyReduction_40
happyReduction_40 _
	_
	_
	 =  HappyAbsSyn15
		 (BOr
	)

happyReduce_41 = happyReduce 4 15 happyReduction_41
happyReduction_41 ((HappyTerminal (TokInt happy_var_4)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn15
		 (IfFalse happy_var_4
	) `HappyStk` happyRest

happyNewToken action sts stk [] =
	action 59 59 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokClass -> cont 16;
	TokName -> cont 17;
	TokClassBody -> cont 18;
	TokSuper -> cont 19;
	TokNone -> cont 20;
	TokLess -> cont 21;
	TokGreater -> cont 22;
	TokMethod -> cont 23;
	TokMethods -> cont 24;
	TokFields -> cont 25;
	TokParameters -> cont 26;
	TokLBracket -> cont 27;
	TokRBracket -> cont 28;
	TokMaxStack -> cont 29;
	TokMaxVars -> cont 30;
	TokByteCode -> cont 31;
	TokMethodBody -> cont 32;
	TokLoad -> cont 33;
	TokStore -> cont 34;
	TokPush -> cont 35;
	TokNew -> cont 36;
	TokGetField -> cont 37;
	TokPutField -> cont 38;
	TokCheckcast -> cont 39;
	TokInvoke -> cont 40;
	TokReturn -> cont 41;
	TokPop -> cont 42;
	TokIadd -> cont 43;
	TokIsub -> cont 44;
	TokGoto -> cont 45;
	TokCmpeq -> cont 46;
	TokCmpNeq -> cont 47;
	TokCmpGeq -> cont 48;
	TokNot -> cont 49;
	TokAnd -> cont 50;
	TokOr -> cont 51;
	TokIffalse -> cont 52;
	TokVar happy_dollar_dollar -> cont 53;
	TokInt happy_dollar_dollar -> cont 54;
	TokColon -> cont 55;
	TokMinus -> cont 56;
	TokUnit -> cont 57;
	TokNull -> cont 58;
	_ -> happyError' (tk:tks)
	}

happyError_ tk tks = happyError' (tk:tks)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Monad HappyIdentity where
    return = HappyIdentity
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (return) a
happyError' :: () => [(Token)] -> HappyIdentity a
happyError' = HappyIdentity . parseError

bcparse tks = happyRunIdentity happySomeParser where
  happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError :: [Token] -> a
parseError _ = error "Parse error"

data Token
      = TokClass
      | TokName
      | TokClassBody
      | TokSuper
      | TokNone
      | TokLess
      | TokGreater
      | TokMethod
      | TokMethods
      | TokFields
      | TokParameters
      | TokLBracket 
      | TokRBracket
      | TokMaxStack
      | TokMaxVars
      | TokByteCode
      | TokMethodBody
      | TokLoad
      | TokStore
      | TokPush
      | TokNew
      | TokGetField
      | TokPutField
      | TokCheckcast
      | TokInvoke
      | TokReturn
      | TokPop
      | TokIadd
      | TokIsub
      | TokGoto
      | TokCmpeq
      | TokCmpGeq
      | TokNot
      | TokAnd
      | TokOr
      | TokIffalse
      | TokVar String
      | TokInt Int
      | TokColon
      | TokMinus
      | TokNull
      | TokUnit
      | TokCmpNeq
 deriving Show

lexer :: String -> [Token]
lexer [] = []
lexer (c:cs)
    | isSpace c = lexer cs
    | isAlpha c = lexerVar (c:cs)
    | isDigit c = lexNum (c:cs)
lexer (':':cs) = TokColon : lexer cs
lexer ('-':cs) = TokMinus : lexer cs
lexer ('<':cs) = TokLess : lexer cs
lexer ('>':cs) = TokGreater : lexer cs
lexer ('[':cs) = TokLBracket : lexer cs
lexer (']':cs) = TokRBracket : lexer cs

lexNum cs = TokInt (read num) : lexer rest
      where (num,rest) = span isDigit cs

lexerVar cs =
    case span isAlpha cs of
        ("Class",rest) -> TokClass : lexer rest
        ("Name",rest) -> TokName : lexer rest
        ("Classbody",rest) -> TokClassBody : lexer rest
        ("Bytecode",rest) -> TokByteCode : lexer rest
        ("Superclass",rest) -> TokSuper : lexer rest
        ("None",rest) -> TokNone : lexer rest
        ("Fields",rest) -> TokFields : lexer rest
        ("Methods",rest) -> TokMethods : lexer rest
        ("Method",rest) -> TokMethod : lexer rest
        ("Parameters",rest) -> TokParameters : lexer rest
        ("Methodbody",rest) -> TokMethodBody : lexer rest
        ("MaxStack",rest) -> TokMaxStack : lexer rest
        ("MaxVars",rest) -> TokMaxVars : lexer rest
        ("Load",rest) -> TokLoad : lexer rest
        ("Store",rest) -> TokStore : lexer rest
        ("Push",rest) -> TokPush : lexer rest
        ("New",rest) -> TokNew : lexer rest
        ("Getfield",rest) -> TokGetField : lexer rest
        ("Putfield",rest) -> TokPutField : lexer rest
        ("Checkcast",rest) -> TokCheckcast : lexer rest
        ("Invoke",rest) -> TokInvoke : lexer rest
        ("Return",rest) -> TokReturn : lexer rest
        ("Pop",rest) -> TokPop : lexer rest
        ("Iadd",rest) -> TokIadd : lexer rest
        ("Isub",rest) -> TokIsub : lexer rest
        ("Goto",rest) -> TokGoto : lexer rest
        ("CmpEq",rest) -> TokCmpeq : lexer rest
        ("CmpGeq",rest) -> TokCmpGeq : lexer rest
        ("CmpNeq",rest) -> TokCmpNeq : lexer rest
        ("INot",rest) -> TokNot : lexer rest
        ("BAnd",rest) -> TokAnd : lexer rest
        ("BOr",rest) -> TokOr : lexer rest
        ("IfFalse",rest) -> TokIffalse : lexer rest
        ("null",rest) -> TokNull : lexer rest
        ("unit",rest) -> TokUnit : lexer rest
        (var,rest) -> TokVar var : lexer rest
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 30 "templates/GenericTemplate.hs" #-}








{-# LINE 51 "templates/GenericTemplate.hs" #-}

{-# LINE 61 "templates/GenericTemplate.hs" #-}

{-# LINE 70 "templates/GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is (1), it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
	happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
	 (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

{-# LINE 148 "templates/GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let (i) = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
	 sts1@(((st1@(HappyState (action))):(_))) ->
        	let r = fn stk in  -- it doesn't hurt to always seq here...
       		happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
        happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))
       where (sts1@(((st1@(HappyState (action))):(_)))) = happyDrop k ((st):(sts))
             drop_stk = happyDropStk k stk

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
       happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))
       where (sts1@(((st1@(HappyState (action))):(_)))) = happyDrop k ((st):(sts))
             drop_stk = happyDropStk k stk





             new_state = action


happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 246 "templates/GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail  (1) tk old_st _ stk =
--	trace "failing" $ 
    	happyError_ tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
						(saved_tok `HappyStk` _ `HappyStk` stk) =
--	trace ("discarding state, depth " ++ show (length stk))  $
	action (1) (1) tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
	action (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--	happySeq = happyDoSeq
-- otherwise it emits
-- 	happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.

{-# LINE 311 "templates/GenericTemplate.hs" #-}
{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
