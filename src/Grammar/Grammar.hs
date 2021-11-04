{-# OPTIONS_GHC -w #-}
module Grammar.Grammar (toAST, sToTree, prettyPrintS, parse' ) where

import Data.Char
import Lexer.Lexer (Token(..), manyToken)
import Data.Tree  
import Data.Tree.Pretty (drawVerticalTree)
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.12

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7
	| HappyAbsSyn8 t8
	| HappyAbsSyn9 t9
	| HappyAbsSyn10 t10

happyExpList :: Happy_Data_Array.Array Int Int
happyExpList = Happy_Data_Array.listArray (0,180) ([3072,3264,64,96,102,2,49152,1791,0,0,0,0,0,0,0,0,0,0,0,0,0,8,0,0,0,1536,512,0,48,16,32792,32793,0,0,0,0,65408,77,0,0,0,0,0,0,0,64,0,0,0,12291,4099,0,0,0,192,204,4,24582,8198,12288,13056,256,384,408,8,49164,16396,24576,26112,512,768,816,16,32792,32793,49152,52224,1024,1536,1632,32,48,51,32769,38913,2049,3072,3264,64,0,40952,0,49152,1791,0,65024,1,0,4080,0,32768,7,0,15360,0,0,480,0,0,15,0,6144,0,0,192,0,0,2,0,4096,0,0,65408,13,0,0,2,512,0,0,65280,155,0,0,0,0,0,6144,6528,128,0,0,0,32768,3583,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_toAST","S","Expr","Constant","FApp","Args","NEArgs","Unary","true","false","mod","lazy","int","bool","while","if","type","'`'","','","';'","TkId","TkNum","'^'","'*'","'+'","'-'","'<='","'>='","'<'","'>'","'='","'<>'","'!'","'||'","'&&'","':='","'('","')'","'{'","'}'","%eof"]
        bit_start = st * 43
        bit_end = (st + 1) * 43
        read_bit = readArrayBit happyExpList
        bits = map read_bit [bit_start..bit_end - 1]
        bits_indexed = zip bits [0..42]
        token_strs_expected = concatMap f bits_indexed
        f (False, _) = []
        f (True, nr) = [token_strs !! nr]

action_0 (11) = happyShift action_5
action_0 (12) = happyShift action_6
action_0 (23) = happyShift action_7
action_0 (24) = happyShift action_8
action_0 (27) = happyShift action_9
action_0 (28) = happyShift action_10
action_0 (39) = happyShift action_11
action_0 (4) = happyGoto action_12
action_0 (5) = happyGoto action_2
action_0 (6) = happyGoto action_3
action_0 (7) = happyGoto action_4
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (11) = happyShift action_5
action_1 (12) = happyShift action_6
action_1 (23) = happyShift action_7
action_1 (24) = happyShift action_8
action_1 (27) = happyShift action_9
action_1 (28) = happyShift action_10
action_1 (39) = happyShift action_11
action_1 (5) = happyGoto action_2
action_1 (6) = happyGoto action_3
action_1 (7) = happyGoto action_4
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (25) = happyShift action_21
action_2 (26) = happyShift action_22
action_2 (27) = happyShift action_23
action_2 (28) = happyShift action_24
action_2 (29) = happyShift action_25
action_2 (30) = happyShift action_26
action_2 (31) = happyShift action_27
action_2 (32) = happyShift action_28
action_2 (33) = happyShift action_29
action_2 (34) = happyShift action_30
action_2 (36) = happyShift action_31
action_2 (37) = happyShift action_32
action_2 _ = happyReduce_1

action_3 _ = happyReduce_20

action_4 _ = happyReduce_19

action_5 _ = happyReduce_21

action_6 _ = happyReduce_22

action_7 (39) = happyShift action_20
action_7 _ = happyReduce_17

action_8 _ = happyReduce_16

action_9 (23) = happyShift action_16
action_9 (24) = happyShift action_17
action_9 (39) = happyShift action_18
action_9 (7) = happyGoto action_14
action_9 (10) = happyGoto action_19
action_9 _ = happyFail (happyExpListPerState 9)

action_10 (23) = happyShift action_16
action_10 (24) = happyShift action_17
action_10 (39) = happyShift action_18
action_10 (7) = happyGoto action_14
action_10 (10) = happyGoto action_15
action_10 _ = happyFail (happyExpListPerState 10)

action_11 (11) = happyShift action_5
action_11 (12) = happyShift action_6
action_11 (23) = happyShift action_7
action_11 (24) = happyShift action_8
action_11 (27) = happyShift action_9
action_11 (28) = happyShift action_10
action_11 (39) = happyShift action_11
action_11 (5) = happyGoto action_13
action_11 (6) = happyGoto action_3
action_11 (7) = happyGoto action_4
action_11 _ = happyFail (happyExpListPerState 11)

action_12 (43) = happyAccept
action_12 _ = happyFail (happyExpListPerState 12)

action_13 (25) = happyShift action_21
action_13 (26) = happyShift action_22
action_13 (27) = happyShift action_23
action_13 (28) = happyShift action_24
action_13 (29) = happyShift action_25
action_13 (30) = happyShift action_26
action_13 (31) = happyShift action_27
action_13 (32) = happyShift action_28
action_13 (33) = happyShift action_29
action_13 (34) = happyShift action_30
action_13 (36) = happyShift action_31
action_13 (37) = happyShift action_32
action_13 (40) = happyShift action_49
action_13 _ = happyFail (happyExpListPerState 13)

action_14 _ = happyReduce_29

action_15 _ = happyReduce_2

action_16 (39) = happyShift action_20
action_16 _ = happyReduce_30

action_17 _ = happyReduce_28

action_18 (11) = happyShift action_5
action_18 (12) = happyShift action_6
action_18 (23) = happyShift action_7
action_18 (24) = happyShift action_8
action_18 (27) = happyShift action_9
action_18 (28) = happyShift action_10
action_18 (39) = happyShift action_11
action_18 (5) = happyGoto action_48
action_18 (6) = happyGoto action_3
action_18 (7) = happyGoto action_4
action_18 _ = happyFail (happyExpListPerState 18)

action_19 _ = happyReduce_3

action_20 (11) = happyShift action_5
action_20 (12) = happyShift action_6
action_20 (23) = happyShift action_7
action_20 (24) = happyShift action_8
action_20 (27) = happyShift action_9
action_20 (28) = happyShift action_10
action_20 (39) = happyShift action_11
action_20 (5) = happyGoto action_45
action_20 (6) = happyGoto action_3
action_20 (7) = happyGoto action_4
action_20 (8) = happyGoto action_46
action_20 (9) = happyGoto action_47
action_20 _ = happyReduce_24

action_21 (11) = happyShift action_5
action_21 (12) = happyShift action_6
action_21 (23) = happyShift action_7
action_21 (24) = happyShift action_8
action_21 (27) = happyShift action_9
action_21 (28) = happyShift action_10
action_21 (39) = happyShift action_11
action_21 (5) = happyGoto action_44
action_21 (6) = happyGoto action_3
action_21 (7) = happyGoto action_4
action_21 _ = happyFail (happyExpListPerState 21)

action_22 (11) = happyShift action_5
action_22 (12) = happyShift action_6
action_22 (23) = happyShift action_7
action_22 (24) = happyShift action_8
action_22 (27) = happyShift action_9
action_22 (28) = happyShift action_10
action_22 (39) = happyShift action_11
action_22 (5) = happyGoto action_43
action_22 (6) = happyGoto action_3
action_22 (7) = happyGoto action_4
action_22 _ = happyFail (happyExpListPerState 22)

action_23 (11) = happyShift action_5
action_23 (12) = happyShift action_6
action_23 (23) = happyShift action_7
action_23 (24) = happyShift action_8
action_23 (27) = happyShift action_9
action_23 (28) = happyShift action_10
action_23 (39) = happyShift action_11
action_23 (5) = happyGoto action_42
action_23 (6) = happyGoto action_3
action_23 (7) = happyGoto action_4
action_23 _ = happyFail (happyExpListPerState 23)

action_24 (11) = happyShift action_5
action_24 (12) = happyShift action_6
action_24 (23) = happyShift action_7
action_24 (24) = happyShift action_8
action_24 (27) = happyShift action_9
action_24 (28) = happyShift action_10
action_24 (39) = happyShift action_11
action_24 (5) = happyGoto action_41
action_24 (6) = happyGoto action_3
action_24 (7) = happyGoto action_4
action_24 _ = happyFail (happyExpListPerState 24)

action_25 (11) = happyShift action_5
action_25 (12) = happyShift action_6
action_25 (23) = happyShift action_7
action_25 (24) = happyShift action_8
action_25 (27) = happyShift action_9
action_25 (28) = happyShift action_10
action_25 (39) = happyShift action_11
action_25 (5) = happyGoto action_40
action_25 (6) = happyGoto action_3
action_25 (7) = happyGoto action_4
action_25 _ = happyFail (happyExpListPerState 25)

action_26 (11) = happyShift action_5
action_26 (12) = happyShift action_6
action_26 (23) = happyShift action_7
action_26 (24) = happyShift action_8
action_26 (27) = happyShift action_9
action_26 (28) = happyShift action_10
action_26 (39) = happyShift action_11
action_26 (5) = happyGoto action_39
action_26 (6) = happyGoto action_3
action_26 (7) = happyGoto action_4
action_26 _ = happyFail (happyExpListPerState 26)

action_27 (11) = happyShift action_5
action_27 (12) = happyShift action_6
action_27 (23) = happyShift action_7
action_27 (24) = happyShift action_8
action_27 (27) = happyShift action_9
action_27 (28) = happyShift action_10
action_27 (39) = happyShift action_11
action_27 (5) = happyGoto action_38
action_27 (6) = happyGoto action_3
action_27 (7) = happyGoto action_4
action_27 _ = happyFail (happyExpListPerState 27)

action_28 (11) = happyShift action_5
action_28 (12) = happyShift action_6
action_28 (23) = happyShift action_7
action_28 (24) = happyShift action_8
action_28 (27) = happyShift action_9
action_28 (28) = happyShift action_10
action_28 (39) = happyShift action_11
action_28 (5) = happyGoto action_37
action_28 (6) = happyGoto action_3
action_28 (7) = happyGoto action_4
action_28 _ = happyFail (happyExpListPerState 28)

action_29 (11) = happyShift action_5
action_29 (12) = happyShift action_6
action_29 (23) = happyShift action_7
action_29 (24) = happyShift action_8
action_29 (27) = happyShift action_9
action_29 (28) = happyShift action_10
action_29 (39) = happyShift action_11
action_29 (5) = happyGoto action_36
action_29 (6) = happyGoto action_3
action_29 (7) = happyGoto action_4
action_29 _ = happyFail (happyExpListPerState 29)

action_30 (11) = happyShift action_5
action_30 (12) = happyShift action_6
action_30 (23) = happyShift action_7
action_30 (24) = happyShift action_8
action_30 (27) = happyShift action_9
action_30 (28) = happyShift action_10
action_30 (39) = happyShift action_11
action_30 (5) = happyGoto action_35
action_30 (6) = happyGoto action_3
action_30 (7) = happyGoto action_4
action_30 _ = happyFail (happyExpListPerState 30)

action_31 (11) = happyShift action_5
action_31 (12) = happyShift action_6
action_31 (23) = happyShift action_7
action_31 (24) = happyShift action_8
action_31 (27) = happyShift action_9
action_31 (28) = happyShift action_10
action_31 (39) = happyShift action_11
action_31 (5) = happyGoto action_34
action_31 (6) = happyGoto action_3
action_31 (7) = happyGoto action_4
action_31 _ = happyFail (happyExpListPerState 31)

action_32 (11) = happyShift action_5
action_32 (12) = happyShift action_6
action_32 (23) = happyShift action_7
action_32 (24) = happyShift action_8
action_32 (27) = happyShift action_9
action_32 (28) = happyShift action_10
action_32 (39) = happyShift action_11
action_32 (5) = happyGoto action_33
action_32 (6) = happyGoto action_3
action_32 (7) = happyGoto action_4
action_32 _ = happyFail (happyExpListPerState 32)

action_33 (25) = happyShift action_21
action_33 (26) = happyShift action_22
action_33 (27) = happyShift action_23
action_33 (28) = happyShift action_24
action_33 (29) = happyShift action_25
action_33 (30) = happyShift action_26
action_33 (31) = happyShift action_27
action_33 (32) = happyShift action_28
action_33 (33) = happyShift action_29
action_33 (34) = happyShift action_30
action_33 (37) = happyShift action_32
action_33 _ = happyReduce_15

action_34 (25) = happyShift action_21
action_34 (26) = happyShift action_22
action_34 (27) = happyShift action_23
action_34 (28) = happyShift action_24
action_34 (29) = happyShift action_25
action_34 (30) = happyShift action_26
action_34 (31) = happyShift action_27
action_34 (32) = happyShift action_28
action_34 (33) = happyShift action_29
action_34 (34) = happyShift action_30
action_34 (36) = happyShift action_31
action_34 (37) = happyShift action_32
action_34 _ = happyReduce_14

action_35 (25) = happyShift action_21
action_35 (26) = happyShift action_22
action_35 (27) = happyShift action_23
action_35 (28) = happyShift action_24
action_35 (29) = happyShift action_25
action_35 (30) = happyShift action_26
action_35 (31) = happyShift action_27
action_35 (32) = happyShift action_28
action_35 (33) = happyFail []
action_35 (34) = happyFail []
action_35 _ = happyReduce_5

action_36 (25) = happyShift action_21
action_36 (26) = happyShift action_22
action_36 (27) = happyShift action_23
action_36 (28) = happyShift action_24
action_36 (29) = happyShift action_25
action_36 (30) = happyShift action_26
action_36 (31) = happyShift action_27
action_36 (32) = happyShift action_28
action_36 (33) = happyFail []
action_36 (34) = happyFail []
action_36 _ = happyReduce_4

action_37 (25) = happyShift action_21
action_37 (26) = happyShift action_22
action_37 (27) = happyShift action_23
action_37 (28) = happyShift action_24
action_37 (29) = happyFail []
action_37 (30) = happyFail []
action_37 (31) = happyFail []
action_37 (32) = happyFail []
action_37 _ = happyReduce_11

action_38 (25) = happyShift action_21
action_38 (26) = happyShift action_22
action_38 (27) = happyShift action_23
action_38 (28) = happyShift action_24
action_38 (29) = happyFail []
action_38 (30) = happyFail []
action_38 (31) = happyFail []
action_38 (32) = happyFail []
action_38 _ = happyReduce_10

action_39 (25) = happyShift action_21
action_39 (26) = happyShift action_22
action_39 (27) = happyShift action_23
action_39 (28) = happyShift action_24
action_39 (29) = happyFail []
action_39 (30) = happyFail []
action_39 (31) = happyFail []
action_39 (32) = happyFail []
action_39 _ = happyReduce_13

action_40 (25) = happyShift action_21
action_40 (26) = happyShift action_22
action_40 (27) = happyShift action_23
action_40 (28) = happyShift action_24
action_40 (29) = happyFail []
action_40 (30) = happyFail []
action_40 (31) = happyFail []
action_40 (32) = happyFail []
action_40 _ = happyReduce_12

action_41 (25) = happyShift action_21
action_41 (26) = happyShift action_22
action_41 _ = happyReduce_7

action_42 (25) = happyShift action_21
action_42 (26) = happyShift action_22
action_42 _ = happyReduce_6

action_43 (25) = happyShift action_21
action_43 _ = happyReduce_8

action_44 (25) = happyShift action_21
action_44 _ = happyReduce_9

action_45 (25) = happyShift action_21
action_45 (26) = happyShift action_22
action_45 (27) = happyShift action_23
action_45 (28) = happyShift action_24
action_45 (29) = happyShift action_25
action_45 (30) = happyShift action_26
action_45 (31) = happyShift action_27
action_45 (32) = happyShift action_28
action_45 (33) = happyShift action_29
action_45 (34) = happyShift action_30
action_45 (36) = happyShift action_31
action_45 (37) = happyShift action_32
action_45 _ = happyReduce_26

action_46 (40) = happyShift action_52
action_46 _ = happyFail (happyExpListPerState 46)

action_47 (21) = happyShift action_51
action_47 _ = happyReduce_25

action_48 (25) = happyShift action_21
action_48 (26) = happyShift action_22
action_48 (27) = happyShift action_23
action_48 (28) = happyShift action_24
action_48 (29) = happyShift action_25
action_48 (30) = happyShift action_26
action_48 (31) = happyShift action_27
action_48 (32) = happyShift action_28
action_48 (33) = happyShift action_29
action_48 (34) = happyShift action_30
action_48 (36) = happyShift action_31
action_48 (37) = happyShift action_32
action_48 (40) = happyShift action_50
action_48 _ = happyFail (happyExpListPerState 48)

action_49 _ = happyReduce_18

action_50 _ = happyReduce_31

action_51 (11) = happyShift action_5
action_51 (12) = happyShift action_6
action_51 (23) = happyShift action_7
action_51 (24) = happyShift action_8
action_51 (27) = happyShift action_9
action_51 (28) = happyShift action_10
action_51 (39) = happyShift action_11
action_51 (5) = happyGoto action_53
action_51 (6) = happyGoto action_3
action_51 (7) = happyGoto action_4
action_51 _ = happyFail (happyExpListPerState 51)

action_52 _ = happyReduce_23

action_53 (25) = happyShift action_21
action_53 (26) = happyShift action_22
action_53 (27) = happyShift action_23
action_53 (28) = happyShift action_24
action_53 (29) = happyShift action_25
action_53 (30) = happyShift action_26
action_53 (31) = happyShift action_27
action_53 (32) = happyShift action_28
action_53 (33) = happyShift action_29
action_53 (34) = happyShift action_30
action_53 (36) = happyShift action_31
action_53 (37) = happyShift action_32
action_53 _ = happyReduce_27

happyReduce_1 = happySpecReduce_1  4 happyReduction_1
happyReduction_1 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 (Expr   happy_var_1
	)
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_2  5 happyReduction_2
happyReduction_2 (HappyAbsSyn10  happy_var_2)
	_
	 =  HappyAbsSyn5
		 (Negate   happy_var_2
	)
happyReduction_2 _ _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_2  5 happyReduction_3
happyReduction_3 (HappyAbsSyn10  happy_var_2)
	_
	 =  HappyAbsSyn5
		 (Pos      happy_var_2
	)
happyReduction_3 _ _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_3  5 happyReduction_4
happyReduction_4 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (EQ'   happy_var_1 happy_var_3
	)
happyReduction_4 _ _ _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_3  5 happyReduction_5
happyReduction_5 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (NEQ'  happy_var_1 happy_var_3
	)
happyReduction_5 _ _ _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_3  5 happyReduction_6
happyReduction_6 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (Plus  happy_var_1 happy_var_3
	)
happyReduction_6 _ _ _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_3  5 happyReduction_7
happyReduction_7 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (Minus happy_var_1 happy_var_3
	)
happyReduction_7 _ _ _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_3  5 happyReduction_8
happyReduction_8 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (Times happy_var_1 happy_var_3
	)
happyReduction_8 _ _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_3  5 happyReduction_9
happyReduction_9 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (Pow   happy_var_1 happy_var_3
	)
happyReduction_9 _ _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_3  5 happyReduction_10
happyReduction_10 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (LT'   happy_var_1 happy_var_3
	)
happyReduction_10 _ _ _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_3  5 happyReduction_11
happyReduction_11 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (GT'   happy_var_1 happy_var_3
	)
happyReduction_11 _ _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_3  5 happyReduction_12
happyReduction_12 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (LE'   happy_var_1 happy_var_3
	)
happyReduction_12 _ _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_3  5 happyReduction_13
happyReduction_13 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (GE'   happy_var_1 happy_var_3
	)
happyReduction_13 _ _ _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_3  5 happyReduction_14
happyReduction_14 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (Or    happy_var_1 happy_var_3
	)
happyReduction_14 _ _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_3  5 happyReduction_15
happyReduction_15 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn5
		 (And   happy_var_1 happy_var_3
	)
happyReduction_15 _ _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_1  5 happyReduction_16
happyReduction_16 (HappyTerminal (TkNum happy_var_1))
	 =  HappyAbsSyn5
		 (Num   happy_var_1
	)
happyReduction_16 _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_1  5 happyReduction_17
happyReduction_17 (HappyTerminal (TkId  happy_var_1))
	 =  HappyAbsSyn5
		 (Var   happy_var_1
	)
happyReduction_17 _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_3  5 happyReduction_18
happyReduction_18 _
	(HappyAbsSyn5  happy_var_2)
	_
	 =  HappyAbsSyn5
		 (Brack happy_var_2
	)
happyReduction_18 _ _ _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_1  5 happyReduction_19
happyReduction_19 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_1
	)
happyReduction_19 _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_1  5 happyReduction_20
happyReduction_20 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_1
	)
happyReduction_20 _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_1  6 happyReduction_21
happyReduction_21 _
	 =  HappyAbsSyn6
		 (BOOL True
	)

happyReduce_22 = happySpecReduce_1  6 happyReduction_22
happyReduction_22 _
	 =  HappyAbsSyn6
		 (BOOL False
	)

happyReduce_23 = happyReduce 4 7 happyReduction_23
happyReduction_23 (_ `HappyStk`
	(HappyAbsSyn8  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TkId  happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (FApp  happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_24 = happySpecReduce_0  8 happyReduction_24
happyReduction_24  =  HappyAbsSyn8
		 ([]
	)

happyReduce_25 = happySpecReduce_1  8 happyReduction_25
happyReduction_25 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn8
		 (reverse happy_var_1
	)
happyReduction_25 _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_1  9 happyReduction_26
happyReduction_26 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_1 : []
	)
happyReduction_26 _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_3  9 happyReduction_27
happyReduction_27 (HappyAbsSyn5  happy_var_3)
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_3 : happy_var_1
	)
happyReduction_27 _ _ _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_1  10 happyReduction_28
happyReduction_28 (HappyTerminal (TkNum happy_var_1))
	 =  HappyAbsSyn10
		 (Num   happy_var_1
	)
happyReduction_28 _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_1  10 happyReduction_29
happyReduction_29 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn10
		 (happy_var_1
	)
happyReduction_29 _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_1  10 happyReduction_30
happyReduction_30 (HappyTerminal (TkId  happy_var_1))
	 =  HappyAbsSyn10
		 (Var   happy_var_1
	)
happyReduction_30 _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_3  10 happyReduction_31
happyReduction_31 _
	(HappyAbsSyn5  happy_var_2)
	_
	 =  HappyAbsSyn10
		 (Brack happy_var_2
	)
happyReduction_31 _ _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 43 43 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TkTrue -> cont 11;
	TkFalse -> cont 12;
	TkMod -> cont 13;
	TkLazy -> cont 14;
	TkInt -> cont 15;
	TkBool -> cont 16;
	TkWhile -> cont 17;
	TkIf -> cont 18;
	TkType -> cont 19;
	TkQuote -> cont 20;
	TkComma -> cont 21;
	TkSemicolon -> cont 22;
	TkId  happy_dollar_dollar -> cont 23;
	TkNum happy_dollar_dollar -> cont 24;
	TkPower -> cont 25;
	TkMult -> cont 26;
	TkPlus -> cont 27;
	TkMinus -> cont 28;
	TkLE -> cont 29;
	TkGE -> cont 30;
	TkLT -> cont 31;
	TkGT -> cont 32;
	TkEQ -> cont 33;
	TkNE -> cont 34;
	TkNot -> cont 35;
	TkOr -> cont 36;
	TkAnd -> cont 37;
	TkAssign -> cont 38;
	TkOpenPar -> cont 39;
	TkClosePar -> cont 40;
	TkOpenBrace -> cont 41;
	TkCloseBrace -> cont 42;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 43 tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

newtype HappyIdentity a = HappyIdentity a
happyIdentity = HappyIdentity
happyRunIdentity (HappyIdentity a) = a

instance Functor HappyIdentity where
    fmap f (HappyIdentity a) = HappyIdentity (f a)

instance Applicative HappyIdentity where
    pure  = HappyIdentity
    (<*>) = ap
instance Monad HappyIdentity where
    return = pure
    (HappyIdentity p) >>= q = q p

happyThen :: () => HappyIdentity a -> (a -> HappyIdentity b) -> HappyIdentity b
happyThen = (>>=)
happyReturn :: () => a -> HappyIdentity a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> HappyIdentity a
happyReturn1 = \a tks -> (return) a
happyError' :: () => ([(Token)], [String]) -> HappyIdentity a
happyError' = HappyIdentity . (\(tokens, _) -> parseError tokens)
toAST tks = happyRunIdentity happySomeParser where
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError :: [Token] -> a
parseError _ = error "Parse error"

data S 
    = Expr Expr
        deriving Show

data Expr
    = Brack  Expr
    | Negate Expr
    | Pos    Expr 
    | Plus   Expr Expr
    | Minus  Expr Expr
    | Mod    Expr Expr 
    | Times  Expr Expr 
    | Pow    Expr Expr
    | EQ'    Expr Expr 
    | NEQ'   Expr Expr
    | Or     Expr Expr
    | And    Expr Expr
    | LT'    Expr Expr
    | GT'    Expr Expr
    | LE'    Expr Expr
    | GE'    Expr Expr
    | Num    Int
    | Var    String
    | BOOL   Bool
    | FApp   String [Expr]
        deriving Show



sToTree :: S -> Tree String
sToTree (Expr e) = exprToTree e 

exprToTree :: Expr -> Tree String
exprToTree (Negate e)  = Node {rootLabel= "-Expr", subForest=[exprToTree e]}
exprToTree (Pos    e)  = Node {rootLabel= "+Expr", subForest=[exprToTree e]}
exprToTree (Plus a b)  = Node {rootLabel= "+", subForest=[exprToTree a, exprToTree b]}
exprToTree (Minus a b) = Node {rootLabel= "-", subForest=[exprToTree a, exprToTree b]}
exprToTree (Times t t')= Node {rootLabel= "*", subForest=[exprToTree t, exprToTree t' ]}
exprToTree (Mod t t')  = Node {rootLabel= "%", subForest=[exprToTree t, exprToTree t' ]}
exprToTree (Pow t t')  = Node {rootLabel= "^", subForest=[exprToTree t, exprToTree t' ]}
exprToTree (Num n)     = Node {rootLabel= show n, subForest=[]}
exprToTree (Var v)     = Node {rootLabel= show v, subForest=[]}
exprToTree (Brack e)   = Node {rootLabel= "(Expr)", subForest=[exprToTree e]}
exprToTree (BOOL p)    = Node {rootLabel=show p, subForest=[]}
exprToTree (EQ' p q)   = Node {rootLabel="=", subForest=[exprToTree p, exprToTree q]}
exprToTree (NEQ' p q)  = Node {rootLabel="<>", subForest=[exprToTree p, exprToTree q]}
exprToTree (LT' p q)   = Node {rootLabel="<", subForest=[exprToTree p, exprToTree q]}
exprToTree (GT' p q)   = Node {rootLabel=">", subForest=[exprToTree p, exprToTree q]}
exprToTree (LE' p q)   = Node {rootLabel="<=", subForest=[exprToTree p, exprToTree q]}
exprToTree (GE' p q)   = Node {rootLabel=">=", subForest=[exprToTree p, exprToTree q]}
exprToTree (Or p q)    = Node {rootLabel="||", subForest=[exprToTree p, exprToTree q]}
exprToTree (And p q)   = Node {rootLabel="&&", subForest=[exprToTree p, exprToTree q]}
exprToTree (FApp name args) = Node {rootLabel="f(..)", subForest= map exprToTree args }

toPrettyS :: S -> String
toPrettyS  = drawVerticalTree . sToTree 

prettyPrintS :: S -> IO ()
prettyPrintS = putStrLn . toPrettyS


parse :: String -> Either [(String,Int)] String
parse = fmap (toPrettyS . toAST)  . manyToken

parse' :: String -> IO ()
parse' s = do
    let ast = parse s
    case ast of
        Right res -> putStrLn res
        Left _    -> putStrLn "Error"
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $










































data Happy_IntList = HappyCons Int Happy_IntList








































infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is ERROR_TOK, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action









































indexShortOffAddr arr off = arr Happy_Data_Array.! off


{-# INLINE happyLt #-}
happyLt x y = (x < y)






readArrayBit arr bit =
    Bits.testBit (indexShortOffAddr arr (bit `div` 16)) (bit `mod` 16)






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
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             _ = nt :: Int
             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction









happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery (ERROR_TOK is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  ERROR_TOK tk old_st CONS(HAPPYSTATE(action),sts) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        DO_ACTION(action,ERROR_TOK,tk,sts,(saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ((HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.









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
