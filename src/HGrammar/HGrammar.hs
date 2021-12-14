{-# OPTIONS_GHC -w #-}
module HGrammar.HGrammar (toAST, sToTree, prettyPrintS, parse ) where

import Data.Char
import Lexer.Lexer (Token(..), manyToken, PrettyToken(..))
import Data.Tree  
import Data.Tree.Pretty (drawVerticalTree)
import AST.AST
import Data.List (intercalate)
import Prelude hiding (EQ,LT,GT)
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.20.0

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14
	= HappyTerminal (Token)
	| HappyErrorToken Prelude.Int
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

happyExpList :: Happy_Data_Array.Array Prelude.Int Prelude.Int
happyExpList = Happy_Data_Array.listArray (0,256) ([49152,52462,1088,24576,26224,544,0,64512,111,0,0,0,0,0,0,0,0,0,0,0,0,0,0,8,0,0,4,28768,8294,2,0,0,1,0,0,0,52748,17420,0,26374,8710,0,13187,4355,32768,39361,2177,0,0,0,0,256,0,0,0,0,0,128,0,0,0,0,0,65424,13,14336,0,0,0,0,0,0,0,0,0,0,3,14384,4147,1,39960,34841,0,0,0,0,26486,8710,0,13187,4355,32768,39361,2177,49152,52448,1088,24576,26224,544,12288,13112,272,6144,6556,136,3072,3278,68,1536,1639,34,33536,819,17,49536,33177,8,57536,16588,4,28768,8294,2,0,0,0,0,16384,0,52972,17420,0,32768,19967,0,16384,0,0,8192,0,0,4096,0,0,63552,223,12288,13112,272,6144,6556,136,0,65280,27,0,0,64,0,4,0,0,0,16,0,0,0,0,0,0,0,0,0,0,10238,0,0,7167,0,32768,127,0,49152,63,0,57344,1,0,61440,0,0,30720,0,0,15360,0,0,1536,0,0,768,0,0,128,0,0,64,0,0,0,0,0,0,8,0,57336,0,0,0,0,0,0,0,52748,17420,0,0,0,0,49152,1791,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_toAST","S","Action","Declaration","Type","Assignment","Expr","Constant","FApp","Args","NEArgs","Unary","true","false","mod","lazy","int","bool","while","if","type","'`'","','","';'","TkId","TkNum","'^'","'*'","'+'","'-'","'<='","'>='","'<'","'>'","'='","'<>'","'!'","'||'","'&&'","':='","'('","')'","'{'","'}'","%eof"]
        bit_start = st Prelude.* 47
        bit_end = (st Prelude.+ 1) Prelude.* 47
        read_bit = readArrayBit happyExpList
        bits = Prelude.map read_bit [bit_start..bit_end Prelude.- 1]
        bits_indexed = Prelude.zip bits [0..46]
        token_strs_expected = Prelude.concatMap f bits_indexed
        f (Prelude.False, _) = []
        f (Prelude.True, nr) = [token_strs Prelude.!! nr]

action_0 (15) = happyShift action_5
action_0 (16) = happyShift action_6
action_0 (18) = happyShift action_22
action_0 (19) = happyShift action_23
action_0 (20) = happyShift action_24
action_0 (22) = happyShift action_7
action_0 (23) = happyShift action_8
action_0 (24) = happyShift action_9
action_0 (27) = happyShift action_25
action_0 (28) = happyShift action_11
action_0 (31) = happyShift action_12
action_0 (32) = happyShift action_13
action_0 (39) = happyShift action_14
action_0 (43) = happyShift action_15
action_0 (4) = happyGoto action_16
action_0 (5) = happyGoto action_17
action_0 (6) = happyGoto action_18
action_0 (7) = happyGoto action_19
action_0 (8) = happyGoto action_20
action_0 (9) = happyGoto action_21
action_0 (10) = happyGoto action_3
action_0 (11) = happyGoto action_4
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (15) = happyShift action_5
action_1 (16) = happyShift action_6
action_1 (22) = happyShift action_7
action_1 (23) = happyShift action_8
action_1 (24) = happyShift action_9
action_1 (27) = happyShift action_10
action_1 (28) = happyShift action_11
action_1 (31) = happyShift action_12
action_1 (32) = happyShift action_13
action_1 (39) = happyShift action_14
action_1 (43) = happyShift action_15
action_1 (9) = happyGoto action_2
action_1 (10) = happyGoto action_3
action_1 (11) = happyGoto action_4
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (29) = happyShift action_30
action_2 (30) = happyShift action_31
action_2 (31) = happyShift action_32
action_2 (32) = happyShift action_33
action_2 (33) = happyShift action_34
action_2 (34) = happyShift action_35
action_2 (35) = happyShift action_36
action_2 (36) = happyShift action_37
action_2 (37) = happyShift action_38
action_2 (38) = happyShift action_39
action_2 (40) = happyShift action_40
action_2 (41) = happyShift action_41
action_2 _ = happyFail (happyExpListPerState 2)

action_3 _ = happyReduce_35

action_4 _ = happyReduce_34

action_5 _ = happyReduce_36

action_6 _ = happyReduce_37

action_7 (43) = happyShift action_51
action_7 _ = happyReduce_31

action_8 (43) = happyShift action_50
action_8 _ = happyReduce_30

action_9 (15) = happyShift action_5
action_9 (16) = happyShift action_6
action_9 (22) = happyShift action_7
action_9 (23) = happyShift action_8
action_9 (24) = happyShift action_9
action_9 (27) = happyShift action_10
action_9 (28) = happyShift action_11
action_9 (31) = happyShift action_12
action_9 (32) = happyShift action_13
action_9 (39) = happyShift action_14
action_9 (43) = happyShift action_15
action_9 (9) = happyGoto action_49
action_9 (10) = happyGoto action_3
action_9 (11) = happyGoto action_4
action_9 _ = happyFail (happyExpListPerState 9)

action_10 (43) = happyShift action_27
action_10 _ = happyReduce_29

action_11 _ = happyReduce_38

action_12 (15) = happyShift action_5
action_12 (16) = happyShift action_6
action_12 (22) = happyShift action_7
action_12 (23) = happyShift action_8
action_12 (24) = happyShift action_9
action_12 (27) = happyShift action_10
action_12 (28) = happyShift action_11
action_12 (31) = happyShift action_12
action_12 (32) = happyShift action_13
action_12 (39) = happyShift action_14
action_12 (43) = happyShift action_15
action_12 (9) = happyGoto action_48
action_12 (10) = happyGoto action_3
action_12 (11) = happyGoto action_4
action_12 _ = happyFail (happyExpListPerState 12)

action_13 (15) = happyShift action_5
action_13 (16) = happyShift action_6
action_13 (22) = happyShift action_7
action_13 (23) = happyShift action_8
action_13 (24) = happyShift action_9
action_13 (27) = happyShift action_10
action_13 (28) = happyShift action_11
action_13 (31) = happyShift action_12
action_13 (32) = happyShift action_13
action_13 (39) = happyShift action_14
action_13 (43) = happyShift action_15
action_13 (9) = happyGoto action_47
action_13 (10) = happyGoto action_3
action_13 (11) = happyGoto action_4
action_13 _ = happyFail (happyExpListPerState 13)

action_14 (15) = happyShift action_5
action_14 (16) = happyShift action_6
action_14 (22) = happyShift action_7
action_14 (23) = happyShift action_8
action_14 (24) = happyShift action_9
action_14 (27) = happyShift action_10
action_14 (28) = happyShift action_11
action_14 (31) = happyShift action_12
action_14 (32) = happyShift action_13
action_14 (39) = happyShift action_14
action_14 (43) = happyShift action_15
action_14 (9) = happyGoto action_46
action_14 (10) = happyGoto action_3
action_14 (11) = happyGoto action_4
action_14 _ = happyFail (happyExpListPerState 14)

action_15 (15) = happyShift action_5
action_15 (16) = happyShift action_6
action_15 (22) = happyShift action_7
action_15 (23) = happyShift action_8
action_15 (24) = happyShift action_9
action_15 (27) = happyShift action_10
action_15 (28) = happyShift action_11
action_15 (31) = happyShift action_12
action_15 (32) = happyShift action_13
action_15 (39) = happyShift action_14
action_15 (43) = happyShift action_15
action_15 (9) = happyGoto action_45
action_15 (10) = happyGoto action_3
action_15 (11) = happyGoto action_4
action_15 _ = happyFail (happyExpListPerState 15)

action_16 (47) = happyAccept
action_16 _ = happyFail (happyExpListPerState 16)

action_17 (26) = happyShift action_44
action_17 _ = happyReduce_2

action_18 _ = happyReduce_7

action_19 (27) = happyShift action_43
action_19 (8) = happyGoto action_42
action_19 _ = happyFail (happyExpListPerState 19)

action_20 _ = happyReduce_8

action_21 (26) = happyShift action_29
action_21 (29) = happyShift action_30
action_21 (30) = happyShift action_31
action_21 (31) = happyShift action_32
action_21 (32) = happyShift action_33
action_21 (33) = happyShift action_34
action_21 (34) = happyShift action_35
action_21 (35) = happyShift action_36
action_21 (36) = happyShift action_37
action_21 (37) = happyShift action_38
action_21 (38) = happyShift action_39
action_21 (40) = happyShift action_40
action_21 (41) = happyShift action_41
action_21 _ = happyReduce_1

action_22 (18) = happyShift action_22
action_22 (19) = happyShift action_23
action_22 (20) = happyShift action_24
action_22 (7) = happyGoto action_28
action_22 _ = happyFail (happyExpListPerState 22)

action_23 _ = happyReduce_10

action_24 _ = happyReduce_11

action_25 (42) = happyShift action_26
action_25 (43) = happyShift action_27
action_25 _ = happyReduce_29

action_26 (15) = happyShift action_5
action_26 (16) = happyShift action_6
action_26 (22) = happyShift action_7
action_26 (23) = happyShift action_8
action_26 (24) = happyShift action_9
action_26 (27) = happyShift action_10
action_26 (28) = happyShift action_11
action_26 (31) = happyShift action_12
action_26 (32) = happyShift action_13
action_26 (39) = happyShift action_14
action_26 (43) = happyShift action_15
action_26 (9) = happyGoto action_73
action_26 (10) = happyGoto action_3
action_26 (11) = happyGoto action_4
action_26 _ = happyFail (happyExpListPerState 26)

action_27 (15) = happyShift action_5
action_27 (16) = happyShift action_6
action_27 (22) = happyShift action_7
action_27 (23) = happyShift action_8
action_27 (24) = happyShift action_9
action_27 (27) = happyShift action_10
action_27 (28) = happyShift action_11
action_27 (31) = happyShift action_12
action_27 (32) = happyShift action_13
action_27 (39) = happyShift action_14
action_27 (43) = happyShift action_15
action_27 (9) = happyGoto action_52
action_27 (10) = happyGoto action_3
action_27 (11) = happyGoto action_4
action_27 (12) = happyGoto action_72
action_27 (13) = happyGoto action_54
action_27 _ = happyReduce_42

action_28 _ = happyReduce_12

action_29 (15) = happyShift action_5
action_29 (16) = happyShift action_6
action_29 (18) = happyShift action_22
action_29 (19) = happyShift action_23
action_29 (20) = happyShift action_24
action_29 (22) = happyShift action_7
action_29 (23) = happyShift action_8
action_29 (24) = happyShift action_9
action_29 (27) = happyShift action_25
action_29 (28) = happyShift action_11
action_29 (31) = happyShift action_12
action_29 (32) = happyShift action_13
action_29 (39) = happyShift action_14
action_29 (43) = happyShift action_15
action_29 (4) = happyGoto action_71
action_29 (5) = happyGoto action_17
action_29 (6) = happyGoto action_18
action_29 (7) = happyGoto action_19
action_29 (8) = happyGoto action_20
action_29 (9) = happyGoto action_21
action_29 (10) = happyGoto action_3
action_29 (11) = happyGoto action_4
action_29 _ = happyReduce_6

action_30 (15) = happyShift action_5
action_30 (16) = happyShift action_6
action_30 (22) = happyShift action_7
action_30 (23) = happyShift action_8
action_30 (24) = happyShift action_9
action_30 (27) = happyShift action_10
action_30 (28) = happyShift action_11
action_30 (31) = happyShift action_12
action_30 (32) = happyShift action_13
action_30 (39) = happyShift action_14
action_30 (43) = happyShift action_15
action_30 (9) = happyGoto action_70
action_30 (10) = happyGoto action_3
action_30 (11) = happyGoto action_4
action_30 _ = happyFail (happyExpListPerState 30)

action_31 (15) = happyShift action_5
action_31 (16) = happyShift action_6
action_31 (22) = happyShift action_7
action_31 (23) = happyShift action_8
action_31 (24) = happyShift action_9
action_31 (27) = happyShift action_10
action_31 (28) = happyShift action_11
action_31 (31) = happyShift action_12
action_31 (32) = happyShift action_13
action_31 (39) = happyShift action_14
action_31 (43) = happyShift action_15
action_31 (9) = happyGoto action_69
action_31 (10) = happyGoto action_3
action_31 (11) = happyGoto action_4
action_31 _ = happyFail (happyExpListPerState 31)

action_32 (15) = happyShift action_5
action_32 (16) = happyShift action_6
action_32 (22) = happyShift action_7
action_32 (23) = happyShift action_8
action_32 (24) = happyShift action_9
action_32 (27) = happyShift action_10
action_32 (28) = happyShift action_11
action_32 (31) = happyShift action_12
action_32 (32) = happyShift action_13
action_32 (39) = happyShift action_14
action_32 (43) = happyShift action_15
action_32 (9) = happyGoto action_68
action_32 (10) = happyGoto action_3
action_32 (11) = happyGoto action_4
action_32 _ = happyFail (happyExpListPerState 32)

action_33 (15) = happyShift action_5
action_33 (16) = happyShift action_6
action_33 (22) = happyShift action_7
action_33 (23) = happyShift action_8
action_33 (24) = happyShift action_9
action_33 (27) = happyShift action_10
action_33 (28) = happyShift action_11
action_33 (31) = happyShift action_12
action_33 (32) = happyShift action_13
action_33 (39) = happyShift action_14
action_33 (43) = happyShift action_15
action_33 (9) = happyGoto action_67
action_33 (10) = happyGoto action_3
action_33 (11) = happyGoto action_4
action_33 _ = happyFail (happyExpListPerState 33)

action_34 (15) = happyShift action_5
action_34 (16) = happyShift action_6
action_34 (22) = happyShift action_7
action_34 (23) = happyShift action_8
action_34 (24) = happyShift action_9
action_34 (27) = happyShift action_10
action_34 (28) = happyShift action_11
action_34 (31) = happyShift action_12
action_34 (32) = happyShift action_13
action_34 (39) = happyShift action_14
action_34 (43) = happyShift action_15
action_34 (9) = happyGoto action_66
action_34 (10) = happyGoto action_3
action_34 (11) = happyGoto action_4
action_34 _ = happyFail (happyExpListPerState 34)

action_35 (15) = happyShift action_5
action_35 (16) = happyShift action_6
action_35 (22) = happyShift action_7
action_35 (23) = happyShift action_8
action_35 (24) = happyShift action_9
action_35 (27) = happyShift action_10
action_35 (28) = happyShift action_11
action_35 (31) = happyShift action_12
action_35 (32) = happyShift action_13
action_35 (39) = happyShift action_14
action_35 (43) = happyShift action_15
action_35 (9) = happyGoto action_65
action_35 (10) = happyGoto action_3
action_35 (11) = happyGoto action_4
action_35 _ = happyFail (happyExpListPerState 35)

action_36 (15) = happyShift action_5
action_36 (16) = happyShift action_6
action_36 (22) = happyShift action_7
action_36 (23) = happyShift action_8
action_36 (24) = happyShift action_9
action_36 (27) = happyShift action_10
action_36 (28) = happyShift action_11
action_36 (31) = happyShift action_12
action_36 (32) = happyShift action_13
action_36 (39) = happyShift action_14
action_36 (43) = happyShift action_15
action_36 (9) = happyGoto action_64
action_36 (10) = happyGoto action_3
action_36 (11) = happyGoto action_4
action_36 _ = happyFail (happyExpListPerState 36)

action_37 (15) = happyShift action_5
action_37 (16) = happyShift action_6
action_37 (22) = happyShift action_7
action_37 (23) = happyShift action_8
action_37 (24) = happyShift action_9
action_37 (27) = happyShift action_10
action_37 (28) = happyShift action_11
action_37 (31) = happyShift action_12
action_37 (32) = happyShift action_13
action_37 (39) = happyShift action_14
action_37 (43) = happyShift action_15
action_37 (9) = happyGoto action_63
action_37 (10) = happyGoto action_3
action_37 (11) = happyGoto action_4
action_37 _ = happyFail (happyExpListPerState 37)

action_38 (15) = happyShift action_5
action_38 (16) = happyShift action_6
action_38 (22) = happyShift action_7
action_38 (23) = happyShift action_8
action_38 (24) = happyShift action_9
action_38 (27) = happyShift action_10
action_38 (28) = happyShift action_11
action_38 (31) = happyShift action_12
action_38 (32) = happyShift action_13
action_38 (39) = happyShift action_14
action_38 (43) = happyShift action_15
action_38 (9) = happyGoto action_62
action_38 (10) = happyGoto action_3
action_38 (11) = happyGoto action_4
action_38 _ = happyFail (happyExpListPerState 38)

action_39 (15) = happyShift action_5
action_39 (16) = happyShift action_6
action_39 (22) = happyShift action_7
action_39 (23) = happyShift action_8
action_39 (24) = happyShift action_9
action_39 (27) = happyShift action_10
action_39 (28) = happyShift action_11
action_39 (31) = happyShift action_12
action_39 (32) = happyShift action_13
action_39 (39) = happyShift action_14
action_39 (43) = happyShift action_15
action_39 (9) = happyGoto action_61
action_39 (10) = happyGoto action_3
action_39 (11) = happyGoto action_4
action_39 _ = happyFail (happyExpListPerState 39)

action_40 (15) = happyShift action_5
action_40 (16) = happyShift action_6
action_40 (22) = happyShift action_7
action_40 (23) = happyShift action_8
action_40 (24) = happyShift action_9
action_40 (27) = happyShift action_10
action_40 (28) = happyShift action_11
action_40 (31) = happyShift action_12
action_40 (32) = happyShift action_13
action_40 (39) = happyShift action_14
action_40 (43) = happyShift action_15
action_40 (9) = happyGoto action_60
action_40 (10) = happyGoto action_3
action_40 (11) = happyGoto action_4
action_40 _ = happyFail (happyExpListPerState 40)

action_41 (15) = happyShift action_5
action_41 (16) = happyShift action_6
action_41 (22) = happyShift action_7
action_41 (23) = happyShift action_8
action_41 (24) = happyShift action_9
action_41 (27) = happyShift action_10
action_41 (28) = happyShift action_11
action_41 (31) = happyShift action_12
action_41 (32) = happyShift action_13
action_41 (39) = happyShift action_14
action_41 (43) = happyShift action_15
action_41 (9) = happyGoto action_59
action_41 (10) = happyGoto action_3
action_41 (11) = happyGoto action_4
action_41 _ = happyFail (happyExpListPerState 41)

action_42 _ = happyReduce_9

action_43 (42) = happyShift action_26
action_43 _ = happyFail (happyExpListPerState 43)

action_44 (15) = happyShift action_5
action_44 (16) = happyShift action_6
action_44 (18) = happyShift action_22
action_44 (19) = happyShift action_23
action_44 (20) = happyShift action_24
action_44 (22) = happyShift action_7
action_44 (23) = happyShift action_8
action_44 (24) = happyShift action_9
action_44 (27) = happyShift action_25
action_44 (28) = happyShift action_11
action_44 (31) = happyShift action_12
action_44 (32) = happyShift action_13
action_44 (39) = happyShift action_14
action_44 (43) = happyShift action_15
action_44 (4) = happyGoto action_58
action_44 (5) = happyGoto action_17
action_44 (6) = happyGoto action_18
action_44 (7) = happyGoto action_19
action_44 (8) = happyGoto action_20
action_44 (9) = happyGoto action_21
action_44 (10) = happyGoto action_3
action_44 (11) = happyGoto action_4
action_44 _ = happyReduce_5

action_45 (29) = happyShift action_30
action_45 (30) = happyShift action_31
action_45 (31) = happyShift action_32
action_45 (32) = happyShift action_33
action_45 (33) = happyShift action_34
action_45 (34) = happyShift action_35
action_45 (35) = happyShift action_36
action_45 (36) = happyShift action_37
action_45 (37) = happyShift action_38
action_45 (38) = happyShift action_39
action_45 (40) = happyShift action_40
action_45 (41) = happyShift action_41
action_45 (44) = happyShift action_57
action_45 _ = happyFail (happyExpListPerState 45)

action_46 (29) = happyShift action_30
action_46 _ = happyReduce_16

action_47 (29) = happyShift action_30
action_47 _ = happyReduce_14

action_48 (29) = happyShift action_30
action_48 _ = happyReduce_15

action_49 (24) = happyShift action_56
action_49 (29) = happyShift action_30
action_49 (30) = happyShift action_31
action_49 (31) = happyShift action_32
action_49 (32) = happyShift action_33
action_49 (33) = happyShift action_34
action_49 (34) = happyShift action_35
action_49 (35) = happyShift action_36
action_49 (36) = happyShift action_37
action_49 (37) = happyShift action_38
action_49 (38) = happyShift action_39
action_49 (40) = happyShift action_40
action_49 (41) = happyShift action_41
action_49 _ = happyFail (happyExpListPerState 49)

action_50 (15) = happyShift action_5
action_50 (16) = happyShift action_6
action_50 (22) = happyShift action_7
action_50 (23) = happyShift action_8
action_50 (24) = happyShift action_9
action_50 (27) = happyShift action_10
action_50 (28) = happyShift action_11
action_50 (31) = happyShift action_12
action_50 (32) = happyShift action_13
action_50 (39) = happyShift action_14
action_50 (43) = happyShift action_15
action_50 (9) = happyGoto action_52
action_50 (10) = happyGoto action_3
action_50 (11) = happyGoto action_4
action_50 (12) = happyGoto action_55
action_50 (13) = happyGoto action_54
action_50 _ = happyReduce_42

action_51 (15) = happyShift action_5
action_51 (16) = happyShift action_6
action_51 (22) = happyShift action_7
action_51 (23) = happyShift action_8
action_51 (24) = happyShift action_9
action_51 (27) = happyShift action_10
action_51 (28) = happyShift action_11
action_51 (31) = happyShift action_12
action_51 (32) = happyShift action_13
action_51 (39) = happyShift action_14
action_51 (43) = happyShift action_15
action_51 (9) = happyGoto action_52
action_51 (10) = happyGoto action_3
action_51 (11) = happyGoto action_4
action_51 (12) = happyGoto action_53
action_51 (13) = happyGoto action_54
action_51 _ = happyReduce_42

action_52 (29) = happyShift action_30
action_52 (30) = happyShift action_31
action_52 (31) = happyShift action_32
action_52 (32) = happyShift action_33
action_52 (33) = happyShift action_34
action_52 (34) = happyShift action_35
action_52 (35) = happyShift action_36
action_52 (36) = happyShift action_37
action_52 (37) = happyShift action_38
action_52 (38) = happyShift action_39
action_52 (40) = happyShift action_40
action_52 (41) = happyShift action_41
action_52 _ = happyReduce_44

action_53 (44) = happyShift action_77
action_53 _ = happyFail (happyExpListPerState 53)

action_54 (25) = happyShift action_76
action_54 _ = happyReduce_43

action_55 (44) = happyShift action_75
action_55 _ = happyFail (happyExpListPerState 55)

action_56 _ = happyReduce_33

action_57 _ = happyReduce_32

action_58 _ = happyReduce_4

action_59 (29) = happyShift action_30
action_59 (30) = happyShift action_31
action_59 (31) = happyShift action_32
action_59 (32) = happyShift action_33
action_59 (33) = happyShift action_34
action_59 (34) = happyShift action_35
action_59 (35) = happyShift action_36
action_59 (36) = happyShift action_37
action_59 (37) = happyShift action_38
action_59 (38) = happyShift action_39
action_59 (41) = happyShift action_41
action_59 _ = happyReduce_28

action_60 (29) = happyShift action_30
action_60 (30) = happyShift action_31
action_60 (31) = happyShift action_32
action_60 (32) = happyShift action_33
action_60 (33) = happyShift action_34
action_60 (34) = happyShift action_35
action_60 (35) = happyShift action_36
action_60 (36) = happyShift action_37
action_60 (37) = happyShift action_38
action_60 (38) = happyShift action_39
action_60 (40) = happyShift action_40
action_60 (41) = happyShift action_41
action_60 _ = happyReduce_27

action_61 (29) = happyShift action_30
action_61 (30) = happyShift action_31
action_61 (31) = happyShift action_32
action_61 (32) = happyShift action_33
action_61 (33) = happyShift action_34
action_61 (34) = happyShift action_35
action_61 (35) = happyShift action_36
action_61 (36) = happyShift action_37
action_61 (37) = happyFail []
action_61 (38) = happyFail []
action_61 _ = happyReduce_18

action_62 (29) = happyShift action_30
action_62 (30) = happyShift action_31
action_62 (31) = happyShift action_32
action_62 (32) = happyShift action_33
action_62 (33) = happyShift action_34
action_62 (34) = happyShift action_35
action_62 (35) = happyShift action_36
action_62 (36) = happyShift action_37
action_62 (37) = happyFail []
action_62 (38) = happyFail []
action_62 _ = happyReduce_17

action_63 (29) = happyShift action_30
action_63 (30) = happyShift action_31
action_63 (31) = happyShift action_32
action_63 (32) = happyShift action_33
action_63 (33) = happyFail []
action_63 (34) = happyFail []
action_63 (35) = happyFail []
action_63 (36) = happyFail []
action_63 _ = happyReduce_24

action_64 (29) = happyShift action_30
action_64 (30) = happyShift action_31
action_64 (31) = happyShift action_32
action_64 (32) = happyShift action_33
action_64 (33) = happyFail []
action_64 (34) = happyFail []
action_64 (35) = happyFail []
action_64 (36) = happyFail []
action_64 _ = happyReduce_23

action_65 (29) = happyShift action_30
action_65 (30) = happyShift action_31
action_65 (31) = happyShift action_32
action_65 (32) = happyShift action_33
action_65 (33) = happyFail []
action_65 (34) = happyFail []
action_65 (35) = happyFail []
action_65 (36) = happyFail []
action_65 _ = happyReduce_26

action_66 (29) = happyShift action_30
action_66 (30) = happyShift action_31
action_66 (31) = happyShift action_32
action_66 (32) = happyShift action_33
action_66 (33) = happyFail []
action_66 (34) = happyFail []
action_66 (35) = happyFail []
action_66 (36) = happyFail []
action_66 _ = happyReduce_25

action_67 (29) = happyShift action_30
action_67 (30) = happyShift action_31
action_67 _ = happyReduce_20

action_68 (29) = happyShift action_30
action_68 (30) = happyShift action_31
action_68 _ = happyReduce_19

action_69 (29) = happyShift action_30
action_69 _ = happyReduce_21

action_70 (29) = happyShift action_30
action_70 _ = happyReduce_22

action_71 _ = happyReduce_3

action_72 (44) = happyShift action_74
action_72 _ = happyFail (happyExpListPerState 72)

action_73 (29) = happyShift action_30
action_73 (30) = happyShift action_31
action_73 (31) = happyShift action_32
action_73 (32) = happyShift action_33
action_73 (33) = happyShift action_34
action_73 (34) = happyShift action_35
action_73 (35) = happyShift action_36
action_73 (36) = happyShift action_37
action_73 (37) = happyShift action_38
action_73 (38) = happyShift action_39
action_73 (40) = happyShift action_40
action_73 (41) = happyShift action_41
action_73 _ = happyReduce_13

action_74 _ = happyReduce_39

action_75 _ = happyReduce_40

action_76 (15) = happyShift action_5
action_76 (16) = happyShift action_6
action_76 (22) = happyShift action_7
action_76 (23) = happyShift action_8
action_76 (24) = happyShift action_9
action_76 (27) = happyShift action_10
action_76 (28) = happyShift action_11
action_76 (31) = happyShift action_12
action_76 (32) = happyShift action_13
action_76 (39) = happyShift action_14
action_76 (43) = happyShift action_15
action_76 (9) = happyGoto action_78
action_76 (10) = happyGoto action_3
action_76 (11) = happyGoto action_4
action_76 _ = happyReduce_46

action_77 _ = happyReduce_41

action_78 (29) = happyShift action_30
action_78 (30) = happyShift action_31
action_78 (31) = happyShift action_32
action_78 (32) = happyShift action_33
action_78 (33) = happyShift action_34
action_78 (34) = happyShift action_35
action_78 (35) = happyShift action_36
action_78 (36) = happyShift action_37
action_78 (37) = happyShift action_38
action_78 (38) = happyShift action_39
action_78 (40) = happyShift action_40
action_78 (41) = happyShift action_41
action_78 _ = happyReduce_45

happyReduce_1 = happySpecReduce_1  4 happyReduction_1
happyReduction_1 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn4
		 (E   happy_var_1
	)
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_1  4 happyReduction_2
happyReduction_2 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 (A   happy_var_1
	)
happyReduction_2 _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_3  4 happyReduction_3
happyReduction_3 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn4
		 (E (SeqE happy_var_1 happy_var_3)
	)
happyReduction_3 _ _ _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_3  4 happyReduction_4
happyReduction_4 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 (A (SeqA happy_var_1 happy_var_3)
	)
happyReduction_4 _ _ _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_2  4 happyReduction_5
happyReduction_5 _
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 (A happy_var_1
	)
happyReduction_5 _ _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_2  4 happyReduction_6
happyReduction_6 _
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn4
		 (E happy_var_1
	)
happyReduction_6 _ _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_1  5 happyReduction_7
happyReduction_7 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_1
	)
happyReduction_7 _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_1  5 happyReduction_8
happyReduction_8 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_1
	)
happyReduction_8 _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_2  6 happyReduction_9
happyReduction_9 (HappyAbsSyn8  happy_var_2)
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn6
		 ((\(Assignment s e) -> Declaration happy_var_1 s e )  happy_var_2
	)
happyReduction_9 _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_1  7 happyReduction_10
happyReduction_10 _
	 =  HappyAbsSyn7
		 (LInt
	)

happyReduce_11 = happySpecReduce_1  7 happyReduction_11
happyReduction_11 _
	 =  HappyAbsSyn7
		 (LBool
	)

happyReduce_12 = happySpecReduce_2  7 happyReduction_12
happyReduction_12 (HappyAbsSyn7  happy_var_2)
	_
	 =  HappyAbsSyn7
		 (LLazy happy_var_2
	)
happyReduction_12 _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_3  8 happyReduction_13
happyReduction_13 (HappyAbsSyn9  happy_var_3)
	_
	(HappyTerminal (TkId  happy_var_1))
	 =  HappyAbsSyn8
		 (Assignment happy_var_1 happy_var_3
	)
happyReduction_13 _ _ _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_2  9 happyReduction_14
happyReduction_14 (HappyAbsSyn9  happy_var_2)
	_
	 =  HappyAbsSyn9
		 (Negate   happy_var_2
	)
happyReduction_14 _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_2  9 happyReduction_15
happyReduction_15 (HappyAbsSyn9  happy_var_2)
	_
	 =  HappyAbsSyn9
		 (Pos      happy_var_2
	)
happyReduction_15 _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_2  9 happyReduction_16
happyReduction_16 (HappyAbsSyn9  happy_var_2)
	_
	 =  HappyAbsSyn9
		 (Not      happy_var_2
	)
happyReduction_16 _ _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_3  9 happyReduction_17
happyReduction_17 (HappyAbsSyn9  happy_var_3)
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (EQ    happy_var_1 happy_var_3
	)
happyReduction_17 _ _ _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_3  9 happyReduction_18
happyReduction_18 (HappyAbsSyn9  happy_var_3)
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (NEQ   happy_var_1 happy_var_3
	)
happyReduction_18 _ _ _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_3  9 happyReduction_19
happyReduction_19 (HappyAbsSyn9  happy_var_3)
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (Plus  happy_var_1 happy_var_3
	)
happyReduction_19 _ _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_3  9 happyReduction_20
happyReduction_20 (HappyAbsSyn9  happy_var_3)
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (Minus happy_var_1 happy_var_3
	)
happyReduction_20 _ _ _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_3  9 happyReduction_21
happyReduction_21 (HappyAbsSyn9  happy_var_3)
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (Times happy_var_1 happy_var_3
	)
happyReduction_21 _ _ _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_3  9 happyReduction_22
happyReduction_22 (HappyAbsSyn9  happy_var_3)
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (Pow   happy_var_1 happy_var_3
	)
happyReduction_22 _ _ _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_3  9 happyReduction_23
happyReduction_23 (HappyAbsSyn9  happy_var_3)
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (LT    happy_var_1 happy_var_3
	)
happyReduction_23 _ _ _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_3  9 happyReduction_24
happyReduction_24 (HappyAbsSyn9  happy_var_3)
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (GT    happy_var_1 happy_var_3
	)
happyReduction_24 _ _ _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_3  9 happyReduction_25
happyReduction_25 (HappyAbsSyn9  happy_var_3)
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (LE    happy_var_1 happy_var_3
	)
happyReduction_25 _ _ _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_3  9 happyReduction_26
happyReduction_26 (HappyAbsSyn9  happy_var_3)
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (GE    happy_var_1 happy_var_3
	)
happyReduction_26 _ _ _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_3  9 happyReduction_27
happyReduction_27 (HappyAbsSyn9  happy_var_3)
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (Or    happy_var_1 happy_var_3
	)
happyReduction_27 _ _ _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_3  9 happyReduction_28
happyReduction_28 (HappyAbsSyn9  happy_var_3)
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (And   happy_var_1 happy_var_3
	)
happyReduction_28 _ _ _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_1  9 happyReduction_29
happyReduction_29 (HappyTerminal (TkId  happy_var_1))
	 =  HappyAbsSyn9
		 (Var   happy_var_1
	)
happyReduction_29 _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_1  9 happyReduction_30
happyReduction_30 _
	 =  HappyAbsSyn9
		 (Var "type"
	)

happyReduce_31 = happySpecReduce_1  9 happyReduction_31
happyReduction_31 _
	 =  HappyAbsSyn9
		 (Var "if"
	)

happyReduce_32 = happySpecReduce_3  9 happyReduction_32
happyReduction_32 _
	(HappyAbsSyn9  happy_var_2)
	_
	 =  HappyAbsSyn9
		 (happy_var_2
	)
happyReduction_32 _ _ _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_3  9 happyReduction_33
happyReduction_33 _
	(HappyAbsSyn9  happy_var_2)
	_
	 =  HappyAbsSyn9
		 (Lazy  happy_var_2
	)
happyReduction_33 _ _ _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_1  9 happyReduction_34
happyReduction_34 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_1
	)
happyReduction_34 _  = notHappyAtAll 

happyReduce_35 = happySpecReduce_1  9 happyReduction_35
happyReduction_35 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_1
	)
happyReduction_35 _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_1  10 happyReduction_36
happyReduction_36 _
	 =  HappyAbsSyn10
		 (toBoolC True
	)

happyReduce_37 = happySpecReduce_1  10 happyReduction_37
happyReduction_37 _
	 =  HappyAbsSyn10
		 (toBoolC False
	)

happyReduce_38 = happySpecReduce_1  10 happyReduction_38
happyReduction_38 (HappyTerminal (TkNum happy_var_1))
	 =  HappyAbsSyn10
		 (toNumC  happy_var_1
	)
happyReduction_38 _  = notHappyAtAll 

happyReduce_39 = happyReduce 4 11 happyReduction_39
happyReduction_39 (_ `HappyStk`
	(HappyAbsSyn12  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TkId  happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn11
		 (FApp  happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_40 = happyReduce 4 11 happyReduction_40
happyReduction_40 (_ `HappyStk`
	(HappyAbsSyn12  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn11
		 (FApp  "type" happy_var_3
	) `HappyStk` happyRest

happyReduce_41 = happyReduce 4 11 happyReduction_41
happyReduction_41 (_ `HappyStk`
	(HappyAbsSyn12  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn11
		 (FApp  "if" happy_var_3
	) `HappyStk` happyRest

happyReduce_42 = happySpecReduce_0  12 happyReduction_42
happyReduction_42  =  HappyAbsSyn12
		 ([]
	)

happyReduce_43 = happySpecReduce_1  12 happyReduction_43
happyReduction_43 (HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn12
		 (reverse happy_var_1
	)
happyReduction_43 _  = notHappyAtAll 

happyReduce_44 = happySpecReduce_1  13 happyReduction_44
happyReduction_44 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn13
		 (happy_var_1 : []
	)
happyReduction_44 _  = notHappyAtAll 

happyReduce_45 = happySpecReduce_3  13 happyReduction_45
happyReduction_45 (HappyAbsSyn9  happy_var_3)
	_
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn13
		 (happy_var_3 : happy_var_1
	)
happyReduction_45 _ _ _  = notHappyAtAll 

happyReduce_46 = happyMonadReduce 2 13 happyReduction_46
happyReduction_46 (_ `HappyStk`
	_ `HappyStk`
	happyRest) tk
	 = happyThen ((( Left ("Parse error on argument list. ", 0)))
	) (\r -> happyReturn (HappyAbsSyn13 r))

happyReduce_47 = happySpecReduce_1  14 happyReduction_47
happyReduction_47 (HappyTerminal (TkNum happy_var_1))
	 =  HappyAbsSyn14
		 (toNumC  happy_var_1
	)
happyReduction_47 _  = notHappyAtAll 

happyReduce_48 = happySpecReduce_1  14 happyReduction_48
happyReduction_48 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn14
		 (happy_var_1
	)
happyReduction_48 _  = notHappyAtAll 

happyReduce_49 = happySpecReduce_1  14 happyReduction_49
happyReduction_49 (HappyTerminal (TkId  happy_var_1))
	 =  HappyAbsSyn14
		 (Var     happy_var_1
	)
happyReduction_49 _  = notHappyAtAll 

happyReduce_50 = happySpecReduce_3  14 happyReduction_50
happyReduction_50 _
	(HappyAbsSyn9  happy_var_2)
	_
	 =  HappyAbsSyn14
		 (happy_var_2
	)
happyReduction_50 _ _ _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 47 47 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TkTrue -> cont 15;
	TkFalse -> cont 16;
	TkMod -> cont 17;
	TkLazy -> cont 18;
	TkInt -> cont 19;
	TkBool -> cont 20;
	TkWhile -> cont 21;
	TkIf -> cont 22;
	TkType -> cont 23;
	TkQuote -> cont 24;
	TkComma -> cont 25;
	TkSemicolon -> cont 26;
	TkId  happy_dollar_dollar -> cont 27;
	TkNum happy_dollar_dollar -> cont 28;
	TkPower -> cont 29;
	TkMult -> cont 30;
	TkPlus -> cont 31;
	TkMinus -> cont 32;
	TkLE -> cont 33;
	TkGE -> cont 34;
	TkLT -> cont 35;
	TkGT -> cont 36;
	TkEQ -> cont 37;
	TkNE -> cont 38;
	TkNot -> cont 39;
	TkOr -> cont 40;
	TkAnd -> cont 41;
	TkAssign -> cont 42;
	TkOpenPar -> cont 43;
	TkClosePar -> cont 44;
	TkOpenBrace -> cont 45;
	TkCloseBrace -> cont 46;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 47 tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

happyThen :: () => Either (String,Int) a -> (a -> Either (String,Int) b) -> Either (String,Int) b
happyThen = (Prelude.>>=)
happyReturn :: () => a -> Either (String,Int) a
happyReturn = (Prelude.return)
happyThen1 m k tks = (Prelude.>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> Either (String,Int) a
happyReturn1 = \a tks -> (Prelude.return) a
happyError' :: () => ([(Token)], [Prelude.String]) -> Either (String,Int) a
happyError' = (\(tokens, _) -> parseError tokens)
toAST tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError :: [Token] -> Either (String,Int) a
parseError fault = Left $ ("Parse error at expression: " ++ intercalate " " (map (show . PT) fault), length fault)
parseError []    = Left ("Lambda does not belong to the language",-1)

toNumC :: Int -> Expr
toNumC  = C . NumConstant

toBoolC :: Bool -> Expr
toBoolC  = C . BConstant


parseLine' :: Int -> [Token] -> Either (String,Int) S
parseLine' n tks = case toAST tks of
    Left (errMsg,remanent) -> Left (errMsg,n-remanent)
    s                      -> s

parse :: String -> Either (String,Int) S
parse input = do 
    let 
        
        manyToken' :: String -> Either (String,Int) [Token]
        manyToken' x' = case manyToken x' of
            Left (e:_)  -> Left e
            (Right tks) -> Right tks 
        
        toAST' :: [Token] -> Either (String,Int) S 
        toAST' tks = case parseLine' (length tks) tks of
            Left (err,n) -> Left (err ++ " at token: " ++ show n,n)
            ast          -> ast
        
    tokens' <- manyToken' input
    toAST' tokens'
             


parse' :: String -> String
parse' x = case parse x of
    Left (errMsg,_) -> errMsg
    Right ast       -> toPrettyS ast
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $










































data Happy_IntList = HappyCons Prelude.Int Happy_IntList








































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
happyLt x y = (x Prelude.< y)






readArrayBit arr bit =
    Bits.testBit (indexShortOffAddr arr (bit `Prelude.div` 16)) (bit `Prelude.mod` 16)






-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Prelude.Int ->                    -- token number
         Prelude.Int ->                    -- token number (yes, again)
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
     = case happyDrop (k Prelude.- ((1) :: Prelude.Int)) sts of
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





             _ = nt :: Prelude.Int
             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n Prelude.- ((1) :: Prelude.Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Prelude.- ((1)::Prelude.Int)) xs

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
notHappyAtAll = Prelude.error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `Prelude.seq` b
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
