{-# OPTIONS_GHC -w #-}
module HGrammar.HGrammar (toAST, sToTree, prettyPrintS, parse ) where

import Data.Char
import Lexer.Lexer (Token(..), manyToken)
import Data.Tree  
import Data.Tree.Pretty (drawVerticalTree)
import AST.AST
import Data.List (intercalate)
import Prelude hiding (EQ,LT,GT)
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.12

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14
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

happyExpList :: Happy_Data_Array.Array Int Int
happyExpList = Happy_Data_Array.listArray (0,222) ([49152,52366,1088,24576,26176,544,0,64640,111,0,0,0,0,0,0,0,0,0,0,0,0,384,33177,8,0,0,4,0,0,0,8240,4147,1,36888,34841,0,51212,17420,0,25606,8710,0,0,0,0,1024,0,0,0,0,0,512,0,0,0,0,49152,1,0,0,0,0,0,0,0,0,0,24,384,33177,8,32960,16588,4,0,0,0,0,0,0,0,16384,0,0,0,0,32768,19967,0,16384,0,0,8192,0,0,4096,0,0,63552,223,0,0,0,6144,6544,136,3072,3272,68,1536,1636,34,768,818,17,384,33177,8,32960,16588,4,16480,8294,2,8240,4147,1,36888,34841,0,51212,17420,0,25606,8710,0,12803,4355,0,57344,639,0,61440,447,0,63488,7,0,64512,3,0,7680,0,0,3840,0,0,1920,0,0,960,0,0,96,0,0,48,0,0,8,0,0,4,0,0,0,0,0,0,0,32768,3583,0,0,8192,0,512,0,0,61440,447,24576,26176,544,0,0,0,0,65024,55,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_toAST","S","Action","Declaration","Type","Assignment","Expr","Constant","FApp","Args","NEArgs","Unary","true","false","mod","lazy","int","bool","while","if","type","'`'","','","';'","TkId","TkNum","'^'","'*'","'+'","'-'","'<='","'>='","'<'","'>'","'='","'<>'","'!'","'||'","'&&'","':='","'('","')'","'{'","'}'","%eof"]
        bit_start = st * 47
        bit_end = (st + 1) * 47
        read_bit = readArrayBit happyExpList
        bits = map read_bit [bit_start..bit_end - 1]
        bits_indexed = zip bits [0..46]
        token_strs_expected = concatMap f bits_indexed
        f (False, _) = []
        f (True, nr) = [token_strs !! nr]

action_0 (15) = happyShift action_5
action_0 (16) = happyShift action_6
action_0 (18) = happyShift action_19
action_0 (19) = happyShift action_20
action_0 (20) = happyShift action_21
action_0 (24) = happyShift action_7
action_0 (27) = happyShift action_22
action_0 (28) = happyShift action_9
action_0 (31) = happyShift action_10
action_0 (32) = happyShift action_11
action_0 (39) = happyShift action_12
action_0 (43) = happyShift action_13
action_0 (4) = happyGoto action_14
action_0 (5) = happyGoto action_15
action_0 (6) = happyGoto action_16
action_0 (7) = happyGoto action_17
action_0 (8) = happyGoto action_18
action_0 (9) = happyGoto action_2
action_0 (10) = happyGoto action_3
action_0 (11) = happyGoto action_4
action_0 _ = happyFail (happyExpListPerState 0)

action_1 (15) = happyShift action_5
action_1 (16) = happyShift action_6
action_1 (24) = happyShift action_7
action_1 (27) = happyShift action_8
action_1 (28) = happyShift action_9
action_1 (31) = happyShift action_10
action_1 (32) = happyShift action_11
action_1 (39) = happyShift action_12
action_1 (43) = happyShift action_13
action_1 (9) = happyGoto action_2
action_1 (10) = happyGoto action_3
action_1 (11) = happyGoto action_4
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (26) = happyShift action_34
action_2 (29) = happyShift action_35
action_2 (30) = happyShift action_36
action_2 (31) = happyShift action_37
action_2 (32) = happyShift action_38
action_2 (33) = happyShift action_39
action_2 (34) = happyShift action_40
action_2 (35) = happyShift action_41
action_2 (36) = happyShift action_42
action_2 (37) = happyShift action_43
action_2 (38) = happyShift action_44
action_2 (40) = happyShift action_45
action_2 (41) = happyShift action_46
action_2 _ = happyFail (happyExpListPerState 2)

action_3 _ = happyReduce_29

action_4 _ = happyReduce_28

action_5 _ = happyReduce_30

action_6 _ = happyReduce_31

action_7 (15) = happyShift action_5
action_7 (16) = happyShift action_6
action_7 (24) = happyShift action_7
action_7 (27) = happyShift action_8
action_7 (28) = happyShift action_9
action_7 (31) = happyShift action_10
action_7 (32) = happyShift action_11
action_7 (39) = happyShift action_12
action_7 (43) = happyShift action_13
action_7 (9) = happyGoto action_33
action_7 (10) = happyGoto action_3
action_7 (11) = happyGoto action_4
action_7 _ = happyFail (happyExpListPerState 7)

action_8 (43) = happyShift action_24
action_8 _ = happyReduce_25

action_9 _ = happyReduce_32

action_10 (15) = happyShift action_5
action_10 (16) = happyShift action_6
action_10 (24) = happyShift action_7
action_10 (27) = happyShift action_8
action_10 (28) = happyShift action_9
action_10 (31) = happyShift action_10
action_10 (32) = happyShift action_11
action_10 (39) = happyShift action_12
action_10 (43) = happyShift action_13
action_10 (9) = happyGoto action_32
action_10 (10) = happyGoto action_3
action_10 (11) = happyGoto action_4
action_10 _ = happyFail (happyExpListPerState 10)

action_11 (15) = happyShift action_5
action_11 (16) = happyShift action_6
action_11 (24) = happyShift action_7
action_11 (27) = happyShift action_8
action_11 (28) = happyShift action_9
action_11 (31) = happyShift action_10
action_11 (32) = happyShift action_11
action_11 (39) = happyShift action_12
action_11 (43) = happyShift action_13
action_11 (9) = happyGoto action_31
action_11 (10) = happyGoto action_3
action_11 (11) = happyGoto action_4
action_11 _ = happyFail (happyExpListPerState 11)

action_12 (15) = happyShift action_5
action_12 (16) = happyShift action_6
action_12 (24) = happyShift action_7
action_12 (27) = happyShift action_8
action_12 (28) = happyShift action_9
action_12 (31) = happyShift action_10
action_12 (32) = happyShift action_11
action_12 (39) = happyShift action_12
action_12 (43) = happyShift action_13
action_12 (9) = happyGoto action_30
action_12 (10) = happyGoto action_3
action_12 (11) = happyGoto action_4
action_12 _ = happyFail (happyExpListPerState 12)

action_13 (15) = happyShift action_5
action_13 (16) = happyShift action_6
action_13 (24) = happyShift action_7
action_13 (27) = happyShift action_8
action_13 (28) = happyShift action_9
action_13 (31) = happyShift action_10
action_13 (32) = happyShift action_11
action_13 (39) = happyShift action_12
action_13 (43) = happyShift action_13
action_13 (9) = happyGoto action_29
action_13 (10) = happyGoto action_3
action_13 (11) = happyGoto action_4
action_13 _ = happyFail (happyExpListPerState 13)

action_14 (47) = happyAccept
action_14 _ = happyFail (happyExpListPerState 14)

action_15 (26) = happyShift action_28
action_15 _ = happyFail (happyExpListPerState 15)

action_16 _ = happyReduce_3

action_17 (27) = happyShift action_27
action_17 (8) = happyGoto action_26
action_17 _ = happyFail (happyExpListPerState 17)

action_18 _ = happyReduce_4

action_19 (18) = happyShift action_19
action_19 (19) = happyShift action_20
action_19 (20) = happyShift action_21
action_19 (7) = happyGoto action_25
action_19 _ = happyFail (happyExpListPerState 19)

action_20 _ = happyReduce_6

action_21 _ = happyReduce_7

action_22 (42) = happyShift action_23
action_22 (43) = happyShift action_24
action_22 _ = happyReduce_25

action_23 (15) = happyShift action_5
action_23 (16) = happyShift action_6
action_23 (24) = happyShift action_7
action_23 (27) = happyShift action_8
action_23 (28) = happyShift action_9
action_23 (31) = happyShift action_10
action_23 (32) = happyShift action_11
action_23 (39) = happyShift action_12
action_23 (43) = happyShift action_13
action_23 (9) = happyGoto action_64
action_23 (10) = happyGoto action_3
action_23 (11) = happyGoto action_4
action_23 _ = happyFail (happyExpListPerState 23)

action_24 (15) = happyShift action_5
action_24 (16) = happyShift action_6
action_24 (24) = happyShift action_7
action_24 (27) = happyShift action_8
action_24 (28) = happyShift action_9
action_24 (31) = happyShift action_10
action_24 (32) = happyShift action_11
action_24 (39) = happyShift action_12
action_24 (43) = happyShift action_13
action_24 (9) = happyGoto action_61
action_24 (10) = happyGoto action_3
action_24 (11) = happyGoto action_4
action_24 (12) = happyGoto action_62
action_24 (13) = happyGoto action_63
action_24 _ = happyReduce_34

action_25 _ = happyReduce_8

action_26 _ = happyReduce_5

action_27 (42) = happyShift action_23
action_27 _ = happyFail (happyExpListPerState 27)

action_28 _ = happyReduce_2

action_29 (29) = happyShift action_35
action_29 (30) = happyShift action_36
action_29 (31) = happyShift action_37
action_29 (32) = happyShift action_38
action_29 (33) = happyShift action_39
action_29 (34) = happyShift action_40
action_29 (35) = happyShift action_41
action_29 (36) = happyShift action_42
action_29 (37) = happyShift action_43
action_29 (38) = happyShift action_44
action_29 (40) = happyShift action_45
action_29 (41) = happyShift action_46
action_29 (44) = happyShift action_60
action_29 _ = happyFail (happyExpListPerState 29)

action_30 (29) = happyShift action_35
action_30 _ = happyReduce_12

action_31 (29) = happyShift action_35
action_31 _ = happyReduce_10

action_32 (29) = happyShift action_35
action_32 _ = happyReduce_11

action_33 (24) = happyShift action_59
action_33 (29) = happyShift action_35
action_33 (30) = happyShift action_36
action_33 (31) = happyShift action_37
action_33 (32) = happyShift action_38
action_33 (33) = happyShift action_39
action_33 (34) = happyShift action_40
action_33 (35) = happyShift action_41
action_33 (36) = happyShift action_42
action_33 (37) = happyShift action_43
action_33 (38) = happyShift action_44
action_33 (40) = happyShift action_45
action_33 (41) = happyShift action_46
action_33 _ = happyFail (happyExpListPerState 33)

action_34 _ = happyReduce_1

action_35 (15) = happyShift action_5
action_35 (16) = happyShift action_6
action_35 (24) = happyShift action_7
action_35 (27) = happyShift action_8
action_35 (28) = happyShift action_9
action_35 (31) = happyShift action_10
action_35 (32) = happyShift action_11
action_35 (39) = happyShift action_12
action_35 (43) = happyShift action_13
action_35 (9) = happyGoto action_58
action_35 (10) = happyGoto action_3
action_35 (11) = happyGoto action_4
action_35 _ = happyFail (happyExpListPerState 35)

action_36 (15) = happyShift action_5
action_36 (16) = happyShift action_6
action_36 (24) = happyShift action_7
action_36 (27) = happyShift action_8
action_36 (28) = happyShift action_9
action_36 (31) = happyShift action_10
action_36 (32) = happyShift action_11
action_36 (39) = happyShift action_12
action_36 (43) = happyShift action_13
action_36 (9) = happyGoto action_57
action_36 (10) = happyGoto action_3
action_36 (11) = happyGoto action_4
action_36 _ = happyFail (happyExpListPerState 36)

action_37 (15) = happyShift action_5
action_37 (16) = happyShift action_6
action_37 (24) = happyShift action_7
action_37 (27) = happyShift action_8
action_37 (28) = happyShift action_9
action_37 (31) = happyShift action_10
action_37 (32) = happyShift action_11
action_37 (39) = happyShift action_12
action_37 (43) = happyShift action_13
action_37 (9) = happyGoto action_56
action_37 (10) = happyGoto action_3
action_37 (11) = happyGoto action_4
action_37 _ = happyFail (happyExpListPerState 37)

action_38 (15) = happyShift action_5
action_38 (16) = happyShift action_6
action_38 (24) = happyShift action_7
action_38 (27) = happyShift action_8
action_38 (28) = happyShift action_9
action_38 (31) = happyShift action_10
action_38 (32) = happyShift action_11
action_38 (39) = happyShift action_12
action_38 (43) = happyShift action_13
action_38 (9) = happyGoto action_55
action_38 (10) = happyGoto action_3
action_38 (11) = happyGoto action_4
action_38 _ = happyFail (happyExpListPerState 38)

action_39 (15) = happyShift action_5
action_39 (16) = happyShift action_6
action_39 (24) = happyShift action_7
action_39 (27) = happyShift action_8
action_39 (28) = happyShift action_9
action_39 (31) = happyShift action_10
action_39 (32) = happyShift action_11
action_39 (39) = happyShift action_12
action_39 (43) = happyShift action_13
action_39 (9) = happyGoto action_54
action_39 (10) = happyGoto action_3
action_39 (11) = happyGoto action_4
action_39 _ = happyFail (happyExpListPerState 39)

action_40 (15) = happyShift action_5
action_40 (16) = happyShift action_6
action_40 (24) = happyShift action_7
action_40 (27) = happyShift action_8
action_40 (28) = happyShift action_9
action_40 (31) = happyShift action_10
action_40 (32) = happyShift action_11
action_40 (39) = happyShift action_12
action_40 (43) = happyShift action_13
action_40 (9) = happyGoto action_53
action_40 (10) = happyGoto action_3
action_40 (11) = happyGoto action_4
action_40 _ = happyFail (happyExpListPerState 40)

action_41 (15) = happyShift action_5
action_41 (16) = happyShift action_6
action_41 (24) = happyShift action_7
action_41 (27) = happyShift action_8
action_41 (28) = happyShift action_9
action_41 (31) = happyShift action_10
action_41 (32) = happyShift action_11
action_41 (39) = happyShift action_12
action_41 (43) = happyShift action_13
action_41 (9) = happyGoto action_52
action_41 (10) = happyGoto action_3
action_41 (11) = happyGoto action_4
action_41 _ = happyFail (happyExpListPerState 41)

action_42 (15) = happyShift action_5
action_42 (16) = happyShift action_6
action_42 (24) = happyShift action_7
action_42 (27) = happyShift action_8
action_42 (28) = happyShift action_9
action_42 (31) = happyShift action_10
action_42 (32) = happyShift action_11
action_42 (39) = happyShift action_12
action_42 (43) = happyShift action_13
action_42 (9) = happyGoto action_51
action_42 (10) = happyGoto action_3
action_42 (11) = happyGoto action_4
action_42 _ = happyFail (happyExpListPerState 42)

action_43 (15) = happyShift action_5
action_43 (16) = happyShift action_6
action_43 (24) = happyShift action_7
action_43 (27) = happyShift action_8
action_43 (28) = happyShift action_9
action_43 (31) = happyShift action_10
action_43 (32) = happyShift action_11
action_43 (39) = happyShift action_12
action_43 (43) = happyShift action_13
action_43 (9) = happyGoto action_50
action_43 (10) = happyGoto action_3
action_43 (11) = happyGoto action_4
action_43 _ = happyFail (happyExpListPerState 43)

action_44 (15) = happyShift action_5
action_44 (16) = happyShift action_6
action_44 (24) = happyShift action_7
action_44 (27) = happyShift action_8
action_44 (28) = happyShift action_9
action_44 (31) = happyShift action_10
action_44 (32) = happyShift action_11
action_44 (39) = happyShift action_12
action_44 (43) = happyShift action_13
action_44 (9) = happyGoto action_49
action_44 (10) = happyGoto action_3
action_44 (11) = happyGoto action_4
action_44 _ = happyFail (happyExpListPerState 44)

action_45 (15) = happyShift action_5
action_45 (16) = happyShift action_6
action_45 (24) = happyShift action_7
action_45 (27) = happyShift action_8
action_45 (28) = happyShift action_9
action_45 (31) = happyShift action_10
action_45 (32) = happyShift action_11
action_45 (39) = happyShift action_12
action_45 (43) = happyShift action_13
action_45 (9) = happyGoto action_48
action_45 (10) = happyGoto action_3
action_45 (11) = happyGoto action_4
action_45 _ = happyFail (happyExpListPerState 45)

action_46 (15) = happyShift action_5
action_46 (16) = happyShift action_6
action_46 (24) = happyShift action_7
action_46 (27) = happyShift action_8
action_46 (28) = happyShift action_9
action_46 (31) = happyShift action_10
action_46 (32) = happyShift action_11
action_46 (39) = happyShift action_12
action_46 (43) = happyShift action_13
action_46 (9) = happyGoto action_47
action_46 (10) = happyGoto action_3
action_46 (11) = happyGoto action_4
action_46 _ = happyFail (happyExpListPerState 46)

action_47 (29) = happyShift action_35
action_47 (30) = happyShift action_36
action_47 (31) = happyShift action_37
action_47 (32) = happyShift action_38
action_47 (33) = happyShift action_39
action_47 (34) = happyShift action_40
action_47 (35) = happyShift action_41
action_47 (36) = happyShift action_42
action_47 (37) = happyShift action_43
action_47 (38) = happyShift action_44
action_47 (41) = happyShift action_46
action_47 _ = happyReduce_24

action_48 (29) = happyShift action_35
action_48 (30) = happyShift action_36
action_48 (31) = happyShift action_37
action_48 (32) = happyShift action_38
action_48 (33) = happyShift action_39
action_48 (34) = happyShift action_40
action_48 (35) = happyShift action_41
action_48 (36) = happyShift action_42
action_48 (37) = happyShift action_43
action_48 (38) = happyShift action_44
action_48 (40) = happyShift action_45
action_48 (41) = happyShift action_46
action_48 _ = happyReduce_23

action_49 (29) = happyShift action_35
action_49 (30) = happyShift action_36
action_49 (31) = happyShift action_37
action_49 (32) = happyShift action_38
action_49 (33) = happyShift action_39
action_49 (34) = happyShift action_40
action_49 (35) = happyShift action_41
action_49 (36) = happyShift action_42
action_49 (37) = happyFail []
action_49 (38) = happyFail []
action_49 _ = happyReduce_14

action_50 (29) = happyShift action_35
action_50 (30) = happyShift action_36
action_50 (31) = happyShift action_37
action_50 (32) = happyShift action_38
action_50 (33) = happyShift action_39
action_50 (34) = happyShift action_40
action_50 (35) = happyShift action_41
action_50 (36) = happyShift action_42
action_50 (37) = happyFail []
action_50 (38) = happyFail []
action_50 _ = happyReduce_13

action_51 (29) = happyShift action_35
action_51 (30) = happyShift action_36
action_51 (31) = happyShift action_37
action_51 (32) = happyShift action_38
action_51 (33) = happyFail []
action_51 (34) = happyFail []
action_51 (35) = happyFail []
action_51 (36) = happyFail []
action_51 _ = happyReduce_20

action_52 (29) = happyShift action_35
action_52 (30) = happyShift action_36
action_52 (31) = happyShift action_37
action_52 (32) = happyShift action_38
action_52 (33) = happyFail []
action_52 (34) = happyFail []
action_52 (35) = happyFail []
action_52 (36) = happyFail []
action_52 _ = happyReduce_19

action_53 (29) = happyShift action_35
action_53 (30) = happyShift action_36
action_53 (31) = happyShift action_37
action_53 (32) = happyShift action_38
action_53 (33) = happyFail []
action_53 (34) = happyFail []
action_53 (35) = happyFail []
action_53 (36) = happyFail []
action_53 _ = happyReduce_22

action_54 (29) = happyShift action_35
action_54 (30) = happyShift action_36
action_54 (31) = happyShift action_37
action_54 (32) = happyShift action_38
action_54 (33) = happyFail []
action_54 (34) = happyFail []
action_54 (35) = happyFail []
action_54 (36) = happyFail []
action_54 _ = happyReduce_21

action_55 (29) = happyShift action_35
action_55 (30) = happyShift action_36
action_55 _ = happyReduce_16

action_56 (29) = happyShift action_35
action_56 (30) = happyShift action_36
action_56 _ = happyReduce_15

action_57 (29) = happyShift action_35
action_57 _ = happyReduce_17

action_58 (29) = happyShift action_35
action_58 _ = happyReduce_18

action_59 _ = happyReduce_27

action_60 _ = happyReduce_26

action_61 (29) = happyShift action_35
action_61 (30) = happyShift action_36
action_61 (31) = happyShift action_37
action_61 (32) = happyShift action_38
action_61 (33) = happyShift action_39
action_61 (34) = happyShift action_40
action_61 (35) = happyShift action_41
action_61 (36) = happyShift action_42
action_61 (37) = happyShift action_43
action_61 (38) = happyShift action_44
action_61 (40) = happyShift action_45
action_61 (41) = happyShift action_46
action_61 _ = happyReduce_36

action_62 (44) = happyShift action_66
action_62 _ = happyFail (happyExpListPerState 62)

action_63 (25) = happyShift action_65
action_63 _ = happyReduce_35

action_64 (29) = happyShift action_35
action_64 (30) = happyShift action_36
action_64 (31) = happyShift action_37
action_64 (32) = happyShift action_38
action_64 (33) = happyShift action_39
action_64 (34) = happyShift action_40
action_64 (35) = happyShift action_41
action_64 (36) = happyShift action_42
action_64 (37) = happyShift action_43
action_64 (38) = happyShift action_44
action_64 (40) = happyShift action_45
action_64 (41) = happyShift action_46
action_64 _ = happyReduce_9

action_65 (15) = happyShift action_5
action_65 (16) = happyShift action_6
action_65 (24) = happyShift action_7
action_65 (27) = happyShift action_8
action_65 (28) = happyShift action_9
action_65 (31) = happyShift action_10
action_65 (32) = happyShift action_11
action_65 (39) = happyShift action_12
action_65 (43) = happyShift action_13
action_65 (9) = happyGoto action_67
action_65 (10) = happyGoto action_3
action_65 (11) = happyGoto action_4
action_65 _ = happyReduce_38

action_66 _ = happyReduce_33

action_67 (29) = happyShift action_35
action_67 (30) = happyShift action_36
action_67 (31) = happyShift action_37
action_67 (32) = happyShift action_38
action_67 (33) = happyShift action_39
action_67 (34) = happyShift action_40
action_67 (35) = happyShift action_41
action_67 (36) = happyShift action_42
action_67 (37) = happyShift action_43
action_67 (38) = happyShift action_44
action_67 (40) = happyShift action_45
action_67 (41) = happyShift action_46
action_67 _ = happyReduce_37

happyReduce_1 = happySpecReduce_2  4 happyReduction_1
happyReduction_1 _
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn4
		 (E   happy_var_1
	)
happyReduction_1 _ _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_2  4 happyReduction_2
happyReduction_2 _
	(HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn4
		 (A   happy_var_1
	)
happyReduction_2 _ _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_1  5 happyReduction_3
happyReduction_3 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_1
	)
happyReduction_3 _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_1  5 happyReduction_4
happyReduction_4 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_1
	)
happyReduction_4 _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_2  6 happyReduction_5
happyReduction_5 (HappyAbsSyn8  happy_var_2)
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn6
		 ((\(Assignment s e) -> Declaration happy_var_1 s e )  happy_var_2
	)
happyReduction_5 _ _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_1  7 happyReduction_6
happyReduction_6 _
	 =  HappyAbsSyn7
		 (LInt
	)

happyReduce_7 = happySpecReduce_1  7 happyReduction_7
happyReduction_7 _
	 =  HappyAbsSyn7
		 (LBool
	)

happyReduce_8 = happySpecReduce_2  7 happyReduction_8
happyReduction_8 (HappyAbsSyn7  happy_var_2)
	_
	 =  HappyAbsSyn7
		 (LLazy happy_var_2
	)
happyReduction_8 _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_3  8 happyReduction_9
happyReduction_9 (HappyAbsSyn9  happy_var_3)
	_
	(HappyTerminal (TkId  happy_var_1))
	 =  HappyAbsSyn8
		 (Assignment happy_var_1 happy_var_3
	)
happyReduction_9 _ _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_2  9 happyReduction_10
happyReduction_10 (HappyAbsSyn9  happy_var_2)
	_
	 =  HappyAbsSyn9
		 (Negate   happy_var_2
	)
happyReduction_10 _ _  = notHappyAtAll 

happyReduce_11 = happySpecReduce_2  9 happyReduction_11
happyReduction_11 (HappyAbsSyn9  happy_var_2)
	_
	 =  HappyAbsSyn9
		 (Pos      happy_var_2
	)
happyReduction_11 _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_2  9 happyReduction_12
happyReduction_12 (HappyAbsSyn9  happy_var_2)
	_
	 =  HappyAbsSyn9
		 (Not      happy_var_2
	)
happyReduction_12 _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_3  9 happyReduction_13
happyReduction_13 (HappyAbsSyn9  happy_var_3)
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (EQ    happy_var_1 happy_var_3
	)
happyReduction_13 _ _ _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_3  9 happyReduction_14
happyReduction_14 (HappyAbsSyn9  happy_var_3)
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (NEQ   happy_var_1 happy_var_3
	)
happyReduction_14 _ _ _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_3  9 happyReduction_15
happyReduction_15 (HappyAbsSyn9  happy_var_3)
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (Plus  happy_var_1 happy_var_3
	)
happyReduction_15 _ _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_3  9 happyReduction_16
happyReduction_16 (HappyAbsSyn9  happy_var_3)
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (Minus happy_var_1 happy_var_3
	)
happyReduction_16 _ _ _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_3  9 happyReduction_17
happyReduction_17 (HappyAbsSyn9  happy_var_3)
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (Times happy_var_1 happy_var_3
	)
happyReduction_17 _ _ _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_3  9 happyReduction_18
happyReduction_18 (HappyAbsSyn9  happy_var_3)
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (Pow   happy_var_1 happy_var_3
	)
happyReduction_18 _ _ _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_3  9 happyReduction_19
happyReduction_19 (HappyAbsSyn9  happy_var_3)
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (LT    happy_var_1 happy_var_3
	)
happyReduction_19 _ _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_3  9 happyReduction_20
happyReduction_20 (HappyAbsSyn9  happy_var_3)
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (GT    happy_var_1 happy_var_3
	)
happyReduction_20 _ _ _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_3  9 happyReduction_21
happyReduction_21 (HappyAbsSyn9  happy_var_3)
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (LE    happy_var_1 happy_var_3
	)
happyReduction_21 _ _ _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_3  9 happyReduction_22
happyReduction_22 (HappyAbsSyn9  happy_var_3)
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (GE    happy_var_1 happy_var_3
	)
happyReduction_22 _ _ _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_3  9 happyReduction_23
happyReduction_23 (HappyAbsSyn9  happy_var_3)
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (Or    happy_var_1 happy_var_3
	)
happyReduction_23 _ _ _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_3  9 happyReduction_24
happyReduction_24 (HappyAbsSyn9  happy_var_3)
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (And   happy_var_1 happy_var_3
	)
happyReduction_24 _ _ _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_1  9 happyReduction_25
happyReduction_25 (HappyTerminal (TkId  happy_var_1))
	 =  HappyAbsSyn9
		 (Var   happy_var_1
	)
happyReduction_25 _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_3  9 happyReduction_26
happyReduction_26 _
	(HappyAbsSyn9  happy_var_2)
	_
	 =  HappyAbsSyn9
		 (happy_var_2
	)
happyReduction_26 _ _ _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_3  9 happyReduction_27
happyReduction_27 _
	(HappyAbsSyn9  happy_var_2)
	_
	 =  HappyAbsSyn9
		 (Lazy  happy_var_2
	)
happyReduction_27 _ _ _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_1  9 happyReduction_28
happyReduction_28 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_1
	)
happyReduction_28 _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_1  9 happyReduction_29
happyReduction_29 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_1
	)
happyReduction_29 _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_1  10 happyReduction_30
happyReduction_30 _
	 =  HappyAbsSyn10
		 (toBoolC True
	)

happyReduce_31 = happySpecReduce_1  10 happyReduction_31
happyReduction_31 _
	 =  HappyAbsSyn10
		 (toBoolC False
	)

happyReduce_32 = happySpecReduce_1  10 happyReduction_32
happyReduction_32 (HappyTerminal (TkNum happy_var_1))
	 =  HappyAbsSyn10
		 (toNumC  happy_var_1
	)
happyReduction_32 _  = notHappyAtAll 

happyReduce_33 = happyReduce 4 11 happyReduction_33
happyReduction_33 (_ `HappyStk`
	(HappyAbsSyn12  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TkId  happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn11
		 (FApp  happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_34 = happySpecReduce_0  12 happyReduction_34
happyReduction_34  =  HappyAbsSyn12
		 ([]
	)

happyReduce_35 = happySpecReduce_1  12 happyReduction_35
happyReduction_35 (HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn12
		 (reverse happy_var_1
	)
happyReduction_35 _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_1  13 happyReduction_36
happyReduction_36 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn13
		 (happy_var_1 : []
	)
happyReduction_36 _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_3  13 happyReduction_37
happyReduction_37 (HappyAbsSyn9  happy_var_3)
	_
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn13
		 (happy_var_3 : happy_var_1
	)
happyReduction_37 _ _ _  = notHappyAtAll 

happyReduce_38 = happyMonadReduce 2 13 happyReduction_38
happyReduction_38 (_ `HappyStk`
	_ `HappyStk`
	happyRest) tk
	 = happyThen ((( Left ("Parse error on argument list. ", 0)))
	) (\r -> happyReturn (HappyAbsSyn13 r))

happyReduce_39 = happySpecReduce_1  14 happyReduction_39
happyReduction_39 (HappyTerminal (TkNum happy_var_1))
	 =  HappyAbsSyn14
		 (toNumC  happy_var_1
	)
happyReduction_39 _  = notHappyAtAll 

happyReduce_40 = happySpecReduce_1  14 happyReduction_40
happyReduction_40 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn14
		 (happy_var_1
	)
happyReduction_40 _  = notHappyAtAll 

happyReduce_41 = happySpecReduce_1  14 happyReduction_41
happyReduction_41 (HappyTerminal (TkId  happy_var_1))
	 =  HappyAbsSyn14
		 (Var     happy_var_1
	)
happyReduction_41 _  = notHappyAtAll 

happyReduce_42 = happySpecReduce_3  14 happyReduction_42
happyReduction_42 _
	(HappyAbsSyn9  happy_var_2)
	_
	 =  HappyAbsSyn14
		 (happy_var_2
	)
happyReduction_42 _ _ _  = notHappyAtAll 

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
happyThen = (>>=)
happyReturn :: () => a -> Either (String,Int) a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> Either (String,Int) a
happyReturn1 = \a tks -> (return) a
happyError' :: () => ([(Token)], [String]) -> Either (String,Int) a
happyError' = (\(tokens, _) -> parseError tokens)
toAST tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError :: [Token] -> Either (String,Int) a
parseError fault = Left $ ("Parse error at expression: " ++ intercalate " " (map show fault), length fault)
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
