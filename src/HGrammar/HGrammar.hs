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
happyExpList = Happy_Data_Array.listArray (0,238) ([49152,52366,1088,24576,26176,544,0,64512,111,0,0,0,0,0,0,0,0,0,0,0,0,384,33177,8,0,0,4,0,0,0,8240,4147,1,36888,34841,0,51212,17420,0,25606,8710,0,0,0,0,1024,0,0,0,0,0,512,0,0,0,0,0,65088,55,57344,0,0,0,0,0,0,0,0,0,0,12,32960,16588,4,16480,8294,2,0,0,0,37336,34841,0,51212,17420,0,25606,8710,0,12803,4355,32768,39169,2177,49152,52352,1088,24576,26176,544,12288,13088,272,6144,6544,136,3072,3272,68,1536,1636,34,768,818,17,384,33177,8,0,0,0,0,0,1,9136,4147,1,0,14334,1,0,1,0,32768,0,0,16384,0,0,57600,895,0,0,0,0,0,0,0,0,0,0,65024,39,0,65280,27,0,32640,0,0,16320,0,0,480,0,0,240,0,0,120,0,0,60,0,0,6,0,0,3,0,32768,0,0,16384,0,0,0,0,0,61440,447,0,0,1024,0,64,0,0,65024,55,3072,3272,68,0,0,0,0,65472,6,0
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
action_0 (18) = happyShift action_20
action_0 (19) = happyShift action_21
action_0 (20) = happyShift action_22
action_0 (24) = happyShift action_7
action_0 (27) = happyShift action_23
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
action_0 (9) = happyGoto action_19
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

action_2 (29) = happyShift action_28
action_2 (30) = happyShift action_29
action_2 (31) = happyShift action_30
action_2 (32) = happyShift action_31
action_2 (33) = happyShift action_32
action_2 (34) = happyShift action_33
action_2 (35) = happyShift action_34
action_2 (36) = happyShift action_35
action_2 (37) = happyShift action_36
action_2 (38) = happyShift action_37
action_2 (40) = happyShift action_38
action_2 (41) = happyShift action_39
action_2 _ = happyFail (happyExpListPerState 2)

action_3 _ = happyReduce_33

action_4 _ = happyReduce_32

action_5 _ = happyReduce_34

action_6 _ = happyReduce_35

action_7 (15) = happyShift action_5
action_7 (16) = happyShift action_6
action_7 (24) = happyShift action_7
action_7 (27) = happyShift action_8
action_7 (28) = happyShift action_9
action_7 (31) = happyShift action_10
action_7 (32) = happyShift action_11
action_7 (39) = happyShift action_12
action_7 (43) = happyShift action_13
action_7 (9) = happyGoto action_47
action_7 (10) = happyGoto action_3
action_7 (11) = happyGoto action_4
action_7 _ = happyFail (happyExpListPerState 7)

action_8 (43) = happyShift action_25
action_8 _ = happyReduce_29

action_9 _ = happyReduce_36

action_10 (15) = happyShift action_5
action_10 (16) = happyShift action_6
action_10 (24) = happyShift action_7
action_10 (27) = happyShift action_8
action_10 (28) = happyShift action_9
action_10 (31) = happyShift action_10
action_10 (32) = happyShift action_11
action_10 (39) = happyShift action_12
action_10 (43) = happyShift action_13
action_10 (9) = happyGoto action_46
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
action_11 (9) = happyGoto action_45
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
action_12 (9) = happyGoto action_44
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
action_13 (9) = happyGoto action_43
action_13 (10) = happyGoto action_3
action_13 (11) = happyGoto action_4
action_13 _ = happyFail (happyExpListPerState 13)

action_14 (47) = happyAccept
action_14 _ = happyFail (happyExpListPerState 14)

action_15 (26) = happyShift action_42
action_15 _ = happyReduce_2

action_16 _ = happyReduce_7

action_17 (27) = happyShift action_41
action_17 (8) = happyGoto action_40
action_17 _ = happyFail (happyExpListPerState 17)

action_18 _ = happyReduce_8

action_19 (26) = happyShift action_27
action_19 (29) = happyShift action_28
action_19 (30) = happyShift action_29
action_19 (31) = happyShift action_30
action_19 (32) = happyShift action_31
action_19 (33) = happyShift action_32
action_19 (34) = happyShift action_33
action_19 (35) = happyShift action_34
action_19 (36) = happyShift action_35
action_19 (37) = happyShift action_36
action_19 (38) = happyShift action_37
action_19 (40) = happyShift action_38
action_19 (41) = happyShift action_39
action_19 _ = happyReduce_1

action_20 (18) = happyShift action_20
action_20 (19) = happyShift action_21
action_20 (20) = happyShift action_22
action_20 (7) = happyGoto action_26
action_20 _ = happyFail (happyExpListPerState 20)

action_21 _ = happyReduce_10

action_22 _ = happyReduce_11

action_23 (42) = happyShift action_24
action_23 (43) = happyShift action_25
action_23 _ = happyReduce_29

action_24 (15) = happyShift action_5
action_24 (16) = happyShift action_6
action_24 (24) = happyShift action_7
action_24 (27) = happyShift action_8
action_24 (28) = happyShift action_9
action_24 (31) = happyShift action_10
action_24 (32) = happyShift action_11
action_24 (39) = happyShift action_12
action_24 (43) = happyShift action_13
action_24 (9) = happyGoto action_67
action_24 (10) = happyGoto action_3
action_24 (11) = happyGoto action_4
action_24 _ = happyFail (happyExpListPerState 24)

action_25 (15) = happyShift action_5
action_25 (16) = happyShift action_6
action_25 (24) = happyShift action_7
action_25 (27) = happyShift action_8
action_25 (28) = happyShift action_9
action_25 (31) = happyShift action_10
action_25 (32) = happyShift action_11
action_25 (39) = happyShift action_12
action_25 (43) = happyShift action_13
action_25 (9) = happyGoto action_64
action_25 (10) = happyGoto action_3
action_25 (11) = happyGoto action_4
action_25 (12) = happyGoto action_65
action_25 (13) = happyGoto action_66
action_25 _ = happyReduce_38

action_26 _ = happyReduce_12

action_27 (15) = happyShift action_5
action_27 (16) = happyShift action_6
action_27 (18) = happyShift action_20
action_27 (19) = happyShift action_21
action_27 (20) = happyShift action_22
action_27 (24) = happyShift action_7
action_27 (27) = happyShift action_23
action_27 (28) = happyShift action_9
action_27 (31) = happyShift action_10
action_27 (32) = happyShift action_11
action_27 (39) = happyShift action_12
action_27 (43) = happyShift action_13
action_27 (4) = happyGoto action_63
action_27 (5) = happyGoto action_15
action_27 (6) = happyGoto action_16
action_27 (7) = happyGoto action_17
action_27 (8) = happyGoto action_18
action_27 (9) = happyGoto action_19
action_27 (10) = happyGoto action_3
action_27 (11) = happyGoto action_4
action_27 _ = happyReduce_6

action_28 (15) = happyShift action_5
action_28 (16) = happyShift action_6
action_28 (24) = happyShift action_7
action_28 (27) = happyShift action_8
action_28 (28) = happyShift action_9
action_28 (31) = happyShift action_10
action_28 (32) = happyShift action_11
action_28 (39) = happyShift action_12
action_28 (43) = happyShift action_13
action_28 (9) = happyGoto action_62
action_28 (10) = happyGoto action_3
action_28 (11) = happyGoto action_4
action_28 _ = happyFail (happyExpListPerState 28)

action_29 (15) = happyShift action_5
action_29 (16) = happyShift action_6
action_29 (24) = happyShift action_7
action_29 (27) = happyShift action_8
action_29 (28) = happyShift action_9
action_29 (31) = happyShift action_10
action_29 (32) = happyShift action_11
action_29 (39) = happyShift action_12
action_29 (43) = happyShift action_13
action_29 (9) = happyGoto action_61
action_29 (10) = happyGoto action_3
action_29 (11) = happyGoto action_4
action_29 _ = happyFail (happyExpListPerState 29)

action_30 (15) = happyShift action_5
action_30 (16) = happyShift action_6
action_30 (24) = happyShift action_7
action_30 (27) = happyShift action_8
action_30 (28) = happyShift action_9
action_30 (31) = happyShift action_10
action_30 (32) = happyShift action_11
action_30 (39) = happyShift action_12
action_30 (43) = happyShift action_13
action_30 (9) = happyGoto action_60
action_30 (10) = happyGoto action_3
action_30 (11) = happyGoto action_4
action_30 _ = happyFail (happyExpListPerState 30)

action_31 (15) = happyShift action_5
action_31 (16) = happyShift action_6
action_31 (24) = happyShift action_7
action_31 (27) = happyShift action_8
action_31 (28) = happyShift action_9
action_31 (31) = happyShift action_10
action_31 (32) = happyShift action_11
action_31 (39) = happyShift action_12
action_31 (43) = happyShift action_13
action_31 (9) = happyGoto action_59
action_31 (10) = happyGoto action_3
action_31 (11) = happyGoto action_4
action_31 _ = happyFail (happyExpListPerState 31)

action_32 (15) = happyShift action_5
action_32 (16) = happyShift action_6
action_32 (24) = happyShift action_7
action_32 (27) = happyShift action_8
action_32 (28) = happyShift action_9
action_32 (31) = happyShift action_10
action_32 (32) = happyShift action_11
action_32 (39) = happyShift action_12
action_32 (43) = happyShift action_13
action_32 (9) = happyGoto action_58
action_32 (10) = happyGoto action_3
action_32 (11) = happyGoto action_4
action_32 _ = happyFail (happyExpListPerState 32)

action_33 (15) = happyShift action_5
action_33 (16) = happyShift action_6
action_33 (24) = happyShift action_7
action_33 (27) = happyShift action_8
action_33 (28) = happyShift action_9
action_33 (31) = happyShift action_10
action_33 (32) = happyShift action_11
action_33 (39) = happyShift action_12
action_33 (43) = happyShift action_13
action_33 (9) = happyGoto action_57
action_33 (10) = happyGoto action_3
action_33 (11) = happyGoto action_4
action_33 _ = happyFail (happyExpListPerState 33)

action_34 (15) = happyShift action_5
action_34 (16) = happyShift action_6
action_34 (24) = happyShift action_7
action_34 (27) = happyShift action_8
action_34 (28) = happyShift action_9
action_34 (31) = happyShift action_10
action_34 (32) = happyShift action_11
action_34 (39) = happyShift action_12
action_34 (43) = happyShift action_13
action_34 (9) = happyGoto action_56
action_34 (10) = happyGoto action_3
action_34 (11) = happyGoto action_4
action_34 _ = happyFail (happyExpListPerState 34)

action_35 (15) = happyShift action_5
action_35 (16) = happyShift action_6
action_35 (24) = happyShift action_7
action_35 (27) = happyShift action_8
action_35 (28) = happyShift action_9
action_35 (31) = happyShift action_10
action_35 (32) = happyShift action_11
action_35 (39) = happyShift action_12
action_35 (43) = happyShift action_13
action_35 (9) = happyGoto action_55
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
action_36 (9) = happyGoto action_54
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
action_37 (9) = happyGoto action_53
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
action_38 (9) = happyGoto action_52
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
action_39 (9) = happyGoto action_51
action_39 (10) = happyGoto action_3
action_39 (11) = happyGoto action_4
action_39 _ = happyFail (happyExpListPerState 39)

action_40 _ = happyReduce_9

action_41 (42) = happyShift action_24
action_41 _ = happyFail (happyExpListPerState 41)

action_42 (15) = happyShift action_5
action_42 (16) = happyShift action_6
action_42 (18) = happyShift action_20
action_42 (19) = happyShift action_21
action_42 (20) = happyShift action_22
action_42 (24) = happyShift action_7
action_42 (27) = happyShift action_23
action_42 (28) = happyShift action_9
action_42 (31) = happyShift action_10
action_42 (32) = happyShift action_11
action_42 (39) = happyShift action_12
action_42 (43) = happyShift action_13
action_42 (4) = happyGoto action_50
action_42 (5) = happyGoto action_15
action_42 (6) = happyGoto action_16
action_42 (7) = happyGoto action_17
action_42 (8) = happyGoto action_18
action_42 (9) = happyGoto action_19
action_42 (10) = happyGoto action_3
action_42 (11) = happyGoto action_4
action_42 _ = happyReduce_5

action_43 (29) = happyShift action_28
action_43 (30) = happyShift action_29
action_43 (31) = happyShift action_30
action_43 (32) = happyShift action_31
action_43 (33) = happyShift action_32
action_43 (34) = happyShift action_33
action_43 (35) = happyShift action_34
action_43 (36) = happyShift action_35
action_43 (37) = happyShift action_36
action_43 (38) = happyShift action_37
action_43 (40) = happyShift action_38
action_43 (41) = happyShift action_39
action_43 (44) = happyShift action_49
action_43 _ = happyFail (happyExpListPerState 43)

action_44 (29) = happyShift action_28
action_44 _ = happyReduce_16

action_45 (29) = happyShift action_28
action_45 _ = happyReduce_14

action_46 (29) = happyShift action_28
action_46 _ = happyReduce_15

action_47 (24) = happyShift action_48
action_47 (29) = happyShift action_28
action_47 (30) = happyShift action_29
action_47 (31) = happyShift action_30
action_47 (32) = happyShift action_31
action_47 (33) = happyShift action_32
action_47 (34) = happyShift action_33
action_47 (35) = happyShift action_34
action_47 (36) = happyShift action_35
action_47 (37) = happyShift action_36
action_47 (38) = happyShift action_37
action_47 (40) = happyShift action_38
action_47 (41) = happyShift action_39
action_47 _ = happyFail (happyExpListPerState 47)

action_48 _ = happyReduce_31

action_49 _ = happyReduce_30

action_50 _ = happyReduce_4

action_51 (29) = happyShift action_28
action_51 (30) = happyShift action_29
action_51 (31) = happyShift action_30
action_51 (32) = happyShift action_31
action_51 (33) = happyShift action_32
action_51 (34) = happyShift action_33
action_51 (35) = happyShift action_34
action_51 (36) = happyShift action_35
action_51 (37) = happyShift action_36
action_51 (38) = happyShift action_37
action_51 (41) = happyShift action_39
action_51 _ = happyReduce_28

action_52 (29) = happyShift action_28
action_52 (30) = happyShift action_29
action_52 (31) = happyShift action_30
action_52 (32) = happyShift action_31
action_52 (33) = happyShift action_32
action_52 (34) = happyShift action_33
action_52 (35) = happyShift action_34
action_52 (36) = happyShift action_35
action_52 (37) = happyShift action_36
action_52 (38) = happyShift action_37
action_52 (40) = happyShift action_38
action_52 (41) = happyShift action_39
action_52 _ = happyReduce_27

action_53 (29) = happyShift action_28
action_53 (30) = happyShift action_29
action_53 (31) = happyShift action_30
action_53 (32) = happyShift action_31
action_53 (33) = happyShift action_32
action_53 (34) = happyShift action_33
action_53 (35) = happyShift action_34
action_53 (36) = happyShift action_35
action_53 (37) = happyFail []
action_53 (38) = happyFail []
action_53 _ = happyReduce_18

action_54 (29) = happyShift action_28
action_54 (30) = happyShift action_29
action_54 (31) = happyShift action_30
action_54 (32) = happyShift action_31
action_54 (33) = happyShift action_32
action_54 (34) = happyShift action_33
action_54 (35) = happyShift action_34
action_54 (36) = happyShift action_35
action_54 (37) = happyFail []
action_54 (38) = happyFail []
action_54 _ = happyReduce_17

action_55 (29) = happyShift action_28
action_55 (30) = happyShift action_29
action_55 (31) = happyShift action_30
action_55 (32) = happyShift action_31
action_55 (33) = happyFail []
action_55 (34) = happyFail []
action_55 (35) = happyFail []
action_55 (36) = happyFail []
action_55 _ = happyReduce_24

action_56 (29) = happyShift action_28
action_56 (30) = happyShift action_29
action_56 (31) = happyShift action_30
action_56 (32) = happyShift action_31
action_56 (33) = happyFail []
action_56 (34) = happyFail []
action_56 (35) = happyFail []
action_56 (36) = happyFail []
action_56 _ = happyReduce_23

action_57 (29) = happyShift action_28
action_57 (30) = happyShift action_29
action_57 (31) = happyShift action_30
action_57 (32) = happyShift action_31
action_57 (33) = happyFail []
action_57 (34) = happyFail []
action_57 (35) = happyFail []
action_57 (36) = happyFail []
action_57 _ = happyReduce_26

action_58 (29) = happyShift action_28
action_58 (30) = happyShift action_29
action_58 (31) = happyShift action_30
action_58 (32) = happyShift action_31
action_58 (33) = happyFail []
action_58 (34) = happyFail []
action_58 (35) = happyFail []
action_58 (36) = happyFail []
action_58 _ = happyReduce_25

action_59 (29) = happyShift action_28
action_59 (30) = happyShift action_29
action_59 _ = happyReduce_20

action_60 (29) = happyShift action_28
action_60 (30) = happyShift action_29
action_60 _ = happyReduce_19

action_61 (29) = happyShift action_28
action_61 _ = happyReduce_21

action_62 (29) = happyShift action_28
action_62 _ = happyReduce_22

action_63 _ = happyReduce_3

action_64 (29) = happyShift action_28
action_64 (30) = happyShift action_29
action_64 (31) = happyShift action_30
action_64 (32) = happyShift action_31
action_64 (33) = happyShift action_32
action_64 (34) = happyShift action_33
action_64 (35) = happyShift action_34
action_64 (36) = happyShift action_35
action_64 (37) = happyShift action_36
action_64 (38) = happyShift action_37
action_64 (40) = happyShift action_38
action_64 (41) = happyShift action_39
action_64 _ = happyReduce_40

action_65 (44) = happyShift action_69
action_65 _ = happyFail (happyExpListPerState 65)

action_66 (25) = happyShift action_68
action_66 _ = happyReduce_39

action_67 (29) = happyShift action_28
action_67 (30) = happyShift action_29
action_67 (31) = happyShift action_30
action_67 (32) = happyShift action_31
action_67 (33) = happyShift action_32
action_67 (34) = happyShift action_33
action_67 (35) = happyShift action_34
action_67 (36) = happyShift action_35
action_67 (37) = happyShift action_36
action_67 (38) = happyShift action_37
action_67 (40) = happyShift action_38
action_67 (41) = happyShift action_39
action_67 _ = happyReduce_13

action_68 (15) = happyShift action_5
action_68 (16) = happyShift action_6
action_68 (24) = happyShift action_7
action_68 (27) = happyShift action_8
action_68 (28) = happyShift action_9
action_68 (31) = happyShift action_10
action_68 (32) = happyShift action_11
action_68 (39) = happyShift action_12
action_68 (43) = happyShift action_13
action_68 (9) = happyGoto action_70
action_68 (10) = happyGoto action_3
action_68 (11) = happyGoto action_4
action_68 _ = happyReduce_42

action_69 _ = happyReduce_37

action_70 (29) = happyShift action_28
action_70 (30) = happyShift action_29
action_70 (31) = happyShift action_30
action_70 (32) = happyShift action_31
action_70 (33) = happyShift action_32
action_70 (34) = happyShift action_33
action_70 (35) = happyShift action_34
action_70 (36) = happyShift action_35
action_70 (37) = happyShift action_36
action_70 (38) = happyShift action_37
action_70 (40) = happyShift action_38
action_70 (41) = happyShift action_39
action_70 _ = happyReduce_41

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

happyReduce_30 = happySpecReduce_3  9 happyReduction_30
happyReduction_30 _
	(HappyAbsSyn9  happy_var_2)
	_
	 =  HappyAbsSyn9
		 (happy_var_2
	)
happyReduction_30 _ _ _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_3  9 happyReduction_31
happyReduction_31 _
	(HappyAbsSyn9  happy_var_2)
	_
	 =  HappyAbsSyn9
		 (Lazy  happy_var_2
	)
happyReduction_31 _ _ _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_1  9 happyReduction_32
happyReduction_32 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_1
	)
happyReduction_32 _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_1  9 happyReduction_33
happyReduction_33 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn9
		 (happy_var_1
	)
happyReduction_33 _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_1  10 happyReduction_34
happyReduction_34 _
	 =  HappyAbsSyn10
		 (toBoolC True
	)

happyReduce_35 = happySpecReduce_1  10 happyReduction_35
happyReduction_35 _
	 =  HappyAbsSyn10
		 (toBoolC False
	)

happyReduce_36 = happySpecReduce_1  10 happyReduction_36
happyReduction_36 (HappyTerminal (TkNum happy_var_1))
	 =  HappyAbsSyn10
		 (toNumC  happy_var_1
	)
happyReduction_36 _  = notHappyAtAll 

happyReduce_37 = happyReduce 4 11 happyReduction_37
happyReduction_37 (_ `HappyStk`
	(HappyAbsSyn12  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TkId  happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn11
		 (FApp  happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_38 = happySpecReduce_0  12 happyReduction_38
happyReduction_38  =  HappyAbsSyn12
		 ([]
	)

happyReduce_39 = happySpecReduce_1  12 happyReduction_39
happyReduction_39 (HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn12
		 (reverse happy_var_1
	)
happyReduction_39 _  = notHappyAtAll 

happyReduce_40 = happySpecReduce_1  13 happyReduction_40
happyReduction_40 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn13
		 (happy_var_1 : []
	)
happyReduction_40 _  = notHappyAtAll 

happyReduce_41 = happySpecReduce_3  13 happyReduction_41
happyReduction_41 (HappyAbsSyn9  happy_var_3)
	_
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn13
		 (happy_var_3 : happy_var_1
	)
happyReduction_41 _ _ _  = notHappyAtAll 

happyReduce_42 = happyMonadReduce 2 13 happyReduction_42
happyReduction_42 (_ `HappyStk`
	_ `HappyStk`
	happyRest) tk
	 = happyThen ((( Left ("Parse error on argument list. ", 0)))
	) (\r -> happyReturn (HappyAbsSyn13 r))

happyReduce_43 = happySpecReduce_1  14 happyReduction_43
happyReduction_43 (HappyTerminal (TkNum happy_var_1))
	 =  HappyAbsSyn14
		 (toNumC  happy_var_1
	)
happyReduction_43 _  = notHappyAtAll 

happyReduce_44 = happySpecReduce_1  14 happyReduction_44
happyReduction_44 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn14
		 (happy_var_1
	)
happyReduction_44 _  = notHappyAtAll 

happyReduce_45 = happySpecReduce_1  14 happyReduction_45
happyReduction_45 (HappyTerminal (TkId  happy_var_1))
	 =  HappyAbsSyn14
		 (Var     happy_var_1
	)
happyReduction_45 _  = notHappyAtAll 

happyReduce_46 = happySpecReduce_3  14 happyReduction_46
happyReduction_46 _
	(HappyAbsSyn9  happy_var_2)
	_
	 =  HappyAbsSyn14
		 (happy_var_2
	)
happyReduction_46 _ _ _  = notHappyAtAll 

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
