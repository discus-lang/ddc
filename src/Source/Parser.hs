{-# OPTIONS -fno-warn-unused-binds #-}

module Source.Parser 
	(parse) 

where

-----
import Data.Char

-----
import Util

-----
import qualified Shared.Var 	as Var
import qualified Shared.VarUtil	as Var
import Shared.Base		(SourcePos(..))
import Shared.Var 		(NameSpace(..), Module(..))
import Shared.VarPrim
import Shared.Pretty

import Shared.Error
import Source.Error


import qualified Source.Token 	as K
import Source.Token
import Source.Exp
import Source.Util
import Type.Util		(pure, empty, takeKindOfType)

import Debug.Trace

stage 	= "Source.Parser"

type SP	= SourcePos

-- parser produced by Happy Version 1.17

data HappyAbsSyn 
	= HappyTerminal TokenP
	| HappyErrorToken Int
	| HappyAbsSyn4 ([Top SP])
	| HappyAbsSyn7 (Foreign SP)
	| HappyAbsSyn9 (Maybe String)
	| HappyAbsSyn10 (Module)
	| HappyAbsSyn11 ([Module])
	| HappyAbsSyn12 (InfixMode SP)
	| HappyAbsSyn13 ([Var])
	| HappyAbsSyn14 (Top SP)
	| HappyAbsSyn16 (([Var], Type))
	| HappyAbsSyn17 ([([Var], Type)])
	| HappyAbsSyn18 (Exp SP)
	| HappyAbsSyn19 ([Alt SP])
	| HappyAbsSyn20 (Maybe (Exp SP))
	| HappyAbsSyn24 ([Exp SP])
	| HappyAbsSyn31 (Bool)
	| HappyAbsSyn32 (Stmt SP)
	| HappyAbsSyn33 ([Stmt SP])
	| HappyAbsSyn40 (Alt SP)
	| HappyAbsSyn44 ([Guard SP])
	| HappyAbsSyn45 (Guard SP)
	| HappyAbsSyn48 (Pat SP)
	| HappyAbsSyn49 ([(Label SP, Pat SP)])
	| HappyAbsSyn50 ((Label SP, Pat SP))
	| HappyAbsSyn55 ([LCQual SP])
	| HappyAbsSyn56 (LCQual SP)
	| HappyAbsSyn58 ([(Var, [DataField (Exp SP) Type])])
	| HappyAbsSyn59 ((Var, [DataField (Exp SP) Type]))
	| HappyAbsSyn60 (DataField (Exp SP) Type)
	| HappyAbsSyn62 ([DataField (Exp SP) Type])
	| HappyAbsSyn65 (Kind)
	| HappyAbsSyn67 (Type)
	| HappyAbsSyn73 ([Type])
	| HappyAbsSyn75 ([(Var, Kind)])
	| HappyAbsSyn76 ((Var, Kind))
	| HappyAbsSyn77 ([Fetter])
	| HappyAbsSyn78 (Fetter)
	| HappyAbsSyn79 (Effect)
	| HappyAbsSyn80 ([Effect])
	| HappyAbsSyn83 (Closure)
	| HappyAbsSyn85 ([Closure])
	| HappyAbsSyn91 (Var)
	| HappyAbsSyn99 (())

type HappyReduction m = 
	   Int 
	-> (TokenP)
	-> HappyState (TokenP) (HappyStk HappyAbsSyn -> [(TokenP)] -> m HappyAbsSyn)
	-> [HappyState (TokenP) (HappyStk HappyAbsSyn -> [(TokenP)] -> m HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(TokenP)] -> m HappyAbsSyn

action_0,
 action_1,
 action_2,
 action_3,
 action_4,
 action_5,
 action_6,
 action_7,
 action_8,
 action_9,
 action_10,
 action_11,
 action_12,
 action_13,
 action_14,
 action_15,
 action_16,
 action_17,
 action_18,
 action_19,
 action_20,
 action_21,
 action_22,
 action_23,
 action_24,
 action_25,
 action_26,
 action_27,
 action_28,
 action_29,
 action_30,
 action_31,
 action_32,
 action_33,
 action_34,
 action_35,
 action_36,
 action_37,
 action_38,
 action_39,
 action_40,
 action_41,
 action_42,
 action_43,
 action_44,
 action_45,
 action_46,
 action_47,
 action_48,
 action_49,
 action_50,
 action_51,
 action_52,
 action_53,
 action_54,
 action_55,
 action_56,
 action_57,
 action_58,
 action_59,
 action_60,
 action_61,
 action_62,
 action_63,
 action_64,
 action_65,
 action_66,
 action_67,
 action_68,
 action_69,
 action_70,
 action_71,
 action_72,
 action_73,
 action_74,
 action_75,
 action_76,
 action_77,
 action_78,
 action_79,
 action_80,
 action_81,
 action_82,
 action_83,
 action_84,
 action_85,
 action_86,
 action_87,
 action_88,
 action_89,
 action_90,
 action_91,
 action_92,
 action_93,
 action_94,
 action_95,
 action_96,
 action_97,
 action_98,
 action_99,
 action_100,
 action_101,
 action_102,
 action_103,
 action_104,
 action_105,
 action_106,
 action_107,
 action_108,
 action_109,
 action_110,
 action_111,
 action_112,
 action_113,
 action_114,
 action_115,
 action_116,
 action_117,
 action_118,
 action_119,
 action_120,
 action_121,
 action_122,
 action_123,
 action_124,
 action_125,
 action_126,
 action_127,
 action_128,
 action_129,
 action_130,
 action_131,
 action_132,
 action_133,
 action_134,
 action_135,
 action_136,
 action_137,
 action_138,
 action_139,
 action_140,
 action_141,
 action_142,
 action_143,
 action_144,
 action_145,
 action_146,
 action_147,
 action_148,
 action_149,
 action_150,
 action_151,
 action_152,
 action_153,
 action_154,
 action_155,
 action_156,
 action_157,
 action_158,
 action_159,
 action_160,
 action_161,
 action_162,
 action_163,
 action_164,
 action_165,
 action_166,
 action_167,
 action_168,
 action_169,
 action_170,
 action_171,
 action_172,
 action_173,
 action_174,
 action_175,
 action_176,
 action_177,
 action_178,
 action_179,
 action_180,
 action_181,
 action_182,
 action_183,
 action_184,
 action_185,
 action_186,
 action_187,
 action_188,
 action_189,
 action_190,
 action_191,
 action_192,
 action_193,
 action_194,
 action_195,
 action_196,
 action_197,
 action_198,
 action_199,
 action_200,
 action_201,
 action_202,
 action_203,
 action_204,
 action_205,
 action_206,
 action_207,
 action_208,
 action_209,
 action_210,
 action_211,
 action_212,
 action_213,
 action_214,
 action_215,
 action_216,
 action_217,
 action_218,
 action_219,
 action_220,
 action_221,
 action_222,
 action_223,
 action_224,
 action_225,
 action_226,
 action_227,
 action_228,
 action_229,
 action_230,
 action_231,
 action_232,
 action_233,
 action_234,
 action_235,
 action_236,
 action_237,
 action_238,
 action_239,
 action_240,
 action_241,
 action_242,
 action_243,
 action_244,
 action_245,
 action_246,
 action_247,
 action_248,
 action_249,
 action_250,
 action_251,
 action_252,
 action_253,
 action_254,
 action_255,
 action_256,
 action_257,
 action_258,
 action_259,
 action_260,
 action_261,
 action_262,
 action_263,
 action_264,
 action_265,
 action_266,
 action_267,
 action_268,
 action_269,
 action_270,
 action_271,
 action_272,
 action_273,
 action_274,
 action_275,
 action_276,
 action_277,
 action_278,
 action_279,
 action_280,
 action_281,
 action_282,
 action_283,
 action_284,
 action_285,
 action_286,
 action_287,
 action_288,
 action_289,
 action_290,
 action_291,
 action_292,
 action_293,
 action_294,
 action_295,
 action_296,
 action_297,
 action_298,
 action_299,
 action_300,
 action_301,
 action_302,
 action_303,
 action_304,
 action_305,
 action_306,
 action_307,
 action_308,
 action_309,
 action_310,
 action_311,
 action_312,
 action_313,
 action_314,
 action_315,
 action_316,
 action_317,
 action_318,
 action_319,
 action_320,
 action_321,
 action_322,
 action_323,
 action_324,
 action_325,
 action_326,
 action_327,
 action_328,
 action_329,
 action_330,
 action_331,
 action_332,
 action_333,
 action_334,
 action_335,
 action_336,
 action_337,
 action_338,
 action_339,
 action_340,
 action_341,
 action_342,
 action_343,
 action_344,
 action_345,
 action_346,
 action_347,
 action_348,
 action_349,
 action_350,
 action_351,
 action_352,
 action_353,
 action_354,
 action_355,
 action_356,
 action_357,
 action_358,
 action_359,
 action_360,
 action_361,
 action_362,
 action_363,
 action_364,
 action_365,
 action_366,
 action_367,
 action_368,
 action_369,
 action_370,
 action_371,
 action_372,
 action_373,
 action_374,
 action_375,
 action_376,
 action_377,
 action_378,
 action_379,
 action_380,
 action_381,
 action_382,
 action_383,
 action_384,
 action_385,
 action_386,
 action_387,
 action_388,
 action_389,
 action_390,
 action_391,
 action_392,
 action_393,
 action_394,
 action_395,
 action_396,
 action_397,
 action_398,
 action_399,
 action_400,
 action_401,
 action_402,
 action_403,
 action_404,
 action_405,
 action_406,
 action_407,
 action_408,
 action_409,
 action_410,
 action_411,
 action_412,
 action_413,
 action_414,
 action_415,
 action_416,
 action_417,
 action_418,
 action_419,
 action_420,
 action_421,
 action_422,
 action_423,
 action_424,
 action_425,
 action_426,
 action_427,
 action_428,
 action_429,
 action_430,
 action_431,
 action_432,
 action_433,
 action_434,
 action_435,
 action_436,
 action_437,
 action_438,
 action_439,
 action_440,
 action_441,
 action_442,
 action_443,
 action_444,
 action_445,
 action_446,
 action_447,
 action_448,
 action_449,
 action_450,
 action_451,
 action_452,
 action_453,
 action_454,
 action_455,
 action_456,
 action_457,
 action_458,
 action_459,
 action_460,
 action_461,
 action_462,
 action_463,
 action_464,
 action_465,
 action_466,
 action_467,
 action_468,
 action_469,
 action_470,
 action_471,
 action_472,
 action_473,
 action_474,
 action_475,
 action_476,
 action_477,
 action_478,
 action_479,
 action_480,
 action_481,
 action_482,
 action_483,
 action_484,
 action_485,
 action_486,
 action_487,
 action_488,
 action_489,
 action_490,
 action_491,
 action_492,
 action_493,
 action_494,
 action_495,
 action_496,
 action_497,
 action_498,
 action_499,
 action_500,
 action_501,
 action_502,
 action_503,
 action_504,
 action_505,
 action_506,
 action_507,
 action_508,
 action_509,
 action_510,
 action_511,
 action_512,
 action_513,
 action_514,
 action_515,
 action_516,
 action_517,
 action_518,
 action_519,
 action_520,
 action_521,
 action_522,
 action_523,
 action_524,
 action_525,
 action_526,
 action_527,
 action_528,
 action_529,
 action_530,
 action_531,
 action_532,
 action_533,
 action_534,
 action_535,
 action_536,
 action_537,
 action_538,
 action_539,
 action_540,
 action_541,
 action_542,
 action_543,
 action_544,
 action_545,
 action_546,
 action_547,
 action_548,
 action_549,
 action_550,
 action_551,
 action_552,
 action_553,
 action_554,
 action_555,
 action_556,
 action_557,
 action_558,
 action_559,
 action_560,
 action_561,
 action_562,
 action_563,
 action_564,
 action_565,
 action_566,
 action_567,
 action_568,
 action_569,
 action_570,
 action_571,
 action_572,
 action_573,
 action_574 :: () => Int -> HappyReduction (HappyIdentity)

happyReduce_1,
 happyReduce_2,
 happyReduce_3,
 happyReduce_4,
 happyReduce_5,
 happyReduce_6,
 happyReduce_7,
 happyReduce_8,
 happyReduce_9,
 happyReduce_10,
 happyReduce_11,
 happyReduce_12,
 happyReduce_13,
 happyReduce_14,
 happyReduce_15,
 happyReduce_16,
 happyReduce_17,
 happyReduce_18,
 happyReduce_19,
 happyReduce_20,
 happyReduce_21,
 happyReduce_22,
 happyReduce_23,
 happyReduce_24,
 happyReduce_25,
 happyReduce_26,
 happyReduce_27,
 happyReduce_28,
 happyReduce_29,
 happyReduce_30,
 happyReduce_31,
 happyReduce_32,
 happyReduce_33,
 happyReduce_34,
 happyReduce_35,
 happyReduce_36,
 happyReduce_37,
 happyReduce_38,
 happyReduce_39,
 happyReduce_40,
 happyReduce_41,
 happyReduce_42,
 happyReduce_43,
 happyReduce_44,
 happyReduce_45,
 happyReduce_46,
 happyReduce_47,
 happyReduce_48,
 happyReduce_49,
 happyReduce_50,
 happyReduce_51,
 happyReduce_52,
 happyReduce_53,
 happyReduce_54,
 happyReduce_55,
 happyReduce_56,
 happyReduce_57,
 happyReduce_58,
 happyReduce_59,
 happyReduce_60,
 happyReduce_61,
 happyReduce_62,
 happyReduce_63,
 happyReduce_64,
 happyReduce_65,
 happyReduce_66,
 happyReduce_67,
 happyReduce_68,
 happyReduce_69,
 happyReduce_70,
 happyReduce_71,
 happyReduce_72,
 happyReduce_73,
 happyReduce_74,
 happyReduce_75,
 happyReduce_76,
 happyReduce_77,
 happyReduce_78,
 happyReduce_79,
 happyReduce_80,
 happyReduce_81,
 happyReduce_82,
 happyReduce_83,
 happyReduce_84,
 happyReduce_85,
 happyReduce_86,
 happyReduce_87,
 happyReduce_88,
 happyReduce_89,
 happyReduce_90,
 happyReduce_91,
 happyReduce_92,
 happyReduce_93,
 happyReduce_94,
 happyReduce_95,
 happyReduce_96,
 happyReduce_97,
 happyReduce_98,
 happyReduce_99,
 happyReduce_100,
 happyReduce_101,
 happyReduce_102,
 happyReduce_103,
 happyReduce_104,
 happyReduce_105,
 happyReduce_106,
 happyReduce_107,
 happyReduce_108,
 happyReduce_109,
 happyReduce_110,
 happyReduce_111,
 happyReduce_112,
 happyReduce_113,
 happyReduce_114,
 happyReduce_115,
 happyReduce_116,
 happyReduce_117,
 happyReduce_118,
 happyReduce_119,
 happyReduce_120,
 happyReduce_121,
 happyReduce_122,
 happyReduce_123,
 happyReduce_124,
 happyReduce_125,
 happyReduce_126,
 happyReduce_127,
 happyReduce_128,
 happyReduce_129,
 happyReduce_130,
 happyReduce_131,
 happyReduce_132,
 happyReduce_133,
 happyReduce_134,
 happyReduce_135,
 happyReduce_136,
 happyReduce_137,
 happyReduce_138,
 happyReduce_139,
 happyReduce_140,
 happyReduce_141,
 happyReduce_142,
 happyReduce_143,
 happyReduce_144,
 happyReduce_145,
 happyReduce_146,
 happyReduce_147,
 happyReduce_148,
 happyReduce_149,
 happyReduce_150,
 happyReduce_151,
 happyReduce_152,
 happyReduce_153,
 happyReduce_154,
 happyReduce_155,
 happyReduce_156,
 happyReduce_157,
 happyReduce_158,
 happyReduce_159,
 happyReduce_160,
 happyReduce_161,
 happyReduce_162,
 happyReduce_163,
 happyReduce_164,
 happyReduce_165,
 happyReduce_166,
 happyReduce_167,
 happyReduce_168,
 happyReduce_169,
 happyReduce_170,
 happyReduce_171,
 happyReduce_172,
 happyReduce_173,
 happyReduce_174,
 happyReduce_175,
 happyReduce_176,
 happyReduce_177,
 happyReduce_178,
 happyReduce_179,
 happyReduce_180,
 happyReduce_181,
 happyReduce_182,
 happyReduce_183,
 happyReduce_184,
 happyReduce_185,
 happyReduce_186,
 happyReduce_187,
 happyReduce_188,
 happyReduce_189,
 happyReduce_190,
 happyReduce_191,
 happyReduce_192,
 happyReduce_193,
 happyReduce_194,
 happyReduce_195,
 happyReduce_196,
 happyReduce_197,
 happyReduce_198,
 happyReduce_199,
 happyReduce_200,
 happyReduce_201,
 happyReduce_202,
 happyReduce_203,
 happyReduce_204,
 happyReduce_205,
 happyReduce_206,
 happyReduce_207,
 happyReduce_208,
 happyReduce_209,
 happyReduce_210,
 happyReduce_211,
 happyReduce_212,
 happyReduce_213,
 happyReduce_214,
 happyReduce_215,
 happyReduce_216,
 happyReduce_217,
 happyReduce_218,
 happyReduce_219,
 happyReduce_220,
 happyReduce_221,
 happyReduce_222,
 happyReduce_223,
 happyReduce_224,
 happyReduce_225,
 happyReduce_226,
 happyReduce_227,
 happyReduce_228,
 happyReduce_229,
 happyReduce_230,
 happyReduce_231,
 happyReduce_232,
 happyReduce_233,
 happyReduce_234,
 happyReduce_235,
 happyReduce_236,
 happyReduce_237,
 happyReduce_238,
 happyReduce_239,
 happyReduce_240,
 happyReduce_241,
 happyReduce_242,
 happyReduce_243,
 happyReduce_244,
 happyReduce_245,
 happyReduce_246,
 happyReduce_247,
 happyReduce_248,
 happyReduce_249,
 happyReduce_250,
 happyReduce_251,
 happyReduce_252,
 happyReduce_253,
 happyReduce_254,
 happyReduce_255,
 happyReduce_256,
 happyReduce_257,
 happyReduce_258,
 happyReduce_259,
 happyReduce_260,
 happyReduce_261,
 happyReduce_262,
 happyReduce_263,
 happyReduce_264,
 happyReduce_265,
 happyReduce_266,
 happyReduce_267,
 happyReduce_268,
 happyReduce_269,
 happyReduce_270,
 happyReduce_271,
 happyReduce_272,
 happyReduce_273,
 happyReduce_274,
 happyReduce_275,
 happyReduce_276,
 happyReduce_277,
 happyReduce_278,
 happyReduce_279,
 happyReduce_280,
 happyReduce_281,
 happyReduce_282 :: () => HappyReduction (HappyIdentity)

action_0 (104) = happyShift action_4
action_0 (145) = happyShift action_2
action_0 (4) = happyGoto action_3
action_0 _ = happyFail

action_1 (145) = happyShift action_2
action_1 _ = happyFail

action_2 (101) = happyShift action_28
action_2 (102) = happyShift action_29
action_2 (103) = happyShift action_30
action_2 (104) = happyShift action_31
action_2 (105) = happyShift action_32
action_2 (106) = happyShift action_33
action_2 (107) = happyShift action_34
action_2 (108) = happyShift action_35
action_2 (109) = happyShift action_36
action_2 (110) = happyShift action_37
action_2 (111) = happyShift action_38
action_2 (112) = happyShift action_39
action_2 (113) = happyShift action_40
action_2 (114) = happyShift action_41
action_2 (115) = happyShift action_42
action_2 (116) = happyShift action_43
action_2 (117) = happyShift action_44
action_2 (121) = happyShift action_45
action_2 (123) = happyShift action_46
action_2 (131) = happyShift action_47
action_2 (135) = happyShift action_48
action_2 (142) = happyShift action_49
action_2 (147) = happyShift action_50
action_2 (149) = happyShift action_51
action_2 (151) = happyShift action_52
action_2 (152) = happyShift action_53
action_2 (153) = happyShift action_54
action_2 (154) = happyShift action_55
action_2 (157) = happyShift action_56
action_2 (163) = happyShift action_57
action_2 (164) = happyShift action_58
action_2 (165) = happyShift action_59
action_2 (166) = happyShift action_60
action_2 (167) = happyShift action_61
action_2 (168) = happyShift action_62
action_2 (169) = happyShift action_63
action_2 (170) = happyShift action_64
action_2 (171) = happyShift action_65
action_2 (172) = happyShift action_66
action_2 (176) = happyShift action_67
action_2 (177) = happyShift action_68
action_2 (178) = happyShift action_8
action_2 (179) = happyShift action_69
action_2 (180) = happyShift action_70
action_2 (181) = happyShift action_71
action_2 (182) = happyShift action_72
action_2 (183) = happyShift action_73
action_2 (184) = happyShift action_74
action_2 (5) = happyGoto action_10
action_2 (6) = happyGoto action_11
action_2 (12) = happyGoto action_12
action_2 (24) = happyGoto action_13
action_2 (27) = happyGoto action_14
action_2 (28) = happyGoto action_15
action_2 (29) = happyGoto action_16
action_2 (30) = happyGoto action_17
action_2 (32) = happyGoto action_18
action_2 (35) = happyGoto action_19
action_2 (51) = happyGoto action_20
action_2 (53) = happyGoto action_21
action_2 (57) = happyGoto action_22
action_2 (91) = happyGoto action_23
action_2 (92) = happyGoto action_24
action_2 (95) = happyGoto action_25
action_2 (96) = happyGoto action_7
action_2 (97) = happyGoto action_26
action_2 (98) = happyGoto action_27
action_2 _ = happyFail

action_3 (185) = happyAccept
action_3 _ = happyFail

action_4 (178) = happyShift action_8
action_4 (180) = happyShift action_9
action_4 (10) = happyGoto action_5
action_4 (95) = happyGoto action_6
action_4 (96) = happyGoto action_7
action_4 _ = happyFail

action_5 (120) = happyShift action_155
action_5 _ = happyFail

action_6 _ = happyReduce_26

action_7 _ = happyReduce_259

action_8 _ = happyReduce_261

action_9 (160) = happyShift action_154
action_9 _ = happyFail

action_10 (146) = happyShift action_153
action_10 _ = happyFail

action_11 (158) = happyShift action_152
action_11 (99) = happyGoto action_150
action_11 (100) = happyGoto action_151
action_11 _ = happyReduce_281

action_12 (181) = happyShift action_149
action_12 _ = happyFail

action_13 (143) = happyShift action_145
action_13 (144) = happyShift action_146
action_13 (155) = happyShift action_147
action_13 (159) = happyShift action_148
action_13 (41) = happyGoto action_141
action_13 (43) = happyGoto action_142
action_13 (44) = happyGoto action_143
action_13 (45) = happyGoto action_144
action_13 _ = happyFail

action_14 (105) = happyShift action_32
action_14 (106) = happyShift action_33
action_14 (107) = happyShift action_34
action_14 (108) = happyShift action_35
action_14 (121) = happyShift action_45
action_14 (123) = happyShift action_46
action_14 (131) = happyShift action_47
action_14 (135) = happyShift action_48
action_14 (142) = happyShift action_49
action_14 (147) = happyShift action_50
action_14 (149) = happyShift action_51
action_14 (151) = happyShift action_52
action_14 (152) = happyShift action_53
action_14 (153) = happyShift action_54
action_14 (154) = happyShift action_55
action_14 (157) = happyShift action_56
action_14 (163) = happyShift action_57
action_14 (164) = happyShift action_58
action_14 (165) = happyShift action_59
action_14 (166) = happyShift action_60
action_14 (167) = happyShift action_61
action_14 (168) = happyShift action_62
action_14 (169) = happyShift action_63
action_14 (170) = happyShift action_64
action_14 (171) = happyShift action_65
action_14 (172) = happyShift action_66
action_14 (176) = happyShift action_67
action_14 (177) = happyShift action_68
action_14 (178) = happyShift action_8
action_14 (179) = happyShift action_69
action_14 (180) = happyShift action_70
action_14 (181) = happyShift action_71
action_14 (182) = happyShift action_72
action_14 (183) = happyShift action_73
action_14 (184) = happyShift action_74
action_14 (25) = happyGoto action_140
action_14 (27) = happyGoto action_134
action_14 (28) = happyGoto action_15
action_14 (29) = happyGoto action_16
action_14 (30) = happyGoto action_17
action_14 (51) = happyGoto action_20
action_14 (53) = happyGoto action_21
action_14 (91) = happyGoto action_23
action_14 (92) = happyGoto action_93
action_14 (95) = happyGoto action_25
action_14 (96) = happyGoto action_7
action_14 (97) = happyGoto action_26
action_14 (98) = happyGoto action_135
action_14 _ = happyReduce_58

action_15 _ = happyReduce_67

action_16 (160) = happyShift action_138
action_16 (162) = happyShift action_139
action_16 _ = happyReduce_73

action_17 _ = happyReduce_70

action_18 _ = happyReduce_103

action_19 _ = happyReduce_20

action_20 _ = happyReduce_71

action_21 _ = happyReduce_72

action_22 _ = happyReduce_13

action_23 _ = happyReduce_90

action_24 (137) = happyShift action_136
action_24 (161) = happyShift action_137
action_24 _ = happyReduce_251

action_25 _ = happyReduce_78

action_26 _ = happyReduce_253

action_27 (105) = happyShift action_32
action_27 (106) = happyShift action_33
action_27 (107) = happyShift action_34
action_27 (108) = happyShift action_35
action_27 (121) = happyShift action_45
action_27 (123) = happyShift action_46
action_27 (131) = happyShift action_47
action_27 (135) = happyShift action_48
action_27 (142) = happyShift action_49
action_27 (147) = happyShift action_50
action_27 (149) = happyShift action_51
action_27 (151) = happyShift action_52
action_27 (152) = happyShift action_53
action_27 (153) = happyShift action_54
action_27 (154) = happyShift action_55
action_27 (157) = happyShift action_56
action_27 (163) = happyShift action_57
action_27 (164) = happyShift action_58
action_27 (165) = happyShift action_59
action_27 (166) = happyShift action_60
action_27 (167) = happyShift action_61
action_27 (168) = happyShift action_62
action_27 (169) = happyShift action_63
action_27 (170) = happyShift action_64
action_27 (171) = happyShift action_65
action_27 (172) = happyShift action_66
action_27 (176) = happyShift action_67
action_27 (177) = happyShift action_68
action_27 (178) = happyShift action_8
action_27 (179) = happyShift action_69
action_27 (180) = happyShift action_70
action_27 (181) = happyShift action_71
action_27 (182) = happyShift action_72
action_27 (183) = happyShift action_73
action_27 (184) = happyShift action_74
action_27 (25) = happyGoto action_133
action_27 (27) = happyGoto action_134
action_27 (28) = happyGoto action_15
action_27 (29) = happyGoto action_16
action_27 (30) = happyGoto action_17
action_27 (51) = happyGoto action_20
action_27 (53) = happyGoto action_21
action_27 (91) = happyGoto action_23
action_27 (92) = happyGoto action_93
action_27 (95) = happyGoto action_25
action_27 (96) = happyGoto action_7
action_27 (97) = happyGoto action_26
action_27 (98) = happyGoto action_135
action_27 _ = happyFail

action_28 (105) = happyShift action_32
action_28 (106) = happyShift action_33
action_28 (107) = happyShift action_34
action_28 (108) = happyShift action_35
action_28 (121) = happyShift action_45
action_28 (123) = happyShift action_46
action_28 (131) = happyShift action_47
action_28 (135) = happyShift action_48
action_28 (142) = happyShift action_49
action_28 (147) = happyShift action_50
action_28 (149) = happyShift action_51
action_28 (151) = happyShift action_52
action_28 (152) = happyShift action_53
action_28 (153) = happyShift action_54
action_28 (154) = happyShift action_55
action_28 (157) = happyShift action_56
action_28 (163) = happyShift action_57
action_28 (164) = happyShift action_58
action_28 (165) = happyShift action_59
action_28 (166) = happyShift action_60
action_28 (167) = happyShift action_61
action_28 (168) = happyShift action_62
action_28 (169) = happyShift action_63
action_28 (170) = happyShift action_64
action_28 (171) = happyShift action_65
action_28 (172) = happyShift action_66
action_28 (176) = happyShift action_67
action_28 (177) = happyShift action_68
action_28 (178) = happyShift action_8
action_28 (179) = happyShift action_69
action_28 (180) = happyShift action_70
action_28 (181) = happyShift action_71
action_28 (182) = happyShift action_72
action_28 (183) = happyShift action_73
action_28 (184) = happyShift action_74
action_28 (24) = happyGoto action_132
action_28 (27) = happyGoto action_14
action_28 (28) = happyGoto action_15
action_28 (29) = happyGoto action_16
action_28 (30) = happyGoto action_17
action_28 (51) = happyGoto action_20
action_28 (53) = happyGoto action_21
action_28 (91) = happyGoto action_23
action_28 (92) = happyGoto action_93
action_28 (95) = happyGoto action_25
action_28 (96) = happyGoto action_7
action_28 (97) = happyGoto action_26
action_28 (98) = happyGoto action_27
action_28 _ = happyFail

action_29 (103) = happyShift action_131
action_29 (7) = happyGoto action_130
action_29 _ = happyFail

action_30 (108) = happyShift action_128
action_30 (145) = happyShift action_129
action_30 (178) = happyShift action_8
action_30 (180) = happyShift action_9
action_30 (10) = happyGoto action_127
action_30 (95) = happyGoto action_6
action_30 (96) = happyGoto action_7
action_30 _ = happyFail

action_31 (178) = happyShift action_8
action_31 (180) = happyShift action_9
action_31 (10) = happyGoto action_126
action_31 (95) = happyGoto action_6
action_31 (96) = happyGoto action_7
action_31 _ = happyFail

action_32 _ = happyReduce_263

action_33 _ = happyReduce_264

action_34 _ = happyReduce_265

action_35 _ = happyReduce_266

action_36 (178) = happyShift action_125
action_36 (96) = happyGoto action_124
action_36 _ = happyFail

action_37 (105) = happyShift action_32
action_37 (106) = happyShift action_33
action_37 (107) = happyShift action_34
action_37 (108) = happyShift action_35
action_37 (147) = happyShift action_82
action_37 (176) = happyShift action_67
action_37 (92) = happyGoto action_123
action_37 (97) = happyGoto action_26
action_37 _ = happyFail

action_38 (178) = happyShift action_8
action_38 (96) = happyGoto action_122
action_38 _ = happyFail

action_39 (178) = happyShift action_8
action_39 (96) = happyGoto action_121
action_39 _ = happyFail

action_40 (178) = happyShift action_8
action_40 (96) = happyGoto action_120
action_40 _ = happyFail

action_41 (105) = happyShift action_32
action_41 (106) = happyShift action_33
action_41 (107) = happyShift action_34
action_41 (108) = happyShift action_35
action_41 (142) = happyShift action_113
action_41 (147) = happyShift action_114
action_41 (149) = happyShift action_115
action_41 (163) = happyShift action_116
action_41 (165) = happyShift action_117
action_41 (166) = happyShift action_118
action_41 (168) = happyShift action_119
action_41 (176) = happyShift action_67
action_41 (178) = happyShift action_8
action_41 (180) = happyShift action_9
action_41 (66) = happyGoto action_108
action_41 (71) = happyGoto action_109
action_41 (72) = happyGoto action_110
action_41 (95) = happyGoto action_111
action_41 (96) = happyGoto action_7
action_41 (97) = happyGoto action_112
action_41 _ = happyFail

action_42 _ = happyReduce_31

action_43 _ = happyReduce_30

action_44 _ = happyReduce_32

action_45 (105) = happyShift action_32
action_45 (106) = happyShift action_33
action_45 (107) = happyShift action_34
action_45 (108) = happyShift action_35
action_45 (118) = happyShift action_94
action_45 (121) = happyShift action_45
action_45 (123) = happyShift action_46
action_45 (124) = happyShift action_95
action_45 (127) = happyShift action_96
action_45 (128) = happyShift action_97
action_45 (131) = happyShift action_47
action_45 (132) = happyShift action_98
action_45 (133) = happyShift action_99
action_45 (134) = happyShift action_100
action_45 (135) = happyShift action_48
action_45 (142) = happyShift action_49
action_45 (147) = happyShift action_50
action_45 (149) = happyShift action_51
action_45 (151) = happyShift action_52
action_45 (152) = happyShift action_53
action_45 (153) = happyShift action_102
action_45 (154) = happyShift action_55
action_45 (157) = happyShift action_56
action_45 (163) = happyShift action_57
action_45 (164) = happyShift action_58
action_45 (165) = happyShift action_59
action_45 (166) = happyShift action_60
action_45 (167) = happyShift action_61
action_45 (168) = happyShift action_62
action_45 (169) = happyShift action_63
action_45 (170) = happyShift action_64
action_45 (171) = happyShift action_65
action_45 (172) = happyShift action_66
action_45 (176) = happyShift action_67
action_45 (177) = happyShift action_68
action_45 (178) = happyShift action_8
action_45 (179) = happyShift action_69
action_45 (180) = happyShift action_70
action_45 (181) = happyShift action_71
action_45 (182) = happyShift action_72
action_45 (183) = happyShift action_73
action_45 (184) = happyShift action_74
action_45 (18) = happyGoto action_107
action_45 (21) = happyGoto action_88
action_45 (22) = happyGoto action_89
action_45 (23) = happyGoto action_90
action_45 (24) = happyGoto action_91
action_45 (27) = happyGoto action_14
action_45 (28) = happyGoto action_15
action_45 (29) = happyGoto action_16
action_45 (30) = happyGoto action_17
action_45 (51) = happyGoto action_20
action_45 (53) = happyGoto action_21
action_45 (91) = happyGoto action_23
action_45 (92) = happyGoto action_93
action_45 (95) = happyGoto action_25
action_45 (96) = happyGoto action_7
action_45 (97) = happyGoto action_26
action_45 (98) = happyGoto action_27
action_45 _ = happyFail

action_46 (145) = happyShift action_106
action_46 _ = happyFail

action_47 (145) = happyShift action_105
action_47 _ = happyFail

action_48 _ = happyReduce_88

action_49 _ = happyReduce_89

action_50 (105) = happyShift action_32
action_50 (106) = happyShift action_33
action_50 (107) = happyShift action_34
action_50 (108) = happyShift action_35
action_50 (118) = happyShift action_94
action_50 (121) = happyShift action_45
action_50 (123) = happyShift action_46
action_50 (124) = happyShift action_95
action_50 (127) = happyShift action_96
action_50 (128) = happyShift action_97
action_50 (131) = happyShift action_47
action_50 (132) = happyShift action_98
action_50 (133) = happyShift action_99
action_50 (134) = happyShift action_100
action_50 (135) = happyShift action_48
action_50 (142) = happyShift action_49
action_50 (147) = happyShift action_50
action_50 (149) = happyShift action_51
action_50 (151) = happyShift action_52
action_50 (152) = happyShift action_53
action_50 (153) = happyShift action_102
action_50 (154) = happyShift action_55
action_50 (157) = happyShift action_56
action_50 (163) = happyShift action_57
action_50 (164) = happyShift action_58
action_50 (165) = happyShift action_59
action_50 (166) = happyShift action_60
action_50 (167) = happyShift action_61
action_50 (168) = happyShift action_62
action_50 (169) = happyShift action_63
action_50 (170) = happyShift action_64
action_50 (171) = happyShift action_65
action_50 (172) = happyShift action_66
action_50 (176) = happyShift action_67
action_50 (177) = happyShift action_68
action_50 (178) = happyShift action_8
action_50 (179) = happyShift action_69
action_50 (180) = happyShift action_70
action_50 (181) = happyShift action_71
action_50 (182) = happyShift action_72
action_50 (183) = happyShift action_73
action_50 (184) = happyShift action_74
action_50 (18) = happyGoto action_103
action_50 (21) = happyGoto action_88
action_50 (22) = happyGoto action_89
action_50 (23) = happyGoto action_90
action_50 (24) = happyGoto action_91
action_50 (27) = happyGoto action_14
action_50 (28) = happyGoto action_15
action_50 (29) = happyGoto action_16
action_50 (30) = happyGoto action_17
action_50 (51) = happyGoto action_20
action_50 (53) = happyGoto action_21
action_50 (91) = happyGoto action_23
action_50 (92) = happyGoto action_93
action_50 (95) = happyGoto action_25
action_50 (96) = happyGoto action_7
action_50 (97) = happyGoto action_26
action_50 (98) = happyGoto action_104
action_50 _ = happyFail

action_51 (105) = happyShift action_32
action_51 (106) = happyShift action_33
action_51 (107) = happyShift action_34
action_51 (108) = happyShift action_35
action_51 (118) = happyShift action_94
action_51 (121) = happyShift action_45
action_51 (123) = happyShift action_46
action_51 (124) = happyShift action_95
action_51 (127) = happyShift action_96
action_51 (128) = happyShift action_97
action_51 (131) = happyShift action_47
action_51 (132) = happyShift action_98
action_51 (133) = happyShift action_99
action_51 (134) = happyShift action_100
action_51 (135) = happyShift action_48
action_51 (142) = happyShift action_49
action_51 (147) = happyShift action_50
action_51 (149) = happyShift action_51
action_51 (150) = happyShift action_101
action_51 (151) = happyShift action_52
action_51 (152) = happyShift action_53
action_51 (153) = happyShift action_102
action_51 (154) = happyShift action_55
action_51 (157) = happyShift action_56
action_51 (163) = happyShift action_57
action_51 (164) = happyShift action_58
action_51 (165) = happyShift action_59
action_51 (166) = happyShift action_60
action_51 (167) = happyShift action_61
action_51 (168) = happyShift action_62
action_51 (169) = happyShift action_63
action_51 (170) = happyShift action_64
action_51 (171) = happyShift action_65
action_51 (172) = happyShift action_66
action_51 (176) = happyShift action_67
action_51 (177) = happyShift action_68
action_51 (178) = happyShift action_8
action_51 (179) = happyShift action_69
action_51 (180) = happyShift action_70
action_51 (181) = happyShift action_71
action_51 (182) = happyShift action_72
action_51 (183) = happyShift action_73
action_51 (184) = happyShift action_74
action_51 (18) = happyGoto action_87
action_51 (21) = happyGoto action_88
action_51 (22) = happyGoto action_89
action_51 (23) = happyGoto action_90
action_51 (24) = happyGoto action_91
action_51 (27) = happyGoto action_14
action_51 (28) = happyGoto action_15
action_51 (29) = happyGoto action_16
action_51 (30) = happyGoto action_17
action_51 (51) = happyGoto action_20
action_51 (53) = happyGoto action_21
action_51 (54) = happyGoto action_92
action_51 (91) = happyGoto action_23
action_51 (92) = happyGoto action_93
action_51 (95) = happyGoto action_25
action_51 (96) = happyGoto action_7
action_51 (97) = happyGoto action_26
action_51 (98) = happyGoto action_27
action_51 _ = happyFail

action_52 _ = happyReduce_273

action_53 _ = happyReduce_274

action_54 (121) = happyShift action_86
action_54 _ = happyFail

action_55 (105) = happyShift action_32
action_55 (106) = happyShift action_33
action_55 (107) = happyShift action_34
action_55 (108) = happyShift action_35
action_55 (147) = happyShift action_82
action_55 (176) = happyShift action_67
action_55 (178) = happyShift action_8
action_55 (180) = happyShift action_70
action_55 (91) = happyGoto action_83
action_55 (92) = happyGoto action_84
action_55 (95) = happyGoto action_85
action_55 (96) = happyGoto action_7
action_55 (97) = happyGoto action_26
action_55 _ = happyFail

action_56 _ = happyReduce_268

action_57 _ = happyReduce_269

action_58 _ = happyReduce_270

action_59 _ = happyReduce_276

action_60 _ = happyReduce_278

action_61 _ = happyReduce_272

action_62 _ = happyReduce_271

action_63 _ = happyReduce_275

action_64 _ = happyReduce_277

action_65 _ = happyReduce_87

action_66 (105) = happyShift action_32
action_66 (106) = happyShift action_33
action_66 (107) = happyShift action_34
action_66 (108) = happyShift action_35
action_66 (147) = happyShift action_82
action_66 (176) = happyShift action_67
action_66 (92) = happyGoto action_81
action_66 (97) = happyGoto action_26
action_66 _ = happyFail

action_67 _ = happyReduce_262

action_68 _ = happyReduce_86

action_69 _ = happyReduce_267

action_70 (160) = happyShift action_80
action_70 _ = happyFail

action_71 (162) = happyShift action_76
action_71 (31) = happyGoto action_79
action_71 _ = happyReduce_95

action_72 (162) = happyShift action_76
action_72 (31) = happyGoto action_78
action_72 _ = happyReduce_95

action_73 (162) = happyShift action_76
action_73 (31) = happyGoto action_77
action_73 _ = happyReduce_95

action_74 (162) = happyShift action_76
action_74 (31) = happyGoto action_75
action_74 _ = happyReduce_95

action_75 _ = happyReduce_94

action_76 _ = happyReduce_96

action_77 _ = happyReduce_93

action_78 _ = happyReduce_92

action_79 _ = happyReduce_91

action_80 (105) = happyShift action_32
action_80 (106) = happyShift action_33
action_80 (107) = happyShift action_34
action_80 (108) = happyShift action_35
action_80 (147) = happyShift action_82
action_80 (176) = happyShift action_67
action_80 (178) = happyShift action_8
action_80 (92) = happyGoto action_243
action_80 (96) = happyGoto action_157
action_80 (97) = happyGoto action_26
action_80 _ = happyFail

action_81 _ = happyReduce_85

action_82 (151) = happyShift action_52
action_82 (152) = happyShift action_53
action_82 (157) = happyShift action_56
action_82 (163) = happyShift action_57
action_82 (164) = happyShift action_58
action_82 (165) = happyShift action_59
action_82 (166) = happyShift action_60
action_82 (167) = happyShift action_61
action_82 (168) = happyShift action_62
action_82 (169) = happyShift action_63
action_82 (170) = happyShift action_64
action_82 (179) = happyShift action_69
action_82 (98) = happyGoto action_242
action_82 _ = happyFail

action_83 (154) = happyShift action_241
action_83 _ = happyFail

action_84 _ = happyReduce_251

action_85 (154) = happyShift action_240
action_85 _ = happyFail

action_86 (145) = happyShift action_239
action_86 _ = happyFail

action_87 (156) = happyShift action_236
action_87 (159) = happyShift action_237
action_87 (173) = happyShift action_238
action_87 _ = happyReduce_144

action_88 _ = happyReduce_42

action_89 _ = happyReduce_47

action_90 _ = happyReduce_54

action_91 _ = happyReduce_57

action_92 (150) = happyShift action_235
action_92 _ = happyFail

action_93 (161) = happyShift action_137
action_93 _ = happyReduce_251

action_94 (105) = happyShift action_32
action_94 (106) = happyShift action_33
action_94 (107) = happyShift action_34
action_94 (108) = happyShift action_35
action_94 (121) = happyShift action_45
action_94 (123) = happyShift action_46
action_94 (131) = happyShift action_47
action_94 (135) = happyShift action_48
action_94 (142) = happyShift action_49
action_94 (147) = happyShift action_50
action_94 (149) = happyShift action_51
action_94 (151) = happyShift action_52
action_94 (152) = happyShift action_53
action_94 (153) = happyShift action_54
action_94 (154) = happyShift action_55
action_94 (157) = happyShift action_56
action_94 (163) = happyShift action_57
action_94 (164) = happyShift action_58
action_94 (165) = happyShift action_59
action_94 (166) = happyShift action_60
action_94 (167) = happyShift action_61
action_94 (168) = happyShift action_62
action_94 (169) = happyShift action_63
action_94 (170) = happyShift action_64
action_94 (171) = happyShift action_65
action_94 (172) = happyShift action_66
action_94 (176) = happyShift action_67
action_94 (177) = happyShift action_68
action_94 (178) = happyShift action_8
action_94 (179) = happyShift action_69
action_94 (180) = happyShift action_70
action_94 (181) = happyShift action_71
action_94 (182) = happyShift action_72
action_94 (183) = happyShift action_73
action_94 (184) = happyShift action_74
action_94 (24) = happyGoto action_13
action_94 (27) = happyGoto action_14
action_94 (28) = happyGoto action_15
action_94 (29) = happyGoto action_16
action_94 (30) = happyGoto action_17
action_94 (32) = happyGoto action_18
action_94 (35) = happyGoto action_233
action_94 (36) = happyGoto action_234
action_94 (51) = happyGoto action_20
action_94 (53) = happyGoto action_21
action_94 (91) = happyGoto action_23
action_94 (92) = happyGoto action_24
action_94 (95) = happyGoto action_25
action_94 (96) = happyGoto action_7
action_94 (97) = happyGoto action_26
action_94 (98) = happyGoto action_27
action_94 _ = happyFail

action_95 (105) = happyShift action_32
action_95 (106) = happyShift action_33
action_95 (107) = happyShift action_34
action_95 (108) = happyShift action_35
action_95 (118) = happyShift action_94
action_95 (121) = happyShift action_45
action_95 (123) = happyShift action_46
action_95 (124) = happyShift action_95
action_95 (127) = happyShift action_96
action_95 (128) = happyShift action_97
action_95 (131) = happyShift action_47
action_95 (132) = happyShift action_98
action_95 (133) = happyShift action_99
action_95 (134) = happyShift action_100
action_95 (135) = happyShift action_48
action_95 (142) = happyShift action_49
action_95 (147) = happyShift action_50
action_95 (149) = happyShift action_51
action_95 (151) = happyShift action_52
action_95 (152) = happyShift action_53
action_95 (153) = happyShift action_102
action_95 (154) = happyShift action_55
action_95 (157) = happyShift action_56
action_95 (163) = happyShift action_57
action_95 (164) = happyShift action_58
action_95 (165) = happyShift action_59
action_95 (166) = happyShift action_60
action_95 (167) = happyShift action_61
action_95 (168) = happyShift action_62
action_95 (169) = happyShift action_63
action_95 (170) = happyShift action_64
action_95 (171) = happyShift action_65
action_95 (172) = happyShift action_66
action_95 (176) = happyShift action_67
action_95 (177) = happyShift action_68
action_95 (178) = happyShift action_8
action_95 (179) = happyShift action_69
action_95 (180) = happyShift action_70
action_95 (181) = happyShift action_71
action_95 (182) = happyShift action_72
action_95 (183) = happyShift action_73
action_95 (184) = happyShift action_74
action_95 (18) = happyGoto action_232
action_95 (21) = happyGoto action_88
action_95 (22) = happyGoto action_89
action_95 (23) = happyGoto action_90
action_95 (24) = happyGoto action_91
action_95 (27) = happyGoto action_14
action_95 (28) = happyGoto action_15
action_95 (29) = happyGoto action_16
action_95 (30) = happyGoto action_17
action_95 (51) = happyGoto action_20
action_95 (53) = happyGoto action_21
action_95 (91) = happyGoto action_23
action_95 (92) = happyGoto action_93
action_95 (95) = happyGoto action_25
action_95 (96) = happyGoto action_7
action_95 (97) = happyGoto action_26
action_95 (98) = happyGoto action_27
action_95 _ = happyFail

action_96 (105) = happyShift action_32
action_96 (106) = happyShift action_33
action_96 (107) = happyShift action_34
action_96 (108) = happyShift action_35
action_96 (121) = happyShift action_45
action_96 (123) = happyShift action_46
action_96 (131) = happyShift action_47
action_96 (135) = happyShift action_48
action_96 (142) = happyShift action_49
action_96 (147) = happyShift action_50
action_96 (149) = happyShift action_51
action_96 (151) = happyShift action_52
action_96 (152) = happyShift action_53
action_96 (153) = happyShift action_54
action_96 (154) = happyShift action_55
action_96 (157) = happyShift action_56
action_96 (163) = happyShift action_57
action_96 (164) = happyShift action_58
action_96 (165) = happyShift action_59
action_96 (166) = happyShift action_60
action_96 (167) = happyShift action_61
action_96 (168) = happyShift action_62
action_96 (169) = happyShift action_63
action_96 (170) = happyShift action_64
action_96 (171) = happyShift action_65
action_96 (172) = happyShift action_66
action_96 (176) = happyShift action_67
action_96 (177) = happyShift action_68
action_96 (178) = happyShift action_8
action_96 (179) = happyShift action_69
action_96 (180) = happyShift action_70
action_96 (181) = happyShift action_71
action_96 (182) = happyShift action_72
action_96 (183) = happyShift action_73
action_96 (184) = happyShift action_74
action_96 (23) = happyGoto action_231
action_96 (24) = happyGoto action_91
action_96 (27) = happyGoto action_14
action_96 (28) = happyGoto action_15
action_96 (29) = happyGoto action_16
action_96 (30) = happyGoto action_17
action_96 (51) = happyGoto action_20
action_96 (53) = happyGoto action_21
action_96 (91) = happyGoto action_23
action_96 (92) = happyGoto action_93
action_96 (95) = happyGoto action_25
action_96 (96) = happyGoto action_7
action_96 (97) = happyGoto action_26
action_96 (98) = happyGoto action_27
action_96 _ = happyFail

action_97 (105) = happyShift action_32
action_97 (106) = happyShift action_33
action_97 (107) = happyShift action_34
action_97 (108) = happyShift action_35
action_97 (118) = happyShift action_94
action_97 (121) = happyShift action_45
action_97 (123) = happyShift action_46
action_97 (124) = happyShift action_95
action_97 (127) = happyShift action_96
action_97 (131) = happyShift action_47
action_97 (132) = happyShift action_98
action_97 (133) = happyShift action_99
action_97 (134) = happyShift action_100
action_97 (135) = happyShift action_48
action_97 (142) = happyShift action_49
action_97 (147) = happyShift action_50
action_97 (149) = happyShift action_51
action_97 (151) = happyShift action_52
action_97 (152) = happyShift action_53
action_97 (153) = happyShift action_102
action_97 (154) = happyShift action_55
action_97 (157) = happyShift action_56
action_97 (163) = happyShift action_57
action_97 (164) = happyShift action_58
action_97 (165) = happyShift action_59
action_97 (166) = happyShift action_60
action_97 (167) = happyShift action_61
action_97 (168) = happyShift action_62
action_97 (169) = happyShift action_63
action_97 (170) = happyShift action_64
action_97 (171) = happyShift action_65
action_97 (172) = happyShift action_66
action_97 (176) = happyShift action_67
action_97 (177) = happyShift action_68
action_97 (178) = happyShift action_8
action_97 (179) = happyShift action_69
action_97 (180) = happyShift action_70
action_97 (181) = happyShift action_71
action_97 (182) = happyShift action_72
action_97 (183) = happyShift action_73
action_97 (184) = happyShift action_74
action_97 (21) = happyGoto action_230
action_97 (22) = happyGoto action_89
action_97 (23) = happyGoto action_90
action_97 (24) = happyGoto action_91
action_97 (27) = happyGoto action_14
action_97 (28) = happyGoto action_15
action_97 (29) = happyGoto action_16
action_97 (30) = happyGoto action_17
action_97 (51) = happyGoto action_20
action_97 (53) = happyGoto action_21
action_97 (91) = happyGoto action_23
action_97 (92) = happyGoto action_93
action_97 (95) = happyGoto action_25
action_97 (96) = happyGoto action_7
action_97 (97) = happyGoto action_26
action_97 (98) = happyGoto action_27
action_97 _ = happyFail

action_98 (105) = happyShift action_32
action_98 (106) = happyShift action_33
action_98 (107) = happyShift action_34
action_98 (108) = happyShift action_35
action_98 (121) = happyShift action_45
action_98 (123) = happyShift action_46
action_98 (131) = happyShift action_47
action_98 (135) = happyShift action_48
action_98 (142) = happyShift action_49
action_98 (147) = happyShift action_50
action_98 (149) = happyShift action_51
action_98 (153) = happyShift action_54
action_98 (154) = happyShift action_55
action_98 (171) = happyShift action_65
action_98 (172) = happyShift action_66
action_98 (176) = happyShift action_67
action_98 (177) = happyShift action_68
action_98 (178) = happyShift action_8
action_98 (180) = happyShift action_70
action_98 (181) = happyShift action_71
action_98 (182) = happyShift action_72
action_98 (183) = happyShift action_73
action_98 (184) = happyShift action_74
action_98 (27) = happyGoto action_229
action_98 (28) = happyGoto action_15
action_98 (29) = happyGoto action_16
action_98 (30) = happyGoto action_17
action_98 (51) = happyGoto action_20
action_98 (53) = happyGoto action_21
action_98 (91) = happyGoto action_23
action_98 (92) = happyGoto action_93
action_98 (95) = happyGoto action_25
action_98 (96) = happyGoto action_7
action_98 (97) = happyGoto action_26
action_98 _ = happyFail

action_99 (105) = happyShift action_32
action_99 (106) = happyShift action_33
action_99 (107) = happyShift action_34
action_99 (108) = happyShift action_35
action_99 (121) = happyShift action_45
action_99 (123) = happyShift action_46
action_99 (131) = happyShift action_47
action_99 (135) = happyShift action_48
action_99 (142) = happyShift action_49
action_99 (147) = happyShift action_50
action_99 (149) = happyShift action_51
action_99 (153) = happyShift action_54
action_99 (154) = happyShift action_55
action_99 (171) = happyShift action_65
action_99 (172) = happyShift action_66
action_99 (176) = happyShift action_67
action_99 (177) = happyShift action_68
action_99 (178) = happyShift action_8
action_99 (180) = happyShift action_70
action_99 (181) = happyShift action_71
action_99 (182) = happyShift action_72
action_99 (183) = happyShift action_73
action_99 (184) = happyShift action_74
action_99 (27) = happyGoto action_228
action_99 (28) = happyGoto action_15
action_99 (29) = happyGoto action_16
action_99 (30) = happyGoto action_17
action_99 (51) = happyGoto action_20
action_99 (53) = happyGoto action_21
action_99 (91) = happyGoto action_23
action_99 (92) = happyGoto action_93
action_99 (95) = happyGoto action_25
action_99 (96) = happyGoto action_7
action_99 (97) = happyGoto action_26
action_99 _ = happyFail

action_100 (105) = happyShift action_32
action_100 (106) = happyShift action_33
action_100 (107) = happyShift action_34
action_100 (108) = happyShift action_35
action_100 (121) = happyShift action_45
action_100 (123) = happyShift action_46
action_100 (131) = happyShift action_47
action_100 (135) = happyShift action_48
action_100 (142) = happyShift action_49
action_100 (147) = happyShift action_50
action_100 (149) = happyShift action_51
action_100 (153) = happyShift action_54
action_100 (154) = happyShift action_55
action_100 (171) = happyShift action_65
action_100 (172) = happyShift action_66
action_100 (176) = happyShift action_67
action_100 (177) = happyShift action_68
action_100 (178) = happyShift action_8
action_100 (180) = happyShift action_70
action_100 (181) = happyShift action_71
action_100 (182) = happyShift action_72
action_100 (183) = happyShift action_73
action_100 (184) = happyShift action_74
action_100 (27) = happyGoto action_227
action_100 (28) = happyGoto action_15
action_100 (29) = happyGoto action_16
action_100 (30) = happyGoto action_17
action_100 (51) = happyGoto action_20
action_100 (53) = happyGoto action_21
action_100 (91) = happyGoto action_23
action_100 (92) = happyGoto action_93
action_100 (95) = happyGoto action_25
action_100 (96) = happyGoto action_7
action_100 (97) = happyGoto action_26
action_100 _ = happyFail

action_101 _ = happyReduce_139

action_102 (105) = happyShift action_32
action_102 (106) = happyShift action_33
action_102 (107) = happyShift action_34
action_102 (108) = happyShift action_35
action_102 (121) = happyShift action_225
action_102 (123) = happyShift action_46
action_102 (131) = happyShift action_47
action_102 (135) = happyShift action_48
action_102 (142) = happyShift action_49
action_102 (147) = happyShift action_50
action_102 (149) = happyShift action_51
action_102 (151) = happyShift action_52
action_102 (152) = happyShift action_53
action_102 (153) = happyShift action_54
action_102 (154) = happyShift action_55
action_102 (157) = happyShift action_56
action_102 (160) = happyShift action_226
action_102 (163) = happyShift action_57
action_102 (164) = happyShift action_58
action_102 (165) = happyShift action_59
action_102 (166) = happyShift action_60
action_102 (167) = happyShift action_61
action_102 (168) = happyShift action_62
action_102 (169) = happyShift action_63
action_102 (170) = happyShift action_64
action_102 (171) = happyShift action_65
action_102 (172) = happyShift action_66
action_102 (176) = happyShift action_67
action_102 (177) = happyShift action_68
action_102 (178) = happyShift action_8
action_102 (179) = happyShift action_69
action_102 (180) = happyShift action_70
action_102 (181) = happyShift action_71
action_102 (182) = happyShift action_72
action_102 (183) = happyShift action_73
action_102 (184) = happyShift action_74
action_102 (24) = happyGoto action_224
action_102 (27) = happyGoto action_14
action_102 (28) = happyGoto action_15
action_102 (29) = happyGoto action_16
action_102 (30) = happyGoto action_17
action_102 (51) = happyGoto action_20
action_102 (53) = happyGoto action_21
action_102 (91) = happyGoto action_23
action_102 (92) = happyGoto action_93
action_102 (95) = happyGoto action_25
action_102 (96) = happyGoto action_7
action_102 (97) = happyGoto action_26
action_102 (98) = happyGoto action_27
action_102 _ = happyFail

action_103 (148) = happyShift action_222
action_103 (156) = happyShift action_223
action_103 _ = happyFail

action_104 (105) = happyShift action_32
action_104 (106) = happyShift action_33
action_104 (107) = happyShift action_34
action_104 (108) = happyShift action_35
action_104 (121) = happyShift action_45
action_104 (123) = happyShift action_46
action_104 (131) = happyShift action_47
action_104 (135) = happyShift action_48
action_104 (142) = happyShift action_49
action_104 (147) = happyShift action_50
action_104 (148) = happyShift action_221
action_104 (149) = happyShift action_51
action_104 (151) = happyShift action_52
action_104 (152) = happyShift action_53
action_104 (153) = happyShift action_54
action_104 (154) = happyShift action_55
action_104 (157) = happyShift action_56
action_104 (163) = happyShift action_57
action_104 (164) = happyShift action_58
action_104 (165) = happyShift action_59
action_104 (166) = happyShift action_60
action_104 (167) = happyShift action_61
action_104 (168) = happyShift action_62
action_104 (169) = happyShift action_63
action_104 (170) = happyShift action_64
action_104 (171) = happyShift action_65
action_104 (172) = happyShift action_66
action_104 (176) = happyShift action_67
action_104 (177) = happyShift action_68
action_104 (178) = happyShift action_8
action_104 (179) = happyShift action_69
action_104 (180) = happyShift action_70
action_104 (181) = happyShift action_71
action_104 (182) = happyShift action_72
action_104 (183) = happyShift action_73
action_104 (184) = happyShift action_74
action_104 (25) = happyGoto action_133
action_104 (27) = happyGoto action_134
action_104 (28) = happyGoto action_15
action_104 (29) = happyGoto action_16
action_104 (30) = happyGoto action_17
action_104 (51) = happyGoto action_20
action_104 (53) = happyGoto action_21
action_104 (91) = happyGoto action_23
action_104 (92) = happyGoto action_93
action_104 (95) = happyGoto action_25
action_104 (96) = happyGoto action_7
action_104 (97) = happyGoto action_26
action_104 (98) = happyGoto action_135
action_104 _ = happyFail

action_105 (105) = happyShift action_32
action_105 (106) = happyShift action_33
action_105 (107) = happyShift action_34
action_105 (108) = happyShift action_35
action_105 (118) = happyShift action_94
action_105 (121) = happyShift action_45
action_105 (123) = happyShift action_46
action_105 (124) = happyShift action_95
action_105 (127) = happyShift action_96
action_105 (128) = happyShift action_97
action_105 (131) = happyShift action_47
action_105 (132) = happyShift action_98
action_105 (133) = happyShift action_99
action_105 (134) = happyShift action_100
action_105 (135) = happyShift action_48
action_105 (142) = happyShift action_49
action_105 (147) = happyShift action_50
action_105 (149) = happyShift action_51
action_105 (151) = happyShift action_52
action_105 (152) = happyShift action_53
action_105 (153) = happyShift action_102
action_105 (154) = happyShift action_55
action_105 (157) = happyShift action_56
action_105 (163) = happyShift action_57
action_105 (164) = happyShift action_58
action_105 (165) = happyShift action_59
action_105 (166) = happyShift action_60
action_105 (167) = happyShift action_61
action_105 (168) = happyShift action_62
action_105 (169) = happyShift action_63
action_105 (170) = happyShift action_64
action_105 (171) = happyShift action_65
action_105 (172) = happyShift action_66
action_105 (176) = happyShift action_67
action_105 (177) = happyShift action_68
action_105 (178) = happyShift action_8
action_105 (179) = happyShift action_69
action_105 (180) = happyShift action_70
action_105 (181) = happyShift action_71
action_105 (182) = happyShift action_72
action_105 (183) = happyShift action_73
action_105 (184) = happyShift action_74
action_105 (18) = happyGoto action_216
action_105 (21) = happyGoto action_88
action_105 (22) = happyGoto action_89
action_105 (23) = happyGoto action_90
action_105 (24) = happyGoto action_217
action_105 (27) = happyGoto action_14
action_105 (28) = happyGoto action_15
action_105 (29) = happyGoto action_16
action_105 (30) = happyGoto action_17
action_105 (32) = happyGoto action_18
action_105 (35) = happyGoto action_218
action_105 (37) = happyGoto action_219
action_105 (38) = happyGoto action_220
action_105 (51) = happyGoto action_20
action_105 (53) = happyGoto action_21
action_105 (91) = happyGoto action_23
action_105 (92) = happyGoto action_24
action_105 (95) = happyGoto action_25
action_105 (96) = happyGoto action_7
action_105 (97) = happyGoto action_26
action_105 (98) = happyGoto action_27
action_105 _ = happyFail

action_106 (143) = happyShift action_145
action_106 (144) = happyShift action_146
action_106 (159) = happyShift action_148
action_106 (42) = happyGoto action_214
action_106 (43) = happyGoto action_215
action_106 (44) = happyGoto action_143
action_106 (45) = happyGoto action_144
action_106 _ = happyFail

action_107 (122) = happyShift action_213
action_107 _ = happyFail

action_108 (171) = happyShift action_212
action_108 _ = happyFail

action_109 (120) = happyShift action_211
action_109 _ = happyFail

action_110 _ = happyReduce_186

action_111 (105) = happyShift action_32
action_111 (106) = happyShift action_33
action_111 (107) = happyShift action_34
action_111 (108) = happyShift action_35
action_111 (142) = happyShift action_113
action_111 (147) = happyShift action_114
action_111 (149) = happyShift action_115
action_111 (162) = happyShift action_210
action_111 (163) = happyShift action_116
action_111 (165) = happyShift action_117
action_111 (166) = happyShift action_118
action_111 (168) = happyShift action_119
action_111 (176) = happyShift action_67
action_111 (178) = happyShift action_8
action_111 (180) = happyShift action_9
action_111 (66) = happyGoto action_108
action_111 (72) = happyGoto action_203
action_111 (73) = happyGoto action_209
action_111 (95) = happyGoto action_205
action_111 (96) = happyGoto action_7
action_111 (97) = happyGoto action_112
action_111 _ = happyReduce_190

action_112 _ = happyReduce_189

action_113 _ = happyReduce_192

action_114 (105) = happyShift action_32
action_114 (106) = happyShift action_33
action_114 (107) = happyShift action_208
action_114 (108) = happyShift action_35
action_114 (142) = happyShift action_113
action_114 (147) = happyShift action_114
action_114 (149) = happyShift action_115
action_114 (163) = happyShift action_116
action_114 (165) = happyShift action_117
action_114 (166) = happyShift action_118
action_114 (168) = happyShift action_119
action_114 (176) = happyShift action_67
action_114 (178) = happyShift action_8
action_114 (180) = happyShift action_9
action_114 (66) = happyGoto action_108
action_114 (70) = happyGoto action_207
action_114 (71) = happyGoto action_185
action_114 (72) = happyGoto action_110
action_114 (95) = happyGoto action_111
action_114 (96) = happyGoto action_7
action_114 (97) = happyGoto action_112
action_114 _ = happyFail

action_115 (105) = happyShift action_32
action_115 (106) = happyShift action_33
action_115 (107) = happyShift action_34
action_115 (108) = happyShift action_35
action_115 (142) = happyShift action_113
action_115 (147) = happyShift action_114
action_115 (149) = happyShift action_115
action_115 (163) = happyShift action_116
action_115 (165) = happyShift action_117
action_115 (166) = happyShift action_118
action_115 (168) = happyShift action_119
action_115 (176) = happyShift action_67
action_115 (178) = happyShift action_8
action_115 (180) = happyShift action_9
action_115 (66) = happyGoto action_108
action_115 (70) = happyGoto action_206
action_115 (71) = happyGoto action_185
action_115 (72) = happyGoto action_110
action_115 (95) = happyGoto action_111
action_115 (96) = happyGoto action_7
action_115 (97) = happyGoto action_112
action_115 _ = happyFail

action_116 _ = happyReduce_172

action_117 _ = happyReduce_175

action_118 _ = happyReduce_173

action_119 _ = happyReduce_174

action_120 (105) = happyShift action_32
action_120 (106) = happyShift action_33
action_120 (107) = happyShift action_34
action_120 (108) = happyShift action_35
action_120 (142) = happyShift action_113
action_120 (147) = happyShift action_114
action_120 (149) = happyShift action_115
action_120 (163) = happyShift action_116
action_120 (165) = happyShift action_117
action_120 (166) = happyShift action_118
action_120 (168) = happyShift action_119
action_120 (176) = happyShift action_67
action_120 (178) = happyShift action_8
action_120 (180) = happyShift action_9
action_120 (66) = happyGoto action_108
action_120 (72) = happyGoto action_203
action_120 (73) = happyGoto action_204
action_120 (95) = happyGoto action_205
action_120 (96) = happyGoto action_7
action_120 (97) = happyGoto action_112
action_120 _ = happyFail

action_121 (105) = happyShift action_32
action_121 (106) = happyShift action_33
action_121 (107) = happyShift action_34
action_121 (108) = happyShift action_35
action_121 (137) = happyShift action_202
action_121 (147) = happyShift action_82
action_121 (176) = happyShift action_67
action_121 (92) = happyGoto action_201
action_121 (97) = happyGoto action_26
action_121 _ = happyFail

action_122 (137) = happyShift action_200
action_122 _ = happyFail

action_123 _ = happyReduce_15

action_124 (105) = happyShift action_32
action_124 (106) = happyShift action_33
action_124 (107) = happyShift action_34
action_124 (108) = happyShift action_35
action_124 (147) = happyShift action_82
action_124 (176) = happyShift action_67
action_124 (92) = happyGoto action_198
action_124 (93) = happyGoto action_199
action_124 (97) = happyGoto action_26
action_124 _ = happyReduce_255

action_125 (162) = happyShift action_197
action_125 _ = happyReduce_261

action_126 _ = happyReduce_6

action_127 _ = happyReduce_9

action_128 (105) = happyShift action_32
action_128 (106) = happyShift action_33
action_128 (107) = happyShift action_34
action_128 (108) = happyShift action_35
action_128 (145) = happyShift action_196
action_128 (147) = happyShift action_82
action_128 (176) = happyShift action_67
action_128 (14) = happyGoto action_194
action_128 (92) = happyGoto action_195
action_128 (97) = happyGoto action_26
action_128 _ = happyFail

action_129 (178) = happyShift action_8
action_129 (180) = happyShift action_9
action_129 (10) = happyGoto action_192
action_129 (11) = happyGoto action_193
action_129 (95) = happyGoto action_6
action_129 (96) = happyGoto action_7
action_129 _ = happyReduce_27

action_130 _ = happyReduce_11

action_131 (108) = happyShift action_191
action_131 (8) = happyGoto action_190
action_131 _ = happyFail

action_132 _ = happyReduce_5

action_133 _ = happyReduce_60

action_134 (105) = happyShift action_32
action_134 (106) = happyShift action_33
action_134 (107) = happyShift action_34
action_134 (108) = happyShift action_35
action_134 (121) = happyShift action_45
action_134 (123) = happyShift action_46
action_134 (131) = happyShift action_47
action_134 (135) = happyShift action_48
action_134 (142) = happyShift action_49
action_134 (147) = happyShift action_50
action_134 (149) = happyShift action_51
action_134 (151) = happyShift action_52
action_134 (152) = happyShift action_53
action_134 (153) = happyShift action_54
action_134 (154) = happyShift action_55
action_134 (157) = happyShift action_56
action_134 (163) = happyShift action_57
action_134 (164) = happyShift action_58
action_134 (165) = happyShift action_59
action_134 (166) = happyShift action_60
action_134 (167) = happyShift action_61
action_134 (168) = happyShift action_62
action_134 (169) = happyShift action_63
action_134 (170) = happyShift action_64
action_134 (171) = happyShift action_65
action_134 (172) = happyShift action_66
action_134 (176) = happyShift action_67
action_134 (177) = happyShift action_68
action_134 (178) = happyShift action_8
action_134 (179) = happyShift action_69
action_134 (180) = happyShift action_70
action_134 (181) = happyShift action_71
action_134 (182) = happyShift action_72
action_134 (183) = happyShift action_73
action_134 (184) = happyShift action_74
action_134 (25) = happyGoto action_189
action_134 (27) = happyGoto action_134
action_134 (28) = happyGoto action_15
action_134 (29) = happyGoto action_16
action_134 (30) = happyGoto action_17
action_134 (51) = happyGoto action_20
action_134 (53) = happyGoto action_21
action_134 (91) = happyGoto action_23
action_134 (92) = happyGoto action_93
action_134 (95) = happyGoto action_25
action_134 (96) = happyGoto action_7
action_134 (97) = happyGoto action_26
action_134 (98) = happyGoto action_135
action_134 _ = happyReduce_62

action_135 (105) = happyShift action_32
action_135 (106) = happyShift action_33
action_135 (107) = happyShift action_34
action_135 (108) = happyShift action_35
action_135 (121) = happyShift action_45
action_135 (123) = happyShift action_46
action_135 (131) = happyShift action_47
action_135 (135) = happyShift action_48
action_135 (142) = happyShift action_49
action_135 (147) = happyShift action_50
action_135 (149) = happyShift action_51
action_135 (151) = happyShift action_52
action_135 (152) = happyShift action_53
action_135 (153) = happyShift action_54
action_135 (154) = happyShift action_55
action_135 (157) = happyShift action_56
action_135 (163) = happyShift action_57
action_135 (164) = happyShift action_58
action_135 (165) = happyShift action_59
action_135 (166) = happyShift action_60
action_135 (167) = happyShift action_61
action_135 (168) = happyShift action_62
action_135 (169) = happyShift action_63
action_135 (170) = happyShift action_64
action_135 (171) = happyShift action_65
action_135 (172) = happyShift action_66
action_135 (176) = happyShift action_67
action_135 (177) = happyShift action_68
action_135 (178) = happyShift action_8
action_135 (179) = happyShift action_69
action_135 (180) = happyShift action_70
action_135 (181) = happyShift action_71
action_135 (182) = happyShift action_72
action_135 (183) = happyShift action_73
action_135 (184) = happyShift action_74
action_135 (25) = happyGoto action_188
action_135 (27) = happyGoto action_134
action_135 (28) = happyGoto action_15
action_135 (29) = happyGoto action_16
action_135 (30) = happyGoto action_17
action_135 (51) = happyGoto action_20
action_135 (53) = happyGoto action_21
action_135 (91) = happyGoto action_23
action_135 (92) = happyGoto action_93
action_135 (95) = happyGoto action_25
action_135 (96) = happyGoto action_7
action_135 (97) = happyGoto action_26
action_135 (98) = happyGoto action_135
action_135 _ = happyReduce_61

action_136 (105) = happyShift action_186
action_136 (106) = happyShift action_33
action_136 (107) = happyShift action_34
action_136 (108) = happyShift action_35
action_136 (136) = happyShift action_187
action_136 (142) = happyShift action_113
action_136 (147) = happyShift action_114
action_136 (149) = happyShift action_115
action_136 (163) = happyShift action_116
action_136 (165) = happyShift action_117
action_136 (166) = happyShift action_118
action_136 (168) = happyShift action_119
action_136 (176) = happyShift action_67
action_136 (178) = happyShift action_8
action_136 (180) = happyShift action_9
action_136 (66) = happyGoto action_108
action_136 (67) = happyGoto action_181
action_136 (68) = happyGoto action_182
action_136 (69) = happyGoto action_183
action_136 (70) = happyGoto action_184
action_136 (71) = happyGoto action_185
action_136 (72) = happyGoto action_110
action_136 (95) = happyGoto action_111
action_136 (96) = happyGoto action_7
action_136 (97) = happyGoto action_112
action_136 _ = happyFail

action_137 (145) = happyShift action_180
action_137 _ = happyFail

action_138 (105) = happyShift action_32
action_138 (106) = happyShift action_33
action_138 (107) = happyShift action_34
action_138 (108) = happyShift action_35
action_138 (147) = happyShift action_179
action_138 (176) = happyShift action_67
action_138 (92) = happyGoto action_178
action_138 (97) = happyGoto action_26
action_138 _ = happyFail

action_139 (105) = happyShift action_32
action_139 (106) = happyShift action_33
action_139 (107) = happyShift action_34
action_139 (108) = happyShift action_35
action_139 (147) = happyShift action_177
action_139 (176) = happyShift action_67
action_139 (92) = happyGoto action_176
action_139 (97) = happyGoto action_26
action_139 _ = happyFail

action_140 _ = happyReduce_59

action_141 _ = happyReduce_98

action_142 (143) = happyShift action_145
action_142 (144) = happyShift action_146
action_142 (159) = happyShift action_148
action_142 (41) = happyGoto action_175
action_142 (43) = happyGoto action_142
action_142 (44) = happyGoto action_143
action_142 (45) = happyGoto action_144
action_142 _ = happyReduce_114

action_143 (155) = happyShift action_174
action_143 _ = happyFail

action_144 (156) = happyShift action_173
action_144 (46) = happyGoto action_171
action_144 (47) = happyGoto action_172
action_144 _ = happyReduce_120

action_145 (105) = happyShift action_32
action_145 (106) = happyShift action_33
action_145 (107) = happyShift action_34
action_145 (108) = happyShift action_35
action_145 (121) = happyShift action_45
action_145 (123) = happyShift action_46
action_145 (131) = happyShift action_47
action_145 (135) = happyShift action_48
action_145 (142) = happyShift action_49
action_145 (147) = happyShift action_50
action_145 (149) = happyShift action_51
action_145 (151) = happyShift action_52
action_145 (152) = happyShift action_53
action_145 (153) = happyShift action_54
action_145 (154) = happyShift action_55
action_145 (157) = happyShift action_56
action_145 (163) = happyShift action_57
action_145 (164) = happyShift action_58
action_145 (165) = happyShift action_59
action_145 (166) = happyShift action_60
action_145 (167) = happyShift action_61
action_145 (168) = happyShift action_62
action_145 (169) = happyShift action_63
action_145 (170) = happyShift action_64
action_145 (171) = happyShift action_65
action_145 (172) = happyShift action_66
action_145 (176) = happyShift action_67
action_145 (177) = happyShift action_68
action_145 (178) = happyShift action_8
action_145 (179) = happyShift action_69
action_145 (180) = happyShift action_70
action_145 (181) = happyShift action_71
action_145 (182) = happyShift action_72
action_145 (183) = happyShift action_73
action_145 (184) = happyShift action_74
action_145 (23) = happyGoto action_169
action_145 (24) = happyGoto action_91
action_145 (27) = happyGoto action_14
action_145 (28) = happyGoto action_15
action_145 (29) = happyGoto action_16
action_145 (30) = happyGoto action_17
action_145 (48) = happyGoto action_170
action_145 (51) = happyGoto action_20
action_145 (53) = happyGoto action_21
action_145 (91) = happyGoto action_23
action_145 (92) = happyGoto action_93
action_145 (95) = happyGoto action_25
action_145 (96) = happyGoto action_166
action_145 (97) = happyGoto action_26
action_145 (98) = happyGoto action_27
action_145 _ = happyFail

action_146 (105) = happyShift action_32
action_146 (106) = happyShift action_33
action_146 (107) = happyShift action_34
action_146 (108) = happyShift action_35
action_146 (118) = happyShift action_94
action_146 (121) = happyShift action_45
action_146 (123) = happyShift action_46
action_146 (124) = happyShift action_95
action_146 (127) = happyShift action_96
action_146 (128) = happyShift action_97
action_146 (131) = happyShift action_47
action_146 (132) = happyShift action_98
action_146 (133) = happyShift action_99
action_146 (134) = happyShift action_100
action_146 (135) = happyShift action_48
action_146 (142) = happyShift action_49
action_146 (147) = happyShift action_50
action_146 (149) = happyShift action_51
action_146 (151) = happyShift action_52
action_146 (152) = happyShift action_53
action_146 (153) = happyShift action_102
action_146 (154) = happyShift action_55
action_146 (157) = happyShift action_56
action_146 (163) = happyShift action_57
action_146 (164) = happyShift action_58
action_146 (165) = happyShift action_59
action_146 (166) = happyShift action_60
action_146 (167) = happyShift action_61
action_146 (168) = happyShift action_62
action_146 (169) = happyShift action_63
action_146 (170) = happyShift action_64
action_146 (171) = happyShift action_65
action_146 (172) = happyShift action_66
action_146 (176) = happyShift action_67
action_146 (177) = happyShift action_68
action_146 (178) = happyShift action_8
action_146 (179) = happyShift action_69
action_146 (180) = happyShift action_70
action_146 (181) = happyShift action_71
action_146 (182) = happyShift action_72
action_146 (183) = happyShift action_73
action_146 (184) = happyShift action_74
action_146 (18) = happyGoto action_168
action_146 (21) = happyGoto action_88
action_146 (22) = happyGoto action_89
action_146 (23) = happyGoto action_90
action_146 (24) = happyGoto action_91
action_146 (27) = happyGoto action_14
action_146 (28) = happyGoto action_15
action_146 (29) = happyGoto action_16
action_146 (30) = happyGoto action_17
action_146 (51) = happyGoto action_20
action_146 (53) = happyGoto action_21
action_146 (91) = happyGoto action_23
action_146 (92) = happyGoto action_93
action_146 (95) = happyGoto action_25
action_146 (96) = happyGoto action_7
action_146 (97) = happyGoto action_26
action_146 (98) = happyGoto action_27
action_146 _ = happyFail

action_147 (105) = happyShift action_32
action_147 (106) = happyShift action_33
action_147 (107) = happyShift action_34
action_147 (108) = happyShift action_35
action_147 (118) = happyShift action_94
action_147 (121) = happyShift action_45
action_147 (123) = happyShift action_46
action_147 (124) = happyShift action_95
action_147 (127) = happyShift action_96
action_147 (128) = happyShift action_97
action_147 (131) = happyShift action_47
action_147 (132) = happyShift action_98
action_147 (133) = happyShift action_99
action_147 (134) = happyShift action_100
action_147 (135) = happyShift action_48
action_147 (142) = happyShift action_49
action_147 (147) = happyShift action_50
action_147 (149) = happyShift action_51
action_147 (151) = happyShift action_52
action_147 (152) = happyShift action_53
action_147 (153) = happyShift action_102
action_147 (154) = happyShift action_55
action_147 (157) = happyShift action_56
action_147 (163) = happyShift action_57
action_147 (164) = happyShift action_58
action_147 (165) = happyShift action_59
action_147 (166) = happyShift action_60
action_147 (167) = happyShift action_61
action_147 (168) = happyShift action_62
action_147 (169) = happyShift action_63
action_147 (170) = happyShift action_64
action_147 (171) = happyShift action_65
action_147 (172) = happyShift action_66
action_147 (176) = happyShift action_67
action_147 (177) = happyShift action_68
action_147 (178) = happyShift action_8
action_147 (179) = happyShift action_69
action_147 (180) = happyShift action_70
action_147 (181) = happyShift action_71
action_147 (182) = happyShift action_72
action_147 (183) = happyShift action_73
action_147 (184) = happyShift action_74
action_147 (18) = happyGoto action_162
action_147 (21) = happyGoto action_88
action_147 (22) = happyGoto action_89
action_147 (23) = happyGoto action_90
action_147 (24) = happyGoto action_91
action_147 (27) = happyGoto action_14
action_147 (28) = happyGoto action_15
action_147 (29) = happyGoto action_16
action_147 (30) = happyGoto action_17
action_147 (34) = happyGoto action_167
action_147 (51) = happyGoto action_20
action_147 (53) = happyGoto action_21
action_147 (91) = happyGoto action_23
action_147 (92) = happyGoto action_93
action_147 (95) = happyGoto action_25
action_147 (96) = happyGoto action_7
action_147 (97) = happyGoto action_26
action_147 (98) = happyGoto action_27
action_147 _ = happyFail

action_148 (105) = happyShift action_32
action_148 (106) = happyShift action_33
action_148 (107) = happyShift action_34
action_148 (108) = happyShift action_35
action_148 (118) = happyShift action_94
action_148 (121) = happyShift action_45
action_148 (123) = happyShift action_46
action_148 (124) = happyShift action_95
action_148 (127) = happyShift action_96
action_148 (128) = happyShift action_97
action_148 (131) = happyShift action_47
action_148 (132) = happyShift action_98
action_148 (133) = happyShift action_99
action_148 (134) = happyShift action_100
action_148 (135) = happyShift action_48
action_148 (142) = happyShift action_49
action_148 (147) = happyShift action_50
action_148 (149) = happyShift action_51
action_148 (151) = happyShift action_52
action_148 (152) = happyShift action_53
action_148 (153) = happyShift action_102
action_148 (154) = happyShift action_55
action_148 (157) = happyShift action_56
action_148 (163) = happyShift action_57
action_148 (164) = happyShift action_58
action_148 (165) = happyShift action_59
action_148 (166) = happyShift action_60
action_148 (167) = happyShift action_61
action_148 (168) = happyShift action_62
action_148 (169) = happyShift action_63
action_148 (170) = happyShift action_64
action_148 (171) = happyShift action_65
action_148 (172) = happyShift action_66
action_148 (176) = happyShift action_67
action_148 (177) = happyShift action_68
action_148 (178) = happyShift action_8
action_148 (179) = happyShift action_69
action_148 (180) = happyShift action_70
action_148 (181) = happyShift action_71
action_148 (182) = happyShift action_72
action_148 (183) = happyShift action_73
action_148 (184) = happyShift action_74
action_148 (18) = happyGoto action_162
action_148 (21) = happyGoto action_88
action_148 (22) = happyGoto action_89
action_148 (23) = happyGoto action_163
action_148 (24) = happyGoto action_91
action_148 (27) = happyGoto action_14
action_148 (28) = happyGoto action_15
action_148 (29) = happyGoto action_16
action_148 (30) = happyGoto action_17
action_148 (34) = happyGoto action_164
action_148 (48) = happyGoto action_165
action_148 (51) = happyGoto action_20
action_148 (53) = happyGoto action_21
action_148 (91) = happyGoto action_23
action_148 (92) = happyGoto action_93
action_148 (95) = happyGoto action_25
action_148 (96) = happyGoto action_166
action_148 (97) = happyGoto action_26
action_148 (98) = happyGoto action_27
action_148 _ = happyFail

action_149 (151) = happyShift action_52
action_149 (152) = happyShift action_53
action_149 (157) = happyShift action_56
action_149 (163) = happyShift action_57
action_149 (164) = happyShift action_58
action_149 (165) = happyShift action_59
action_149 (166) = happyShift action_60
action_149 (167) = happyShift action_61
action_149 (168) = happyShift action_62
action_149 (169) = happyShift action_63
action_149 (170) = happyShift action_64
action_149 (179) = happyShift action_69
action_149 (13) = happyGoto action_160
action_149 (98) = happyGoto action_161
action_149 _ = happyFail

action_150 (101) = happyShift action_28
action_150 (102) = happyShift action_29
action_150 (103) = happyShift action_30
action_150 (104) = happyShift action_31
action_150 (105) = happyShift action_32
action_150 (106) = happyShift action_33
action_150 (107) = happyShift action_34
action_150 (108) = happyShift action_35
action_150 (109) = happyShift action_36
action_150 (110) = happyShift action_37
action_150 (111) = happyShift action_38
action_150 (112) = happyShift action_39
action_150 (113) = happyShift action_40
action_150 (114) = happyShift action_41
action_150 (115) = happyShift action_42
action_150 (116) = happyShift action_43
action_150 (117) = happyShift action_44
action_150 (121) = happyShift action_45
action_150 (123) = happyShift action_46
action_150 (131) = happyShift action_47
action_150 (135) = happyShift action_48
action_150 (142) = happyShift action_49
action_150 (147) = happyShift action_50
action_150 (149) = happyShift action_51
action_150 (151) = happyShift action_52
action_150 (152) = happyShift action_53
action_150 (153) = happyShift action_54
action_150 (154) = happyShift action_55
action_150 (157) = happyShift action_56
action_150 (163) = happyShift action_57
action_150 (164) = happyShift action_58
action_150 (165) = happyShift action_59
action_150 (166) = happyShift action_60
action_150 (167) = happyShift action_61
action_150 (168) = happyShift action_62
action_150 (169) = happyShift action_63
action_150 (170) = happyShift action_64
action_150 (171) = happyShift action_65
action_150 (172) = happyShift action_66
action_150 (176) = happyShift action_67
action_150 (177) = happyShift action_68
action_150 (178) = happyShift action_8
action_150 (179) = happyShift action_69
action_150 (180) = happyShift action_70
action_150 (181) = happyShift action_71
action_150 (182) = happyShift action_72
action_150 (183) = happyShift action_73
action_150 (184) = happyShift action_74
action_150 (5) = happyGoto action_159
action_150 (6) = happyGoto action_11
action_150 (12) = happyGoto action_12
action_150 (24) = happyGoto action_13
action_150 (27) = happyGoto action_14
action_150 (28) = happyGoto action_15
action_150 (29) = happyGoto action_16
action_150 (30) = happyGoto action_17
action_150 (32) = happyGoto action_18
action_150 (35) = happyGoto action_19
action_150 (51) = happyGoto action_20
action_150 (53) = happyGoto action_21
action_150 (57) = happyGoto action_22
action_150 (91) = happyGoto action_23
action_150 (92) = happyGoto action_24
action_150 (95) = happyGoto action_25
action_150 (96) = happyGoto action_7
action_150 (97) = happyGoto action_26
action_150 (98) = happyGoto action_27
action_150 _ = happyReduce_282

action_151 _ = happyReduce_3

action_152 (158) = happyShift action_152
action_152 (99) = happyGoto action_158
action_152 _ = happyReduce_279

action_153 _ = happyReduce_1

action_154 (178) = happyShift action_8
action_154 (96) = happyGoto action_157
action_154 _ = happyFail

action_155 (145) = happyShift action_156
action_155 _ = happyFail

action_156 (101) = happyShift action_28
action_156 (102) = happyShift action_29
action_156 (103) = happyShift action_30
action_156 (104) = happyShift action_31
action_156 (105) = happyShift action_32
action_156 (106) = happyShift action_33
action_156 (107) = happyShift action_34
action_156 (108) = happyShift action_35
action_156 (109) = happyShift action_36
action_156 (110) = happyShift action_37
action_156 (111) = happyShift action_38
action_156 (112) = happyShift action_39
action_156 (113) = happyShift action_40
action_156 (114) = happyShift action_41
action_156 (115) = happyShift action_42
action_156 (116) = happyShift action_43
action_156 (117) = happyShift action_44
action_156 (121) = happyShift action_45
action_156 (123) = happyShift action_46
action_156 (131) = happyShift action_47
action_156 (135) = happyShift action_48
action_156 (142) = happyShift action_49
action_156 (147) = happyShift action_50
action_156 (149) = happyShift action_51
action_156 (151) = happyShift action_52
action_156 (152) = happyShift action_53
action_156 (153) = happyShift action_54
action_156 (154) = happyShift action_55
action_156 (157) = happyShift action_56
action_156 (163) = happyShift action_57
action_156 (164) = happyShift action_58
action_156 (165) = happyShift action_59
action_156 (166) = happyShift action_60
action_156 (167) = happyShift action_61
action_156 (168) = happyShift action_62
action_156 (169) = happyShift action_63
action_156 (170) = happyShift action_64
action_156 (171) = happyShift action_65
action_156 (172) = happyShift action_66
action_156 (176) = happyShift action_67
action_156 (177) = happyShift action_68
action_156 (178) = happyShift action_8
action_156 (179) = happyShift action_69
action_156 (180) = happyShift action_70
action_156 (181) = happyShift action_71
action_156 (182) = happyShift action_72
action_156 (183) = happyShift action_73
action_156 (184) = happyShift action_74
action_156 (5) = happyGoto action_316
action_156 (6) = happyGoto action_11
action_156 (12) = happyGoto action_12
action_156 (24) = happyGoto action_13
action_156 (27) = happyGoto action_14
action_156 (28) = happyGoto action_15
action_156 (29) = happyGoto action_16
action_156 (30) = happyGoto action_17
action_156 (32) = happyGoto action_18
action_156 (35) = happyGoto action_19
action_156 (51) = happyGoto action_20
action_156 (53) = happyGoto action_21
action_156 (57) = happyGoto action_22
action_156 (91) = happyGoto action_23
action_156 (92) = happyGoto action_24
action_156 (95) = happyGoto action_25
action_156 (96) = happyGoto action_7
action_156 (97) = happyGoto action_26
action_156 (98) = happyGoto action_27
action_156 _ = happyFail

action_157 _ = happyReduce_260

action_158 _ = happyReduce_280

action_159 _ = happyReduce_4

action_160 _ = happyReduce_12

action_161 (156) = happyShift action_315
action_161 _ = happyReduce_33

action_162 (120) = happyShift action_314
action_162 _ = happyReduce_101

action_163 (174) = happyReduce_129
action_163 _ = happyReduce_54

action_164 _ = happyReduce_123

action_165 (174) = happyShift action_313
action_165 _ = happyFail

action_166 (145) = happyShift action_312
action_166 _ = happyReduce_259

action_167 _ = happyReduce_97

action_168 _ = happyReduce_119

action_169 _ = happyReduce_129

action_170 _ = happyReduce_124

action_171 _ = happyReduce_121

action_172 (156) = happyShift action_173
action_172 (46) = happyGoto action_311
action_172 (47) = happyGoto action_172
action_172 _ = happyReduce_125

action_173 (105) = happyShift action_32
action_173 (106) = happyShift action_33
action_173 (107) = happyShift action_34
action_173 (108) = happyShift action_35
action_173 (118) = happyShift action_94
action_173 (121) = happyShift action_45
action_173 (123) = happyShift action_46
action_173 (124) = happyShift action_95
action_173 (127) = happyShift action_96
action_173 (128) = happyShift action_97
action_173 (131) = happyShift action_47
action_173 (132) = happyShift action_98
action_173 (133) = happyShift action_99
action_173 (134) = happyShift action_100
action_173 (135) = happyShift action_48
action_173 (142) = happyShift action_49
action_173 (147) = happyShift action_50
action_173 (149) = happyShift action_51
action_173 (151) = happyShift action_52
action_173 (152) = happyShift action_53
action_173 (153) = happyShift action_102
action_173 (154) = happyShift action_55
action_173 (157) = happyShift action_56
action_173 (163) = happyShift action_57
action_173 (164) = happyShift action_58
action_173 (165) = happyShift action_59
action_173 (166) = happyShift action_60
action_173 (167) = happyShift action_61
action_173 (168) = happyShift action_62
action_173 (169) = happyShift action_63
action_173 (170) = happyShift action_64
action_173 (171) = happyShift action_65
action_173 (172) = happyShift action_66
action_173 (176) = happyShift action_67
action_173 (177) = happyShift action_68
action_173 (178) = happyShift action_8
action_173 (179) = happyShift action_69
action_173 (180) = happyShift action_70
action_173 (181) = happyShift action_71
action_173 (182) = happyShift action_72
action_173 (183) = happyShift action_73
action_173 (184) = happyShift action_74
action_173 (18) = happyGoto action_162
action_173 (21) = happyGoto action_88
action_173 (22) = happyGoto action_89
action_173 (23) = happyGoto action_163
action_173 (24) = happyGoto action_91
action_173 (27) = happyGoto action_14
action_173 (28) = happyGoto action_15
action_173 (29) = happyGoto action_16
action_173 (30) = happyGoto action_17
action_173 (34) = happyGoto action_309
action_173 (48) = happyGoto action_310
action_173 (51) = happyGoto action_20
action_173 (53) = happyGoto action_21
action_173 (91) = happyGoto action_23
action_173 (92) = happyGoto action_93
action_173 (95) = happyGoto action_25
action_173 (96) = happyGoto action_166
action_173 (97) = happyGoto action_26
action_173 (98) = happyGoto action_27
action_173 _ = happyFail

action_174 (105) = happyShift action_32
action_174 (106) = happyShift action_33
action_174 (107) = happyShift action_34
action_174 (108) = happyShift action_35
action_174 (118) = happyShift action_94
action_174 (121) = happyShift action_45
action_174 (123) = happyShift action_46
action_174 (124) = happyShift action_95
action_174 (127) = happyShift action_96
action_174 (128) = happyShift action_97
action_174 (131) = happyShift action_47
action_174 (132) = happyShift action_98
action_174 (133) = happyShift action_99
action_174 (134) = happyShift action_100
action_174 (135) = happyShift action_48
action_174 (142) = happyShift action_49
action_174 (147) = happyShift action_50
action_174 (149) = happyShift action_51
action_174 (151) = happyShift action_52
action_174 (152) = happyShift action_53
action_174 (153) = happyShift action_102
action_174 (154) = happyShift action_55
action_174 (157) = happyShift action_56
action_174 (163) = happyShift action_57
action_174 (164) = happyShift action_58
action_174 (165) = happyShift action_59
action_174 (166) = happyShift action_60
action_174 (167) = happyShift action_61
action_174 (168) = happyShift action_62
action_174 (169) = happyShift action_63
action_174 (170) = happyShift action_64
action_174 (171) = happyShift action_65
action_174 (172) = happyShift action_66
action_174 (176) = happyShift action_67
action_174 (177) = happyShift action_68
action_174 (178) = happyShift action_8
action_174 (179) = happyShift action_69
action_174 (180) = happyShift action_70
action_174 (181) = happyShift action_71
action_174 (182) = happyShift action_72
action_174 (183) = happyShift action_73
action_174 (184) = happyShift action_74
action_174 (18) = happyGoto action_162
action_174 (21) = happyGoto action_88
action_174 (22) = happyGoto action_89
action_174 (23) = happyGoto action_90
action_174 (24) = happyGoto action_91
action_174 (27) = happyGoto action_14
action_174 (28) = happyGoto action_15
action_174 (29) = happyGoto action_16
action_174 (30) = happyGoto action_17
action_174 (34) = happyGoto action_308
action_174 (51) = happyGoto action_20
action_174 (53) = happyGoto action_21
action_174 (91) = happyGoto action_23
action_174 (92) = happyGoto action_93
action_174 (95) = happyGoto action_25
action_174 (96) = happyGoto action_7
action_174 (97) = happyGoto action_26
action_174 (98) = happyGoto action_27
action_174 _ = happyFail

action_175 _ = happyReduce_115

action_176 _ = happyReduce_81

action_177 (105) = happyShift action_32
action_177 (106) = happyShift action_33
action_177 (107) = happyShift action_34
action_177 (108) = happyShift action_35
action_177 (118) = happyShift action_94
action_177 (121) = happyShift action_45
action_177 (123) = happyShift action_46
action_177 (124) = happyShift action_95
action_177 (127) = happyShift action_96
action_177 (128) = happyShift action_97
action_177 (131) = happyShift action_47
action_177 (132) = happyShift action_98
action_177 (133) = happyShift action_99
action_177 (134) = happyShift action_100
action_177 (135) = happyShift action_48
action_177 (142) = happyShift action_49
action_177 (147) = happyShift action_50
action_177 (149) = happyShift action_51
action_177 (151) = happyShift action_52
action_177 (152) = happyShift action_53
action_177 (153) = happyShift action_102
action_177 (154) = happyShift action_55
action_177 (157) = happyShift action_56
action_177 (163) = happyShift action_57
action_177 (164) = happyShift action_58
action_177 (165) = happyShift action_59
action_177 (166) = happyShift action_60
action_177 (167) = happyShift action_61
action_177 (168) = happyShift action_62
action_177 (169) = happyShift action_63
action_177 (170) = happyShift action_64
action_177 (171) = happyShift action_65
action_177 (172) = happyShift action_66
action_177 (176) = happyShift action_67
action_177 (177) = happyShift action_68
action_177 (178) = happyShift action_8
action_177 (179) = happyShift action_69
action_177 (180) = happyShift action_70
action_177 (181) = happyShift action_71
action_177 (182) = happyShift action_72
action_177 (183) = happyShift action_73
action_177 (184) = happyShift action_74
action_177 (18) = happyGoto action_307
action_177 (21) = happyGoto action_88
action_177 (22) = happyGoto action_89
action_177 (23) = happyGoto action_90
action_177 (24) = happyGoto action_91
action_177 (27) = happyGoto action_14
action_177 (28) = happyGoto action_15
action_177 (29) = happyGoto action_16
action_177 (30) = happyGoto action_17
action_177 (51) = happyGoto action_20
action_177 (53) = happyGoto action_21
action_177 (91) = happyGoto action_23
action_177 (92) = happyGoto action_93
action_177 (95) = happyGoto action_25
action_177 (96) = happyGoto action_7
action_177 (97) = happyGoto action_26
action_177 (98) = happyGoto action_104
action_177 _ = happyFail

action_178 _ = happyReduce_80

action_179 (105) = happyShift action_32
action_179 (106) = happyShift action_33
action_179 (107) = happyShift action_34
action_179 (108) = happyShift action_35
action_179 (118) = happyShift action_94
action_179 (121) = happyShift action_45
action_179 (123) = happyShift action_46
action_179 (124) = happyShift action_95
action_179 (127) = happyShift action_96
action_179 (128) = happyShift action_97
action_179 (131) = happyShift action_47
action_179 (132) = happyShift action_98
action_179 (133) = happyShift action_99
action_179 (134) = happyShift action_100
action_179 (135) = happyShift action_48
action_179 (142) = happyShift action_49
action_179 (147) = happyShift action_50
action_179 (149) = happyShift action_51
action_179 (151) = happyShift action_52
action_179 (152) = happyShift action_53
action_179 (153) = happyShift action_102
action_179 (154) = happyShift action_55
action_179 (157) = happyShift action_56
action_179 (163) = happyShift action_57
action_179 (164) = happyShift action_58
action_179 (165) = happyShift action_59
action_179 (166) = happyShift action_60
action_179 (167) = happyShift action_61
action_179 (168) = happyShift action_62
action_179 (169) = happyShift action_63
action_179 (170) = happyShift action_64
action_179 (171) = happyShift action_65
action_179 (172) = happyShift action_66
action_179 (176) = happyShift action_67
action_179 (177) = happyShift action_68
action_179 (178) = happyShift action_8
action_179 (179) = happyShift action_69
action_179 (180) = happyShift action_70
action_179 (181) = happyShift action_71
action_179 (182) = happyShift action_72
action_179 (183) = happyShift action_73
action_179 (184) = happyShift action_74
action_179 (18) = happyGoto action_306
action_179 (21) = happyGoto action_88
action_179 (22) = happyGoto action_89
action_179 (23) = happyGoto action_90
action_179 (24) = happyGoto action_91
action_179 (27) = happyGoto action_14
action_179 (28) = happyGoto action_15
action_179 (29) = happyGoto action_16
action_179 (30) = happyGoto action_17
action_179 (51) = happyGoto action_20
action_179 (53) = happyGoto action_21
action_179 (91) = happyGoto action_23
action_179 (92) = happyGoto action_93
action_179 (95) = happyGoto action_25
action_179 (96) = happyGoto action_7
action_179 (97) = happyGoto action_26
action_179 (98) = happyGoto action_104
action_179 _ = happyFail

action_180 (105) = happyShift action_32
action_180 (106) = happyShift action_33
action_180 (107) = happyShift action_34
action_180 (108) = happyShift action_35
action_180 (142) = happyShift action_113
action_180 (147) = happyShift action_114
action_180 (149) = happyShift action_115
action_180 (163) = happyShift action_116
action_180 (165) = happyShift action_117
action_180 (166) = happyShift action_118
action_180 (168) = happyShift action_119
action_180 (176) = happyShift action_67
action_180 (178) = happyShift action_8
action_180 (180) = happyShift action_9
action_180 (66) = happyGoto action_108
action_180 (70) = happyGoto action_305
action_180 (71) = happyGoto action_185
action_180 (72) = happyGoto action_110
action_180 (95) = happyGoto action_111
action_180 (96) = happyGoto action_7
action_180 (97) = happyGoto action_112
action_180 _ = happyFail

action_181 _ = happyReduce_104

action_182 _ = happyReduce_176

action_183 _ = happyReduce_178

action_184 (139) = happyShift action_304
action_184 _ = happyReduce_180

action_185 (140) = happyShift action_302
action_185 (164) = happyShift action_303
action_185 _ = happyReduce_182

action_186 (105) = happyShift action_186
action_186 (106) = happyShift action_33
action_186 (107) = happyShift action_34
action_186 (108) = happyShift action_35
action_186 (136) = happyShift action_187
action_186 (142) = happyShift action_113
action_186 (147) = happyShift action_114
action_186 (149) = happyShift action_115
action_186 (163) = happyShift action_116
action_186 (165) = happyShift action_117
action_186 (166) = happyShift action_118
action_186 (168) = happyShift action_119
action_186 (176) = happyShift action_67
action_186 (178) = happyShift action_8
action_186 (180) = happyShift action_9
action_186 (66) = happyGoto action_108
action_186 (67) = happyGoto action_301
action_186 (68) = happyGoto action_182
action_186 (69) = happyGoto action_183
action_186 (70) = happyGoto action_184
action_186 (71) = happyGoto action_185
action_186 (72) = happyGoto action_110
action_186 (95) = happyGoto action_111
action_186 (96) = happyGoto action_7
action_186 (97) = happyGoto action_112
action_186 _ = happyReduce_263

action_187 (105) = happyShift action_32
action_187 (106) = happyShift action_33
action_187 (107) = happyShift action_34
action_187 (108) = happyShift action_35
action_187 (147) = happyShift action_300
action_187 (176) = happyShift action_67
action_187 (75) = happyGoto action_297
action_187 (76) = happyGoto action_298
action_187 (92) = happyGoto action_299
action_187 (97) = happyGoto action_26
action_187 _ = happyFail

action_188 _ = happyReduce_63

action_189 _ = happyReduce_64

action_190 _ = happyReduce_21

action_191 (184) = happyShift action_296
action_191 (9) = happyGoto action_295
action_191 _ = happyReduce_24

action_192 (158) = happyShift action_294
action_192 _ = happyReduce_28

action_193 (146) = happyShift action_293
action_193 _ = happyFail

action_194 _ = happyReduce_7

action_195 (137) = happyShift action_292
action_195 _ = happyFail

action_196 (105) = happyShift action_32
action_196 (106) = happyShift action_33
action_196 (107) = happyShift action_34
action_196 (108) = happyShift action_35
action_196 (147) = happyShift action_82
action_196 (176) = happyShift action_67
action_196 (14) = happyGoto action_290
action_196 (15) = happyGoto action_291
action_196 (92) = happyGoto action_195
action_196 (97) = happyGoto action_26
action_196 _ = happyFail

action_197 (105) = happyShift action_32
action_197 (106) = happyShift action_33
action_197 (107) = happyShift action_34
action_197 (108) = happyShift action_35
action_197 (147) = happyShift action_82
action_197 (176) = happyShift action_67
action_197 (92) = happyGoto action_198
action_197 (93) = happyGoto action_289
action_197 (97) = happyGoto action_26
action_197 _ = happyReduce_255

action_198 (105) = happyShift action_32
action_198 (106) = happyShift action_33
action_198 (107) = happyShift action_34
action_198 (108) = happyShift action_35
action_198 (147) = happyShift action_82
action_198 (176) = happyShift action_67
action_198 (92) = happyGoto action_198
action_198 (93) = happyGoto action_288
action_198 (97) = happyGoto action_26
action_198 _ = happyReduce_255

action_199 (155) = happyShift action_287
action_199 _ = happyReduce_151

action_200 (163) = happyShift action_116
action_200 (165) = happyShift action_117
action_200 (166) = happyShift action_118
action_200 (168) = happyShift action_119
action_200 (65) = happyGoto action_286
action_200 (66) = happyGoto action_284
action_200 _ = happyFail

action_201 (120) = happyShift action_285
action_201 _ = happyFail

action_202 (163) = happyShift action_116
action_202 (165) = happyShift action_117
action_202 (166) = happyShift action_118
action_202 (168) = happyShift action_119
action_202 (65) = happyGoto action_283
action_202 (66) = happyGoto action_284
action_202 _ = happyFail

action_203 (105) = happyShift action_32
action_203 (106) = happyShift action_33
action_203 (107) = happyShift action_34
action_203 (108) = happyShift action_35
action_203 (142) = happyShift action_113
action_203 (147) = happyShift action_114
action_203 (149) = happyShift action_115
action_203 (163) = happyShift action_116
action_203 (165) = happyShift action_117
action_203 (166) = happyShift action_118
action_203 (168) = happyShift action_119
action_203 (176) = happyShift action_67
action_203 (178) = happyShift action_8
action_203 (180) = happyShift action_9
action_203 (66) = happyGoto action_108
action_203 (72) = happyGoto action_203
action_203 (73) = happyGoto action_282
action_203 (95) = happyGoto action_205
action_203 (96) = happyGoto action_7
action_203 (97) = happyGoto action_112
action_203 _ = happyReduce_198

action_204 (120) = happyShift action_281
action_204 _ = happyFail

action_205 (162) = happyShift action_280
action_205 _ = happyReduce_190

action_206 (150) = happyShift action_279
action_206 _ = happyFail

action_207 (148) = happyShift action_277
action_207 (156) = happyShift action_278
action_207 _ = happyFail

action_208 (105) = happyShift action_32
action_208 (106) = happyShift action_33
action_208 (107) = happyShift action_34
action_208 (108) = happyShift action_35
action_208 (142) = happyShift action_113
action_208 (147) = happyShift action_114
action_208 (149) = happyShift action_115
action_208 (163) = happyShift action_116
action_208 (165) = happyShift action_117
action_208 (166) = happyShift action_118
action_208 (168) = happyShift action_119
action_208 (176) = happyShift action_67
action_208 (178) = happyShift action_8
action_208 (180) = happyShift action_9
action_208 (66) = happyGoto action_108
action_208 (70) = happyGoto action_276
action_208 (71) = happyGoto action_185
action_208 (72) = happyGoto action_110
action_208 (95) = happyGoto action_111
action_208 (96) = happyGoto action_7
action_208 (97) = happyGoto action_112
action_208 _ = happyReduce_265

action_209 _ = happyReduce_187

action_210 (105) = happyShift action_32
action_210 (106) = happyShift action_33
action_210 (107) = happyShift action_34
action_210 (108) = happyShift action_35
action_210 (142) = happyShift action_113
action_210 (147) = happyShift action_114
action_210 (149) = happyShift action_115
action_210 (163) = happyShift action_116
action_210 (165) = happyShift action_117
action_210 (166) = happyShift action_118
action_210 (168) = happyShift action_119
action_210 (176) = happyShift action_67
action_210 (178) = happyShift action_8
action_210 (180) = happyShift action_9
action_210 (66) = happyGoto action_108
action_210 (72) = happyGoto action_203
action_210 (73) = happyGoto action_275
action_210 (95) = happyGoto action_205
action_210 (96) = happyGoto action_7
action_210 (97) = happyGoto action_112
action_210 _ = happyReduce_191

action_211 (145) = happyShift action_274
action_211 _ = happyFail

action_212 _ = happyReduce_195

action_213 (145) = happyShift action_273
action_213 _ = happyFail

action_214 (146) = happyShift action_272
action_214 _ = happyFail

action_215 (158) = happyShift action_152
action_215 (99) = happyGoto action_270
action_215 (100) = happyGoto action_271
action_215 _ = happyReduce_281

action_216 _ = happyReduce_108

action_217 (143) = happyShift action_145
action_217 (144) = happyShift action_146
action_217 (155) = happyShift action_147
action_217 (159) = happyShift action_148
action_217 (41) = happyGoto action_141
action_217 (43) = happyGoto action_142
action_217 (44) = happyGoto action_143
action_217 (45) = happyGoto action_144
action_217 _ = happyReduce_57

action_218 _ = happyReduce_107

action_219 (158) = happyShift action_152
action_219 (99) = happyGoto action_268
action_219 (100) = happyGoto action_269
action_219 _ = happyReduce_281

action_220 (146) = happyShift action_267
action_220 _ = happyFail

action_221 _ = happyReduce_254

action_222 _ = happyReduce_79

action_223 (105) = happyShift action_32
action_223 (106) = happyShift action_33
action_223 (107) = happyShift action_34
action_223 (108) = happyShift action_35
action_223 (118) = happyShift action_94
action_223 (121) = happyShift action_45
action_223 (123) = happyShift action_46
action_223 (124) = happyShift action_95
action_223 (127) = happyShift action_96
action_223 (128) = happyShift action_97
action_223 (131) = happyShift action_47
action_223 (132) = happyShift action_98
action_223 (133) = happyShift action_99
action_223 (134) = happyShift action_100
action_223 (135) = happyShift action_48
action_223 (142) = happyShift action_49
action_223 (147) = happyShift action_50
action_223 (149) = happyShift action_51
action_223 (151) = happyShift action_52
action_223 (152) = happyShift action_53
action_223 (153) = happyShift action_102
action_223 (154) = happyShift action_55
action_223 (157) = happyShift action_56
action_223 (163) = happyShift action_57
action_223 (164) = happyShift action_58
action_223 (165) = happyShift action_59
action_223 (166) = happyShift action_60
action_223 (167) = happyShift action_61
action_223 (168) = happyShift action_62
action_223 (169) = happyShift action_63
action_223 (170) = happyShift action_64
action_223 (171) = happyShift action_65
action_223 (172) = happyShift action_66
action_223 (176) = happyShift action_67
action_223 (177) = happyShift action_68
action_223 (178) = happyShift action_8
action_223 (179) = happyShift action_69
action_223 (180) = happyShift action_70
action_223 (181) = happyShift action_71
action_223 (182) = happyShift action_72
action_223 (183) = happyShift action_73
action_223 (184) = happyShift action_74
action_223 (18) = happyGoto action_265
action_223 (21) = happyGoto action_88
action_223 (22) = happyGoto action_89
action_223 (23) = happyGoto action_90
action_223 (24) = happyGoto action_91
action_223 (27) = happyGoto action_14
action_223 (28) = happyGoto action_15
action_223 (29) = happyGoto action_16
action_223 (30) = happyGoto action_17
action_223 (51) = happyGoto action_20
action_223 (52) = happyGoto action_266
action_223 (53) = happyGoto action_21
action_223 (91) = happyGoto action_23
action_223 (92) = happyGoto action_93
action_223 (95) = happyGoto action_25
action_223 (96) = happyGoto action_7
action_223 (97) = happyGoto action_26
action_223 (98) = happyGoto action_27
action_223 _ = happyFail

action_224 (140) = happyShift action_264
action_224 _ = happyFail

action_225 (105) = happyShift action_32
action_225 (106) = happyShift action_33
action_225 (107) = happyShift action_34
action_225 (108) = happyShift action_35
action_225 (118) = happyShift action_94
action_225 (121) = happyShift action_45
action_225 (123) = happyShift action_46
action_225 (124) = happyShift action_95
action_225 (127) = happyShift action_96
action_225 (128) = happyShift action_97
action_225 (131) = happyShift action_47
action_225 (132) = happyShift action_98
action_225 (133) = happyShift action_99
action_225 (134) = happyShift action_100
action_225 (135) = happyShift action_48
action_225 (142) = happyShift action_49
action_225 (145) = happyShift action_239
action_225 (147) = happyShift action_50
action_225 (149) = happyShift action_51
action_225 (151) = happyShift action_52
action_225 (152) = happyShift action_53
action_225 (153) = happyShift action_102
action_225 (154) = happyShift action_55
action_225 (157) = happyShift action_56
action_225 (163) = happyShift action_57
action_225 (164) = happyShift action_58
action_225 (165) = happyShift action_59
action_225 (166) = happyShift action_60
action_225 (167) = happyShift action_61
action_225 (168) = happyShift action_62
action_225 (169) = happyShift action_63
action_225 (170) = happyShift action_64
action_225 (171) = happyShift action_65
action_225 (172) = happyShift action_66
action_225 (176) = happyShift action_67
action_225 (177) = happyShift action_68
action_225 (178) = happyShift action_8
action_225 (179) = happyShift action_69
action_225 (180) = happyShift action_70
action_225 (181) = happyShift action_71
action_225 (182) = happyShift action_72
action_225 (183) = happyShift action_73
action_225 (184) = happyShift action_74
action_225 (18) = happyGoto action_107
action_225 (21) = happyGoto action_88
action_225 (22) = happyGoto action_89
action_225 (23) = happyGoto action_90
action_225 (24) = happyGoto action_91
action_225 (27) = happyGoto action_14
action_225 (28) = happyGoto action_15
action_225 (29) = happyGoto action_16
action_225 (30) = happyGoto action_17
action_225 (51) = happyGoto action_20
action_225 (53) = happyGoto action_21
action_225 (91) = happyGoto action_23
action_225 (92) = happyGoto action_93
action_225 (95) = happyGoto action_25
action_225 (96) = happyGoto action_7
action_225 (97) = happyGoto action_26
action_225 (98) = happyGoto action_27
action_225 _ = happyFail

action_226 (105) = happyShift action_32
action_226 (106) = happyShift action_33
action_226 (107) = happyShift action_34
action_226 (108) = happyShift action_35
action_226 (147) = happyShift action_82
action_226 (176) = happyShift action_67
action_226 (92) = happyGoto action_263
action_226 (97) = happyGoto action_26
action_226 _ = happyFail

action_227 (105) = happyShift action_32
action_227 (106) = happyShift action_33
action_227 (107) = happyShift action_34
action_227 (108) = happyShift action_35
action_227 (118) = happyShift action_94
action_227 (121) = happyShift action_45
action_227 (123) = happyShift action_46
action_227 (124) = happyShift action_95
action_227 (127) = happyShift action_96
action_227 (131) = happyShift action_47
action_227 (132) = happyShift action_98
action_227 (133) = happyShift action_99
action_227 (134) = happyShift action_100
action_227 (135) = happyShift action_48
action_227 (142) = happyShift action_49
action_227 (147) = happyShift action_50
action_227 (149) = happyShift action_51
action_227 (151) = happyShift action_52
action_227 (152) = happyShift action_53
action_227 (153) = happyShift action_102
action_227 (154) = happyShift action_55
action_227 (157) = happyShift action_56
action_227 (163) = happyShift action_57
action_227 (164) = happyShift action_58
action_227 (165) = happyShift action_59
action_227 (166) = happyShift action_60
action_227 (167) = happyShift action_61
action_227 (168) = happyShift action_62
action_227 (169) = happyShift action_63
action_227 (170) = happyShift action_64
action_227 (171) = happyShift action_65
action_227 (172) = happyShift action_66
action_227 (176) = happyShift action_67
action_227 (177) = happyShift action_68
action_227 (178) = happyShift action_8
action_227 (179) = happyShift action_69
action_227 (180) = happyShift action_70
action_227 (181) = happyShift action_71
action_227 (182) = happyShift action_72
action_227 (183) = happyShift action_73
action_227 (184) = happyShift action_74
action_227 (21) = happyGoto action_262
action_227 (22) = happyGoto action_89
action_227 (23) = happyGoto action_90
action_227 (24) = happyGoto action_91
action_227 (27) = happyGoto action_14
action_227 (28) = happyGoto action_15
action_227 (29) = happyGoto action_16
action_227 (30) = happyGoto action_17
action_227 (51) = happyGoto action_20
action_227 (53) = happyGoto action_21
action_227 (91) = happyGoto action_23
action_227 (92) = happyGoto action_93
action_227 (95) = happyGoto action_25
action_227 (96) = happyGoto action_7
action_227 (97) = happyGoto action_26
action_227 (98) = happyGoto action_27
action_227 _ = happyFail

action_228 (105) = happyShift action_32
action_228 (106) = happyShift action_33
action_228 (107) = happyShift action_34
action_228 (108) = happyShift action_35
action_228 (118) = happyShift action_94
action_228 (121) = happyShift action_45
action_228 (123) = happyShift action_46
action_228 (124) = happyShift action_95
action_228 (127) = happyShift action_96
action_228 (131) = happyShift action_47
action_228 (132) = happyShift action_98
action_228 (133) = happyShift action_99
action_228 (134) = happyShift action_100
action_228 (135) = happyShift action_48
action_228 (142) = happyShift action_49
action_228 (147) = happyShift action_50
action_228 (149) = happyShift action_51
action_228 (151) = happyShift action_52
action_228 (152) = happyShift action_53
action_228 (153) = happyShift action_102
action_228 (154) = happyShift action_55
action_228 (157) = happyShift action_56
action_228 (163) = happyShift action_57
action_228 (164) = happyShift action_58
action_228 (165) = happyShift action_59
action_228 (166) = happyShift action_60
action_228 (167) = happyShift action_61
action_228 (168) = happyShift action_62
action_228 (169) = happyShift action_63
action_228 (170) = happyShift action_64
action_228 (171) = happyShift action_65
action_228 (172) = happyShift action_66
action_228 (176) = happyShift action_67
action_228 (177) = happyShift action_68
action_228 (178) = happyShift action_8
action_228 (179) = happyShift action_69
action_228 (180) = happyShift action_70
action_228 (181) = happyShift action_71
action_228 (182) = happyShift action_72
action_228 (183) = happyShift action_73
action_228 (184) = happyShift action_74
action_228 (21) = happyGoto action_261
action_228 (22) = happyGoto action_89
action_228 (23) = happyGoto action_90
action_228 (24) = happyGoto action_91
action_228 (27) = happyGoto action_14
action_228 (28) = happyGoto action_15
action_228 (29) = happyGoto action_16
action_228 (30) = happyGoto action_17
action_228 (51) = happyGoto action_20
action_228 (53) = happyGoto action_21
action_228 (91) = happyGoto action_23
action_228 (92) = happyGoto action_93
action_228 (95) = happyGoto action_25
action_228 (96) = happyGoto action_7
action_228 (97) = happyGoto action_26
action_228 (98) = happyGoto action_27
action_228 _ = happyFail

action_229 (105) = happyShift action_32
action_229 (106) = happyShift action_33
action_229 (107) = happyShift action_34
action_229 (108) = happyShift action_35
action_229 (118) = happyShift action_94
action_229 (121) = happyShift action_45
action_229 (123) = happyShift action_46
action_229 (124) = happyShift action_95
action_229 (127) = happyShift action_96
action_229 (131) = happyShift action_47
action_229 (132) = happyShift action_98
action_229 (133) = happyShift action_99
action_229 (134) = happyShift action_100
action_229 (135) = happyShift action_48
action_229 (142) = happyShift action_49
action_229 (147) = happyShift action_50
action_229 (149) = happyShift action_51
action_229 (151) = happyShift action_52
action_229 (152) = happyShift action_53
action_229 (153) = happyShift action_102
action_229 (154) = happyShift action_55
action_229 (157) = happyShift action_56
action_229 (163) = happyShift action_57
action_229 (164) = happyShift action_58
action_229 (165) = happyShift action_59
action_229 (166) = happyShift action_60
action_229 (167) = happyShift action_61
action_229 (168) = happyShift action_62
action_229 (169) = happyShift action_63
action_229 (170) = happyShift action_64
action_229 (171) = happyShift action_65
action_229 (172) = happyShift action_66
action_229 (176) = happyShift action_67
action_229 (177) = happyShift action_68
action_229 (178) = happyShift action_8
action_229 (179) = happyShift action_69
action_229 (180) = happyShift action_70
action_229 (181) = happyShift action_71
action_229 (182) = happyShift action_72
action_229 (183) = happyShift action_73
action_229 (184) = happyShift action_74
action_229 (21) = happyGoto action_260
action_229 (22) = happyGoto action_89
action_229 (23) = happyGoto action_90
action_229 (24) = happyGoto action_91
action_229 (27) = happyGoto action_14
action_229 (28) = happyGoto action_15
action_229 (29) = happyGoto action_16
action_229 (30) = happyGoto action_17
action_229 (51) = happyGoto action_20
action_229 (53) = happyGoto action_21
action_229 (91) = happyGoto action_23
action_229 (92) = happyGoto action_93
action_229 (95) = happyGoto action_25
action_229 (96) = happyGoto action_7
action_229 (97) = happyGoto action_26
action_229 (98) = happyGoto action_27
action_229 _ = happyFail

action_230 (129) = happyShift action_259
action_230 (19) = happyGoto action_258
action_230 _ = happyReduce_43

action_231 _ = happyReduce_56

action_232 (125) = happyShift action_257
action_232 _ = happyFail

action_233 (158) = happyShift action_152
action_233 (99) = happyGoto action_255
action_233 (100) = happyGoto action_256
action_233 _ = happyReduce_281

action_234 (119) = happyShift action_254
action_234 _ = happyFail

action_235 _ = happyReduce_140

action_236 (105) = happyShift action_32
action_236 (106) = happyShift action_33
action_236 (107) = happyShift action_34
action_236 (108) = happyShift action_35
action_236 (118) = happyShift action_94
action_236 (121) = happyShift action_45
action_236 (123) = happyShift action_46
action_236 (124) = happyShift action_95
action_236 (127) = happyShift action_96
action_236 (128) = happyShift action_97
action_236 (131) = happyShift action_47
action_236 (132) = happyShift action_98
action_236 (133) = happyShift action_99
action_236 (134) = happyShift action_100
action_236 (135) = happyShift action_48
action_236 (142) = happyShift action_49
action_236 (147) = happyShift action_50
action_236 (149) = happyShift action_51
action_236 (151) = happyShift action_52
action_236 (152) = happyShift action_53
action_236 (153) = happyShift action_102
action_236 (154) = happyShift action_55
action_236 (157) = happyShift action_56
action_236 (163) = happyShift action_57
action_236 (164) = happyShift action_58
action_236 (165) = happyShift action_59
action_236 (166) = happyShift action_60
action_236 (167) = happyShift action_61
action_236 (168) = happyShift action_62
action_236 (169) = happyShift action_63
action_236 (170) = happyShift action_64
action_236 (171) = happyShift action_65
action_236 (172) = happyShift action_66
action_236 (176) = happyShift action_67
action_236 (177) = happyShift action_68
action_236 (178) = happyShift action_8
action_236 (179) = happyShift action_69
action_236 (180) = happyShift action_70
action_236 (181) = happyShift action_71
action_236 (182) = happyShift action_72
action_236 (183) = happyShift action_73
action_236 (184) = happyShift action_74
action_236 (18) = happyGoto action_252
action_236 (21) = happyGoto action_88
action_236 (22) = happyGoto action_89
action_236 (23) = happyGoto action_90
action_236 (24) = happyGoto action_91
action_236 (27) = happyGoto action_14
action_236 (28) = happyGoto action_15
action_236 (29) = happyGoto action_16
action_236 (30) = happyGoto action_17
action_236 (51) = happyGoto action_20
action_236 (53) = happyGoto action_21
action_236 (54) = happyGoto action_253
action_236 (91) = happyGoto action_23
action_236 (92) = happyGoto action_93
action_236 (95) = happyGoto action_25
action_236 (96) = happyGoto action_7
action_236 (97) = happyGoto action_26
action_236 (98) = happyGoto action_27
action_236 _ = happyFail

action_237 (105) = happyShift action_32
action_237 (106) = happyShift action_33
action_237 (107) = happyShift action_34
action_237 (108) = happyShift action_35
action_237 (118) = happyShift action_94
action_237 (121) = happyShift action_45
action_237 (123) = happyShift action_46
action_237 (124) = happyShift action_95
action_237 (127) = happyShift action_96
action_237 (128) = happyShift action_97
action_237 (131) = happyShift action_47
action_237 (132) = happyShift action_98
action_237 (133) = happyShift action_99
action_237 (134) = happyShift action_100
action_237 (135) = happyShift action_48
action_237 (142) = happyShift action_49
action_237 (147) = happyShift action_50
action_237 (149) = happyShift action_51
action_237 (151) = happyShift action_52
action_237 (152) = happyShift action_53
action_237 (153) = happyShift action_102
action_237 (154) = happyShift action_55
action_237 (157) = happyShift action_56
action_237 (163) = happyShift action_57
action_237 (164) = happyShift action_58
action_237 (165) = happyShift action_59
action_237 (166) = happyShift action_60
action_237 (167) = happyShift action_61
action_237 (168) = happyShift action_62
action_237 (169) = happyShift action_63
action_237 (170) = happyShift action_64
action_237 (171) = happyShift action_65
action_237 (172) = happyShift action_66
action_237 (176) = happyShift action_67
action_237 (177) = happyShift action_68
action_237 (178) = happyShift action_8
action_237 (179) = happyShift action_69
action_237 (180) = happyShift action_70
action_237 (181) = happyShift action_71
action_237 (182) = happyShift action_72
action_237 (183) = happyShift action_73
action_237 (184) = happyShift action_74
action_237 (18) = happyGoto action_249
action_237 (21) = happyGoto action_88
action_237 (22) = happyGoto action_89
action_237 (23) = happyGoto action_90
action_237 (24) = happyGoto action_91
action_237 (27) = happyGoto action_14
action_237 (28) = happyGoto action_15
action_237 (29) = happyGoto action_16
action_237 (30) = happyGoto action_17
action_237 (51) = happyGoto action_20
action_237 (53) = happyGoto action_21
action_237 (55) = happyGoto action_250
action_237 (56) = happyGoto action_251
action_237 (91) = happyGoto action_23
action_237 (92) = happyGoto action_93
action_237 (95) = happyGoto action_25
action_237 (96) = happyGoto action_7
action_237 (97) = happyGoto action_26
action_237 (98) = happyGoto action_27
action_237 _ = happyFail

action_238 (105) = happyShift action_32
action_238 (106) = happyShift action_33
action_238 (107) = happyShift action_34
action_238 (108) = happyShift action_35
action_238 (118) = happyShift action_94
action_238 (121) = happyShift action_45
action_238 (123) = happyShift action_46
action_238 (124) = happyShift action_95
action_238 (127) = happyShift action_96
action_238 (128) = happyShift action_97
action_238 (131) = happyShift action_47
action_238 (132) = happyShift action_98
action_238 (133) = happyShift action_99
action_238 (134) = happyShift action_100
action_238 (135) = happyShift action_48
action_238 (142) = happyShift action_49
action_238 (147) = happyShift action_50
action_238 (149) = happyShift action_51
action_238 (150) = happyShift action_248
action_238 (151) = happyShift action_52
action_238 (152) = happyShift action_53
action_238 (153) = happyShift action_102
action_238 (154) = happyShift action_55
action_238 (157) = happyShift action_56
action_238 (163) = happyShift action_57
action_238 (164) = happyShift action_58
action_238 (165) = happyShift action_59
action_238 (166) = happyShift action_60
action_238 (167) = happyShift action_61
action_238 (168) = happyShift action_62
action_238 (169) = happyShift action_63
action_238 (170) = happyShift action_64
action_238 (171) = happyShift action_65
action_238 (172) = happyShift action_66
action_238 (176) = happyShift action_67
action_238 (177) = happyShift action_68
action_238 (178) = happyShift action_8
action_238 (179) = happyShift action_69
action_238 (180) = happyShift action_70
action_238 (181) = happyShift action_71
action_238 (182) = happyShift action_72
action_238 (183) = happyShift action_73
action_238 (184) = happyShift action_74
action_238 (18) = happyGoto action_247
action_238 (21) = happyGoto action_88
action_238 (22) = happyGoto action_89
action_238 (23) = happyGoto action_90
action_238 (24) = happyGoto action_91
action_238 (27) = happyGoto action_14
action_238 (28) = happyGoto action_15
action_238 (29) = happyGoto action_16
action_238 (30) = happyGoto action_17
action_238 (51) = happyGoto action_20
action_238 (53) = happyGoto action_21
action_238 (91) = happyGoto action_23
action_238 (92) = happyGoto action_93
action_238 (95) = happyGoto action_25
action_238 (96) = happyGoto action_7
action_238 (97) = happyGoto action_26
action_238 (98) = happyGoto action_27
action_238 _ = happyFail

action_239 (105) = happyShift action_32
action_239 (106) = happyShift action_33
action_239 (107) = happyShift action_34
action_239 (108) = happyShift action_35
action_239 (121) = happyShift action_45
action_239 (123) = happyShift action_46
action_239 (131) = happyShift action_47
action_239 (135) = happyShift action_48
action_239 (142) = happyShift action_49
action_239 (147) = happyShift action_50
action_239 (149) = happyShift action_51
action_239 (151) = happyShift action_52
action_239 (152) = happyShift action_53
action_239 (153) = happyShift action_54
action_239 (154) = happyShift action_55
action_239 (157) = happyShift action_56
action_239 (163) = happyShift action_57
action_239 (164) = happyShift action_58
action_239 (165) = happyShift action_59
action_239 (166) = happyShift action_60
action_239 (167) = happyShift action_61
action_239 (168) = happyShift action_62
action_239 (169) = happyShift action_63
action_239 (170) = happyShift action_64
action_239 (171) = happyShift action_65
action_239 (172) = happyShift action_66
action_239 (176) = happyShift action_67
action_239 (177) = happyShift action_68
action_239 (178) = happyShift action_8
action_239 (179) = happyShift action_69
action_239 (180) = happyShift action_70
action_239 (181) = happyShift action_71
action_239 (182) = happyShift action_72
action_239 (183) = happyShift action_73
action_239 (184) = happyShift action_74
action_239 (23) = happyGoto action_169
action_239 (24) = happyGoto action_91
action_239 (27) = happyGoto action_14
action_239 (28) = happyGoto action_15
action_239 (29) = happyGoto action_16
action_239 (30) = happyGoto action_17
action_239 (39) = happyGoto action_244
action_239 (40) = happyGoto action_245
action_239 (48) = happyGoto action_246
action_239 (51) = happyGoto action_20
action_239 (53) = happyGoto action_21
action_239 (91) = happyGoto action_23
action_239 (92) = happyGoto action_93
action_239 (95) = happyGoto action_25
action_239 (96) = happyGoto action_166
action_239 (97) = happyGoto action_26
action_239 (98) = happyGoto action_27
action_239 _ = happyFail

action_240 _ = happyReduce_69

action_241 _ = happyReduce_68

action_242 (148) = happyShift action_221
action_242 _ = happyFail

action_243 _ = happyReduce_252

action_244 (146) = happyShift action_376
action_244 _ = happyFail

action_245 (158) = happyShift action_152
action_245 (99) = happyGoto action_374
action_245 (100) = happyGoto action_375
action_245 _ = happyReduce_281

action_246 (140) = happyShift action_373
action_246 _ = happyFail

action_247 (150) = happyShift action_372
action_247 _ = happyFail

action_248 _ = happyReduce_142

action_249 (174) = happyShift action_370
action_249 (175) = happyShift action_371
action_249 _ = happyReduce_150

action_250 (150) = happyShift action_369
action_250 _ = happyFail

action_251 (156) = happyShift action_368
action_251 _ = happyReduce_146

action_252 (156) = happyShift action_236
action_252 _ = happyReduce_144

action_253 _ = happyReduce_145

action_254 (105) = happyShift action_32
action_254 (106) = happyShift action_33
action_254 (107) = happyShift action_34
action_254 (108) = happyShift action_35
action_254 (121) = happyShift action_45
action_254 (123) = happyShift action_46
action_254 (131) = happyShift action_47
action_254 (135) = happyShift action_48
action_254 (142) = happyShift action_49
action_254 (147) = happyShift action_50
action_254 (149) = happyShift action_51
action_254 (151) = happyShift action_52
action_254 (152) = happyShift action_53
action_254 (153) = happyShift action_54
action_254 (154) = happyShift action_55
action_254 (157) = happyShift action_56
action_254 (163) = happyShift action_57
action_254 (164) = happyShift action_58
action_254 (165) = happyShift action_59
action_254 (166) = happyShift action_60
action_254 (167) = happyShift action_61
action_254 (168) = happyShift action_62
action_254 (169) = happyShift action_63
action_254 (170) = happyShift action_64
action_254 (171) = happyShift action_65
action_254 (172) = happyShift action_66
action_254 (176) = happyShift action_67
action_254 (177) = happyShift action_68
action_254 (178) = happyShift action_8
action_254 (179) = happyShift action_69
action_254 (180) = happyShift action_70
action_254 (181) = happyShift action_71
action_254 (182) = happyShift action_72
action_254 (183) = happyShift action_73
action_254 (184) = happyShift action_74
action_254 (23) = happyGoto action_367
action_254 (24) = happyGoto action_91
action_254 (27) = happyGoto action_14
action_254 (28) = happyGoto action_15
action_254 (29) = happyGoto action_16
action_254 (30) = happyGoto action_17
action_254 (51) = happyGoto action_20
action_254 (53) = happyGoto action_21
action_254 (91) = happyGoto action_23
action_254 (92) = happyGoto action_93
action_254 (95) = happyGoto action_25
action_254 (96) = happyGoto action_7
action_254 (97) = happyGoto action_26
action_254 (98) = happyGoto action_27
action_254 _ = happyFail

action_255 (105) = happyShift action_32
action_255 (106) = happyShift action_33
action_255 (107) = happyShift action_34
action_255 (108) = happyShift action_35
action_255 (121) = happyShift action_45
action_255 (123) = happyShift action_46
action_255 (131) = happyShift action_47
action_255 (135) = happyShift action_48
action_255 (142) = happyShift action_49
action_255 (147) = happyShift action_50
action_255 (149) = happyShift action_51
action_255 (151) = happyShift action_52
action_255 (152) = happyShift action_53
action_255 (153) = happyShift action_54
action_255 (154) = happyShift action_55
action_255 (157) = happyShift action_56
action_255 (163) = happyShift action_57
action_255 (164) = happyShift action_58
action_255 (165) = happyShift action_59
action_255 (166) = happyShift action_60
action_255 (167) = happyShift action_61
action_255 (168) = happyShift action_62
action_255 (169) = happyShift action_63
action_255 (170) = happyShift action_64
action_255 (171) = happyShift action_65
action_255 (172) = happyShift action_66
action_255 (176) = happyShift action_67
action_255 (177) = happyShift action_68
action_255 (178) = happyShift action_8
action_255 (179) = happyShift action_69
action_255 (180) = happyShift action_70
action_255 (181) = happyShift action_71
action_255 (182) = happyShift action_72
action_255 (183) = happyShift action_73
action_255 (184) = happyShift action_74
action_255 (24) = happyGoto action_13
action_255 (27) = happyGoto action_14
action_255 (28) = happyGoto action_15
action_255 (29) = happyGoto action_16
action_255 (30) = happyGoto action_17
action_255 (32) = happyGoto action_18
action_255 (35) = happyGoto action_233
action_255 (36) = happyGoto action_366
action_255 (51) = happyGoto action_20
action_255 (53) = happyGoto action_21
action_255 (91) = happyGoto action_23
action_255 (92) = happyGoto action_24
action_255 (95) = happyGoto action_25
action_255 (96) = happyGoto action_7
action_255 (97) = happyGoto action_26
action_255 (98) = happyGoto action_27
action_255 _ = happyReduce_282

action_256 _ = happyReduce_105

action_257 (105) = happyShift action_32
action_257 (106) = happyShift action_33
action_257 (107) = happyShift action_34
action_257 (108) = happyShift action_35
action_257 (118) = happyShift action_94
action_257 (121) = happyShift action_45
action_257 (123) = happyShift action_46
action_257 (124) = happyShift action_95
action_257 (127) = happyShift action_96
action_257 (128) = happyShift action_97
action_257 (131) = happyShift action_47
action_257 (132) = happyShift action_98
action_257 (133) = happyShift action_99
action_257 (134) = happyShift action_100
action_257 (135) = happyShift action_48
action_257 (142) = happyShift action_49
action_257 (147) = happyShift action_50
action_257 (149) = happyShift action_51
action_257 (151) = happyShift action_52
action_257 (152) = happyShift action_53
action_257 (153) = happyShift action_102
action_257 (154) = happyShift action_55
action_257 (157) = happyShift action_56
action_257 (163) = happyShift action_57
action_257 (164) = happyShift action_58
action_257 (165) = happyShift action_59
action_257 (166) = happyShift action_60
action_257 (167) = happyShift action_61
action_257 (168) = happyShift action_62
action_257 (169) = happyShift action_63
action_257 (170) = happyShift action_64
action_257 (171) = happyShift action_65
action_257 (172) = happyShift action_66
action_257 (176) = happyShift action_67
action_257 (177) = happyShift action_68
action_257 (178) = happyShift action_8
action_257 (179) = happyShift action_69
action_257 (180) = happyShift action_70
action_257 (181) = happyShift action_71
action_257 (182) = happyShift action_72
action_257 (183) = happyShift action_73
action_257 (184) = happyShift action_74
action_257 (18) = happyGoto action_365
action_257 (21) = happyGoto action_88
action_257 (22) = happyGoto action_89
action_257 (23) = happyGoto action_90
action_257 (24) = happyGoto action_91
action_257 (27) = happyGoto action_14
action_257 (28) = happyGoto action_15
action_257 (29) = happyGoto action_16
action_257 (30) = happyGoto action_17
action_257 (51) = happyGoto action_20
action_257 (53) = happyGoto action_21
action_257 (91) = happyGoto action_23
action_257 (92) = happyGoto action_93
action_257 (95) = happyGoto action_25
action_257 (96) = happyGoto action_7
action_257 (97) = happyGoto action_26
action_257 (98) = happyGoto action_27
action_257 _ = happyFail

action_258 (130) = happyShift action_364
action_258 (20) = happyGoto action_363
action_258 _ = happyReduce_45

action_259 (145) = happyShift action_362
action_259 _ = happyFail

action_260 _ = happyReduce_53

action_261 _ = happyReduce_51

action_262 _ = happyReduce_52

action_263 (105) = happyShift action_32
action_263 (106) = happyShift action_33
action_263 (107) = happyShift action_34
action_263 (108) = happyShift action_35
action_263 (121) = happyShift action_45
action_263 (123) = happyShift action_46
action_263 (131) = happyShift action_47
action_263 (135) = happyShift action_48
action_263 (142) = happyShift action_49
action_263 (147) = happyShift action_50
action_263 (149) = happyShift action_51
action_263 (153) = happyShift action_54
action_263 (154) = happyShift action_55
action_263 (171) = happyShift action_65
action_263 (172) = happyShift action_66
action_263 (176) = happyShift action_67
action_263 (177) = happyShift action_68
action_263 (178) = happyShift action_8
action_263 (180) = happyShift action_70
action_263 (181) = happyShift action_71
action_263 (182) = happyShift action_72
action_263 (183) = happyShift action_73
action_263 (184) = happyShift action_74
action_263 (26) = happyGoto action_360
action_263 (27) = happyGoto action_361
action_263 (28) = happyGoto action_15
action_263 (29) = happyGoto action_16
action_263 (30) = happyGoto action_17
action_263 (51) = happyGoto action_20
action_263 (53) = happyGoto action_21
action_263 (91) = happyGoto action_23
action_263 (92) = happyGoto action_93
action_263 (95) = happyGoto action_25
action_263 (96) = happyGoto action_7
action_263 (97) = happyGoto action_26
action_263 _ = happyReduce_65

action_264 (105) = happyShift action_32
action_264 (106) = happyShift action_33
action_264 (107) = happyShift action_34
action_264 (108) = happyShift action_35
action_264 (118) = happyShift action_94
action_264 (121) = happyShift action_45
action_264 (123) = happyShift action_46
action_264 (124) = happyShift action_95
action_264 (127) = happyShift action_96
action_264 (131) = happyShift action_47
action_264 (132) = happyShift action_98
action_264 (133) = happyShift action_99
action_264 (134) = happyShift action_100
action_264 (135) = happyShift action_48
action_264 (142) = happyShift action_49
action_264 (147) = happyShift action_50
action_264 (149) = happyShift action_51
action_264 (151) = happyShift action_52
action_264 (152) = happyShift action_53
action_264 (153) = happyShift action_102
action_264 (154) = happyShift action_55
action_264 (157) = happyShift action_56
action_264 (163) = happyShift action_57
action_264 (164) = happyShift action_58
action_264 (165) = happyShift action_59
action_264 (166) = happyShift action_60
action_264 (167) = happyShift action_61
action_264 (168) = happyShift action_62
action_264 (169) = happyShift action_63
action_264 (170) = happyShift action_64
action_264 (171) = happyShift action_65
action_264 (172) = happyShift action_66
action_264 (176) = happyShift action_67
action_264 (177) = happyShift action_68
action_264 (178) = happyShift action_8
action_264 (179) = happyShift action_69
action_264 (180) = happyShift action_70
action_264 (181) = happyShift action_71
action_264 (182) = happyShift action_72
action_264 (183) = happyShift action_73
action_264 (184) = happyShift action_74
action_264 (21) = happyGoto action_359
action_264 (22) = happyGoto action_89
action_264 (23) = happyGoto action_90
action_264 (24) = happyGoto action_91
action_264 (27) = happyGoto action_14
action_264 (28) = happyGoto action_15
action_264 (29) = happyGoto action_16
action_264 (30) = happyGoto action_17
action_264 (51) = happyGoto action_20
action_264 (53) = happyGoto action_21
action_264 (91) = happyGoto action_23
action_264 (92) = happyGoto action_93
action_264 (95) = happyGoto action_25
action_264 (96) = happyGoto action_7
action_264 (97) = happyGoto action_26
action_264 (98) = happyGoto action_27
action_264 _ = happyFail

action_265 (156) = happyShift action_358
action_265 _ = happyReduce_137

action_266 (148) = happyShift action_357
action_266 _ = happyFail

action_267 _ = happyReduce_74

action_268 (105) = happyShift action_32
action_268 (106) = happyShift action_33
action_268 (107) = happyShift action_34
action_268 (108) = happyShift action_35
action_268 (118) = happyShift action_94
action_268 (121) = happyShift action_45
action_268 (123) = happyShift action_46
action_268 (124) = happyShift action_95
action_268 (127) = happyShift action_96
action_268 (128) = happyShift action_97
action_268 (131) = happyShift action_47
action_268 (132) = happyShift action_98
action_268 (133) = happyShift action_99
action_268 (134) = happyShift action_100
action_268 (135) = happyShift action_48
action_268 (142) = happyShift action_49
action_268 (147) = happyShift action_50
action_268 (149) = happyShift action_51
action_268 (151) = happyShift action_52
action_268 (152) = happyShift action_53
action_268 (153) = happyShift action_102
action_268 (154) = happyShift action_55
action_268 (157) = happyShift action_56
action_268 (163) = happyShift action_57
action_268 (164) = happyShift action_58
action_268 (165) = happyShift action_59
action_268 (166) = happyShift action_60
action_268 (167) = happyShift action_61
action_268 (168) = happyShift action_62
action_268 (169) = happyShift action_63
action_268 (170) = happyShift action_64
action_268 (171) = happyShift action_65
action_268 (172) = happyShift action_66
action_268 (176) = happyShift action_67
action_268 (177) = happyShift action_68
action_268 (178) = happyShift action_8
action_268 (179) = happyShift action_69
action_268 (180) = happyShift action_70
action_268 (181) = happyShift action_71
action_268 (182) = happyShift action_72
action_268 (183) = happyShift action_73
action_268 (184) = happyShift action_74
action_268 (18) = happyGoto action_216
action_268 (21) = happyGoto action_88
action_268 (22) = happyGoto action_89
action_268 (23) = happyGoto action_90
action_268 (24) = happyGoto action_217
action_268 (27) = happyGoto action_14
action_268 (28) = happyGoto action_15
action_268 (29) = happyGoto action_16
action_268 (30) = happyGoto action_17
action_268 (32) = happyGoto action_18
action_268 (35) = happyGoto action_218
action_268 (37) = happyGoto action_219
action_268 (38) = happyGoto action_356
action_268 (51) = happyGoto action_20
action_268 (53) = happyGoto action_21
action_268 (91) = happyGoto action_23
action_268 (92) = happyGoto action_24
action_268 (95) = happyGoto action_25
action_268 (96) = happyGoto action_7
action_268 (97) = happyGoto action_26
action_268 (98) = happyGoto action_27
action_268 _ = happyReduce_282

action_269 _ = happyReduce_109

action_270 (143) = happyShift action_145
action_270 (144) = happyShift action_146
action_270 (159) = happyShift action_148
action_270 (42) = happyGoto action_355
action_270 (43) = happyGoto action_215
action_270 (44) = happyGoto action_143
action_270 (45) = happyGoto action_144
action_270 _ = happyReduce_282

action_271 _ = happyReduce_116

action_272 _ = happyReduce_76

action_273 (105) = happyShift action_32
action_273 (106) = happyShift action_33
action_273 (107) = happyShift action_34
action_273 (108) = happyShift action_35
action_273 (121) = happyShift action_45
action_273 (123) = happyShift action_46
action_273 (131) = happyShift action_47
action_273 (135) = happyShift action_48
action_273 (142) = happyShift action_49
action_273 (147) = happyShift action_50
action_273 (149) = happyShift action_51
action_273 (151) = happyShift action_52
action_273 (152) = happyShift action_53
action_273 (153) = happyShift action_54
action_273 (154) = happyShift action_55
action_273 (157) = happyShift action_56
action_273 (163) = happyShift action_57
action_273 (164) = happyShift action_58
action_273 (165) = happyShift action_59
action_273 (166) = happyShift action_60
action_273 (167) = happyShift action_61
action_273 (168) = happyShift action_62
action_273 (169) = happyShift action_63
action_273 (170) = happyShift action_64
action_273 (171) = happyShift action_65
action_273 (172) = happyShift action_66
action_273 (176) = happyShift action_67
action_273 (177) = happyShift action_68
action_273 (178) = happyShift action_8
action_273 (179) = happyShift action_69
action_273 (180) = happyShift action_70
action_273 (181) = happyShift action_71
action_273 (182) = happyShift action_72
action_273 (183) = happyShift action_73
action_273 (184) = happyShift action_74
action_273 (23) = happyGoto action_169
action_273 (24) = happyGoto action_91
action_273 (27) = happyGoto action_14
action_273 (28) = happyGoto action_15
action_273 (29) = happyGoto action_16
action_273 (30) = happyGoto action_17
action_273 (39) = happyGoto action_354
action_273 (40) = happyGoto action_245
action_273 (48) = happyGoto action_246
action_273 (51) = happyGoto action_20
action_273 (53) = happyGoto action_21
action_273 (91) = happyGoto action_23
action_273 (92) = happyGoto action_93
action_273 (95) = happyGoto action_25
action_273 (96) = happyGoto action_166
action_273 (97) = happyGoto action_26
action_273 (98) = happyGoto action_27
action_273 _ = happyFail

action_274 (105) = happyShift action_32
action_274 (106) = happyShift action_33
action_274 (107) = happyShift action_34
action_274 (108) = happyShift action_35
action_274 (121) = happyShift action_45
action_274 (123) = happyShift action_46
action_274 (131) = happyShift action_47
action_274 (135) = happyShift action_48
action_274 (142) = happyShift action_49
action_274 (147) = happyShift action_50
action_274 (149) = happyShift action_51
action_274 (151) = happyShift action_52
action_274 (152) = happyShift action_53
action_274 (153) = happyShift action_54
action_274 (154) = happyShift action_55
action_274 (157) = happyShift action_56
action_274 (163) = happyShift action_57
action_274 (164) = happyShift action_58
action_274 (165) = happyShift action_59
action_274 (166) = happyShift action_60
action_274 (167) = happyShift action_61
action_274 (168) = happyShift action_62
action_274 (169) = happyShift action_63
action_274 (170) = happyShift action_64
action_274 (171) = happyShift action_65
action_274 (172) = happyShift action_66
action_274 (176) = happyShift action_67
action_274 (177) = happyShift action_68
action_274 (178) = happyShift action_8
action_274 (179) = happyShift action_69
action_274 (180) = happyShift action_70
action_274 (181) = happyShift action_71
action_274 (182) = happyShift action_72
action_274 (183) = happyShift action_73
action_274 (184) = happyShift action_74
action_274 (24) = happyGoto action_13
action_274 (27) = happyGoto action_14
action_274 (28) = happyGoto action_15
action_274 (29) = happyGoto action_16
action_274 (30) = happyGoto action_17
action_274 (32) = happyGoto action_18
action_274 (35) = happyGoto action_233
action_274 (36) = happyGoto action_353
action_274 (51) = happyGoto action_20
action_274 (53) = happyGoto action_21
action_274 (91) = happyGoto action_23
action_274 (92) = happyGoto action_24
action_274 (95) = happyGoto action_25
action_274 (96) = happyGoto action_7
action_274 (97) = happyGoto action_26
action_274 (98) = happyGoto action_27
action_274 _ = happyFail

action_275 _ = happyReduce_188

action_276 (148) = happyShift action_352
action_276 _ = happyFail

action_277 _ = happyReduce_193

action_278 (105) = happyShift action_32
action_278 (106) = happyShift action_33
action_278 (107) = happyShift action_34
action_278 (108) = happyShift action_35
action_278 (142) = happyShift action_113
action_278 (147) = happyShift action_114
action_278 (149) = happyShift action_115
action_278 (163) = happyShift action_116
action_278 (165) = happyShift action_117
action_278 (166) = happyShift action_118
action_278 (168) = happyShift action_119
action_278 (176) = happyShift action_67
action_278 (178) = happyShift action_8
action_278 (180) = happyShift action_9
action_278 (66) = happyGoto action_108
action_278 (70) = happyGoto action_350
action_278 (71) = happyGoto action_185
action_278 (72) = happyGoto action_110
action_278 (74) = happyGoto action_351
action_278 (95) = happyGoto action_111
action_278 (96) = happyGoto action_7
action_278 (97) = happyGoto action_112
action_278 _ = happyFail

action_279 _ = happyReduce_196

action_280 _ = happyReduce_191

action_281 (145) = happyShift action_349
action_281 _ = happyFail

action_282 _ = happyReduce_199

action_283 _ = happyReduce_16

action_284 (140) = happyShift action_348
action_284 _ = happyReduce_170

action_285 (145) = happyShift action_347
action_285 _ = happyFail

action_286 _ = happyReduce_14

action_287 (178) = happyShift action_8
action_287 (58) = happyGoto action_344
action_287 (59) = happyGoto action_345
action_287 (96) = happyGoto action_346
action_287 _ = happyFail

action_288 _ = happyReduce_256

action_289 (102) = happyShift action_343
action_289 _ = happyReduce_153

action_290 (105) = happyShift action_32
action_290 (106) = happyShift action_33
action_290 (107) = happyShift action_34
action_290 (108) = happyShift action_35
action_290 (147) = happyShift action_82
action_290 (176) = happyShift action_67
action_290 (14) = happyGoto action_290
action_290 (15) = happyGoto action_342
action_290 (92) = happyGoto action_195
action_290 (97) = happyGoto action_26
action_290 _ = happyReduce_36

action_291 (146) = happyShift action_341
action_291 _ = happyFail

action_292 (105) = happyShift action_186
action_292 (106) = happyShift action_33
action_292 (107) = happyShift action_34
action_292 (108) = happyShift action_35
action_292 (136) = happyShift action_187
action_292 (142) = happyShift action_113
action_292 (147) = happyShift action_114
action_292 (149) = happyShift action_115
action_292 (163) = happyShift action_116
action_292 (165) = happyShift action_117
action_292 (166) = happyShift action_118
action_292 (168) = happyShift action_119
action_292 (176) = happyShift action_67
action_292 (178) = happyShift action_8
action_292 (180) = happyShift action_9
action_292 (66) = happyGoto action_108
action_292 (67) = happyGoto action_340
action_292 (68) = happyGoto action_182
action_292 (69) = happyGoto action_183
action_292 (70) = happyGoto action_184
action_292 (71) = happyGoto action_185
action_292 (72) = happyGoto action_110
action_292 (95) = happyGoto action_111
action_292 (96) = happyGoto action_7
action_292 (97) = happyGoto action_112
action_292 _ = happyFail

action_293 _ = happyReduce_10

action_294 (178) = happyShift action_8
action_294 (180) = happyShift action_9
action_294 (10) = happyGoto action_192
action_294 (11) = happyGoto action_339
action_294 (95) = happyGoto action_6
action_294 (96) = happyGoto action_7
action_294 _ = happyReduce_27

action_295 (105) = happyShift action_32
action_295 (106) = happyShift action_33
action_295 (107) = happyShift action_34
action_295 (108) = happyShift action_35
action_295 (147) = happyShift action_82
action_295 (176) = happyShift action_67
action_295 (92) = happyGoto action_338
action_295 (97) = happyGoto action_26
action_295 _ = happyFail

action_296 _ = happyReduce_25

action_297 (160) = happyShift action_337
action_297 _ = happyFail

action_298 (105) = happyShift action_32
action_298 (106) = happyShift action_33
action_298 (107) = happyShift action_34
action_298 (108) = happyShift action_35
action_298 (147) = happyShift action_300
action_298 (176) = happyShift action_67
action_298 (75) = happyGoto action_336
action_298 (76) = happyGoto action_298
action_298 (92) = happyGoto action_299
action_298 (97) = happyGoto action_26
action_298 _ = happyReduce_202

action_299 _ = happyReduce_204

action_300 (105) = happyShift action_32
action_300 (106) = happyShift action_33
action_300 (107) = happyShift action_34
action_300 (108) = happyShift action_35
action_300 (147) = happyShift action_82
action_300 (151) = happyShift action_52
action_300 (152) = happyShift action_53
action_300 (157) = happyShift action_56
action_300 (163) = happyShift action_57
action_300 (164) = happyShift action_58
action_300 (165) = happyShift action_59
action_300 (166) = happyShift action_60
action_300 (167) = happyShift action_61
action_300 (168) = happyShift action_62
action_300 (169) = happyShift action_63
action_300 (170) = happyShift action_64
action_300 (176) = happyShift action_67
action_300 (179) = happyShift action_69
action_300 (92) = happyGoto action_335
action_300 (97) = happyGoto action_26
action_300 (98) = happyGoto action_242
action_300 _ = happyFail

action_301 _ = happyReduce_177

action_302 (105) = happyShift action_32
action_302 (106) = happyShift action_33
action_302 (107) = happyShift action_34
action_302 (108) = happyShift action_35
action_302 (142) = happyShift action_113
action_302 (147) = happyShift action_114
action_302 (149) = happyShift action_115
action_302 (163) = happyShift action_116
action_302 (165) = happyShift action_117
action_302 (166) = happyShift action_118
action_302 (168) = happyShift action_119
action_302 (176) = happyShift action_67
action_302 (178) = happyShift action_8
action_302 (180) = happyShift action_9
action_302 (66) = happyGoto action_108
action_302 (70) = happyGoto action_334
action_302 (71) = happyGoto action_185
action_302 (72) = happyGoto action_110
action_302 (95) = happyGoto action_111
action_302 (96) = happyGoto action_7
action_302 (97) = happyGoto action_112
action_302 _ = happyFail

action_303 (147) = happyShift action_333
action_303 _ = happyFail

action_304 (105) = happyShift action_32
action_304 (106) = happyShift action_33
action_304 (107) = happyShift action_34
action_304 (108) = happyShift action_35
action_304 (147) = happyShift action_82
action_304 (176) = happyShift action_67
action_304 (178) = happyShift action_8
action_304 (180) = happyShift action_9
action_304 (77) = happyGoto action_329
action_304 (78) = happyGoto action_330
action_304 (92) = happyGoto action_331
action_304 (95) = happyGoto action_332
action_304 (96) = happyGoto action_7
action_304 (97) = happyGoto action_26
action_304 _ = happyFail

action_305 (146) = happyShift action_328
action_305 _ = happyFail

action_306 (148) = happyShift action_327
action_306 _ = happyFail

action_307 (148) = happyShift action_326
action_307 _ = happyFail

action_308 _ = happyReduce_118

action_309 _ = happyReduce_128

action_310 (174) = happyShift action_325
action_310 _ = happyFail

action_311 _ = happyReduce_126

action_312 (146) = happyShift action_323
action_312 (160) = happyShift action_324
action_312 (49) = happyGoto action_321
action_312 (50) = happyGoto action_322
action_312 _ = happyFail

action_313 (105) = happyShift action_32
action_313 (106) = happyShift action_33
action_313 (107) = happyShift action_34
action_313 (108) = happyShift action_35
action_313 (118) = happyShift action_94
action_313 (121) = happyShift action_45
action_313 (123) = happyShift action_46
action_313 (124) = happyShift action_95
action_313 (127) = happyShift action_96
action_313 (128) = happyShift action_97
action_313 (131) = happyShift action_47
action_313 (132) = happyShift action_98
action_313 (133) = happyShift action_99
action_313 (134) = happyShift action_100
action_313 (135) = happyShift action_48
action_313 (142) = happyShift action_49
action_313 (147) = happyShift action_50
action_313 (149) = happyShift action_51
action_313 (151) = happyShift action_52
action_313 (152) = happyShift action_53
action_313 (153) = happyShift action_102
action_313 (154) = happyShift action_55
action_313 (157) = happyShift action_56
action_313 (163) = happyShift action_57
action_313 (164) = happyShift action_58
action_313 (165) = happyShift action_59
action_313 (166) = happyShift action_60
action_313 (167) = happyShift action_61
action_313 (168) = happyShift action_62
action_313 (169) = happyShift action_63
action_313 (170) = happyShift action_64
action_313 (171) = happyShift action_65
action_313 (172) = happyShift action_66
action_313 (176) = happyShift action_67
action_313 (177) = happyShift action_68
action_313 (178) = happyShift action_8
action_313 (179) = happyShift action_69
action_313 (180) = happyShift action_70
action_313 (181) = happyShift action_71
action_313 (182) = happyShift action_72
action_313 (183) = happyShift action_73
action_313 (184) = happyShift action_74
action_313 (18) = happyGoto action_162
action_313 (21) = happyGoto action_88
action_313 (22) = happyGoto action_89
action_313 (23) = happyGoto action_90
action_313 (24) = happyGoto action_91
action_313 (27) = happyGoto action_14
action_313 (28) = happyGoto action_15
action_313 (29) = happyGoto action_16
action_313 (30) = happyGoto action_17
action_313 (34) = happyGoto action_320
action_313 (51) = happyGoto action_20
action_313 (53) = happyGoto action_21
action_313 (91) = happyGoto action_23
action_313 (92) = happyGoto action_93
action_313 (95) = happyGoto action_25
action_313 (96) = happyGoto action_7
action_313 (97) = happyGoto action_26
action_313 (98) = happyGoto action_27
action_313 _ = happyFail

action_314 (145) = happyShift action_319
action_314 _ = happyFail

action_315 (151) = happyShift action_52
action_315 (152) = happyShift action_53
action_315 (157) = happyShift action_56
action_315 (163) = happyShift action_57
action_315 (164) = happyShift action_58
action_315 (165) = happyShift action_59
action_315 (166) = happyShift action_60
action_315 (167) = happyShift action_61
action_315 (168) = happyShift action_62
action_315 (169) = happyShift action_63
action_315 (170) = happyShift action_64
action_315 (179) = happyShift action_69
action_315 (13) = happyGoto action_318
action_315 (98) = happyGoto action_161
action_315 _ = happyFail

action_316 (146) = happyShift action_317
action_316 _ = happyFail

action_317 _ = happyReduce_2

action_318 _ = happyReduce_34

action_319 (105) = happyShift action_32
action_319 (106) = happyShift action_33
action_319 (107) = happyShift action_34
action_319 (108) = happyShift action_35
action_319 (121) = happyShift action_45
action_319 (123) = happyShift action_46
action_319 (131) = happyShift action_47
action_319 (135) = happyShift action_48
action_319 (142) = happyShift action_49
action_319 (147) = happyShift action_50
action_319 (149) = happyShift action_51
action_319 (151) = happyShift action_52
action_319 (152) = happyShift action_53
action_319 (153) = happyShift action_54
action_319 (154) = happyShift action_55
action_319 (157) = happyShift action_56
action_319 (163) = happyShift action_57
action_319 (164) = happyShift action_58
action_319 (165) = happyShift action_59
action_319 (166) = happyShift action_60
action_319 (167) = happyShift action_61
action_319 (168) = happyShift action_62
action_319 (169) = happyShift action_63
action_319 (170) = happyShift action_64
action_319 (171) = happyShift action_65
action_319 (172) = happyShift action_66
action_319 (176) = happyShift action_67
action_319 (177) = happyShift action_68
action_319 (178) = happyShift action_8
action_319 (179) = happyShift action_69
action_319 (180) = happyShift action_70
action_319 (181) = happyShift action_71
action_319 (182) = happyShift action_72
action_319 (183) = happyShift action_73
action_319 (184) = happyShift action_74
action_319 (24) = happyGoto action_13
action_319 (27) = happyGoto action_14
action_319 (28) = happyGoto action_15
action_319 (29) = happyGoto action_16
action_319 (30) = happyGoto action_17
action_319 (32) = happyGoto action_18
action_319 (35) = happyGoto action_233
action_319 (36) = happyGoto action_430
action_319 (51) = happyGoto action_20
action_319 (53) = happyGoto action_21
action_319 (91) = happyGoto action_23
action_319 (92) = happyGoto action_24
action_319 (95) = happyGoto action_25
action_319 (96) = happyGoto action_7
action_319 (97) = happyGoto action_26
action_319 (98) = happyGoto action_27
action_319 _ = happyFail

action_320 _ = happyReduce_122

action_321 (146) = happyShift action_429
action_321 _ = happyFail

action_322 (156) = happyShift action_428
action_322 _ = happyReduce_132

action_323 _ = happyReduce_130

action_324 (105) = happyShift action_32
action_324 (106) = happyShift action_33
action_324 (107) = happyShift action_34
action_324 (108) = happyShift action_35
action_324 (147) = happyShift action_82
action_324 (176) = happyShift action_67
action_324 (181) = happyShift action_427
action_324 (92) = happyGoto action_426
action_324 (97) = happyGoto action_26
action_324 _ = happyFail

action_325 (105) = happyShift action_32
action_325 (106) = happyShift action_33
action_325 (107) = happyShift action_34
action_325 (108) = happyShift action_35
action_325 (118) = happyShift action_94
action_325 (121) = happyShift action_45
action_325 (123) = happyShift action_46
action_325 (124) = happyShift action_95
action_325 (127) = happyShift action_96
action_325 (128) = happyShift action_97
action_325 (131) = happyShift action_47
action_325 (132) = happyShift action_98
action_325 (133) = happyShift action_99
action_325 (134) = happyShift action_100
action_325 (135) = happyShift action_48
action_325 (142) = happyShift action_49
action_325 (147) = happyShift action_50
action_325 (149) = happyShift action_51
action_325 (151) = happyShift action_52
action_325 (152) = happyShift action_53
action_325 (153) = happyShift action_102
action_325 (154) = happyShift action_55
action_325 (157) = happyShift action_56
action_325 (163) = happyShift action_57
action_325 (164) = happyShift action_58
action_325 (165) = happyShift action_59
action_325 (166) = happyShift action_60
action_325 (167) = happyShift action_61
action_325 (168) = happyShift action_62
action_325 (169) = happyShift action_63
action_325 (170) = happyShift action_64
action_325 (171) = happyShift action_65
action_325 (172) = happyShift action_66
action_325 (176) = happyShift action_67
action_325 (177) = happyShift action_68
action_325 (178) = happyShift action_8
action_325 (179) = happyShift action_69
action_325 (180) = happyShift action_70
action_325 (181) = happyShift action_71
action_325 (182) = happyShift action_72
action_325 (183) = happyShift action_73
action_325 (184) = happyShift action_74
action_325 (18) = happyGoto action_162
action_325 (21) = happyGoto action_88
action_325 (22) = happyGoto action_89
action_325 (23) = happyGoto action_90
action_325 (24) = happyGoto action_91
action_325 (27) = happyGoto action_14
action_325 (28) = happyGoto action_15
action_325 (29) = happyGoto action_16
action_325 (30) = happyGoto action_17
action_325 (34) = happyGoto action_425
action_325 (51) = happyGoto action_20
action_325 (53) = happyGoto action_21
action_325 (91) = happyGoto action_23
action_325 (92) = happyGoto action_93
action_325 (95) = happyGoto action_25
action_325 (96) = happyGoto action_7
action_325 (97) = happyGoto action_26
action_325 (98) = happyGoto action_27
action_325 _ = happyFail

action_326 _ = happyReduce_84

action_327 _ = happyReduce_83

action_328 _ = happyReduce_82

action_329 _ = happyReduce_181

action_330 (156) = happyShift action_424
action_330 _ = happyReduce_206

action_331 (155) = happyShift action_423
action_331 _ = happyFail

action_332 (105) = happyShift action_32
action_332 (106) = happyShift action_33
action_332 (107) = happyShift action_34
action_332 (108) = happyShift action_35
action_332 (147) = happyShift action_420
action_332 (168) = happyShift action_421
action_332 (170) = happyShift action_422
action_332 (176) = happyShift action_67
action_332 (88) = happyGoto action_417
action_332 (90) = happyGoto action_418
action_332 (92) = happyGoto action_419
action_332 (97) = happyGoto action_26
action_332 _ = happyFail

action_333 (105) = happyShift action_32
action_333 (106) = happyShift action_33
action_333 (107) = happyShift action_34
action_333 (108) = happyShift action_35
action_333 (147) = happyShift action_414
action_333 (168) = happyShift action_415
action_333 (170) = happyShift action_416
action_333 (176) = happyShift action_67
action_333 (178) = happyShift action_8
action_333 (180) = happyShift action_70
action_333 (81) = happyGoto action_408
action_333 (82) = happyGoto action_409
action_333 (87) = happyGoto action_410
action_333 (91) = happyGoto action_411
action_333 (92) = happyGoto action_412
action_333 (95) = happyGoto action_413
action_333 (96) = happyGoto action_7
action_333 (97) = happyGoto action_26
action_333 _ = happyFail

action_334 _ = happyReduce_183

action_335 (137) = happyShift action_407
action_335 _ = happyFail

action_336 _ = happyReduce_203

action_337 (105) = happyShift action_32
action_337 (106) = happyShift action_33
action_337 (107) = happyShift action_34
action_337 (108) = happyShift action_35
action_337 (142) = happyShift action_113
action_337 (147) = happyShift action_114
action_337 (149) = happyShift action_115
action_337 (163) = happyShift action_116
action_337 (165) = happyShift action_117
action_337 (166) = happyShift action_118
action_337 (168) = happyShift action_119
action_337 (176) = happyShift action_67
action_337 (178) = happyShift action_8
action_337 (180) = happyShift action_9
action_337 (66) = happyGoto action_108
action_337 (69) = happyGoto action_406
action_337 (70) = happyGoto action_184
action_337 (71) = happyGoto action_185
action_337 (72) = happyGoto action_110
action_337 (95) = happyGoto action_111
action_337 (96) = happyGoto action_7
action_337 (97) = happyGoto action_112
action_337 _ = happyFail

action_338 (137) = happyShift action_405
action_338 _ = happyFail

action_339 _ = happyReduce_29

action_340 (138) = happyShift action_404
action_340 _ = happyFail

action_341 _ = happyReduce_8

action_342 _ = happyReduce_37

action_343 (184) = happyShift action_403
action_343 _ = happyFail

action_344 _ = happyReduce_152

action_345 (159) = happyShift action_402
action_345 _ = happyReduce_155

action_346 (105) = happyShift action_32
action_346 (106) = happyShift action_33
action_346 (107) = happyShift action_34
action_346 (108) = happyShift action_35
action_346 (142) = happyShift action_113
action_346 (145) = happyShift action_401
action_346 (147) = happyShift action_114
action_346 (149) = happyShift action_115
action_346 (163) = happyShift action_116
action_346 (165) = happyShift action_117
action_346 (166) = happyShift action_118
action_346 (168) = happyShift action_119
action_346 (176) = happyShift action_67
action_346 (178) = happyShift action_8
action_346 (180) = happyShift action_9
action_346 (63) = happyGoto action_398
action_346 (64) = happyGoto action_399
action_346 (66) = happyGoto action_108
action_346 (72) = happyGoto action_400
action_346 (95) = happyGoto action_205
action_346 (96) = happyGoto action_7
action_346 (97) = happyGoto action_112
action_346 _ = happyReduce_157

action_347 (105) = happyShift action_32
action_347 (106) = happyShift action_33
action_347 (107) = happyShift action_34
action_347 (108) = happyShift action_35
action_347 (147) = happyShift action_82
action_347 (176) = happyShift action_67
action_347 (16) = happyGoto action_394
action_347 (17) = happyGoto action_395
action_347 (92) = happyGoto action_396
action_347 (94) = happyGoto action_397
action_347 (97) = happyGoto action_26
action_347 _ = happyFail

action_348 (163) = happyShift action_116
action_348 (165) = happyShift action_117
action_348 (166) = happyShift action_118
action_348 (168) = happyShift action_119
action_348 (65) = happyGoto action_393
action_348 (66) = happyGoto action_284
action_348 _ = happyFail

action_349 (105) = happyShift action_32
action_349 (106) = happyShift action_33
action_349 (107) = happyShift action_34
action_349 (108) = happyShift action_35
action_349 (121) = happyShift action_45
action_349 (123) = happyShift action_46
action_349 (131) = happyShift action_47
action_349 (135) = happyShift action_48
action_349 (142) = happyShift action_49
action_349 (147) = happyShift action_50
action_349 (149) = happyShift action_51
action_349 (151) = happyShift action_52
action_349 (152) = happyShift action_53
action_349 (153) = happyShift action_54
action_349 (154) = happyShift action_55
action_349 (157) = happyShift action_56
action_349 (163) = happyShift action_57
action_349 (164) = happyShift action_58
action_349 (165) = happyShift action_59
action_349 (166) = happyShift action_60
action_349 (167) = happyShift action_61
action_349 (168) = happyShift action_62
action_349 (169) = happyShift action_63
action_349 (170) = happyShift action_64
action_349 (171) = happyShift action_65
action_349 (172) = happyShift action_66
action_349 (176) = happyShift action_67
action_349 (177) = happyShift action_68
action_349 (178) = happyShift action_8
action_349 (179) = happyShift action_69
action_349 (180) = happyShift action_70
action_349 (181) = happyShift action_71
action_349 (182) = happyShift action_72
action_349 (183) = happyShift action_73
action_349 (184) = happyShift action_74
action_349 (24) = happyGoto action_13
action_349 (27) = happyGoto action_14
action_349 (28) = happyGoto action_15
action_349 (29) = happyGoto action_16
action_349 (30) = happyGoto action_17
action_349 (32) = happyGoto action_391
action_349 (33) = happyGoto action_392
action_349 (51) = happyGoto action_20
action_349 (53) = happyGoto action_21
action_349 (91) = happyGoto action_23
action_349 (92) = happyGoto action_93
action_349 (95) = happyGoto action_25
action_349 (96) = happyGoto action_7
action_349 (97) = happyGoto action_26
action_349 (98) = happyGoto action_27
action_349 _ = happyFail

action_350 (156) = happyShift action_390
action_350 _ = happyReduce_200

action_351 (148) = happyShift action_389
action_351 _ = happyFail

action_352 _ = happyReduce_194

action_353 (146) = happyShift action_388
action_353 _ = happyFail

action_354 (146) = happyShift action_387
action_354 _ = happyFail

action_355 _ = happyReduce_117

action_356 _ = happyReduce_110

action_357 _ = happyReduce_136

action_358 (105) = happyShift action_32
action_358 (106) = happyShift action_33
action_358 (107) = happyShift action_34
action_358 (108) = happyShift action_35
action_358 (118) = happyShift action_94
action_358 (121) = happyShift action_45
action_358 (123) = happyShift action_46
action_358 (124) = happyShift action_95
action_358 (127) = happyShift action_96
action_358 (128) = happyShift action_97
action_358 (131) = happyShift action_47
action_358 (132) = happyShift action_98
action_358 (133) = happyShift action_99
action_358 (134) = happyShift action_100
action_358 (135) = happyShift action_48
action_358 (142) = happyShift action_49
action_358 (147) = happyShift action_50
action_358 (149) = happyShift action_51
action_358 (151) = happyShift action_52
action_358 (152) = happyShift action_53
action_358 (153) = happyShift action_102
action_358 (154) = happyShift action_55
action_358 (157) = happyShift action_56
action_358 (163) = happyShift action_57
action_358 (164) = happyShift action_58
action_358 (165) = happyShift action_59
action_358 (166) = happyShift action_60
action_358 (167) = happyShift action_61
action_358 (168) = happyShift action_62
action_358 (169) = happyShift action_63
action_358 (170) = happyShift action_64
action_358 (171) = happyShift action_65
action_358 (172) = happyShift action_66
action_358 (176) = happyShift action_67
action_358 (177) = happyShift action_68
action_358 (178) = happyShift action_8
action_358 (179) = happyShift action_69
action_358 (180) = happyShift action_70
action_358 (181) = happyShift action_71
action_358 (182) = happyShift action_72
action_358 (183) = happyShift action_73
action_358 (184) = happyShift action_74
action_358 (18) = happyGoto action_265
action_358 (21) = happyGoto action_88
action_358 (22) = happyGoto action_89
action_358 (23) = happyGoto action_90
action_358 (24) = happyGoto action_91
action_358 (27) = happyGoto action_14
action_358 (28) = happyGoto action_15
action_358 (29) = happyGoto action_16
action_358 (30) = happyGoto action_17
action_358 (51) = happyGoto action_20
action_358 (52) = happyGoto action_386
action_358 (53) = happyGoto action_21
action_358 (91) = happyGoto action_23
action_358 (92) = happyGoto action_93
action_358 (95) = happyGoto action_25
action_358 (96) = happyGoto action_7
action_358 (97) = happyGoto action_26
action_358 (98) = happyGoto action_27
action_358 _ = happyFail

action_359 _ = happyReduce_48

action_360 _ = happyReduce_49

action_361 (105) = happyShift action_32
action_361 (106) = happyShift action_33
action_361 (107) = happyShift action_34
action_361 (108) = happyShift action_35
action_361 (121) = happyShift action_45
action_361 (123) = happyShift action_46
action_361 (131) = happyShift action_47
action_361 (135) = happyShift action_48
action_361 (142) = happyShift action_49
action_361 (147) = happyShift action_50
action_361 (149) = happyShift action_51
action_361 (153) = happyShift action_54
action_361 (154) = happyShift action_55
action_361 (171) = happyShift action_65
action_361 (172) = happyShift action_66
action_361 (176) = happyShift action_67
action_361 (177) = happyShift action_68
action_361 (178) = happyShift action_8
action_361 (180) = happyShift action_70
action_361 (181) = happyShift action_71
action_361 (182) = happyShift action_72
action_361 (183) = happyShift action_73
action_361 (184) = happyShift action_74
action_361 (26) = happyGoto action_385
action_361 (27) = happyGoto action_361
action_361 (28) = happyGoto action_15
action_361 (29) = happyGoto action_16
action_361 (30) = happyGoto action_17
action_361 (51) = happyGoto action_20
action_361 (53) = happyGoto action_21
action_361 (91) = happyGoto action_23
action_361 (92) = happyGoto action_93
action_361 (95) = happyGoto action_25
action_361 (96) = happyGoto action_7
action_361 (97) = happyGoto action_26
action_361 _ = happyReduce_65

action_362 (105) = happyShift action_32
action_362 (106) = happyShift action_33
action_362 (107) = happyShift action_34
action_362 (108) = happyShift action_35
action_362 (121) = happyShift action_45
action_362 (123) = happyShift action_46
action_362 (131) = happyShift action_47
action_362 (135) = happyShift action_48
action_362 (142) = happyShift action_49
action_362 (147) = happyShift action_50
action_362 (149) = happyShift action_51
action_362 (151) = happyShift action_52
action_362 (152) = happyShift action_53
action_362 (153) = happyShift action_54
action_362 (154) = happyShift action_55
action_362 (157) = happyShift action_56
action_362 (163) = happyShift action_57
action_362 (164) = happyShift action_58
action_362 (165) = happyShift action_59
action_362 (166) = happyShift action_60
action_362 (167) = happyShift action_61
action_362 (168) = happyShift action_62
action_362 (169) = happyShift action_63
action_362 (170) = happyShift action_64
action_362 (171) = happyShift action_65
action_362 (172) = happyShift action_66
action_362 (176) = happyShift action_67
action_362 (177) = happyShift action_68
action_362 (178) = happyShift action_8
action_362 (179) = happyShift action_69
action_362 (180) = happyShift action_70
action_362 (181) = happyShift action_71
action_362 (182) = happyShift action_72
action_362 (183) = happyShift action_73
action_362 (184) = happyShift action_74
action_362 (23) = happyGoto action_169
action_362 (24) = happyGoto action_91
action_362 (27) = happyGoto action_14
action_362 (28) = happyGoto action_15
action_362 (29) = happyGoto action_16
action_362 (30) = happyGoto action_17
action_362 (39) = happyGoto action_384
action_362 (40) = happyGoto action_245
action_362 (48) = happyGoto action_246
action_362 (51) = happyGoto action_20
action_362 (53) = happyGoto action_21
action_362 (91) = happyGoto action_23
action_362 (92) = happyGoto action_93
action_362 (95) = happyGoto action_25
action_362 (96) = happyGoto action_166
action_362 (97) = happyGoto action_26
action_362 (98) = happyGoto action_27
action_362 _ = happyFail

action_363 _ = happyReduce_41

action_364 (105) = happyShift action_32
action_364 (106) = happyShift action_33
action_364 (107) = happyShift action_34
action_364 (108) = happyShift action_35
action_364 (118) = happyShift action_94
action_364 (121) = happyShift action_45
action_364 (123) = happyShift action_46
action_364 (124) = happyShift action_95
action_364 (127) = happyShift action_96
action_364 (131) = happyShift action_47
action_364 (132) = happyShift action_98
action_364 (133) = happyShift action_99
action_364 (134) = happyShift action_100
action_364 (135) = happyShift action_48
action_364 (142) = happyShift action_49
action_364 (147) = happyShift action_50
action_364 (149) = happyShift action_51
action_364 (151) = happyShift action_52
action_364 (152) = happyShift action_53
action_364 (153) = happyShift action_102
action_364 (154) = happyShift action_55
action_364 (157) = happyShift action_56
action_364 (163) = happyShift action_57
action_364 (164) = happyShift action_58
action_364 (165) = happyShift action_59
action_364 (166) = happyShift action_60
action_364 (167) = happyShift action_61
action_364 (168) = happyShift action_62
action_364 (169) = happyShift action_63
action_364 (170) = happyShift action_64
action_364 (171) = happyShift action_65
action_364 (172) = happyShift action_66
action_364 (176) = happyShift action_67
action_364 (177) = happyShift action_68
action_364 (178) = happyShift action_8
action_364 (179) = happyShift action_69
action_364 (180) = happyShift action_70
action_364 (181) = happyShift action_71
action_364 (182) = happyShift action_72
action_364 (183) = happyShift action_73
action_364 (184) = happyShift action_74
action_364 (21) = happyGoto action_383
action_364 (22) = happyGoto action_89
action_364 (23) = happyGoto action_90
action_364 (24) = happyGoto action_91
action_364 (27) = happyGoto action_14
action_364 (28) = happyGoto action_15
action_364 (29) = happyGoto action_16
action_364 (30) = happyGoto action_17
action_364 (51) = happyGoto action_20
action_364 (53) = happyGoto action_21
action_364 (91) = happyGoto action_23
action_364 (92) = happyGoto action_93
action_364 (95) = happyGoto action_25
action_364 (96) = happyGoto action_7
action_364 (97) = happyGoto action_26
action_364 (98) = happyGoto action_27
action_364 _ = happyFail

action_365 (126) = happyShift action_382
action_365 _ = happyFail

action_366 _ = happyReduce_106

action_367 _ = happyReduce_55

action_368 (105) = happyShift action_32
action_368 (106) = happyShift action_33
action_368 (107) = happyShift action_34
action_368 (108) = happyShift action_35
action_368 (118) = happyShift action_94
action_368 (121) = happyShift action_45
action_368 (123) = happyShift action_46
action_368 (124) = happyShift action_95
action_368 (127) = happyShift action_96
action_368 (128) = happyShift action_97
action_368 (131) = happyShift action_47
action_368 (132) = happyShift action_98
action_368 (133) = happyShift action_99
action_368 (134) = happyShift action_100
action_368 (135) = happyShift action_48
action_368 (142) = happyShift action_49
action_368 (147) = happyShift action_50
action_368 (149) = happyShift action_51
action_368 (151) = happyShift action_52
action_368 (152) = happyShift action_53
action_368 (153) = happyShift action_102
action_368 (154) = happyShift action_55
action_368 (157) = happyShift action_56
action_368 (163) = happyShift action_57
action_368 (164) = happyShift action_58
action_368 (165) = happyShift action_59
action_368 (166) = happyShift action_60
action_368 (167) = happyShift action_61
action_368 (168) = happyShift action_62
action_368 (169) = happyShift action_63
action_368 (170) = happyShift action_64
action_368 (171) = happyShift action_65
action_368 (172) = happyShift action_66
action_368 (176) = happyShift action_67
action_368 (177) = happyShift action_68
action_368 (178) = happyShift action_8
action_368 (179) = happyShift action_69
action_368 (180) = happyShift action_70
action_368 (181) = happyShift action_71
action_368 (182) = happyShift action_72
action_368 (183) = happyShift action_73
action_368 (184) = happyShift action_74
action_368 (18) = happyGoto action_249
action_368 (21) = happyGoto action_88
action_368 (22) = happyGoto action_89
action_368 (23) = happyGoto action_90
action_368 (24) = happyGoto action_91
action_368 (27) = happyGoto action_14
action_368 (28) = happyGoto action_15
action_368 (29) = happyGoto action_16
action_368 (30) = happyGoto action_17
action_368 (51) = happyGoto action_20
action_368 (53) = happyGoto action_21
action_368 (55) = happyGoto action_381
action_368 (56) = happyGoto action_251
action_368 (91) = happyGoto action_23
action_368 (92) = happyGoto action_93
action_368 (95) = happyGoto action_25
action_368 (96) = happyGoto action_7
action_368 (97) = happyGoto action_26
action_368 (98) = happyGoto action_27
action_368 _ = happyFail

action_369 _ = happyReduce_143

action_370 (105) = happyShift action_32
action_370 (106) = happyShift action_33
action_370 (107) = happyShift action_34
action_370 (108) = happyShift action_35
action_370 (118) = happyShift action_94
action_370 (121) = happyShift action_45
action_370 (123) = happyShift action_46
action_370 (124) = happyShift action_95
action_370 (127) = happyShift action_96
action_370 (128) = happyShift action_97
action_370 (131) = happyShift action_47
action_370 (132) = happyShift action_98
action_370 (133) = happyShift action_99
action_370 (134) = happyShift action_100
action_370 (135) = happyShift action_48
action_370 (142) = happyShift action_49
action_370 (147) = happyShift action_50
action_370 (149) = happyShift action_51
action_370 (151) = happyShift action_52
action_370 (152) = happyShift action_53
action_370 (153) = happyShift action_102
action_370 (154) = happyShift action_55
action_370 (157) = happyShift action_56
action_370 (163) = happyShift action_57
action_370 (164) = happyShift action_58
action_370 (165) = happyShift action_59
action_370 (166) = happyShift action_60
action_370 (167) = happyShift action_61
action_370 (168) = happyShift action_62
action_370 (169) = happyShift action_63
action_370 (170) = happyShift action_64
action_370 (171) = happyShift action_65
action_370 (172) = happyShift action_66
action_370 (176) = happyShift action_67
action_370 (177) = happyShift action_68
action_370 (178) = happyShift action_8
action_370 (179) = happyShift action_69
action_370 (180) = happyShift action_70
action_370 (181) = happyShift action_71
action_370 (182) = happyShift action_72
action_370 (183) = happyShift action_73
action_370 (184) = happyShift action_74
action_370 (18) = happyGoto action_380
action_370 (21) = happyGoto action_88
action_370 (22) = happyGoto action_89
action_370 (23) = happyGoto action_90
action_370 (24) = happyGoto action_91
action_370 (27) = happyGoto action_14
action_370 (28) = happyGoto action_15
action_370 (29) = happyGoto action_16
action_370 (30) = happyGoto action_17
action_370 (51) = happyGoto action_20
action_370 (53) = happyGoto action_21
action_370 (91) = happyGoto action_23
action_370 (92) = happyGoto action_93
action_370 (95) = happyGoto action_25
action_370 (96) = happyGoto action_7
action_370 (97) = happyGoto action_26
action_370 (98) = happyGoto action_27
action_370 _ = happyFail

action_371 (105) = happyShift action_32
action_371 (106) = happyShift action_33
action_371 (107) = happyShift action_34
action_371 (108) = happyShift action_35
action_371 (118) = happyShift action_94
action_371 (121) = happyShift action_45
action_371 (123) = happyShift action_46
action_371 (124) = happyShift action_95
action_371 (127) = happyShift action_96
action_371 (128) = happyShift action_97
action_371 (131) = happyShift action_47
action_371 (132) = happyShift action_98
action_371 (133) = happyShift action_99
action_371 (134) = happyShift action_100
action_371 (135) = happyShift action_48
action_371 (142) = happyShift action_49
action_371 (147) = happyShift action_50
action_371 (149) = happyShift action_51
action_371 (151) = happyShift action_52
action_371 (152) = happyShift action_53
action_371 (153) = happyShift action_102
action_371 (154) = happyShift action_55
action_371 (157) = happyShift action_56
action_371 (163) = happyShift action_57
action_371 (164) = happyShift action_58
action_371 (165) = happyShift action_59
action_371 (166) = happyShift action_60
action_371 (167) = happyShift action_61
action_371 (168) = happyShift action_62
action_371 (169) = happyShift action_63
action_371 (170) = happyShift action_64
action_371 (171) = happyShift action_65
action_371 (172) = happyShift action_66
action_371 (176) = happyShift action_67
action_371 (177) = happyShift action_68
action_371 (178) = happyShift action_8
action_371 (179) = happyShift action_69
action_371 (180) = happyShift action_70
action_371 (181) = happyShift action_71
action_371 (182) = happyShift action_72
action_371 (183) = happyShift action_73
action_371 (184) = happyShift action_74
action_371 (18) = happyGoto action_379
action_371 (21) = happyGoto action_88
action_371 (22) = happyGoto action_89
action_371 (23) = happyGoto action_90
action_371 (24) = happyGoto action_91
action_371 (27) = happyGoto action_14
action_371 (28) = happyGoto action_15
action_371 (29) = happyGoto action_16
action_371 (30) = happyGoto action_17
action_371 (51) = happyGoto action_20
action_371 (53) = happyGoto action_21
action_371 (91) = happyGoto action_23
action_371 (92) = happyGoto action_93
action_371 (95) = happyGoto action_25
action_371 (96) = happyGoto action_7
action_371 (97) = happyGoto action_26
action_371 (98) = happyGoto action_27
action_371 _ = happyFail

action_372 _ = happyReduce_141

action_373 (105) = happyShift action_32
action_373 (106) = happyShift action_33
action_373 (107) = happyShift action_34
action_373 (108) = happyShift action_35
action_373 (118) = happyShift action_94
action_373 (121) = happyShift action_45
action_373 (123) = happyShift action_46
action_373 (124) = happyShift action_95
action_373 (127) = happyShift action_96
action_373 (128) = happyShift action_97
action_373 (131) = happyShift action_47
action_373 (132) = happyShift action_98
action_373 (133) = happyShift action_99
action_373 (134) = happyShift action_100
action_373 (135) = happyShift action_48
action_373 (142) = happyShift action_49
action_373 (147) = happyShift action_50
action_373 (149) = happyShift action_51
action_373 (151) = happyShift action_52
action_373 (152) = happyShift action_53
action_373 (153) = happyShift action_102
action_373 (154) = happyShift action_55
action_373 (157) = happyShift action_56
action_373 (163) = happyShift action_57
action_373 (164) = happyShift action_58
action_373 (165) = happyShift action_59
action_373 (166) = happyShift action_60
action_373 (167) = happyShift action_61
action_373 (168) = happyShift action_62
action_373 (169) = happyShift action_63
action_373 (170) = happyShift action_64
action_373 (171) = happyShift action_65
action_373 (172) = happyShift action_66
action_373 (176) = happyShift action_67
action_373 (177) = happyShift action_68
action_373 (178) = happyShift action_8
action_373 (179) = happyShift action_69
action_373 (180) = happyShift action_70
action_373 (181) = happyShift action_71
action_373 (182) = happyShift action_72
action_373 (183) = happyShift action_73
action_373 (184) = happyShift action_74
action_373 (18) = happyGoto action_162
action_373 (21) = happyGoto action_88
action_373 (22) = happyGoto action_89
action_373 (23) = happyGoto action_90
action_373 (24) = happyGoto action_91
action_373 (27) = happyGoto action_14
action_373 (28) = happyGoto action_15
action_373 (29) = happyGoto action_16
action_373 (30) = happyGoto action_17
action_373 (34) = happyGoto action_378
action_373 (51) = happyGoto action_20
action_373 (53) = happyGoto action_21
action_373 (91) = happyGoto action_23
action_373 (92) = happyGoto action_93
action_373 (95) = happyGoto action_25
action_373 (96) = happyGoto action_7
action_373 (97) = happyGoto action_26
action_373 (98) = happyGoto action_27
action_373 _ = happyFail

action_374 (105) = happyShift action_32
action_374 (106) = happyShift action_33
action_374 (107) = happyShift action_34
action_374 (108) = happyShift action_35
action_374 (121) = happyShift action_45
action_374 (123) = happyShift action_46
action_374 (131) = happyShift action_47
action_374 (135) = happyShift action_48
action_374 (142) = happyShift action_49
action_374 (147) = happyShift action_50
action_374 (149) = happyShift action_51
action_374 (151) = happyShift action_52
action_374 (152) = happyShift action_53
action_374 (153) = happyShift action_54
action_374 (154) = happyShift action_55
action_374 (157) = happyShift action_56
action_374 (163) = happyShift action_57
action_374 (164) = happyShift action_58
action_374 (165) = happyShift action_59
action_374 (166) = happyShift action_60
action_374 (167) = happyShift action_61
action_374 (168) = happyShift action_62
action_374 (169) = happyShift action_63
action_374 (170) = happyShift action_64
action_374 (171) = happyShift action_65
action_374 (172) = happyShift action_66
action_374 (176) = happyShift action_67
action_374 (177) = happyShift action_68
action_374 (178) = happyShift action_8
action_374 (179) = happyShift action_69
action_374 (180) = happyShift action_70
action_374 (181) = happyShift action_71
action_374 (182) = happyShift action_72
action_374 (183) = happyShift action_73
action_374 (184) = happyShift action_74
action_374 (23) = happyGoto action_169
action_374 (24) = happyGoto action_91
action_374 (27) = happyGoto action_14
action_374 (28) = happyGoto action_15
action_374 (29) = happyGoto action_16
action_374 (30) = happyGoto action_17
action_374 (39) = happyGoto action_377
action_374 (40) = happyGoto action_245
action_374 (48) = happyGoto action_246
action_374 (51) = happyGoto action_20
action_374 (53) = happyGoto action_21
action_374 (91) = happyGoto action_23
action_374 (92) = happyGoto action_93
action_374 (95) = happyGoto action_25
action_374 (96) = happyGoto action_166
action_374 (97) = happyGoto action_26
action_374 (98) = happyGoto action_27
action_374 _ = happyReduce_282

action_375 _ = happyReduce_111

action_376 _ = happyReduce_77

action_377 _ = happyReduce_112

action_378 _ = happyReduce_113

action_379 _ = happyReduce_149

action_380 _ = happyReduce_148

action_381 _ = happyReduce_147

action_382 (105) = happyShift action_32
action_382 (106) = happyShift action_33
action_382 (107) = happyShift action_34
action_382 (108) = happyShift action_35
action_382 (118) = happyShift action_94
action_382 (121) = happyShift action_45
action_382 (123) = happyShift action_46
action_382 (124) = happyShift action_95
action_382 (127) = happyShift action_96
action_382 (131) = happyShift action_47
action_382 (132) = happyShift action_98
action_382 (133) = happyShift action_99
action_382 (134) = happyShift action_100
action_382 (135) = happyShift action_48
action_382 (142) = happyShift action_49
action_382 (147) = happyShift action_50
action_382 (149) = happyShift action_51
action_382 (151) = happyShift action_52
action_382 (152) = happyShift action_53
action_382 (153) = happyShift action_102
action_382 (154) = happyShift action_55
action_382 (157) = happyShift action_56
action_382 (163) = happyShift action_57
action_382 (164) = happyShift action_58
action_382 (165) = happyShift action_59
action_382 (166) = happyShift action_60
action_382 (167) = happyShift action_61
action_382 (168) = happyShift action_62
action_382 (169) = happyShift action_63
action_382 (170) = happyShift action_64
action_382 (171) = happyShift action_65
action_382 (172) = happyShift action_66
action_382 (176) = happyShift action_67
action_382 (177) = happyShift action_68
action_382 (178) = happyShift action_8
action_382 (179) = happyShift action_69
action_382 (180) = happyShift action_70
action_382 (181) = happyShift action_71
action_382 (182) = happyShift action_72
action_382 (183) = happyShift action_73
action_382 (184) = happyShift action_74
action_382 (21) = happyGoto action_483
action_382 (22) = happyGoto action_89
action_382 (23) = happyGoto action_90
action_382 (24) = happyGoto action_91
action_382 (27) = happyGoto action_14
action_382 (28) = happyGoto action_15
action_382 (29) = happyGoto action_16
action_382 (30) = happyGoto action_17
action_382 (51) = happyGoto action_20
action_382 (53) = happyGoto action_21
action_382 (91) = happyGoto action_23
action_382 (92) = happyGoto action_93
action_382 (95) = happyGoto action_25
action_382 (96) = happyGoto action_7
action_382 (97) = happyGoto action_26
action_382 (98) = happyGoto action_27
action_382 _ = happyFail

action_383 _ = happyReduce_46

action_384 (146) = happyShift action_482
action_384 _ = happyFail

action_385 _ = happyReduce_66

action_386 _ = happyReduce_138

action_387 _ = happyReduce_75

action_388 _ = happyReduce_19

action_389 _ = happyReduce_197

action_390 (105) = happyShift action_32
action_390 (106) = happyShift action_33
action_390 (107) = happyShift action_34
action_390 (108) = happyShift action_35
action_390 (142) = happyShift action_113
action_390 (147) = happyShift action_114
action_390 (149) = happyShift action_115
action_390 (163) = happyShift action_116
action_390 (165) = happyShift action_117
action_390 (166) = happyShift action_118
action_390 (168) = happyShift action_119
action_390 (176) = happyShift action_67
action_390 (178) = happyShift action_8
action_390 (180) = happyShift action_9
action_390 (66) = happyGoto action_108
action_390 (70) = happyGoto action_350
action_390 (71) = happyGoto action_185
action_390 (72) = happyGoto action_110
action_390 (74) = happyGoto action_481
action_390 (95) = happyGoto action_111
action_390 (96) = happyGoto action_7
action_390 (97) = happyGoto action_112
action_390 _ = happyFail

action_391 (158) = happyShift action_152
action_391 (99) = happyGoto action_479
action_391 (100) = happyGoto action_480
action_391 _ = happyReduce_281

action_392 (146) = happyShift action_478
action_392 _ = happyFail

action_393 _ = happyReduce_171

action_394 (158) = happyShift action_152
action_394 (99) = happyGoto action_476
action_394 (100) = happyGoto action_477
action_394 _ = happyReduce_281

action_395 (146) = happyShift action_475
action_395 _ = happyFail

action_396 (156) = happyShift action_474
action_396 _ = happyReduce_257

action_397 (137) = happyShift action_473
action_397 _ = happyFail

action_398 (105) = happyShift action_32
action_398 (106) = happyShift action_33
action_398 (107) = happyShift action_34
action_398 (108) = happyShift action_35
action_398 (142) = happyShift action_113
action_398 (147) = happyShift action_114
action_398 (149) = happyShift action_115
action_398 (163) = happyShift action_116
action_398 (165) = happyShift action_117
action_398 (166) = happyShift action_118
action_398 (168) = happyShift action_119
action_398 (176) = happyShift action_67
action_398 (178) = happyShift action_8
action_398 (180) = happyShift action_9
action_398 (63) = happyGoto action_398
action_398 (64) = happyGoto action_472
action_398 (66) = happyGoto action_108
action_398 (72) = happyGoto action_400
action_398 (95) = happyGoto action_205
action_398 (96) = happyGoto action_7
action_398 (97) = happyGoto action_112
action_398 _ = happyReduce_168

action_399 _ = happyReduce_158

action_400 _ = happyReduce_167

action_401 (105) = happyShift action_186
action_401 (106) = happyShift action_33
action_401 (107) = happyShift action_34
action_401 (108) = happyShift action_35
action_401 (136) = happyShift action_187
action_401 (142) = happyShift action_113
action_401 (147) = happyShift action_470
action_401 (149) = happyShift action_115
action_401 (160) = happyShift action_471
action_401 (163) = happyShift action_116
action_401 (165) = happyShift action_117
action_401 (166) = happyShift action_118
action_401 (168) = happyShift action_119
action_401 (176) = happyShift action_67
action_401 (178) = happyShift action_8
action_401 (180) = happyShift action_9
action_401 (60) = happyGoto action_465
action_401 (62) = happyGoto action_466
action_401 (66) = happyGoto action_108
action_401 (67) = happyGoto action_467
action_401 (68) = happyGoto action_182
action_401 (69) = happyGoto action_183
action_401 (70) = happyGoto action_184
action_401 (71) = happyGoto action_185
action_401 (72) = happyGoto action_110
action_401 (92) = happyGoto action_468
action_401 (95) = happyGoto action_111
action_401 (96) = happyGoto action_7
action_401 (97) = happyGoto action_469
action_401 _ = happyFail

action_402 (178) = happyShift action_8
action_402 (58) = happyGoto action_464
action_402 (59) = happyGoto action_345
action_402 (96) = happyGoto action_346
action_402 _ = happyFail

action_403 _ = happyReduce_154

action_404 (105) = happyShift action_32
action_404 (106) = happyShift action_33
action_404 (107) = happyShift action_34
action_404 (108) = happyShift action_35
action_404 (142) = happyShift action_113
action_404 (147) = happyShift action_114
action_404 (149) = happyShift action_115
action_404 (163) = happyShift action_116
action_404 (165) = happyShift action_117
action_404 (166) = happyShift action_118
action_404 (168) = happyShift action_119
action_404 (176) = happyShift action_67
action_404 (178) = happyShift action_8
action_404 (180) = happyShift action_9
action_404 (66) = happyGoto action_108
action_404 (70) = happyGoto action_463
action_404 (71) = happyGoto action_185
action_404 (72) = happyGoto action_110
action_404 (95) = happyGoto action_111
action_404 (96) = happyGoto action_7
action_404 (97) = happyGoto action_112
action_404 _ = happyFail

action_405 (105) = happyShift action_186
action_405 (106) = happyShift action_33
action_405 (107) = happyShift action_34
action_405 (108) = happyShift action_35
action_405 (136) = happyShift action_187
action_405 (142) = happyShift action_113
action_405 (147) = happyShift action_114
action_405 (149) = happyShift action_115
action_405 (163) = happyShift action_116
action_405 (165) = happyShift action_117
action_405 (166) = happyShift action_118
action_405 (168) = happyShift action_119
action_405 (176) = happyShift action_67
action_405 (178) = happyShift action_8
action_405 (180) = happyShift action_9
action_405 (66) = happyGoto action_108
action_405 (67) = happyGoto action_462
action_405 (68) = happyGoto action_182
action_405 (69) = happyGoto action_183
action_405 (70) = happyGoto action_184
action_405 (71) = happyGoto action_185
action_405 (72) = happyGoto action_110
action_405 (95) = happyGoto action_111
action_405 (96) = happyGoto action_7
action_405 (97) = happyGoto action_112
action_405 _ = happyFail

action_406 _ = happyReduce_179

action_407 (163) = happyShift action_116
action_407 (165) = happyShift action_117
action_407 (166) = happyShift action_118
action_407 (168) = happyShift action_119
action_407 (65) = happyGoto action_461
action_407 (66) = happyGoto action_284
action_407 _ = happyFail

action_408 _ = happyReduce_237

action_409 (105) = happyShift action_32
action_409 (106) = happyShift action_33
action_409 (107) = happyShift action_34
action_409 (108) = happyShift action_35
action_409 (147) = happyShift action_460
action_409 (176) = happyShift action_67
action_409 (86) = happyGoto action_458
action_409 (92) = happyGoto action_459
action_409 (97) = happyGoto action_26
action_409 _ = happyFail

action_410 (148) = happyShift action_457
action_410 _ = happyFail

action_411 (157) = happyShift action_456
action_411 _ = happyFail

action_412 (148) = happyReduce_232
action_412 (153) = happyShift action_455
action_412 (157) = happyReduce_251
action_412 _ = happyReduce_217

action_413 (105) = happyShift action_32
action_413 (106) = happyShift action_33
action_413 (107) = happyShift action_34
action_413 (108) = happyShift action_35
action_413 (142) = happyShift action_113
action_413 (147) = happyShift action_114
action_413 (149) = happyShift action_115
action_413 (163) = happyShift action_116
action_413 (165) = happyShift action_117
action_413 (166) = happyShift action_118
action_413 (168) = happyShift action_119
action_413 (176) = happyShift action_67
action_413 (178) = happyShift action_8
action_413 (180) = happyShift action_9
action_413 (66) = happyGoto action_108
action_413 (72) = happyGoto action_203
action_413 (73) = happyGoto action_454
action_413 (95) = happyGoto action_205
action_413 (96) = happyGoto action_7
action_413 (97) = happyGoto action_112
action_413 _ = happyReduce_215

action_414 (105) = happyShift action_32
action_414 (106) = happyShift action_33
action_414 (107) = happyShift action_34
action_414 (108) = happyShift action_35
action_414 (147) = happyShift action_82
action_414 (151) = happyShift action_52
action_414 (152) = happyShift action_53
action_414 (157) = happyShift action_56
action_414 (163) = happyShift action_57
action_414 (164) = happyShift action_58
action_414 (165) = happyShift action_59
action_414 (166) = happyShift action_60
action_414 (167) = happyShift action_61
action_414 (168) = happyShift action_453
action_414 (169) = happyShift action_63
action_414 (170) = happyShift action_64
action_414 (176) = happyShift action_67
action_414 (178) = happyShift action_8
action_414 (179) = happyShift action_69
action_414 (180) = happyShift action_9
action_414 (79) = happyGoto action_450
action_414 (81) = happyGoto action_451
action_414 (92) = happyGoto action_452
action_414 (95) = happyGoto action_413
action_414 (96) = happyGoto action_7
action_414 (97) = happyGoto action_26
action_414 (98) = happyGoto action_242
action_414 _ = happyFail

action_415 (145) = happyShift action_449
action_415 _ = happyFail

action_416 (145) = happyShift action_448
action_416 _ = happyFail

action_417 (105) = happyShift action_32
action_417 (106) = happyShift action_33
action_417 (107) = happyShift action_34
action_417 (108) = happyShift action_35
action_417 (147) = happyShift action_420
action_417 (168) = happyShift action_421
action_417 (170) = happyShift action_422
action_417 (176) = happyShift action_67
action_417 (88) = happyGoto action_417
action_417 (90) = happyGoto action_447
action_417 (92) = happyGoto action_419
action_417 (97) = happyGoto action_26
action_417 _ = happyReduce_249

action_418 _ = happyReduce_209

action_419 _ = happyReduce_239

action_420 (105) = happyShift action_32
action_420 (106) = happyShift action_33
action_420 (107) = happyShift action_34
action_420 (108) = happyShift action_35
action_420 (147) = happyShift action_420
action_420 (151) = happyShift action_52
action_420 (152) = happyShift action_53
action_420 (157) = happyShift action_56
action_420 (163) = happyShift action_57
action_420 (164) = happyShift action_58
action_420 (165) = happyShift action_59
action_420 (166) = happyShift action_60
action_420 (167) = happyShift action_61
action_420 (168) = happyShift action_445
action_420 (169) = happyShift action_63
action_420 (170) = happyShift action_446
action_420 (176) = happyShift action_67
action_420 (178) = happyShift action_8
action_420 (179) = happyShift action_69
action_420 (180) = happyShift action_70
action_420 (88) = happyGoto action_440
action_420 (89) = happyGoto action_441
action_420 (91) = happyGoto action_442
action_420 (92) = happyGoto action_443
action_420 (95) = happyGoto action_444
action_420 (96) = happyGoto action_7
action_420 (97) = happyGoto action_26
action_420 (98) = happyGoto action_242
action_420 _ = happyFail

action_421 (145) = happyShift action_439
action_421 _ = happyFail

action_422 (145) = happyShift action_438
action_422 _ = happyFail

action_423 (105) = happyShift action_32
action_423 (106) = happyShift action_33
action_423 (107) = happyShift action_34
action_423 (108) = happyShift action_35
action_423 (147) = happyShift action_82
action_423 (168) = happyShift action_415
action_423 (170) = happyShift action_416
action_423 (176) = happyShift action_67
action_423 (178) = happyShift action_8
action_423 (180) = happyShift action_70
action_423 (81) = happyGoto action_408
action_423 (87) = happyGoto action_436
action_423 (91) = happyGoto action_411
action_423 (92) = happyGoto action_437
action_423 (95) = happyGoto action_413
action_423 (96) = happyGoto action_7
action_423 (97) = happyGoto action_26
action_423 _ = happyFail

action_424 (105) = happyShift action_32
action_424 (106) = happyShift action_33
action_424 (107) = happyShift action_34
action_424 (108) = happyShift action_35
action_424 (147) = happyShift action_82
action_424 (176) = happyShift action_67
action_424 (178) = happyShift action_8
action_424 (180) = happyShift action_9
action_424 (77) = happyGoto action_435
action_424 (78) = happyGoto action_330
action_424 (92) = happyGoto action_331
action_424 (95) = happyGoto action_332
action_424 (96) = happyGoto action_7
action_424 (97) = happyGoto action_26
action_424 _ = happyFail

action_425 _ = happyReduce_127

action_426 (155) = happyShift action_434
action_426 _ = happyFail

action_427 (155) = happyShift action_433
action_427 _ = happyFail

action_428 (160) = happyShift action_324
action_428 (49) = happyGoto action_432
action_428 (50) = happyGoto action_322
action_428 _ = happyFail

action_429 _ = happyReduce_131

action_430 (146) = happyShift action_431
action_430 _ = happyFail

action_431 _ = happyReduce_102

action_432 _ = happyReduce_133

action_433 (105) = happyShift action_32
action_433 (106) = happyShift action_33
action_433 (107) = happyShift action_34
action_433 (108) = happyShift action_35
action_433 (147) = happyShift action_82
action_433 (176) = happyShift action_67
action_433 (92) = happyGoto action_529
action_433 (97) = happyGoto action_26
action_433 _ = happyFail

action_434 (105) = happyShift action_32
action_434 (106) = happyShift action_33
action_434 (107) = happyShift action_34
action_434 (108) = happyShift action_35
action_434 (147) = happyShift action_82
action_434 (176) = happyShift action_67
action_434 (92) = happyGoto action_528
action_434 (97) = happyGoto action_26
action_434 _ = happyFail

action_435 _ = happyReduce_207

action_436 _ = happyReduce_208

action_437 (153) = happyShift action_455
action_437 (157) = happyReduce_251
action_437 _ = happyReduce_232

action_438 (105) = happyShift action_32
action_438 (106) = happyShift action_33
action_438 (107) = happyShift action_34
action_438 (108) = happyShift action_35
action_438 (147) = happyShift action_82
action_438 (170) = happyShift action_520
action_438 (176) = happyShift action_67
action_438 (180) = happyShift action_505
action_438 (83) = happyGoto action_518
action_438 (85) = happyGoto action_527
action_438 (91) = happyGoto action_502
action_438 (92) = happyGoto action_503
action_438 (97) = happyGoto action_26
action_438 _ = happyFail

action_439 (105) = happyShift action_32
action_439 (106) = happyShift action_33
action_439 (107) = happyShift action_34
action_439 (108) = happyShift action_35
action_439 (147) = happyShift action_82
action_439 (168) = happyShift action_517
action_439 (176) = happyShift action_67
action_439 (178) = happyShift action_8
action_439 (180) = happyShift action_9
action_439 (79) = happyGoto action_515
action_439 (80) = happyGoto action_526
action_439 (81) = happyGoto action_451
action_439 (92) = happyGoto action_452
action_439 (95) = happyGoto action_413
action_439 (96) = happyGoto action_7
action_439 (97) = happyGoto action_26
action_439 _ = happyFail

action_440 _ = happyReduce_243

action_441 (148) = happyShift action_525
action_441 _ = happyFail

action_442 (157) = happyShift action_524
action_442 _ = happyFail

action_443 (153) = happyShift action_523
action_443 (157) = happyReduce_251
action_443 _ = happyReduce_239

action_444 (105) = happyShift action_32
action_444 (106) = happyShift action_33
action_444 (107) = happyShift action_34
action_444 (108) = happyShift action_35
action_444 (142) = happyShift action_113
action_444 (147) = happyShift action_114
action_444 (149) = happyShift action_115
action_444 (162) = happyShift action_522
action_444 (163) = happyShift action_116
action_444 (165) = happyShift action_117
action_444 (166) = happyShift action_118
action_444 (168) = happyShift action_119
action_444 (176) = happyShift action_67
action_444 (178) = happyShift action_8
action_444 (180) = happyShift action_9
action_444 (66) = happyGoto action_108
action_444 (72) = happyGoto action_203
action_444 (73) = happyGoto action_521
action_444 (95) = happyGoto action_205
action_444 (96) = happyGoto action_7
action_444 (97) = happyGoto action_112
action_444 _ = happyFail

action_445 (145) = happyShift action_439
action_445 _ = happyReduce_271

action_446 (145) = happyShift action_438
action_446 _ = happyReduce_277

action_447 _ = happyReduce_250

action_448 (105) = happyShift action_32
action_448 (106) = happyShift action_33
action_448 (107) = happyShift action_34
action_448 (108) = happyShift action_35
action_448 (147) = happyShift action_82
action_448 (170) = happyShift action_520
action_448 (176) = happyShift action_67
action_448 (180) = happyShift action_505
action_448 (83) = happyGoto action_518
action_448 (85) = happyGoto action_519
action_448 (91) = happyGoto action_502
action_448 (92) = happyGoto action_503
action_448 (97) = happyGoto action_26
action_448 _ = happyFail

action_449 (105) = happyShift action_32
action_449 (106) = happyShift action_33
action_449 (107) = happyShift action_34
action_449 (108) = happyShift action_35
action_449 (147) = happyShift action_82
action_449 (168) = happyShift action_517
action_449 (176) = happyShift action_67
action_449 (178) = happyShift action_8
action_449 (180) = happyShift action_9
action_449 (79) = happyGoto action_515
action_449 (80) = happyGoto action_516
action_449 (81) = happyGoto action_451
action_449 (92) = happyGoto action_452
action_449 (95) = happyGoto action_413
action_449 (96) = happyGoto action_7
action_449 (97) = happyGoto action_26
action_449 _ = happyFail

action_450 (148) = happyShift action_514
action_450 _ = happyFail

action_451 _ = happyReduce_211

action_452 _ = happyReduce_210

action_453 (145) = happyShift action_513
action_453 _ = happyReduce_271

action_454 _ = happyReduce_216

action_455 (105) = happyShift action_32
action_455 (106) = happyShift action_33
action_455 (107) = happyShift action_34
action_455 (108) = happyShift action_35
action_455 (147) = happyShift action_82
action_455 (176) = happyShift action_67
action_455 (180) = happyShift action_505
action_455 (91) = happyGoto action_512
action_455 (92) = happyGoto action_84
action_455 (97) = happyGoto action_26
action_455 _ = happyFail

action_456 (105) = happyShift action_32
action_456 (106) = happyShift action_33
action_456 (107) = happyShift action_34
action_456 (108) = happyShift action_35
action_456 (142) = happyShift action_113
action_456 (147) = happyShift action_470
action_456 (149) = happyShift action_115
action_456 (163) = happyShift action_116
action_456 (165) = happyShift action_117
action_456 (166) = happyShift action_118
action_456 (168) = happyShift action_119
action_456 (170) = happyShift action_511
action_456 (176) = happyShift action_67
action_456 (178) = happyShift action_8
action_456 (180) = happyShift action_9
action_456 (66) = happyGoto action_108
action_456 (70) = happyGoto action_508
action_456 (71) = happyGoto action_185
action_456 (72) = happyGoto action_110
action_456 (84) = happyGoto action_509
action_456 (92) = happyGoto action_510
action_456 (95) = happyGoto action_111
action_456 (96) = happyGoto action_7
action_456 (97) = happyGoto action_469
action_456 _ = happyFail

action_457 (152) = happyShift action_507
action_457 _ = happyFail

action_458 (148) = happyShift action_506
action_458 _ = happyFail

action_459 _ = happyReduce_230

action_460 (105) = happyShift action_32
action_460 (106) = happyShift action_33
action_460 (107) = happyShift action_34
action_460 (108) = happyShift action_35
action_460 (147) = happyShift action_82
action_460 (151) = happyShift action_52
action_460 (152) = happyShift action_53
action_460 (157) = happyShift action_56
action_460 (163) = happyShift action_57
action_460 (164) = happyShift action_58
action_460 (165) = happyShift action_59
action_460 (166) = happyShift action_60
action_460 (167) = happyShift action_61
action_460 (168) = happyShift action_62
action_460 (169) = happyShift action_63
action_460 (170) = happyShift action_504
action_460 (176) = happyShift action_67
action_460 (179) = happyShift action_69
action_460 (180) = happyShift action_505
action_460 (83) = happyGoto action_501
action_460 (91) = happyGoto action_502
action_460 (92) = happyGoto action_503
action_460 (97) = happyGoto action_26
action_460 (98) = happyGoto action_242
action_460 _ = happyFail

action_461 (148) = happyShift action_500
action_461 _ = happyFail

action_462 (138) = happyShift action_499
action_462 _ = happyReduce_22

action_463 (158) = happyShift action_498
action_463 _ = happyFail

action_464 _ = happyReduce_156

action_465 (105) = happyShift action_186
action_465 (106) = happyShift action_33
action_465 (107) = happyShift action_34
action_465 (108) = happyShift action_35
action_465 (136) = happyShift action_187
action_465 (142) = happyShift action_113
action_465 (147) = happyShift action_470
action_465 (149) = happyShift action_115
action_465 (160) = happyShift action_471
action_465 (163) = happyShift action_116
action_465 (165) = happyShift action_117
action_465 (166) = happyShift action_118
action_465 (168) = happyShift action_119
action_465 (176) = happyShift action_67
action_465 (178) = happyShift action_8
action_465 (180) = happyShift action_9
action_465 (60) = happyGoto action_465
action_465 (62) = happyGoto action_497
action_465 (66) = happyGoto action_108
action_465 (67) = happyGoto action_467
action_465 (68) = happyGoto action_182
action_465 (69) = happyGoto action_183
action_465 (70) = happyGoto action_184
action_465 (71) = happyGoto action_185
action_465 (72) = happyGoto action_110
action_465 (92) = happyGoto action_468
action_465 (95) = happyGoto action_111
action_465 (96) = happyGoto action_7
action_465 (97) = happyGoto action_469
action_465 _ = happyReduce_165

action_466 (146) = happyShift action_496
action_466 _ = happyFail

action_467 (158) = happyShift action_495
action_467 _ = happyFail

action_468 (137) = happyShift action_494
action_468 _ = happyFail

action_469 (137) = happyReduce_253
action_469 (141) = happyReduce_253
action_469 (153) = happyReduce_253
action_469 _ = happyReduce_189

action_470 (105) = happyShift action_32
action_470 (106) = happyShift action_33
action_470 (107) = happyShift action_208
action_470 (108) = happyShift action_35
action_470 (142) = happyShift action_113
action_470 (147) = happyShift action_114
action_470 (149) = happyShift action_115
action_470 (151) = happyShift action_52
action_470 (152) = happyShift action_53
action_470 (157) = happyShift action_56
action_470 (163) = happyShift action_490
action_470 (164) = happyShift action_58
action_470 (165) = happyShift action_491
action_470 (166) = happyShift action_492
action_470 (167) = happyShift action_61
action_470 (168) = happyShift action_493
action_470 (169) = happyShift action_63
action_470 (170) = happyShift action_64
action_470 (176) = happyShift action_67
action_470 (178) = happyShift action_8
action_470 (179) = happyShift action_69
action_470 (180) = happyShift action_9
action_470 (66) = happyGoto action_108
action_470 (70) = happyGoto action_207
action_470 (71) = happyGoto action_185
action_470 (72) = happyGoto action_110
action_470 (95) = happyGoto action_111
action_470 (96) = happyGoto action_7
action_470 (97) = happyGoto action_112
action_470 (98) = happyGoto action_242
action_470 _ = happyFail

action_471 (105) = happyShift action_32
action_471 (106) = happyShift action_33
action_471 (107) = happyShift action_34
action_471 (108) = happyShift action_35
action_471 (147) = happyShift action_82
action_471 (176) = happyShift action_67
action_471 (92) = happyGoto action_489
action_471 (97) = happyGoto action_26
action_471 _ = happyFail

action_472 _ = happyReduce_169

action_473 (105) = happyShift action_186
action_473 (106) = happyShift action_33
action_473 (107) = happyShift action_34
action_473 (108) = happyShift action_35
action_473 (136) = happyShift action_187
action_473 (142) = happyShift action_113
action_473 (147) = happyShift action_114
action_473 (149) = happyShift action_115
action_473 (163) = happyShift action_116
action_473 (165) = happyShift action_117
action_473 (166) = happyShift action_118
action_473 (168) = happyShift action_119
action_473 (176) = happyShift action_67
action_473 (178) = happyShift action_8
action_473 (180) = happyShift action_9
action_473 (66) = happyGoto action_108
action_473 (67) = happyGoto action_488
action_473 (68) = happyGoto action_182
action_473 (69) = happyGoto action_183
action_473 (70) = happyGoto action_184
action_473 (71) = happyGoto action_185
action_473 (72) = happyGoto action_110
action_473 (95) = happyGoto action_111
action_473 (96) = happyGoto action_7
action_473 (97) = happyGoto action_112
action_473 _ = happyFail

action_474 (105) = happyShift action_32
action_474 (106) = happyShift action_33
action_474 (107) = happyShift action_34
action_474 (108) = happyShift action_35
action_474 (147) = happyShift action_82
action_474 (176) = happyShift action_67
action_474 (92) = happyGoto action_396
action_474 (94) = happyGoto action_487
action_474 (97) = happyGoto action_26
action_474 _ = happyFail

action_475 _ = happyReduce_17

action_476 (105) = happyShift action_32
action_476 (106) = happyShift action_33
action_476 (107) = happyShift action_34
action_476 (108) = happyShift action_35
action_476 (147) = happyShift action_82
action_476 (176) = happyShift action_67
action_476 (16) = happyGoto action_394
action_476 (17) = happyGoto action_486
action_476 (92) = happyGoto action_396
action_476 (94) = happyGoto action_397
action_476 (97) = happyGoto action_26
action_476 _ = happyReduce_282

action_477 _ = happyReduce_39

action_478 _ = happyReduce_18

action_479 (105) = happyShift action_32
action_479 (106) = happyShift action_33
action_479 (107) = happyShift action_34
action_479 (108) = happyShift action_35
action_479 (121) = happyShift action_45
action_479 (123) = happyShift action_46
action_479 (131) = happyShift action_47
action_479 (135) = happyShift action_48
action_479 (142) = happyShift action_49
action_479 (147) = happyShift action_50
action_479 (149) = happyShift action_51
action_479 (151) = happyShift action_52
action_479 (152) = happyShift action_53
action_479 (153) = happyShift action_54
action_479 (154) = happyShift action_55
action_479 (157) = happyShift action_56
action_479 (163) = happyShift action_57
action_479 (164) = happyShift action_58
action_479 (165) = happyShift action_59
action_479 (166) = happyShift action_60
action_479 (167) = happyShift action_61
action_479 (168) = happyShift action_62
action_479 (169) = happyShift action_63
action_479 (170) = happyShift action_64
action_479 (171) = happyShift action_65
action_479 (172) = happyShift action_66
action_479 (176) = happyShift action_67
action_479 (177) = happyShift action_68
action_479 (178) = happyShift action_8
action_479 (179) = happyShift action_69
action_479 (180) = happyShift action_70
action_479 (181) = happyShift action_71
action_479 (182) = happyShift action_72
action_479 (183) = happyShift action_73
action_479 (184) = happyShift action_74
action_479 (24) = happyGoto action_13
action_479 (27) = happyGoto action_14
action_479 (28) = happyGoto action_15
action_479 (29) = happyGoto action_16
action_479 (30) = happyGoto action_17
action_479 (32) = happyGoto action_391
action_479 (33) = happyGoto action_485
action_479 (51) = happyGoto action_20
action_479 (53) = happyGoto action_21
action_479 (91) = happyGoto action_23
action_479 (92) = happyGoto action_93
action_479 (95) = happyGoto action_25
action_479 (96) = happyGoto action_7
action_479 (97) = happyGoto action_26
action_479 (98) = happyGoto action_27
action_479 _ = happyReduce_282

action_480 _ = happyReduce_99

action_481 _ = happyReduce_201

action_482 (129) = happyShift action_259
action_482 (19) = happyGoto action_484
action_482 _ = happyReduce_43

action_483 _ = happyReduce_50

action_484 _ = happyReduce_44

action_485 _ = happyReduce_100

action_486 _ = happyReduce_40

action_487 _ = happyReduce_258

action_488 _ = happyReduce_38

action_489 (137) = happyShift action_554
action_489 _ = happyFail

action_490 (171) = happyReduce_172
action_490 _ = happyReduce_269

action_491 (171) = happyReduce_175
action_491 _ = happyReduce_276

action_492 (171) = happyReduce_173
action_492 _ = happyReduce_278

action_493 (171) = happyReduce_174
action_493 _ = happyReduce_271

action_494 (105) = happyShift action_186
action_494 (106) = happyShift action_33
action_494 (107) = happyShift action_34
action_494 (108) = happyShift action_35
action_494 (136) = happyShift action_187
action_494 (142) = happyShift action_113
action_494 (147) = happyShift action_114
action_494 (149) = happyShift action_115
action_494 (163) = happyShift action_116
action_494 (165) = happyShift action_117
action_494 (166) = happyShift action_118
action_494 (168) = happyShift action_119
action_494 (176) = happyShift action_67
action_494 (178) = happyShift action_8
action_494 (180) = happyShift action_9
action_494 (66) = happyGoto action_108
action_494 (67) = happyGoto action_553
action_494 (68) = happyGoto action_182
action_494 (69) = happyGoto action_183
action_494 (70) = happyGoto action_184
action_494 (71) = happyGoto action_185
action_494 (72) = happyGoto action_110
action_494 (95) = happyGoto action_111
action_494 (96) = happyGoto action_7
action_494 (97) = happyGoto action_112
action_494 _ = happyFail

action_495 _ = happyReduce_160

action_496 _ = happyReduce_159

action_497 _ = happyReduce_166

action_498 _ = happyReduce_35

action_499 (105) = happyShift action_32
action_499 (106) = happyShift action_33
action_499 (107) = happyShift action_34
action_499 (108) = happyShift action_35
action_499 (142) = happyShift action_113
action_499 (147) = happyShift action_114
action_499 (149) = happyShift action_115
action_499 (163) = happyShift action_116
action_499 (165) = happyShift action_117
action_499 (166) = happyShift action_118
action_499 (168) = happyShift action_119
action_499 (176) = happyShift action_67
action_499 (178) = happyShift action_8
action_499 (180) = happyShift action_9
action_499 (66) = happyGoto action_108
action_499 (70) = happyGoto action_552
action_499 (71) = happyGoto action_185
action_499 (72) = happyGoto action_110
action_499 (95) = happyGoto action_111
action_499 (96) = happyGoto action_7
action_499 (97) = happyGoto action_112
action_499 _ = happyFail

action_500 _ = happyReduce_205

action_501 (148) = happyShift action_551
action_501 _ = happyFail

action_502 (157) = happyShift action_550
action_502 _ = happyFail

action_503 (141) = happyShift action_548
action_503 (153) = happyShift action_549
action_503 (157) = happyReduce_251
action_503 _ = happyReduce_219

action_504 (145) = happyShift action_536
action_504 _ = happyReduce_277

action_505 (160) = happyShift action_547
action_505 _ = happyFail

action_506 (152) = happyShift action_546
action_506 _ = happyFail

action_507 (105) = happyShift action_32
action_507 (106) = happyShift action_33
action_507 (107) = happyShift action_34
action_507 (108) = happyShift action_35
action_507 (142) = happyShift action_113
action_507 (147) = happyShift action_114
action_507 (149) = happyShift action_115
action_507 (163) = happyShift action_116
action_507 (165) = happyShift action_117
action_507 (166) = happyShift action_118
action_507 (168) = happyShift action_119
action_507 (176) = happyShift action_67
action_507 (178) = happyShift action_8
action_507 (180) = happyShift action_9
action_507 (66) = happyGoto action_108
action_507 (70) = happyGoto action_545
action_507 (71) = happyGoto action_185
action_507 (72) = happyGoto action_110
action_507 (95) = happyGoto action_111
action_507 (96) = happyGoto action_7
action_507 (97) = happyGoto action_112
action_507 _ = happyFail

action_508 _ = happyReduce_235

action_509 _ = happyReduce_234

action_510 (141) = happyShift action_543
action_510 (153) = happyShift action_544
action_510 _ = happyFail

action_511 (145) = happyShift action_542
action_511 _ = happyFail

action_512 _ = happyReduce_236

action_513 (105) = happyShift action_32
action_513 (106) = happyShift action_33
action_513 (107) = happyShift action_34
action_513 (108) = happyShift action_35
action_513 (147) = happyShift action_82
action_513 (168) = happyShift action_517
action_513 (176) = happyShift action_67
action_513 (178) = happyShift action_8
action_513 (180) = happyShift action_9
action_513 (79) = happyGoto action_515
action_513 (80) = happyGoto action_541
action_513 (81) = happyGoto action_451
action_513 (92) = happyGoto action_452
action_513 (95) = happyGoto action_413
action_513 (96) = happyGoto action_7
action_513 (97) = happyGoto action_26
action_513 _ = happyFail

action_514 _ = happyReduce_218

action_515 (158) = happyShift action_540
action_515 _ = happyReduce_213

action_516 (146) = happyShift action_539
action_516 _ = happyFail

action_517 (145) = happyShift action_513
action_517 _ = happyFail

action_518 (158) = happyShift action_538
action_518 _ = happyReduce_228

action_519 (146) = happyShift action_537
action_519 _ = happyFail

action_520 (145) = happyShift action_536
action_520 _ = happyFail

action_521 _ = happyReduce_247

action_522 (105) = happyShift action_32
action_522 (106) = happyShift action_33
action_522 (107) = happyShift action_34
action_522 (108) = happyShift action_35
action_522 (142) = happyShift action_113
action_522 (147) = happyShift action_114
action_522 (149) = happyShift action_115
action_522 (163) = happyShift action_116
action_522 (165) = happyShift action_117
action_522 (166) = happyShift action_118
action_522 (168) = happyShift action_119
action_522 (176) = happyShift action_67
action_522 (178) = happyShift action_8
action_522 (180) = happyShift action_9
action_522 (66) = happyGoto action_108
action_522 (72) = happyGoto action_203
action_522 (73) = happyGoto action_535
action_522 (95) = happyGoto action_205
action_522 (96) = happyGoto action_7
action_522 (97) = happyGoto action_112
action_522 _ = happyFail

action_523 (105) = happyShift action_32
action_523 (106) = happyShift action_33
action_523 (107) = happyShift action_34
action_523 (108) = happyShift action_35
action_523 (147) = happyShift action_82
action_523 (176) = happyShift action_67
action_523 (180) = happyShift action_505
action_523 (91) = happyGoto action_534
action_523 (92) = happyGoto action_84
action_523 (97) = happyGoto action_26
action_523 _ = happyFail

action_524 (105) = happyShift action_32
action_524 (106) = happyShift action_33
action_524 (107) = happyShift action_34
action_524 (108) = happyShift action_35
action_524 (142) = happyShift action_113
action_524 (147) = happyShift action_470
action_524 (149) = happyShift action_115
action_524 (163) = happyShift action_116
action_524 (165) = happyShift action_117
action_524 (166) = happyShift action_118
action_524 (168) = happyShift action_119
action_524 (170) = happyShift action_511
action_524 (176) = happyShift action_67
action_524 (178) = happyShift action_8
action_524 (180) = happyShift action_9
action_524 (66) = happyGoto action_108
action_524 (70) = happyGoto action_532
action_524 (71) = happyGoto action_185
action_524 (72) = happyGoto action_110
action_524 (84) = happyGoto action_533
action_524 (92) = happyGoto action_510
action_524 (95) = happyGoto action_111
action_524 (96) = happyGoto action_7
action_524 (97) = happyGoto action_469
action_524 _ = happyFail

action_525 _ = happyReduce_242

action_526 (146) = happyShift action_531
action_526 _ = happyFail

action_527 (146) = happyShift action_530
action_527 _ = happyFail

action_528 _ = happyReduce_134

action_529 _ = happyReduce_135

action_530 _ = happyReduce_240

action_531 _ = happyReduce_241

action_532 _ = happyReduce_245

action_533 _ = happyReduce_244

action_534 _ = happyReduce_246

action_535 _ = happyReduce_248

action_536 (105) = happyShift action_32
action_536 (106) = happyShift action_33
action_536 (107) = happyShift action_34
action_536 (108) = happyShift action_35
action_536 (147) = happyShift action_82
action_536 (170) = happyShift action_520
action_536 (176) = happyShift action_67
action_536 (180) = happyShift action_505
action_536 (83) = happyGoto action_518
action_536 (85) = happyGoto action_568
action_536 (91) = happyGoto action_502
action_536 (92) = happyGoto action_503
action_536 (97) = happyGoto action_26
action_536 _ = happyFail

action_537 _ = happyReduce_233

action_538 (105) = happyShift action_32
action_538 (106) = happyShift action_33
action_538 (107) = happyShift action_34
action_538 (108) = happyShift action_35
action_538 (147) = happyShift action_82
action_538 (170) = happyShift action_520
action_538 (176) = happyShift action_67
action_538 (180) = happyShift action_505
action_538 (83) = happyGoto action_518
action_538 (85) = happyGoto action_567
action_538 (91) = happyGoto action_502
action_538 (92) = happyGoto action_503
action_538 (97) = happyGoto action_26
action_538 _ = happyFail

action_539 _ = happyReduce_238

action_540 (105) = happyShift action_32
action_540 (106) = happyShift action_33
action_540 (107) = happyShift action_34
action_540 (108) = happyShift action_35
action_540 (147) = happyShift action_82
action_540 (168) = happyShift action_517
action_540 (176) = happyShift action_67
action_540 (178) = happyShift action_8
action_540 (180) = happyShift action_9
action_540 (79) = happyGoto action_515
action_540 (80) = happyGoto action_566
action_540 (81) = happyGoto action_451
action_540 (92) = happyGoto action_452
action_540 (95) = happyGoto action_413
action_540 (96) = happyGoto action_7
action_540 (97) = happyGoto action_26
action_540 _ = happyFail

action_541 (146) = happyShift action_565
action_541 _ = happyFail

action_542 (105) = happyShift action_32
action_542 (106) = happyShift action_33
action_542 (107) = happyShift action_34
action_542 (108) = happyShift action_35
action_542 (147) = happyShift action_82
action_542 (170) = happyShift action_520
action_542 (176) = happyShift action_67
action_542 (180) = happyShift action_505
action_542 (83) = happyGoto action_518
action_542 (85) = happyGoto action_564
action_542 (91) = happyGoto action_502
action_542 (92) = happyGoto action_503
action_542 (97) = happyGoto action_26
action_542 _ = happyFail

action_543 (105) = happyShift action_32
action_543 (106) = happyShift action_33
action_543 (107) = happyShift action_34
action_543 (108) = happyShift action_35
action_543 (142) = happyShift action_113
action_543 (147) = happyShift action_114
action_543 (149) = happyShift action_115
action_543 (163) = happyShift action_116
action_543 (165) = happyShift action_117
action_543 (166) = happyShift action_118
action_543 (168) = happyShift action_119
action_543 (176) = happyShift action_67
action_543 (178) = happyShift action_8
action_543 (180) = happyShift action_9
action_543 (66) = happyGoto action_108
action_543 (70) = happyGoto action_563
action_543 (71) = happyGoto action_185
action_543 (72) = happyGoto action_110
action_543 (95) = happyGoto action_111
action_543 (96) = happyGoto action_7
action_543 (97) = happyGoto action_112
action_543 _ = happyFail

action_544 (105) = happyShift action_32
action_544 (106) = happyShift action_33
action_544 (107) = happyShift action_34
action_544 (108) = happyShift action_35
action_544 (147) = happyShift action_82
action_544 (176) = happyShift action_67
action_544 (180) = happyShift action_505
action_544 (91) = happyGoto action_562
action_544 (92) = happyGoto action_84
action_544 (97) = happyGoto action_26
action_544 _ = happyFail

action_545 _ = happyReduce_184

action_546 (105) = happyShift action_32
action_546 (106) = happyShift action_33
action_546 (107) = happyShift action_34
action_546 (108) = happyShift action_35
action_546 (142) = happyShift action_113
action_546 (147) = happyShift action_114
action_546 (149) = happyShift action_115
action_546 (163) = happyShift action_116
action_546 (165) = happyShift action_117
action_546 (166) = happyShift action_118
action_546 (168) = happyShift action_119
action_546 (176) = happyShift action_67
action_546 (178) = happyShift action_8
action_546 (180) = happyShift action_9
action_546 (66) = happyGoto action_108
action_546 (70) = happyGoto action_561
action_546 (71) = happyGoto action_185
action_546 (72) = happyGoto action_110
action_546 (95) = happyGoto action_111
action_546 (96) = happyGoto action_7
action_546 (97) = happyGoto action_112
action_546 _ = happyFail

action_547 (105) = happyShift action_32
action_547 (106) = happyShift action_33
action_547 (107) = happyShift action_34
action_547 (108) = happyShift action_35
action_547 (147) = happyShift action_82
action_547 (176) = happyShift action_67
action_547 (92) = happyGoto action_243
action_547 (97) = happyGoto action_26
action_547 _ = happyFail

action_548 (105) = happyShift action_32
action_548 (106) = happyShift action_33
action_548 (107) = happyShift action_34
action_548 (108) = happyShift action_35
action_548 (142) = happyShift action_113
action_548 (147) = happyShift action_114
action_548 (149) = happyShift action_115
action_548 (163) = happyShift action_116
action_548 (165) = happyShift action_117
action_548 (166) = happyShift action_118
action_548 (168) = happyShift action_119
action_548 (176) = happyShift action_67
action_548 (178) = happyShift action_8
action_548 (180) = happyShift action_9
action_548 (66) = happyGoto action_108
action_548 (70) = happyGoto action_560
action_548 (71) = happyGoto action_185
action_548 (72) = happyGoto action_110
action_548 (95) = happyGoto action_111
action_548 (96) = happyGoto action_7
action_548 (97) = happyGoto action_112
action_548 _ = happyFail

action_549 (105) = happyShift action_32
action_549 (106) = happyShift action_33
action_549 (107) = happyShift action_34
action_549 (108) = happyShift action_35
action_549 (147) = happyShift action_82
action_549 (176) = happyShift action_67
action_549 (180) = happyShift action_505
action_549 (91) = happyGoto action_559
action_549 (92) = happyGoto action_84
action_549 (97) = happyGoto action_26
action_549 _ = happyFail

action_550 (105) = happyShift action_32
action_550 (106) = happyShift action_33
action_550 (107) = happyShift action_34
action_550 (108) = happyShift action_35
action_550 (142) = happyShift action_113
action_550 (147) = happyShift action_470
action_550 (149) = happyShift action_115
action_550 (163) = happyShift action_116
action_550 (165) = happyShift action_117
action_550 (166) = happyShift action_118
action_550 (168) = happyShift action_119
action_550 (170) = happyShift action_511
action_550 (176) = happyShift action_67
action_550 (178) = happyShift action_8
action_550 (180) = happyShift action_9
action_550 (66) = happyGoto action_108
action_550 (70) = happyGoto action_557
action_550 (71) = happyGoto action_185
action_550 (72) = happyGoto action_110
action_550 (84) = happyGoto action_558
action_550 (92) = happyGoto action_510
action_550 (95) = happyGoto action_111
action_550 (96) = happyGoto action_7
action_550 (97) = happyGoto action_469
action_550 _ = happyFail

action_551 _ = happyReduce_231

action_552 _ = happyReduce_23

action_553 (158) = happyShift action_556
action_553 _ = happyFail

action_554 (105) = happyShift action_186
action_554 (106) = happyShift action_33
action_554 (107) = happyShift action_34
action_554 (108) = happyShift action_35
action_554 (136) = happyShift action_187
action_554 (142) = happyShift action_113
action_554 (147) = happyShift action_114
action_554 (149) = happyShift action_115
action_554 (163) = happyShift action_116
action_554 (165) = happyShift action_117
action_554 (166) = happyShift action_118
action_554 (168) = happyShift action_119
action_554 (176) = happyShift action_67
action_554 (178) = happyShift action_8
action_554 (180) = happyShift action_9
action_554 (66) = happyGoto action_108
action_554 (67) = happyGoto action_555
action_554 (68) = happyGoto action_182
action_554 (69) = happyGoto action_183
action_554 (70) = happyGoto action_184
action_554 (71) = happyGoto action_185
action_554 (72) = happyGoto action_110
action_554 (95) = happyGoto action_111
action_554 (96) = happyGoto action_7
action_554 (97) = happyGoto action_112
action_554 _ = happyFail

action_555 (155) = happyShift action_572
action_555 (61) = happyGoto action_571
action_555 _ = happyReduce_163

action_556 _ = happyReduce_161

action_557 _ = happyReduce_222

action_558 _ = happyReduce_221

action_559 _ = happyReduce_223

action_560 _ = happyReduce_224

action_561 _ = happyReduce_185

action_562 _ = happyReduce_226

action_563 _ = happyReduce_227

action_564 (146) = happyShift action_570
action_564 _ = happyFail

action_565 _ = happyReduce_212

action_566 _ = happyReduce_214

action_567 _ = happyReduce_229

action_568 (146) = happyShift action_569
action_568 _ = happyFail

action_569 _ = happyReduce_220

action_570 _ = happyReduce_225

action_571 (158) = happyShift action_574
action_571 _ = happyFail

action_572 (105) = happyShift action_32
action_572 (106) = happyShift action_33
action_572 (107) = happyShift action_34
action_572 (108) = happyShift action_35
action_572 (118) = happyShift action_94
action_572 (121) = happyShift action_45
action_572 (123) = happyShift action_46
action_572 (124) = happyShift action_95
action_572 (127) = happyShift action_96
action_572 (128) = happyShift action_97
action_572 (131) = happyShift action_47
action_572 (132) = happyShift action_98
action_572 (133) = happyShift action_99
action_572 (134) = happyShift action_100
action_572 (135) = happyShift action_48
action_572 (142) = happyShift action_49
action_572 (147) = happyShift action_50
action_572 (149) = happyShift action_51
action_572 (151) = happyShift action_52
action_572 (152) = happyShift action_53
action_572 (153) = happyShift action_102
action_572 (154) = happyShift action_55
action_572 (157) = happyShift action_56
action_572 (163) = happyShift action_57
action_572 (164) = happyShift action_58
action_572 (165) = happyShift action_59
action_572 (166) = happyShift action_60
action_572 (167) = happyShift action_61
action_572 (168) = happyShift action_62
action_572 (169) = happyShift action_63
action_572 (170) = happyShift action_64
action_572 (171) = happyShift action_65
action_572 (172) = happyShift action_66
action_572 (176) = happyShift action_67
action_572 (177) = happyShift action_68
action_572 (178) = happyShift action_8
action_572 (179) = happyShift action_69
action_572 (180) = happyShift action_70
action_572 (181) = happyShift action_71
action_572 (182) = happyShift action_72
action_572 (183) = happyShift action_73
action_572 (184) = happyShift action_74
action_572 (18) = happyGoto action_162
action_572 (21) = happyGoto action_88
action_572 (22) = happyGoto action_89
action_572 (23) = happyGoto action_90
action_572 (24) = happyGoto action_91
action_572 (27) = happyGoto action_14
action_572 (28) = happyGoto action_15
action_572 (29) = happyGoto action_16
action_572 (30) = happyGoto action_17
action_572 (34) = happyGoto action_573
action_572 (51) = happyGoto action_20
action_572 (53) = happyGoto action_21
action_572 (91) = happyGoto action_23
action_572 (92) = happyGoto action_93
action_572 (95) = happyGoto action_25
action_572 (96) = happyGoto action_7
action_572 (97) = happyGoto action_26
action_572 (98) = happyGoto action_27
action_572 _ = happyFail

action_573 _ = happyReduce_164

action_574 _ = happyReduce_162

happyReduce_1 = happySpecReduce_3  4 happyReduction_1
happyReduction_1 _
	(HappyAbsSyn4  happy_var_2)
	_
	 =  HappyAbsSyn4
		 (happy_var_2
	)
happyReduction_1 _ _ _  = notHappyAtAll 

happyReduce_2 = happyReduce 6 4 happyReduction_2
happyReduction_2 (_ `HappyStk`
	(HappyAbsSyn4  happy_var_5) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn10  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (PModule (spTP happy_var_1) happy_var_2 : happy_var_5
	) `HappyStk` happyRest

happyReduce_3 = happySpecReduce_2  5 happyReduction_3
happyReduction_3 _
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1
	)
happyReduction_3 _ _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_3  5 happyReduction_4
happyReduction_4 (HappyAbsSyn4  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1 ++ happy_var_3
	)
happyReduction_4 _ _ _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_2  6 happyReduction_5
happyReduction_5 (HappyAbsSyn24  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn4
		 ([PPragma (spTP happy_var_1) happy_var_2]
	)
happyReduction_5 _ _  = notHappyAtAll 

happyReduce_6 = happySpecReduce_2  6 happyReduction_6
happyReduction_6 (HappyAbsSyn10  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn4
		 ([PModule (spTP happy_var_1) happy_var_2]
	)
happyReduction_6 _ _  = notHappyAtAll 

happyReduce_7 = happySpecReduce_3  6 happyReduction_7
happyReduction_7 (HappyAbsSyn14  happy_var_3)
	_
	_
	 =  HappyAbsSyn4
		 ([happy_var_3]
	)
happyReduction_7 _ _ _  = notHappyAtAll 

happyReduce_8 = happyReduce 5 6 happyReduction_8
happyReduction_8 (_ `HappyStk`
	(HappyAbsSyn4  happy_var_4) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 (happy_var_4
	) `HappyStk` happyRest

happyReduce_9 = happySpecReduce_2  6 happyReduction_9
happyReduction_9 (HappyAbsSyn10  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn4
		 ([PImportModule (spTP happy_var_1) [happy_var_2]]
	)
happyReduction_9 _ _  = notHappyAtAll 

happyReduce_10 = happyReduce 4 6 happyReduction_10
happyReduction_10 (_ `HappyStk`
	(HappyAbsSyn11  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 ([PImportModule (spTP happy_var_1) happy_var_3]
	) `HappyStk` happyRest

happyReduce_11 = happySpecReduce_2  6 happyReduction_11
happyReduction_11 (HappyAbsSyn7  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn4
		 ([PForeign (spTP happy_var_1) happy_var_2]
	)
happyReduction_11 _ _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_3  6 happyReduction_12
happyReduction_12 (HappyAbsSyn13  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn4
		 ([PInfix (spTP happy_var_2) happy_var_1 (getCIntValue happy_var_2) happy_var_3]
	)
happyReduction_12 _ _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_1  6 happyReduction_13
happyReduction_13 (HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn4
		 ([happy_var_1]
	)
happyReduction_13 _  = notHappyAtAll 

happyReduce_14 = happyReduce 4 6 happyReduction_14
happyReduction_14 ((HappyAbsSyn65  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn91  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 ([PEffect (spTP happy_var_1) (vNameE happy_var_2) happy_var_4]
	) `HappyStk` happyRest

happyReduce_15 = happySpecReduce_2  6 happyReduction_15
happyReduction_15 (HappyAbsSyn91  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn4
		 ([PRegion (spTP happy_var_1) (vNameR happy_var_2)]
	)
happyReduction_15 _ _  = notHappyAtAll 

happyReduce_16 = happyReduce 4 6 happyReduction_16
happyReduction_16 ((HappyAbsSyn65  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn91  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 ([PClass (spTP happy_var_1) (vNameW happy_var_2) happy_var_4]
	) `HappyStk` happyRest

happyReduce_17 = happyReduce 7 6 happyReduction_17
happyReduction_17 (_ `HappyStk`
	(HappyAbsSyn17  happy_var_6) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn91  happy_var_3) `HappyStk`
	(HappyAbsSyn91  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 ([PClassDict (spTP happy_var_1) (vNameW happy_var_2) [vNameT happy_var_3] [] happy_var_6 ]
	) `HappyStk` happyRest

happyReduce_18 = happyReduce 7 6 happyReduction_18
happyReduction_18 (_ `HappyStk`
	(HappyAbsSyn33  happy_var_6) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn73  happy_var_3) `HappyStk`
	(HappyAbsSyn91  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 ([PClassInst (spTP happy_var_1) (vNameW happy_var_2) happy_var_3 [] happy_var_6 ]
	) `HappyStk` happyRest

happyReduce_19 = happyReduce 6 6 happyReduction_19
happyReduction_19 (_ `HappyStk`
	(HappyAbsSyn33  happy_var_5) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn67  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn4
		 ([PProjDict (spTP happy_var_1) happy_var_2 happy_var_5]
	) `HappyStk` happyRest

happyReduce_20 = happySpecReduce_1  6 happyReduction_20
happyReduction_20 (HappyAbsSyn32  happy_var_1)
	 =  HappyAbsSyn4
		 ([PStmt happy_var_1]
	)
happyReduction_20 _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_2  7 happyReduction_21
happyReduction_21 (HappyAbsSyn7  happy_var_2)
	_
	 =  HappyAbsSyn7
		 (OImport happy_var_2
	)
happyReduction_21 _ _  = notHappyAtAll 

happyReduce_22 = happyReduce 5 8 happyReduction_22
happyReduction_22 ((HappyAbsSyn67  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn91  happy_var_3) `HappyStk`
	(HappyAbsSyn9  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (OExtern happy_var_2 happy_var_3 happy_var_5 Nothing
	) `HappyStk` happyRest

happyReduce_23 = happyReduce 7 8 happyReduction_23
happyReduction_23 ((HappyAbsSyn67  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn67  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn91  happy_var_3) `HappyStk`
	(HappyAbsSyn9  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn7
		 (OExtern happy_var_2 happy_var_3 happy_var_5 (Just happy_var_7)
	) `HappyStk` happyRest

happyReduce_24 = happySpecReduce_0  9 happyReduction_24
happyReduction_24  =  HappyAbsSyn9
		 (Nothing
	)

happyReduce_25 = happySpecReduce_1  9 happyReduction_25
happyReduction_25 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn9
		 (Just ((\(K.CString s) -> s) (token happy_var_1))
	)
happyReduction_25 _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_1  10 happyReduction_26
happyReduction_26 (HappyAbsSyn91  happy_var_1)
	 =  HappyAbsSyn10
		 (ModuleAbsolute
	   $	(case Var.nameModule happy_var_1 of 
			ModuleAbsolute strs	-> strs
			_			-> [])
						
		++ [Var.name happy_var_1]
	)
happyReduction_26 _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_0  11 happyReduction_27
happyReduction_27  =  HappyAbsSyn11
		 ([]
	)

happyReduce_28 = happySpecReduce_1  11 happyReduction_28
happyReduction_28 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn11
		 ([happy_var_1]
	)
happyReduction_28 _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_3  11 happyReduction_29
happyReduction_29 (HappyAbsSyn11  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn11
		 (happy_var_1 : happy_var_3
	)
happyReduction_29 _ _ _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_1  12 happyReduction_30
happyReduction_30 _
	 =  HappyAbsSyn12
		 (InfixLeft
	)

happyReduce_31 = happySpecReduce_1  12 happyReduction_31
happyReduction_31 _
	 =  HappyAbsSyn12
		 (InfixRight
	)

happyReduce_32 = happySpecReduce_1  12 happyReduction_32
happyReduction_32 _
	 =  HappyAbsSyn12
		 (InfixNone
	)

happyReduce_33 = happySpecReduce_1  13 happyReduction_33
happyReduction_33 (HappyAbsSyn91  happy_var_1)
	 =  HappyAbsSyn13
		 ([happy_var_1]
	)
happyReduction_33 _  = notHappyAtAll 

happyReduce_34 = happySpecReduce_3  13 happyReduction_34
happyReduction_34 (HappyAbsSyn13  happy_var_3)
	_
	(HappyAbsSyn91  happy_var_1)
	 =  HappyAbsSyn13
		 (happy_var_1 : happy_var_3
	)
happyReduction_34 _ _ _  = notHappyAtAll 

happyReduce_35 = happyReduce 6 14 happyReduction_35
happyReduction_35 (_ `HappyStk`
	(HappyAbsSyn67  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn67  happy_var_3) `HappyStk`
	(HappyTerminal happy_var_2) `HappyStk`
	(HappyAbsSyn91  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn14
		 (PImportExtern (spTP happy_var_2) happy_var_1 happy_var_3 (Just happy_var_5)
	) `HappyStk` happyRest

happyReduce_36 = happySpecReduce_1  15 happyReduction_36
happyReduction_36 (HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn4
		 ([happy_var_1]
	)
happyReduction_36 _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_2  15 happyReduction_37
happyReduction_37 (HappyAbsSyn4  happy_var_2)
	(HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn4
		 (happy_var_1 : happy_var_2
	)
happyReduction_37 _ _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_3  16 happyReduction_38
happyReduction_38 (HappyAbsSyn67  happy_var_3)
	_
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn16
		 ((happy_var_1, happy_var_3)
	)
happyReduction_38 _ _ _  = notHappyAtAll 

happyReduce_39 = happySpecReduce_2  17 happyReduction_39
happyReduction_39 _
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn17
		 ([happy_var_1]
	)
happyReduction_39 _ _  = notHappyAtAll 

happyReduce_40 = happySpecReduce_3  17 happyReduction_40
happyReduction_40 (HappyAbsSyn17  happy_var_3)
	_
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn17
		 (happy_var_1 : happy_var_3
	)
happyReduction_40 _ _ _  = notHappyAtAll 

happyReduce_41 = happyReduce 4 18 happyReduction_41
happyReduction_41 ((HappyAbsSyn20  happy_var_4) `HappyStk`
	(HappyAbsSyn19  happy_var_3) `HappyStk`
	(HappyAbsSyn18  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn18
		 (XTry		(spTP happy_var_1) happy_var_2 happy_var_3 happy_var_4
	) `HappyStk` happyRest

happyReduce_42 = happySpecReduce_1  18 happyReduction_42
happyReduction_42 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn18
		 (happy_var_1
	)
happyReduction_42 _  = notHappyAtAll 

happyReduce_43 = happySpecReduce_0  19 happyReduction_43
happyReduction_43  =  HappyAbsSyn19
		 ([]
	)

happyReduce_44 = happyReduce 5 19 happyReduction_44
happyReduction_44 ((HappyAbsSyn19  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn19  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn19
		 (happy_var_3 ++ happy_var_5
	) `HappyStk` happyRest

happyReduce_45 = happySpecReduce_0  20 happyReduction_45
happyReduction_45  =  HappyAbsSyn20
		 (Nothing
	)

happyReduce_46 = happySpecReduce_2  20 happyReduction_46
happyReduction_46 (HappyAbsSyn18  happy_var_2)
	_
	 =  HappyAbsSyn20
		 (Just happy_var_2
	)
happyReduction_46 _ _  = notHappyAtAll 

happyReduce_47 = happySpecReduce_1  21 happyReduction_47
happyReduction_47 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn18
		 (happy_var_1
	)
happyReduction_47 _  = notHappyAtAll 

happyReduce_48 = happyReduce 4 21 happyReduction_48
happyReduction_48 ((HappyAbsSyn18  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn24  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn18
		 (XLambdaPats   (spTP happy_var_1) (toPatterns happy_var_2) happy_var_4
	) `HappyStk` happyRest

happyReduce_49 = happyReduce 4 21 happyReduction_49
happyReduction_49 ((HappyAbsSyn24  happy_var_4) `HappyStk`
	(HappyAbsSyn91  happy_var_3) `HappyStk`
	(HappyTerminal happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn18
		 (XLambdaProj   (spTP happy_var_1) (JField (spTP happy_var_2) happy_var_3) happy_var_4
	) `HappyStk` happyRest

happyReduce_50 = happyReduce 6 21 happyReduction_50
happyReduction_50 ((HappyAbsSyn18  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn18  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn18  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn18
		 (XIfThenElse 	(spTP happy_var_1) happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_51 = happySpecReduce_3  21 happyReduction_51
happyReduction_51 (HappyAbsSyn18  happy_var_3)
	(HappyAbsSyn18  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn18
		 (XWhen		(spTP happy_var_1) happy_var_2 happy_var_3
	)
happyReduction_51 _ _ _  = notHappyAtAll 

happyReduce_52 = happySpecReduce_3  21 happyReduction_52
happyReduction_52 (HappyAbsSyn18  happy_var_3)
	(HappyAbsSyn18  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn18
		 (XUnless	(spTP happy_var_1) happy_var_2 happy_var_3
	)
happyReduction_52 _ _ _  = notHappyAtAll 

happyReduce_53 = happySpecReduce_3  21 happyReduction_53
happyReduction_53 (HappyAbsSyn18  happy_var_3)
	(HappyAbsSyn18  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn18
		 (XWhile	(spTP happy_var_1) happy_var_2 happy_var_3
	)
happyReduction_53 _ _ _  = notHappyAtAll 

happyReduce_54 = happySpecReduce_1  22 happyReduction_54
happyReduction_54 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn18
		 (happy_var_1
	)
happyReduction_54 _  = notHappyAtAll 

happyReduce_55 = happyReduce 4 22 happyReduction_55
happyReduction_55 ((HappyAbsSyn18  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn33  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn18
		 (XLet		(spTP happy_var_1) happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_56 = happySpecReduce_2  22 happyReduction_56
happyReduction_56 (HappyAbsSyn18  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn18
		 (XThrow	(spTP happy_var_1) happy_var_2
	)
happyReduction_56 _ _  = notHappyAtAll 

happyReduce_57 = happySpecReduce_1  23 happyReduction_57
happyReduction_57 (HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn18
		 (case happy_var_1 of
								[s]	-> s
								(s:_)	-> XDefix (spX s) happy_var_1
	)
happyReduction_57 _  = notHappyAtAll 

happyReduce_58 = happySpecReduce_1  24 happyReduction_58
happyReduction_58 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn24
		 ([happy_var_1]
	)
happyReduction_58 _  = notHappyAtAll 

happyReduce_59 = happySpecReduce_2  24 happyReduction_59
happyReduction_59 (HappyAbsSyn24  happy_var_2)
	(HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn24
		 (happy_var_1 : happy_var_2
	)
happyReduction_59 _ _  = notHappyAtAll 

happyReduce_60 = happySpecReduce_2  24 happyReduction_60
happyReduction_60 (HappyAbsSyn24  happy_var_2)
	(HappyAbsSyn91  happy_var_1)
	 =  HappyAbsSyn24
		 (XOp (spV happy_var_1) (vNameV happy_var_1) : happy_var_2
	)
happyReduction_60 _ _  = notHappyAtAll 

happyReduce_61 = happySpecReduce_1  25 happyReduction_61
happyReduction_61 (HappyAbsSyn91  happy_var_1)
	 =  HappyAbsSyn24
		 ([XOp (spV happy_var_1) (vNameV happy_var_1)]
	)
happyReduction_61 _  = notHappyAtAll 

happyReduce_62 = happySpecReduce_1  25 happyReduction_62
happyReduction_62 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn24
		 ([happy_var_1]
	)
happyReduction_62 _  = notHappyAtAll 

happyReduce_63 = happySpecReduce_2  25 happyReduction_63
happyReduction_63 (HappyAbsSyn24  happy_var_2)
	(HappyAbsSyn91  happy_var_1)
	 =  HappyAbsSyn24
		 (XOp (spV happy_var_1) (vNameV happy_var_1) : happy_var_2
	)
happyReduction_63 _ _  = notHappyAtAll 

happyReduce_64 = happySpecReduce_2  25 happyReduction_64
happyReduction_64 (HappyAbsSyn24  happy_var_2)
	(HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn24
		 (happy_var_1 : happy_var_2
	)
happyReduction_64 _ _  = notHappyAtAll 

happyReduce_65 = happySpecReduce_0  26 happyReduction_65
happyReduction_65  =  HappyAbsSyn24
		 ([]
	)

happyReduce_66 = happySpecReduce_2  26 happyReduction_66
happyReduction_66 (HappyAbsSyn24  happy_var_2)
	(HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn24
		 (happy_var_1 : happy_var_2
	)
happyReduction_66 _ _  = notHappyAtAll 

happyReduce_67 = happySpecReduce_1  27 happyReduction_67
happyReduction_67 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn18
		 (happy_var_1
	)
happyReduction_67 _  = notHappyAtAll 

happyReduce_68 = happySpecReduce_3  27 happyReduction_68
happyReduction_68 _
	(HappyAbsSyn91  happy_var_2)
	_
	 =  HappyAbsSyn18
		 (XOp  	(spV happy_var_2) (vNameV happy_var_2)
	)
happyReduction_68 _ _ _  = notHappyAtAll 

happyReduce_69 = happySpecReduce_3  27 happyReduction_69
happyReduction_69 _
	(HappyAbsSyn91  happy_var_2)
	_
	 =  HappyAbsSyn18
		 (XOp	(spV happy_var_2) (vNameV happy_var_2)
	)
happyReduction_69 _ _ _  = notHappyAtAll 

happyReduce_70 = happySpecReduce_1  27 happyReduction_70
happyReduction_70 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn18
		 (happy_var_1
	)
happyReduction_70 _  = notHappyAtAll 

happyReduce_71 = happySpecReduce_1  27 happyReduction_71
happyReduction_71 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn18
		 (happy_var_1
	)
happyReduction_71 _  = notHappyAtAll 

happyReduce_72 = happySpecReduce_1  27 happyReduction_72
happyReduction_72 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn18
		 (happy_var_1
	)
happyReduction_72 _  = notHappyAtAll 

happyReduce_73 = happySpecReduce_1  28 happyReduction_73
happyReduction_73 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn18
		 (happy_var_1
	)
happyReduction_73 _  = notHappyAtAll 

happyReduce_74 = happyReduce 4 28 happyReduction_74
happyReduction_74 (_ `HappyStk`
	(HappyAbsSyn33  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn18
		 (XDo           (spTP happy_var_1) happy_var_3
	) `HappyStk` happyRest

happyReduce_75 = happyReduce 6 28 happyReduction_75
happyReduction_75 (_ `HappyStk`
	(HappyAbsSyn19  happy_var_5) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn18  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn18
		 (XCase 	(spTP happy_var_1) happy_var_2 happy_var_5
	) `HappyStk` happyRest

happyReduce_76 = happyReduce 4 28 happyReduction_76
happyReduction_76 (_ `HappyStk`
	(HappyAbsSyn19  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn18
		 (XMatch	(spTP happy_var_1) happy_var_3
	) `HappyStk` happyRest

happyReduce_77 = happyReduce 5 28 happyReduction_77
happyReduction_77 (_ `HappyStk`
	(HappyAbsSyn19  happy_var_4) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn18
		 (XLambdaCase	(spTP happy_var_1) happy_var_4
	) `HappyStk` happyRest

happyReduce_78 = happySpecReduce_1  28 happyReduction_78
happyReduction_78 (HappyAbsSyn91  happy_var_1)
	 =  HappyAbsSyn18
		 (XVar		(spV happy_var_1) (vNameV happy_var_1)
	)
happyReduction_78 _  = notHappyAtAll 

happyReduce_79 = happySpecReduce_3  29 happyReduction_79
happyReduction_79 _
	(HappyAbsSyn18  happy_var_2)
	_
	 =  HappyAbsSyn18
		 (happy_var_2
	)
happyReduction_79 _ _ _  = notHappyAtAll 

happyReduce_80 = happySpecReduce_3  29 happyReduction_80
happyReduction_80 (HappyAbsSyn91  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn18
		 (XProj  (spTP happy_var_2) happy_var_1 (JField  (spTP happy_var_2) happy_var_3)
	)
happyReduction_80 _ _ _  = notHappyAtAll 

happyReduce_81 = happySpecReduce_3  29 happyReduction_81
happyReduction_81 (HappyAbsSyn91  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn18
		 (XProj  (spTP happy_var_2) happy_var_1 (JFieldR (spTP happy_var_2) happy_var_3)
	)
happyReduction_81 _ _ _  = notHappyAtAll 

happyReduce_82 = happyReduce 5 29 happyReduction_82
happyReduction_82 (_ `HappyStk`
	(HappyAbsSyn67  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_2) `HappyStk`
	(HappyAbsSyn91  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn18
		 (XProjT (spTP happy_var_2) happy_var_4 (JField  (spTP happy_var_2) happy_var_1)
	) `HappyStk` happyRest

happyReduce_83 = happyReduce 5 29 happyReduction_83
happyReduction_83 (_ `HappyStk`
	(HappyAbsSyn18  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_2) `HappyStk`
	(HappyAbsSyn18  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn18
		 (XProj  (spTP happy_var_2) happy_var_1 (JIndex  (spTP happy_var_2) happy_var_4)
	) `HappyStk` happyRest

happyReduce_84 = happyReduce 5 29 happyReduction_84
happyReduction_84 (_ `HappyStk`
	(HappyAbsSyn18  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_2) `HappyStk`
	(HappyAbsSyn18  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn18
		 (XProj  (spTP happy_var_2) happy_var_1 (JIndexR (spTP happy_var_2) happy_var_4)
	) `HappyStk` happyRest

happyReduce_85 = happySpecReduce_2  29 happyReduction_85
happyReduction_85 (HappyAbsSyn91  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn18
		 (XObjVar   (spTP happy_var_1) (vNameV happy_var_2)
	)
happyReduction_85 _ _  = notHappyAtAll 

happyReduce_86 = happySpecReduce_1  29 happyReduction_86
happyReduction_86 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn18
		 (XObjField (spTP happy_var_1) (vNameF (toVar happy_var_1))
	)
happyReduction_86 _  = notHappyAtAll 

happyReduce_87 = happySpecReduce_1  29 happyReduction_87
happyReduction_87 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn18
		 (XWildCard (spTP happy_var_1)
	)
happyReduction_87 _  = notHappyAtAll 

happyReduce_88 = happySpecReduce_1  29 happyReduction_88
happyReduction_88 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn18
		 (XBreak  (spTP happy_var_1)
	)
happyReduction_88 _  = notHappyAtAll 

happyReduce_89 = happySpecReduce_1  29 happyReduction_89
happyReduction_89 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn18
		 (XVar	  (spTP happy_var_1) (makeVar "Unit" happy_var_1)
	)
happyReduction_89 _  = notHappyAtAll 

happyReduce_90 = happySpecReduce_1  29 happyReduction_90
happyReduction_90 (HappyAbsSyn91  happy_var_1)
	 =  HappyAbsSyn18
		 (XVar 	  (spV happy_var_1) (vNameV happy_var_1)
	)
happyReduction_90 _  = notHappyAtAll 

happyReduce_91 = happySpecReduce_2  30 happyReduction_91
happyReduction_91 (HappyAbsSyn31  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn18
		 (makeConst    	happy_var_2 happy_var_1
	)
happyReduction_91 _ _  = notHappyAtAll 

happyReduce_92 = happySpecReduce_2  30 happyReduction_92
happyReduction_92 (HappyAbsSyn31  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn18
		 (makeConst 	happy_var_2 happy_var_1
	)
happyReduction_92 _ _  = notHappyAtAll 

happyReduce_93 = happySpecReduce_2  30 happyReduction_93
happyReduction_93 (HappyAbsSyn31  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn18
		 (makeConst 	happy_var_2 happy_var_1
	)
happyReduction_93 _ _  = notHappyAtAll 

happyReduce_94 = happySpecReduce_2  30 happyReduction_94
happyReduction_94 (HappyAbsSyn31  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn18
		 (makeConst	happy_var_2 happy_var_1
	)
happyReduction_94 _ _  = notHappyAtAll 

happyReduce_95 = happySpecReduce_0  31 happyReduction_95
happyReduction_95  =  HappyAbsSyn31
		 (False
	)

happyReduce_96 = happySpecReduce_1  31 happyReduction_96
happyReduction_96 _
	 =  HappyAbsSyn31
		 (True
	)

happyReduce_97 = happySpecReduce_3  32 happyReduction_97
happyReduction_97 (HappyAbsSyn18  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn32
		 (SBindPats (spTP happy_var_2) (checkVar happy_var_2 $ head happy_var_1) (toPatterns $ tail happy_var_1) happy_var_3
	)
happyReduction_97 _ _ _  = notHappyAtAll 

happyReduce_98 = happySpecReduce_2  32 happyReduction_98
happyReduction_98 (HappyAbsSyn19  happy_var_2)
	(HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn32
		 (let sp	= spX (head happy_var_1)
	  in  SBindPats sp (checkVarSP sp $ head happy_var_1) (toPatterns $ tail happy_var_1) (XMatch sp happy_var_2)
	)
happyReduction_98 _ _  = notHappyAtAll 

happyReduce_99 = happySpecReduce_2  33 happyReduction_99
happyReduction_99 _
	(HappyAbsSyn32  happy_var_1)
	 =  HappyAbsSyn33
		 ([happy_var_1]
	)
happyReduction_99 _ _  = notHappyAtAll 

happyReduce_100 = happySpecReduce_3  33 happyReduction_100
happyReduction_100 (HappyAbsSyn33  happy_var_3)
	_
	(HappyAbsSyn32  happy_var_1)
	 =  HappyAbsSyn33
		 (happy_var_1 : happy_var_3
	)
happyReduction_100 _ _ _  = notHappyAtAll 

happyReduce_101 = happySpecReduce_1  34 happyReduction_101
happyReduction_101 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn18
		 (happy_var_1
	)
happyReduction_101 _  = notHappyAtAll 

happyReduce_102 = happyReduce 5 34 happyReduction_102
happyReduction_102 (_ `HappyStk`
	(HappyAbsSyn33  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_2) `HappyStk`
	(HappyAbsSyn18  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn18
		 (XWhere (spTP happy_var_2) happy_var_1 happy_var_4
	) `HappyStk` happyRest

happyReduce_103 = happySpecReduce_1  35 happyReduction_103
happyReduction_103 (HappyAbsSyn32  happy_var_1)
	 =  HappyAbsSyn32
		 (happy_var_1
	)
happyReduction_103 _  = notHappyAtAll 

happyReduce_104 = happySpecReduce_3  35 happyReduction_104
happyReduction_104 (HappyAbsSyn67  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn91  happy_var_1)
	 =  HappyAbsSyn32
		 (SSig (spTP happy_var_2) (vNameV happy_var_1) (happy_var_3)
	)
happyReduction_104 _ _ _  = notHappyAtAll 

happyReduce_105 = happySpecReduce_2  36 happyReduction_105
happyReduction_105 _
	(HappyAbsSyn32  happy_var_1)
	 =  HappyAbsSyn33
		 ([happy_var_1]
	)
happyReduction_105 _ _  = notHappyAtAll 

happyReduce_106 = happySpecReduce_3  36 happyReduction_106
happyReduction_106 (HappyAbsSyn33  happy_var_3)
	_
	(HappyAbsSyn32  happy_var_1)
	 =  HappyAbsSyn33
		 (happy_var_1 : happy_var_3
	)
happyReduction_106 _ _ _  = notHappyAtAll 

happyReduce_107 = happySpecReduce_1  37 happyReduction_107
happyReduction_107 (HappyAbsSyn32  happy_var_1)
	 =  HappyAbsSyn32
		 (happy_var_1
	)
happyReduction_107 _  = notHappyAtAll 

happyReduce_108 = happySpecReduce_1  37 happyReduction_108
happyReduction_108 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn32
		 (SStmt (spX happy_var_1) happy_var_1
	)
happyReduction_108 _  = notHappyAtAll 

happyReduce_109 = happySpecReduce_2  38 happyReduction_109
happyReduction_109 _
	(HappyAbsSyn32  happy_var_1)
	 =  HappyAbsSyn33
		 ([happy_var_1]
	)
happyReduction_109 _ _  = notHappyAtAll 

happyReduce_110 = happySpecReduce_3  38 happyReduction_110
happyReduction_110 (HappyAbsSyn33  happy_var_3)
	_
	(HappyAbsSyn32  happy_var_1)
	 =  HappyAbsSyn33
		 (happy_var_1 : happy_var_3
	)
happyReduction_110 _ _ _  = notHappyAtAll 

happyReduce_111 = happySpecReduce_2  39 happyReduction_111
happyReduction_111 _
	(HappyAbsSyn40  happy_var_1)
	 =  HappyAbsSyn19
		 ([happy_var_1]
	)
happyReduction_111 _ _  = notHappyAtAll 

happyReduce_112 = happySpecReduce_3  39 happyReduction_112
happyReduction_112 (HappyAbsSyn19  happy_var_3)
	_
	(HappyAbsSyn40  happy_var_1)
	 =  HappyAbsSyn19
		 (happy_var_1 : happy_var_3
	)
happyReduction_112 _ _ _  = notHappyAtAll 

happyReduce_113 = happySpecReduce_3  40 happyReduction_113
happyReduction_113 (HappyAbsSyn18  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn48  happy_var_1)
	 =  HappyAbsSyn40
		 (APat (spTP happy_var_2) happy_var_1 happy_var_3
	)
happyReduction_113 _ _ _  = notHappyAtAll 

happyReduce_114 = happySpecReduce_1  41 happyReduction_114
happyReduction_114 (HappyAbsSyn40  happy_var_1)
	 =  HappyAbsSyn19
		 ([happy_var_1]
	)
happyReduction_114 _  = notHappyAtAll 

happyReduce_115 = happySpecReduce_2  41 happyReduction_115
happyReduction_115 (HappyAbsSyn19  happy_var_2)
	(HappyAbsSyn40  happy_var_1)
	 =  HappyAbsSyn19
		 (happy_var_1 : happy_var_2
	)
happyReduction_115 _ _  = notHappyAtAll 

happyReduce_116 = happySpecReduce_2  42 happyReduction_116
happyReduction_116 _
	(HappyAbsSyn40  happy_var_1)
	 =  HappyAbsSyn19
		 ([happy_var_1]
	)
happyReduction_116 _ _  = notHappyAtAll 

happyReduce_117 = happySpecReduce_3  42 happyReduction_117
happyReduction_117 (HappyAbsSyn19  happy_var_3)
	_
	(HappyAbsSyn40  happy_var_1)
	 =  HappyAbsSyn19
		 (happy_var_1 : happy_var_3
	)
happyReduction_117 _ _ _  = notHappyAtAll 

happyReduce_118 = happySpecReduce_3  43 happyReduction_118
happyReduction_118 (HappyAbsSyn18  happy_var_3)
	(HappyTerminal happy_var_2)
	(HappyAbsSyn44  happy_var_1)
	 =  HappyAbsSyn40
		 (AAlt (spTP happy_var_2) happy_var_1 happy_var_3
	)
happyReduction_118 _ _ _  = notHappyAtAll 

happyReduce_119 = happySpecReduce_2  43 happyReduction_119
happyReduction_119 (HappyAbsSyn18  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn40
		 (AAlt (spTP happy_var_1) [] happy_var_2
	)
happyReduction_119 _ _  = notHappyAtAll 

happyReduce_120 = happySpecReduce_1  44 happyReduction_120
happyReduction_120 (HappyAbsSyn45  happy_var_1)
	 =  HappyAbsSyn44
		 ([happy_var_1]
	)
happyReduction_120 _  = notHappyAtAll 

happyReduce_121 = happySpecReduce_2  44 happyReduction_121
happyReduction_121 (HappyAbsSyn44  happy_var_2)
	(HappyAbsSyn45  happy_var_1)
	 =  HappyAbsSyn44
		 (happy_var_1 : happy_var_2
	)
happyReduction_121 _ _  = notHappyAtAll 

happyReduce_122 = happyReduce 4 45 happyReduction_122
happyReduction_122 ((HappyAbsSyn18  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn48  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn45
		 (GExp   (spTP happy_var_1) happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_123 = happySpecReduce_2  45 happyReduction_123
happyReduction_123 (HappyAbsSyn18  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn45
		 (GBool  (spTP happy_var_1) happy_var_2
	)
happyReduction_123 _ _  = notHappyAtAll 

happyReduce_124 = happySpecReduce_2  45 happyReduction_124
happyReduction_124 (HappyAbsSyn48  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn45
		 (GCase  (spTP happy_var_1) happy_var_2
	)
happyReduction_124 _ _  = notHappyAtAll 

happyReduce_125 = happySpecReduce_1  46 happyReduction_125
happyReduction_125 (HappyAbsSyn45  happy_var_1)
	 =  HappyAbsSyn44
		 ([happy_var_1]
	)
happyReduction_125 _  = notHappyAtAll 

happyReduce_126 = happySpecReduce_2  46 happyReduction_126
happyReduction_126 (HappyAbsSyn44  happy_var_2)
	(HappyAbsSyn45  happy_var_1)
	 =  HappyAbsSyn44
		 (happy_var_1 : happy_var_2
	)
happyReduction_126 _ _  = notHappyAtAll 

happyReduce_127 = happyReduce 4 47 happyReduction_127
happyReduction_127 ((HappyAbsSyn18  happy_var_4) `HappyStk`
	(HappyTerminal happy_var_3) `HappyStk`
	(HappyAbsSyn48  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn45
		 (GExp	 (spTP happy_var_3) happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_128 = happySpecReduce_2  47 happyReduction_128
happyReduction_128 (HappyAbsSyn18  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn45
		 (GBool	 (spTP happy_var_1) happy_var_2
	)
happyReduction_128 _ _  = notHappyAtAll 

happyReduce_129 = happySpecReduce_1  48 happyReduction_129
happyReduction_129 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn48
		 (toPattern happy_var_1
	)
happyReduction_129 _  = notHappyAtAll 

happyReduce_130 = happySpecReduce_3  48 happyReduction_130
happyReduction_130 _
	(HappyTerminal happy_var_2)
	(HappyAbsSyn91  happy_var_1)
	 =  HappyAbsSyn48
		 (WConLabel (spTP happy_var_2) happy_var_1 []
	)
happyReduction_130 _ _ _  = notHappyAtAll 

happyReduce_131 = happyReduce 4 48 happyReduction_131
happyReduction_131 (_ `HappyStk`
	(HappyAbsSyn49  happy_var_3) `HappyStk`
	(HappyTerminal happy_var_2) `HappyStk`
	(HappyAbsSyn91  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn48
		 (WConLabel (spTP happy_var_2) happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_132 = happySpecReduce_1  49 happyReduction_132
happyReduction_132 (HappyAbsSyn50  happy_var_1)
	 =  HappyAbsSyn49
		 ([happy_var_1]
	)
happyReduction_132 _  = notHappyAtAll 

happyReduce_133 = happySpecReduce_3  49 happyReduction_133
happyReduction_133 (HappyAbsSyn49  happy_var_3)
	_
	(HappyAbsSyn50  happy_var_1)
	 =  HappyAbsSyn49
		 (happy_var_1 : happy_var_3
	)
happyReduction_133 _ _ _  = notHappyAtAll 

happyReduce_134 = happyReduce 4 50 happyReduction_134
happyReduction_134 ((HappyAbsSyn91  happy_var_4) `HappyStk`
	(HappyTerminal happy_var_3) `HappyStk`
	(HappyAbsSyn91  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn50
		 ((LVar (spTP happy_var_1) happy_var_2, WVar (spTP happy_var_3) happy_var_4)
	) `HappyStk` happyRest

happyReduce_135 = happyReduce 4 50 happyReduction_135
happyReduction_135 ((HappyAbsSyn91  happy_var_4) `HappyStk`
	(HappyTerminal happy_var_3) `HappyStk`
	(HappyTerminal happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn50
		 ((LIndex (spTP happy_var_1) (getCIntValue happy_var_2), WVar (spTP happy_var_3) happy_var_4)
	) `HappyStk` happyRest

happyReduce_136 = happyReduce 5 51 happyReduction_136
happyReduction_136 (_ `HappyStk`
	(HappyAbsSyn24  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn18  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn18
		 (XTuple (spTP happy_var_1) (happy_var_2:happy_var_4)
	) `HappyStk` happyRest

happyReduce_137 = happySpecReduce_1  52 happyReduction_137
happyReduction_137 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn24
		 ([happy_var_1]
	)
happyReduction_137 _  = notHappyAtAll 

happyReduce_138 = happySpecReduce_3  52 happyReduction_138
happyReduction_138 (HappyAbsSyn24  happy_var_3)
	_
	(HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn24
		 (happy_var_1 : happy_var_3
	)
happyReduction_138 _ _ _  = notHappyAtAll 

happyReduce_139 = happySpecReduce_2  53 happyReduction_139
happyReduction_139 _
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn18
		 (XList (spTP happy_var_1) []
	)
happyReduction_139 _ _  = notHappyAtAll 

happyReduce_140 = happySpecReduce_3  53 happyReduction_140
happyReduction_140 _
	(HappyAbsSyn24  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn18
		 (XList (spTP happy_var_1) happy_var_2
	)
happyReduction_140 _ _ _  = notHappyAtAll 

happyReduce_141 = happyReduce 5 53 happyReduction_141
happyReduction_141 (_ `HappyStk`
	(HappyAbsSyn18  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn18  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn18
		 (XListRange  (spTP happy_var_1) False happy_var_2 (Just happy_var_4)
	) `HappyStk` happyRest

happyReduce_142 = happyReduce 4 53 happyReduction_142
happyReduction_142 (_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn18  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn18
		 (XListRange  (spTP happy_var_1) True  happy_var_2 Nothing
	) `HappyStk` happyRest

happyReduce_143 = happyReduce 5 53 happyReduction_143
happyReduction_143 (_ `HappyStk`
	(HappyAbsSyn55  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn18  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn18
		 (XListComp   (spTP happy_var_1) happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_144 = happySpecReduce_1  54 happyReduction_144
happyReduction_144 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn24
		 ([happy_var_1]
	)
happyReduction_144 _  = notHappyAtAll 

happyReduce_145 = happySpecReduce_3  54 happyReduction_145
happyReduction_145 (HappyAbsSyn24  happy_var_3)
	_
	(HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn24
		 (happy_var_1 : happy_var_3
	)
happyReduction_145 _ _ _  = notHappyAtAll 

happyReduce_146 = happySpecReduce_1  55 happyReduction_146
happyReduction_146 (HappyAbsSyn56  happy_var_1)
	 =  HappyAbsSyn55
		 ([happy_var_1]
	)
happyReduction_146 _  = notHappyAtAll 

happyReduce_147 = happySpecReduce_3  55 happyReduction_147
happyReduction_147 (HappyAbsSyn55  happy_var_3)
	_
	(HappyAbsSyn56  happy_var_1)
	 =  HappyAbsSyn55
		 (happy_var_1 : happy_var_3
	)
happyReduction_147 _ _ _  = notHappyAtAll 

happyReduce_148 = happySpecReduce_3  56 happyReduction_148
happyReduction_148 (HappyAbsSyn18  happy_var_3)
	_
	(HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn56
		 (LCGen False happy_var_1 happy_var_3
	)
happyReduction_148 _ _ _  = notHappyAtAll 

happyReduce_149 = happySpecReduce_3  56 happyReduction_149
happyReduction_149 (HappyAbsSyn18  happy_var_3)
	_
	(HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn56
		 (LCGen True  happy_var_1 happy_var_3
	)
happyReduction_149 _ _ _  = notHappyAtAll 

happyReduce_150 = happySpecReduce_1  56 happyReduction_150
happyReduction_150 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn56
		 (LCExp happy_var_1
	)
happyReduction_150 _  = notHappyAtAll 

happyReduce_151 = happySpecReduce_3  57 happyReduction_151
happyReduction_151 (HappyAbsSyn13  happy_var_3)
	(HappyAbsSyn91  happy_var_2)
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn14
		 (PData (spTP happy_var_1) happy_var_2 (map (vNameDefaultN NameType) happy_var_3) []
	)
happyReduction_151 _ _ _  = notHappyAtAll 

happyReduce_152 = happyReduce 5 57 happyReduction_152
happyReduction_152 ((HappyAbsSyn58  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn13  happy_var_3) `HappyStk`
	(HappyAbsSyn91  happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn14
		 (PData (spTP happy_var_1) happy_var_2 (map (vNameDefaultN NameType) happy_var_3) happy_var_5
	) `HappyStk` happyRest

happyReduce_153 = happyReduce 4 57 happyReduction_153
happyReduction_153 ((HappyAbsSyn13  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn14
		 (PData (spTP happy_var_1) (toVarHash NameType happy_var_2) (map (vNameDefaultN NameType) happy_var_4) []
	) `HappyStk` happyRest

happyReduce_154 = happyReduce 6 57 happyReduction_154
happyReduction_154 ((HappyTerminal happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn13  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyTerminal happy_var_2) `HappyStk`
	(HappyTerminal happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn14
		 (let	K.CString name	= token happy_var_6
			var		= (toVarHash NameType happy_var_2) { Var.info = [Var.ISeaName name] }
	  	in	PData (spTP happy_var_1) var (map (vNameDefaultN NameType) happy_var_4) []
	) `HappyStk` happyRest

happyReduce_155 = happySpecReduce_1  58 happyReduction_155
happyReduction_155 (HappyAbsSyn59  happy_var_1)
	 =  HappyAbsSyn58
		 ([happy_var_1]
	)
happyReduction_155 _  = notHappyAtAll 

happyReduce_156 = happySpecReduce_3  58 happyReduction_156
happyReduction_156 (HappyAbsSyn58  happy_var_3)
	_
	(HappyAbsSyn59  happy_var_1)
	 =  HappyAbsSyn58
		 (happy_var_1 : happy_var_3
	)
happyReduction_156 _ _ _  = notHappyAtAll 

happyReduce_157 = happySpecReduce_1  59 happyReduction_157
happyReduction_157 (HappyAbsSyn91  happy_var_1)
	 =  HappyAbsSyn59
		 ((happy_var_1, [])
	)
happyReduction_157 _  = notHappyAtAll 

happyReduce_158 = happySpecReduce_2  59 happyReduction_158
happyReduction_158 (HappyAbsSyn62  happy_var_2)
	(HappyAbsSyn91  happy_var_1)
	 =  HappyAbsSyn59
		 ((happy_var_1, happy_var_2)
	)
happyReduction_158 _ _  = notHappyAtAll 

happyReduce_159 = happyReduce 4 59 happyReduction_159
happyReduction_159 (_ `HappyStk`
	(HappyAbsSyn62  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn91  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn59
		 ((happy_var_1, happy_var_3)
	) `HappyStk` happyRest

happyReduce_160 = happySpecReduce_2  60 happyReduction_160
happyReduction_160 _
	(HappyAbsSyn67  happy_var_1)
	 =  HappyAbsSyn60
		 (DataField 
								{ dPrimary	= True
								, dLabel	= Nothing
								, dType		= happy_var_1
								, dInit		= Nothing }
	)
happyReduction_160 _ _  = notHappyAtAll 

happyReduce_161 = happyReduce 4 60 happyReduction_161
happyReduction_161 (_ `HappyStk`
	(HappyAbsSyn67  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn91  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn60
		 (DataField 
								{ dPrimary	= True	
								, dLabel 	= Just (vNameF happy_var_1)
								, dType		= happy_var_3
								, dInit		= Nothing }
	) `HappyStk` happyRest

happyReduce_162 = happyReduce 6 60 happyReduction_162
happyReduction_162 (_ `HappyStk`
	(HappyAbsSyn20  happy_var_5) `HappyStk`
	(HappyAbsSyn67  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn91  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn60
		 (DataField 
								{ dPrimary	= False
								, dLabel	= Just (vNameF happy_var_2)
								, dType		= happy_var_4
								, dInit		= happy_var_5 }
	) `HappyStk` happyRest

happyReduce_163 = happySpecReduce_0  61 happyReduction_163
happyReduction_163  =  HappyAbsSyn20
		 (Nothing
	)

happyReduce_164 = happySpecReduce_2  61 happyReduction_164
happyReduction_164 (HappyAbsSyn18  happy_var_2)
	_
	 =  HappyAbsSyn20
		 (Just happy_var_2
	)
happyReduction_164 _ _  = notHappyAtAll 

happyReduce_165 = happySpecReduce_1  62 happyReduction_165
happyReduction_165 (HappyAbsSyn60  happy_var_1)
	 =  HappyAbsSyn62
		 ([happy_var_1]
	)
happyReduction_165 _  = notHappyAtAll 

happyReduce_166 = happySpecReduce_2  62 happyReduction_166
happyReduction_166 (HappyAbsSyn62  happy_var_2)
	(HappyAbsSyn60  happy_var_1)
	 =  HappyAbsSyn62
		 (happy_var_1 : happy_var_2
	)
happyReduction_166 _ _  = notHappyAtAll 

happyReduce_167 = happySpecReduce_1  63 happyReduction_167
happyReduction_167 (HappyAbsSyn67  happy_var_1)
	 =  HappyAbsSyn60
		 (DataField
								{ dPrimary	= True
								, dLabel	= Nothing
								, dType		= happy_var_1
								, dInit		= Nothing }
	)
happyReduction_167 _  = notHappyAtAll 

happyReduce_168 = happySpecReduce_1  64 happyReduction_168
happyReduction_168 (HappyAbsSyn60  happy_var_1)
	 =  HappyAbsSyn62
		 ([happy_var_1]
	)
happyReduction_168 _  = notHappyAtAll 

happyReduce_169 = happySpecReduce_2  64 happyReduction_169
happyReduction_169 (HappyAbsSyn62  happy_var_2)
	(HappyAbsSyn60  happy_var_1)
	 =  HappyAbsSyn62
		 (happy_var_1 : happy_var_2
	)
happyReduction_169 _ _  = notHappyAtAll 

happyReduce_170 = happySpecReduce_1  65 happyReduction_170
happyReduction_170 (HappyAbsSyn65  happy_var_1)
	 =  HappyAbsSyn65
		 (happy_var_1
	)
happyReduction_170 _  = notHappyAtAll 

happyReduce_171 = happySpecReduce_3  65 happyReduction_171
happyReduction_171 (HappyAbsSyn65  happy_var_3)
	_
	(HappyAbsSyn65  happy_var_1)
	 =  HappyAbsSyn65
		 (KFun happy_var_1 happy_var_3
	)
happyReduction_171 _ _ _  = notHappyAtAll 

happyReduce_172 = happySpecReduce_1  66 happyReduction_172
happyReduction_172 _
	 =  HappyAbsSyn65
		 (KData
	)

happyReduce_173 = happySpecReduce_1  66 happyReduction_173
happyReduction_173 _
	 =  HappyAbsSyn65
		 (KRegion
	)

happyReduce_174 = happySpecReduce_1  66 happyReduction_174
happyReduction_174 _
	 =  HappyAbsSyn65
		 (KEffect
	)

happyReduce_175 = happySpecReduce_1  66 happyReduction_175
happyReduction_175 _
	 =  HappyAbsSyn65
		 (KFetter
	)

happyReduce_176 = happySpecReduce_1  67 happyReduction_176
happyReduction_176 (HappyAbsSyn67  happy_var_1)
	 =  HappyAbsSyn67
		 (happy_var_1
	)
happyReduction_176 _  = notHappyAtAll 

happyReduce_177 = happySpecReduce_2  67 happyReduction_177
happyReduction_177 (HappyAbsSyn67  happy_var_2)
	_
	 =  HappyAbsSyn67
		 (TElaborate happy_var_2
	)
happyReduction_177 _ _  = notHappyAtAll 

happyReduce_178 = happySpecReduce_1  68 happyReduction_178
happyReduction_178 (HappyAbsSyn67  happy_var_1)
	 =  HappyAbsSyn67
		 (happy_var_1
	)
happyReduction_178 _  = notHappyAtAll 

happyReduce_179 = happyReduce 4 68 happyReduction_179
happyReduction_179 ((HappyAbsSyn67  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn75  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn67
		 (TForall happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_180 = happySpecReduce_1  69 happyReduction_180
happyReduction_180 (HappyAbsSyn67  happy_var_1)
	 =  HappyAbsSyn67
		 (happy_var_1
	)
happyReduction_180 _  = notHappyAtAll 

happyReduce_181 = happySpecReduce_3  69 happyReduction_181
happyReduction_181 (HappyAbsSyn77  happy_var_3)
	_
	(HappyAbsSyn67  happy_var_1)
	 =  HappyAbsSyn67
		 (TFetters happy_var_3 happy_var_1
	)
happyReduction_181 _ _ _  = notHappyAtAll 

happyReduce_182 = happySpecReduce_1  70 happyReduction_182
happyReduction_182 (HappyAbsSyn67  happy_var_1)
	 =  HappyAbsSyn67
		 (happy_var_1
	)
happyReduction_182 _  = notHappyAtAll 

happyReduce_183 = happySpecReduce_3  70 happyReduction_183
happyReduction_183 (HappyAbsSyn67  happy_var_3)
	_
	(HappyAbsSyn67  happy_var_1)
	 =  HappyAbsSyn67
		 (TFun   happy_var_1 happy_var_3 pure empty
	)
happyReduction_183 _ _ _  = notHappyAtAll 

happyReduce_184 = happyReduce 7 70 happyReduction_184
happyReduction_184 ((HappyAbsSyn67  happy_var_7) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn67  happy_var_4) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn67  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn67
		 (let Just k	= takeKindOfType happy_var_4
	  in  case k of
	  	KEffect	 -> TFun happy_var_1 happy_var_7 happy_var_4 empty 
		KClosure -> TFun happy_var_1 happy_var_7 pure happy_var_4
	) `HappyStk` happyRest

happyReduce_185 = happyReduce 8 70 happyReduction_185
happyReduction_185 ((HappyAbsSyn67  happy_var_8) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn83  happy_var_5) `HappyStk`
	(HappyAbsSyn79  happy_var_4) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn67  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn67
		 (TFun   happy_var_1 happy_var_8 happy_var_4 happy_var_5
	) `HappyStk` happyRest

happyReduce_186 = happySpecReduce_1  71 happyReduction_186
happyReduction_186 (HappyAbsSyn67  happy_var_1)
	 =  HappyAbsSyn67
		 (happy_var_1
	)
happyReduction_186 _  = notHappyAtAll 

happyReduce_187 = happySpecReduce_2  71 happyReduction_187
happyReduction_187 (HappyAbsSyn73  happy_var_2)
	(HappyAbsSyn91  happy_var_1)
	 =  HappyAbsSyn67
		 (TData (vNameT  happy_var_1) happy_var_2
	)
happyReduction_187 _ _  = notHappyAtAll 

happyReduce_188 = happySpecReduce_3  71 happyReduction_188
happyReduction_188 (HappyAbsSyn73  happy_var_3)
	_
	(HappyAbsSyn91  happy_var_1)
	 =  HappyAbsSyn67
		 (TData (vNameTU happy_var_1) happy_var_3
	)
happyReduction_188 _ _ _  = notHappyAtAll 

happyReduce_189 = happySpecReduce_1  72 happyReduction_189
happyReduction_189 (HappyAbsSyn91  happy_var_1)
	 =  HappyAbsSyn67
		 (TVar 	(kindOfVarSpace (Var.nameSpace happy_var_1)) 
		(vNameDefaultN NameType happy_var_1)
	)
happyReduction_189 _  = notHappyAtAll 

happyReduce_190 = happySpecReduce_1  72 happyReduction_190
happyReduction_190 (HappyAbsSyn91  happy_var_1)
	 =  HappyAbsSyn67
		 (TData (vNameT  happy_var_1) []
	)
happyReduction_190 _  = notHappyAtAll 

happyReduce_191 = happySpecReduce_2  72 happyReduction_191
happyReduction_191 _
	(HappyAbsSyn91  happy_var_1)
	 =  HappyAbsSyn67
		 (TData (vNameTU happy_var_1) []
	)
happyReduction_191 _ _  = notHappyAtAll 

happyReduce_192 = happySpecReduce_1  72 happyReduction_192
happyReduction_192 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn67
		 (TData (vNameT $ makeVar "Unit" happy_var_1) []
	)
happyReduction_192 _  = notHappyAtAll 

happyReduce_193 = happySpecReduce_3  72 happyReduction_193
happyReduction_193 _
	(HappyAbsSyn67  happy_var_2)
	_
	 =  HappyAbsSyn67
		 (happy_var_2
	)
happyReduction_193 _ _ _  = notHappyAtAll 

happyReduce_194 = happyReduce 4 72 happyReduction_194
happyReduction_194 (_ `HappyStk`
	(HappyAbsSyn67  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn67
		 (TMutable happy_var_3
	) `HappyStk` happyRest

happyReduce_195 = happySpecReduce_2  72 happyReduction_195
happyReduction_195 _
	(HappyAbsSyn65  happy_var_1)
	 =  HappyAbsSyn67
		 (TWild	happy_var_1
	)
happyReduction_195 _ _  = notHappyAtAll 

happyReduce_196 = happySpecReduce_3  72 happyReduction_196
happyReduction_196 _
	(HappyAbsSyn67  happy_var_2)
	_
	 =  HappyAbsSyn67
		 (TData primTList [happy_var_2]
	)
happyReduction_196 _ _ _  = notHappyAtAll 

happyReduce_197 = happyReduce 5 72 happyReduction_197
happyReduction_197 (_ `HappyStk`
	(HappyAbsSyn73  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn67  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn67
		 (TData (primTTuple (length happy_var_4 + 1)) (happy_var_2:happy_var_4)
	) `HappyStk` happyRest

happyReduce_198 = happySpecReduce_1  73 happyReduction_198
happyReduction_198 (HappyAbsSyn67  happy_var_1)
	 =  HappyAbsSyn73
		 ([happy_var_1]
	)
happyReduction_198 _  = notHappyAtAll 

happyReduce_199 = happySpecReduce_2  73 happyReduction_199
happyReduction_199 (HappyAbsSyn73  happy_var_2)
	(HappyAbsSyn67  happy_var_1)
	 =  HappyAbsSyn73
		 (happy_var_1 : happy_var_2
	)
happyReduction_199 _ _  = notHappyAtAll 

happyReduce_200 = happySpecReduce_1  74 happyReduction_200
happyReduction_200 (HappyAbsSyn67  happy_var_1)
	 =  HappyAbsSyn73
		 ([happy_var_1]
	)
happyReduction_200 _  = notHappyAtAll 

happyReduce_201 = happySpecReduce_3  74 happyReduction_201
happyReduction_201 (HappyAbsSyn73  happy_var_3)
	_
	(HappyAbsSyn67  happy_var_1)
	 =  HappyAbsSyn73
		 (happy_var_1 : happy_var_3
	)
happyReduction_201 _ _ _  = notHappyAtAll 

happyReduce_202 = happySpecReduce_1  75 happyReduction_202
happyReduction_202 (HappyAbsSyn76  happy_var_1)
	 =  HappyAbsSyn75
		 ([happy_var_1]
	)
happyReduction_202 _  = notHappyAtAll 

happyReduce_203 = happySpecReduce_2  75 happyReduction_203
happyReduction_203 (HappyAbsSyn75  happy_var_2)
	(HappyAbsSyn76  happy_var_1)
	 =  HappyAbsSyn75
		 (happy_var_1 : happy_var_2
	)
happyReduction_203 _ _  = notHappyAtAll 

happyReduce_204 = happySpecReduce_1  76 happyReduction_204
happyReduction_204 (HappyAbsSyn91  happy_var_1)
	 =  HappyAbsSyn76
		 (( vNameDefaultN NameType happy_var_1
							  , kindOfVarSpace (Var.nameSpace happy_var_1))
	)
happyReduction_204 _  = notHappyAtAll 

happyReduce_205 = happyReduce 5 76 happyReduction_205
happyReduction_205 (_ `HappyStk`
	(HappyAbsSyn65  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn91  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn76
		 ((happy_var_2,	happy_var_4)
	) `HappyStk` happyRest

happyReduce_206 = happySpecReduce_1  77 happyReduction_206
happyReduction_206 (HappyAbsSyn78  happy_var_1)
	 =  HappyAbsSyn77
		 ([happy_var_1]
	)
happyReduction_206 _  = notHappyAtAll 

happyReduce_207 = happySpecReduce_3  77 happyReduction_207
happyReduction_207 (HappyAbsSyn77  happy_var_3)
	_
	(HappyAbsSyn78  happy_var_1)
	 =  HappyAbsSyn77
		 (happy_var_1 : happy_var_3
	)
happyReduction_207 _ _ _  = notHappyAtAll 

happyReduce_208 = happySpecReduce_3  78 happyReduction_208
happyReduction_208 (HappyAbsSyn67  happy_var_3)
	_
	(HappyAbsSyn91  happy_var_1)
	 =  HappyAbsSyn78
		 (FLet	(TVar (kindOfVarSpace (Var.nameSpace happy_var_1)) happy_var_1)
		happy_var_3
	)
happyReduction_208 _ _ _  = notHappyAtAll 

happyReduce_209 = happySpecReduce_2  78 happyReduction_209
happyReduction_209 (HappyAbsSyn73  happy_var_2)
	(HappyAbsSyn91  happy_var_1)
	 =  HappyAbsSyn78
		 (FConstraint (vNameW happy_var_1) happy_var_2
	)
happyReduction_209 _ _  = notHappyAtAll 

happyReduce_210 = happySpecReduce_1  79 happyReduction_210
happyReduction_210 (HappyAbsSyn91  happy_var_1)
	 =  HappyAbsSyn79
		 (TVar KEffect happy_var_1
	)
happyReduction_210 _  = notHappyAtAll 

happyReduce_211 = happySpecReduce_1  79 happyReduction_211
happyReduction_211 (HappyAbsSyn79  happy_var_1)
	 =  HappyAbsSyn79
		 (happy_var_1
	)
happyReduction_211 _  = notHappyAtAll 

happyReduce_212 = happyReduce 4 79 happyReduction_212
happyReduction_212 (_ `HappyStk`
	(HappyAbsSyn80  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn79
		 (TSum KEffect happy_var_3
	) `HappyStk` happyRest

happyReduce_213 = happySpecReduce_1  80 happyReduction_213
happyReduction_213 (HappyAbsSyn79  happy_var_1)
	 =  HappyAbsSyn80
		 ([happy_var_1]
	)
happyReduction_213 _  = notHappyAtAll 

happyReduce_214 = happySpecReduce_3  80 happyReduction_214
happyReduction_214 (HappyAbsSyn80  happy_var_3)
	_
	(HappyAbsSyn79  happy_var_1)
	 =  HappyAbsSyn80
		 (happy_var_1 : happy_var_3
	)
happyReduction_214 _ _ _  = notHappyAtAll 

happyReduce_215 = happySpecReduce_1  81 happyReduction_215
happyReduction_215 (HappyAbsSyn91  happy_var_1)
	 =  HappyAbsSyn79
		 (TEffect happy_var_1 []
	)
happyReduction_215 _  = notHappyAtAll 

happyReduce_216 = happySpecReduce_2  81 happyReduction_216
happyReduction_216 (HappyAbsSyn73  happy_var_2)
	(HappyAbsSyn91  happy_var_1)
	 =  HappyAbsSyn79
		 (TEffect happy_var_1 happy_var_2
	)
happyReduction_216 _ _  = notHappyAtAll 

happyReduce_217 = happySpecReduce_1  82 happyReduction_217
happyReduction_217 (HappyAbsSyn91  happy_var_1)
	 =  HappyAbsSyn79
		 (TVar KEffect (vNameE happy_var_1)
	)
happyReduction_217 _  = notHappyAtAll 

happyReduce_218 = happySpecReduce_3  82 happyReduction_218
happyReduction_218 _
	(HappyAbsSyn79  happy_var_2)
	_
	 =  HappyAbsSyn79
		 (happy_var_2
	)
happyReduction_218 _ _ _  = notHappyAtAll 

happyReduce_219 = happySpecReduce_1  83 happyReduction_219
happyReduction_219 (HappyAbsSyn91  happy_var_1)
	 =  HappyAbsSyn83
		 (TVar KClosure (vNameC happy_var_1)
	)
happyReduction_219 _  = notHappyAtAll 

happyReduce_220 = happyReduce 4 83 happyReduction_220
happyReduction_220 (_ `HappyStk`
	(HappyAbsSyn85  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn83
		 (TSum KClosure happy_var_3
	) `HappyStk` happyRest

happyReduce_221 = happySpecReduce_3  83 happyReduction_221
happyReduction_221 (HappyAbsSyn83  happy_var_3)
	_
	(HappyAbsSyn91  happy_var_1)
	 =  HappyAbsSyn83
		 (TFree (vNameV happy_var_1) happy_var_3
	)
happyReduction_221 _ _ _  = notHappyAtAll 

happyReduce_222 = happySpecReduce_3  83 happyReduction_222
happyReduction_222 (HappyAbsSyn67  happy_var_3)
	_
	(HappyAbsSyn91  happy_var_1)
	 =  HappyAbsSyn83
		 (TFree (vNameV happy_var_1) happy_var_3
	)
happyReduction_222 _ _ _  = notHappyAtAll 

happyReduce_223 = happySpecReduce_3  83 happyReduction_223
happyReduction_223 (HappyAbsSyn91  happy_var_3)
	_
	(HappyAbsSyn91  happy_var_1)
	 =  HappyAbsSyn83
		 (TMask   KClosure (TVar KClosure happy_var_1) (TVar KClosure happy_var_3)
	)
happyReduction_223 _ _ _  = notHappyAtAll 

happyReduce_224 = happySpecReduce_3  83 happyReduction_224
happyReduction_224 (HappyAbsSyn67  happy_var_3)
	_
	(HappyAbsSyn91  happy_var_1)
	 =  HappyAbsSyn83
		 (TDanger (TVar KRegion happy_var_1) happy_var_3
	)
happyReduction_224 _ _ _  = notHappyAtAll 

happyReduce_225 = happyReduce 4 84 happyReduction_225
happyReduction_225 (_ `HappyStk`
	(HappyAbsSyn85  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn83
		 (TSum  KClosure happy_var_3
	) `HappyStk` happyRest

happyReduce_226 = happySpecReduce_3  84 happyReduction_226
happyReduction_226 (HappyAbsSyn91  happy_var_3)
	_
	(HappyAbsSyn91  happy_var_1)
	 =  HappyAbsSyn83
		 (TMask KClosure (TVar KClosure happy_var_1) (TTag happy_var_3)
	)
happyReduction_226 _ _ _  = notHappyAtAll 

happyReduce_227 = happySpecReduce_3  84 happyReduction_227
happyReduction_227 (HappyAbsSyn67  happy_var_3)
	_
	(HappyAbsSyn91  happy_var_1)
	 =  HappyAbsSyn83
		 (TDanger (TVar KRegion happy_var_1) happy_var_3
	)
happyReduction_227 _ _ _  = notHappyAtAll 

happyReduce_228 = happySpecReduce_1  85 happyReduction_228
happyReduction_228 (HappyAbsSyn83  happy_var_1)
	 =  HappyAbsSyn85
		 ([happy_var_1]
	)
happyReduction_228 _  = notHappyAtAll 

happyReduce_229 = happySpecReduce_3  85 happyReduction_229
happyReduction_229 (HappyAbsSyn85  happy_var_3)
	_
	(HappyAbsSyn83  happy_var_1)
	 =  HappyAbsSyn85
		 (happy_var_1 : happy_var_3
	)
happyReduction_229 _ _ _  = notHappyAtAll 

happyReduce_230 = happySpecReduce_1  86 happyReduction_230
happyReduction_230 (HappyAbsSyn91  happy_var_1)
	 =  HappyAbsSyn83
		 (TVar KClosure (vNameC happy_var_1)
	)
happyReduction_230 _  = notHappyAtAll 

happyReduce_231 = happySpecReduce_3  86 happyReduction_231
happyReduction_231 _
	(HappyAbsSyn83  happy_var_2)
	_
	 =  HappyAbsSyn83
		 (happy_var_2
	)
happyReduction_231 _ _ _  = notHappyAtAll 

happyReduce_232 = happySpecReduce_1  87 happyReduction_232
happyReduction_232 (HappyAbsSyn91  happy_var_1)
	 =  HappyAbsSyn67
		 (TVar (kindOfVarSpace (Var.nameSpace happy_var_1)) happy_var_1
	)
happyReduction_232 _  = notHappyAtAll 

happyReduce_233 = happyReduce 4 87 happyReduction_233
happyReduction_233 (_ `HappyStk`
	(HappyAbsSyn85  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn67
		 (TSum KClosure happy_var_3
	) `HappyStk` happyRest

happyReduce_234 = happySpecReduce_3  87 happyReduction_234
happyReduction_234 (HappyAbsSyn83  happy_var_3)
	_
	(HappyAbsSyn91  happy_var_1)
	 =  HappyAbsSyn67
		 (TFree (vNameV happy_var_1) happy_var_3
	)
happyReduction_234 _ _ _  = notHappyAtAll 

happyReduce_235 = happySpecReduce_3  87 happyReduction_235
happyReduction_235 (HappyAbsSyn67  happy_var_3)
	_
	(HappyAbsSyn91  happy_var_1)
	 =  HappyAbsSyn67
		 (TFree (vNameV happy_var_1) happy_var_3
	)
happyReduction_235 _ _ _  = notHappyAtAll 

happyReduce_236 = happySpecReduce_3  87 happyReduction_236
happyReduction_236 (HappyAbsSyn91  happy_var_3)
	_
	(HappyAbsSyn91  happy_var_1)
	 =  HappyAbsSyn67
		 (TMask KClosure (TVar KClosure happy_var_1) (TTag happy_var_3)
	)
happyReduction_236 _ _ _  = notHappyAtAll 

happyReduce_237 = happySpecReduce_1  87 happyReduction_237
happyReduction_237 (HappyAbsSyn79  happy_var_1)
	 =  HappyAbsSyn67
		 (happy_var_1
	)
happyReduction_237 _  = notHappyAtAll 

happyReduce_238 = happyReduce 4 87 happyReduction_238
happyReduction_238 (_ `HappyStk`
	(HappyAbsSyn80  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn67
		 (TSum KEffect happy_var_3
	) `HappyStk` happyRest

happyReduce_239 = happySpecReduce_1  88 happyReduction_239
happyReduction_239 (HappyAbsSyn91  happy_var_1)
	 =  HappyAbsSyn67
		 (TVar (kindOfVarSpace (Var.nameSpace happy_var_1)) happy_var_1
	)
happyReduction_239 _  = notHappyAtAll 

happyReduce_240 = happyReduce 4 88 happyReduction_240
happyReduction_240 (_ `HappyStk`
	(HappyAbsSyn85  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn67
		 (TSum KClosure happy_var_3
	) `HappyStk` happyRest

happyReduce_241 = happyReduce 4 88 happyReduction_241
happyReduction_241 (_ `HappyStk`
	(HappyAbsSyn80  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn67
		 (TSum KEffect happy_var_3
	) `HappyStk` happyRest

happyReduce_242 = happySpecReduce_3  88 happyReduction_242
happyReduction_242 _
	(HappyAbsSyn67  happy_var_2)
	_
	 =  HappyAbsSyn67
		 (happy_var_2
	)
happyReduction_242 _ _ _  = notHappyAtAll 

happyReduce_243 = happySpecReduce_1  89 happyReduction_243
happyReduction_243 (HappyAbsSyn67  happy_var_1)
	 =  HappyAbsSyn67
		 (happy_var_1
	)
happyReduction_243 _  = notHappyAtAll 

happyReduce_244 = happySpecReduce_3  89 happyReduction_244
happyReduction_244 (HappyAbsSyn83  happy_var_3)
	_
	(HappyAbsSyn91  happy_var_1)
	 =  HappyAbsSyn67
		 (TFree (vNameV happy_var_1) happy_var_3
	)
happyReduction_244 _ _ _  = notHappyAtAll 

happyReduce_245 = happySpecReduce_3  89 happyReduction_245
happyReduction_245 (HappyAbsSyn67  happy_var_3)
	_
	(HappyAbsSyn91  happy_var_1)
	 =  HappyAbsSyn67
		 (TFree (vNameV happy_var_1) happy_var_3
	)
happyReduction_245 _ _ _  = notHappyAtAll 

happyReduce_246 = happySpecReduce_3  89 happyReduction_246
happyReduction_246 (HappyAbsSyn91  happy_var_3)
	_
	(HappyAbsSyn91  happy_var_1)
	 =  HappyAbsSyn67
		 (TMask KClosure (TVar KClosure happy_var_1) (TTag happy_var_3)
	)
happyReduction_246 _ _ _  = notHappyAtAll 

happyReduce_247 = happySpecReduce_2  89 happyReduction_247
happyReduction_247 (HappyAbsSyn73  happy_var_2)
	(HappyAbsSyn91  happy_var_1)
	 =  HappyAbsSyn67
		 (makeTECon (dNameN NameType happy_var_1) happy_var_2
	)
happyReduction_247 _ _  = notHappyAtAll 

happyReduce_248 = happySpecReduce_3  89 happyReduction_248
happyReduction_248 (HappyAbsSyn73  happy_var_3)
	_
	(HappyAbsSyn91  happy_var_1)
	 =  HappyAbsSyn67
		 (TData (vNameTU happy_var_1) happy_var_3
	)
happyReduction_248 _ _ _  = notHappyAtAll 

happyReduce_249 = happySpecReduce_1  90 happyReduction_249
happyReduction_249 (HappyAbsSyn67  happy_var_1)
	 =  HappyAbsSyn73
		 ([ happy_var_1 ]
	)
happyReduction_249 _  = notHappyAtAll 

happyReduce_250 = happySpecReduce_2  90 happyReduction_250
happyReduction_250 (HappyAbsSyn73  happy_var_2)
	(HappyAbsSyn67  happy_var_1)
	 =  HappyAbsSyn73
		 (happy_var_1 : happy_var_2
	)
happyReduction_250 _ _  = notHappyAtAll 

happyReduce_251 = happySpecReduce_1  91 happyReduction_251
happyReduction_251 (HappyAbsSyn91  happy_var_1)
	 =  HappyAbsSyn91
		 (happy_var_1
	)
happyReduction_251 _  = notHappyAtAll 

happyReduce_252 = happySpecReduce_3  91 happyReduction_252
happyReduction_252 (HappyAbsSyn91  happy_var_3)
	_
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn91
		 (happy_var_3 { Var.nameModule = makeModule happy_var_1 }
	)
happyReduction_252 _ _ _  = notHappyAtAll 

happyReduce_253 = happySpecReduce_1  92 happyReduction_253
happyReduction_253 (HappyAbsSyn91  happy_var_1)
	 =  HappyAbsSyn91
		 (happy_var_1
	)
happyReduction_253 _  = notHappyAtAll 

happyReduce_254 = happySpecReduce_3  92 happyReduction_254
happyReduction_254 _
	(HappyAbsSyn91  happy_var_2)
	_
	 =  HappyAbsSyn91
		 (happy_var_2
	)
happyReduction_254 _ _ _  = notHappyAtAll 

happyReduce_255 = happySpecReduce_0  93 happyReduction_255
happyReduction_255  =  HappyAbsSyn13
		 ([]
	)

happyReduce_256 = happySpecReduce_2  93 happyReduction_256
happyReduction_256 (HappyAbsSyn13  happy_var_2)
	(HappyAbsSyn91  happy_var_1)
	 =  HappyAbsSyn13
		 (happy_var_1 : happy_var_2
	)
happyReduction_256 _ _  = notHappyAtAll 

happyReduce_257 = happySpecReduce_1  94 happyReduction_257
happyReduction_257 (HappyAbsSyn91  happy_var_1)
	 =  HappyAbsSyn13
		 ([happy_var_1]
	)
happyReduction_257 _  = notHappyAtAll 

happyReduce_258 = happySpecReduce_3  94 happyReduction_258
happyReduction_258 (HappyAbsSyn13  happy_var_3)
	_
	(HappyAbsSyn91  happy_var_1)
	 =  HappyAbsSyn13
		 (happy_var_1 : happy_var_3
	)
happyReduction_258 _ _ _  = notHappyAtAll 

happyReduce_259 = happySpecReduce_1  95 happyReduction_259
happyReduction_259 (HappyAbsSyn91  happy_var_1)
	 =  HappyAbsSyn91
		 (happy_var_1
	)
happyReduction_259 _  = notHappyAtAll 

happyReduce_260 = happySpecReduce_3  95 happyReduction_260
happyReduction_260 (HappyAbsSyn91  happy_var_3)
	_
	(HappyTerminal happy_var_1)
	 =  HappyAbsSyn91
		 (happy_var_3 { Var.nameModule = makeModule happy_var_1 }
	)
happyReduction_260 _ _ _  = notHappyAtAll 

happyReduce_261 = happySpecReduce_1  96 happyReduction_261
happyReduction_261 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn91
		 (toVar happy_var_1
	)
happyReduction_261 _  = notHappyAtAll 

happyReduce_262 = happySpecReduce_1  97 happyReduction_262
happyReduction_262 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn91
		 (toVar happy_var_1
	)
happyReduction_262 _  = notHappyAtAll 

happyReduce_263 = happySpecReduce_1  97 happyReduction_263
happyReduction_263 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn91
		 (makeVar "elaborate" happy_var_1
	)
happyReduction_263 _  = notHappyAtAll 

happyReduce_264 = happySpecReduce_1  97 happyReduction_264
happyReduction_264 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn91
		 (makeVar "const"  happy_var_1
	)
happyReduction_264 _  = notHappyAtAll 

happyReduce_265 = happySpecReduce_1  97 happyReduction_265
happyReduction_265 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn91
		 (makeVar "mutable" happy_var_1
	)
happyReduction_265 _  = notHappyAtAll 

happyReduce_266 = happySpecReduce_1  97 happyReduction_266
happyReduction_266 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn91
		 (makeVar "extern" happy_var_1
	)
happyReduction_266 _  = notHappyAtAll 

happyReduce_267 = happySpecReduce_1  98 happyReduction_267
happyReduction_267 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn91
		 (toVar happy_var_1
	)
happyReduction_267 _  = notHappyAtAll 

happyReduce_268 = happySpecReduce_1  98 happyReduction_268
happyReduction_268 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn91
		 (toVar happy_var_1
	)
happyReduction_268 _  = notHappyAtAll 

happyReduce_269 = happySpecReduce_1  98 happyReduction_269
happyReduction_269 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn91
		 (toVar happy_var_1
	)
happyReduction_269 _  = notHappyAtAll 

happyReduce_270 = happySpecReduce_1  98 happyReduction_270
happyReduction_270 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn91
		 (toVar happy_var_1
	)
happyReduction_270 _  = notHappyAtAll 

happyReduce_271 = happySpecReduce_1  98 happyReduction_271
happyReduction_271 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn91
		 (toVar happy_var_1
	)
happyReduction_271 _  = notHappyAtAll 

happyReduce_272 = happySpecReduce_1  98 happyReduction_272
happyReduction_272 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn91
		 (toVar happy_var_1
	)
happyReduction_272 _  = notHappyAtAll 

happyReduce_273 = happySpecReduce_1  98 happyReduction_273
happyReduction_273 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn91
		 (toVar happy_var_1
	)
happyReduction_273 _  = notHappyAtAll 

happyReduce_274 = happySpecReduce_1  98 happyReduction_274
happyReduction_274 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn91
		 (toVar happy_var_1
	)
happyReduction_274 _  = notHappyAtAll 

happyReduce_275 = happySpecReduce_1  98 happyReduction_275
happyReduction_275 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn91
		 (toVar happy_var_1
	)
happyReduction_275 _  = notHappyAtAll 

happyReduce_276 = happySpecReduce_1  98 happyReduction_276
happyReduction_276 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn91
		 (toVar happy_var_1
	)
happyReduction_276 _  = notHappyAtAll 

happyReduce_277 = happySpecReduce_1  98 happyReduction_277
happyReduction_277 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn91
		 (toVar happy_var_1
	)
happyReduction_277 _  = notHappyAtAll 

happyReduce_278 = happySpecReduce_1  98 happyReduction_278
happyReduction_278 (HappyTerminal happy_var_1)
	 =  HappyAbsSyn91
		 (toVar happy_var_1
	)
happyReduction_278 _  = notHappyAtAll 

happyReduce_279 = happySpecReduce_1  99 happyReduction_279
happyReduction_279 _
	 =  HappyAbsSyn99
		 (()
	)

happyReduce_280 = happySpecReduce_2  99 happyReduction_280
happyReduction_280 _
	_
	 =  HappyAbsSyn99
		 (()
	)

happyReduce_281 = happySpecReduce_0  100 happyReduction_281
happyReduction_281  =  HappyAbsSyn99
		 (()
	)

happyReduce_282 = happySpecReduce_1  100 happyReduction_282
happyReduction_282 _
	 =  HappyAbsSyn99
		 (()
	)

happyNewToken action sts stk [] =
	action 185 185 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TokenP { token = K.Pragma	} -> cont 101;
	TokenP { token = K.Foreign	} -> cont 102;
	TokenP { token = K.Import	} -> cont 103;
	TokenP { token = K.Module	} -> cont 104;
	TokenP { token = K.Elaborate	} -> cont 105;
	TokenP { token = K.Const	} -> cont 106;
	TokenP { token = K.Mutable	} -> cont 107;
	TokenP { token = K.Extern	} -> cont 108;
	TokenP { token = K.Data	} -> cont 109;
	TokenP { token = K.Region	} -> cont 110;
	TokenP { token = K.Effect     } -> cont 111;
	TokenP { token = K.Class	} -> cont 112;
	TokenP { token = K.Instance   } -> cont 113;
	TokenP { token = K.Project	} -> cont 114;
	TokenP { token = K.InfixR	} -> cont 115;
	TokenP { token = K.InfixL	} -> cont 116;
	TokenP { token = K.Infix	} -> cont 117;
	TokenP { token = K.Let	} -> cont 118;
	TokenP { token = K.In		} -> cont 119;
	TokenP { token = K.Where	} -> cont 120;
	TokenP { token = K.Case	} -> cont 121;
	TokenP { token = K.Of		} -> cont 122;
	TokenP { token = K.Match	} -> cont 123;
	TokenP { token = K.If		} -> cont 124;
	TokenP { token = K.Then	} -> cont 125;
	TokenP { token = K.Else	} -> cont 126;
	TokenP { token = K.Throw	} -> cont 127;
	TokenP { token = K.Try	} -> cont 128;
	TokenP { token = K.Catch	} -> cont 129;
	TokenP { token = K.With	} -> cont 130;
	TokenP { token = K.Do		} -> cont 131;
	TokenP { token = K.While	} -> cont 132;
	TokenP { token = K.When	} -> cont 133;
	TokenP { token = K.Unless	} -> cont 134;
	TokenP { token = K.Break	} -> cont 135;
	TokenP { token = K.Forall	} -> cont 136;
	TokenP { token = K.HasType	   } -> cont 137;
	TokenP { token = K.HasOpType	   } -> cont 138;
	TokenP { token = K.HasConstraint } -> cont 139;
	TokenP { token = K.RightArrow } -> cont 140;
	TokenP { token = K.HoldsMono } -> cont 141;
	TokenP { token = K.Unit	} -> cont 142;
	TokenP { token = K.GuardCase } -> cont 143;
	TokenP { token = K.GuardDefault } -> cont 144;
	TokenP { token = K.CBra	} -> cont 145;
	TokenP { token = K.CKet	} -> cont 146;
	TokenP { token = K.RBra	} -> cont 147;
	TokenP { token = K.RKet	} -> cont 148;
	TokenP { token = K.SBra	} -> cont 149;
	TokenP { token = K.SKet	} -> cont 150;
	TokenP { token = K.ABra	} -> cont 151;
	TokenP { token = K.AKet	} -> cont 152;
	TokenP { token = K.BackSlash	} -> cont 153;
	TokenP { token = K.BackTick	} -> cont 154;
	TokenP { token = K.Equals	} -> cont 155;
	TokenP { token = K.Comma	} -> cont 156;
	TokenP { token = K.Colon	} -> cont 157;
	TokenP { token = K.SemiColon	} -> cont 158;
	TokenP { token = K.Bar	} -> cont 159;
	TokenP { token = K.Dot	} -> cont 160;
	TokenP { token = K.And	} -> cont 161;
	TokenP { token = K.Hash	} -> cont 162;
	TokenP { token = K.Star	} -> cont 163;
	TokenP { token = K.Dash	} -> cont 164;
	TokenP { token = K.Plus	} -> cont 165;
	TokenP { token = K.Percent	} -> cont 166;
	TokenP { token = K.At		} -> cont 167;
	TokenP { token = K.Bang	} -> cont 168;
	TokenP { token = K.ForwardSlash } -> cont 169;
	TokenP { token = K.Dollar	} -> cont 170;
	TokenP { token = K.Underscore	} -> cont 171;
	TokenP { token = K.Hat	} -> cont 172;
	TokenP { token = K.DotDot	} -> cont 173;
	TokenP { token = K.LeftArrow	} -> cont 174;
	TokenP { token = K.LeftArrowLazy } -> cont 175;
	TokenP { token = K.Var    _	} -> cont 176;
	TokenP { token = K.VarField _ } -> cont 177;
	TokenP { token = K.Con    _   } -> cont 178;
	TokenP { token = K.Symbol _	} -> cont 179;
	TokenP { token = K.ModuleName _ } -> cont 180;
	TokenP { token = K.CInt    _	} -> cont 181;
	TokenP { token = K.CChar   _  } -> cont 182;
	TokenP { token = K.CFloat  _	} -> cont 183;
	TokenP { token = K.CString _	} -> cont 184;
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
happyError' :: () => [TokenP] -> HappyIdentity a
happyError' = HappyIdentity . happyError

parse tks = happyRunIdentity happySomeParser where
  happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


-- Start of Happy Haskell code


-- | Callback for happy to use when it's not happy.
happyError ::	[TokenP] -> a
happyError	[]	= dieWithUserError [ErrorParseEnd]
happyError	(x:xs)	= dieWithUserError [ErrorParseBefore (x:xs)]


-- | Convert a token to a variable
--	We need to undo the lexer tokens here because some of our 
--	"reserved symbols" alias with valid variable names.
--
toVar :: TokenP -> Var
toVar	 tok
 = case token tok of
	K.Var    	name	-> Var.loadSpaceQualifier $ makeVar name tok
	K.VarField	name	-> vNameF $ makeVar name tok
	K.Con		name	-> Var.loadSpaceQualifier $ makeVar name tok
	K.Symbol 	name	-> makeVar name tok
	_ -> case lookup (token tok) toVar_table of
		Just name	-> makeVar name tok
		Nothing		-> panic stage ("toVar: bad token: " ++ show tok)


-- | String representations for these tokens.
toVar_table :: [(Token, String)]
toVar_table = 
	[ (K.Colon,		":")
	, (K.Star,		"*")
	, (K.Dash,		"-")
	, (K.At,		"@")
	, (K.Hash,		"#") 
	, (K.ABra,		"<")
	, (K.AKet,		">") 
	, (K.ForwardSlash,	"/")
	, (K.Plus,		"+")
	, (K.Dot,		".")
	, (K.Dollar,		"$")
	, (K.Tilde,		"~")
	, (K.Percent,		"%") ]

toVarHash space tok
 = let	v	= toVar tok
   in	v	{ Var.name	= (Var.name v ++ "#")
   		, Var.nameSpace	= space }


-- | Make a variable with this name,
--	using the token as the source location for the var.
makeVar :: String -> TokenP -> Var
makeVar    name@(n:_) tok
	= (Var.new name)
	 	{ Var.info	=	
		[ Var.ISourcePos (SourcePos (tokenFile tok, tokenLine tok, tokenColumn tok)) ] }

-- | Make either a TData or TEffect constructor, depending on the namespace of the variable
makeTECon :: Var -> [Type] -> Type
makeTECon v ts
 = case Var.nameSpace v of
 	NameType	-> TData v ts
	NameEffect	-> TEffect v ts


-- | Make a module name from this token
makeModule :: TokenP -> Module
makeModule tok
 = case token tok of
 	K.ModuleName names	-> ModuleAbsolute names
	K.Con        name	-> ModuleAbsolute [name]
	_			-> dieWithUserError [ ErrorParse tok "parse error" ]

-- | Force an expresion to be a variable
--	Throw a user error if it's not.
checkVar ::	TokenP -> Exp SP -> Var
checkVar	tok	  (XVar sp v)	= v
checkVar	tok	  e
 	= dieWithUserError [ErrorParse tok "parse error"]

checkVarSP ::	SourcePos -> Exp SP -> Var
checkVarSP	sp'	  (XVar sp v)	= v
checkVarSP	sp'	  e
 	= dieWithUserError [ErrorParsePos sp' "parse error"]


-- | Make a constant expression from this token
makeConst ::	Bool -> TokenP -> Exp SP
makeConst	isUnboxed tok
 = let	sp	= SourcePos (tokenFile tok, tokenLine tok, tokenColumn tok)
   in   if isUnboxed 
   		then XConst sp $ CConstU $ makeLit tok
		else XConst sp $ CConst  $ makeLit tok
  
  
-- Make a literal from this token.
makeLit :: TokenP -> Literal
makeLit tok 
 = case token tok of
 	K.CInt    i	-> LInt    i
	K.CChar   c	-> LChar   c
	K.CFloat  f	-> LFloat  f
	K.CString s	-> LString s
 

-----
getCIntValue ::	TokenP -> Int
getCIntValue	tok
 = case token tok of
 	K.CInt   i	-> i


-------------------------------------------------------------------
-- Helper functions to use when creating the syntax tree
--

-- | Slurp the source position from this token.
spTP :: TokenP -> SP
spTP    tok
 = SourcePos (tokenFile tok, tokenLine tok, tokenColumn tok)


-- | Slurp the source position from this expression.
spX :: Exp SP -> SP
spX 	= sourcePosX


-- | Slurp the source position from this variable.
spV :: Var -> SP
spV var
 = let	[sp]	= [sp | Var.ISourcePos sp <- Var.info var]
   in	sp

-- | Force the namespace of this variable
--	If it has already been set differently then panic
vNameN :: NameSpace -> Var -> Var
vNameN space v
	-- var has no namespace, so give it one
	| Var.nameSpace v == NameNothing
	= v { Var.nameSpace = space }

	-- var had a different namespace, oh oh.
	| Var.nameSpace v /= space
	= panic stage
	$ "vNameN: conflicting namespace for variable " % v	% "\n"
	% "   name space was     " % Var.nameSpace v		% "\n"
	% "   tried to set it to " % space			% "\n"
	
	-- var already has the right namespace
	| otherwise
	= v 

vNameV		= vNameN NameValue
vNameT		= vNameN NameType
vNameR		= vNameN NameRegion
vNameE		= vNameN NameEffect
vNameC		= vNameN NameClosure
vNameW		= vNameN NameClass
vNameF		= vNameN NameField

vNameTU v	= v 
		{ Var.name 	= (Var.name v ++ "#")
   		, Var.nameSpace = NameType }

-- | If the namespace of this var is NameNothing, set it to this one
dNameN :: NameSpace -> Var -> Var
dNameN space v
 	| Var.nameSpace v == NameNothing
	= v { Var.nameSpace = space }
	
	| otherwise
	= v


-- | If the var has no namespace set, then give it this one.
vNameDefaultN	:: NameSpace -> Var -> Var
vNameDefaultN space var
 = case Var.nameSpace var of
 	NameNothing	-> var { Var.nameSpace = space }
	_		-> var



-- | Decide on the kind of a type var from it's namespace
kindOfVarSpace :: NameSpace -> Kind
kindOfVarSpace space
 = case space of
 	NameNothing	-> KData
	NameRegion	-> KRegion
	NameEffect	-> KEffect
	NameClosure	-> KClosure


-- | Rewrite expression on the LHS of a binding to patterns.
--	Patterns on the LHS of function bindings are initially parsed as expressions
--	due to limiations with the LR(1) parser.
--
--	Namely, when parsing some expression:
--
--	     fun   (x, y) 5 ...
--	         ^
--	When we're at the point marked with the ^, we don't know whether the following
--	(x, y) is going to be a pattern which matches a tuple, or an expression which 
--	constructs one. For example, we might be parsing:
--
--		do { fun (x, y) 5; }			-- apply fun to two arguments
--	or
--		do { fun (x, y) 5 = x + y;  ... }	-- define a binding called fun
--
--	We won't know which option to take until reach the '=' (or the ';'), and
--	LR(1) only gives us one token look-ahead, so we can't scan ahead to check.
--
--	In lesser languages, the programmer lets the parser know what option to take 
--	with with a def or var keyword
--
--	ie, 
--		do { fun (x, y) 5; }
--		do { def fun (x, y) 5 = x + y; ... }
--
--	but we're not interested in that slackness here.
--
--	The solution we take is to parse the patterns in both options as
--	expressions, then convert the LHS to real patterns during the production.
--
toPatterns :: [Exp SP] -> [Pat SP]
toPatterns xx
 {- = trace (pprStrPlain 
   		$ "toPatterns\n"
   		% "    xx = " % xx % "\n") -}
 =		(toPatterns' xx)

toPatterns' []	= []

-- The at in at-patterns is parsed as an infix op.
toPatterns' (XVar sp1 v : XOp sp2 vAt : x2 : xs)
	| Var.name vAt == "@"
	= WAt sp2 v (toPattern x2) : toPatterns xs
	
toPatterns' (x:xs)
	= toPattern x : toPatterns' xs


-- convert an expression to a pattern
toPattern :: Exp SP -> Pat SP
toPattern x
{-  = trace (pprStrPlain
   		$ "toPattern\n"
		% "    x = " % x % "\n") -}
 =		(toPattern' x)

toPattern' x
 = case x of
	XVar sp v
	  | Var.isCtorName v	-> WCon sp v []
	  | otherwise		-> WVar sp v 

	XObjVar sp v		-> WObjVar sp v

	XApp sp _ _
	 -> let	(XVar sp v : xs)	= flattenApps x
	    in	if Var.isCtorName v
	    		then WCon sp v (toPatterns xs)
			else panic stage
				$ "toPatterns: parse error in pattern " % x  % "\n"
	  
 	XTuple sp xx		-> WTuple sp (toPatterns xx)
	XList  sp xx		-> WList  sp (toPatterns xx)
	XConst sp c		-> WConst sp c
	XWildCard sp 		-> WWildcard sp
	
	-- We need to handle infix uses of '@' and ':' here because Source.Defix
	--	needs a renamed tree, and the renamer doesn't handle expresison
	--	patterns. This isn't as general as Source.Defix though as all the
	--	infix constructors have the same precedence.
	XDefix sp [xx]		-> toPattern xx

	XDefix sp (XVar sp1 v : XOp sp2 vAt : x2 : [])		
	 | Var.name vAt == "@"
	 -> WAt sp2 v (toPattern x2) 

	XDefix sp (x1 : XOp sp1 v : xs)		
	 | Var.name v == ":"
	 -> WCons sp (toPattern x1) (toPattern (XDefix sp xs))

	XDefix sp (x1 : x2 : xs)
	 -> toPattern $ XDefix sp (XApp sp x1 x2 : xs)

	_	-> panic stage
		$ "toPatterns: parse error in pattern " % x  % "\n"
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 28 "templates/GenericTemplate.hs" #-}








{-# LINE 49 "templates/GenericTemplate.hs" #-}

{-# LINE 59 "templates/GenericTemplate.hs" #-}

{-# LINE 68 "templates/GenericTemplate.hs" #-}

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

{-# LINE 155 "templates/GenericTemplate.hs" #-}

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
       where sts1@(((st1@(HappyState (action))):(_))) = happyDrop k ((st):(sts))
             drop_stk = happyDropStk k stk

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
       happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))
       where sts1@(((st1@(HappyState (action))):(_))) = happyDrop k ((st):(sts))
             drop_stk = happyDropStk k stk





             new_state = action


happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 253 "templates/GenericTemplate.hs" #-}
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

{-# LINE 317 "templates/GenericTemplate.hs" #-}
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
