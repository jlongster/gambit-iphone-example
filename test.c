#ifdef ___LINKER_INFO
; File: "test.c", produced by Gambit-C v4.5.2
(
405002
" test.o1"
(" test.o1")
(
)
(
)
(
" test.o1"
)
(
"a"
)
(
)
 #f
)
#else
#define ___VERSION 405002
#define ___MODULE_NAME " test.o1"
#define ___LINKER_ID ____20_test_2e_o1
#define ___MH_PROC ___H__20_test_2e_o1
#define ___SCRIPT_LINE 0
#define ___GLO_COUNT 2
#define ___SUP_COUNT 2
#define ___LBL_COUNT 2
#include "gambit.h"

___NEED_GLO(___G__20_test_2e_o1)
___NEED_GLO(___G_a)

___BEGIN_GLO
___DEF_GLO(0," test.o1")
___DEF_GLO(1,"a")
___END_GLO


#undef ___MD_ALL
#define ___MD_ALL ___D_R0 ___D_R1
#undef ___MR_ALL
#define ___MR_ALL ___R_R0 ___R_R1
#undef ___MW_ALL
#define ___MW_ALL ___W_R1
___BEGIN_M_COD
___BEGIN_M_HLBL
___DEF_M_HLBL_INTRO
___DEF_M_HLBL(___L0__20_test_2e_o1)
___END_M_HLBL

___BEGIN_M_SW

#undef ___PH_PROC
#define ___PH_PROC ___H__20_test_2e_o1
#undef ___PH_LBL0
#define ___PH_LBL0 1
#undef ___PD_ALL
#define ___PD_ALL ___D_R0 ___D_R1
#undef ___PR_ALL
#define ___PR_ALL ___R_R0 ___R_R1
#undef ___PW_ALL
#define ___PW_ALL ___W_R1
___BEGIN_P_COD
___BEGIN_P_HLBL
___DEF_P_HLBL_INTRO
___DEF_P_HLBL(___L0__20_test_2e_o1)
___END_P_HLBL
___BEGIN_P_SW
___DEF_SLBL(0,___L0__20_test_2e_o1)
   ___IF_NARGS_EQ(0,___NOTHING)
   ___WRONG_NARGS(0,0,0,0)
___DEF_GLBL(___L__20_test_2e_o1)
   ___SET_GLO(1,___G_a,___FIX(5L))
   ___SET_R1(___VOID)
   ___JUMPPRM(___NOTHING,___R0)
___END_P_SW
___END_P_COD

___END_M_SW
___END_M_COD

___BEGIN_LBL
 ___DEF_LBL_INTRO(___H__20_test_2e_o1," test.o1",___REF_FAL,1,0)
,___DEF_LBL_PROC(___H__20_test_2e_o1,0,0)
___END_LBL

___BEGIN_MOD1
___DEF_PRM(0,___G__20_test_2e_o1,1)
___END_MOD1

___BEGIN_MOD2
___END_MOD2

#endif
