/* A Bison parser, made by GNU Bison 3.0.4.  */

/* Bison implementation for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015 Free Software Foundation, Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* C LALR(1) parser skeleton written by Richard Stallman, by
   simplifying the original so-called "semantic" parser.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output.  */
#define YYBISON 1

/* Bison version.  */
#define YYBISON_VERSION "3.0.4"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1




/* Copy the first part of user declarations.  */
#line 1 "DEMOgram.y" /* yacc.c:339  */
 
/* Copyright 2016, Keith D. Cooper & Linda Torczon
 * 
 * Written at Rice University, Houston, Texas as part
 * of the instructional materials for COMP 506.
 *
 * The University may have some rights in this work.
 *
 */

#define YYERROR_VERBOSE


#include <stdio.h>
#include "demo.h"

int yylineno;
char *yytext;
struct NODE * children[CHILD_NUM];


#line 88 "DEMOgram.tab.c" /* yacc.c:339  */

# ifndef YY_NULLPTR
#  if defined __cplusplus && 201103L <= __cplusplus
#   define YY_NULLPTR nullptr
#  else
#   define YY_NULLPTR 0
#  endif
# endif

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
# undef YYERROR_VERBOSE
# define YYERROR_VERBOSE 1
#else
# define YYERROR_VERBOSE 0
#endif

/* In a future release of Bison, this section will be replaced
   by #include "DEMOgram.tab.h".  */
#ifndef YY_YY_DEMOGRAM_TAB_H_INCLUDED
# define YY_YY_DEMOGRAM_TAB_H_INCLUDED
/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 1
#endif
#if YYDEBUG
extern int yydebug;
#endif

/* Token type.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
    BY = 258,
    CHAR = 259,
    CHARCONST = 260,
    COLON = 261,
    COMMA = 262,
    DIVIDE = 263,
    ELSE = 264,
    ENDOFFILE = 265,
    EQUALS = 266,
    FOR = 267,
    IF = 268,
    INT = 269,
    LBRACKET = 270,
    LPAREN = 271,
    LSQUARE = 272,
    MINUS = 273,
    NAME = 274,
    ELE = 275,
    NOT = 276,
    NUMBER = 277,
    PLUS = 278,
    PROCEDURE = 279,
    RBRACKET = 280,
    READ = 281,
    RPAREN = 282,
    RSQUARE = 283,
    SEMICOLON = 284,
    THEN = 285,
    TIMES = 286,
    TO = 287,
    WHILE = 288,
    WRITE = 289,
    UNKNOWN = 290,
    STRING = 291,
    LT = 292,
    LE = 293,
    EQ = 294,
    NE = 295,
    GE = 296,
    GT = 297,
    AND = 298,
    OR = 299
  };
#endif

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED

union YYSTYPE
{
#line 23 "DEMOgram.y" /* yacc.c:355  */

  unsigned long int val;
  char             *cptr;
  struct NODE     *nodeptr;
  struct BP        *bp;
  struct LOOP      *loop;
  struct LOOPEXPR  *loopex;
  struct ITEHEAD   *ite;

#line 183 "DEMOgram.tab.c" /* yacc.c:355  */
};

typedef union YYSTYPE YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE yylval;

int yyparse (void);

#endif /* !YY_YY_DEMOGRAM_TAB_H_INCLUDED  */

/* Copy the second part of user declarations.  */

#line 200 "DEMOgram.tab.c" /* yacc.c:358  */

#ifdef short
# undef short
#endif

#ifdef YYTYPE_UINT8
typedef YYTYPE_UINT8 yytype_uint8;
#else
typedef unsigned char yytype_uint8;
#endif

#ifdef YYTYPE_INT8
typedef YYTYPE_INT8 yytype_int8;
#else
typedef signed char yytype_int8;
#endif

#ifdef YYTYPE_UINT16
typedef YYTYPE_UINT16 yytype_uint16;
#else
typedef unsigned short int yytype_uint16;
#endif

#ifdef YYTYPE_INT16
typedef YYTYPE_INT16 yytype_int16;
#else
typedef short int yytype_int16;
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif ! defined YYSIZE_T
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned int
# endif
#endif

#define YYSIZE_MAXIMUM ((YYSIZE_T) -1)

#ifndef YY_
# if defined YYENABLE_NLS && YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(Msgid) dgettext ("bison-runtime", Msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(Msgid) Msgid
# endif
#endif

#ifndef YY_ATTRIBUTE
# if (defined __GNUC__                                               \
      && (2 < __GNUC__ || (__GNUC__ == 2 && 96 <= __GNUC_MINOR__)))  \
     || defined __SUNPRO_C && 0x5110 <= __SUNPRO_C
#  define YY_ATTRIBUTE(Spec) __attribute__(Spec)
# else
#  define YY_ATTRIBUTE(Spec) /* empty */
# endif
#endif

#ifndef YY_ATTRIBUTE_PURE
# define YY_ATTRIBUTE_PURE   YY_ATTRIBUTE ((__pure__))
#endif

#ifndef YY_ATTRIBUTE_UNUSED
# define YY_ATTRIBUTE_UNUSED YY_ATTRIBUTE ((__unused__))
#endif

#if !defined _Noreturn \
     && (!defined __STDC_VERSION__ || __STDC_VERSION__ < 201112)
# if defined _MSC_VER && 1200 <= _MSC_VER
#  define _Noreturn __declspec (noreturn)
# else
#  define _Noreturn YY_ATTRIBUTE ((__noreturn__))
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YYUSE(E) ((void) (E))
#else
# define YYUSE(E) /* empty */
#endif

#if defined __GNUC__ && 407 <= __GNUC__ * 100 + __GNUC_MINOR__
/* Suppress an incorrect diagnostic about yylval being uninitialized.  */
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN \
    _Pragma ("GCC diagnostic push") \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")\
    _Pragma ("GCC diagnostic ignored \"-Wmaybe-uninitialized\"")
# define YY_IGNORE_MAYBE_UNINITIALIZED_END \
    _Pragma ("GCC diagnostic pop")
#else
# define YY_INITIAL_VALUE(Value) Value
#endif
#ifndef YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_END
#endif
#ifndef YY_INITIAL_VALUE
# define YY_INITIAL_VALUE(Value) /* Nothing. */
#endif


#if ! defined yyoverflow || YYERROR_VERBOSE

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   elif defined __BUILTIN_VA_ARG_INCR
#    include <alloca.h> /* INFRINGES ON USER NAME SPACE */
#   elif defined _AIX
#    define YYSTACK_ALLOC __alloca
#   elif defined _MSC_VER
#    include <malloc.h> /* INFRINGES ON USER NAME SPACE */
#    define alloca _alloca
#   else
#    define YYSTACK_ALLOC alloca
#    if ! defined _ALLOCA_H && ! defined EXIT_SUCCESS
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
      /* Use EXIT_SUCCESS as a witness for stdlib.h.  */
#     ifndef EXIT_SUCCESS
#      define EXIT_SUCCESS 0
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's 'empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (0)
#  ifndef YYSTACK_ALLOC_MAXIMUM
    /* The OS might guarantee only one guard page at the bottom of the stack,
       and a page size can be as small as 4096 bytes.  So we cannot safely
       invoke alloca (N) if N exceeds 4096.  Use a slightly smaller number
       to allow for a few compiler-allocated temporary stack slots.  */
#   define YYSTACK_ALLOC_MAXIMUM 4032 /* reasonable circa 2006 */
#  endif
# else
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
#  ifndef YYSTACK_ALLOC_MAXIMUM
#   define YYSTACK_ALLOC_MAXIMUM YYSIZE_MAXIMUM
#  endif
#  if (defined __cplusplus && ! defined EXIT_SUCCESS \
       && ! ((defined YYMALLOC || defined malloc) \
             && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef EXIT_SUCCESS
#    define EXIT_SUCCESS 0
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined EXIT_SUCCESS
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined EXIT_SUCCESS
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* ! defined yyoverflow || YYERROR_VERBOSE */


#if (! defined yyoverflow \
     && (! defined __cplusplus \
         || (defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yytype_int16 yyss_alloc;
  YYSTYPE yyvs_alloc;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (sizeof (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (sizeof (yytype_int16) + sizeof (YYSTYPE)) \
      + YYSTACK_GAP_MAXIMUM)

# define YYCOPY_NEEDED 1

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack_alloc, Stack)                           \
    do                                                                  \
      {                                                                 \
        YYSIZE_T yynewbytes;                                            \
        YYCOPY (&yyptr->Stack_alloc, Stack, yysize);                    \
        Stack = &yyptr->Stack_alloc;                                    \
        yynewbytes = yystacksize * sizeof (*Stack) + YYSTACK_GAP_MAXIMUM; \
        yyptr += yynewbytes / sizeof (*yyptr);                          \
      }                                                                 \
    while (0)

#endif

#if defined YYCOPY_NEEDED && YYCOPY_NEEDED
/* Copy COUNT objects from SRC to DST.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(Dst, Src, Count) \
      __builtin_memcpy (Dst, Src, (Count) * sizeof (*(Src)))
#  else
#   define YYCOPY(Dst, Src, Count)              \
      do                                        \
        {                                       \
          YYSIZE_T yyi;                         \
          for (yyi = 0; yyi < (Count); yyi++)   \
            (Dst)[yyi] = (Src)[yyi];            \
        }                                       \
      while (0)
#  endif
# endif
#endif /* !YYCOPY_NEEDED */

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  7
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   116

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  45
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  25
/* YYNRULES -- Number of rules.  */
#define YYNRULES  53
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  99

/* YYTRANSLATE[YYX] -- Symbol number corresponding to YYX as returned
   by yylex, with out-of-bounds checking.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   299

#define YYTRANSLATE(YYX)                                                \
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[TOKEN-NUM] -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex, without out-of-bounds checking.  */
static const yytype_uint8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44
};

#if YYDEBUG
  /* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,   115,   115,   122,   124,   128,   131,   137,   138,   141,
     146,   147,   150,   151,   154,   159,   162,   169,   170,   173,
     178,   179,   183,   192,   199,   204,   212,   215,   218,   226,
     229,   234,   237,   241,   245,   249,   253,   257,   261,   265,
     273,   276,   279,   282,   283,   288,   295,   296,   297,   298,
     301,   308,   313,   322
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || 0
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "BY", "CHAR", "CHARCONST", "COLON",
  "COMMA", "DIVIDE", "ELSE", "ENDOFFILE", "EQUALS", "FOR", "IF", "INT",
  "LBRACKET", "LPAREN", "LSQUARE", "MINUS", "NAME", "ELE", "NOT", "NUMBER",
  "PLUS", "PROCEDURE", "RBRACKET", "READ", "RPAREN", "RSQUARE",
  "SEMICOLON", "THEN", "TIMES", "TO", "WHILE", "WRITE", "UNKNOWN",
  "STRING", "LT", "LE", "EQ", "NE", "GE", "GT", "AND", "OR", "$accept",
  "Grammar", "ProcList", "Proc", "Decls", "Decl", "Type", "SpecList",
  "Spec", "Stmts", "Stmt", "MatchStmt", "OpenStmt", "Bool", "OrTerm",
  "AndTerm", "RelExpr", "Write", "Expr", "Term", "Factor", "Charconst",
  "Reference", "Name", "Number", YY_NULLPTR
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[NUM] -- (External) token number corresponding to the
   (internal) symbol number NUM (which must be that of a token).  */
static const yytype_uint16 yytoknum[] =
{
       0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   270,   271,   272,   273,   274,
     275,   276,   277,   278,   279,   280,   281,   282,   283,   284,
     285,   286,   287,   288,   289,   290,   291,   292,   293,   294,
     295,   296,   297,   298,   299
};
# endif

#define YYPACT_NINF -74

#define yypact_value_is_default(Yystate) \
  (!!((Yystate) == (-74)))

#define YYTABLE_NINF -1

#define yytable_value_is_error(Yytable_value) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int8 yypact[] =
{
     -17,     8,    34,    -7,   -74,   -74,    27,   -74,   -74,   -74,
      25,   -74,   -74,     2,    20,     8,    37,    47,    49,    21,
      13,   -74,   -74,   -74,   -74,    55,     8,   -74,    77,   -74,
     -74,    56,    56,   -74,    49,   -74,    44,    -3,   -74,   -74,
     -74,   -74,   -74,   -74,   -74,   -74,   -74,    49,   -74,     8,
      49,    58,    43,    45,    59,    -5,    62,    68,    49,    49,
     -74,    49,    49,    51,   -74,    43,    72,    49,    49,    49,
      49,    49,    49,    49,    49,    88,   -74,    -3,    -3,   -74,
     -74,   -74,    11,    45,    59,    -5,    -5,    -5,    -5,    -5,
      -5,    11,   -74,    83,    18,    11,   -74,   -74,   -74
};

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
       0,     0,     0,     0,     4,    52,     0,     1,     2,     3,
       0,    11,    10,     0,     0,     0,     0,     0,     0,     0,
       0,    16,    17,    18,    20,     0,    51,     8,     9,    13,
      14,     0,     0,    50,     0,    53,     0,    42,    43,    49,
      46,    51,    47,     7,     6,     5,    15,     0,    21,     0,
       0,     0,    27,    29,    31,    38,     0,     0,     0,     0,
      39,     0,     0,     0,    12,    26,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    48,    41,    40,    45,
      44,    19,     0,    28,    30,    32,    33,    34,    35,    36,
      37,     0,    24,    17,     0,     0,    23,    22,    25
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int8 yypgoto[] =
{
     -74,   -74,   -74,   101,   -74,    92,   -74,   -74,    57,    16,
     -18,   -73,    14,    76,    60,    46,    48,   -74,   -14,   -48,
     -21,   -74,   -12,    -1,   -74
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int8 yydefgoto[] =
{
      -1,     2,     3,     4,    13,    14,    15,    28,    29,    20,
      21,    22,    23,    51,    52,    53,    54,    24,    55,    37,
      38,    39,    40,    41,    42
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_uint8 yytable[] =
{
       6,    25,    46,     8,    36,    61,    11,     1,    25,    93,
      77,    78,    26,    58,    30,    16,    12,     1,    59,    26,
      57,     5,    97,    44,    16,    48,    16,     5,    62,    11,
       5,    16,     5,    63,     7,    17,    18,     5,    45,    12,
      79,    80,    10,    96,    17,    18,    17,    18,    30,    27,
      43,    17,    18,    31,    33,    85,    86,    87,    88,    89,
      90,    33,    58,    32,    92,    34,    47,    59,     5,    58,
      25,    35,    34,    60,    59,     5,    46,    50,    35,    25,
      81,    26,    25,    25,    49,    66,    58,    67,    68,    75,
      26,    59,    95,    26,    26,    76,    69,    70,    71,    72,
      73,    74,    82,    91,     9,    19,    64,    94,    56,    98,
      65,     0,     0,    83,     0,     0,    84
};

static const yytype_int8 yycheck[] =
{
       1,    13,    20,    10,    18,     8,     4,    24,    20,    82,
      58,    59,    13,    18,    15,    13,    14,    24,    23,    20,
      34,    19,    95,    10,    13,    26,    13,    19,    31,     4,
      19,    13,    19,    47,     0,    33,    34,    19,    25,    14,
      61,    62,    15,    25,    33,    34,    33,    34,    49,    29,
      29,    33,    34,    16,     5,    69,    70,    71,    72,    73,
      74,     5,    18,    16,    82,    16,    11,    23,    19,    18,
      82,    22,    16,    29,    23,    19,    94,    21,    22,    91,
      29,    82,    94,    95,     7,    27,    18,    44,    43,    27,
      91,    23,     9,    94,    95,    27,    37,    38,    39,    40,
      41,    42,    30,    15,     3,    13,    49,    91,    32,    95,
      50,    -1,    -1,    67,    -1,    -1,    68
};

  /* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
     symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,    24,    46,    47,    48,    19,    68,     0,    10,    48,
      15,     4,    14,    49,    50,    51,    13,    33,    34,    50,
      54,    55,    56,    57,    62,    67,    68,    29,    52,    53,
      68,    16,    16,     5,    16,    22,    63,    64,    65,    66,
      67,    68,    69,    29,    10,    25,    55,    11,    68,     7,
      21,    58,    59,    60,    61,    63,    58,    63,    18,    23,
      29,     8,    31,    63,    53,    59,    27,    44,    43,    37,
      38,    39,    40,    41,    42,    27,    27,    64,    64,    65,
      65,    29,    30,    60,    61,    63,    63,    63,    63,    63,
      63,    15,    55,    56,    54,     9,    25,    56,    57
};

  /* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint8 yyr1[] =
{
       0,    45,    46,    47,    47,    48,    48,    49,    49,    50,
      51,    51,    52,    52,    53,    54,    54,    55,    55,    56,
      56,    56,    56,    56,    57,    57,    58,    58,    59,    59,
      60,    60,    61,    61,    61,    61,    61,    61,    61,    62,
      63,    63,    63,    64,    64,    64,    65,    65,    65,    65,
      66,    67,    68,    69
};

  /* YYR2[YYN] -- Number of symbols on the right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     2,     2,     1,     6,     6,     3,     2,     2,
       1,     1,     3,     1,     1,     2,     1,     1,     1,     4,
       1,     2,     8,     7,     6,     8,     2,     1,     3,     1,
       3,     1,     3,     3,     3,     3,     3,     3,     1,     3,
       3,     3,     1,     1,     3,     3,     1,     1,     3,     1,
       1,     1,     1,     1
};


#define yyerrok         (yyerrstatus = 0)
#define yyclearin       (yychar = YYEMPTY)
#define YYEMPTY         (-2)
#define YYEOF           0

#define YYACCEPT        goto yyacceptlab
#define YYABORT         goto yyabortlab
#define YYERROR         goto yyerrorlab


#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)                                  \
do                                                              \
  if (yychar == YYEMPTY)                                        \
    {                                                           \
      yychar = (Token);                                         \
      yylval = (Value);                                         \
      YYPOPSTACK (yylen);                                       \
      yystate = *yyssp;                                         \
      goto yybackup;                                            \
    }                                                           \
  else                                                          \
    {                                                           \
      yyerror (YY_("syntax error: cannot back up")); \
      YYERROR;                                                  \
    }                                                           \
while (0)

/* Error token number */
#define YYTERROR        1
#define YYERRCODE       256



/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)                        \
do {                                            \
  if (yydebug)                                  \
    YYFPRINTF Args;                             \
} while (0)

/* This macro is provided for backward compatibility. */
#ifndef YY_LOCATION_PRINT
# define YY_LOCATION_PRINT(File, Loc) ((void) 0)
#endif


# define YY_SYMBOL_PRINT(Title, Type, Value, Location)                    \
do {                                                                      \
  if (yydebug)                                                            \
    {                                                                     \
      YYFPRINTF (stderr, "%s ", Title);                                   \
      yy_symbol_print (stderr,                                            \
                  Type, Value); \
      YYFPRINTF (stderr, "\n");                                           \
    }                                                                     \
} while (0)


/*----------------------------------------.
| Print this symbol's value on YYOUTPUT.  |
`----------------------------------------*/

static void
yy_symbol_value_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
{
  FILE *yyo = yyoutput;
  YYUSE (yyo);
  if (!yyvaluep)
    return;
# ifdef YYPRINT
  if (yytype < YYNTOKENS)
    YYPRINT (yyoutput, yytoknum[yytype], *yyvaluep);
# endif
  YYUSE (yytype);
}


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

static void
yy_symbol_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
{
  YYFPRINTF (yyoutput, "%s %s (",
             yytype < YYNTOKENS ? "token" : "nterm", yytname[yytype]);

  yy_symbol_value_print (yyoutput, yytype, yyvaluep);
  YYFPRINTF (yyoutput, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

static void
yy_stack_print (yytype_int16 *yybottom, yytype_int16 *yytop)
{
  YYFPRINTF (stderr, "Stack now");
  for (; yybottom <= yytop; yybottom++)
    {
      int yybot = *yybottom;
      YYFPRINTF (stderr, " %d", yybot);
    }
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)                            \
do {                                                            \
  if (yydebug)                                                  \
    yy_stack_print ((Bottom), (Top));                           \
} while (0)


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

static void
yy_reduce_print (yytype_int16 *yyssp, YYSTYPE *yyvsp, int yyrule)
{
  unsigned long int yylno = yyrline[yyrule];
  int yynrhs = yyr2[yyrule];
  int yyi;
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %lu):\n",
             yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      YYFPRINTF (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr,
                       yystos[yyssp[yyi + 1 - yynrhs]],
                       &(yyvsp[(yyi + 1) - (yynrhs)])
                                              );
      YYFPRINTF (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)          \
do {                                    \
  if (yydebug)                          \
    yy_reduce_print (yyssp, yyvsp, Rule); \
} while (0)

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args)
# define YY_SYMBOL_PRINT(Title, Type, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   YYSTACK_ALLOC_MAXIMUM < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif


#if YYERROR_VERBOSE

# ifndef yystrlen
#  if defined __GLIBC__ && defined _STRING_H
#   define yystrlen strlen
#  else
/* Return the length of YYSTR.  */
static YYSIZE_T
yystrlen (const char *yystr)
{
  YYSIZE_T yylen;
  for (yylen = 0; yystr[yylen]; yylen++)
    continue;
  return yylen;
}
#  endif
# endif

# ifndef yystpcpy
#  if defined __GLIBC__ && defined _STRING_H && defined _GNU_SOURCE
#   define yystpcpy stpcpy
#  else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
static char *
yystpcpy (char *yydest, const char *yysrc)
{
  char *yyd = yydest;
  const char *yys = yysrc;

  while ((*yyd++ = *yys++) != '\0')
    continue;

  return yyd - 1;
}
#  endif
# endif

# ifndef yytnamerr
/* Copy to YYRES the contents of YYSTR after stripping away unnecessary
   quotes and backslashes, so that it's suitable for yyerror.  The
   heuristic is that double-quoting is unnecessary unless the string
   contains an apostrophe, a comma, or backslash (other than
   backslash-backslash).  YYSTR is taken from yytname.  If YYRES is
   null, do not copy; instead, return the length of what the result
   would have been.  */
static YYSIZE_T
yytnamerr (char *yyres, const char *yystr)
{
  if (*yystr == '"')
    {
      YYSIZE_T yyn = 0;
      char const *yyp = yystr;

      for (;;)
        switch (*++yyp)
          {
          case '\'':
          case ',':
            goto do_not_strip_quotes;

          case '\\':
            if (*++yyp != '\\')
              goto do_not_strip_quotes;
            /* Fall through.  */
          default:
            if (yyres)
              yyres[yyn] = *yyp;
            yyn++;
            break;

          case '"':
            if (yyres)
              yyres[yyn] = '\0';
            return yyn;
          }
    do_not_strip_quotes: ;
    }

  if (! yyres)
    return yystrlen (yystr);

  return yystpcpy (yyres, yystr) - yyres;
}
# endif

/* Copy into *YYMSG, which is of size *YYMSG_ALLOC, an error message
   about the unexpected token YYTOKEN for the state stack whose top is
   YYSSP.

   Return 0 if *YYMSG was successfully written.  Return 1 if *YYMSG is
   not large enough to hold the message.  In that case, also set
   *YYMSG_ALLOC to the required number of bytes.  Return 2 if the
   required number of bytes is too large to store.  */
static int
yysyntax_error (YYSIZE_T *yymsg_alloc, char **yymsg,
                yytype_int16 *yyssp, int yytoken)
{
  YYSIZE_T yysize0 = yytnamerr (YY_NULLPTR, yytname[yytoken]);
  YYSIZE_T yysize = yysize0;
  enum { YYERROR_VERBOSE_ARGS_MAXIMUM = 5 };
  /* Internationalized format string. */
  const char *yyformat = YY_NULLPTR;
  /* Arguments of yyformat. */
  char const *yyarg[YYERROR_VERBOSE_ARGS_MAXIMUM];
  /* Number of reported tokens (one for the "unexpected", one per
     "expected"). */
  int yycount = 0;

  /* There are many possibilities here to consider:
     - If this state is a consistent state with a default action, then
       the only way this function was invoked is if the default action
       is an error action.  In that case, don't check for expected
       tokens because there are none.
     - The only way there can be no lookahead present (in yychar) is if
       this state is a consistent state with a default action.  Thus,
       detecting the absence of a lookahead is sufficient to determine
       that there is no unexpected or expected token to report.  In that
       case, just report a simple "syntax error".
     - Don't assume there isn't a lookahead just because this state is a
       consistent state with a default action.  There might have been a
       previous inconsistent state, consistent state with a non-default
       action, or user semantic action that manipulated yychar.
     - Of course, the expected token list depends on states to have
       correct lookahead information, and it depends on the parser not
       to perform extra reductions after fetching a lookahead from the
       scanner and before detecting a syntax error.  Thus, state merging
       (from LALR or IELR) and default reductions corrupt the expected
       token list.  However, the list is correct for canonical LR with
       one exception: it will still contain any token that will not be
       accepted due to an error action in a later state.
  */
  if (yytoken != YYEMPTY)
    {
      int yyn = yypact[*yyssp];
      yyarg[yycount++] = yytname[yytoken];
      if (!yypact_value_is_default (yyn))
        {
          /* Start YYX at -YYN if negative to avoid negative indexes in
             YYCHECK.  In other words, skip the first -YYN actions for
             this state because they are default actions.  */
          int yyxbegin = yyn < 0 ? -yyn : 0;
          /* Stay within bounds of both yycheck and yytname.  */
          int yychecklim = YYLAST - yyn + 1;
          int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
          int yyx;

          for (yyx = yyxbegin; yyx < yyxend; ++yyx)
            if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR
                && !yytable_value_is_error (yytable[yyx + yyn]))
              {
                if (yycount == YYERROR_VERBOSE_ARGS_MAXIMUM)
                  {
                    yycount = 1;
                    yysize = yysize0;
                    break;
                  }
                yyarg[yycount++] = yytname[yyx];
                {
                  YYSIZE_T yysize1 = yysize + yytnamerr (YY_NULLPTR, yytname[yyx]);
                  if (! (yysize <= yysize1
                         && yysize1 <= YYSTACK_ALLOC_MAXIMUM))
                    return 2;
                  yysize = yysize1;
                }
              }
        }
    }

  switch (yycount)
    {
# define YYCASE_(N, S)                      \
      case N:                               \
        yyformat = S;                       \
      break
      YYCASE_(0, YY_("syntax error"));
      YYCASE_(1, YY_("syntax error, unexpected %s"));
      YYCASE_(2, YY_("syntax error, unexpected %s, expecting %s"));
      YYCASE_(3, YY_("syntax error, unexpected %s, expecting %s or %s"));
      YYCASE_(4, YY_("syntax error, unexpected %s, expecting %s or %s or %s"));
      YYCASE_(5, YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s"));
# undef YYCASE_
    }

  {
    YYSIZE_T yysize1 = yysize + yystrlen (yyformat);
    if (! (yysize <= yysize1 && yysize1 <= YYSTACK_ALLOC_MAXIMUM))
      return 2;
    yysize = yysize1;
  }

  if (*yymsg_alloc < yysize)
    {
      *yymsg_alloc = 2 * yysize;
      if (! (yysize <= *yymsg_alloc
             && *yymsg_alloc <= YYSTACK_ALLOC_MAXIMUM))
        *yymsg_alloc = YYSTACK_ALLOC_MAXIMUM;
      return 1;
    }

  /* Avoid sprintf, as that infringes on the user's name space.
     Don't have undefined behavior even if the translation
     produced a string with the wrong number of "%s"s.  */
  {
    char *yyp = *yymsg;
    int yyi = 0;
    while ((*yyp = *yyformat) != '\0')
      if (*yyp == '%' && yyformat[1] == 's' && yyi < yycount)
        {
          yyp += yytnamerr (yyp, yyarg[yyi++]);
          yyformat += 2;
        }
      else
        {
          yyp++;
          yyformat++;
        }
  }
  return 0;
}
#endif /* YYERROR_VERBOSE */

/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

static void
yydestruct (const char *yymsg, int yytype, YYSTYPE *yyvaluep)
{
  YYUSE (yyvaluep);
  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yytype, yyvaluep, yylocationp);

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YYUSE (yytype);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}




/* The lookahead symbol.  */
int yychar;

/* The semantic value of the lookahead symbol.  */
YYSTYPE yylval;
/* Number of syntax errors so far.  */
int yynerrs;


/*----------.
| yyparse.  |
`----------*/

int
yyparse (void)
{
    int yystate;
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus;

    /* The stacks and their tools:
       'yyss': related to states.
       'yyvs': related to semantic values.

       Refer to the stacks through separate pointers, to allow yyoverflow
       to reallocate them elsewhere.  */

    /* The state stack.  */
    yytype_int16 yyssa[YYINITDEPTH];
    yytype_int16 *yyss;
    yytype_int16 *yyssp;

    /* The semantic value stack.  */
    YYSTYPE yyvsa[YYINITDEPTH];
    YYSTYPE *yyvs;
    YYSTYPE *yyvsp;

    YYSIZE_T yystacksize;

  int yyn;
  int yyresult;
  /* Lookahead token as an internal (translated) token number.  */
  int yytoken = 0;
  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;

#if YYERROR_VERBOSE
  /* Buffer for error messages, and its allocated size.  */
  char yymsgbuf[128];
  char *yymsg = yymsgbuf;
  YYSIZE_T yymsg_alloc = sizeof yymsgbuf;
#endif

#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  yyssp = yyss = yyssa;
  yyvsp = yyvs = yyvsa;
  yystacksize = YYINITDEPTH;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY; /* Cause a token to be read.  */
  goto yysetstate;

/*------------------------------------------------------------.
| yynewstate -- Push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
 yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;

 yysetstate:
  *yyssp = yystate;

  if (yyss + yystacksize - 1 <= yyssp)
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYSIZE_T yysize = yyssp - yyss + 1;

#ifdef yyoverflow
      {
        /* Give user a chance to reallocate the stack.  Use copies of
           these so that the &'s don't force the real ones into
           memory.  */
        YYSTYPE *yyvs1 = yyvs;
        yytype_int16 *yyss1 = yyss;

        /* Each stack pointer address is followed by the size of the
           data in use in that stack, in bytes.  This used to be a
           conditional around just the two extra args, but that might
           be undefined if yyoverflow is a macro.  */
        yyoverflow (YY_("memory exhausted"),
                    &yyss1, yysize * sizeof (*yyssp),
                    &yyvs1, yysize * sizeof (*yyvsp),
                    &yystacksize);

        yyss = yyss1;
        yyvs = yyvs1;
      }
#else /* no yyoverflow */
# ifndef YYSTACK_RELOCATE
      goto yyexhaustedlab;
# else
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
        goto yyexhaustedlab;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
        yystacksize = YYMAXDEPTH;

      {
        yytype_int16 *yyss1 = yyss;
        union yyalloc *yyptr =
          (union yyalloc *) YYSTACK_ALLOC (YYSTACK_BYTES (yystacksize));
        if (! yyptr)
          goto yyexhaustedlab;
        YYSTACK_RELOCATE (yyss_alloc, yyss);
        YYSTACK_RELOCATE (yyvs_alloc, yyvs);
#  undef YYSTACK_RELOCATE
        if (yyss1 != yyssa)
          YYSTACK_FREE (yyss1);
      }
# endif
#endif /* no yyoverflow */

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;

      YYDPRINTF ((stderr, "Stack size increased to %lu\n",
                  (unsigned long int) yystacksize));

      if (yyss + yystacksize - 1 <= yyssp)
        YYABORT;
    }

  YYDPRINTF ((stderr, "Entering state %d\n", yystate));

  if (yystate == YYFINAL)
    YYACCEPT;

  goto yybackup;

/*-----------.
| yybackup.  |
`-----------*/
yybackup:

  /* Do appropriate processing given the current state.  Read a
     lookahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to lookahead token.  */
  yyn = yypact[yystate];
  if (yypact_value_is_default (yyn))
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* YYCHAR is either YYEMPTY or YYEOF or a valid lookahead symbol.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token: "));
      yychar = yylex ();
    }

  if (yychar <= YYEOF)
    {
      yychar = yytoken = YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YY_SYMBOL_PRINT ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yytable_value_is_error (yyn))
        goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the lookahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);

  /* Discard the shifted token.  */
  yychar = YYEMPTY;

  yystate = yyn;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END

  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- Do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     '$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];


  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
        case 2:
#line 115 "DEMOgram.y" /* yacc.c:1646  */
    { walk((yyvsp[-1].val));
                                                           freeAST((yyvsp[-1].val));
                                                           freeST(sym_table);
       return 1; }
#line 1352 "DEMOgram.tab.c" /* yacc.c:1646  */
    break;

  case 3:
#line 122 "DEMOgram.y" /* yacc.c:1646  */
    { add_sybling_node((yyvsp[-1].val), (yyvsp[0].val));	/* Stmts = root Stmt = sibling*/
							(yyval.val) = (yyvsp[-1].val);}
#line 1359 "DEMOgram.tab.c" /* yacc.c:1646  */
    break;

  case 4:
#line 124 "DEMOgram.y" /* yacc.c:1646  */
    {	children[0] = (yyvsp[0].val);
					(yyval.val) = make_op_node( NODE_STATEMENTS, children, 1);}
#line 1366 "DEMOgram.tab.c" /* yacc.c:1646  */
    break;

  case 5:
#line 130 "DEMOgram.y" /* yacc.c:1646  */
    { (yyval.val) = (yyvsp[-1].val); }
#line 1372 "DEMOgram.tab.c" /* yacc.c:1646  */
    break;

  case 6:
#line 132 "DEMOgram.y" /* yacc.c:1646  */
    { yyerror("Missing final right bracket ('}')"); }
#line 1378 "DEMOgram.tab.c" /* yacc.c:1646  */
    break;

  case 7:
#line 137 "DEMOgram.y" /* yacc.c:1646  */
    { cur = TYPE_UNK; }
#line 1384 "DEMOgram.tab.c" /* yacc.c:1646  */
    break;

  case 8:
#line 138 "DEMOgram.y" /* yacc.c:1646  */
    { cur = TYPE_UNK; }
#line 1390 "DEMOgram.tab.c" /* yacc.c:1646  */
    break;

  case 10:
#line 146 "DEMOgram.y" /* yacc.c:1646  */
    { cur = TYPE_INT; }
#line 1396 "DEMOgram.tab.c" /* yacc.c:1646  */
    break;

  case 11:
#line 147 "DEMOgram.y" /* yacc.c:1646  */
    { cur = TYPE_CHAR; }
#line 1402 "DEMOgram.tab.c" /* yacc.c:1646  */
    break;

  case 15:
#line 159 "DEMOgram.y" /* yacc.c:1646  */
    { 
              add_sybling_node((yyvsp[-1].val), (yyvsp[0].val));	/* Stmts = root Stmt = sibling*/
							(yyval.val) = (yyvsp[-1].val); }
#line 1410 "DEMOgram.tab.c" /* yacc.c:1646  */
    break;

  case 16:
#line 162 "DEMOgram.y" /* yacc.c:1646  */
    { 	
              children[0] = (yyvsp[0].val);
       				(yyval.val) = make_op_node( NODE_STATEMENTS, children, 1); }
#line 1418 "DEMOgram.tab.c" /* yacc.c:1646  */
    break;

  case 17:
#line 169 "DEMOgram.y" /* yacc.c:1646  */
    {(yyval.val) = (yyvsp[0].val);}
#line 1424 "DEMOgram.tab.c" /* yacc.c:1646  */
    break;

  case 18:
#line 170 "DEMOgram.y" /* yacc.c:1646  */
    {(yyval.val) = (yyvsp[0].val);}
#line 1430 "DEMOgram.tab.c" /* yacc.c:1646  */
    break;

  case 19:
#line 173 "DEMOgram.y" /* yacc.c:1646  */
    {  
                      children[0] = (yyvsp[-3].val);
											children[1] = (yyvsp[-1].nodeptr);
											(yyval.val) = make_op_node( NODE_ASSIGN, children, 2);
                                          }
#line 1440 "DEMOgram.tab.c" /* yacc.c:1646  */
    break;

  case 21:
#line 179 "DEMOgram.y" /* yacc.c:1646  */
    {
            	printf("%s at ",(yyvsp[-1].val));
            	yyerror("is not a key word.\n");
      }
#line 1449 "DEMOgram.tab.c" /* yacc.c:1646  */
    break;

  case 22:
#line 183 "DEMOgram.y" /* yacc.c:1646  */
    {  
                      children[0] = (yyvsp[-5].val);
											children[1] = (yyvsp[-2].val);
                      children[2] = (yyvsp[0].val);
											(yyval.val) = make_op_node( NODE_IF_ELSE, children, 3);
                                          }
#line 1460 "DEMOgram.tab.c" /* yacc.c:1646  */
    break;

  case 23:
#line 192 "DEMOgram.y" /* yacc.c:1646  */
    {
                      children[0] = (yyvsp[-4].val);
                      children[1] = (yyvsp[-1].val);
                      (yyval.val) = make_op_node( NODE_WHILE, children, 2);
      }
#line 1470 "DEMOgram.tab.c" /* yacc.c:1646  */
    break;

  case 24:
#line 199 "DEMOgram.y" /* yacc.c:1646  */
    {  
                      children[0] = (yyvsp[-3].val);
                      children[1] = (yyvsp[0].val);
											(yyval.val) = make_op_node( NODE_IF, children, 2);
                                          }
#line 1480 "DEMOgram.tab.c" /* yacc.c:1646  */
    break;

  case 25:
#line 204 "DEMOgram.y" /* yacc.c:1646  */
    {  
                      children[0] = (yyvsp[-5].val);
                      children[1] = (yyvsp[-2].val); 
                      children[2] = (yyvsp[0].val);
											(yyval.val) = make_op_node( NODE_IF_ELSE, children, 3);
                                          }
#line 1491 "DEMOgram.tab.c" /* yacc.c:1646  */
    break;

  case 26:
#line 212 "DEMOgram.y" /* yacc.c:1646  */
    {  children[0] = (yyvsp[0].val);
											(yyval.val) = make_op_node( NODE_NOT, children, 1);
                                          }
#line 1499 "DEMOgram.tab.c" /* yacc.c:1646  */
    break;

  case 27:
#line 215 "DEMOgram.y" /* yacc.c:1646  */
    {(yyval.val) = (yyvsp[0].val);}
#line 1505 "DEMOgram.tab.c" /* yacc.c:1646  */
    break;

  case 28:
#line 218 "DEMOgram.y" /* yacc.c:1646  */
    {  
                      children[0] = (yyvsp[-2].val);
											children[1] = (yyvsp[0].val);
                      // printf("OrTerm(%s:%d) OR AndTerm(%s:%d)\n", 
                      // children[0]->var_name, children[0]->var_val, 
                      // children[1]->var_name, children[1]->var_val);
											(yyval.val) = make_op_node( NODE_OR, children, 2);
                                          }
#line 1518 "DEMOgram.tab.c" /* yacc.c:1646  */
    break;

  case 29:
#line 226 "DEMOgram.y" /* yacc.c:1646  */
    {(yyval.val) = (yyvsp[0].val);}
#line 1524 "DEMOgram.tab.c" /* yacc.c:1646  */
    break;

  case 30:
#line 229 "DEMOgram.y" /* yacc.c:1646  */
    {  
                      children[0] = (yyvsp[-2].val);
											children[1] = (yyvsp[0].val);
											(yyval.val) = make_op_node( NODE_AND, children, 2);
                                          }
#line 1534 "DEMOgram.tab.c" /* yacc.c:1646  */
    break;

  case 31:
#line 234 "DEMOgram.y" /* yacc.c:1646  */
    {           (yyval.val) = (yyvsp[0].val);}
#line 1540 "DEMOgram.tab.c" /* yacc.c:1646  */
    break;

  case 32:
#line 237 "DEMOgram.y" /* yacc.c:1646  */
    {  children[0] = (yyvsp[-2].val);
											children[1] = (yyvsp[0].nodeptr);
											(yyval.val) = make_op_node( NODE_LT, children, 2);
                                          }
#line 1549 "DEMOgram.tab.c" /* yacc.c:1646  */
    break;

  case 33:
#line 241 "DEMOgram.y" /* yacc.c:1646  */
    {  children[0] = (yyvsp[-2].val);
											children[1] = (yyvsp[0].nodeptr);
											(yyval.val) = make_op_node( NODE_LE, children, 2);
                                          }
#line 1558 "DEMOgram.tab.c" /* yacc.c:1646  */
    break;

  case 34:
#line 245 "DEMOgram.y" /* yacc.c:1646  */
    {  children[0] = (yyvsp[-2].val);
											children[1] = (yyvsp[0].nodeptr);
											(yyval.val) = make_op_node( NODE_EQ, children, 2);
                                          }
#line 1567 "DEMOgram.tab.c" /* yacc.c:1646  */
    break;

  case 35:
#line 249 "DEMOgram.y" /* yacc.c:1646  */
    {  children[0] = (yyvsp[-2].val);
											children[1] = (yyvsp[0].nodeptr);
											(yyval.val) = make_op_node( NODE_NE, children, 2);
                                          }
#line 1576 "DEMOgram.tab.c" /* yacc.c:1646  */
    break;

  case 36:
#line 253 "DEMOgram.y" /* yacc.c:1646  */
    {  children[0] = (yyvsp[-2].val);
											children[1] = (yyvsp[0].nodeptr);
											(yyval.val) = make_op_node( NODE_GE, children, 2);
                                          }
#line 1585 "DEMOgram.tab.c" /* yacc.c:1646  */
    break;

  case 37:
#line 257 "DEMOgram.y" /* yacc.c:1646  */
    {  children[0] = (yyvsp[-2].val);
											children[1] = (yyvsp[0].nodeptr);
											(yyval.val) = make_op_node( NODE_GT, children, 2);
                                          }
#line 1594 "DEMOgram.tab.c" /* yacc.c:1646  */
    break;

  case 38:
#line 261 "DEMOgram.y" /* yacc.c:1646  */
    {(yyval.val) = (yyvsp[0].nodeptr);}
#line 1600 "DEMOgram.tab.c" /* yacc.c:1646  */
    break;

  case 39:
#line 265 "DEMOgram.y" /* yacc.c:1646  */
    { 
                children[0]=(yyvsp[-1].nodeptr);
								(yyval.val) = make_op_node(NODE_WRITE, children, 1); }
#line 1608 "DEMOgram.tab.c" /* yacc.c:1646  */
    break;

  case 40:
#line 273 "DEMOgram.y" /* yacc.c:1646  */
    {	children[0] = (yyvsp[-2].nodeptr);
							children[1] = (yyvsp[0].nodeptr);
							(yyval.nodeptr) = make_op_node(NODE_PLUS, children, 2);}
#line 1616 "DEMOgram.tab.c" /* yacc.c:1646  */
    break;

  case 41:
#line 276 "DEMOgram.y" /* yacc.c:1646  */
    {	children[0] = (yyvsp[-2].nodeptr);
							children[1] = (yyvsp[0].nodeptr);
							(yyval.nodeptr) = make_op_node(NODE_MINUS, children, 2);}
#line 1624 "DEMOgram.tab.c" /* yacc.c:1646  */
    break;

  case 42:
#line 279 "DEMOgram.y" /* yacc.c:1646  */
    {(yyval.nodeptr) = (yyvsp[0].nodeptr);}
#line 1630 "DEMOgram.tab.c" /* yacc.c:1646  */
    break;

  case 43:
#line 282 "DEMOgram.y" /* yacc.c:1646  */
    {(yyval.nodeptr) = (yyvsp[0].nodeptr);}
#line 1636 "DEMOgram.tab.c" /* yacc.c:1646  */
    break;

  case 44:
#line 283 "DEMOgram.y" /* yacc.c:1646  */
    {
          children[0] = (yyvsp[-2].nodeptr);
          children[1] = (yyvsp[0].nodeptr);
          (yyval.nodeptr) = make_op_node(NODE_TIMES, children, 2);
        }
#line 1646 "DEMOgram.tab.c" /* yacc.c:1646  */
    break;

  case 45:
#line 288 "DEMOgram.y" /* yacc.c:1646  */
    {
          children[0] = (yyvsp[-2].nodeptr);
          children[1] = (yyvsp[0].nodeptr);
          (yyval.nodeptr) = make_op_node(NODE_DIVIDE, children, 2);
        }
#line 1656 "DEMOgram.tab.c" /* yacc.c:1646  */
    break;

  case 46:
#line 295 "DEMOgram.y" /* yacc.c:1646  */
    { (yyval.nodeptr) = (yyvsp[0].val); }
#line 1662 "DEMOgram.tab.c" /* yacc.c:1646  */
    break;

  case 47:
#line 296 "DEMOgram.y" /* yacc.c:1646  */
    { (yyval.nodeptr) = (yyvsp[0].nodeptr); }
#line 1668 "DEMOgram.tab.c" /* yacc.c:1646  */
    break;

  case 48:
#line 297 "DEMOgram.y" /* yacc.c:1646  */
    {(yyval.nodeptr) = (yyvsp[-2].val); }
#line 1674 "DEMOgram.tab.c" /* yacc.c:1646  */
    break;

  case 50:
#line 301 "DEMOgram.y" /* yacc.c:1646  */
    {
  (yyval.val) = make_leaf_node(TYPE_CHAR,"char", TokenString);
}
#line 1682 "DEMOgram.tab.c" /* yacc.c:1646  */
    break;

  case 51:
#line 308 "DEMOgram.y" /* yacc.c:1646  */
    { 
  // printf("Reference: get node name: %s\n", $1);
  (yyval.val) = make_leaf_node(NODE_NAME, strdup((yyvsp[0].val)), 0);}
#line 1690 "DEMOgram.tab.c" /* yacc.c:1646  */
    break;

  case 52:
#line 313 "DEMOgram.y" /* yacc.c:1646  */
    {
				  (yyval.val) = strdup(TokenString);
				  if (getsym(TokenString) == NULL && cur != TYPE_UNK) {

					putsym(TokenString, cur, 0);
				  }
              }
#line 1702 "DEMOgram.tab.c" /* yacc.c:1646  */
    break;

  case 53:
#line 322 "DEMOgram.y" /* yacc.c:1646  */
    {(yyval.nodeptr) = make_leaf_node(TYPE_INT,"number", TokenString);}
#line 1708 "DEMOgram.tab.c" /* yacc.c:1646  */
    break;


#line 1712 "DEMOgram.tab.c" /* yacc.c:1646  */
      default: break;
    }
  /* User semantic actions sometimes alter yychar, and that requires
     that yytoken be updated with the new translation.  We take the
     approach of translating immediately before every use of yytoken.
     One alternative is translating here after every semantic action,
     but that translation would be missed if the semantic action invokes
     YYABORT, YYACCEPT, or YYERROR immediately after altering yychar or
     if it invokes YYBACKUP.  In the case of YYABORT or YYACCEPT, an
     incorrect destructor might then be invoked immediately.  In the
     case of YYERROR or YYBACKUP, subsequent parser actions might lead
     to an incorrect destructor call or verbose syntax error message
     before the lookahead is translated.  */
  YY_SYMBOL_PRINT ("-> $$ =", yyr1[yyn], &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);

  *++yyvsp = yyval;

  /* Now 'shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTOKENS] + *yyssp;
  if (0 <= yystate && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTOKENS];

  goto yynewstate;


/*--------------------------------------.
| yyerrlab -- here on detecting error.  |
`--------------------------------------*/
yyerrlab:
  /* Make sure we have latest lookahead translation.  See comments at
     user semantic actions for why this is necessary.  */
  yytoken = yychar == YYEMPTY ? YYEMPTY : YYTRANSLATE (yychar);

  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
#if ! YYERROR_VERBOSE
      yyerror (YY_("syntax error"));
#else
# define YYSYNTAX_ERROR yysyntax_error (&yymsg_alloc, &yymsg, \
                                        yyssp, yytoken)
      {
        char const *yymsgp = YY_("syntax error");
        int yysyntax_error_status;
        yysyntax_error_status = YYSYNTAX_ERROR;
        if (yysyntax_error_status == 0)
          yymsgp = yymsg;
        else if (yysyntax_error_status == 1)
          {
            if (yymsg != yymsgbuf)
              YYSTACK_FREE (yymsg);
            yymsg = (char *) YYSTACK_ALLOC (yymsg_alloc);
            if (!yymsg)
              {
                yymsg = yymsgbuf;
                yymsg_alloc = sizeof yymsgbuf;
                yysyntax_error_status = 2;
              }
            else
              {
                yysyntax_error_status = YYSYNTAX_ERROR;
                yymsgp = yymsg;
              }
          }
        yyerror (yymsgp);
        if (yysyntax_error_status == 2)
          goto yyexhaustedlab;
      }
# undef YYSYNTAX_ERROR
#endif
    }



  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse lookahead token after an
         error, discard it.  */

      if (yychar <= YYEOF)
        {
          /* Return failure if at end of input.  */
          if (yychar == YYEOF)
            YYABORT;
        }
      else
        {
          yydestruct ("Error: discarding",
                      yytoken, &yylval);
          yychar = YYEMPTY;
        }
    }

  /* Else will try to reuse lookahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:

  /* Pacify compilers like GCC when the user code never invokes
     YYERROR and the label yyerrorlab therefore never appears in user
     code.  */
  if (/*CONSTCOND*/ 0)
     goto yyerrorlab;

  /* Do not reclaim the symbols of the rule whose action triggered
     this YYERROR.  */
  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);
  yystate = *yyssp;
  goto yyerrlab1;


/*-------------------------------------------------------------.
| yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3;      /* Each real token shifted decrements this.  */

  for (;;)
    {
      yyn = yypact[yystate];
      if (!yypact_value_is_default (yyn))
        {
          yyn += YYTERROR;
          if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYTERROR)
            {
              yyn = yytable[yyn];
              if (0 < yyn)
                break;
            }
        }

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
        YYABORT;


      yydestruct ("Error: popping",
                  yystos[yystate], yyvsp);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END


  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", yystos[yyn], yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturn;

/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturn;

#if !defined yyoverflow || YYERROR_VERBOSE
/*-------------------------------------------------.
| yyexhaustedlab -- memory exhaustion comes here.  |
`-------------------------------------------------*/
yyexhaustedlab:
  yyerror (YY_("memory exhausted"));
  yyresult = 2;
  /* Fall through.  */
#endif

yyreturn:
  if (yychar != YYEMPTY)
    {
      /* Make sure we have latest lookahead translation.  See comments at
         user semantic actions for why this is necessary.  */
      yytoken = YYTRANSLATE (yychar);
      yydestruct ("Cleanup: discarding lookahead",
                  yytoken, &yylval);
    }
  /* Do not reclaim the symbols of the rule whose action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
                  yystos[*yyssp], yyvsp);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif
#if YYERROR_VERBOSE
  if (yymsg != yymsgbuf)
    YYSTACK_FREE (yymsg);
#endif
  return yyresult;
}
#line 325 "DEMOgram.y" /* yacc.c:1906  */

static int ErrLine = 0;

int yywrap()
{
  return 1;
} 

void yyerror(char *s)
{
  if (yylineno != ErrLine) {
    fprintf(stderr, "Line %d: %s\n",yylineno,s);
    syntax_error++;
  }

  ErrLine = yylineno;
  fprintf(LOG, "Parser, line %d: %s\n",yylineno,s);
  fprintf(LOG, "\tlexeme is '%s'.\n", yytext);

  /* will delete CODE and Storage Layout files back in main() */
}
