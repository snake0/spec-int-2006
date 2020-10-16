#define PERL_constant_NOTFOUND	1
#define PERL_constant_NOTDEF	2
#define PERL_constant_ISIV	3
#define PERL_constant_ISNO	4
#define PERL_constant_ISNV	5
#define PERL_constant_ISPV	6
#define PERL_constant_ISPVN	7
#define PERL_constant_ISSV	8
#define PERL_constant_ISUNDEF	9
#define PERL_constant_ISUV	10
#define PERL_constant_ISYES	11

#ifndef NVTYPE
typedef double NV; /* 5.6 and later define NVTYPE, and typedef NV to it.  */
#endif
#ifndef aTHX_
#define aTHX_ /* 5.6 or later define this for threading support.  */
#endif
#ifndef pTHX_
#define pTHX_ /* 5.6 or later define this for threading support.  */
#endif

static int
constant_5 (pTHX_ const char *name, IV *iv_return) {
  /* When generated this function returned values for the list of names given
     here.  However, subsequent manual editing may have added or removed some.
     DAY_1 DAY_2 DAY_3 DAY_4 DAY_5 DAY_6 DAY_7 D_FMT MON_1 MON_2 MON_3 MON_4
     MON_5 MON_6 MON_7 MON_8 MON_9 NOSTR T_FMT */
  /* Offset 4 gives the best switch position.  */
  switch (name[4]) {
  case '1':
    if (memEQ(name, "DAY_", 4)) {
    /*                   1     */
#if defined(DAY_1) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))
      *iv_return = DAY_1;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "MON_", 4)) {
    /*                   1     */
#if defined(MON_1) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))
      *iv_return = MON_1;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case '2':
    if (memEQ(name, "DAY_", 4)) {
    /*                   2     */
#if defined(DAY_2) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))
      *iv_return = DAY_2;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "MON_", 4)) {
    /*                   2     */
#if defined(MON_2) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))
      *iv_return = MON_2;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case '3':
    if (memEQ(name, "DAY_", 4)) {
    /*                   3     */
#if defined(DAY_3) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))
      *iv_return = DAY_3;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "MON_", 4)) {
    /*                   3     */
#if defined(MON_3) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))
      *iv_return = MON_3;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case '4':
    if (memEQ(name, "DAY_", 4)) {
    /*                   4     */
#if defined(DAY_4) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))
      *iv_return = DAY_4;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "MON_", 4)) {
    /*                   4     */
#if defined(MON_4) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))
      *iv_return = MON_4;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case '5':
    if (memEQ(name, "DAY_", 4)) {
    /*                   5     */
#if defined(DAY_5) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))
      *iv_return = DAY_5;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "MON_", 4)) {
    /*                   5     */
#if defined(MON_5) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))
      *iv_return = MON_5;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case '6':
    if (memEQ(name, "DAY_", 4)) {
    /*                   6     */
#if defined(DAY_6) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))
      *iv_return = DAY_6;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "MON_", 4)) {
    /*                   6     */
#if defined(MON_6) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))
      *iv_return = MON_6;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case '7':
    if (memEQ(name, "DAY_", 4)) {
    /*                   7     */
#if defined(DAY_7) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))
      *iv_return = DAY_7;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "MON_", 4)) {
    /*                   7     */
#if defined(MON_7) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))
      *iv_return = MON_7;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case '8':
    if (memEQ(name, "MON_", 4)) {
    /*                   8     */
#if defined(MON_8) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))
      *iv_return = MON_8;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case '9':
    if (memEQ(name, "MON_", 4)) {
    /*                   9     */
#if defined(MON_9) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))
      *iv_return = MON_9;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'R':
    if (memEQ(name, "NOST", 4)) {
    /*                   R     */
#if defined(NOSTR) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))
      *iv_return = NOSTR;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'T':
    if (memEQ(name, "D_FM", 4)) {
    /*                   T     */
#if defined(D_FMT) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))
      *iv_return = D_FMT;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "T_FM", 4)) {
    /*                   T     */
#if defined(T_FMT) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))
      *iv_return = T_FMT;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  }
  return PERL_constant_NOTFOUND;
}

static int
constant_6 (pTHX_ const char *name, IV *iv_return) {
  /* When generated this function returned values for the list of names given
     here.  However, subsequent manual editing may have added or removed some.
     AM_STR MON_10 MON_11 MON_12 NOEXPR PM_STR YESSTR */
  /* Offset 0 gives the best switch position.  */
  switch (*name++) {
  case 'A':
    if (memEQ(name, "M_STR", 5)) {
    /*                A           */
#if defined(AM_STR) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))
      *iv_return = AM_STR;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'M':
    if (memEQ(name, "ON_10", 5)) {
    /*                M           */
#if defined(MON_10) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))
      *iv_return = MON_10;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "ON_11", 5)) {
    /*                M           */
#if defined(MON_11) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))
      *iv_return = MON_11;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "ON_12", 5)) {
    /*                M           */
#if defined(MON_12) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))
      *iv_return = MON_12;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'N':
    if (memEQ(name, "OEXPR", 5)) {
    /*                N           */
#if defined(NOEXPR) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))
      *iv_return = NOEXPR;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'P':
    if (memEQ(name, "M_STR", 5)) {
    /*                P           */
#if defined(PM_STR) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))
      *iv_return = PM_STR;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'Y':
    if (memEQ(name, "ESSTR", 5)) {
    /*                Y           */
#if defined(YESSTR) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))
      *iv_return = YESSTR;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  }
  return PERL_constant_NOTFOUND;
}

static int
constant_7 (pTHX_ const char *name, IV *iv_return) {
  /* When generated this function returned values for the list of names given
     here.  However, subsequent manual editing may have added or removed some.
     ABDAY_1 ABDAY_2 ABDAY_3 ABDAY_4 ABDAY_5 ABDAY_6 ABDAY_7 ABMON_1 ABMON_2
     ABMON_3 ABMON_4 ABMON_5 ABMON_6 ABMON_7 ABMON_8 ABMON_9 CODESET D_T_FMT
     THOUSEP YESEXPR */
  /* Offset 6 gives the best switch position.  */
  switch (name[6]) {
  case '1':
    if (memEQ(name, "ABDAY_", 6)) {
    /*                     1     */
#if defined(ABDAY_1) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))
      *iv_return = ABDAY_1;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "ABMON_", 6)) {
    /*                     1     */
#if defined(ABMON_1) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))
      *iv_return = ABMON_1;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case '2':
    if (memEQ(name, "ABDAY_", 6)) {
    /*                     2     */
#if defined(ABDAY_2) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))
      *iv_return = ABDAY_2;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "ABMON_", 6)) {
    /*                     2     */
#if defined(ABMON_2) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))
      *iv_return = ABMON_2;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case '3':
    if (memEQ(name, "ABDAY_", 6)) {
    /*                     3     */
#if defined(ABDAY_3) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))
      *iv_return = ABDAY_3;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "ABMON_", 6)) {
    /*                     3     */
#if defined(ABMON_3) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))
      *iv_return = ABMON_3;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case '4':
    if (memEQ(name, "ABDAY_", 6)) {
    /*                     4     */
#if defined(ABDAY_4) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))
      *iv_return = ABDAY_4;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "ABMON_", 6)) {
    /*                     4     */
#if defined(ABMON_4) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))
      *iv_return = ABMON_4;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case '5':
    if (memEQ(name, "ABDAY_", 6)) {
    /*                     5     */
#if defined(ABDAY_5) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))
      *iv_return = ABDAY_5;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "ABMON_", 6)) {
    /*                     5     */
#if defined(ABMON_5) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))
      *iv_return = ABMON_5;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case '6':
    if (memEQ(name, "ABDAY_", 6)) {
    /*                     6     */
#if defined(ABDAY_6) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))
      *iv_return = ABDAY_6;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "ABMON_", 6)) {
    /*                     6     */
#if defined(ABMON_6) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))
      *iv_return = ABMON_6;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case '7':
    if (memEQ(name, "ABDAY_", 6)) {
    /*                     7     */
#if defined(ABDAY_7) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))
      *iv_return = ABDAY_7;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "ABMON_", 6)) {
    /*                     7     */
#if defined(ABMON_7) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))
      *iv_return = ABMON_7;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case '8':
    if (memEQ(name, "ABMON_", 6)) {
    /*                     8     */
#if defined(ABMON_8) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))
      *iv_return = ABMON_8;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case '9':
    if (memEQ(name, "ABMON_", 6)) {
    /*                     9     */
#if defined(ABMON_9) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))
      *iv_return = ABMON_9;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'P':
    if (memEQ(name, "THOUSE", 6)) {
    /*                     P     */
#if defined(THOUSEP) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM) && defined(__SVR4_I386_ABI_L1__))
      *iv_return = THOUSEP;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'R':
    if (memEQ(name, "YESEXP", 6)) {
    /*                     R     */
#if defined(YESEXPR) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))
      *iv_return = YESEXPR;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'T':
    if (memEQ(name, "CODESE", 6)) {
    /*                     T     */
#ifdef CODESET
      *iv_return = CODESET;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "D_T_FM", 6)) {
    /*                     T     */
#if defined(D_T_FMT) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))
      *iv_return = D_T_FMT;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  }
  return PERL_constant_NOTFOUND;
}

static int
constant_8 (pTHX_ const char *name, IV *iv_return) {
  /* When generated this function returned values for the list of names given
     here.  However, subsequent manual editing may have added or removed some.
     ABMON_10 ABMON_11 ABMON_12 CRNCYSTR */
  /* Offset 7 gives the best switch position.  */
  switch (name[7]) {
  case '0':
    if (memEQ(name, "ABMON_1", 7)) {
    /*                      0     */
#if defined(ABMON_10) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))
      *iv_return = ABMON_10;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case '1':
    if (memEQ(name, "ABMON_1", 7)) {
    /*                      1     */
#if defined(ABMON_11) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))
      *iv_return = ABMON_11;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case '2':
    if (memEQ(name, "ABMON_1", 7)) {
    /*                      2     */
#if defined(ABMON_12) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))
      *iv_return = ABMON_12;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'R':
    if (memEQ(name, "CRNCYST", 7)) {
    /*                      R     */
#if defined(CRNCYSTR) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM) && defined(__SVR4_I386_ABI_L1__))
      *iv_return = CRNCYSTR;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  }
  return PERL_constant_NOTFOUND;
}

static int
constant_9 (pTHX_ const char *name, IV *iv_return) {
  /* When generated this function returned values for the list of names given
     here.  However, subsequent manual editing may have added or removed some.
     ERA_D_FMT ERA_T_FMT RADIXCHAR */
  /* Offset 4 gives the best switch position.  */
  switch (name[4]) {
  case 'D':
    if (memEQ(name, "ERA_D_FMT", 9)) {
    /*                   ^          */
#if defined(ERA_D_FMT) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))
      *iv_return = ERA_D_FMT;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'T':
    if (memEQ(name, "ERA_T_FMT", 9)) {
    /*                   ^          */
#if defined(ERA_T_FMT) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))
      *iv_return = ERA_T_FMT;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'X':
    if (memEQ(name, "RADIXCHAR", 9)) {
    /*                   ^          */
#if defined(RADIXCHAR) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM) && defined(__SVR4_I386_ABI_L1__))
      *iv_return = RADIXCHAR;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  }
  return PERL_constant_NOTFOUND;
}

static int
constant (pTHX_ const char *name, STRLEN len, IV *iv_return) {
  /* Initially switch on the length of the name.  */
  /* When generated this function returned values for the list of names given
     in this section of perl code.  Rather than manually editing these functions
     to add or remove constants, which would result in this comment and section
     of code becoming inaccurate, we recommend that you edit this section of
     code, and use it to regenerate a new set of constant functions which you
     then use to replace the originals.

     Regenerate these constant functions by feeding this entire source file to
     perl -x

#!/var/services/homes/spparmar/spec2006/tools/src/perl-5.8.7/miniperl -w
use ExtUtils::Constant qw (constant_types C_constant XS_constant);

my $types = {map {($_, 1)} qw(IV)};
my @names = (qw(CODESET),
            {name=>"ABDAY_1", type=>"IV", macro=>["#if defined(ABDAY_1) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))\n", "#endif\n"]},
            {name=>"ABDAY_2", type=>"IV", macro=>["#if defined(ABDAY_2) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))\n", "#endif\n"]},
            {name=>"ABDAY_3", type=>"IV", macro=>["#if defined(ABDAY_3) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))\n", "#endif\n"]},
            {name=>"ABDAY_4", type=>"IV", macro=>["#if defined(ABDAY_4) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))\n", "#endif\n"]},
            {name=>"ABDAY_5", type=>"IV", macro=>["#if defined(ABDAY_5) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))\n", "#endif\n"]},
            {name=>"ABDAY_6", type=>"IV", macro=>["#if defined(ABDAY_6) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))\n", "#endif\n"]},
            {name=>"ABDAY_7", type=>"IV", macro=>["#if defined(ABDAY_7) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))\n", "#endif\n"]},
            {name=>"ABMON_1", type=>"IV", macro=>["#if defined(ABMON_1) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))\n", "#endif\n"]},
            {name=>"ABMON_10", type=>"IV", macro=>["#if defined(ABMON_10) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))\n", "#endif\n"]},
            {name=>"ABMON_11", type=>"IV", macro=>["#if defined(ABMON_11) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))\n", "#endif\n"]},
            {name=>"ABMON_12", type=>"IV", macro=>["#if defined(ABMON_12) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))\n", "#endif\n"]},
            {name=>"ABMON_2", type=>"IV", macro=>["#if defined(ABMON_2) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))\n", "#endif\n"]},
            {name=>"ABMON_3", type=>"IV", macro=>["#if defined(ABMON_3) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))\n", "#endif\n"]},
            {name=>"ABMON_4", type=>"IV", macro=>["#if defined(ABMON_4) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))\n", "#endif\n"]},
            {name=>"ABMON_5", type=>"IV", macro=>["#if defined(ABMON_5) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))\n", "#endif\n"]},
            {name=>"ABMON_6", type=>"IV", macro=>["#if defined(ABMON_6) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))\n", "#endif\n"]},
            {name=>"ABMON_7", type=>"IV", macro=>["#if defined(ABMON_7) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))\n", "#endif\n"]},
            {name=>"ABMON_8", type=>"IV", macro=>["#if defined(ABMON_8) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))\n", "#endif\n"]},
            {name=>"ABMON_9", type=>"IV", macro=>["#if defined(ABMON_9) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))\n", "#endif\n"]},
            {name=>"ALT_DIGITS", type=>"IV", macro=>["#if defined(ALT_DIGITS) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))\n", "#endif\n"]},
            {name=>"AM_STR", type=>"IV", macro=>["#if defined(AM_STR) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))\n", "#endif\n"]},
            {name=>"CRNCYSTR", type=>"IV", macro=>["#if defined(CRNCYSTR) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM) && defined(__SVR4_I386_ABI_L1__))\n", "#endif\n"]},
            {name=>"DAY_1", type=>"IV", macro=>["#if defined(DAY_1) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))\n", "#endif\n"]},
            {name=>"DAY_2", type=>"IV", macro=>["#if defined(DAY_2) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))\n", "#endif\n"]},
            {name=>"DAY_3", type=>"IV", macro=>["#if defined(DAY_3) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))\n", "#endif\n"]},
            {name=>"DAY_4", type=>"IV", macro=>["#if defined(DAY_4) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))\n", "#endif\n"]},
            {name=>"DAY_5", type=>"IV", macro=>["#if defined(DAY_5) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))\n", "#endif\n"]},
            {name=>"DAY_6", type=>"IV", macro=>["#if defined(DAY_6) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))\n", "#endif\n"]},
            {name=>"DAY_7", type=>"IV", macro=>["#if defined(DAY_7) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))\n", "#endif\n"]},
            {name=>"D_FMT", type=>"IV", macro=>["#if defined(D_FMT) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))\n", "#endif\n"]},
            {name=>"D_T_FMT", type=>"IV", macro=>["#if defined(D_T_FMT) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))\n", "#endif\n"]},
            {name=>"ERA", type=>"IV", macro=>["#if defined(ERA) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))\n", "#endif\n"]},
            {name=>"ERA_D_FMT", type=>"IV", macro=>["#if defined(ERA_D_FMT) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))\n", "#endif\n"]},
            {name=>"ERA_D_T_FMT", type=>"IV", macro=>["#if defined(ERA_D_T_FMT) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))\n", "#endif\n"]},
            {name=>"ERA_T_FMT", type=>"IV", macro=>["#if defined(ERA_T_FMT) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))\n", "#endif\n"]},
            {name=>"MON_1", type=>"IV", macro=>["#if defined(MON_1) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))\n", "#endif\n"]},
            {name=>"MON_10", type=>"IV", macro=>["#if defined(MON_10) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))\n", "#endif\n"]},
            {name=>"MON_11", type=>"IV", macro=>["#if defined(MON_11) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))\n", "#endif\n"]},
            {name=>"MON_12", type=>"IV", macro=>["#if defined(MON_12) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))\n", "#endif\n"]},
            {name=>"MON_2", type=>"IV", macro=>["#if defined(MON_2) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))\n", "#endif\n"]},
            {name=>"MON_3", type=>"IV", macro=>["#if defined(MON_3) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))\n", "#endif\n"]},
            {name=>"MON_4", type=>"IV", macro=>["#if defined(MON_4) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))\n", "#endif\n"]},
            {name=>"MON_5", type=>"IV", macro=>["#if defined(MON_5) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))\n", "#endif\n"]},
            {name=>"MON_6", type=>"IV", macro=>["#if defined(MON_6) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))\n", "#endif\n"]},
            {name=>"MON_7", type=>"IV", macro=>["#if defined(MON_7) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))\n", "#endif\n"]},
            {name=>"MON_8", type=>"IV", macro=>["#if defined(MON_8) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))\n", "#endif\n"]},
            {name=>"MON_9", type=>"IV", macro=>["#if defined(MON_9) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))\n", "#endif\n"]},
            {name=>"NOEXPR", type=>"IV", macro=>["#if defined(NOEXPR) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))\n", "#endif\n"]},
            {name=>"NOSTR", type=>"IV", macro=>["#if defined(NOSTR) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))\n", "#endif\n"]},
            {name=>"PM_STR", type=>"IV", macro=>["#if defined(PM_STR) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))\n", "#endif\n"]},
            {name=>"RADIXCHAR", type=>"IV", macro=>["#if defined(RADIXCHAR) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM) && defined(__SVR4_I386_ABI_L1__))\n", "#endif\n"]},
            {name=>"THOUSEP", type=>"IV", macro=>["#if defined(THOUSEP) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM) && defined(__SVR4_I386_ABI_L1__))\n", "#endif\n"]},
            {name=>"T_FMT", type=>"IV", macro=>["#if defined(T_FMT) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))\n", "#endif\n"]},
            {name=>"T_FMT_AMPM", type=>"IV", macro=>["#if defined(T_FMT_AMPM) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))\n", "#endif\n"]},
            {name=>"YESEXPR", type=>"IV", macro=>["#if defined(YESEXPR) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))\n", "#endif\n"]},
            {name=>"YESSTR", type=>"IV", macro=>["#if defined(YESSTR) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))\n", "#endif\n"]});

print constant_types(); # macro defs
foreach (C_constant ("I18N::Langinfo", 'constant', 'IV', $types, undef, 3, @names) ) {
    print $_, "\n"; # C constant subs
}
print "#### XS Section:\n";
print XS_constant ("I18N::Langinfo", $types);
__END__
   */

  switch (len) {
  case 3:
    if (memEQ(name, "ERA", 3)) {
#if defined(ERA) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))
      *iv_return = ERA;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 5:
    return constant_5 (aTHX_ name, iv_return);
    break;
  case 6:
    return constant_6 (aTHX_ name, iv_return);
    break;
  case 7:
    return constant_7 (aTHX_ name, iv_return);
    break;
  case 8:
    return constant_8 (aTHX_ name, iv_return);
    break;
  case 9:
    return constant_9 (aTHX_ name, iv_return);
    break;
  case 10:
    /* Names all of length 10.  */
    /* ALT_DIGITS T_FMT_AMPM */
    /* Offset 7 gives the best switch position.  */
    switch (name[7]) {
    case 'I':
      if (memEQ(name, "ALT_DIGITS", 10)) {
      /*                      ^         */
#if defined(ALT_DIGITS) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))
        *iv_return = ALT_DIGITS;
        return PERL_constant_ISIV;
#else
        return PERL_constant_NOTDEF;
#endif
      }
      break;
    case 'M':
      if (memEQ(name, "T_FMT_AMPM", 10)) {
      /*                      ^         */
#if defined(T_FMT_AMPM) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))
        *iv_return = T_FMT_AMPM;
        return PERL_constant_ISIV;
#else
        return PERL_constant_NOTDEF;
#endif
      }
      break;
    }
    break;
  case 11:
    if (memEQ(name, "ERA_D_T_FMT", 11)) {
#if defined(ERA_D_T_FMT) || (defined(__GNU_LIBRARY__) && defined(_NL_ITEM))
      *iv_return = ERA_D_T_FMT;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  }
  return PERL_constant_NOTFOUND;
}

