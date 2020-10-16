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
my @names = (qw(GLOB_ABEND GLOB_ALPHASORT GLOB_ALTDIRFUNC GLOB_BRACE GLOB_ERR
	       GLOB_LIMIT GLOB_MARK GLOB_NOCASE GLOB_NOCHECK GLOB_NOMAGIC
	       GLOB_NOSORT GLOB_NOSPACE GLOB_QUOTE GLOB_TILDE),
            {name=>"GLOB_ERROR", type=>"IV", macro=>["#ifdef GLOB_ERROR\n\tdMY_CXT;\n\n", "#endif\n"]});

print constant_types(); # macro defs
foreach (C_constant ("File::Glob", 'constant', 'IV', $types, undef, 8, @names) ) {
    print $_, "\n"; # C constant subs
}
print "#### XS Section:\n";
print XS_constant ("File::Glob", $types);
__END__
   */

  switch (len) {
  case 8:
    if (memEQ(name, "GLOB_ERR", 8)) {
#ifdef GLOB_ERR
      *iv_return = GLOB_ERR;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 9:
    if (memEQ(name, "GLOB_MARK", 9)) {
#ifdef GLOB_MARK
      *iv_return = GLOB_MARK;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 10:
    /* Names all of length 10.  */
    /* GLOB_ABEND GLOB_BRACE GLOB_ERROR GLOB_LIMIT GLOB_QUOTE GLOB_TILDE */
    /* Offset 7 gives the best switch position.  */
    switch (name[7]) {
    case 'A':
      if (memEQ(name, "GLOB_BRACE", 10)) {
      /*                      ^         */
#ifdef GLOB_BRACE
        *iv_return = GLOB_BRACE;
        return PERL_constant_ISIV;
#else
        return PERL_constant_NOTDEF;
#endif
      }
      break;
    case 'E':
      if (memEQ(name, "GLOB_ABEND", 10)) {
      /*                      ^         */
#ifdef GLOB_ABEND
        *iv_return = GLOB_ABEND;
        return PERL_constant_ISIV;
#else
        return PERL_constant_NOTDEF;
#endif
      }
      break;
    case 'L':
      if (memEQ(name, "GLOB_TILDE", 10)) {
      /*                      ^         */
#ifdef GLOB_TILDE
        *iv_return = GLOB_TILDE;
        return PERL_constant_ISIV;
#else
        return PERL_constant_NOTDEF;
#endif
      }
      break;
    case 'M':
      if (memEQ(name, "GLOB_LIMIT", 10)) {
      /*                      ^         */
#ifdef GLOB_LIMIT
        *iv_return = GLOB_LIMIT;
        return PERL_constant_ISIV;
#else
        return PERL_constant_NOTDEF;
#endif
      }
      break;
    case 'O':
      if (memEQ(name, "GLOB_QUOTE", 10)) {
      /*                      ^         */
#ifdef GLOB_QUOTE
        *iv_return = GLOB_QUOTE;
        return PERL_constant_ISIV;
#else
        return PERL_constant_NOTDEF;
#endif
      }
      break;
    case 'R':
      if (memEQ(name, "GLOB_ERROR", 10)) {
      /*                      ^         */
#ifdef GLOB_ERROR
	dMY_CXT;

        *iv_return = GLOB_ERROR;
        return PERL_constant_ISIV;
#else
        return PERL_constant_NOTDEF;
#endif
      }
      break;
    }
    break;
  case 11:
    /* Names all of length 11.  */
    /* GLOB_NOCASE GLOB_NOSORT */
    /* Offset 9 gives the best switch position.  */
    switch (name[9]) {
    case 'R':
      if (memEQ(name, "GLOB_NOSORT", 11)) {
      /*                        ^        */
#ifdef GLOB_NOSORT
        *iv_return = GLOB_NOSORT;
        return PERL_constant_ISIV;
#else
        return PERL_constant_NOTDEF;
#endif
      }
      break;
    case 'S':
      if (memEQ(name, "GLOB_NOCASE", 11)) {
      /*                        ^        */
#ifdef GLOB_NOCASE
        *iv_return = GLOB_NOCASE;
        return PERL_constant_ISIV;
#else
        return PERL_constant_NOTDEF;
#endif
      }
      break;
    }
    break;
  case 12:
    /* Names all of length 12.  */
    /* GLOB_NOCHECK GLOB_NOMAGIC GLOB_NOSPACE */
    /* Offset 9 gives the best switch position.  */
    switch (name[9]) {
    case 'A':
      if (memEQ(name, "GLOB_NOSPACE", 12)) {
      /*                        ^         */
#ifdef GLOB_NOSPACE
        *iv_return = GLOB_NOSPACE;
        return PERL_constant_ISIV;
#else
        return PERL_constant_NOTDEF;
#endif
      }
      break;
    case 'E':
      if (memEQ(name, "GLOB_NOCHECK", 12)) {
      /*                        ^         */
#ifdef GLOB_NOCHECK
        *iv_return = GLOB_NOCHECK;
        return PERL_constant_ISIV;
#else
        return PERL_constant_NOTDEF;
#endif
      }
      break;
    case 'G':
      if (memEQ(name, "GLOB_NOMAGIC", 12)) {
      /*                        ^         */
#ifdef GLOB_NOMAGIC
        *iv_return = GLOB_NOMAGIC;
        return PERL_constant_ISIV;
#else
        return PERL_constant_NOTDEF;
#endif
      }
      break;
    }
    break;
  case 14:
    if (memEQ(name, "GLOB_ALPHASORT", 14)) {
#ifdef GLOB_ALPHASORT
      *iv_return = GLOB_ALPHASORT;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 15:
    if (memEQ(name, "GLOB_ALTDIRFUNC", 15)) {
#ifdef GLOB_ALTDIRFUNC
      *iv_return = GLOB_ALTDIRFUNC;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  }
  return PERL_constant_NOTFOUND;
}

