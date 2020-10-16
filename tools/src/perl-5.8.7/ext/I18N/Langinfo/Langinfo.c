/*
 * This file was generated automatically by xsubpp version 1.9508 from the
 * contents of Langinfo.xs. Do not edit this file, edit Langinfo.xs instead.
 *
 *	ANY CHANGES MADE HERE WILL BE LOST!
 *
 */

#line 1 "Langinfo.xs"
#include "EXTERN.h"
#include "perl.h"
#include "XSUB.h"

#ifdef I_LANGINFO
#   define __USE_GNU 1 /* Enables YESSTR, otherwise only __YESSTR. */
#   include <langinfo.h>
#endif

#include "const-c.inc"

#line 22 "Langinfo.c"

/* INCLUDE:  Including 'const-xs.inc' from 'Langinfo.xs' */

XS(XS_I18N__Langinfo_constant); /* prototype to pass -Wmissing-prototypes */
XS(XS_I18N__Langinfo_constant)
{
    dXSARGS;
    if (items != 1)
	Perl_croak(aTHX_ "Usage: I18N::Langinfo::constant(sv)");
    SP -= items;
    {
#line 4 "const-xs.inc"
#ifdef dXSTARG
	dXSTARG; /* Faster if we have it.  */
#else
	dTARGET;
#endif
	STRLEN		len;
        int		type;
	IV		iv;
	/* NV		nv;	Uncomment this if you need to return NVs */
	/* const char	*pv;	Uncomment this if you need to return PVs */
#line 45 "Langinfo.c"
	SV *	sv = ST(0);
	const char *	s = SvPV(sv, len);
#line 18 "const-xs.inc"
        /* Change this to constant(aTHX_ s, len, &iv, &nv);
           if you need to return both NVs and IVs */
	type = constant(aTHX_ s, len, &iv);
      /* Return 1 or 2 items. First is error message, or undef if no error.
           Second, if present, is found value */
        switch (type) {
        case PERL_constant_NOTFOUND:
          sv = sv_2mortal(newSVpvf("%s is not a valid I18N::Langinfo macro", s));
          PUSHs(sv);
          break;
        case PERL_constant_NOTDEF:
          sv = sv_2mortal(newSVpvf(
	    "Your vendor has not defined I18N::Langinfo macro %s, used", s));
          PUSHs(sv);
          break;
        case PERL_constant_ISIV:
          EXTEND(SP, 1);
          PUSHs(&PL_sv_undef);
          PUSHi(iv);
          break;
	/* Uncomment this if you need to return NOs
        case PERL_constant_ISNO:
          EXTEND(SP, 1);
          PUSHs(&PL_sv_undef);
          PUSHs(&PL_sv_no);
          break; */
	/* Uncomment this if you need to return NVs
        case PERL_constant_ISNV:
          EXTEND(SP, 1);
          PUSHs(&PL_sv_undef);
          PUSHn(nv);
          break; */
	/* Uncomment this if you need to return PVs
        case PERL_constant_ISPV:
          EXTEND(SP, 1);
          PUSHs(&PL_sv_undef);
          PUSHp(pv, strlen(pv));
          break; */
	/* Uncomment this if you need to return PVNs
        case PERL_constant_ISPVN:
          EXTEND(SP, 1);
          PUSHs(&PL_sv_undef);
          PUSHp(pv, iv);
          break; */
	/* Uncomment this if you need to return SVs
        case PERL_constant_ISSV:
          EXTEND(SP, 1);
          PUSHs(&PL_sv_undef);
          PUSHs(sv);
          break; */
	/* Uncomment this if you need to return UNDEFs
        case PERL_constant_ISUNDEF:
          break; */
	/* Uncomment this if you need to return UVs
        case PERL_constant_ISUV:
          EXTEND(SP, 1);
          PUSHs(&PL_sv_undef);
          PUSHu((UV)iv);
          break; */
	/* Uncomment this if you need to return YESs
        case PERL_constant_ISYES:
          EXTEND(SP, 1);
          PUSHs(&PL_sv_undef);
          PUSHs(&PL_sv_yes);
          break; */
        default:
          sv = sv_2mortal(newSVpvf(
	    "Unexpected return type %d while processing I18N::Langinfo macro %s, used",
               type, s));
          PUSHs(sv);
        }
#line 120 "Langinfo.c"
	PUTBACK;
	return;
    }
}


/* INCLUDE: Returning to 'Langinfo.xs' from 'const-xs.inc' */

XS(XS_I18N__Langinfo_langinfo); /* prototype to pass -Wmissing-prototypes */
XS(XS_I18N__Langinfo_langinfo)
{
    dXSARGS;
    if (items != 1)
	Perl_croak(aTHX_ "Usage: I18N::Langinfo::langinfo(code)");
    {
	int	code = (int)SvIV(ST(0));
	SV *	RETVAL;
#line 22 "Langinfo.xs"
#ifdef HAS_NL_LANGINFO
	{
	  char *s;

	  if ((s = nl_langinfo(code)))
	      RETVAL = newSVpvn(s, strlen(s));
	  else
	      RETVAL = &PL_sv_undef;
	}
#else
	croak("nl_langinfo() not implemented on this architecture");
#endif
#line 151 "Langinfo.c"
	ST(0) = RETVAL;
	sv_2mortal(ST(0));
    }
    XSRETURN(1);
}

#ifdef __cplusplus
extern "C"
#endif
XS(boot_I18N__Langinfo); /* prototype to pass -Wmissing-prototypes */
XS(boot_I18N__Langinfo)
{
    dXSARGS;
    char* file = __FILE__;

    XS_VERSION_BOOTCHECK ;

        newXSproto("I18N::Langinfo::constant", XS_I18N__Langinfo_constant, file, "$");
        newXSproto("I18N::Langinfo::langinfo", XS_I18N__Langinfo_langinfo, file, "$");
    XSRETURN_YES;
}

