/*
 * This file was generated automatically by xsubpp version 1.9508 from the
 * contents of Syslog.xs. Do not edit this file, edit Syslog.xs instead.
 *
 *	ANY CHANGES MADE HERE WILL BE LOST!
 *
 */

#line 1 "Syslog.xs"
#include "EXTERN.h"
#include "perl.h"
#include "XSUB.h"

#ifdef I_SYSLOG
#include <syslog.h>
#endif

#include "const-c.inc"

#line 21 "Syslog.c"

/* INCLUDE:  Including 'const-xs.inc' from 'Syslog.xs' */

XS(XS_Sys__Syslog_constant); /* prototype to pass -Wmissing-prototypes */
XS(XS_Sys__Syslog_constant)
{
    dXSARGS;
    if (items != 1)
	Perl_croak(aTHX_ "Usage: Sys::Syslog::constant(sv)");
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
	const char	*pv;
#line 44 "Syslog.c"
	SV *	sv = ST(0);
	const char *	s = SvPV(sv, len);
#line 18 "const-xs.inc"
        /* Change this to constant(aTHX_ s, len, &iv, &nv);
           if you need to return both NVs and IVs */
	type = constant(aTHX_ s, len, &iv, &pv);
      /* Return 1 or 2 items. First is error message, or undef if no error.
           Second, if present, is found value */
        switch (type) {
        case PERL_constant_NOTFOUND:
          sv = sv_2mortal(newSVpvf("%s is not a valid Sys::Syslog macro", s));
          PUSHs(sv);
          break;
        case PERL_constant_NOTDEF:
          sv = sv_2mortal(newSVpvf(
	    "Your vendor has not defined Sys::Syslog macro %s, used", s));
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
        case PERL_constant_ISPV:
          EXTEND(SP, 1);
          PUSHs(&PL_sv_undef);
          PUSHp(pv, strlen(pv));
          break;
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
	    "Unexpected return type %d while processing Sys::Syslog macro %s, used",
               type, s));
          PUSHs(sv);
        }
#line 118 "Syslog.c"
	PUTBACK;
	return;
    }
}


/* INCLUDE: Returning to 'Syslog.xs' from 'const-xs.inc' */

XS(XS_Sys__Syslog_LOG_FAC); /* prototype to pass -Wmissing-prototypes */
XS(XS_Sys__Syslog_LOG_FAC)
{
    dXSARGS;
    if (items != 1)
	Perl_croak(aTHX_ "Usage: Sys::Syslog::LOG_FAC(p)");
    {
	int	p = (int)SvIV(ST(0));
	int	RETVAL;
	dXSTARG;
#line 20 "Syslog.xs"
#ifdef LOG_FAC
	RETVAL = LOG_FAC(p);
#else
	croak("Your vendor has not defined the Sys::Syslog macro LOG_FAC");
	RETVAL = -1;
#endif
#line 144 "Syslog.c"
	XSprePUSH; PUSHi((IV)RETVAL);
    }
    XSRETURN(1);
}

XS(XS_Sys__Syslog_LOG_PRI); /* prototype to pass -Wmissing-prototypes */
XS(XS_Sys__Syslog_LOG_PRI)
{
    dXSARGS;
    if (items != 1)
	Perl_croak(aTHX_ "Usage: Sys::Syslog::LOG_PRI(p)");
    {
	int	p = (int)SvIV(ST(0));
	int	RETVAL;
	dXSTARG;
#line 34 "Syslog.xs"
#ifdef LOG_PRI
	RETVAL = LOG_PRI(p);
#else
	croak("Your vendor has not defined the Sys::Syslog macro LOG_PRI");
	RETVAL = -1;
#endif
#line 167 "Syslog.c"
	XSprePUSH; PUSHi((IV)RETVAL);
    }
    XSRETURN(1);
}

XS(XS_Sys__Syslog_LOG_MAKEPRI); /* prototype to pass -Wmissing-prototypes */
XS(XS_Sys__Syslog_LOG_MAKEPRI)
{
    dXSARGS;
    if (items != 2)
	Perl_croak(aTHX_ "Usage: Sys::Syslog::LOG_MAKEPRI(fac, pri)");
    {
	int	fac = (int)SvIV(ST(0));
	int	pri = (int)SvIV(ST(1));
	int	RETVAL;
	dXSTARG;
#line 49 "Syslog.xs"
#ifdef LOG_MAKEPRI
	RETVAL = LOG_MAKEPRI(fac,pri);
#else
	croak("Your vendor has not defined the Sys::Syslog macro LOG_MAKEPRI");
	RETVAL = -1;
#endif
#line 191 "Syslog.c"
	XSprePUSH; PUSHi((IV)RETVAL);
    }
    XSRETURN(1);
}

XS(XS_Sys__Syslog_LOG_MASK); /* prototype to pass -Wmissing-prototypes */
XS(XS_Sys__Syslog_LOG_MASK)
{
    dXSARGS;
    if (items != 1)
	Perl_croak(aTHX_ "Usage: Sys::Syslog::LOG_MASK(pri)");
    {
	int	pri = (int)SvIV(ST(0));
	int	RETVAL;
	dXSTARG;
#line 63 "Syslog.xs"
#ifdef LOG_MASK
	RETVAL = LOG_MASK(pri);
#else
	croak("Your vendor has not defined the Sys::Syslog macro LOG_MASK");
	RETVAL = -1;
#endif
#line 214 "Syslog.c"
	XSprePUSH; PUSHi((IV)RETVAL);
    }
    XSRETURN(1);
}

XS(XS_Sys__Syslog_LOG_UPTO); /* prototype to pass -Wmissing-prototypes */
XS(XS_Sys__Syslog_LOG_UPTO)
{
    dXSARGS;
    if (items != 1)
	Perl_croak(aTHX_ "Usage: Sys::Syslog::LOG_UPTO(pri)");
    {
	int	pri = (int)SvIV(ST(0));
	int	RETVAL;
	dXSTARG;
#line 77 "Syslog.xs"
#ifdef LOG_UPTO
	RETVAL = LOG_UPTO(pri);
#else
	croak("Your vendor has not defined the Sys::Syslog macro LOG_UPTO");
	RETVAL = -1;
#endif
#line 237 "Syslog.c"
	XSprePUSH; PUSHi((IV)RETVAL);
    }
    XSRETURN(1);
}

#ifdef __cplusplus
extern "C"
#endif
XS(boot_Sys__Syslog); /* prototype to pass -Wmissing-prototypes */
XS(boot_Sys__Syslog)
{
    dXSARGS;
    char* file = __FILE__;

    XS_VERSION_BOOTCHECK ;

        newXS("Sys::Syslog::constant", XS_Sys__Syslog_constant, file);
        newXS("Sys::Syslog::LOG_FAC", XS_Sys__Syslog_LOG_FAC, file);
        newXS("Sys::Syslog::LOG_PRI", XS_Sys__Syslog_LOG_PRI, file);
        newXS("Sys::Syslog::LOG_MAKEPRI", XS_Sys__Syslog_LOG_MAKEPRI, file);
        newXS("Sys::Syslog::LOG_MASK", XS_Sys__Syslog_LOG_MASK, file);
        newXS("Sys::Syslog::LOG_UPTO", XS_Sys__Syslog_LOG_UPTO, file);
    XSRETURN_YES;
}

