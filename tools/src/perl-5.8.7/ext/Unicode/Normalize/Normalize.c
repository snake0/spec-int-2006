/*
 * This file was generated automatically by xsubpp version 1.9508 from the
 * contents of Normalize.xs. Do not edit this file, edit Normalize.xs instead.
 *
 *	ANY CHANGES MADE HERE WILL BE LOST!
 *
 */

#line 1 "Normalize.xs"

#include "EXTERN.h"
#include "perl.h"
#include "XSUB.h"

/* These 5 files are prepared by mkheader */
#include "unfcmb.h"
#include "unfcan.h"
#include "unfcpt.h"
#include "unfcmp.h"
#include "unfexc.h"

/* Perl 5.6.1 ? */
#ifndef uvuni_to_utf8
#define uvuni_to_utf8   uv_to_utf8
#endif /* uvuni_to_utf8 */

/* Perl 5.6.1 ? */
#ifndef utf8n_to_uvuni
#define utf8n_to_uvuni  utf8_to_uv
#endif /* utf8n_to_uvuni */

/* UTF8_ALLOW_BOM is used before Perl 5.8.0 */
#ifdef UTF8_ALLOW_BOM
#define AllowAnyUTF (UTF8_ALLOW_SURROGATE|UTF8_ALLOW_BOM|UTF8_ALLOW_FFFF)
#else
#define AllowAnyUTF (UTF8_ALLOW_SURROGATE|UTF8_ALLOW_FFFF)
#endif

/* if utf8n_to_uvuni() sets retlen to 0 (?) */
#define ErrRetlenIsZero "panic (Unicode::Normalize): zero-length character"

/* utf8_hop() hops back before start. Maybe broken UTF-8 */
#define ErrHopBeforeStart "panic (Unicode::Normalize): hopping before start"

/* At present, char > 0x10ffff are unaffected without complaint, right? */
#define VALID_UTF_MAX    (0x10ffff)
#define OVER_UTF_MAX(uv) (VALID_UTF_MAX < (uv))

/* HANGUL_H */
#define Hangul_SBase  0xAC00
#define Hangul_SFinal 0xD7A3
#define Hangul_SCount  11172

#define Hangul_NCount    588

#define Hangul_LBase  0x1100
#define Hangul_LFinal 0x1112
#define Hangul_LCount     19

#define Hangul_VBase  0x1161
#define Hangul_VFinal 0x1175
#define Hangul_VCount     21

#define Hangul_TBase  0x11A7
#define Hangul_TFinal 0x11C2
#define Hangul_TCount     28

#define Hangul_IsS(u)  ((Hangul_SBase <= (u)) && ((u) <= Hangul_SFinal))
#define Hangul_IsN(u)  (((u) - Hangul_SBase) % Hangul_TCount == 0)
#define Hangul_IsLV(u) (Hangul_IsS(u) && Hangul_IsN(u))
#define Hangul_IsL(u)  ((Hangul_LBase <= (u)) && ((u) <= Hangul_LFinal))
#define Hangul_IsV(u)  ((Hangul_VBase <= (u)) && ((u) <= Hangul_VFinal))
#define Hangul_IsT(u)  ((Hangul_TBase  < (u)) && ((u) <= Hangul_TFinal))
/* HANGUL_H */

/* this is used for canonical ordering of combining characters (c.c.). */
typedef struct {
    U8 cc;	/* combining class */
    UV uv;	/* codepoint */
    STRLEN pos; /* position */
} UNF_cc;

static int compare_cc (const void *a, const void *b)
{
    int ret_cc;
    ret_cc = ((UNF_cc*) a)->cc - ((UNF_cc*) b)->cc;
    if (ret_cc)
	return ret_cc;

    return ( ((UNF_cc*) a)->pos > ((UNF_cc*) b)->pos )
	 - ( ((UNF_cc*) a)->pos < ((UNF_cc*) b)->pos );
}

static U8* dec_canonical (UV uv)
{
    U8 ***plane, **row;
    if (OVER_UTF_MAX(uv))
	return NULL;
    plane = (U8***)UNF_canon[uv >> 16];
    if (! plane)
	return NULL;
    row = plane[(uv >> 8) & 0xff];
    return row ? row[uv & 0xff] : NULL;
}

static U8* dec_compat (UV uv)
{
    U8 ***plane, **row;
    if (OVER_UTF_MAX(uv))
	return NULL;
    plane = (U8***)UNF_compat[uv >> 16];
    if (! plane)
	return NULL;
    row = plane[(uv >> 8) & 0xff];
    return row ? row[uv & 0xff] : NULL;
}

static UV composite_uv (UV uv, UV uv2)
{
    UNF_complist ***plane, **row, *cell, *i;

    if (! uv2 || OVER_UTF_MAX(uv) || OVER_UTF_MAX(uv2))
	return 0;

    if (Hangul_IsL(uv) && Hangul_IsV(uv2)) {
	uv  -= Hangul_LBase; /* lindex */
	uv2 -= Hangul_VBase; /* vindex */
	return(Hangul_SBase + (uv * Hangul_VCount + uv2) * Hangul_TCount);
    }
    if (Hangul_IsLV(uv) && Hangul_IsT(uv2)) {
	uv2 -= Hangul_TBase; /* tindex */
	return(uv + uv2);
    }
    plane = UNF_compos[uv >> 16];
    if (! plane)
	return 0;
    row = plane[(uv >> 8) & 0xff];
    if (! row)
	return 0;
    cell = row[uv & 0xff];
    if (! cell)
	return 0;
    for (i = cell; i->nextchar; i++) {
	if (uv2 == i->nextchar)
	    return i->composite;
    }
    return 0;
}

static U8 getCombinClass (UV uv)
{
    U8 **plane, *row;
    if (OVER_UTF_MAX(uv))
	return 0;
    plane = (U8**)UNF_combin[uv >> 16];
    if (! plane)
	return 0;
    row = plane[(uv >> 8) & 0xff];
    return row ? row[uv & 0xff] : 0;
}

static void sv_cat_decompHangul (SV* sv, UV uv)
{
    UV sindex, lindex, vindex, tindex;
    U8 *t, tmp[3 * UTF8_MAXLEN + 1];

    if (! Hangul_IsS(uv))
	return;

    sindex =  uv - Hangul_SBase;
    lindex =  sindex / Hangul_NCount;
    vindex = (sindex % Hangul_NCount) / Hangul_TCount;
    tindex =  sindex % Hangul_TCount;

    t = tmp;
    t = uvuni_to_utf8(t, (lindex + Hangul_LBase));
    t = uvuni_to_utf8(t, (vindex + Hangul_VBase));
    if (tindex)
	t = uvuni_to_utf8(t, (tindex + Hangul_TBase));
    *t = '\0';
    sv_catpvn(sv, (char *)tmp, t - tmp);
}

static void sv_cat_uvuni (SV* sv, UV uv)
{
    U8 *t, tmp[UTF8_MAXLEN + 1];

    t = tmp;
    t = uvuni_to_utf8(t, uv);
    *t = '\0';
    sv_catpvn(sv, (char *)tmp, t - tmp);
}

static char * sv_2pvunicode(SV *sv, STRLEN *lp)
{
    char *s;
    STRLEN len;
    s = (char*)SvPV(sv,len);
    if (!SvUTF8(sv)) {
	SV* tmpsv = sv_mortalcopy(sv);
	if (!SvPOK(tmpsv))
	    (void)sv_pvn_force(tmpsv,&len);
	sv_utf8_upgrade(tmpsv);
	s = (char*)SvPV(tmpsv,len);
    }
    *lp = len;
    return s;
}

#line 211 "Normalize.c"
XS(XS_Unicode__Normalize_decompose); /* prototype to pass -Wmissing-prototypes */
XS(XS_Unicode__Normalize_decompose)
{
    dXSARGS;
    if (items < 1 || items > 2)
	Perl_croak(aTHX_ "Usage: Unicode::Normalize::decompose(src, compat = &PL_sv_no)");
    {
	SV *	src = ST(0);
	SV *	compat;
#line 209 "Normalize.xs"
    SV *dst;
    STRLEN srclen, retlen;
    U8 *s, *e, *p, *r;
    UV uv;
    bool iscompat;
#line 227 "Normalize.c"
	SV *	RETVAL;

	if (items < 2)
	    compat = &PL_sv_no;
	else {
	    compat = ST(1);
	}
#line 215 "Normalize.xs"
    iscompat = SvTRUE(compat);
    s = (U8*)sv_2pvunicode(src,&srclen);
    e = s + srclen;

    dst = newSV(1);
    (void)SvPOK_only(dst);
    SvUTF8_on(dst);

    for (p = s; p < e; p += retlen) {
	uv = utf8n_to_uvuni(p, e - p, &retlen, AllowAnyUTF);
	if (!retlen)
	    croak(ErrRetlenIsZero);

	if (Hangul_IsS(uv))
	    sv_cat_decompHangul(dst, uv);
	else {
	    r = iscompat ? dec_compat(uv) : dec_canonical(uv);
	    if (r)
		sv_catpv(dst, (char *)r);
	    else
		sv_cat_uvuni(dst, uv);
	}
    }
    RETVAL = dst;
#line 260 "Normalize.c"
	ST(0) = RETVAL;
	sv_2mortal(ST(0));
    }
    XSRETURN(1);
}

XS(XS_Unicode__Normalize_reorder); /* prototype to pass -Wmissing-prototypes */
XS(XS_Unicode__Normalize_reorder)
{
    dXSARGS;
    if (items != 1)
	Perl_croak(aTHX_ "Usage: Unicode::Normalize::reorder(src)");
    {
	SV *	src = ST(0);
#line 249 "Normalize.xs"
    SV *dst;
    STRLEN srclen, dstlen, retlen, stk_cc_max;
    U8 *s, *e, *p, *d, curCC;
    UV uv, uvlast;
    UNF_cc * stk_cc;
    STRLEN i, cc_pos;
    bool valid_uvlast;
#line 283 "Normalize.c"
	SV *	RETVAL;
#line 257 "Normalize.xs"
    s = (U8*)sv_2pvunicode(src,&srclen);
    e = s + srclen;

    dstlen = srclen + 1;
    dst = newSV(dstlen);
    (void)SvPOK_only(dst);
    SvUTF8_on(dst);
    d = (U8*)SvPVX(dst);

    stk_cc_max = 10; /* enough as an initial value? */
    New(0, stk_cc, stk_cc_max, UNF_cc);

    for (p = s; p < e;) {
	uv = utf8n_to_uvuni(p, e - p, &retlen, AllowAnyUTF);
	if (!retlen)
	    croak(ErrRetlenIsZero);
	p += retlen;

	curCC = getCombinClass(uv);
	if (curCC == 0) {
	    d = uvuni_to_utf8(d, uv);
	    continue;
	}

	cc_pos = 0;
	stk_cc[cc_pos].cc  = curCC;
	stk_cc[cc_pos].uv  = uv;
	stk_cc[cc_pos].pos = cc_pos;

	valid_uvlast = FALSE;
	while (p < e) {
	    uv = utf8n_to_uvuni(p, e - p, &retlen, AllowAnyUTF);
	    if (!retlen)
		croak(ErrRetlenIsZero);
	    p += retlen;

	    curCC = getCombinClass(uv);
	    if (curCC == 0) {
		uvlast = uv;
		valid_uvlast = TRUE;
		break;
	    }

	    cc_pos++;
	    if (stk_cc_max <= cc_pos) { /* extend if need */
		stk_cc_max = cc_pos + 1;
		Renew(stk_cc, stk_cc_max, UNF_cc);
	    }
	    stk_cc[cc_pos].cc  = curCC;
	    stk_cc[cc_pos].uv  = uv;
	    stk_cc[cc_pos].pos = cc_pos;
	}

	/* reordered if there are two c.c.'s */
	if (cc_pos) {
	    qsort((void*)stk_cc, cc_pos + 1, sizeof(UNF_cc), compare_cc);
	}

	for (i = 0; i <= cc_pos; i++) {
	    d = uvuni_to_utf8(d, stk_cc[i].uv);
	}
	if (valid_uvlast)
	{
	    d = uvuni_to_utf8(d, uvlast);
	}
    }
    *d = '\0';
    SvCUR_set(dst, d - (U8*)SvPVX(dst));
    Safefree(stk_cc);
    RETVAL = dst;
#line 356 "Normalize.c"
	ST(0) = RETVAL;
	sv_2mortal(ST(0));
    }
    XSRETURN(1);
}

XS(XS_Unicode__Normalize_compose); /* prototype to pass -Wmissing-prototypes */
XS(XS_Unicode__Normalize_compose)
{
    dXSARGS;
    dXSI32;
    if (items != 1)
       Perl_croak(aTHX_ "Usage: %s(src)", GvNAME(CvGV(cv)));
    {
	SV *	src = ST(0);
#line 339 "Normalize.xs"
    SV  *dst, *tmp;
    U8  *s, *p, *e, *d, *t, *tmp_start, curCC, preCC;
    UV uv, uvS, uvComp;
    STRLEN srclen, dstlen, tmplen, retlen;
    bool beginning = TRUE;
#line 378 "Normalize.c"
	SV *	RETVAL;
#line 345 "Normalize.xs"
    s = (U8*)sv_2pvunicode(src,&srclen);
    e = s + srclen;

    dstlen = srclen + 1;
    dst = newSV(dstlen);
    (void)SvPOK_only(dst);
    SvUTF8_on(dst);
    d = (U8*)SvPVX(dst);

  /* for uncomposed combining char */
    tmp = sv_2mortal(newSV(dstlen));
    (void)SvPOK_only(tmp);
    SvUTF8_on(tmp);

    for (p = s; p < e;) {
	if (beginning) {
	    uvS = utf8n_to_uvuni(p, e - p, &retlen, AllowAnyUTF);
	    if (!retlen)
		croak(ErrRetlenIsZero);
	    p += retlen;

            if (getCombinClass(uvS)) { /* no Starter found yet */
		d = uvuni_to_utf8(d, uvS);
		continue;
	    }
            beginning = FALSE;
	}

    /* Starter */
	t = tmp_start = (U8*)SvPVX(tmp);
	preCC = 0;

    /* to the next Starter */
	while (p < e) {
	    uv = utf8n_to_uvuni(p, e - p, &retlen, AllowAnyUTF);
	    if (!retlen)
		croak(ErrRetlenIsZero);
	    p += retlen;

	    curCC = getCombinClass(uv);

	    if (preCC && preCC == curCC) {
		preCC = curCC;
		t = uvuni_to_utf8(t, uv);
	    } else {
		uvComp = composite_uv(uvS, uv);

		if (uvComp && ! isExclusion(uvComp) &&
			(ix ? (t == tmp_start) : (preCC <= curCC))) {
		    STRLEN leftcur, rightcur, dstcur;
		    leftcur  = UNISKIP(uvComp);
		    rightcur = UNISKIP(uvS) + UNISKIP(uv);

		    if (leftcur > rightcur) {
			dstcur = d - (U8*)SvPVX(dst);
			dstlen += leftcur - rightcur;
			d = (U8*)SvGROW(dst,dstlen) + dstcur;
		    }
		    /* preCC not changed to curCC */
		    uvS = uvComp;
		} else if (! curCC && p < e) { /* blocked */
		    break;
		} else {
		    preCC = curCC;
		    t = uvuni_to_utf8(t, uv);
		}
	    }
	}
	d = uvuni_to_utf8(d, uvS); /* starter (composed or not) */
	tmplen = t - tmp_start;
	if (tmplen) { /* uncomposed combining char */
	    t = (U8*)SvPVX(tmp);
	    while (tmplen--)
		*d++ = *t++;
	}
	uvS = uv;
    } /* for */
    *d = '\0';
    SvCUR_set(dst, d - (U8*)SvPVX(dst));
    RETVAL = dst;
#line 461 "Normalize.c"
	ST(0) = RETVAL;
	sv_2mortal(ST(0));
    }
    XSRETURN(1);
}

XS(XS_Unicode__Normalize_checkNFD); /* prototype to pass -Wmissing-prototypes */
XS(XS_Unicode__Normalize_checkNFD)
{
    dXSARGS;
    dXSI32;
    if (items != 1)
       Perl_croak(aTHX_ "Usage: %s(src)", GvNAME(CvGV(cv)));
    {
	SV *	src = ST(0);
#line 436 "Normalize.xs"
    STRLEN srclen, retlen;
    U8 *s, *e, *p, curCC, preCC;
    UV uv;
#line 481 "Normalize.c"
#line 440 "Normalize.xs"
    s = (U8*)sv_2pvunicode(src,&srclen);
    e = s + srclen;

    preCC = 0;
    for (p = s; p < e; p += retlen) {
	uv = utf8n_to_uvuni(p, e - p, &retlen, AllowAnyUTF);
	if (!retlen)
	    croak(ErrRetlenIsZero);

	curCC = getCombinClass(uv);
	if (preCC > curCC && curCC != 0) /* canonical ordering violated */
	    XSRETURN_NO;
	if (Hangul_IsS(uv) || (ix ? dec_compat(uv) : dec_canonical(uv)))
	    XSRETURN_NO;
	preCC = curCC;
    }
    XSRETURN_YES;
#line 500 "Normalize.c"
    }
    XSRETURN_EMPTY;
}

XS(XS_Unicode__Normalize_checkNFC); /* prototype to pass -Wmissing-prototypes */
XS(XS_Unicode__Normalize_checkNFC)
{
    dXSARGS;
    dXSI32;
    if (items != 1)
       Perl_croak(aTHX_ "Usage: %s(src)", GvNAME(CvGV(cv)));
    {
	SV *	src = ST(0);
#line 467 "Normalize.xs"
    STRLEN srclen, retlen;
    U8 *s, *e, *p, curCC, preCC;
    UV uv;
    bool isMAYBE;
#line 519 "Normalize.c"
#line 472 "Normalize.xs"
    s = (U8*)sv_2pvunicode(src,&srclen);
    e = s + srclen;

    preCC = 0;
    isMAYBE = FALSE;
    for (p = s; p < e; p += retlen) {
	uv = utf8n_to_uvuni(p, e - p, &retlen, AllowAnyUTF);
	if (!retlen)
	    croak(ErrRetlenIsZero);

	curCC = getCombinClass(uv);

	if (preCC > curCC && curCC != 0) /* canonical ordering violated */
	    XSRETURN_NO;

	/* get NFC/NFKC property */
	if (Hangul_IsS(uv)) /* Hangul syllables are canonical composites */
	    ; /* YES */
	else if (isExclusion(uv) || isSingleton(uv) || isNonStDecomp(uv))
	    XSRETURN_NO;
	else if (isComp2nd(uv))
	    isMAYBE = TRUE;
	else if (ix) {
	    char *canon, *compat;
	  /* NFKC_NO when having compatibility mapping. */
	    canon  = (char *) dec_canonical(uv);
	    compat = (char *) dec_compat(uv);
	    if (compat && !(canon && strEQ(canon, compat)))
		XSRETURN_NO;
	} /* end of get NFC/NFKC property */

	preCC = curCC;
    }
    if (isMAYBE)
	XSRETURN_UNDEF;
    else
	XSRETURN_YES;
#line 558 "Normalize.c"
    }
    XSRETURN_EMPTY;
}

XS(XS_Unicode__Normalize_checkFCD); /* prototype to pass -Wmissing-prototypes */
XS(XS_Unicode__Normalize_checkFCD)
{
    dXSARGS;
    dXSI32;
    if (items != 1)
       Perl_croak(aTHX_ "Usage: %s(src)", GvNAME(CvGV(cv)));
    {
	SV *	src = ST(0);
#line 519 "Normalize.xs"
    STRLEN srclen, retlen, canlen, canret;
    U8 *s, *e, *p, curCC, preCC;
    UV uv, uvLead, uvTrail;
    U8 *sCan, *pCan, *eCan;
    bool isMAYBE;
#line 578 "Normalize.c"
#line 525 "Normalize.xs"
    s = (U8*)sv_2pvunicode(src,&srclen);
    e = s + srclen;

    preCC = 0;
    isMAYBE = FALSE;
    for (p = s; p < e; p += retlen) {
	uv = utf8n_to_uvuni(p, e - p, &retlen, AllowAnyUTF);
	if (!retlen)
	    croak(ErrRetlenIsZero);

	sCan = (U8*) dec_canonical(uv);

	if (sCan) {
	    canlen = (STRLEN)strlen((char *) sCan);
	    uvLead = utf8n_to_uvuni(sCan, canlen, &canret, AllowAnyUTF);
	}
	else {
	    uvLead = uv;
	}

	curCC = getCombinClass(uvLead);

	if (curCC != 0 && curCC < preCC) /* canonical ordering violated */
	    XSRETURN_NO;

	if (ix) {
	    if (isExclusion(uv) || isSingleton(uv) || isNonStDecomp(uv))
		XSRETURN_NO;
	    else if (isComp2nd(uv))
		isMAYBE = TRUE;
	}

	if (sCan) {
	    eCan = sCan + canlen;
	    pCan = utf8_hop(eCan, -1);
	    if (pCan < sCan)
		croak(ErrHopBeforeStart);
	    uvTrail = utf8n_to_uvuni(pCan, eCan - pCan, &canret, AllowAnyUTF);
	    preCC = getCombinClass(uvTrail);
	}
	else {
	    preCC = curCC;
	}
    }
    if (isMAYBE)
	XSRETURN_UNDEF;
    else
	XSRETURN_YES;
#line 628 "Normalize.c"
    }
    XSRETURN_EMPTY;
}

XS(XS_Unicode__Normalize_getCombinClass); /* prototype to pass -Wmissing-prototypes */
XS(XS_Unicode__Normalize_getCombinClass)
{
    dXSARGS;
    if (items != 1)
	Perl_croak(aTHX_ "Usage: Unicode::Normalize::getCombinClass(uv)");
    {
	UV	uv = (UV)SvUV(ST(0));
	U8	RETVAL;
	dXSTARG;

	RETVAL = getCombinClass(uv);
	XSprePUSH; PUSHu((UV)RETVAL);
    }
    XSRETURN(1);
}

XS(XS_Unicode__Normalize_isExclusion); /* prototype to pass -Wmissing-prototypes */
XS(XS_Unicode__Normalize_isExclusion)
{
    dXSARGS;
    if (items != 1)
	Perl_croak(aTHX_ "Usage: Unicode::Normalize::isExclusion(uv)");
    {
	UV	uv = (UV)SvUV(ST(0));
	bool	RETVAL;

	RETVAL = isExclusion(uv);
	ST(0) = boolSV(RETVAL);
	sv_2mortal(ST(0));
    }
    XSRETURN(1);
}

XS(XS_Unicode__Normalize_isSingleton); /* prototype to pass -Wmissing-prototypes */
XS(XS_Unicode__Normalize_isSingleton)
{
    dXSARGS;
    if (items != 1)
	Perl_croak(aTHX_ "Usage: Unicode::Normalize::isSingleton(uv)");
    {
	UV	uv = (UV)SvUV(ST(0));
	bool	RETVAL;

	RETVAL = isSingleton(uv);
	ST(0) = boolSV(RETVAL);
	sv_2mortal(ST(0));
    }
    XSRETURN(1);
}

XS(XS_Unicode__Normalize_isNonStDecomp); /* prototype to pass -Wmissing-prototypes */
XS(XS_Unicode__Normalize_isNonStDecomp)
{
    dXSARGS;
    if (items != 1)
	Perl_croak(aTHX_ "Usage: Unicode::Normalize::isNonStDecomp(uv)");
    {
	UV	uv = (UV)SvUV(ST(0));
	bool	RETVAL;

	RETVAL = isNonStDecomp(uv);
	ST(0) = boolSV(RETVAL);
	sv_2mortal(ST(0));
    }
    XSRETURN(1);
}

XS(XS_Unicode__Normalize_isComp2nd); /* prototype to pass -Wmissing-prototypes */
XS(XS_Unicode__Normalize_isComp2nd)
{
    dXSARGS;
    dXSI32;
    if (items != 1)
       Perl_croak(aTHX_ "Usage: %s(uv)", GvNAME(CvGV(cv)));
    {
	UV	uv = (UV)SvUV(ST(0));
	bool	RETVAL;

	RETVAL = isComp2nd(uv);
	ST(0) = boolSV(RETVAL);
	sv_2mortal(ST(0));
    }
    XSRETURN(1);
}

XS(XS_Unicode__Normalize_isNFD_NO); /* prototype to pass -Wmissing-prototypes */
XS(XS_Unicode__Normalize_isNFD_NO)
{
    dXSARGS;
    dXSI32;
    if (items != 1)
       Perl_croak(aTHX_ "Usage: %s(uv)", GvNAME(CvGV(cv)));
    {
	UV	uv = (UV)SvUV(ST(0));
#line 613 "Normalize.xs"
    if (Hangul_IsS(uv) || (ix ? dec_compat(uv) : dec_canonical(uv)))
	XSRETURN_YES; /* NFD_NO or NFKD_NO */
    else
	XSRETURN_NO;
#line 733 "Normalize.c"
    }
    XSRETURN_EMPTY;
}

XS(XS_Unicode__Normalize_isComp_Ex); /* prototype to pass -Wmissing-prototypes */
XS(XS_Unicode__Normalize_isComp_Ex)
{
    dXSARGS;
    dXSI32;
    if (items != 1)
       Perl_croak(aTHX_ "Usage: %s(uv)", GvNAME(CvGV(cv)));
    {
	UV	uv = (UV)SvUV(ST(0));
#line 628 "Normalize.xs"
    if (isExclusion(uv) || isSingleton(uv) || isNonStDecomp(uv))
	XSRETURN_YES; /* NFC_NO or NFKC_NO */
    else if (ix) {
	char *canon, *compat;
	canon  = (char *) dec_canonical(uv);
	compat = (char *) dec_compat(uv);
	if (compat && (!canon || strNE(canon, compat)))
	    XSRETURN_YES; /* NFC_NO or NFKC_NO */
	else
	    XSRETURN_NO;
    }
    else
	XSRETURN_NO;
#line 761 "Normalize.c"
    }
    XSRETURN_EMPTY;
}

XS(XS_Unicode__Normalize_getComposite); /* prototype to pass -Wmissing-prototypes */
XS(XS_Unicode__Normalize_getComposite)
{
    dXSARGS;
    if (items != 2)
	Perl_croak(aTHX_ "Usage: Unicode::Normalize::getComposite(uv, uv2)");
    {
	UV	uv = (UV)SvUV(ST(0));
	UV	uv2 = (UV)SvUV(ST(1));
#line 650 "Normalize.xs"
    UV composite;
#line 777 "Normalize.c"
	SV *	RETVAL;
#line 652 "Normalize.xs"
    composite = composite_uv(uv, uv2);
    RETVAL = composite ? newSVuv(composite) : &PL_sv_undef;
#line 782 "Normalize.c"
	ST(0) = RETVAL;
	sv_2mortal(ST(0));
    }
    XSRETURN(1);
}

XS(XS_Unicode__Normalize_getCanon); /* prototype to pass -Wmissing-prototypes */
XS(XS_Unicode__Normalize_getCanon)
{
    dXSARGS;
    dXSI32;
    if (items != 1)
       Perl_croak(aTHX_ "Usage: %s(uv)", GvNAME(CvGV(cv)));
    {
	UV	uv = (UV)SvUV(ST(0));
#line 666 "Normalize.xs"
    U8 * rstr;
#line 800 "Normalize.c"
	SV *	RETVAL;
#line 668 "Normalize.xs"
    if (Hangul_IsS(uv)) {
	SV * dst;
	dst = newSV(1);
	(void)SvPOK_only(dst);
	sv_cat_decompHangul(dst, uv);
	RETVAL = dst;
    } else {
	rstr = ix ? dec_compat(uv) : dec_canonical(uv);
	if (!rstr)
	    XSRETURN_UNDEF;
	RETVAL = newSVpvn((char *)rstr, strlen((char *)rstr));
    }
    SvUTF8_on(RETVAL);
#line 816 "Normalize.c"
	ST(0) = RETVAL;
	sv_2mortal(ST(0));
    }
    XSRETURN(1);
}

XS(XS_Unicode__Normalize_splitOnLastStarter); /* prototype to pass -Wmissing-prototypes */
XS(XS_Unicode__Normalize_splitOnLastStarter)
{
    dXSARGS;
    if (items != 1)
	Perl_croak(aTHX_ "Usage: Unicode::Normalize::splitOnLastStarter(src)");
    SP -= items;
    {
	SV *	src = ST(0);
#line 689 "Normalize.xs"
    SV *svp;
    STRLEN srclen, retlen;
    U8 *s, *e, *p;
    UV uv;
#line 837 "Normalize.c"
#line 694 "Normalize.xs"
    s = (U8*)sv_2pvunicode(src,&srclen);
    e = s + srclen;

    for (p = e; s < p; ) {
	p = utf8_hop(p, -1);
	if (p < s)
	    croak(ErrHopBeforeStart);
	uv = utf8n_to_uvuni(p, e - p, &retlen, AllowAnyUTF);
	if (getCombinClass(uv) == 0) /* Last Starter found */
	    break;
    }

    svp = sv_2mortal(newSVpvn((char*)s, p - s));
    SvUTF8_on(svp);
    XPUSHs(svp);

    svp = sv_2mortal(newSVpvn((char*)p, e - p));
    SvUTF8_on(svp);
    XPUSHs(svp);
#line 858 "Normalize.c"
	PUTBACK;
	return;
    }
}

#ifdef __cplusplus
extern "C"
#endif
XS(boot_Unicode__Normalize); /* prototype to pass -Wmissing-prototypes */
XS(boot_Unicode__Normalize)
{
    dXSARGS;
    char* file = __FILE__;

    XS_VERSION_BOOTCHECK ;

    {
        CV * cv ;

        newXSproto("Unicode::Normalize::decompose", XS_Unicode__Normalize_decompose, file, "$;$");
        newXSproto("Unicode::Normalize::reorder", XS_Unicode__Normalize_reorder, file, "$");
        cv = newXS("Unicode::Normalize::composeContiguous", XS_Unicode__Normalize_compose, file);
        XSANY.any_i32 = 1 ;
        sv_setpv((SV*)cv, "$") ;
        cv = newXS("Unicode::Normalize::compose", XS_Unicode__Normalize_compose, file);
        XSANY.any_i32 = 0 ;
        sv_setpv((SV*)cv, "$") ;
        cv = newXS("Unicode::Normalize::checkNFD", XS_Unicode__Normalize_checkNFD, file);
        XSANY.any_i32 = 0 ;
        sv_setpv((SV*)cv, "$") ;
        cv = newXS("Unicode::Normalize::checkNFKD", XS_Unicode__Normalize_checkNFD, file);
        XSANY.any_i32 = 1 ;
        sv_setpv((SV*)cv, "$") ;
        cv = newXS("Unicode::Normalize::checkNFC", XS_Unicode__Normalize_checkNFC, file);
        XSANY.any_i32 = 0 ;
        sv_setpv((SV*)cv, "$") ;
        cv = newXS("Unicode::Normalize::checkNFKC", XS_Unicode__Normalize_checkNFC, file);
        XSANY.any_i32 = 1 ;
        sv_setpv((SV*)cv, "$") ;
        cv = newXS("Unicode::Normalize::checkFCD", XS_Unicode__Normalize_checkFCD, file);
        XSANY.any_i32 = 0 ;
        sv_setpv((SV*)cv, "$") ;
        cv = newXS("Unicode::Normalize::checkFCC", XS_Unicode__Normalize_checkFCD, file);
        XSANY.any_i32 = 1 ;
        sv_setpv((SV*)cv, "$") ;
        newXSproto("Unicode::Normalize::getCombinClass", XS_Unicode__Normalize_getCombinClass, file, "$");
        newXSproto("Unicode::Normalize::isExclusion", XS_Unicode__Normalize_isExclusion, file, "$");
        newXSproto("Unicode::Normalize::isSingleton", XS_Unicode__Normalize_isSingleton, file, "$");
        newXSproto("Unicode::Normalize::isNonStDecomp", XS_Unicode__Normalize_isNonStDecomp, file, "$");
        cv = newXS("Unicode::Normalize::isNFKC_MAYBE", XS_Unicode__Normalize_isComp2nd, file);
        XSANY.any_i32 = 2 ;
        sv_setpv((SV*)cv, "$") ;
        cv = newXS("Unicode::Normalize::isComp2nd", XS_Unicode__Normalize_isComp2nd, file);
        XSANY.any_i32 = 0 ;
        sv_setpv((SV*)cv, "$") ;
        cv = newXS("Unicode::Normalize::isNFC_MAYBE", XS_Unicode__Normalize_isComp2nd, file);
        XSANY.any_i32 = 1 ;
        sv_setpv((SV*)cv, "$") ;
        cv = newXS("Unicode::Normalize::isNFKD_NO", XS_Unicode__Normalize_isNFD_NO, file);
        XSANY.any_i32 = 1 ;
        sv_setpv((SV*)cv, "$") ;
        cv = newXS("Unicode::Normalize::isNFD_NO", XS_Unicode__Normalize_isNFD_NO, file);
        XSANY.any_i32 = 0 ;
        sv_setpv((SV*)cv, "$") ;
        cv = newXS("Unicode::Normalize::isNFKC_NO", XS_Unicode__Normalize_isComp_Ex, file);
        XSANY.any_i32 = 1 ;
        sv_setpv((SV*)cv, "$") ;
        cv = newXS("Unicode::Normalize::isComp_Ex", XS_Unicode__Normalize_isComp_Ex, file);
        XSANY.any_i32 = 0 ;
        sv_setpv((SV*)cv, "$") ;
        cv = newXS("Unicode::Normalize::isNFC_NO", XS_Unicode__Normalize_isComp_Ex, file);
        XSANY.any_i32 = 0 ;
        sv_setpv((SV*)cv, "$") ;
        newXSproto("Unicode::Normalize::getComposite", XS_Unicode__Normalize_getComposite, file, "$$");
        cv = newXS("Unicode::Normalize::getCanon", XS_Unicode__Normalize_getCanon, file);
        XSANY.any_i32 = 0 ;
        sv_setpv((SV*)cv, "$") ;
        cv = newXS("Unicode::Normalize::getCompat", XS_Unicode__Normalize_getCanon, file);
        XSANY.any_i32 = 1 ;
        sv_setpv((SV*)cv, "$") ;
        newXS("Unicode::Normalize::splitOnLastStarter", XS_Unicode__Normalize_splitOnLastStarter, file);
    }
    XSRETURN_YES;
}

