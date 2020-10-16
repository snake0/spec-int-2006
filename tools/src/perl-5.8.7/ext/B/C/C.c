/*
 * This file was generated automatically by xsubpp version 1.9508 from the
 * contents of C.xs. Do not edit this file, edit C.xs instead.
 *
 *	ANY CHANGES MADE HERE WILL BE LOST!
 *
 */

#line 1 "C.xs"
#include <EXTERN.h>
#include <perl.h>
#include <XSUB.h>

int
my_runops(pTHX)
{
    HV* regexp_hv = get_hv( "B::C::REGEXP", 0 );
    SV* key = newSViv( 0 );

    do {
	PERL_ASYNC_CHECK();

        if( PL_op->op_type == OP_QR ) {
            PMOP* op;
            REGEXP* rx = PM_GETRE( (PMOP*)PL_op );
            SV* rv = newSViv( 0 );

            New( 671, op, 1, PMOP );
            Copy( PL_op, op, 1, PMOP );
            /* we need just the flags */
            op->op_next = NULL;
            op->op_sibling = NULL;
            op->op_first = NULL;
            op->op_last = NULL;
            op->op_pmreplroot = NULL;
            op->op_pmreplstart = NULL;
            op->op_pmnext = NULL;
#ifdef USE_ITHREADS
            op->op_pmoffset = 0;
#else
            op->op_pmregexp = 0;
#endif

            sv_setiv( key, PTR2IV( rx ) );
            sv_setref_iv( rv, "B::PMOP", PTR2IV( op ) );

            hv_store_ent( regexp_hv, key, rv, 0 );
        }
    } while ((PL_op = CALL_FPTR(PL_op->op_ppaddr)(aTHX)));

    SvREFCNT_dec( key );

    TAINT_NOT;
    return 0;
}

#line 58 "C.c"
#ifdef __cplusplus
extern "C"
#endif
XS(boot_B__C); /* prototype to pass -Wmissing-prototypes */
XS(boot_B__C)
{
    dXSARGS;

    XS_VERSION_BOOTCHECK ;


    /* Initialisation Section */

#line 53 "C.xs"
    PL_runops = my_runops;

#line 75 "C.c"

    /* End of Initialisation Section */

    XSRETURN_YES;
}

