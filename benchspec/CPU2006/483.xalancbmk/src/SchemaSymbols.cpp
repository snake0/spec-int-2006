/*
 * The Apache Software License, Version 1.1
 *
 * Copyright (c) 2001 The Apache Software Foundation.  All rights
 * reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in
 *    the documentation and/or other materials provided with the
 *    distribution.
 *
 * 3. The end-user documentation included with the redistribution,
 *    if any, must include the following acknowledgment:
 *       "This product includes software developed by the
 *        Apache Software Foundation (http://www.apache.org/)."
 *    Alternately, this acknowledgment may appear in the software itself,
 *    if and wherever such third-party acknowledgments normally appear.
 *
 * 4. The names "Xerces" and "Apache Software Foundation" must
 *    not be used to endorse or promote products derived from this
 *    software without prior written permission. For written
 *    permission, please contact apache\@apache.org.
 *
 * 5. Products derived from this software may not be called "Apache",
 *    nor may "Apache" appear in their name, without prior written
 *    permission of the Apache Software Foundation.
 *
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND ANY EXPRESSED OR IMPLIED
 * WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED.  IN NO EVENT SHALL THE APACHE SOFTWARE FOUNDATION OR
 * ITS CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
 * USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 * OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
 * OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 * ====================================================================
 *
 * This software consists of voluntary contributions made by many
 * individuals on behalf of the Apache Software Foundation, and was
 * originally based on software copyright (c) 2001, International
 * Business Machines, Inc., http://www.ibm.com .  For more information
 * on the Apache Software Foundation, please see
 * <http://www.apache.org/>.
 */

/*
 * $Log: SchemaSymbols.cpp,v $
 * Revision 1.3  2002/11/04 14:49:42  tng
 * C++ Namespace Support.
 *
 * Revision 1.2  2002/05/27 19:39:25  knoaman
 * remove unused constants.
 *
 * Revision 1.1.1.1  2002/02/01 22:22:46  peiyongz
 * sane_include
 *
 * Revision 1.14  2001/11/07 19:20:01  peiyongz
 * DateTime Port
 *
 * Revision 1.13  2001/09/18 14:41:56  knoaman
 * Add support for <annotation>.
 *
 * Revision 1.12  2001/08/24 20:36:37  knoaman
 * Add support for <redefine>.
 *
 * Revision 1.11  2001/08/24 17:12:02  knoaman
 * Add support for anySimpleType.
 * Remove parameter 'baseValidator' from the virtual method 'newInstance'.
 *
 * Revision 1.10  2001/08/22 16:57:53  tng
 * typo in ##other.
 *
 * Revision 1.9  2001/08/01 18:49:32  peiyongz
 * AnyRUIDatatypeValidator
 *
 * Revision 1.8  2001/05/16 15:24:51  tng
 * Schema: Add Base64 and HexBin.  By Pei Yong Zhang.
 *
 * Revision 1.7  2001/05/15 21:59:34  knoaman
 * TraverseSchema: add attribute checking + some fixes + more error messages.
 * More attribute cheking to come.
 *
 * Revision 1.6  2001/05/14 17:53:48  tng
 * Schema: Update Schema URL
 *
 * Revision 1.5  2001/05/11 15:17:43  tng
 * Schema: Nillable fixes.
 *
 * Revision 1.4  2001/05/11 13:27:37  tng
 * Copyright update.
 *
 * Revision 1.3  2001/05/09 18:43:49  tng
 * Add StringDatatypeValidator and BooleanDatatypeValidator.  By Pei Yong Zhang.
 *
 * Revision 1.2  2001/05/03 19:18:03  knoaman
 * TraverseSchema Part II.
 *
 * Revision 1.1  2001/03/21 21:39:21  knoaman
 * Schema symbols and Datatype validator part I
 *
 */


// ---------------------------------------------------------------------------
//  Includes
// ---------------------------------------------------------------------------
#include <xercesc/util/XMLUniDefs.hpp>
#include <xercesc/validators/schema/SchemaSymbols.hpp>

XERCES_CPP_NAMESPACE_BEGIN

// ---------------------------------------------------------------------------
//  SchemaSymbols: Static data
// ---------------------------------------------------------------------------
const XMLCh SchemaSymbols::fgURI_XSI[] =
{
    chLatin_h, chLatin_t, chLatin_t, chLatin_p, chColon, chForwardSlash,
    chForwardSlash, chLatin_w, chLatin_w, chLatin_w, chPeriod, chLatin_w,
    chDigit_3, chPeriod, chLatin_o, chLatin_r, chLatin_g, chForwardSlash,
    chDigit_2, chDigit_0, chDigit_0, chDigit_1, chForwardSlash,
    chLatin_X, chLatin_M, chLatin_L, chLatin_S,
    chLatin_c, chLatin_h, chLatin_e, chLatin_m, chLatin_a, chDash, chLatin_i,
    chLatin_n, chLatin_s, chLatin_t, chLatin_a, chLatin_n, chLatin_c,
    chLatin_e, chNull
};

const XMLCh SchemaSymbols::fgURI_SCHEMAFORSCHEMA[] =
{
    chLatin_h, chLatin_t, chLatin_t, chLatin_p, chColon, chForwardSlash,
    chForwardSlash, chLatin_w, chLatin_w, chLatin_w, chPeriod, chLatin_w,
    chDigit_3, chPeriod, chLatin_o, chLatin_r, chLatin_g, chForwardSlash,
    chDigit_2, chDigit_0, chDigit_0, chDigit_1, chForwardSlash,
    chLatin_X, chLatin_M, chLatin_L, chLatin_S,
    chLatin_c, chLatin_h, chLatin_e, chLatin_m, chLatin_a, chNull
};

const XMLCh SchemaSymbols::fgXSI_SCHEMALOCACTION[] =
{
    chLatin_s, chLatin_c, chLatin_h, chLatin_e, chLatin_m, chLatin_a,
    chLatin_L, chLatin_o, chLatin_c, chLatin_a, chLatin_t, chLatin_i,
    chLatin_o, chLatin_n, chNull
};

const XMLCh SchemaSymbols::fgXSI_NONAMESPACESCHEMALOCACTION[] =
{
    chLatin_n, chLatin_o, chLatin_N, chLatin_a, chLatin_m, chLatin_e,
    chLatin_s, chLatin_p, chLatin_a, chLatin_c, chLatin_e, chLatin_S,
    chLatin_c, chLatin_h, chLatin_e, chLatin_m, chLatin_a, chLatin_L,
    chLatin_o, chLatin_c, chLatin_a, chLatin_t, chLatin_i, chLatin_o,
    chLatin_n, chNull
};

const XMLCh SchemaSymbols::fgXSI_TYPE[] =
{
    chLatin_t, chLatin_y, chLatin_p, chLatin_e, chNull
};

const XMLCh SchemaSymbols::fgELT_ALL[] =
{
    chLatin_a, chLatin_l, chLatin_l, chNull
};

const XMLCh SchemaSymbols::fgELT_ANNOTATION[] =
{
    chLatin_a, chLatin_n, chLatin_n, chLatin_o, chLatin_t, chLatin_a,
    chLatin_t, chLatin_i, chLatin_o, chLatin_n, chNull
};

const XMLCh SchemaSymbols::fgELT_ANY[] =
{
    chLatin_a, chLatin_n, chLatin_y, chNull
};

const XMLCh SchemaSymbols::fgELT_WILDCARD[] =
{
    chLatin_a, chLatin_n, chLatin_y, chNull
};

const XMLCh SchemaSymbols::fgELT_ANYATTRIBUTE[] =
{
    chLatin_a, chLatin_n, chLatin_y, chLatin_A, chLatin_t, chLatin_t,
    chLatin_r, chLatin_i, chLatin_b, chLatin_u, chLatin_t, chLatin_e, chNull
};

const XMLCh SchemaSymbols::fgELT_APPINFO[] =
{
    chLatin_a, chLatin_p, chLatin_p, chLatin_i, chLatin_n, chLatin_f, chLatin_o, chNull
};

const XMLCh SchemaSymbols::fgELT_ATTRIBUTE[] =
{
    chLatin_a, chLatin_t, chLatin_t, chLatin_r, chLatin_i, chLatin_b,
    chLatin_u, chLatin_t, chLatin_e, chNull
};

const XMLCh SchemaSymbols::fgELT_ATTRIBUTEGROUP[] =
{
    chLatin_a, chLatin_t, chLatin_t, chLatin_r, chLatin_i, chLatin_b,
    chLatin_u, chLatin_t, chLatin_e, chLatin_G, chLatin_r, chLatin_o,
    chLatin_u, chLatin_p, chNull
};

const XMLCh SchemaSymbols::fgELT_CHOICE[] =
{
    chLatin_c, chLatin_h, chLatin_o, chLatin_i, chLatin_c, chLatin_e, chNull
};

const XMLCh SchemaSymbols::fgELT_COMPLEXTYPE[] =
{
    chLatin_c, chLatin_o, chLatin_m, chLatin_p, chLatin_l, chLatin_e,
    chLatin_x, chLatin_T, chLatin_y, chLatin_p, chLatin_e, chNull
};

const XMLCh SchemaSymbols::fgELT_CONTENT[] =
{
    chLatin_c, chLatin_o, chLatin_n, chLatin_t, chLatin_e, chLatin_n,
    chLatin_t, chNull
};

const XMLCh SchemaSymbols::fgELT_DOCUMENTATION[] =
{
    chLatin_d, chLatin_o, chLatin_c, chLatin_u, chLatin_m, chLatin_e, chLatin_n,
    chLatin_t, chLatin_a, chLatin_t, chLatin_i, chLatin_o, chLatin_n, chNull
};

const XMLCh SchemaSymbols::fgELT_DURATION[] =
{
    chLatin_d, chLatin_u, chLatin_r, chLatin_a, chLatin_t, chLatin_i,
    chLatin_o, chLatin_n, chNull
};

const XMLCh SchemaSymbols::fgELT_ELEMENT[] =
{
    chLatin_e, chLatin_l, chLatin_e, chLatin_m, chLatin_e, chLatin_n,
	chLatin_t, chNull
};

const XMLCh SchemaSymbols::fgELT_ENCODING[] =
{
    chLatin_e, chLatin_n, chLatin_c, chLatin_o, chLatin_d, chLatin_i,
    chLatin_n, chLatin_g, chNull
};

const XMLCh SchemaSymbols::fgELT_ENUMERATION[] =
{
    chLatin_e, chLatin_n, chLatin_u, chLatin_m, chLatin_e, chLatin_r,
    chLatin_a, chLatin_t, chLatin_i, chLatin_o, chLatin_n, chNull
};

const XMLCh SchemaSymbols::fgELT_FIELD[] =
{
    chLatin_f, chLatin_i, chLatin_e, chLatin_l, chLatin_d, chNull
};


const XMLCh SchemaSymbols::fgELT_WHITESPACE[] =
{
    chLatin_w, chLatin_h, chLatin_i, chLatin_t, chLatin_e, chLatin_S,
    chLatin_p, chLatin_a, chLatin_c, chLatin_e, chNull
};

const XMLCh SchemaSymbols::fgELT_GROUP[] =
{
    chLatin_g, chLatin_r, chLatin_o, chLatin_u, chLatin_p,  chNull
};

const XMLCh SchemaSymbols::fgELT_IMPORT[] =
{
    chLatin_i, chLatin_m, chLatin_p, chLatin_o, chLatin_r, chLatin_t, chNull
};

const XMLCh SchemaSymbols::fgELT_INCLUDE[] =
{
    chLatin_i, chLatin_n, chLatin_c, chLatin_l, chLatin_u, chLatin_d,
    chLatin_e, chNull
};

const XMLCh SchemaSymbols::fgELT_REDEFINE[] =
{
    chLatin_r, chLatin_e, chLatin_d, chLatin_e, chLatin_f, chLatin_i,
    chLatin_n, chLatin_e, chNull
};

const XMLCh SchemaSymbols::fgELT_KEY[] =
{
    chLatin_k, chLatin_e, chLatin_y,  chNull
};

const XMLCh SchemaSymbols::fgELT_KEYREF[] =
{
    chLatin_k, chLatin_e, chLatin_y, chLatin_r, chLatin_e, chLatin_f, chNull
};

const XMLCh SchemaSymbols::fgELT_LENGTH[] =
{
    chLatin_l, chLatin_e, chLatin_n, chLatin_g, chLatin_t, chLatin_h, chNull
};

const XMLCh SchemaSymbols::fgELT_MAXEXCLUSIVE[] =
{
    chLatin_m, chLatin_a, chLatin_x, chLatin_E, chLatin_x, chLatin_c,
    chLatin_l, chLatin_u, chLatin_s, chLatin_i, chLatin_v, chLatin_e, chNull
};

const XMLCh SchemaSymbols::fgELT_MAXINCLUSIVE[] =
{
    chLatin_m, chLatin_a, chLatin_x, chLatin_I, chLatin_n, chLatin_c,
    chLatin_l, chLatin_u, chLatin_s, chLatin_i, chLatin_v, chLatin_e, chNull
};

const XMLCh SchemaSymbols::fgELT_MAXLENGTH[] =
{
    chLatin_m, chLatin_a, chLatin_x, chLatin_L, chLatin_e, chLatin_n,
    chLatin_g, chLatin_t, chLatin_h, chNull
};

const XMLCh SchemaSymbols::fgELT_MINEXCLUSIVE[] =
{
    chLatin_m, chLatin_i, chLatin_n, chLatin_E, chLatin_x, chLatin_c,
    chLatin_l, chLatin_u, chLatin_s, chLatin_i, chLatin_v, chLatin_e, chNull
};

const XMLCh SchemaSymbols::fgELT_MININCLUSIVE[] =
{
    chLatin_m, chLatin_i, chLatin_n, chLatin_I, chLatin_n, chLatin_c,
    chLatin_l, chLatin_u, chLatin_s, chLatin_i, chLatin_v, chLatin_e, chNull
};

const XMLCh SchemaSymbols::fgELT_MINLENGTH[] =
{
    chLatin_m, chLatin_i, chLatin_n, chLatin_L, chLatin_e, chLatin_n,
    chLatin_g, chLatin_t, chLatin_h, chNull
};

const XMLCh SchemaSymbols::fgELT_NOTATION[] =
{
    chLatin_n, chLatin_o, chLatin_t, chLatin_a, chLatin_t, chLatin_i,
    chLatin_o, chLatin_n,  chNull
};

const XMLCh SchemaSymbols::fgELT_PATTERN[] =
{
    chLatin_p, chLatin_a, chLatin_t, chLatin_t, chLatin_e, chLatin_r,
    chLatin_n,  chNull
};

const XMLCh SchemaSymbols::fgELT_PERIOD[] =
{
    chLatin_p, chLatin_e, chLatin_r, chLatin_i, chLatin_o, chLatin_d, chNull
};

const XMLCh SchemaSymbols::fgELT_TOTALDIGITS[] =
{
    chLatin_t, chLatin_o, chLatin_t, chLatin_a, chLatin_l, chLatin_D,
    chLatin_i, chLatin_g, chLatin_i, chLatin_t, chLatin_s, chNull
};

const XMLCh SchemaSymbols::fgELT_FRACTIONDIGITS[] =
{
    chLatin_f, chLatin_r, chLatin_a, chLatin_c, chLatin_t, chLatin_i, chLatin_o,
    chLatin_n, chLatin_D, chLatin_i, chLatin_g, chLatin_i, chLatin_t, chLatin_s, chNull
};

const XMLCh SchemaSymbols::fgELT_SCHEMA[] =
{
    chLatin_s, chLatin_c, chLatin_h, chLatin_e, chLatin_m, chLatin_a, chNull
};

const XMLCh SchemaSymbols::fgELT_SELECTOR[] =
{
    chLatin_s, chLatin_e, chLatin_l, chLatin_e, chLatin_c, chLatin_t,
    chLatin_o, chLatin_r, chNull
};

const XMLCh SchemaSymbols::fgELT_SEQUENCE[] =
{
    chLatin_s, chLatin_e, chLatin_q, chLatin_u, chLatin_e, chLatin_n,
    chLatin_c, chLatin_e, chNull
};

const XMLCh SchemaSymbols::fgELT_SIMPLETYPE[] =
{
    chLatin_s, chLatin_i, chLatin_m, chLatin_p, chLatin_l, chLatin_e,
    chLatin_T, chLatin_y, chLatin_p, chLatin_e, chNull
};

const XMLCh SchemaSymbols::fgELT_UNION[] =
{
    chLatin_u, chLatin_n, chLatin_i, chLatin_o, chLatin_n,  chNull
};

const XMLCh SchemaSymbols::fgELT_LIST[] =
{
    chLatin_l, chLatin_i, chLatin_s, chLatin_t,  chNull
};

const XMLCh SchemaSymbols::fgELT_UNIQUE[] =
{
    chLatin_u, chLatin_n, chLatin_i, chLatin_q, chLatin_u, chLatin_e, chNull
};

const XMLCh SchemaSymbols::fgELT_COMPLEXCONTENT[] =
{
    chLatin_c, chLatin_o, chLatin_m, chLatin_p, chLatin_l, chLatin_e,
    chLatin_x, chLatin_C, chLatin_o, chLatin_n, chLatin_t, chLatin_e,
    chLatin_n, chLatin_t, chNull
};

const XMLCh SchemaSymbols::fgELT_SIMPLECONTENT[] =
{
    chLatin_s, chLatin_i, chLatin_m, chLatin_p, chLatin_l, chLatin_e, chLatin_C,
    chLatin_o, chLatin_n, chLatin_t, chLatin_e, chLatin_n, chLatin_t, chNull
};

const XMLCh SchemaSymbols::fgELT_RESTRICTION[] =
{
    chLatin_r, chLatin_e, chLatin_s, chLatin_t, chLatin_r, chLatin_i,
    chLatin_c, chLatin_t, chLatin_i, chLatin_o, chLatin_n, chNull
};

const XMLCh SchemaSymbols::fgELT_EXTENSION[] =
{
    chLatin_e, chLatin_x, chLatin_t, chLatin_e, chLatin_n, chLatin_s,
    chLatin_i, chLatin_o, chLatin_n, chNull
};

const XMLCh SchemaSymbols::fgATT_ABSTRACT[] =
{
    chLatin_a, chLatin_b, chLatin_s, chLatin_t, chLatin_r, chLatin_a,
    chLatin_c, chLatin_t, chNull
};

const XMLCh SchemaSymbols::fgATT_ATTRIBUTEFORMDEFAULT[] =
{
    chLatin_a, chLatin_t, chLatin_t, chLatin_r, chLatin_i, chLatin_b,
    chLatin_u, chLatin_t, chLatin_e, chLatin_F, chLatin_o, chLatin_r,
    chLatin_m, chLatin_D, chLatin_e, chLatin_f, chLatin_a, chLatin_u,
    chLatin_l, chLatin_t, chNull
};

const XMLCh SchemaSymbols::fgATT_BASE[] =
{
    chLatin_b, chLatin_a, chLatin_s, chLatin_e,  chNull
};

const XMLCh SchemaSymbols::fgATT_ITEMTYPE[] =
{
    chLatin_i, chLatin_t, chLatin_e, chLatin_m, chLatin_T, chLatin_y,
    chLatin_p, chLatin_e,  chNull
};

const XMLCh SchemaSymbols::fgATT_MEMBERTYPES[] =
{
    chLatin_m, chLatin_e, chLatin_m, chLatin_b, chLatin_e, chLatin_r,
    chLatin_T, chLatin_y, chLatin_p, chLatin_e, chLatin_s, chNull
};

const XMLCh SchemaSymbols::fgATT_BLOCK[] =
{
    chLatin_b, chLatin_l, chLatin_o, chLatin_c, chLatin_k,  chNull
};

const XMLCh SchemaSymbols::fgATT_BLOCKDEFAULT[] =
{
    chLatin_b, chLatin_l, chLatin_o, chLatin_c, chLatin_k, chLatin_D,
    chLatin_e, chLatin_f, chLatin_a, chLatin_u, chLatin_l, chLatin_t, chNull
};

const XMLCh SchemaSymbols::fgATT_DEFAULT[] =
{
    chLatin_d, chLatin_e, chLatin_f, chLatin_a, chLatin_u, chLatin_l, chLatin_t, chNull
};

const XMLCh SchemaSymbols::fgATT_ELEMENTFORMDEFAULT[] =
{
    chLatin_e, chLatin_l, chLatin_e, chLatin_m, chLatin_e, chLatin_n,
    chLatin_t, chLatin_F, chLatin_o, chLatin_r, chLatin_m, chLatin_D,
    chLatin_e, chLatin_f, chLatin_a, chLatin_u, chLatin_l, chLatin_t, chNull
};

const XMLCh SchemaSymbols::fgATT_SUBSTITUTIONGROUP[] =
{
    chLatin_s, chLatin_u, chLatin_b, chLatin_s, chLatin_t, chLatin_i,
    chLatin_t, chLatin_u, chLatin_t, chLatin_i, chLatin_o, chLatin_n,
    chLatin_G, chLatin_r, chLatin_o, chLatin_u, chLatin_p, chNull
};

const XMLCh SchemaSymbols::fgATT_FINAL[] =
{
    chLatin_f, chLatin_i, chLatin_n, chLatin_a, chLatin_l, chNull
};

const XMLCh SchemaSymbols::fgATT_FINALDEFAULT[] =
{
    chLatin_f, chLatin_i, chLatin_n, chLatin_a, chLatin_l, chLatin_D,
    chLatin_e, chLatin_f, chLatin_a, chLatin_u, chLatin_l, chLatin_t, chNull
};

const XMLCh SchemaSymbols::fgATT_FIXED[] =
{
    chLatin_f, chLatin_i, chLatin_x, chLatin_e, chLatin_d,  chNull
};

const XMLCh SchemaSymbols::fgATT_FORM[] =
{
    chLatin_f, chLatin_o, chLatin_r, chLatin_m, chNull
};

const XMLCh SchemaSymbols::fgATT_ID[] =
{
    chLatin_i, chLatin_d, chNull
};

const XMLCh SchemaSymbols::fgATT_MAXOCCURS[] =
{
    chLatin_m, chLatin_a, chLatin_x, chLatin_O, chLatin_c, chLatin_c,
    chLatin_u, chLatin_r, chLatin_s, chNull
};

const XMLCh SchemaSymbols::fgATT_MINOCCURS[] =
{
    chLatin_m, chLatin_i, chLatin_n, chLatin_O, chLatin_c, chLatin_c,
    chLatin_u, chLatin_r, chLatin_s, chNull
};

const XMLCh SchemaSymbols::fgATT_NAME[] =
{
    chLatin_n, chLatin_a, chLatin_m, chLatin_e, chNull
};

const XMLCh SchemaSymbols::fgATT_NAMESPACE[] =
{
    chLatin_n, chLatin_a, chLatin_m, chLatin_e, chLatin_s, chLatin_p,
    chLatin_a, chLatin_c, chLatin_e, chNull
};

const XMLCh SchemaSymbols::fgATT_NILL[] =
{
    chLatin_n, chLatin_i, chLatin_l, chNull
};

const XMLCh SchemaSymbols::fgATT_NILLABLE[] =
{
    chLatin_n, chLatin_i, chLatin_l, chLatin_l, chLatin_a, chLatin_b,
    chLatin_l, chLatin_e, chNull
};

const XMLCh SchemaSymbols::fgATT_PROCESSCONTENTS[] =
{
    chLatin_p, chLatin_r, chLatin_o, chLatin_c, chLatin_e, chLatin_s,
    chLatin_s, chLatin_C, chLatin_o, chLatin_n, chLatin_t, chLatin_e,
    chLatin_n, chLatin_t, chLatin_s, chNull
};

const XMLCh SchemaSymbols::fgATT_REF[] =
{
    chLatin_r, chLatin_e, chLatin_f, chNull
};

const XMLCh SchemaSymbols::fgATT_REFER[] =
{
    chLatin_r, chLatin_e, chLatin_f, chLatin_e, chLatin_r, chNull
};

const XMLCh SchemaSymbols::fgATT_SCHEMALOCATION[] =
{
    chLatin_s, chLatin_c, chLatin_h, chLatin_e, chLatin_m, chLatin_a,
    chLatin_L, chLatin_o, chLatin_c, chLatin_a, chLatin_t, chLatin_i,
    chLatin_o, chLatin_n, chNull
};

const XMLCh SchemaSymbols::fgATT_SOURCE[] =
{
    chLatin_s, chLatin_o, chLatin_u, chLatin_r, chLatin_c, chLatin_e, chNull
};

const XMLCh SchemaSymbols::fgATT_SYSTEM[] =
{
    chLatin_s, chLatin_y, chLatin_s, chLatin_t, chLatin_e, chLatin_m, chNull
};

const XMLCh SchemaSymbols::fgATT_PUBLIC[] =
{
    chLatin_p, chLatin_u, chLatin_b, chLatin_l, chLatin_i, chLatin_c, chNull
};

const XMLCh SchemaSymbols::fgATT_TARGETNAMESPACE[] =
{
    chLatin_t, chLatin_a, chLatin_r, chLatin_g, chLatin_e, chLatin_t,
    chLatin_N, chLatin_a, chLatin_m, chLatin_e, chLatin_s, chLatin_p,
    chLatin_a, chLatin_c, chLatin_e, chNull
};

const XMLCh SchemaSymbols::fgATT_TYPE[] =
{
    chLatin_t, chLatin_y, chLatin_p, chLatin_e, chNull
};

const XMLCh SchemaSymbols::fgATT_USE[] =
{
    chLatin_u, chLatin_s, chLatin_e, chNull
};

const XMLCh SchemaSymbols::fgATT_VALUE[] =
{
    chLatin_v, chLatin_a, chLatin_l, chLatin_u, chLatin_e, chNull
};

const XMLCh SchemaSymbols::fgATT_MIXED[] =
{
    chLatin_m, chLatin_i, chLatin_x, chLatin_e, chLatin_d, chNull
};

const XMLCh SchemaSymbols::fgATT_VERSION[] =
{
    chLatin_v, chLatin_e, chLatin_r, chLatin_s, chLatin_i,
    chLatin_o, chLatin_n, chNull
};

const XMLCh SchemaSymbols::fgATT_XPATH[] =
{
    chLatin_x, chLatin_p, chLatin_a, chLatin_t, chLatin_h, chNull
};

const XMLCh SchemaSymbols::fgATTVAL_TWOPOUNDANY[] =
{
    chPound, chPound, chLatin_a, chLatin_n, chLatin_y, chNull
};

const XMLCh SchemaSymbols::fgATTVAL_TWOPOUNDLOCAL[] =
{
    chPound, chPound, chLatin_l, chLatin_o, chLatin_c, chLatin_a, chLatin_l, chNull
};

const XMLCh SchemaSymbols::fgATTVAL_TWOPOUNDOTHER[] =
{
    chPound, chPound, chLatin_o, chLatin_t, chLatin_h, chLatin_e, chLatin_r, chNull
};

const XMLCh SchemaSymbols::fgATTVAL_TWOPOUNDTRAGETNAMESPACE[] =
{
    chPound, chPound, chLatin_t, chLatin_a, chLatin_r, chLatin_g, chLatin_e,
    chLatin_t, chLatin_N, chLatin_a, chLatin_m, chLatin_e, chLatin_s, chLatin_p,
    chLatin_a, chLatin_c, chLatin_e, chNull
};

const XMLCh SchemaSymbols::fgATTVAL_POUNDALL[] =
{
    chPound, chLatin_a, chLatin_l, chLatin_l, chNull
};

const XMLCh SchemaSymbols::fgATTVAL_BASE64[] =
{
    chLatin_b, chLatin_a, chLatin_s, chLatin_e, chDigit_6, chDigit_4, chNull
};

const XMLCh SchemaSymbols::fgATTVAL_BOOLEAN[] =
{
    chLatin_b, chLatin_o, chLatin_o, chLatin_l, chLatin_e, chLatin_a, chLatin_n, chNull
};

const XMLCh SchemaSymbols::fgATTVAL_DEFAULT[] =
{
    chLatin_d, chLatin_e, chLatin_f, chLatin_a, chLatin_u, chLatin_l, chLatin_t, chNull
};

const XMLCh SchemaSymbols::fgATTVAL_ELEMENTONLY[] =
{
    chLatin_e, chLatin_l, chLatin_e, chLatin_m, chLatin_e, chLatin_n,
    chLatin_t, chLatin_O, chLatin_n, chLatin_l, chLatin_y, chNull
};

const XMLCh SchemaSymbols::fgATTVAL_EMPTY[] =
{
    chLatin_e, chLatin_m, chLatin_p, chLatin_t, chLatin_y, chNull
};

const XMLCh SchemaSymbols::fgATTVAL_EXTENSION[] =
{
    chLatin_e, chLatin_x, chLatin_t, chLatin_e, chLatin_n, chLatin_s,
    chLatin_i, chLatin_o, chLatin_n, chNull
};

const XMLCh SchemaSymbols::fgATTVAL_FALSE[] =
{
    chLatin_f, chLatin_a, chLatin_l, chLatin_s, chLatin_e, chNull
};

const XMLCh SchemaSymbols::fgATTVAL_FIXED[] =
{
    chLatin_f, chLatin_i, chLatin_x, chLatin_e, chLatin_d, chNull
};

const XMLCh SchemaSymbols::fgATTVAL_HEX[] =
{
    chLatin_h, chLatin_e, chLatin_x, chNull
};

const XMLCh SchemaSymbols::fgATTVAL_ID[] =
{
    chLatin_I, chLatin_D, chNull
};

const XMLCh SchemaSymbols::fgATTVAL_LAX[] =
{
    chLatin_l, chLatin_a, chLatin_x, chNull
};

const XMLCh SchemaSymbols::fgATTVAL_MAXLENGTH[] =
{
    chLatin_m, chLatin_a, chLatin_x, chLatin_L, chLatin_e, chLatin_n,
    chLatin_g, chLatin_t, chLatin_h, chNull
};

const XMLCh SchemaSymbols::fgATTVAL_MINLENGTH[] =
{
    chLatin_m, chLatin_i, chLatin_n, chLatin_L, chLatin_e, chLatin_n,
    chLatin_g, chLatin_t, chLatin_h, chNull
};


const XMLCh SchemaSymbols::fgATTVAL_MIXED[] =
{
    chLatin_m, chLatin_i, chLatin_x, chLatin_e, chLatin_d, chNull
};


const XMLCh SchemaSymbols::fgATTVAL_NCNAME[] =
{
    chLatin_N, chLatin_C, chLatin_N, chLatin_a, chLatin_m, chLatin_e, chNull
};

const XMLCh SchemaSymbols::fgATTVAL_OPTIONAL[] =
{
    chLatin_o, chLatin_p, chLatin_t, chLatin_i, chLatin_o, chLatin_n,
    chLatin_a, chLatin_l, chNull
};

const XMLCh SchemaSymbols::fgATTVAL_PROHIBITED[] =
{
    chLatin_p, chLatin_r, chLatin_o, chLatin_h, chLatin_i, chLatin_b,
    chLatin_i, chLatin_t, chLatin_e, chLatin_d, chNull
};

const XMLCh SchemaSymbols::fgATTVAL_QNAME[] =
{
    chLatin_Q, chLatin_N, chLatin_a, chLatin_m, chLatin_e, chNull
};

const XMLCh SchemaSymbols::fgATTVAL_QUALIFIED[] =
{
    chLatin_q, chLatin_u, chLatin_a, chLatin_l, chLatin_i, chLatin_f,
    chLatin_i, chLatin_e, chLatin_d, chNull
};

const XMLCh SchemaSymbols::fgATTVAL_REQUIRED[] =
{
    chLatin_r, chLatin_e, chLatin_q, chLatin_u, chLatin_i, chLatin_r,
    chLatin_e, chLatin_d, chNull
};

const XMLCh SchemaSymbols::fgATTVAL_RESTRICTION[] =
{
    chLatin_r, chLatin_e, chLatin_s, chLatin_t, chLatin_r, chLatin_i,
    chLatin_c, chLatin_t, chLatin_i, chLatin_o, chLatin_n, chNull
};

const XMLCh SchemaSymbols::fgATTVAL_SKIP[] =
{
    chLatin_s, chLatin_k, chLatin_i, chLatin_p, chNull
};

const XMLCh SchemaSymbols::fgATTVAL_STRICT[] =
{
    chLatin_s, chLatin_t, chLatin_r, chLatin_i, chLatin_c, chLatin_t, chNull
};

const XMLCh SchemaSymbols::fgATTVAL_STRING[] =
{
    chLatin_s, chLatin_t, chLatin_r, chLatin_i, chLatin_n, chLatin_g, chNull
};

const XMLCh SchemaSymbols::fgATTVAL_TEXTONLY[] =
{
    chLatin_t, chLatin_e, chLatin_x, chLatin_t, chLatin_O, chLatin_n,
    chLatin_l, chLatin_y, chNull
};

const XMLCh SchemaSymbols::fgATTVAL_TIMEDURATION[] =
{
    chLatin_t, chLatin_i, chLatin_m, chLatin_e, chLatin_D, chLatin_u,
    chLatin_r, chLatin_a, chLatin_t, chLatin_i, chLatin_o, chLatin_n, chNull
};

const XMLCh SchemaSymbols::fgATTVAL_TRUE[] =
{
    chLatin_t, chLatin_r, chLatin_u, chLatin_e, chNull
};

const XMLCh SchemaSymbols::fgATTVAL_UNQUALIFIED[] =
{
    chLatin_u, chLatin_n, chLatin_q, chLatin_u, chLatin_a, chLatin_l,
    chLatin_i, chLatin_f, chLatin_i, chLatin_e, chLatin_d, chNull
};

const XMLCh SchemaSymbols::fgATTVAL_URI[] =
{
    chLatin_u, chLatin_r, chLatin_i, chNull
};

const XMLCh SchemaSymbols::fgATTVAL_URIREFERENCE[] =
{
    chLatin_u, chLatin_r, chLatin_i, chLatin_R, chLatin_e, chLatin_f,
    chLatin_e, chLatin_r, chLatin_e, chLatin_n, chLatin_c, chLatin_e, chNull
};

const XMLCh SchemaSymbols::fgATTVAL_SUBSTITUTIONGROUP[] =
{
    chLatin_s, chLatin_u, chLatin_b, chLatin_s, chLatin_t, chLatin_i,
    chLatin_t, chLatin_u, chLatin_t, chLatin_i, chLatin_o, chLatin_n,
    chLatin_G, chLatin_r, chLatin_o, chLatin_u, chLatin_p, chNull
};

const XMLCh SchemaSymbols::fgATTVAL_SUBSTITUTION[] =
{
    chLatin_s, chLatin_u, chLatin_b, chLatin_s, chLatin_t, chLatin_i,
    chLatin_t, chLatin_u, chLatin_t, chLatin_i, chLatin_o, chLatin_n, chNull
};

const XMLCh SchemaSymbols::fgATTVAL_ANYTYPE[] =
{
    chLatin_a, chLatin_n, chLatin_y, chLatin_T, chLatin_y, chLatin_p,
    chLatin_e, chNull
};

const XMLCh SchemaSymbols::fgWS_PRESERVE[] =
{
    chLatin_p, chLatin_r, chLatin_e, chLatin_s, chLatin_e, chLatin_r,
    chLatin_v, chLatin_e, chNull
};

const XMLCh SchemaSymbols::fgWS_COLLAPSE[] =
{
    chLatin_c, chLatin_o, chLatin_l, chLatin_l, chLatin_a, chLatin_p,
    chLatin_s, chLatin_e, chNull
};

const XMLCh SchemaSymbols::fgWS_REPLACE[] =
{
    chLatin_r, chLatin_e, chLatin_p, chLatin_l, chLatin_a, chLatin_c, chLatin_e, chNull
};

const XMLCh SchemaSymbols::fgDT_STRING[] =
{
    chLatin_s, chLatin_t, chLatin_r, chLatin_i, chLatin_n, chLatin_g, chNull
};

const XMLCh SchemaSymbols::fgDT_TOKEN[] =
{
    chLatin_t, chLatin_o, chLatin_k, chLatin_e, chLatin_n, chNull
};

const XMLCh SchemaSymbols::fgDT_LANGUAGE[] =
{
    chLatin_l, chLatin_a, chLatin_n, chLatin_g, chLatin_u, chLatin_a,
    chLatin_g, chLatin_e, chNull
};

const XMLCh SchemaSymbols::fgDT_NAME[] =
{
    chLatin_N, chLatin_a, chLatin_m, chLatin_e, chNull
};

const XMLCh SchemaSymbols::fgDT_NCNAME[] =
{
    chLatin_N, chLatin_C, chLatin_N, chLatin_a, chLatin_m, chLatin_e, chNull
};

const XMLCh SchemaSymbols::fgDT_INTEGER[] =
{
    chLatin_i, chLatin_n, chLatin_t, chLatin_e, chLatin_g, chLatin_e, chLatin_r, chNull
};

const XMLCh SchemaSymbols::fgDT_DECIMAL[] =
{
    chLatin_d, chLatin_e, chLatin_c, chLatin_i, chLatin_m, chLatin_a, chLatin_l, chNull
};

const XMLCh SchemaSymbols::fgDT_BOOLEAN[] =
{
    chLatin_b, chLatin_o, chLatin_o, chLatin_l, chLatin_e, chLatin_a, chLatin_n, chNull
};

const XMLCh SchemaSymbols::fgDT_NONPOSITIVEINTEGER[] =
{
    chLatin_n, chLatin_o, chLatin_n, chLatin_P, chLatin_o, chLatin_s,
    chLatin_i, chLatin_t, chLatin_i, chLatin_v, chLatin_e, chLatin_I,
    chLatin_n, chLatin_t, chLatin_e, chLatin_g, chLatin_e, chLatin_r, chNull
};

const XMLCh SchemaSymbols::fgDT_NEGATIVEINTEGER[] =
{
    chLatin_n, chLatin_e, chLatin_g, chLatin_a, chLatin_t, chLatin_i,
    chLatin_v, chLatin_e, chLatin_I, chLatin_n, chLatin_t, chLatin_e,
    chLatin_g, chLatin_e, chLatin_r, chNull
};

const XMLCh SchemaSymbols::fgDT_LONG[] =
{
    chLatin_l, chLatin_o, chLatin_n, chLatin_g, chNull
};

const XMLCh SchemaSymbols::fgDT_INT[] =
{
    chLatin_i, chLatin_n, chLatin_t, chNull
};

const XMLCh SchemaSymbols::fgDT_SHORT[] =
{
    chLatin_s, chLatin_h, chLatin_o, chLatin_r, chLatin_t, chNull
};

const XMLCh SchemaSymbols::fgDT_BYTE[] =
{
    chLatin_b, chLatin_y, chLatin_t, chLatin_e, chNull
};

const XMLCh SchemaSymbols::fgDT_NONNEGATIVEINTEGER[] =
{
    chLatin_n, chLatin_o, chLatin_n, chLatin_N, chLatin_e, chLatin_g,
    chLatin_a, chLatin_t, chLatin_i, chLatin_v, chLatin_e, chLatin_I,
    chLatin_n, chLatin_t, chLatin_e, chLatin_g, chLatin_e, chLatin_r, chNull
};

const XMLCh SchemaSymbols::fgDT_ULONG[] =
{
    chLatin_u, chLatin_n, chLatin_s, chLatin_i, chLatin_g, chLatin_n,
    chLatin_e, chLatin_d, chLatin_L, chLatin_o, chLatin_n, chLatin_g, chNull
};

const XMLCh SchemaSymbols::fgDT_UINT[] =
{
    chLatin_u, chLatin_n, chLatin_s, chLatin_i, chLatin_g, chLatin_n,
    chLatin_e, chLatin_d, chLatin_I, chLatin_n, chLatin_t, chNull
};

const XMLCh SchemaSymbols::fgDT_USHORT[] =
{
    chLatin_u, chLatin_n, chLatin_s, chLatin_i, chLatin_g, chLatin_n, chLatin_e,
	chLatin_d, chLatin_S, chLatin_h, chLatin_o, chLatin_r, chLatin_t, chNull
};

const XMLCh SchemaSymbols::fgDT_UBYTE[] =
{
    chLatin_u, chLatin_n, chLatin_s, chLatin_i, chLatin_g, chLatin_n,
    chLatin_e, chLatin_d, chLatin_B, chLatin_y, chLatin_t, chLatin_e, chNull
};

const XMLCh SchemaSymbols::fgDT_POSITIVEINTEGER[] =
{
    chLatin_p, chLatin_o, chLatin_s, chLatin_i, chLatin_t, chLatin_i,
    chLatin_v, chLatin_e, chLatin_I, chLatin_n, chLatin_t, chLatin_e,
    chLatin_g, chLatin_e, chLatin_r, chNull
};

const XMLCh SchemaSymbols::fgDT_DATETIME[] =
{
    chLatin_d, chLatin_a, chLatin_t, chLatin_e,
    chLatin_T, chLatin_i, chLatin_m, chLatin_e, chNull
};

const XMLCh SchemaSymbols::fgDT_DATE[] =
{
    chLatin_d, chLatin_a, chLatin_t, chLatin_e, chNull
};

const XMLCh SchemaSymbols::fgDT_TIME[] =
{
    chLatin_t, chLatin_i, chLatin_m, chLatin_e, chNull
};

const XMLCh SchemaSymbols::fgDT_DURATION[] =
{
    chLatin_d, chLatin_u, chLatin_r, chLatin_a,
    chLatin_t, chLatin_i, chLatin_o, chLatin_n, chNull
};

const XMLCh SchemaSymbols::fgDT_DAY[] =
{
    chLatin_g, chLatin_D, chLatin_a, chLatin_y, chNull
};

const XMLCh SchemaSymbols::fgDT_MONTH[] =
{
    chLatin_g, chLatin_M, chLatin_o, chLatin_n, chLatin_t, chLatin_h, chNull
};

const XMLCh SchemaSymbols::fgDT_MONTHDAY[] =
{
    chLatin_g, chLatin_M, chLatin_o, chLatin_n, chLatin_t, chLatin_h,
    chLatin_D, chLatin_a, chLatin_y, chNull
};

const XMLCh SchemaSymbols::fgDT_YEAR[] =
{
    chLatin_g, chLatin_Y, chLatin_e, chLatin_a, chLatin_r, chNull
};

const XMLCh SchemaSymbols::fgDT_YEARMONTH[] =
{
    chLatin_g, chLatin_Y, chLatin_e, chLatin_a, chLatin_r,
    chLatin_M, chLatin_o, chLatin_n, chLatin_t, chLatin_h, chNull
};

const XMLCh SchemaSymbols::fgDT_BASE64BINARY[] =
{
    chLatin_b, chLatin_a, chLatin_s, chLatin_e, chDigit_6, chDigit_4,
    chLatin_B, chLatin_i, chLatin_n, chLatin_a, chLatin_r, chLatin_y, chNull
};

const XMLCh SchemaSymbols::fgDT_HEXBINARY[] =
{
    chLatin_h, chLatin_e, chLatin_x,
    chLatin_B, chLatin_i, chLatin_n, chLatin_a, chLatin_r, chLatin_y, chNull
};

const XMLCh SchemaSymbols::fgDT_FLOAT[] =
{
    chLatin_f, chLatin_l, chLatin_o, chLatin_a, chLatin_t, chNull
};

const XMLCh SchemaSymbols::fgDT_DOUBLE[] =
{
    chLatin_d, chLatin_o, chLatin_u, chLatin_b, chLatin_l, chLatin_e, chNull
};

const XMLCh SchemaSymbols::fgDT_URIREFERENCE[] =
{
    chLatin_u, chLatin_r, chLatin_i, chLatin_R, chLatin_e, chLatin_f,
    chLatin_e, chLatin_r, chLatin_e, chLatin_n, chLatin_c, chLatin_e, chNull
};

const XMLCh SchemaSymbols::fgDT_ANYURI[] =
{
    chLatin_a, chLatin_n, chLatin_y, chLatin_U, chLatin_R, chLatin_I, chNull
};

const XMLCh SchemaSymbols::fgDT_QNAME[] =
{
    chLatin_Q, chLatin_N, chLatin_a, chLatin_m, chLatin_e, chNull
};

const XMLCh SchemaSymbols::fgDT_NORMALIZEDSTRING[] =
{
    chLatin_n, chLatin_o, chLatin_r, chLatin_m, chLatin_a, chLatin_l, chLatin_i,
    chLatin_z, chLatin_e, chLatin_d, chLatin_S, chLatin_t, chLatin_r, chLatin_i,
    chLatin_n, chLatin_g, chNull
};

const XMLCh SchemaSymbols::fgDT_ANYSIMPLETYPE[] =
{
    chLatin_a, chLatin_n, chLatin_y, chLatin_S, chLatin_i, chLatin_m, chLatin_p,
    chLatin_l, chLatin_e, chLatin_T, chLatin_y, chLatin_p, chLatin_e, chNull
};

const XMLCh SchemaSymbols::fgRegEx_XOption[] =
{
    chLatin_X, chNull
};

const XMLCh SchemaSymbols::fgRedefIdentifier[] =
{
    chUnderscore, chLatin_r, chLatin_d, chLatin_f, chLatin_n, chNull
};

const int SchemaSymbols::fgINT_MIN_VALUE = 0x80000000;

const int SchemaSymbols::fgINT_MAX_VALUE = 0x7fffffff;

XERCES_CPP_NAMESPACE_END

/**
  * End of file SchemaSymbols.cpp
  */

