/*
 * The Apache Software License, Version 1.1
 *
 * Copyright (c) 2001-2004 The Apache Software Foundation.  All rights
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
 * $Id: XMLDateTime.cpp,v 1.25 2004/01/29 11:48:47 cargilld Exp $
 * $Log: XMLDateTime.cpp,v $
 * Revision 1.25  2004/01/29 11:48:47  cargilld
 * Code cleanup changes to get rid of various compiler diagnostic messages.
 *
 * Revision 1.24  2004/01/25 23:23:26  jberry
 * Step around CodeWarrior compiler warning
 *
 * Revision 1.23  2004/01/13 19:50:56  peiyongz
 * remove parseContent()
 *
 * Revision 1.22  2004/01/13 16:34:20  cargilld
 * Misc memory management changes.
 *
 * Revision 1.21  2004/01/03 00:03:18  peiyongz
 * parseContent
 *
 * Revision 1.20  2003/12/31 02:34:11  neilg
 * enable production of canonical representations for dates with negative years, or years >9999
 *
 * Revision 1.19  2003/12/17 00:18:35  cargilld
 * Update to memory management so that the static memory manager (one used to call Initialize) is only for static data.
 *
 * Revision 1.18  2003/12/16 22:48:52  peiyongz
 * exception thrown upon invalid number, thanks Gareth Reakes.
 *
 * Revision 1.17  2003/12/11 21:38:12  peiyongz
 * support for Canonical Representation for Datatype
 *
 * Revision 1.16  2003/09/25 22:24:28  peiyongz
 * Using writeString/readString
 *
 * Revision 1.15  2003/09/25 15:22:54  peiyongz
 * Solve HP complier error
 *
 * Revision 1.14  2003/09/23 18:16:07  peiyongz
 * Inplementation for Serialization/Deserialization
 *
 * Revision 1.13  2003/08/14 02:57:27  knoaman
 * Code refactoring to improve performance of validation.
 *
 * Revision 1.12  2003/05/22 02:10:52  knoaman
 * Default the memory manager.
 *
 * Revision 1.11  2003/05/18 14:02:05  knoaman
 * Memory manager implementation: pass per instance manager.
 *
 * Revision 1.10  2003/05/15 19:07:46  knoaman
 * Partial implementation of the configurable memory manager.
 *
 * Revision 1.9  2003/05/15 16:32:19  gareth
 * We did not allow dateTimes with a timezone due to the last seconds fix.
 *
 * Revision 1.8  2003/03/23 22:54:49  peiyongz
 * invalid second values
 *
 * Revision 1.7  2003/02/22 22:49:09  peiyongz
 * Schema Errata E2-45 24:00:00 allowed
 *
 * Revision 1.6  2003/02/02 23:54:43  peiyongz
 * getFormattedString() added to return original and converted value.
 *
 * Revision 1.5  2003/01/30 21:55:22  tng
 * Performance: create getRawData which is similar to toString but return the internal data directly, user is not required to delete the returned memory.
 *
 * Revision 1.4  2002/11/28 20:39:27  peiyongz
 * Schema Errata: E2-23 seconds part shall have at least one digit after the dot
 *                           if it appears.
 *
 * Revision 1.3  2002/11/06 22:22:21  peiyongz
 * Schema-Errata: E2-12: gMonth
 *
 * Revision 1.2  2002/11/04 15:22:05  tng
 * C++ Namespace Support.
 *
 * Revision 1.1.1.1  2002/02/01 22:22:14  peiyongz
 * sane_include
 *
 * Revision 1.4  2001/11/14 22:04:03  peiyongz
 * Patch to apply check on Year and more rigorous on other fields as well.
 *
 * Revision 1.3  2001/11/12 20:36:54  peiyongz
 * SchemaDateTimeException defined
 *
 * Revision 1.2  2001/11/09 20:41:45  peiyongz
 * Fix: compilation error on Solaris and AIX.
 *
 * Revision 1.1  2001/11/07 19:16:03  peiyongz
 * DateTime Port
 *
 *
 */

// ---------------------------------------------------------------------------
//  Includes
// ---------------------------------------------------------------------------
#include <stdlib.h>
#include <assert.h>
#include <xercesc/util/XMLDateTime.hpp>
#include <xercesc/util/XMLString.hpp>
#include <xercesc/util/XMLUni.hpp>
#include <xercesc/util/Janitor.hpp>
#include <xercesc/util/NumberFormatException.hpp>

XERCES_CPP_NAMESPACE_BEGIN

//
// constants used to process raw data (fBuffer)
//
// [-]{CCYY-MM-DD}'T'{HH:MM:SS.MS}['Z']
//                                [{+|-}hh:mm']
//

static const XMLCh DURATION_STARTER     = chLatin_P;              // 'P'
static const XMLCh DURATION_Y           = chLatin_Y;              // 'Y'
static const XMLCh DURATION_M           = chLatin_M;              // 'M'
static const XMLCh DURATION_D           = chLatin_D;              // 'D'
static const XMLCh DURATION_H           = chLatin_H;              // 'H'
static const XMLCh DURATION_S           = chLatin_S;              // 'S'

static const XMLCh DATE_SEPARATOR       = chDash;                 // '-'
static const XMLCh TIME_SEPARATOR       = chColon;                // ':'
static const XMLCh TIMEZONE_SEPARATOR   = chColon;                // ':'
static const XMLCh DATETIME_SEPARATOR   = chLatin_T;              // 'T'
static const XMLCh MILISECOND_SEPARATOR = chPeriod;               // '.'

static const XMLCh UTC_STD_CHAR         = chLatin_Z;              // 'Z'
static const XMLCh UTC_POS_CHAR         = chPlus;                 // '+'
static const XMLCh UTC_NEG_CHAR         = chDash;                 // '-'

static const XMLCh UTC_SET[]            = {UTC_STD_CHAR           //"Z+-"
                                         , UTC_POS_CHAR
                                         , UTC_NEG_CHAR
                                         , chNull};

static const int YMD_MIN_SIZE    = 10;   // CCYY-MM-DD
static const int YMONTH_MIN_SIZE = 7;    // CCYY_MM
static const int TIME_MIN_SIZE   = 8;    // hh:mm:ss
static const int TIMEZONE_SIZE   = 5;    // hh:mm
static const int DAY_SIZE        = 5;    // ---DD
//static const int MONTH_SIZE      = 6;    // --MM--
static const int MONTHDAY_SIZE   = 7;    // --MM-DD
static const int NOT_FOUND       = -1;

//define constants to be used in assigning default values for
//all date/time excluding duration
static const int YEAR_DEFAULT  = 2000;
static const int MONTH_DEFAULT = 01;
static const int DAY_DEFAULT   = 15;

// order-relation on duration is a partial order. The dates below are used to
// for comparison of 2 durations, based on the fact that
// duration x and y is x<=y iff s+x<=s+y
// see 3.2.6 duration W3C schema datatype specs
//
// the dates are in format: {CCYY,MM,DD, H, S, M, MS, timezone}
static const int DATETIMES[][XMLDateTime::TOTAL_SIZE] =
{
    {1696, 9, 1, 0, 0, 0, 0, XMLDateTime::UTC_STD},
	{1697, 2, 1, 0, 0, 0, 0, XMLDateTime::UTC_STD},
	{1903, 3, 1, 0, 0, 0, 0, XMLDateTime::UTC_STD},
	{1903, 7, 1, 0, 0, 0, 0, XMLDateTime::UTC_STD}
};

// ---------------------------------------------------------------------------
//  local methods
// ---------------------------------------------------------------------------
static inline int fQuotient(int a, int b)
{
    div_t div_result = div(a, b);
    return div_result.quot;
}

static inline int fQuotient(int temp, int low, int high)
{
    return fQuotient(temp - low, high - low);
}

static inline int mod(int a, int b, int quotient)
{
	return (a - quotient*b) ;
}

static inline int modulo (int temp, int low, int high)
{
    //modulo(a - low, high - low) + low
    int a = temp - low;
    int b = high - low;
    return (mod (a, b, fQuotient(a, b)) + low) ;
}

static inline bool isLeapYear(int year)
{
    return((year%4 == 0) && ((year%100 != 0) || (year%400 == 0)));
}

static int maxDayInMonthFor(int year, int month)
{

    if ( month == 4 || month == 6 || month == 9 || month == 11 )
    {
        return 30;
    }
    else if ( month==2 )
    {
        if ( isLeapYear(year) )
            return 29;
        else
            return 28;
    }
    else
    {
        return 31;
    }

}

// ---------------------------------------------------------------------------
//  static methods : for duration
// ---------------------------------------------------------------------------
/**
 * Compares 2 given durations. (refer to W3C Schema Datatypes "3.2.6 duration")
 *
 * 3.2.6.2 Order relation on duration
 *
 *     In general, the order-relation on duration is a partial order since there is no
 *  determinate relationship between certain durations such as one month (P1M) and 30 days (P30D).
 *  The order-relation of two duration values x and y is x < y iff s+x < s+y for each qualified
 *  dateTime s in the list below.
 *
 *     These values for s cause the greatest deviations in the addition of dateTimes and durations
 *
 **/
int XMLDateTime::compare(const XMLDateTime* const pDate1
                       , const XMLDateTime* const pDate2
                       , bool  strict)
{
    //REVISIT: this is unoptimazed vs of comparing 2 durations
    //         Algorithm is described in 3.2.6.2 W3C Schema Datatype specs
    //

    int resultA, resultB = INDETERMINATE;

    //try and see if the objects are equal
    if ( (resultA = compareOrder(pDate1, pDate2)) == EQUAL)
        return EQUAL;

    //long comparison algorithm is required
    XMLDateTime tempA(XMLPlatformUtils::fgMemoryManager), *pTempA = &tempA;
    XMLDateTime tempB(XMLPlatformUtils::fgMemoryManager), *pTempB = &tempB;

    addDuration(pTempA, pDate1, 0);
    addDuration(pTempB, pDate2, 0);
    resultA = compareOrder(pTempA, pTempB);
    if ( resultA == INDETERMINATE )
        return INDETERMINATE;

    addDuration(pTempA, pDate1, 1);
    addDuration(pTempB, pDate2, 1);
    resultB = compareOrder(pTempA, pTempB);
    resultA = compareResult(resultA, resultB, strict);
    if ( resultA == INDETERMINATE )
        return INDETERMINATE;

    addDuration(pTempA, pDate1, 2);
    addDuration(pTempB, pDate2, 2);
    resultB = compareOrder(pTempA, pTempB);
    resultA = compareResult(resultA, resultB, strict);
    if ( resultA == INDETERMINATE )
        return INDETERMINATE;

    addDuration(pTempA, pDate1, 3);
    addDuration(pTempB, pDate2, 3);
    resultB = compareOrder(pTempA, pTempB);
    resultA = compareResult(resultA, resultB, strict);

    return resultA;

}

//
// Form a new XMLDateTime with duration and baseDate array
// Note: C++        Java
//       fNewDate   duration
//       fDuration  date
//

void XMLDateTime::addDuration(XMLDateTime*             fNewDate
                            , const XMLDateTime* const fDuration
                            , int index)

{

    //REVISIT: some code could be shared between normalize() and this method,
    //         however is it worth moving it? The structures are different...
    //

    fNewDate->reset();
    //add months (may be modified additionaly below)
    int temp = DATETIMES[index][Month] + fDuration->fValue[Month];
    fNewDate->fValue[Month] = modulo(temp, 1, 13);
    int carry = fQuotient(temp, 1, 13);

    //add years (may be modified additionaly below)
    fNewDate->fValue[CentYear] = DATETIMES[index][CentYear] + fDuration->fValue[CentYear] + carry;

    //add seconds
    temp = DATETIMES[index][Second] + fDuration->fValue[Second];
    carry = fQuotient (temp, 60);
    fNewDate->fValue[Second] =  mod(temp, 60, carry);
		
    //add minutes
    temp = DATETIMES[index][Minute] + fDuration->fValue[Minute] + carry;
    carry = fQuotient(temp, 60);
    fNewDate->fValue[Minute] = mod(temp, 60, carry);

    //add hours
    temp = DATETIMES[index][Hour] + fDuration->fValue[Hour] + carry;
    carry = fQuotient(temp, 24);
    fNewDate->fValue[Hour] = mod(temp, 24, carry);
		
    fNewDate->fValue[Day] = DATETIMES[index][Day] + fDuration->fValue[Day] + carry;

    while ( true )
    {
        temp = maxDayInMonthFor(fNewDate->fValue[CentYear], fNewDate->fValue[Month]);
        if ( fNewDate->fValue[Day] < 1 )
        { //original fNewDate was negative
            fNewDate->fValue[Day] += maxDayInMonthFor(fNewDate->fValue[CentYear], fNewDate->fValue[Month]-1);
            carry = -1;
        }
        else if ( fNewDate->fValue[Day] > temp )
        {
            fNewDate->fValue[Day] -= temp;
            carry = 1;
        }
        else
        {
            break;
        }

        temp = fNewDate->fValue[Month] + carry;
        fNewDate->fValue[Month] = modulo(temp, 1, 13);
        fNewDate->fValue[CentYear] += fQuotient(temp, 1, 13);
    }

    //fNewDate->fValue[utc] = UTC_STD_CHAR;
    fNewDate->fValue[utc] = UTC_STD;
}

int XMLDateTime::compareResult(int resultA
                             , int resultB
                             , bool strict)
{

    if ( resultB == INDETERMINATE )
    {
        return INDETERMINATE;
    }
    else if ( (resultA != resultB) &&
              strict                )
    {
        return INDETERMINATE;
    }
    else if ( (resultA != resultB) &&
              !strict               )
    {
        if ( (resultA != EQUAL) &&
             (resultB != EQUAL)  )
        {
            return INDETERMINATE;
        }
        else
        {
            return (resultA != EQUAL)? resultA : resultB;
        }
    }

    return resultA;
	
}

// ---------------------------------------------------------------------------
//  static methods : for others
// ---------------------------------------------------------------------------
int XMLDateTime::compare(const XMLDateTime* const pDate1
                       , const XMLDateTime* const pDate2)
{

    if (pDate1->fValue[utc] == pDate2->fValue[utc])
    {
        return XMLDateTime::compareOrder(pDate1, pDate2);
    }

    int c1, c2;

    if ( pDate1->isNormalized())
    {
        c1 = compareResult(pDate1, pDate2, false, UTC_POS);
        c2 = compareResult(pDate1, pDate2, false, UTC_NEG);
        return getRetVal(c1, c2);
    }
    else if ( pDate2->isNormalized())
    {
        c1 = compareResult(pDate1, pDate2, true, UTC_POS);
        c2 = compareResult(pDate1, pDate2, true, UTC_NEG);
        return getRetVal(c1, c2);
    }

    return INDETERMINATE;	
}

int XMLDateTime::compareResult(const XMLDateTime* const pDate1
                             , const XMLDateTime* const pDate2
                             , bool  set2Left
                             , int   utc_type)
{
    XMLDateTime tmpDate = (set2Left ? *pDate1 : *pDate2);

    tmpDate.fTimeZone[hh] = 14;
    tmpDate.fTimeZone[mm] = 0;
    tmpDate.fValue[utc] = utc_type;
    tmpDate.normalize();

    return (set2Left? XMLDateTime::compareOrder(&tmpDate, pDate2) :
                      XMLDateTime::compareOrder(pDate1, &tmpDate));
}

int XMLDateTime::compareOrder(const XMLDateTime* const lValue
                            , const XMLDateTime* const rValue)
                            //, MemoryManager* const memMgr)
{
    //
    // If any of the them is not normalized() yet,
    // we need to do something here.
    //
    XMLDateTime lTemp = *lValue;
    XMLDateTime rTemp = *rValue;

    lTemp.normalize();
    rTemp.normalize();

    for ( int i = 0 ; i < TOTAL_SIZE; i++ )
    {
        if ( lTemp.fValue[i] < rTemp.fValue[i] )
        {
            return LESS_THAN;
        }
        else if ( lTemp.fValue[i] > rTemp.fValue[i] )
        {
            return GREATER_THAN;
        }
    }

    return EQUAL;
}

// ---------------------------------------------------------------------------
//  ctor and dtor
// ---------------------------------------------------------------------------
XMLDateTime::~XMLDateTime()
{    
    if (fBuffer)
        fMemoryManager->deallocate(fBuffer);//delete[] fBuffer;  
}

XMLDateTime::XMLDateTime(MemoryManager* const manager)
: fStart(0)
, fEnd(0)
, fBufferMaxLen(0)
, fBuffer(0)
, fMemoryManager(manager)
{
    reset();
}

XMLDateTime::XMLDateTime(const XMLCh* const aString,
                         MemoryManager* const manager)
: fStart(0)
, fEnd(0)
, fBufferMaxLen(0)
, fBuffer(0)
, fMemoryManager(manager)
{
    setBuffer(aString);
}

// -----------------------------------------------------------------------
// Copy ctor and Assignment operators
// -----------------------------------------------------------------------

XMLDateTime::XMLDateTime(const XMLDateTime &toCopy)
: fBufferMaxLen(0)
, fBuffer(0)
, fMemoryManager(toCopy.fMemoryManager)
{
    copy(toCopy);
}

XMLDateTime& XMLDateTime::operator=(const XMLDateTime& rhs)
{
    if (this == &rhs)
        return *this;

    copy(rhs);
    return *this;
}

// -----------------------------------------------------------------------
// Implementation of Abstract Interface
// -----------------------------------------------------------------------

//
// We may simply return the handle to fBuffer, but
// for the sake of consistency, we return a duplicated copy
// and the caller is responsible for the release of the buffer
// just like any other things in the XMLNumber family.
//
XMLCh*  XMLDateTime::toString() const
{
    assertBuffer();

    // Return data using global operator new
    XMLCh* retBuf = XMLString::replicate(fBuffer);
    return retBuf;
}

//
// We may simply return the handle to fBuffer
//
XMLCh*  XMLDateTime::getRawData() const
{
    assertBuffer();    
    return fBuffer;
}


const XMLCh*  XMLDateTime::getFormattedString() const
{
    return getRawData();
}

int XMLDateTime::getSign() const
{
    return 0;
}

// ---------------------------------------------------------------------------
//  Parsers
// ---------------------------------------------------------------------------

//
// [-]{CCYY-MM-DD}'T'{HH:MM:SS.MS}[TimeZone]
//
void XMLDateTime::parseDateTime()
{
    initParser();
    getDate();

    //fStart is supposed to point to 'T'
    if (fBuffer[fStart++] != DATETIME_SEPARATOR)
        ThrowXMLwithMemMgr1(SchemaDateTimeException
                , XMLExcepts::DateTime_dt_missingT
                , fBuffer
                , fMemoryManager);

    getTime();
    validateDateTime();
    normalize();
}

//
// [-]{CCYY-MM-DD}[TimeZone]
//
void XMLDateTime::parseDate()
{
    initParser();
    getDate();
    parseTimeZone();
    validateDateTime();
    normalize();
}

void XMLDateTime::parseTime()
{
    initParser();

    // time initialize to default values
    fValue[CentYear]= YEAR_DEFAULT;
    fValue[Month]   = MONTH_DEFAULT;
    fValue[Day]     = DAY_DEFAULT;

    getTime();

    validateDateTime();
    normalize();
}

//
// {---DD}[TimeZone]
//  01234
//
void XMLDateTime::parseDay()
{
    initParser();

    if (fBuffer[0] != DATE_SEPARATOR ||
        fBuffer[1] != DATE_SEPARATOR ||
        fBuffer[2] != DATE_SEPARATOR  )
    {
        ThrowXMLwithMemMgr1(SchemaDateTimeException
                , XMLExcepts::DateTime_gDay_invalid
                , fBuffer
                , fMemoryManager);
    }

    //initialize values
    fValue[CentYear] = YEAR_DEFAULT;
    fValue[Month]    = MONTH_DEFAULT;
    fValue[Day]      = parseInt(fStart+3, fStart+5);

    if ( DAY_SIZE < fEnd )
    {
        int sign = findUTCSign(DAY_SIZE);
        if ( sign < 0 )
        {
            ThrowXMLwithMemMgr1(SchemaDateTimeException
                    , XMLExcepts::DateTime_gDay_invalid
                    , fBuffer
                    , fMemoryManager);
        }
        else
        {
            getTimeZone(sign);
        }
    }

    validateDateTime();
    normalize();
}

//
// {--MM--}[TimeZone]
// {--MM}[TimeZone]
//  012345
//
void XMLDateTime::parseMonth()
{
    initParser();

    if (fBuffer[0] != DATE_SEPARATOR ||
        fBuffer[1] != DATE_SEPARATOR  )
    {
        ThrowXMLwithMemMgr1(SchemaDateTimeException
                , XMLExcepts::DateTime_gMth_invalid
                , fBuffer
                , fMemoryManager);
    }

    //set constants
    fValue[CentYear] = YEAR_DEFAULT;
    fValue[Day]      = DAY_DEFAULT;
    fValue[Month]    = parseInt(2, 4);

    // REVISIT: allow both --MM and --MM-- now. 
    // need to remove the following lines to disallow --MM-- 
    // when the errata is officially in the rec. 
    fStart = 4;
    if ( fEnd >= fStart+2 && fBuffer[fStart] == DATE_SEPARATOR && fBuffer[fStart+1] == DATE_SEPARATOR ) 
    { 
        fStart += 2; 
    } 

    //
    // parse TimeZone if any
    //
    if ( fStart < fEnd )
    {
        int sign = findUTCSign(fStart);
        if ( sign < 0 )
        {
            ThrowXMLwithMemMgr1(SchemaDateTimeException
                    , XMLExcepts::DateTime_gMth_invalid
                    , fBuffer
                    , fMemoryManager);
        }
        else
        {
            getTimeZone(sign);
        }
    }

    validateDateTime();
    normalize();
}

//
//[-]{CCYY}[TimeZone]
// 0  1234
//
void XMLDateTime::parseYear()
{
    initParser();

    // skip the first '-' and search for timezone
    //
    int sign = findUTCSign((fBuffer[0] == chDash) ? 1 : 0);

    if (sign == NOT_FOUND)
    {
        fValue[CentYear] = parseIntYear(fEnd);
    }
    else
    {
        fValue[CentYear] = parseIntYear(sign);
        getTimeZone(sign);
    }

    //initialize values
    fValue[Month] = MONTH_DEFAULT;
    fValue[Day]   = DAY_DEFAULT;   //java is 1

    validateDateTime();
    normalize();
}

//
//{--MM-DD}[TimeZone]
// 0123456
//
void XMLDateTime::parseMonthDay()
{
    initParser();

    if (fBuffer[0] != DATE_SEPARATOR ||
        fBuffer[1] != DATE_SEPARATOR ||
        fBuffer[4] != DATE_SEPARATOR )
    {
        ThrowXMLwithMemMgr1(SchemaDateTimeException
                , XMLExcepts::DateTime_gMthDay_invalid
                , fBuffer
                , fMemoryManager);
    }


    //initialize
    fValue[CentYear] = YEAR_DEFAULT;
    fValue[Month]    = parseInt(2, 4);	
    fValue[Day]      = parseInt(5, 7);

    if ( MONTHDAY_SIZE < fEnd )
    {
        int sign = findUTCSign(MONTHDAY_SIZE);
        if ( sign<0 )
        {
            ThrowXMLwithMemMgr1(SchemaDateTimeException
                    , XMLExcepts::DateTime_gMthDay_invalid
                    , fBuffer
                    , fMemoryManager);
        }
        else
        {
            getTimeZone(sign);
        }
    }

    validateDateTime();
    normalize();
}

void XMLDateTime::parseYearMonth()
{
    initParser();

    // get date
    getYearMonth();
    fValue[Day] = DAY_DEFAULT;
    parseTimeZone();

    validateDateTime();
    normalize();
}

//
//PnYn MnDTnH nMnS: -P1Y2M3DT10H30M
//
// [-]{'P'{[n'Y'][n'M'][n'D']['T'][n'H'][n'M'][n'S']}}
//
//  Note: the n above shall be >= 0
//        if no time element found, 'T' shall be absent
//
void XMLDateTime::parseDuration()
{
    initParser();

    // must start with '-' or 'P'
    //
    XMLCh c = fBuffer[fStart++];
    if ( (c != DURATION_STARTER) &&
         (c != chDash)            )
    {
        ThrowXMLwithMemMgr1(SchemaDateTimeException
                , XMLExcepts::DateTime_dur_Start_dashP
                , fBuffer
                , fMemoryManager);
    }

    // 'P' must ALWAYS be present in either case
    if ( (c == chDash) &&
         (fBuffer[fStart++]!= DURATION_STARTER ))
    {
        ThrowXMLwithMemMgr1(SchemaDateTimeException
                , XMLExcepts::DateTime_dur_noP
                , fBuffer
                , fMemoryManager);
    }

    // java code
    //date[utc]=(c=='-')?'-':0;
    //fValue[utc] = UTC_STD;
    fValue[utc] = (fBuffer[0] == chDash? UTC_NEG : UTC_STD);

    int negate = ( fBuffer[0] == chDash ? -1 : 1);

    //
    // No negative value is allowed after 'P'
    //
    // eg P-1234, invalid
    //
    if (indexOf(fStart, fEnd, chDash) != NOT_FOUND)
    {
        ThrowXMLwithMemMgr1(SchemaDateTimeException
                , XMLExcepts::DateTime_dur_DashNotFirst
                , fBuffer
                , fMemoryManager);
    }

    //at least one number and designator must be seen after P
    bool designator = false;

    int endDate = indexOf(fStart, fEnd, DATETIME_SEPARATOR);
    if ( endDate == NOT_FOUND )
    {
        endDate = fEnd;  // 'T' absent
    }

    //find 'Y'
    int end = indexOf(fStart, endDate, DURATION_Y);
    if ( end != NOT_FOUND )
    {
        //scan year
        fValue[CentYear] = negate * parseInt(fStart, end);
        fStart = end+1;
        designator = true;
    }

    end = indexOf(fStart, endDate, DURATION_M);
    if ( end != NOT_FOUND )
    {
        //scan month
        fValue[Month] = negate * parseInt(fStart, end);
        fStart = end+1;
        designator = true;
    }

    end = indexOf(fStart, endDate, DURATION_D);
    if ( end != NOT_FOUND )
    {
        //scan day
        fValue[Day] = negate * parseInt(fStart,end);
        fStart = end+1;
        designator = true;
    }

    if ( (fEnd == endDate) &&   // 'T' absent
         (fStart != fEnd)   )   // something after Day
    {
        ThrowXMLwithMemMgr1(SchemaDateTimeException
                , XMLExcepts::DateTime_dur_inv_b4T
                , fBuffer
                , fMemoryManager);
    }

    if ( fEnd != endDate ) // 'T' present
    {
        //scan hours, minutes, seconds
        //

        // skip 'T' first
        end = indexOf(++fStart, fEnd, DURATION_H);
        if ( end != NOT_FOUND )
        {
            //scan hours
            fValue[Hour] = negate * parseInt(fStart, end);
            fStart = end+1;
            designator = true;
        }

        end = indexOf(fStart, fEnd, DURATION_M);
        if ( end != NOT_FOUND )
        {
            //scan min
            fValue[Minute] = negate * parseInt(fStart, end);
            fStart = end+1;
            designator = true;
        }

        end = indexOf(fStart, fEnd, DURATION_S);
        if ( end != NOT_FOUND )
        {
            //scan seconds
            int mlsec = indexOf (fStart, end, MILISECOND_SEPARATOR);

            /***
             * Schema Errata: E2-23
             * at least one digit must follow the decimal point if it appears. 
             * That is, the value of the seconds component must conform 
             * to the following pattern: [0-9]+(.[0-9]+)? 
             */
            if ( mlsec != NOT_FOUND )
            {
                /***
                 * make usure there is something after the '.' and before the end.
                 */
                if ( mlsec+1 == end )
                {
                    ThrowXMLwithMemMgr1(SchemaDateTimeException
                            , XMLExcepts::DateTime_dur_inv_seconds
                            , fBuffer
                            , fMemoryManager);
                }

                fValue[Second]     = negate * parseInt(fStart, mlsec);
                fValue[MiliSecond] = negate * parseInt(mlsec+1, end);
            }
            else
            {
                fValue[Second] = negate * parseInt(fStart,end);
            }

            fStart = end+1;
            designator = true;
        }

        // no additional data should appear after last item
        // P1Y1M1DT is illigal value as well
        if ( (fStart != fEnd) ||
              fBuffer[--fStart] == DATETIME_SEPARATOR )
        {
            ThrowXMLwithMemMgr1(SchemaDateTimeException
                    , XMLExcepts::DateTime_dur_NoTimeAfterT
                    , fBuffer
                    , fMemoryManager);
        }
    }

    if ( !designator )
    {
        ThrowXMLwithMemMgr1(SchemaDateTimeException
                , XMLExcepts::DateTime_dur_NoElementAtAll
                , fBuffer
                , fMemoryManager);
    }

}

// ---------------------------------------------------------------------------
//  Scanners
// ---------------------------------------------------------------------------

//
// [-]{CCYY-MM-DD}
//
// Note: CCYY could be more than 4 digits
//       Assuming fStart point to the beginning of the Date Section
//       fStart updated to point to the position right AFTER the second 'D'
//       Since the lenght of CCYY might be variable, we can't check format upfront
//
void XMLDateTime::getDate()
{

    // Ensure enough chars in buffer
    if ( (fStart+YMD_MIN_SIZE) > fEnd)
        ThrowXMLwithMemMgr1(SchemaDateTimeException
                , XMLExcepts::DateTime_date_incomplete
                , fBuffer
                , fMemoryManager);

    getYearMonth();    // Scan YearMonth and
                       // fStart point to the next '-'

    if (fBuffer[fStart++] != DATE_SEPARATOR)
    {
        ThrowXMLwithMemMgr1(SchemaDateTimeException
                , XMLExcepts::DateTime_date_invalid
                , fBuffer
                , fMemoryManager);
        //("CCYY-MM must be followed by '-' sign");
    }

    fValue[Day] = parseInt(fStart, fStart+2);
    fStart += 2 ;  //fStart points right after the Day

    return;
}

//
// hh:mm:ss[.msssss]['Z']
// hh:mm:ss[.msssss][['+'|'-']hh:mm]
// 012345678
//
// Note: Assuming fStart point to the beginning of the Time Section
//       fStart updated to point to the position right AFTER the second 's'
//                                                  or ms if any
//
void XMLDateTime::getTime()
{

    // Ensure enough chars in buffer
    if ( (fStart+TIME_MIN_SIZE) > fEnd)
        ThrowXMLwithMemMgr1(SchemaDateTimeException
                , XMLExcepts::DateTime_time_incomplete
                , fBuffer
                , fMemoryManager);
        //"Imcomplete Time Format"

    // check (fixed) format first
    if ((fBuffer[fStart + 2] != TIME_SEPARATOR) ||
        (fBuffer[fStart + 5] != TIME_SEPARATOR)  )
    {
        ThrowXMLwithMemMgr1(SchemaDateTimeException
                , XMLExcepts::DateTime_time_invalid
                , fBuffer
                , fMemoryManager);
        //("Error in parsing time" );
    }

    //
    // get hours, minute and second
    //
    fValue[Hour]   = parseInt(fStart + 0, fStart + 2);
    fValue[Minute] = parseInt(fStart + 3, fStart + 5);
    fValue[Second] = parseInt(fStart + 6, fStart + 8);
    fStart += 8;

    // to see if any ms and/or utc part after that
    if (fStart >= fEnd)
        return;

    //find UTC sign if any
    int sign = findUTCSign(fStart);

    //parse miliseconds
    int milisec = (fBuffer[fStart] == MILISECOND_SEPARATOR)? fStart : NOT_FOUND;
    if ( milisec != NOT_FOUND )
    {
        fStart++;   // skip the '.'
        // make sure we have some thing between the '.' and fEnd
        if (fStart >= fEnd)
        {
            ThrowXMLwithMemMgr1(SchemaDateTimeException
                    , XMLExcepts::DateTime_ms_noDigit
                    , fBuffer
                    , fMemoryManager);
            //("ms shall be present once '.' is present" );
        }

        if ( sign == NOT_FOUND )
        {
            fValue[MiliSecond] = parseInt(fStart, fEnd);  //get ms between '.' and fEnd
            fStart = fEnd;
        }
        else
        {
            //to do: parseInt would eliminate any leading zeros
            //       therefore for 12:01:01.0034
            //       fValue[MiliSecond] would catch 34
            //       rather than 0034
            //
            fValue[MiliSecond] = parseInt(fStart, sign);  //get ms between UTC sign and fEnd
        }
	}
    else if(sign == 0 || sign != fStart)
    {
        // seconds has more than 2 digits
        ThrowXMLwithMemMgr1(SchemaDateTimeException
                , XMLExcepts::DateTime_min_invalid
                , fBuffer
                , fMemoryManager);
    }

    //parse UTC time zone (hh:mm)
    if ( sign > 0 ) {
        getTimeZone(sign);
    }

}

//
// [-]{CCYY-MM}
//
// Note: CCYY could be more than 4 digits
//       fStart updated to point AFTER the second 'M' (probably meet the fEnd)
//
void XMLDateTime::getYearMonth()
{

    // Ensure enough chars in buffer
    if ( (fStart+YMONTH_MIN_SIZE) > fEnd)
        ThrowXMLwithMemMgr1(SchemaDateTimeException
                , XMLExcepts::DateTime_ym_incomplete
                , fBuffer
                , fMemoryManager);
        //"Imcomplete YearMonth Format";

    // skip the first leading '-'
    int start = ( fBuffer[0] == chDash ) ? fStart + 1 : fStart;

    //
    // search for year separator '-'
    //
    int yearSeparator = indexOf(start, fEnd, DATE_SEPARATOR);
    if ( yearSeparator == NOT_FOUND)
        ThrowXMLwithMemMgr1(SchemaDateTimeException
                , XMLExcepts::DateTime_ym_invalid
                , fBuffer
                , fMemoryManager);
        //("Year separator is missing or misplaced");

    fValue[CentYear] = parseIntYear(yearSeparator);
    fStart = yearSeparator + 1;  //skip the '-' and point to the first M

    //
    //gonna check we have enough byte for month
    //
    if ((fStart + 2) > fEnd )
        ThrowXMLwithMemMgr1(SchemaDateTimeException
                , XMLExcepts::DateTime_ym_noMonth
                , fBuffer
                , fMemoryManager);
        //"no month in buffer"

    fValue[Month] = parseInt(fStart, yearSeparator + 3);
    fStart += 2;  //fStart points right after the MONTH

    return;
}

void XMLDateTime::parseTimeZone()
{
    if ( fStart < fEnd )
    {
        int sign = findUTCSign(fStart);
        if ( sign < 0 )
        {
            ThrowXMLwithMemMgr1(SchemaDateTimeException
                    , XMLExcepts::DateTime_tz_noUTCsign
                    , fBuffer
                    , fMemoryManager);
            //("Error in month parsing");
        }
        else
        {
            getTimeZone(sign);
        }
    }

    return;
}

//
// 'Z'
// ['+'|'-']hh:mm
//
// Note: Assuming fStart points to the beginning of TimeZone section
//       fStart updated to meet fEnd
//
void XMLDateTime::getTimeZone(const int sign)
{

    if ( fBuffer[sign] == UTC_STD_CHAR )
    {
        if ((sign + 1) != fEnd )
        {
            ThrowXMLwithMemMgr1(SchemaDateTimeException
                    , XMLExcepts::DateTime_tz_stuffAfterZ
                    , fBuffer
                    , fMemoryManager);
            //"Error in parsing time zone");
        }		

        return;	
    }

    //
    // otherwise, it has to be this format
    // '[+|-]'hh:mm
    //    1   23456 7
    //   sign      fEnd
    //
    if ( ( ( sign + TIMEZONE_SIZE + 1) != fEnd )      ||
         ( fBuffer[sign + 3] != TIMEZONE_SEPARATOR ) )
    {
        ThrowXMLwithMemMgr1(SchemaDateTimeException
                , XMLExcepts::DateTime_tz_invalid
                , fBuffer
                , fMemoryManager);
        //("Error in parsing time zone");
    }

    fTimeZone[hh] = parseInt(sign+1, sign+3);		
    fTimeZone[mm] = parseInt(sign+4, fEnd);
        		
    return;
}

// ---------------------------------------------------------------------------
//  Validator and normalizer
// ---------------------------------------------------------------------------

/**
 * If timezone present - normalize dateTime  [E Adding durations to dateTimes]
 *
 * @param date   CCYY-MM-DDThh:mm:ss+03
 * @return CCYY-MM-DDThh:mm:ssZ
 */
void XMLDateTime::normalize()
{

    if ((fValue[utc] == UTC_UNKNOWN) ||
        (fValue[utc] == UTC_STD)      )
        return;

    int negate = (fValue[utc] == UTC_POS)? -1: 1;

    // add mins
    int temp = fValue[Minute] + negate * fTimeZone[mm];
    int carry = fQuotient(temp, 60);
    fValue[Minute] = mod(temp, 60, carry);

    //add hours
    temp = fValue[Hour] + negate * fTimeZone[hh] + carry;
    carry = fQuotient(temp, 24);
    fValue[Hour] = mod(temp, 24, carry);

    fValue[Day] += carry;

    while (1)
    {
        temp = maxDayInMonthFor(fValue[CentYear], fValue[Month]);
        if (fValue[Day] < 1)
        {
            fValue[Day] += maxDayInMonthFor(fValue[CentYear], fValue[Month] - 1);
            carry = -1;
        }
        else if ( fValue[Day] > temp )
        {
            fValue[Day] -= temp;
            carry = 1;
        }
        else
        {
            break;
        }

        temp = fValue[Month] + carry;
        fValue[Month] = modulo(temp, 1, 13);
        fValue[CentYear] += fQuotient(temp, 1, 13);
    }

    // set to normalized
    fValue[utc] = UTC_STD;

    return;
}

void XMLDateTime::validateDateTime() const
{

    //REVISIT: should we throw an exception for not valid dates
    //          or reporting an error message should be sufficient?
    if ( fValue[CentYear] == 0 )
    {
        ThrowXMLwithMemMgr1(SchemaDateTimeException
                , XMLExcepts::DateTime_year_zero
                , fBuffer
                , fMemoryManager);
        //"The year \"0000\" is an illegal year value");
    }

    if ( fValue[Month] < 1  ||
         fValue[Month] > 12  )
    {
        ThrowXMLwithMemMgr1(SchemaDateTimeException
                , XMLExcepts::DateTime_mth_invalid
                , fBuffer
                , fMemoryManager);
		//"The month must have values 1 to 12");
    }

    //validate days
    if ( fValue[Day] > maxDayInMonthFor( fValue[CentYear], fValue[Month]) ||
         fValue[Day] == 0 )
    {
        ThrowXMLwithMemMgr1(SchemaDateTimeException
                , XMLExcepts::DateTime_day_invalid
                , fBuffer
                , fMemoryManager);
        //"The day must have values 1 to 31");
    }

    //validate hours
    if ((fValue[Hour] < 0)  ||
        (fValue[Hour] > 24) ||
        ((fValue[Hour] == 24) && ((fValue[Minute] !=0) ||
                                  (fValue[Second] !=0) ||
                                  (fValue[MiliSecond] !=0))))
    {
        ThrowXMLwithMemMgr1(SchemaDateTimeException
                , XMLExcepts::DateTime_hour_invalid
                , fBuffer
                , fMemoryManager);
        //("Hour must have values 0-23");
    }

    //validate minutes
    if ( fValue[Minute] < 0 ||
         fValue[Minute] > 59 )
    {
        ThrowXMLwithMemMgr1(SchemaDateTimeException
                , XMLExcepts::DateTime_min_invalid
                , fBuffer
                , fMemoryManager);
        //"Minute must have values 0-59");
    }

    //validate seconds
    if ( fValue[Second] < 0 ||
         fValue[Second] > 60 )
    {
        ThrowXMLwithMemMgr1(SchemaDateTimeException
                , XMLExcepts::DateTime_second_invalid
                , fBuffer
                , fMemoryManager);
        //"Second must have values 0-60");
    }

    //validate time-zone hours
    if ( (abs(fTimeZone[hh]) > 14) ||
         ((abs(fTimeZone[hh]) == 14) && (fTimeZone[mm] != 0)) )
    {
        ThrowXMLwithMemMgr1(SchemaDateTimeException
                , XMLExcepts::DateTime_tz_hh_invalid
                , fBuffer
                , fMemoryManager);
        //"Time zone should have range -14..+14");
    }

    //validate time-zone minutes
    if ( abs(fTimeZone[mm]) > 59 )
    {
        ThrowXMLwithMemMgr1(SchemaDateTimeException
                , XMLExcepts::DateTime_min_invalid
                , fBuffer
                , fMemoryManager);
        //("Minute must have values 0-59");
    }
	
    return;
}

// -----------------------------------------------------------------------
// locator and converter
// -----------------------------------------------------------------------
int XMLDateTime::indexOf(const int start, const int end, const XMLCh ch) const
{
    for ( int i = start; i < end; i++ )
        if ( fBuffer[i] == ch )
            return i;

    return NOT_FOUND;
}

int XMLDateTime::findUTCSign (const int start)
{
    int  pos;
    for ( int index = start; index < fEnd; index++ )
    {
        pos = XMLString::indexOf(UTC_SET, fBuffer[index]);
        if ( pos != NOT_FOUND)
        {
            fValue[utc] = pos+1;   // refer to utcType, there is 1 diff
            return index;
        }
    }

    return NOT_FOUND;
}

//
// Note:
//    start: starting point in fBuffer
//    end:   ending point in fBuffer (exclusive)
//    fStart NOT updated
//
int XMLDateTime::parseInt(const int start, const int end) const
{
    unsigned int retVal = 0;
    for (int i=start; i < end; i++) {

        if (fBuffer[i] < chDigit_0 || fBuffer[i] > chDigit_9)
            ThrowXMLwithMemMgr(NumberFormatException, XMLExcepts::XMLNUM_Inv_chars, fMemoryManager);

        retVal = (retVal * 10) + (unsigned int) (fBuffer[i] - chDigit_0);
    }

    return (int) retVal;
}

//
// [-]CCYY
//
// Note: start from fStart
//       end (exclusive)
//       fStart NOT updated
//
int XMLDateTime::parseIntYear(const int end) const
{
    // skip the first leading '-'
    int start = ( fBuffer[0] == chDash ) ? fStart + 1 : fStart;

    int length = end - start;
    if (length < 4)
    {
        ThrowXMLwithMemMgr1(SchemaDateTimeException
                , XMLExcepts::DateTime_year_tooShort
                , fBuffer
                , fMemoryManager);
        //"Year must have 'CCYY' format");
    }
    else if (length > 4 &&
             fBuffer[start] == chDigit_0)
    {
        ThrowXMLwithMemMgr1(SchemaDateTimeException
                , XMLExcepts::DateTime_year_leadingZero
                , fBuffer
                , fMemoryManager);
        //"Leading zeros are required if the year value would otherwise have fewer than four digits;
        // otherwise they are forbidden");
    }

    bool negative = (fBuffer[0] == chDash);
    int  yearVal = parseInt((negative ? 1 : 0), end);
    return ( negative ? (-1) * yearVal : yearVal );
}

/***
 * E2-41
 *
 *  3.2.7.2 Canonical representation
 * 
 *  Except for trailing fractional zero digits in the seconds representation, 
 *  '24:00:00' time representations, and timezone (for timezoned values), 
 *  the mapping from literals to values is one-to-one. Where there is more 
 *  than one possible representation, the canonical representation is as follows: 
 *  redundant trailing zero digits in fractional-second literals are prohibited. 
 *  An hour representation of '24' is prohibited. Timezoned values are canonically
 *  represented by appending 'Z' to the nontimezoned representation. (All 
 *  timezoned dateTime values are UTC.) 
 *
 *  .'24:00:00' -> '00:00:00'
 *  .milisecond: trailing zeros removed
 *  .'Z'
 *
 ***/
XMLCh* XMLDateTime::getDateTimeCanonicalRepresentation(MemoryManager* const memMgr) const
{
    XMLCh *miliStartPtr, *miliEndPtr;
    searchMiliSeconds(miliStartPtr, miliEndPtr);
    int miliSecondsLen = miliEndPtr - miliStartPtr;

    MemoryManager* toUse = memMgr? memMgr : fMemoryManager;
    XMLCh* retBuf = (XMLCh*) toUse->allocate( (21 + miliSecondsLen + 2) * sizeof(XMLCh));
    XMLCh* retPtr = retBuf;

    // (-?) cc+yy-mm-dd'T'hh:mm:ss'Z'    ('.'s+)?
    //      2+  8       1      8   1
    //
    int additionalLen = fillYearString(retPtr, CentYear);
    if(additionalLen != 0)
    {
        // very bad luck; have to resize the buffer...
        XMLCh *tmpBuf = (XMLCh*) toUse->allocate( (additionalLen+21+miliSecondsLen +2) * sizeof(XMLCh));
        XMLString::moveChars(tmpBuf, retBuf, 4+additionalLen);
        retPtr = tmpBuf+(retPtr-retBuf);
        toUse->deallocate(retBuf);
        retBuf = tmpBuf;
    }
    *retPtr++ = DATE_SEPARATOR;
    fillString(retPtr, Month, 2);
    *retPtr++ = DATE_SEPARATOR;
    fillString(retPtr, Day, 2);
    *retPtr++ = DATETIME_SEPARATOR;

    fillString(retPtr, Hour, 2);
    if (fValue[Hour] == 24)
    {
        *(retPtr - 2) = chDigit_0;
        *(retPtr - 1) = chDigit_0;
    }
    *retPtr++ = TIME_SEPARATOR;
    fillString(retPtr, Minute, 2);
    *retPtr++ = TIME_SEPARATOR;
    fillString(retPtr, Second, 2);

    if (miliSecondsLen)
    {
        *retPtr++ = chPeriod;
        XMLString::copyNString(retPtr, miliStartPtr, miliSecondsLen);
        retPtr += miliSecondsLen;
    }

    *retPtr++ = UTC_STD_CHAR;
    *retPtr = chNull;

    return retBuf;
}

/***
 * 3.2.8 time
 *
 *  . either the time zone must be omitted or, 
 *    if present, the time zone must be Coordinated Universal Time (UTC) indicated by a "Z".   
 *
 *  . Additionally, the canonical representation for midnight is 00:00:00.
 *
***/
XMLCh* XMLDateTime::getTimeCanonicalRepresentation(MemoryManager* const memMgr) const
{
    XMLCh *miliStartPtr, *miliEndPtr;
    searchMiliSeconds(miliStartPtr, miliEndPtr);
    int miliSecondsLen = miliEndPtr - miliStartPtr;

    MemoryManager* toUse = memMgr? memMgr : fMemoryManager;
    XMLCh* retBuf = (XMLCh*) toUse->allocate( (10 + miliSecondsLen + 2) * sizeof(XMLCh));
    XMLCh* retPtr = retBuf;

    // 'hh:mm:ss'Z'    ('.'s+)?
    //      8    1
    //

    fillString(retPtr, Hour, 2);
    if (fValue[Hour] == 24)
    {
        *(retPtr - 2) = chDigit_0;
        *(retPtr - 1) = chDigit_0;
    }
    *retPtr++ = TIME_SEPARATOR;
    fillString(retPtr, Minute, 2);
    *retPtr++ = TIME_SEPARATOR;
    fillString(retPtr, Second, 2);

    if (miliSecondsLen)
    {
        *retPtr++ = chPeriod;
        XMLString::copyNString(retPtr, miliStartPtr, miliSecondsLen);
        retPtr += miliSecondsLen;
    }

    *retPtr++ = UTC_STD_CHAR;
    *retPtr = chNull;

    return retBuf;
}

void XMLDateTime::fillString(XMLCh*& ptr, valueIndex ind, int expLen) const
{
    XMLCh strBuffer[16];
    assert(expLen < 16);
    XMLString::binToText(fValue[ind], strBuffer, expLen, 10, fMemoryManager);
    int   actualLen = XMLString::stringLen(strBuffer);
    int   i;
    //append leading zeros
    for (i = 0; i < expLen - actualLen; i++)
    {
        *ptr++ = chDigit_0;
    }

    for (i = 0; i < actualLen; i++)
    {
        *ptr++ = strBuffer[i];
    }

}

int XMLDateTime::fillYearString(XMLCh*& ptr, valueIndex ind) const
{
    XMLCh strBuffer[16];
    // let's hope we get no years of 15 digits...
    XMLString::binToText(fValue[ind], strBuffer, 15, 10, fMemoryManager);
    int   actualLen = XMLString::stringLen(strBuffer);
    // don't forget that years can be negative...
    int negativeYear = 0;
    if(strBuffer[0] == chDash)
    {
        *ptr++ = strBuffer[0];
        negativeYear = 1;
    }
    int   i;
    //append leading zeros
    for (i = 0; i < 4 - actualLen+negativeYear; i++)
    {
        *ptr++ = chDigit_0;
    }

    for (i = negativeYear; i < actualLen; i++)
    {
        *ptr++ = strBuffer[i];
    }
    if(actualLen > 4)
        return actualLen-4;
    return 0;
}

/***
 *
 *   .check if the rawData has the mili second component
 *   .capture the substring
 *
 ***/
void XMLDateTime::searchMiliSeconds(XMLCh*& miliStartPtr, XMLCh*& miliEndPtr) const
{
    miliStartPtr = miliEndPtr = 0;

    int milisec = XMLString::indexOf(fBuffer, MILISECOND_SEPARATOR);
    if (milisec == -1)
        return;

    miliStartPtr = fBuffer + milisec + 1;
    miliEndPtr   = miliStartPtr;
    while (*miliEndPtr)
    {
        if ((*miliEndPtr < chDigit_0) || (*miliEndPtr > chDigit_9))
            break;

        miliEndPtr++;
    }

    //remove trailing zeros
    while( *(miliEndPtr - 1) == chDigit_0)
        miliEndPtr--;

    return;
}

/***
 * Support for Serialization/De-serialization
 ***/

IMPL_XSERIALIZABLE_TOCREATE(XMLDateTime)

void XMLDateTime::serialize(XSerializeEngine& serEng)
{
    //REVISIT: may not need to call base since it does nothing
    XMLNumber::serialize(serEng);

    int i = 0;

    if (serEng.isStoring())
    {
        for (i = 0; i < TOTAL_SIZE; i++)
        {
            serEng<<fValue[i];
        }

        for (i = 0; i < TIMEZONE_ARRAYSIZE; i++)
        {
            serEng<<fTimeZone[i];
        }

        serEng<<fStart;
        serEng<<fEnd;

        serEng.writeString(fBuffer, fBufferMaxLen, XSerializeEngine::toWriteBufferLen);
    }
    else
    {
        for (i = 0; i < TOTAL_SIZE; i++)
        {
            serEng>>fValue[i];
        }

        for (i = 0; i < TIMEZONE_ARRAYSIZE; i++)
        {
            serEng>>fTimeZone[i];
        }

        serEng>>fStart;
        serEng>>fEnd;

        int dataLen = 0;
        serEng.readString(fBuffer, fBufferMaxLen, dataLen ,XSerializeEngine::toReadBufferLen);

    }

}

XERCES_CPP_NAMESPACE_END
