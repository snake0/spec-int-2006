/*
 * The Apache Software License, Version 1.1
 *
 * Copyright (c) 2001-2002 The Apache Software Foundation.  All rights
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
 * $Log: UnicodeRangeFactory.cpp,v $
 * Revision 1.4  2003/12/17 00:18:37  cargilld
 * Update to memory management so that the static memory manager (one used to call Initialize) is only for static data.
 *
 * Revision 1.3  2002/11/04 15:17:01  tng
 * C++ Namespace Support.
 *
 * Revision 1.2  2002/02/05 13:20:06  tng
 * [Bug 5716] Can't parse with Validation more than one file.
 *
 * Revision 1.1.1.1  2002/02/01 22:22:34  peiyongz
 * sane_include
 *
 * Revision 1.4  2001/10/15 18:30:40  knoaman
 * Add support for 'Pi' and 'Pf'.
 *
 * Revision 1.3  2001/05/11 13:26:51  tng
 * Copyright update.
 *
 * Revision 1.2  2001/05/03 18:17:56  knoaman
 * Some design changes:
 * o Changed the TokenFactory from a single static instance, to a
 *    normal class. Each RegularExpression object will have its own
 *    instance of TokenFactory, and that instance will be passed to
 *    other classes that need to use a TokenFactory to create Token
 *    objects (with the exception of RangeTokenMap).
 * o Added a new class RangeTokenMap to map a the different ranges
 *    in a given category to a specific RangeFactory object. In the old
 *    design RangeFactory had dual functionality (act as a Map, and as
 *    a factory for creating RangeToken(s)). The RangeTokenMap will
 *    have its own copy of the TokenFactory. There will be only one
 *    instance of the RangeTokenMap class, and that instance will be
 *    lazily deleted when XPlatformUtils::Terminate is called.
 *
 * Revision 1.1  2001/03/02 19:26:48  knoaman
 * Schema: Regular expression handling part II
 *
 */

// ---------------------------------------------------------------------------
//  Includes
// ---------------------------------------------------------------------------
#include <xercesc/util/regx/UnicodeRangeFactory.hpp>
#include <xercesc/util/regx/TokenFactory.hpp>
#include <xercesc/util/regx/RangeToken.hpp>
#include <xercesc/util/regx/RangeTokenMap.hpp>
#include <xercesc/util/regx/RegxDefs.hpp>
#include <xercesc/util/regx/XMLUniCharacter.hpp>

XERCES_CPP_NAMESPACE_BEGIN

// ---------------------------------------------------------------------------
//  Local data
// ---------------------------------------------------------------------------
const int            UNICATEGSIZE     = 37;
const unsigned short CHAR_LETTER      = 30;
const unsigned short CHAR_MARK        = 31;
const unsigned short CHAR_NUMBER      = 32;
const unsigned short CHAR_SEPARATOR   = 33;
const unsigned short CHAR_OTHER       = 34;
const unsigned short CHAR_PUNCTUATION = 35;
const unsigned short CHAR_SYMBOL      = 36;

const XMLCh uniCategNames[][UNICATEGSIZE] =
{
    {chLatin_C, chLatin_n, chNull},
    {chLatin_L, chLatin_u, chNull},
    {chLatin_L, chLatin_l, chNull},
    {chLatin_L, chLatin_t, chNull},
    {chLatin_L, chLatin_m, chNull},
    {chLatin_L, chLatin_o, chNull},
    {chLatin_M, chLatin_n, chNull},
    {chLatin_M, chLatin_e, chNull},
    {chLatin_M, chLatin_c, chNull},
    {chLatin_N, chLatin_d, chNull},
    {chLatin_N, chLatin_l, chNull},
    {chLatin_N, chLatin_o, chNull},
    {chLatin_Z, chLatin_s, chNull},
    {chLatin_Z, chLatin_l, chNull},
    {chLatin_Z, chLatin_p, chNull},
    {chLatin_C, chLatin_c, chNull},
    {chLatin_C, chLatin_f, chNull},
    {chLatin_C, chLatin_o, chNull},
    {chLatin_C, chLatin_s, chNull},
    {chLatin_P, chLatin_d, chNull},
    {chLatin_P, chLatin_s, chNull},
    {chLatin_P, chLatin_e, chNull},
    {chLatin_P, chLatin_c, chNull},
    {chLatin_P, chLatin_o, chNull},
    {chLatin_S, chLatin_m, chNull},
    {chLatin_S, chLatin_c, chNull},
    {chLatin_S, chLatin_k, chNull},
    {chLatin_S, chLatin_o, chNull},
    {chLatin_P, chLatin_i, chNull},
    {chLatin_P, chLatin_f, chNull},
    {chLatin_L, chNull},
    {chLatin_M, chNull},
    {chLatin_N, chNull},
    {chLatin_Z, chNull},
    {chLatin_C, chNull},
    {chLatin_P, chNull},
    {chLatin_S, chNull},
};

// ---------------------------------------------------------------------------
//  UnicodeRangeFactory: Constructors and Destructor
// ---------------------------------------------------------------------------
UnicodeRangeFactory::UnicodeRangeFactory() :
   fRangesCreated(false)
 , fKeywordsInitialized(false)
{
}

UnicodeRangeFactory::~UnicodeRangeFactory() {

}

// ---------------------------------------------------------------------------
//  UnicodeRangeFactory: Range creation methods
// ---------------------------------------------------------------------------
void UnicodeRangeFactory::buildRanges() {

    if (fRangesCreated)
        return;

    if (!fKeywordsInitialized) {
        initializeKeywordMap();
    }

    RangeTokenMap* rangeTokMap = RangeTokenMap::instance();
    TokenFactory* tokFactory = rangeTokMap->getTokenFactory();
	RangeToken* ranges[UNICATEGSIZE];

    for (int i=0; i < UNICATEGSIZE; i++) {
        ranges[i] = tokFactory->createRange();
    }

    for (int j=0; j < 0x10000; j++) {

        unsigned short charType = XMLUniCharacter::getType(j);

		ranges[charType]->addRange(j, j);
		charType = getUniCategory(charType);
		ranges[charType]->addRange(j, j);
    }

	ranges[XMLUniCharacter::UNASSIGNED]->addRange(0x10000, Token::UTF16_MAX);

	for (int k=0; k < UNICATEGSIZE; k++) {
        rangeTokMap->setRangeToken(uniCategNames[k], ranges[k]);
    }

    // Create all range
	RangeToken* tok = tokFactory->createRange();
	tok->addRange(0, Token::UTF16_MAX);
	rangeTokMap->setRangeToken(fgUniAll, tok);

    // Create alpha range
    tok = tokFactory->createRange();
    tok->mergeRanges(ranges[XMLUniCharacter::UPPERCASE_LETTER]);
    tok->mergeRanges(ranges[XMLUniCharacter::LOWERCASE_LETTER]);
    tok->mergeRanges(ranges[XMLUniCharacter::OTHER_LETTER]);
    rangeTokMap->setRangeToken(fgUniIsAlpha, tok);

    // Create alpha-num range
    RangeToken* alnumTok = tokFactory->createRange();
    alnumTok->mergeRanges(tok);
    alnumTok->mergeRanges(ranges[XMLUniCharacter::DECIMAL_DIGIT_NUMBER]);
    rangeTokMap->setRangeToken(fgUniIsAlnum, alnumTok);

    // Create word range
    tok = tokFactory->createRange();
    tok->mergeRanges(alnumTok);
    tok->addRange(chUnderscore, chUnderscore);
    rangeTokMap->setRangeToken(fgUniIsWord, tok);

    // Create assigned range
    tok = ranges[XMLUniCharacter::UNASSIGNED];
    rangeTokMap->setRangeToken(fgUniAssigned,(RangeToken*)RangeToken::complementRanges(tok,
		          tokFactory, tokFactory->getMemoryManager()));

    fRangesCreated = true;
}

// ---------------------------------------------------------------------------
//  UnicodeRangeFactory: Initialization methods
// ---------------------------------------------------------------------------
void UnicodeRangeFactory::initializeKeywordMap() {

    if (fKeywordsInitialized)
        return;

    RangeTokenMap* rangeTokMap = RangeTokenMap::instance();

	for (int k=0; k < UNICATEGSIZE; k++) {
        rangeTokMap->addKeywordMap(uniCategNames[k], fgUnicodeCategory);
    }

	rangeTokMap->addKeywordMap(fgUniAll, fgUnicodeCategory);
    rangeTokMap->addKeywordMap(fgUniIsAlpha, fgUnicodeCategory);
    rangeTokMap->addKeywordMap(fgUniIsAlnum, fgUnicodeCategory);
    rangeTokMap->addKeywordMap(fgUniIsWord, fgUnicodeCategory);
    rangeTokMap->addKeywordMap(fgUniAssigned, fgUnicodeCategory);

    fKeywordsInitialized = true;
}

// ---------------------------------------------------------------------------
//  UnicodeRangeFactory: Helper methods
// ---------------------------------------------------------------------------
unsigned short UnicodeRangeFactory::getUniCategory(const unsigned short type)
{

    switch(type) {
    case XMLUniCharacter::UPPERCASE_LETTER:
    case XMLUniCharacter::LOWERCASE_LETTER:
    case XMLUniCharacter::TITLECASE_LETTER:
    case XMLUniCharacter::MODIFIER_LETTER:
    case XMLUniCharacter::OTHER_LETTER:
        return CHAR_LETTER;
    case XMLUniCharacter::NON_SPACING_MARK:
    case XMLUniCharacter::COMBINING_SPACING_MARK:
    case XMLUniCharacter::ENCLOSING_MARK:
        return CHAR_MARK;
    case XMLUniCharacter::DECIMAL_DIGIT_NUMBER:
    case XMLUniCharacter::LETTER_NUMBER:
    case XMLUniCharacter::OTHER_NUMBER:
		return CHAR_NUMBER;
    case XMLUniCharacter::SPACE_SEPARATOR:
    case XMLUniCharacter::LINE_SEPARATOR:
    case XMLUniCharacter::PARAGRAPH_SEPARATOR:
		return CHAR_SEPARATOR;
    case XMLUniCharacter::CONTROL:
    case XMLUniCharacter::FORMAT:
    case XMLUniCharacter::SURROGATE:
    case XMLUniCharacter::PRIVATE_USE:
    case XMLUniCharacter::UNASSIGNED:
		return CHAR_OTHER;
    case XMLUniCharacter::CONNECTOR_PUNCTUATION:
    case XMLUniCharacter::DASH_PUNCTUATION:
    case XMLUniCharacter::START_PUNCTUATION:
    case XMLUniCharacter::END_PUNCTUATION:
    case XMLUniCharacter::OTHER_PUNCTUATION:
    case XMLUniCharacter::INITIAL_PUNCTUATION:
    case XMLUniCharacter::FINAL_PUNCTUATION:
		return CHAR_PUNCTUATION;
    case XMLUniCharacter::MATH_SYMBOL:
    case XMLUniCharacter::CURRENCY_SYMBOL:
    case XMLUniCharacter::MODIFIER_SYMBOL:
    case XMLUniCharacter::OTHER_SYMBOL:
		return CHAR_SYMBOL;
    }

    return 0;
}

XERCES_CPP_NAMESPACE_END

/**
  * End of file UnicodeRangeFactory.cpp
  */
