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
 * $Log: ParserForXMLSchema.cpp,v $
 * Revision 1.8  2004/01/29 11:51:21  cargilld
 * Code cleanup changes to get rid of various compiler diagnostic messages.
 *
 * Revision 1.7  2003/12/17 00:18:37  cargilld
 * Update to memory management so that the static memory manager (one used to call Initialize) is only for static data.
 *
 * Revision 1.6  2003/05/15 18:42:54  knoaman
 * Partial implementation of the configurable memory manager.
 *
 * Revision 1.5  2003/03/18 19:38:28  knoaman
 * Schema Errata E2-18 + misc. regex fixes.
 *
 * Revision 1.4  2003/01/13 19:02:23  knoaman
 * [Bug 14390] C++ Indentifier collision with Python.
 *
 * Revision 1.3  2002/11/04 15:17:00  tng
 * C++ Namespace Support.
 *
 * Revision 1.2  2002/03/18 19:29:53  knoaman
 * Change constant names to eliminate possible conflict with user defined ones.
 *
 * Revision 1.1.1.1  2002/02/01 22:22:29  peiyongz
 * sane_include
 *
 * Revision 1.6  2001/09/20 13:11:42  knoaman
 * Regx  + misc. fixes
 *
 * Revision 1.5  2001/06/01 14:15:37  knoaman
 * Add a return value to satisfy compilers that complain about
 * no return value, although that code will not be executed.
 *
 * Revision 1.4  2001/05/11 21:50:56  knoaman
 * Schema updates and fixes.
 *
 * Revision 1.3  2001/05/11 13:26:44  tng
 * Copyright update.
 *
 * Revision 1.2  2001/05/03 18:17:30  knoaman
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
 * Revision 1.1  2001/03/02 19:26:43  knoaman
 * Schema: Regular expression handling part II
 *
 */

// ---------------------------------------------------------------------------
//  Includes
// ---------------------------------------------------------------------------
#include <xercesc/util/regx/ParserForXMLSchema.hpp>
#include <xercesc/util/regx/TokenFactory.hpp>
#include <xercesc/util/regx/RangeToken.hpp>
#include <xercesc/util/regx/TokenInc.hpp>
#include <xercesc/util/regx/RegxDefs.hpp>
#include <xercesc/util/ParseException.hpp>
#include <xercesc/util/RuntimeException.hpp>
#include <xercesc/util/PlatformUtils.hpp>

XERCES_CPP_NAMESPACE_BEGIN

// ---------------------------------------------------------------------------
//  ParserForXMLSchema: Constructors and Destructors
// ---------------------------------------------------------------------------
ParserForXMLSchema::ParserForXMLSchema(MemoryManager* const manager)
    : RegxParser(manager)
{

}

ParserForXMLSchema::~ParserForXMLSchema() {

}

// ---------------------------------------------------------------------------
//  ParserForXMLSchema: Parsing/Processing methods
// ---------------------------------------------------------------------------
Token* ParserForXMLSchema::processCaret() {

    processNext();
    return getTokenFactory()->createChar(chCaret);
}

Token* ParserForXMLSchema::processDollar() {

    processNext();
    return getTokenFactory()->createChar(chDollarSign);
}

Token* ParserForXMLSchema::processPlus(Token* const tok) {

    processNext();
    return getTokenFactory()->createConcat(tok,
                               getTokenFactory()->createClosure(tok));
}

Token* ParserForXMLSchema::processStar(Token* const tok) {

    processNext();
    return getTokenFactory()->createClosure(tok);
}

Token* ParserForXMLSchema::processQuestion(Token* const tok) {

    processNext();

    TokenFactory* tokFactory = getTokenFactory();
    Token* retTok = tokFactory->createUnion();
    retTok->addChild(tok, tokFactory);
    retTok->addChild(tokFactory->createToken(Token::T_EMPTY), tokFactory);
    return retTok;
}

Token* ParserForXMLSchema::processParen() {

    processNext();
    Token* retTok = getTokenFactory()->createParenthesis(parseRegx(true), 0);

    if (getState() != REGX_T_RPAREN) {
        ThrowXMLwithMemMgr(ParseException, XMLExcepts::Parser_Factor1, getMemoryManager());
    }

    processNext();
    return retTok;
}

RangeToken* ParserForXMLSchema::parseCharacterClass(const bool) {

    setParseContext(S_INBRACKETS);
    processNext();

    RangeToken* base = 0;
    RangeToken* tok = 0;
    bool isNRange = false;

    if (getState() == REGX_T_CHAR && getCharData() == chCaret) {

        isNRange = true;
        processNext();
        base = getTokenFactory()->createRange();
        base->addRange(0, Token::UTF16_MAX);
        tok = getTokenFactory()->createRange();
    }
    else {
        tok= getTokenFactory()->createRange();
    }

    int type;
    bool firstLoop = true;

    while ( (type = getState()) != REGX_T_EOF) {

        // single range | from-to-range | subtraction
        if (type == REGX_T_CHAR && getCharData() == chCloseSquare && !firstLoop) {

            if (isNRange) {

                base->subtractRanges(tok);
                tok = base;
            }
            break;
        }

        XMLInt32 ch = getCharData();
        bool     end = false;

        if (type == REGX_T_BACKSOLIDUS) {

            switch(ch) {
            case chLatin_d:
            case chLatin_D:
            case chLatin_w:
            case chLatin_W:
            case chLatin_s:
            case chLatin_S:
                {
                    tok->mergeRanges(getTokenForShorthand(ch));
                    end = true;
                }
                break;
            case chLatin_i:
            case chLatin_I:
            case chLatin_c:
            case chLatin_C:
                {
                    ch = processCInCharacterClass(tok, ch);
                    if (ch < 0) {
                        end = true;
                    }
                }
                break;
            case chLatin_p:
            case chLatin_P:
                {                    
                    RangeToken* tok2 = processBacksolidus_pP(ch);

                    if (tok2 == 0) {
                        ThrowXMLwithMemMgr(ParseException,XMLExcepts::Parser_Atom5, getMemoryManager());
                    }

                    tok->mergeRanges(tok2);
                    end = true;
                }
                break;
            default:
                ch = decodeEscaped();
            }
        } // end if REGX_T_BACKSOLIDUS
        else if (type == REGX_T_XMLSCHEMA_CC_SUBTRACTION && !firstLoop) {

            if (isNRange) {

                base->subtractRanges(tok);
                tok = base;
            }

            RangeToken* rangeTok = parseCharacterClass(false);
            tok->subtractRanges(rangeTok);

            if (getState() != REGX_T_CHAR || getCharData() != chCloseSquare) {
                ThrowXMLwithMemMgr(ParseException,XMLExcepts::Parser_CC5, getMemoryManager());
            }
            break;
        } // end if REGX_T_XMLSCHEMA...

        processNext();

        if (!end) {

            if (type == REGX_T_CHAR
                && (ch == chOpenSquare
                    || ch == chCloseSquare
                    || ch == chDash)) {
                // '[', ']', '-' not allowed and should be esacaped
                XMLCh chStr[] = { ch, chNull };
                ThrowXMLwithMemMgr2(ParseException,XMLExcepts::Parser_CC6, chStr, chStr, getMemoryManager());
            }

            if (getState() != REGX_T_CHAR || getCharData() != chDash) {
                tok->addRange(ch, ch);
            }
            else {

                processNext();
                if ((type = getState()) == REGX_T_EOF)
                    ThrowXMLwithMemMgr(ParseException,XMLExcepts::Parser_CC2, getMemoryManager());

                if ((type == REGX_T_CHAR && getCharData() == chCloseSquare)
                    || type == REGX_T_XMLSCHEMA_CC_SUBTRACTION) {

                    static const XMLCh dashStr[] = { chDash, chNull};
                    ThrowXMLwithMemMgr2(ParseException, XMLExcepts::Parser_CC6, dashStr, dashStr, getMemoryManager());
                }
                else {

                    XMLInt32 rangeEnd = getCharData();
                    XMLCh rangeEndStr[] = { rangeEnd, chNull };

                    if (type == REGX_T_CHAR) {

                        if (rangeEnd == chOpenSquare
                            || rangeEnd == chCloseSquare
                            || rangeEnd == chDash)
                            // '[', ']', '-' not allowed and should be esacaped
                            ThrowXMLwithMemMgr2(ParseException, XMLExcepts::Parser_CC6, rangeEndStr, rangeEndStr, getMemoryManager());
                    }
                    else if (type == REGX_T_BACKSOLIDUS) {
                        rangeEnd = decodeEscaped();
                    }

                    processNext();

                    if (ch > rangeEnd) {
                        XMLCh chStr[] = { ch, chNull };
                        ThrowXMLwithMemMgr2(ParseException,XMLExcepts::Parser_Ope3, rangeEndStr, chStr, getMemoryManager());
                    }

                    tok->addRange(ch, rangeEnd);
                }
            }
        }
        firstLoop = false;
    }

    if (getState() == REGX_T_EOF)
        ThrowXMLwithMemMgr(ParseException,XMLExcepts::Parser_CC2, getMemoryManager());

    tok->sortRanges();
    tok->compactRanges();
    setParseContext(S_NORMAL);
    processNext();

    return tok;
}

XMLInt32 ParserForXMLSchema::processCInCharacterClass(RangeToken* const tok,
                                                      const XMLInt32 ch)
{
    tok->mergeRanges(getTokenForShorthand(ch));
    return -1;
}

Token* ParserForXMLSchema::processLook(const unsigned short) {

    ThrowXMLwithMemMgr(RuntimeException, XMLExcepts::Regex_NotSupported, getMemoryManager());
    return 0; // for compilers that complain about no return value
}

Token* ParserForXMLSchema::processBacksolidus_A() {

    ThrowXMLwithMemMgr(RuntimeException, XMLExcepts::Regex_NotSupported, getMemoryManager());
    return 0; // for compilers that complain about no return value
}

Token* ParserForXMLSchema::processBacksolidus_B() {

    ThrowXMLwithMemMgr(RuntimeException, XMLExcepts::Regex_NotSupported, getMemoryManager());
    return 0; // for compilers that complain about no return value
}

Token* ParserForXMLSchema::processBacksolidus_b() {

    ThrowXMLwithMemMgr(RuntimeException, XMLExcepts::Regex_NotSupported, getMemoryManager());
    return 0; // for compilers that complain about no return value
}

Token* ParserForXMLSchema::processBacksolidus_C() {

    processNext();
    return getTokenForShorthand(chLatin_C);
}

Token* ParserForXMLSchema::processBacksolidus_c() {

    processNext();
    return getTokenForShorthand(chLatin_c);
}

Token* ParserForXMLSchema::processBacksolidus_g() {

    ThrowXMLwithMemMgr(RuntimeException, XMLExcepts::Regex_NotSupported, getMemoryManager());
    return 0; // for compilers that complain about no return value
}

Token* ParserForXMLSchema::processBacksolidus_gt() {

    ThrowXMLwithMemMgr(RuntimeException, XMLExcepts::Regex_NotSupported, getMemoryManager());
    return 0; // for compilers that complain about no return value
}

Token* ParserForXMLSchema::processBacksolidus_I() {

    processNext();
    return getTokenForShorthand(chLatin_I);
}

Token* ParserForXMLSchema::processBacksolidus_i() {

    processNext();
    return getTokenForShorthand(chLatin_i);
}

Token* ParserForXMLSchema::processBacksolidus_lt() {

    ThrowXMLwithMemMgr(RuntimeException, XMLExcepts::Regex_NotSupported, getMemoryManager());
    return 0; // for compilers that complain about no return value
}

Token* ParserForXMLSchema::processBacksolidus_X() {

    ThrowXMLwithMemMgr(RuntimeException, XMLExcepts::Regex_NotSupported, getMemoryManager());
    return 0; // for compilers that complain about no return value
}

Token* ParserForXMLSchema::processBacksolidus_Z() {

    ThrowXMLwithMemMgr(RuntimeException, XMLExcepts::Regex_NotSupported, getMemoryManager());
    return 0; // for compilers that complain about no return value
}

Token* ParserForXMLSchema::processBacksolidus_z() {

    ThrowXMLwithMemMgr(RuntimeException, XMLExcepts::Regex_NotSupported, getMemoryManager());
    return 0; // for compilers that complain about no return value
}

Token* ParserForXMLSchema::processBackReference() {

    ThrowXMLwithMemMgr(RuntimeException, XMLExcepts::Regex_NotSupported, getMemoryManager());
    return 0; // for compilers that complain about no return value
}

Token* ParserForXMLSchema::processCondition() {

    ThrowXMLwithMemMgr(RuntimeException, XMLExcepts::Regex_NotSupported, getMemoryManager());
    return 0; // for compilers that complain about no return value
}

Token* ParserForXMLSchema::processIndependent() {

    ThrowXMLwithMemMgr(RuntimeException, XMLExcepts::Regex_NotSupported, getMemoryManager());
    return 0; // for compilers that complain about no return value
}

Token* ParserForXMLSchema::processModifiers() {

    ThrowXMLwithMemMgr(RuntimeException, XMLExcepts::Regex_NotSupported, getMemoryManager());
    return 0; // for compilers that complain about no return value
}

Token* ParserForXMLSchema::processParen2() {

    ThrowXMLwithMemMgr(RuntimeException, XMLExcepts::Regex_NotSupported, getMemoryManager());
    return 0; // for compilers that complain about no return value
}

RangeToken* ParserForXMLSchema::parseSetOperations() {

    ThrowXMLwithMemMgr(RuntimeException, XMLExcepts::Regex_NotSupported, getMemoryManager());
    return 0; // for compilers that complain about no return value
}

// ---------------------------------------------------------------------------
//  ParserForXMLSchema: Getter methods
// ---------------------------------------------------------------------------
Token* ParserForXMLSchema::getTokenForShorthand(const XMLInt32 ch) {

    switch(ch) {
    case chLatin_d:
        return getTokenFactory()->getRange(fgXMLDigit);
    case chLatin_D:
        return getTokenFactory()->getRange(fgXMLDigit, true);
    case chLatin_w:
        return getTokenFactory()->getRange(fgXMLWord);
    case chLatin_W:
        return getTokenFactory()->getRange(fgXMLWord, true);
    case chLatin_s:
        return getTokenFactory()->getRange(fgXMLSpace);
    case chLatin_S:
        return getTokenFactory()->getRange(fgXMLSpace, true);
    case chLatin_c:
        return getTokenFactory()->getRange(fgXMLNameChar);
    case chLatin_C:
        return getTokenFactory()->getRange(fgXMLNameChar, true);
    case chLatin_i:
        return getTokenFactory()->getRange(fgXMLInitialNameChar);
    case chLatin_I:
        return getTokenFactory()->getRange(fgXMLInitialNameChar, true);
    }

    return 0;
}

// ---------------------------------------------------------------------------
//  ParserForXMLSchema: Helper methods
// ---------------------------------------------------------------------------
bool ParserForXMLSchema::checkQuestion(const int) {

    return false;
}


XMLInt32 ParserForXMLSchema::decodeEscaped() {

    if (getState() != REGX_T_BACKSOLIDUS)
        ThrowXMLwithMemMgr(ParseException,XMLExcepts::Parser_Next1, getMemoryManager());

    XMLInt32 ch = getCharData();

    switch (ch) {
    case chLatin_n:
        ch = chLF;
        break;
    case chLatin_r:
        ch = chCR;
        break;
    case chLatin_t:
        ch = chHTab;
        break;
    case chBackSlash:
    case chPipe:
    case chPeriod:
    case chCaret:
    case chDash:
    case chQuestion:
    case chAsterisk:
    case chPlus:
    case chOpenCurly:
    case chCloseCurly:
    case chOpenParen:
    case chCloseParen:
    case chOpenSquare:
    case chCloseSquare:
        break;
    default:
		{
        XMLCh chString[] = {chBackSlash, ch, chNull};        
        ThrowXMLwithMemMgr1(ParseException,XMLExcepts::Parser_Process2, chString, getMemoryManager());
        }
    }

    return ch;
}

XERCES_CPP_NAMESPACE_END

/**
  * End of file ParserForXMLSchema.cpp
  */
