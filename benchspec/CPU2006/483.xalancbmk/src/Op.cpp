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
 * $Log: Op.cpp,v $
 * Revision 1.6  2004/01/29 11:51:21  cargilld
 * Code cleanup changes to get rid of various compiler diagnostic messages.
 *
 * Revision 1.5  2003/12/17 00:18:37  cargilld
 * Update to memory management so that the static memory manager (one used to call Initialize) is only for static data.
 *
 * Revision 1.4  2003/05/18 14:02:06  knoaman
 * Memory manager implementation: pass per instance manager.
 *
 * Revision 1.3  2003/05/16 00:03:10  knoaman
 * Partial implementation of the configurable memory manager.
 *
 * Revision 1.2  2002/11/04 15:17:00  tng
 * C++ Namespace Support.
 *
 * Revision 1.1.1.1  2002/02/01 22:22:29  peiyongz
 * sane_include
 *
 * Revision 1.3  2001/06/01 14:15:36  knoaman
 * Add a return value to satisfy compilers that complain about
 * no return value, although that code will not be executed.
 *
 * Revision 1.2  2001/05/11 13:26:43  tng
 * Copyright update.
 *
 * Revision 1.1  2001/03/02 19:22:47  knoaman
 * Schema: Regular expression handling part I
 *
 */

// ---------------------------------------------------------------------------
//  Includes
// ---------------------------------------------------------------------------
#include <xercesc/util/regx/Op.hpp>
#include <xercesc/util/XMLString.hpp>

XERCES_CPP_NAMESPACE_BEGIN

// ---------------------------------------------------------------------------
//  Op: Constructors and Destructors
// ---------------------------------------------------------------------------
Op::Op(const short type, MemoryManager* const manager) 
    : fMemoryManager(manager)
    , fOpType(type)
    , fNextOp(0)
{
}

// ---------------------------------------------------------------------------
//  Op: Getter methods
// ---------------------------------------------------------------------------
int Op::getSize() const {

	ThrowXMLwithMemMgr(RuntimeException, XMLExcepts::Regex_NotSupported, fMemoryManager);
    return 0; // for compilers that complain about no return value
}

XMLInt32 Op::getData() const {

	ThrowXMLwithMemMgr(RuntimeException, XMLExcepts::Regex_NotSupported, fMemoryManager);
    return 0; // for compilers that complain about no return value
}

XMLInt32 Op::getData2() const {

	ThrowXMLwithMemMgr(RuntimeException, XMLExcepts::Regex_NotSupported, fMemoryManager);
    return 0; // for compilers that complain about no return value
}

int Op::getRefNo() const {

	ThrowXMLwithMemMgr(RuntimeException, XMLExcepts::Regex_NotSupported, fMemoryManager);
    return 0; // for compilers that complain about no return value
}

const Op* Op::elementAt(int) const {

	ThrowXMLwithMemMgr(RuntimeException, XMLExcepts::Regex_NotSupported, fMemoryManager);
    return 0; // for compilers that complain about no return value
}

const Op* Op::getChild() const {

	ThrowXMLwithMemMgr(RuntimeException, XMLExcepts::Regex_NotSupported, fMemoryManager);
    return 0; // for compilers that complain about no return value
}

const Op* Op::getConditionFlow() const {

	ThrowXMLwithMemMgr(RuntimeException, XMLExcepts::Regex_NotSupported, fMemoryManager);
    return 0; // for compilers that complain about no return value
}

const Op* Op::getYesFlow() const {

	ThrowXMLwithMemMgr(RuntimeException, XMLExcepts::Regex_NotSupported, fMemoryManager);
    return 0; // for compilers that complain about no return value
}

const Op* Op::getNoFlow() const {

	ThrowXMLwithMemMgr(RuntimeException, XMLExcepts::Regex_NotSupported, fMemoryManager);
    return 0; // for compilers that complain about no return value
}
	
const XMLCh* Op::getLiteral() const {

	ThrowXMLwithMemMgr(RuntimeException, XMLExcepts::Regex_NotSupported, fMemoryManager);
    return 0; // for compilers that complain about no return value
}
	
const Token* Op::getToken() const {

	ThrowXMLwithMemMgr(RuntimeException, XMLExcepts::Regex_NotSupported, fMemoryManager);
    return 0; // for compilers that complain about no return value
}


// ---------------------------------------------------------------------------
//  CharOp: Constructors and Destuctors
// ---------------------------------------------------------------------------
CharOp::CharOp(const short type, const XMLInt32 charData
               , MemoryManager* const manager)
    : Op(type, manager)
      , fCharData(charData) {
}

// ---------------------------------------------------------------------------
//  CharOp: Getter methods
// ---------------------------------------------------------------------------
XMLInt32 CharOp::getData() const {

	return fCharData;
}

// ---------------------------------------------------------------------------
//  UnionOp: Constructors and Destuctors
// ---------------------------------------------------------------------------
UnionOp::UnionOp(const short type, const int size, MemoryManager* const manager)
    : Op(type, manager)
      , fBranches(new (manager) RefVectorOf<Op> (size, false, manager)) {

}

// ---------------------------------------------------------------------------
//  UnionOp: Getter/Setter methods
// ---------------------------------------------------------------------------
int UnionOp::getSize() const {

	return fBranches->size();
}

const Op* UnionOp::elementAt(int index) const {

	return fBranches->elementAt(index);
}

void UnionOp::addElement(Op* const op) {

	fBranches->addElement(op);
}

// ---------------------------------------------------------------------------
//  ChildOp: Constructors and Destuctors
// ---------------------------------------------------------------------------
ChildOp::ChildOp(const short type, MemoryManager* const manager)
    : Op(type, manager)
      , fChild(0) {

}

// ---------------------------------------------------------------------------
//  ChildOp: Getter/Setter methods
// ---------------------------------------------------------------------------
const Op* ChildOp::getChild() const {

	return fChild;
}

void ChildOp::setChild(const Op* const child) {

	fChild = child;
}

// ---------------------------------------------------------------------------
//  ModifierOp: Constructors and Destuctors
// ---------------------------------------------------------------------------
ModifierOp::ModifierOp(const short type, const XMLInt32 v1, const XMLInt32 v2
                       , MemoryManager* const manager)
    : ChildOp(type, manager)
      , fVal1(v1)
      , fVal2(v2) {

}

// ---------------------------------------------------------------------------
//  ModifierOp: Getter methods
// ---------------------------------------------------------------------------
XMLInt32 ModifierOp::getData() const {

	return fVal1;
}

XMLInt32 ModifierOp::getData2() const {

	return fVal2;
}

// ---------------------------------------------------------------------------
//  RangeOp: Constructors and Destuctors
// ---------------------------------------------------------------------------
RangeOp::RangeOp(const short type, const Token* const token, MemoryManager* const manager)
    : Op (type, manager)
      , fToken(token) {

}

// ---------------------------------------------------------------------------
//  RangeOp: Getter methods
// ---------------------------------------------------------------------------
const Token* RangeOp::getToken() const {

	return fToken;
}


// ---------------------------------------------------------------------------
//  StringOp: Constructors and Destuctors
// ---------------------------------------------------------------------------
StringOp::StringOp(const short type, const XMLCh* const literal
                   , MemoryManager* const manager)
    : Op (type, manager)
      , fLiteral(XMLString::replicate(literal, manager)) {

}

// ---------------------------------------------------------------------------
//  StringOp: Getter methods
// ---------------------------------------------------------------------------
const XMLCh* StringOp::getLiteral() const {

	return fLiteral;
}

// ---------------------------------------------------------------------------
//  ConditionOp: Constructors and Destuctors
// ---------------------------------------------------------------------------
ConditionOp::ConditionOp(const short type, const int refNo,
                         const Op* const condFlow, const Op* const yesFlow,
                         const Op* const noFlow, MemoryManager* const manager)
    : Op (type, manager)
      , fRefNo(refNo)
      , fConditionOp(condFlow)
      , fYesOp(yesFlow)
      , fNoOp(noFlow) {

}

// ---------------------------------------------------------------------------
//  ConditionOp: Getter methods
// ---------------------------------------------------------------------------
int ConditionOp::getRefNo() const {
	
	return fRefNo;
}

const Op* ConditionOp::getConditionFlow() const {

	return fConditionOp;
}

const Op* ConditionOp::getYesFlow() const {

	return fYesOp;
}

const Op* ConditionOp::getNoFlow() const {

	return fNoOp;
}

XERCES_CPP_NAMESPACE_END

/**
  * End file Op.cpp
  */
