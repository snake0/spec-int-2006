/*
 * The Apache Software License, Version 1.1
 *
 * Copyright (c) 1999-2001 The Apache Software Foundation.  All rights
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
 * originally based on software copyright (c) 1999, International
 * Business Machines, Inc., http://www.ibm.com .  For more information
 * on the Apache Software Foundation, please see
 * <http://www.apache.org/>.
 */

/*
 * $Log: ElemStack.cpp,v $
 * Revision 1.10  2003/12/17 00:18:34  cargilld
 * Update to memory management so that the static memory manager (one used to call Initialize) is only for static data.
 *
 * Revision 1.9  2003/10/23 14:11:07  knoaman
 * Fix memory leak.
 *
 * Revision 1.8  2003/10/22 20:22:30  knoaman
 * Prepare for annotation support.
 *
 * Revision 1.7  2003/05/18 14:02:04  knoaman
 * Memory manager implementation: pass per instance manager.
 *
 * Revision 1.6  2003/05/16 21:36:57  knoaman
 * Memory manager implementation: Modify constructors to pass in the memory manager.
 *
 * Revision 1.5  2003/05/15 18:26:29  knoaman
 * Partial implementation of the configurable memory manager.
 *
 * Revision 1.4  2003/01/02 16:38:00  knoaman
 * Some cleanup.
 *
 * Revision 1.3  2002/12/04 02:23:50  knoaman
 * Scanner re-organization.
 *
 * Revision 1.2  2002/11/04 14:58:18  tng
 * C++ Namespace Support.
 *
 * Revision 1.1.1.1  2002/02/01 22:21:57  peiyongz
 * sane_include
 *
 * Revision 1.17  2001/12/12 14:29:50  tng
 * Remove obsolete code in ElemStack which can help performance.
 *
 * Revision 1.16  2001/08/29 16:42:27  tng
 * No need to new the child QName in ElemStack addChild.  Remove it for performance gain.
 *
 * Revision 1.15  2001/08/07 13:47:47  tng
 * Schema: Fix unmatched end tag for qualified/unqualifed start tag.
 *
 * Revision 1.14  2001/06/18 21:33:57  peiyongz
 * Memory leak fix: to addlevel(), by Erik Rydgren.
 *
 * Revision 1.13  2001/06/12 19:08:27  peiyongz
 * Memory leak: fixed by Erik Rydgren
 *
 * Revision 1.12  2001/05/28 20:55:19  tng
 * Schema: Store Grammar in ElemStack as well.
 *
 * Revision 1.11  2001/05/11 13:26:16  tng
 * Copyright update.
 *
 * Revision 1.10  2001/05/03 20:34:28  tng
 * Schema: SchemaValidator update
 *
 * Revision 1.9  2001/04/19 18:16:57  tng
 * Schema: SchemaValidator update, and use QName in Content Model
 *
 * Revision 1.8  2001/02/16 17:58:02  tng
 * use EmptyNamespaceId for attribute, GlobalNamespaceId for element.
 *
 * Revision 1.7  2000/07/05 05:20:17  roddey
 * Fixed a memory leak when namespaces are enabled.
 *
 * Revision 1.6  2000/05/15 22:31:15  andyh
 * Replace #include<memory.h> with <string.h> everywhere.
 *
 * Revision 1.5  2000/03/02 19:54:28  roddey
 * This checkin includes many changes done while waiting for the
 * 1.1.0 code to be finished. I can't list them all here, but a list is
 * available elsewhere.
 *
 * Revision 1.4  2000/02/08 19:38:58  roddey
 * xmlns:xxx="" should affect the mapping of the prefixes of sibling attributes,
 * which was not being done.
 *
 * Revision 1.3  2000/02/06 07:47:52  rahulj
 * Year 2K copyright swat.
 *
 * Revision 1.2  2000/01/19 00:55:45  roddey
 * Changes to get rid of dependence on old utils standard streams classes
 * and a small fix in the progressive parseFirst() call.
 *
 * Revision 1.1.1.1  1999/11/09 01:08:04  twl
 * Initial checkin
 *
 * Revision 1.4  1999/11/08 20:44:41  rahul
 * Swat for adding in Product name and CVS comment log variable.
 *
 */

// ---------------------------------------------------------------------------
//  Includes
// ---------------------------------------------------------------------------
#include <string.h>
#include <xercesc/util/EmptyStackException.hpp>
#include <xercesc/util/NoSuchElementException.hpp>
#include <xercesc/framework/XMLElementDecl.hpp>
#include <xercesc/internal/ElemStack.hpp>
#include <xercesc/validators/common/Grammar.hpp>

XERCES_CPP_NAMESPACE_BEGIN

// ---------------------------------------------------------------------------
//  ElemStack: Constructors and Destructor
// ---------------------------------------------------------------------------
ElemStack::ElemStack(MemoryManager* const manager) :

    fEmptyNamespaceId(0)
    , fGlobalPoolId(0)
    , fPrefixPool(109, manager)
    , fStack(0)
    , fStackCapacity(32)
    , fStackTop(0)
    , fUnknownNamespaceId(0)
    , fXMLNamespaceId(0)
    , fXMLPoolId(0)
    , fXMLNSNamespaceId(0)
    , fXMLNSPoolId(0)
    , fNamespaceMap(0)
    , fMemoryManager(manager)
{
    // Do an initial allocation of the stack and zero it out
    fStack = (StackElem**) fMemoryManager->allocate
    (
        fStackCapacity * sizeof(StackElem*)
    );//new StackElem*[fStackCapacity];
    memset(fStack, 0, fStackCapacity * sizeof(StackElem*));

    fNamespaceMap = new (fMemoryManager) ValueVectorOf<PrefMapElem*>(16, fMemoryManager);
}

ElemStack::~ElemStack()
{
    //
    //  Start working from the bottom of the stack and clear it out as we
    //  go up. Once we hit an uninitialized one, we can break out.
    //
    for (unsigned int stackInd = 0; stackInd < fStackCapacity; stackInd++)
    {
        // If this entry has been set, then lets clean it up
        if (!fStack[stackInd])
            break;

        fMemoryManager->deallocate(fStack[stackInd]->fChildren);//delete [] fStack[stackInd]->fChildren;
        fMemoryManager->deallocate(fStack[stackInd]->fMap);//delete [] fStack[stackInd]->fMap;
        delete fStack[stackInd];
    }

    // Delete the stack array itself now
    fMemoryManager->deallocate(fStack);//delete [] fStack;
    delete fNamespaceMap;
}


// ---------------------------------------------------------------------------
//  ElemStack: Stack access
// ---------------------------------------------------------------------------
unsigned int ElemStack::addLevel()
{
    // See if we need to expand the stack
    if (fStackTop == fStackCapacity)
        expandStack();

    // If this element has not been initialized yet, then initialize it
    if (!fStack[fStackTop])
    {
        fStack[fStackTop] = new (fMemoryManager) StackElem;
        fStack[fStackTop]->fChildCapacity = 0;
        fStack[fStackTop]->fChildren = 0;
        fStack[fStackTop]->fMapCapacity = 0;
        fStack[fStackTop]->fMap = 0;
    }

    // Set up the new top row
    fStack[fStackTop]->fThisElement = 0;
    fStack[fStackTop]->fReaderNum = 0xFFFFFFFF;
    fStack[fStackTop]->fChildCount = 0;
    fStack[fStackTop]->fMapCount = 0;
    fStack[fStackTop]->fValidationFlag = false;
    fStack[fStackTop]->fCurrentURI = fUnknownNamespaceId;
    fStack[fStackTop]->fCurrentScope = Grammar::TOP_LEVEL_SCOPE;
    fStack[fStackTop]->fCurrentGrammar = 0;

    // Bump the top of stack
    fStackTop++;

    return fStackTop-1;
}


unsigned int
ElemStack::addLevel(XMLElementDecl* const toSet, const unsigned int readerNum)
{
    // See if we need to expand the stack
    if (fStackTop == fStackCapacity)
        expandStack();

    // If this element has not been initialized yet, then initialize it
    if (!fStack[fStackTop])
    {
        fStack[fStackTop] = new (fMemoryManager) StackElem;
        fStack[fStackTop]->fChildCapacity = 0;
        fStack[fStackTop]->fChildren = 0;
        fStack[fStackTop]->fMapCapacity = 0;
        fStack[fStackTop]->fMap = 0;
    }

    // Set up the new top row
    fStack[fStackTop]->fThisElement = toSet;
    fStack[fStackTop]->fReaderNum = readerNum;
    fStack[fStackTop]->fChildCount = 0;
    fStack[fStackTop]->fMapCount = 0;
    fStack[fStackTop]->fValidationFlag = false;
    fStack[fStackTop]->fCurrentURI = fUnknownNamespaceId;
    fStack[fStackTop]->fCurrentScope = Grammar::TOP_LEVEL_SCOPE;
    fStack[fStackTop]->fCurrentGrammar = 0;

    // Bump the top of stack
    fStackTop++;

    return fStackTop-1;
}



const ElemStack::StackElem* ElemStack::popTop()
{
    // Watch for an underflow error
    if (!fStackTop)
        ThrowXMLwithMemMgr(EmptyStackException, XMLExcepts::ElemStack_StackUnderflow, fMemoryManager);

    fStackTop--;
    return fStack[fStackTop];
}


void
ElemStack::setElement(XMLElementDecl* const toSet, const unsigned int readerNum)
{
    if (!fStackTop)
        ThrowXMLwithMemMgr(EmptyStackException, XMLExcepts::ElemStack_EmptyStack, fMemoryManager);

    fStack[fStackTop - 1]->fThisElement = toSet;
    fStack[fStackTop - 1]->fReaderNum = readerNum;
}


// ---------------------------------------------------------------------------
//  ElemStack: Stack top access
// ---------------------------------------------------------------------------
unsigned int ElemStack::addChild(QName* const child, const bool toParent)
{
    if (!fStackTop)
        ThrowXMLwithMemMgr(EmptyStackException, XMLExcepts::ElemStack_EmptyStack, fMemoryManager);

    //
    //  If they want to add to the parent, then we have to have at least two
    //  elements on the stack.
    //
    if (toParent && (fStackTop < 2))
        ThrowXMLwithMemMgr(NoSuchElementException, XMLExcepts::ElemStack_NoParentPushed, fMemoryManager);

    // Get a convenience pointer to the stack top row
    StackElem* curRow = toParent
                        ? fStack[fStackTop - 2] : fStack[fStackTop - 1];

    // See if we need to expand this row's child array
    if (curRow->fChildCount == curRow->fChildCapacity)
    {
        // Increase the capacity by a quarter and allocate a new row
        const unsigned int newCapacity = curRow->fChildCapacity ?
                                         (unsigned int)(curRow->fChildCapacity * 1.25) :
                                         32;
        QName** newRow = (QName**) fMemoryManager->allocate
        (
            newCapacity * sizeof(QName*)
        );//new QName*[newCapacity];

        //
        //  Copy over the old contents. We don't have to initialize the new
        //  part because The current child count is used to know how much of
        //  it is valid.
        //
        //  Only both doing this if there is any current content, since
        //  this code also does the initial faulting in of the array when
        //  both the current capacity and child count are zero.
        //
        if (curRow->fChildCount)
        {
             unsigned int index = 0;
             for (; index < curRow->fChildCount; index++)
                 newRow[index] = curRow->fChildren[index];
        }

        // Clean up the old children and store the new info
        fMemoryManager->deallocate(curRow->fChildren);//delete [] curRow->fChildren;
        curRow->fChildren = newRow;
        curRow->fChildCapacity = newCapacity;
    }

    // Add this id to the end of the row's child id array and bump the count
    curRow->fChildren[curRow->fChildCount++] = child;

    // Return the level of the index we just filled (before the bump)
    return curRow->fChildCount - 1;
}

const ElemStack::StackElem* ElemStack::topElement() const
{
    if (!fStackTop)
        ThrowXMLwithMemMgr(EmptyStackException, XMLExcepts::ElemStack_EmptyStack, fMemoryManager);

    return fStack[fStackTop - 1];
}


// ---------------------------------------------------------------------------
//  ElemStack: Prefix map methods
// ---------------------------------------------------------------------------
void ElemStack::addPrefix(  const   XMLCh* const    prefixToAdd
                            , const unsigned int    uriId)
{
    if (!fStackTop)
        ThrowXMLwithMemMgr(EmptyStackException, XMLExcepts::ElemStack_EmptyStack, fMemoryManager);

    // Get a convenience pointer to the stack top row
    StackElem* curRow = fStack[fStackTop - 1];

    // Map the prefix to its unique id
    const unsigned int prefId = fPrefixPool.addOrFind(prefixToAdd);

    //
    //  Add a new element to the prefix map for this element. If its full,
    //  then expand it out.
    //
    if (curRow->fMapCount == curRow->fMapCapacity)
        expandMap(curRow);

    //
    //  And now add a new element for this prefix. Watch for the special case
    //  of xmlns=="", and force it to ""=[globalid]
    //
    curRow->fMap[curRow->fMapCount].fPrefId = prefId;
    if ((prefId == fGlobalPoolId) && (uriId == fEmptyNamespaceId))
        curRow->fMap[curRow->fMapCount].fURIId = fEmptyNamespaceId;
    else
        curRow->fMap[curRow->fMapCount].fURIId = uriId;

    // Bump the map count now
    curRow->fMapCount++;
}


unsigned int ElemStack::mapPrefixToURI( const   XMLCh* const    prefixToMap
                                        , const MapModes        mode
                                        ,       bool&           unknown) const
{
    // Assume we find it
    unknown = false;

    //
    //  Map the prefix to its unique id, from the prefix string pool. If its
    //  not a valid prefix, then its a failure.
    //
    unsigned int prefixId = fPrefixPool.getId(prefixToMap);
    if (!prefixId)
    {
        unknown = true;
        return fUnknownNamespaceId;
    }

    //
    //  If the prefix is empty, and we are in attribute mode, then we assign
    //  it to the empty namespace because the default namespace does not
    //  apply to attributes.
    //
    if (!*prefixToMap && (mode == Mode_Attribute))
        return fEmptyNamespaceId;

    //
    //  Check for the special prefixes 'xml' and 'xmlns' since they cannot
    //  be overridden.
    //
    if (prefixId == fXMLPoolId)
        return fXMLNamespaceId;
    else if (prefixId == fXMLNSPoolId)
        return fXMLNSNamespaceId;

    //
    //  Start at the stack top and work backwards until we come to some
    //  element that mapped this prefix.
    //
    int startAt = (int)(fStackTop - 1);
    for (int index = startAt; index >= 0; index--)
    {
        // Get a convenience pointer to the current element
        StackElem* curRow = fStack[index];

        // If no prefixes mapped at this level, then go the next one
        if (!curRow->fMapCount)
            continue;

        // Search the map at this level for the passed prefix
        for (unsigned int mapIndex = 0; mapIndex < curRow->fMapCount; mapIndex++)
        {
            if (curRow->fMap[mapIndex].fPrefId == prefixId)
                return curRow->fMap[mapIndex].fURIId;
        }
    }

    //
    //  If the prefix is an empty string, then we will return the special
    //  global namespace id. This can be overridden, but no one has or we
    //  would have not gotten here.
    //
    if (!*prefixToMap)
        return fEmptyNamespaceId;

    // Oh well, don't have a clue so return the unknown id
    unknown = true;
    return fUnknownNamespaceId;
}


ValueVectorOf<PrefMapElem*>* ElemStack::getNamespaceMap() const
{
    fNamespaceMap->removeAllElements();

    //  Start at the stack top and work backwards until we come to some
    //  element that mapped this prefix.
    int startAt = (int)(fStackTop - 1);
    for (int index = startAt; index >= 0; index--)
    {
        // Get a convenience pointer to the current element
        StackElem* curRow = fStack[index];

        // If no prefixes mapped at this level, then go the next one
        if (!curRow->fMapCount)
            continue;

        // Search the map at this level for the passed prefix
        for (unsigned int mapIndex = 0; mapIndex < curRow->fMapCount; mapIndex++)
        {
            fNamespaceMap->addElement(&(curRow->fMap[mapIndex]));
        }
    }

    return fNamespaceMap;
}

// ---------------------------------------------------------------------------
//  ElemStack: Miscellaneous methods
// ---------------------------------------------------------------------------
void ElemStack::reset(  const   unsigned int    emptyId
                        , const unsigned int    unknownId
                        , const unsigned int    xmlId
                        , const unsigned int    xmlNSId)
{
    // Reset the stack top to clear the stack
    fStackTop = 0;

    // if first time, put in the standard prefixes
    if (fXMLPoolId == 0) {

        fGlobalPoolId = fPrefixPool.addOrFind(XMLUni::fgZeroLenString);
        fXMLPoolId = fPrefixPool.addOrFind(XMLUni::fgXMLString);
        fXMLNSPoolId = fPrefixPool.addOrFind(XMLUni::fgXMLNSString);
    }

    // And store the new special URI ids
    fEmptyNamespaceId = emptyId;
    fUnknownNamespaceId = unknownId;
    fXMLNamespaceId = xmlId;
    fXMLNSNamespaceId = xmlNSId;
}


// ---------------------------------------------------------------------------
//  ElemStack: Private helpers
// ---------------------------------------------------------------------------
void ElemStack::expandMap(StackElem* const toExpand)
{
    // For convenience get the old map size
    const unsigned int oldCap = toExpand->fMapCapacity;

    //
    //  Expand the capacity by 25%, or initialize it to 16 if its currently
    //  empty. Then allocate a new temp buffer.
    //
    const unsigned int newCapacity = oldCap ?
                                     (unsigned int)(oldCap * 1.25) : 16;
    PrefMapElem* newMap = (PrefMapElem*) fMemoryManager->allocate
    (
        newCapacity * sizeof(PrefMapElem)
    );//new PrefMapElem[newCapacity];

    //
    //  Copy over the old stuff. We DON'T have to zero out the new stuff
    //  since this is a by value map and the current map index controls what
    //  is relevant.
    //
    memcpy(newMap, toExpand->fMap, oldCap * sizeof(PrefMapElem));

    // Delete the old map and store the new stuff
    fMemoryManager->deallocate(toExpand->fMap);//delete [] toExpand->fMap;
    toExpand->fMap = newMap;
    toExpand->fMapCapacity = newCapacity;
}

void ElemStack::expandStack()
{
    // Expand the capacity by 25% and allocate a new buffer
    const unsigned int newCapacity = (unsigned int)(fStackCapacity * 1.25);
    StackElem** newStack = (StackElem**) fMemoryManager->allocate
    (
        newCapacity * sizeof(StackElem*)
    );//new StackElem*[newCapacity];

    // Copy over the old stuff
    memcpy(newStack, fStack, fStackCapacity * sizeof(StackElem*));

    //
    //  And zero out the new stuff. Though we use a stack top, we reuse old
    //  stack contents so we need to know if elements have been initially
    //  allocated or not as we push new stuff onto the stack.
    //
    memset
    (
        &newStack[fStackCapacity]
        , 0
        , (newCapacity - fStackCapacity) * sizeof(StackElem*)
    );

    // Delete the old array and update our members
    fMemoryManager->deallocate(fStack);//delete [] fStack;
    fStack = newStack;
    fStackCapacity = newCapacity;
}



// ---------------------------------------------------------------------------
//  WFElemStack: Constructors and Destructor
// ---------------------------------------------------------------------------
WFElemStack::WFElemStack(MemoryManager* const manager) :

    fEmptyNamespaceId(0)
    , fGlobalPoolId(0)
    , fStackCapacity(32)
    , fStackTop(0)
    , fUnknownNamespaceId(0)
    , fXMLNamespaceId(0)
    , fXMLPoolId(0)
    , fXMLNSNamespaceId(0)
    , fXMLNSPoolId(0)
    , fMapCapacity(0)
    , fMap(0)
    , fStack(0)
    , fPrefixPool(109, manager)
    , fMemoryManager(manager)
{
    // Do an initial allocation of the stack and zero it out
    fStack = (StackElem**) fMemoryManager->allocate
    (
        fStackCapacity * sizeof(StackElem*)
    );//new StackElem*[fStackCapacity];
    memset(fStack, 0, fStackCapacity * sizeof(StackElem*));
}

WFElemStack::~WFElemStack()
{
    //
    //  Start working from the bottom of the stack and clear it out as we
    //  go up. Once we hit an uninitialized one, we can break out.
    //
    for (unsigned int stackInd = 0; stackInd < fStackCapacity; stackInd++)
    {
        // If this entry has been set, then lets clean it up
        if (!fStack[stackInd])
            break;

        fMemoryManager->deallocate(fStack[stackInd]->fThisElement);//delete [] fStack[stackInd]->fThisElement;
        delete fStack[stackInd];
    }

    if (fMap)
        fMemoryManager->deallocate(fMap);//delete [] fMap;

    // Delete the stack array itself now
    fMemoryManager->deallocate(fStack);//delete [] fStack;
}


// ---------------------------------------------------------------------------
//  WFElemStack: Stack access
// ---------------------------------------------------------------------------
unsigned int WFElemStack::addLevel()
{
    // See if we need to expand the stack
    if (fStackTop == fStackCapacity)
        expandStack();

    
    // If this element has not been initialized yet, then initialize it
    if (!fStack[fStackTop])
    {
        fStack[fStackTop] = new (fMemoryManager) StackElem;
        fStack[fStackTop]->fThisElement = 0;
        fStack[fStackTop]->fElemMaxLength = 0;
    }

    // Set up the new top row
    fStack[fStackTop]->fReaderNum = 0xFFFFFFFF;
    fStack[fStackTop]->fCurrentURI = fUnknownNamespaceId;
    fStack[fStackTop]->fTopPrefix = -1;

    if (fStackTop != 0)
        fStack[fStackTop]->fTopPrefix = fStack[fStackTop - 1]->fTopPrefix;

    // Bump the top of stack
    fStackTop++;

    return fStackTop-1;
}


unsigned int
WFElemStack::addLevel(const XMLCh* const toSet,
                      const unsigned int toSetLen,
                      const unsigned int readerNum)
{
    // See if we need to expand the stack
    if (fStackTop == fStackCapacity)
        expandStack();

    // If this element has not been initialized yet, then initialize it
    if (!fStack[fStackTop])
    {
        fStack[fStackTop] = new (fMemoryManager) StackElem;
        fStack[fStackTop]->fThisElement = 0;
        fStack[fStackTop]->fElemMaxLength = 0;
    }

    // Set up the new top row
    fStack[fStackTop]->fCurrentURI = fUnknownNamespaceId;
    fStack[fStackTop]->fTopPrefix = -1;

    // And store the new stuff
    if (toSetLen > fStack[fStackTop]->fElemMaxLength) {

        fMemoryManager->deallocate(fStack[fStackTop]->fThisElement);//delete [] fStack[fStackTop]->fThisElement;
        fStack[fStackTop]->fElemMaxLength = toSetLen;
        fStack[fStackTop]->fThisElement = (XMLCh*) fMemoryManager->allocate
        (
            (toSetLen + 1) * sizeof(XMLCh)
        );//new XMLCh[toSetLen + 1];
    }

    XMLString::moveChars(fStack[fStackTop]->fThisElement, toSet, toSetLen + 1);
    fStack[fStackTop]->fReaderNum = readerNum;

    if (fStackTop != 0)
        fStack[fStackTop]->fTopPrefix = fStack[fStackTop - 1]->fTopPrefix;

    // Bump the top of stack
    fStackTop++;

    return fStackTop-1;
}



const WFElemStack::StackElem* WFElemStack::popTop()
{
    // Watch for an underflow error
    if (!fStackTop)
        ThrowXMLwithMemMgr(EmptyStackException, XMLExcepts::ElemStack_StackUnderflow, fMemoryManager);

    fStackTop--;
    return fStack[fStackTop];
}


void
WFElemStack::setElement(const XMLCh* const toSet,
                      const unsigned int toSetLen,
                      const unsigned int readerNum)
{
    if (!fStackTop)
        ThrowXMLwithMemMgr(EmptyStackException, XMLExcepts::ElemStack_EmptyStack, fMemoryManager);

    if (toSetLen > fStack[fStackTop - 1]->fElemMaxLength) {

        fMemoryManager->deallocate(fStack[fStackTop - 1]->fThisElement);//delete [] fStack[fStackTop - 1]->fThisElement;
        fStack[fStackTop - 1]->fElemMaxLength = toSetLen;
        fStack[fStackTop - 1]->fThisElement = (XMLCh*) fMemoryManager->allocate
        (
            (toSetLen + 1) * sizeof(XMLCh)
        );//new XMLCh[toSetLen + 1];
    }

    XMLString::moveChars(fStack[fStackTop - 1]->fThisElement, toSet, toSetLen + 1);
    fStack[fStackTop - 1]->fReaderNum = readerNum;
}


// ---------------------------------------------------------------------------
//  WFElemStack: Stack top access
// ---------------------------------------------------------------------------
const WFElemStack::StackElem* WFElemStack::topElement() const
{
    if (!fStackTop)
        ThrowXMLwithMemMgr(EmptyStackException, XMLExcepts::ElemStack_EmptyStack, fMemoryManager);

    return fStack[fStackTop - 1];
}


// ---------------------------------------------------------------------------
//  WFElemStack: Prefix map methods
// ---------------------------------------------------------------------------
void WFElemStack::addPrefix(  const   XMLCh* const    prefixToAdd
                              , const unsigned int    uriId)
{
    if (!fStackTop)
        ThrowXMLwithMemMgr(EmptyStackException, XMLExcepts::ElemStack_EmptyStack, fMemoryManager);

    // Get a convenience pointer to the stack top row
    StackElem* curRow = fStack[fStackTop - 1];

    // Map the prefix to its unique id
    const unsigned int prefId = fPrefixPool.addOrFind(prefixToAdd);

    //
    //  Add a new element to the prefix map for this element. If its full,
    //  then expand it out.
    //
    if ((unsigned int)curRow->fTopPrefix + 1 == fMapCapacity)
        expandMap();

    //
    //  And now add a new element for this prefix. Watch for the special case
    //  of xmlns=="", and force it to ""=[globalid]
    //
    fMap[curRow->fTopPrefix + 1].fPrefId = prefId;
    if ((prefId == fGlobalPoolId) && (uriId == fEmptyNamespaceId))
        fMap[curRow->fTopPrefix + 1].fURIId = fEmptyNamespaceId;
    else
        fMap[curRow->fTopPrefix + 1].fURIId = uriId;

    // Bump the map count now
    curRow->fTopPrefix++;
}


unsigned int WFElemStack::mapPrefixToURI( const   XMLCh* const    prefixToMap
                                          , const MapModes        mode
                                          ,       bool&           unknown) const
{
    // Assume we find it
    unknown = false;

    //
    //  Map the prefix to its unique id, from the prefix string pool. If its
    //  not a valid prefix, then its a failure.
    //
    unsigned int prefixId = fPrefixPool.getId(prefixToMap);
    if (!prefixId)
    {
        unknown = true;
        return fUnknownNamespaceId;
    }

    //
    //  If the prefix is empty, and we are in attribute mode, then we assign
    //  it to the empty namespace because the default namespace does not
    //  apply to attributes.
    //
    if (!*prefixToMap && (mode == Mode_Attribute))
        return fEmptyNamespaceId;

    //
    //  Check for the special prefixes 'xml' and 'xmlns' since they cannot
    //  be overridden.
    //
    if (prefixId == fXMLPoolId)
        return fXMLNamespaceId;
    else if (prefixId == fXMLNSPoolId)
        return fXMLNSNamespaceId;

    //
    //  Start at the stack top and work backwards until we come to some
    //  element that mapped this prefix.
    //
    //  Get a convenience pointer to the stack top row
    StackElem* curRow = fStack[fStackTop - 1];
    for (int mapIndex = curRow->fTopPrefix; mapIndex >=0; mapIndex--)
    {
        if (fMap[mapIndex].fPrefId == prefixId)
            return fMap[mapIndex].fURIId;
    }

    //
    //  If the prefix is an empty string, then we will return the special
    //  global namespace id. This can be overridden, but no one has or we
    //  would have not gotten here.
    //
    if (!*prefixToMap)
        return fEmptyNamespaceId;

    // Oh well, don't have a clue so return the unknown id
    unknown = true;
    return fUnknownNamespaceId;
}


// ---------------------------------------------------------------------------
//  WFElemStack: Miscellaneous methods
// ---------------------------------------------------------------------------
void WFElemStack::reset(  const   unsigned int    emptyId
                          , const unsigned int    unknownId
                          , const unsigned int    xmlId
                          , const unsigned int    xmlNSId)
{
    // Reset the stack top to clear the stack
    fStackTop = 0;

    // if first time, put in the standard prefixes
    if (fXMLPoolId == 0) {

        fGlobalPoolId = fPrefixPool.addOrFind(XMLUni::fgZeroLenString);
        fXMLPoolId = fPrefixPool.addOrFind(XMLUni::fgXMLString);
        fXMLNSPoolId = fPrefixPool.addOrFind(XMLUni::fgXMLNSString);
    }

    // And store the new special URI ids
    fEmptyNamespaceId = emptyId;
    fUnknownNamespaceId = unknownId;
    fXMLNamespaceId = xmlId;
    fXMLNSNamespaceId = xmlNSId;
}


// ---------------------------------------------------------------------------
//  WFElemStack: Private helpers
// ---------------------------------------------------------------------------
void WFElemStack::expandMap()
{
    //
    //  Expand the capacity by 25%, or initialize it to 16 if its currently
    //  empty. Then allocate a new temp buffer.
    //
    const unsigned int newCapacity = fMapCapacity ?
                                     (unsigned int)(fMapCapacity * 1.25) : 16;
    PrefMapElem* newMap = (PrefMapElem*) fMemoryManager->allocate
    (
        newCapacity * sizeof(PrefMapElem)
    );//new PrefMapElem[newCapacity];

    //
    //  Copy over the old stuff. We DON'T have to zero out the new stuff
    //  since this is a by value map and the current map index controls what
    //  is relevant.
    //
    if (fMapCapacity) {

        memcpy(newMap, fMap, fMapCapacity * sizeof(PrefMapElem));
        fMemoryManager->deallocate(fMap);//delete [] fMap;
    }

    fMap = newMap;
    fMapCapacity = newCapacity;
}

void WFElemStack::expandStack()
{
    // Expand the capacity by 25% and allocate a new buffer
    const unsigned int newCapacity = (unsigned int)(fStackCapacity * 1.25);
    StackElem** newStack = (StackElem**) fMemoryManager->allocate
    (
        newCapacity * sizeof(StackElem*)
    );//new StackElem*[newCapacity];

    // Copy over the old stuff
    memcpy(newStack, fStack, fStackCapacity * sizeof(StackElem*));

    //
    //  And zero out the new stuff. Though we use a stack top, we reuse old
    //  stack contents so we need to know if elements have been initially
    //  allocated or not as we push new stuff onto the stack.
    //
    memset
    (
        &newStack[fStackCapacity]
        , 0
        , (newCapacity - fStackCapacity) * sizeof(StackElem*)
    );

    // Delete the old array and update our members
    fMemoryManager->deallocate(fStack);//delete [] fStack;
    fStack = newStack;
    fStackCapacity = newCapacity;
}


XERCES_CPP_NAMESPACE_END
