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
 * $Log: Match.cpp,v $
 * Revision 1.4  2003/05/16 00:03:10  knoaman
 * Partial implementation of the configurable memory manager.
 *
 * Revision 1.3  2002/12/18 13:01:02  gareth
 * New functionality - tokenize and replace. Fixed REVISIT for case insensitive match. Patch by Jennifer Schachter.
 *
 * Revision 1.2  2002/11/04 15:17:00  tng
 * C++ Namespace Support.
 *
 * Revision 1.1.1.1  2002/02/01 22:22:29  peiyongz
 * sane_include
 *
 * Revision 1.2  2001/05/11 13:26:42  tng
 * Copyright update.
 *
 * Revision 1.1  2001/03/02 19:22:40  knoaman
 * Schema: Regular expression handling part I
 *
 */

// ---------------------------------------------------------------------------
//  Includes
// ---------------------------------------------------------------------------
#include <xercesc/util/regx/Match.hpp>
#include <xercesc/framework/MemoryManager.hpp>

XERCES_CPP_NAMESPACE_BEGIN

// ---------------------------------------------------------------------------
//  Match: Constructors and Destructors
// ---------------------------------------------------------------------------
Match::Match(MemoryManager* const manager) :
    fNoGroups(0)
    , fPositionsSize(0)
    , fStartPositions(0)
    , fEndPositions(0)
    , fMemoryManager(manager)
{

}

Match::Match(const Match& toCopy) :
    fNoGroups(0)
    , fPositionsSize(0)
    , fStartPositions(0)
    , fEndPositions(0)
    , fMemoryManager(0)
{
  initialize(toCopy);
}

Match& Match::operator=(const Match& toAssign){
  
  initialize(toAssign);
  return *this;
}


Match::~Match() {

	cleanUp();
}

// ---------------------------------------------------------------------------
//  Match: Setter Methods
// ---------------------------------------------------------------------------
void Match::setNoGroups(const int n) {

	if (fNoGroups <= 0 || fPositionsSize < n) {

		cleanUp();
		fPositionsSize = n;
		fStartPositions = (int*) fMemoryManager->allocate(n * sizeof(int));//new int[n];
		fEndPositions = (int*) fMemoryManager->allocate(n * sizeof(int));//new int[n];
	}

	fNoGroups = n;

	for (int i=0; i< fPositionsSize; i++) {

		fStartPositions[i] = -1;
		fEndPositions[i] = -1;
	}
}

// ---------------------------------------------------------------------------
//  Match: private helpers methods
// ---------------------------------------------------------------------------
void Match::initialize(const Match &toCopy){

  //do not copy over value of fPositionSize as it is irrelevant to the 
  //state of the Match

  fMemoryManager = toCopy.fMemoryManager;
  int toCopySize = toCopy.getNoGroups();
  setNoGroups(toCopySize);

  for (int i=0; i<toCopySize; i++){
    setStartPos(i, toCopy.getStartPos(i));
    setEndPos(i, toCopy.getEndPos(i));
  }           

}

void Match::cleanUp() {

	fMemoryManager->deallocate(fStartPositions);//delete [] fStartPositions;
	fMemoryManager->deallocate(fEndPositions);//delete [] fEndPositions;

	fStartPositions = 0;
	fEndPositions = 0;
}

XERCES_CPP_NAMESPACE_END

/**
  * End of file Match.cpp
  */
