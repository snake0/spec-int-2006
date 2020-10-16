/*
 * The Apache Software License, Version 1.1
 *
 * Copyright (c) 1999-2002 The Apache Software Foundation.  All rights
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
 * $Id: DOM_DOMImplementation.cpp,v 1.5 2003/05/22 02:26:50 knoaman Exp $
 */

#include "DOM_DOMImplementation.hpp"
#include "DOM_Document.hpp"
#include "DOM_DocumentType.hpp"
#include "DOM_DOMException.hpp"
#include "DocumentImpl.hpp"
#include "DocumentTypeImpl.hpp"
#include "DStringPool.hpp"
#include <xercesc/util/XMLChar.hpp>
#include <xercesc/util/PlatformUtils.hpp>
#include <xercesc/util/XMLRegisterCleanup.hpp>

XERCES_CPP_NAMESPACE_BEGIN


//
//  Static constants.  These are lazily initialized on first usage.
//                     (Static constructors can not be safely used because
//                      of order of initialization dependencies.)


static DOM_DOMImplementation    *gDomimp;   // Points to the singleton instance
                                            //  of DOMImplementation that is returned
                                            //  by any call to getImplementation().

static DOMString                *gXML = 0;      // Points to "XML"
static DOMString                *g1_0 = 0;      // Points to "1.0"
static DOMString                *g2_0 = 0;      // Points to "2.0"
static DOMString                *gTrav  = 0;     // Points to "Traversal"
static DOMString                *gRange = 0;     // Points to "Range"
static DOMString                *gCore  = 0;     // Points to "Core"

//
// we define only one clean up object, if any of the above
// ever get initialized, then the cleanup function will be
// registered to the same cleanup Obj again and again.
//
// that cleanup function will delete/null all the g*
//
static XMLRegisterCleanup DOM_DOMImplementationCleanup;

// Note #1136 - There needs to be a separate implementation class for
//              DOMImplementation, so that the user programming model
//              is consistent with the rest of the C++ DOM API, and
//              so that hasFeature will only work on the result of
//              getImplementation(), and not on DOM_DOMImplemenation objects
//              created with the default constructor.
//
DOM_DOMImplementation::DOM_DOMImplementation() {
}



DOM_DOMImplementation::DOM_DOMImplementation(const DOM_DOMImplementation & other)
{
}


DOM_DOMImplementation::~DOM_DOMImplementation()
{
}


DOM_DOMImplementation & DOM_DOMImplementation::operator = (const DOM_DOMImplementation & other)
{
    return *this;
}

// -----------------------------------------------------------------------
//  Reset the singleton DOM_DOMImplementation
// -----------------------------------------------------------------------
static void reinitImplementation() {
	delete gDomimp;
	gDomimp = 0;
}

//  getImplementation()  - Always returns the same singleton instance, which
//                         is lazily created on the first call.  Note that
//                         DOM_Implementation must be thread-safe because
//                         it is common to all DOM documents, and while a single
//                         document is not thread-safe within itself, we do
//                         promise that different documents can safely be
//                         used concurrently by different threads.
//
DOM_DOMImplementation &DOM_DOMImplementation::getImplementation() {
	static XMLRegisterCleanup implementationCleanup;

    if (gDomimp == 0)
    {
        DOM_DOMImplementation *t = new DOM_DOMImplementation;
        if (XMLPlatformUtils::compareAndSwap((void **)&gDomimp, t, 0) != 0)
        {
            delete t;
        }
        else
        {
			implementationCleanup.registerCleanup(reinitImplementation);
        }

    }
    return *gDomimp;
}

bool  DOM_DOMImplementation::hasFeature(const DOMString &feature,  const DOMString &version)
{
    bool anyVersion = (version == null || version.length() == 0);
    bool version1_0 = version.equals(DStringPool::getStaticString("1.0"
                                                     , &g1_0
                                                     , reinitDOM_DOMImplementation
                                                     , DOM_DOMImplementationCleanup));
    bool version2_0 = version.equals(DStringPool::getStaticString("2.0"
                                                     , &g2_0
                                                     , reinitDOM_DOMImplementation
                                                     , DOM_DOMImplementationCleanup));

    // case-insensitive compare
    if(!XMLString::compareIString(feature.rawBuffer(), DStringPool::getStaticString("XML"
                                                           , &gXML
                                                           , reinitDOM_DOMImplementation
                                                           , DOM_DOMImplementationCleanup).rawBuffer())
       && (anyVersion || version1_0 || version2_0))
        return true;

    if(!XMLString::compareIString(feature.rawBuffer(), DStringPool::getStaticString("Core"
                                                           , &gCore
                                                           , reinitDOM_DOMImplementation
                                                           , DOM_DOMImplementationCleanup).rawBuffer())
       && (anyVersion || version1_0 || version2_0))
        return true;

    if(!XMLString::compareIString(feature.rawBuffer(), DStringPool::getStaticString("Traversal"
                                                           , &gTrav
                                                           , reinitDOM_DOMImplementation
                                                           , DOM_DOMImplementationCleanup).rawBuffer())
       && (anyVersion || version2_0))
        return true;

    if(!XMLString::compareIString(feature.rawBuffer(), DStringPool::getStaticString("Range"
                                                           , &gRange
                                                           , reinitDOM_DOMImplementation
                                                           , DOM_DOMImplementationCleanup).rawBuffer())
       && (anyVersion || version2_0))
        return true;


    return false;
}


//Introduced in DOM Level 2

DOM_DocumentType DOM_DOMImplementation::createDocumentType(const DOMString &qualifiedName,
	const DOMString &publicId, const DOMString &systemId)
{    
    if(!XMLChar1_0::isValidName(qualifiedName.rawBuffer(), XMLString::stringLen(qualifiedName.rawBuffer())))
        throw DOM_DOMException(DOM_DOMException::INVALID_CHARACTER_ERR,null);
    return DOM_DocumentType(new DocumentTypeImpl(null, qualifiedName, publicId, systemId));
}

DOM_Document DOM_DOMImplementation::createDocument(const DOMString &namespaceURI,
	const DOMString &qualifiedName, const DOM_DocumentType &doctype, MemoryManager* const manager)
{
    return DOM_Document(new (manager) DocumentImpl(namespaceURI, qualifiedName,
	doctype == null ? null : (DocumentTypeImpl *) doctype.fImpl, manager));
}

// -----------------------------------------------------------------------
//  Notification that lazy data has been deleted
// -----------------------------------------------------------------------
void DOM_DOMImplementation::reinitDOM_DOMImplementation() {

    delete gXML;
    gXML = 0;

    delete g1_0;
    g1_0 = 0;

    delete g2_0;
    g2_0 = 0;

    delete gTrav;
    gTrav = 0;

    delete gRange;
    gRange = 0;

    delete gCore;
    gCore = 0;

}

XERCES_CPP_NAMESPACE_END

