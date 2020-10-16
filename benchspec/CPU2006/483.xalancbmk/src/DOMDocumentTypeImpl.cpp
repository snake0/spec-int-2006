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
 * $Id: DOMDocumentTypeImpl.cpp,v 1.23 2004/01/29 11:44:26 cargilld Exp $
 */

#include "DOMDocumentTypeImpl.hpp"
#include <xercesc/dom/DOMNode.hpp>
#include <xercesc/dom/DOMException.hpp>
#include <xercesc/dom/DOMImplementationRegistry.hpp>
#include <xercesc/dom/DOMImplementation.hpp>
#include <xercesc/util/XMLUniDefs.hpp>
#include <xercesc/util/XMLChar.hpp>
#include <xercesc/util/XMLRegisterCleanup.hpp>

#include "DOMNamedNodeMapImpl.hpp"
#include "DOMDocumentImpl.hpp"
#include "DOMCasts.hpp"

XERCES_CPP_NAMESPACE_BEGIN

// ---------------------------------------------------------------------------
//  Local static data
// ---------------------------------------------------------------------------

static DOMDocument*       sDocument = 0;
static XMLRegisterCleanup documentCleanup;

static void reinitDocument()
{
    if (sDocument) {
        sDocument->release();
        sDocument = 0;
    }
}

static DOMDocument& gDocTypeDocument()
{
    if (!sDocument)
    {
        static const XMLCh gCoreStr[] = { chLatin_C, chLatin_o, chLatin_r, chLatin_e, chNull };
        DOMImplementation* impl =  DOMImplementationRegistry::getDOMImplementation(gCoreStr);
        DOMDocument* tmpDoc = impl->createDocument();                   // document type object (DTD).

        if (XMLPlatformUtils::compareAndSwap((void**)&sDocument, tmpDoc, 0))
        {
            // Someone beat us to it, so let's clean up ours
            delete tmpDoc;
        }
        else
        {
            documentCleanup.registerCleanup(reinitDocument);
        }
    }

    return *sDocument;
}


DOMDocumentTypeImpl::DOMDocumentTypeImpl(DOMDocument *ownerDoc,
                                   const XMLCh *dtName,
                                   bool heap)
    : fNode(ownerDoc),
    fParent(ownerDoc),
    fName(0),
    fEntities(0),
    fNotations(0),
    fElements(0),
    fPublicId(0),
    fSystemId(0),
    fInternalSubset(0),
    fIntSubsetReading(false),
    fIsCreatedFromHeap(heap)
{
    if (ownerDoc) {
        fName = ((DOMDocumentImpl *)ownerDoc)->getPooledString(dtName);
        fEntities = new (ownerDoc) DOMNamedNodeMapImpl(this);
        fNotations= new (ownerDoc) DOMNamedNodeMapImpl(this);
        fElements = new (ownerDoc) DOMNamedNodeMapImpl(this);
    }
    else {
        DOMDocument* doc = &gDocTypeDocument();
        fName = ((DOMDocumentImpl *)doc)->getPooledString(dtName);
        fEntities = new (doc) DOMNamedNodeMapImpl(this);
        fNotations= new (doc) DOMNamedNodeMapImpl(this);
        fElements = new (doc) DOMNamedNodeMapImpl(this);
    }
}


//Introduced in DOM Level 2
DOMDocumentTypeImpl::DOMDocumentTypeImpl(DOMDocument *ownerDoc,
                                   const XMLCh *qualifiedName,
                                   const XMLCh *pubId,
                                   const XMLCh *sysId,
                                   bool heap)
    : fNode(ownerDoc),
    fParent(ownerDoc),
    fName(0),
    fEntities(0),
    fNotations(0),
    fElements(0),
    fPublicId(0),
    fSystemId(0),
    fInternalSubset(0),    
    fIntSubsetReading(false),        
    fIsCreatedFromHeap(heap)
{
    int index = DOMDocumentImpl::indexofQualifiedName(qualifiedName);
    if (index < 0)
        throw DOMException(DOMException::NAMESPACE_ERR, 0);
    else if (index > 0)
    {
        // we have to make sure the qualifiedName has correct prefix and localName
        // although we don't really to store them separately
        XMLCh* newName;
        XMLCh temp[4000];
        if (index >= 3999)
            newName = (XMLCh*) XMLPlatformUtils::fgMemoryManager->allocate
            (
                (XMLString::stringLen(qualifiedName)+1) * sizeof(XMLCh)
            );//new XMLCh[XMLString::stringLen(qualifiedName)+1];
        else
            newName = temp;

        XMLString::copyNString(newName, qualifiedName, index);
        newName[index] = chNull;

        // Before we carry on, we should check if the prefix or localName are valid XMLName
        if (ownerDoc) {
            if (!((DOMDocumentImpl*)ownerDoc)->isXMLName(newName) || !((DOMDocumentImpl*)ownerDoc)->isXMLName(qualifiedName+index+1))
                throw DOMException(DOMException::NAMESPACE_ERR, 0);
        }
        else {
            // document is not there yet, so assume XML 1.0
            if (!XMLChar1_0::isValidName(newName, index) || !XMLChar1_0::isValidName(qualifiedName+index+1, XMLString::stringLen(qualifiedName)-index-1))
                throw DOMException(DOMException::NAMESPACE_ERR, 0);
        }

        if (index >= 3999)
            XMLPlatformUtils::fgMemoryManager->deallocate(newName);//delete[] newName;
    }

    if (ownerDoc) {
        DOMDocumentImpl *docImpl = (DOMDocumentImpl *)ownerDoc;
        fPublicId = docImpl->cloneString(pubId);
        fSystemId = docImpl->cloneString(sysId);
        fName = ((DOMDocumentImpl *)ownerDoc)->getPooledString(qualifiedName);
        fEntities = new (ownerDoc) DOMNamedNodeMapImpl(this);
        fNotations= new (ownerDoc) DOMNamedNodeMapImpl(this);
        fElements = new (ownerDoc) DOMNamedNodeMapImpl(this);
    }
    else {
        DOMDocument* doc = &gDocTypeDocument();
        fPublicId = ((DOMDocumentImpl*) doc)->cloneString(pubId);
        fSystemId = ((DOMDocumentImpl*) doc)->cloneString(sysId);
        fName = ((DOMDocumentImpl*) doc)->getPooledString(qualifiedName);
        fEntities = new (doc) DOMNamedNodeMapImpl(this);
        fNotations= new (doc) DOMNamedNodeMapImpl(this);
        fElements = new (doc) DOMNamedNodeMapImpl(this);
    }
}


DOMDocumentTypeImpl::DOMDocumentTypeImpl(const DOMDocumentTypeImpl &other, bool heap, bool deep)
    : fNode(other.fNode),
    fParent(other.fParent),
    fChild(other.fChild),
    fName(0),
    fEntities(0),
    fNotations(0),
    fElements(0),
    fPublicId(0),
    fSystemId(0),
    fInternalSubset(0),
    fIntSubsetReading(other.fIntSubsetReading),       
    fIsCreatedFromHeap(heap)
{
    fName = other.fName;

    //DOM Level 2
    fPublicId        = other.fPublicId;
    fSystemId        = other.fSystemId;
    fInternalSubset  = other.fInternalSubset;

    if ((DOMDocumentImpl *)this->fNode.getOwnerDocument() && deep)
        fParent.cloneChildren(&other);

    fEntities = ((DOMNamedNodeMapImpl *)other.fEntities)->cloneMap(this);
    fNotations= ((DOMNamedNodeMapImpl *)other.fNotations)->cloneMap(this);
    fElements = ((DOMNamedNodeMapImpl *)other.fElements)->cloneMap(this);
}


DOMDocumentTypeImpl::~DOMDocumentTypeImpl()
{
}


DOMNode *DOMDocumentTypeImpl::cloneNode(bool deep) const
{
    DOMNode* newNode = 0;
    if (castToNodeImpl(this)->getOwnerDocument())
        newNode = new (castToNodeImpl(this)->getOwnerDocument(), DOMDocumentImpl::DOCUMENT_TYPE_OBJECT) DOMDocumentTypeImpl(*this, false, deep);
    else
        newNode = new (&gDocTypeDocument(), DOMDocumentImpl::DOCUMENT_TYPE_OBJECT) DOMDocumentTypeImpl(*this, false, deep);

    fNode.callUserDataHandlers(DOMUserDataHandler::NODE_CLONED, this, newNode);
    return newNode;
}

/**
 * NON-DOM
 * set the ownerDocument of this node and its children
 */
void DOMDocumentTypeImpl::setOwnerDocument(DOMDocument *doc) {

    if (castToNodeImpl(this)->getOwnerDocument()) {
        fNode.setOwnerDocument(doc);
        fParent.setOwnerDocument(doc);
    }
    else {
        if (doc) {
            DOMDocumentImpl *docImpl = (DOMDocumentImpl *)doc;

            fPublicId = docImpl->cloneString(fPublicId);
            fSystemId = docImpl->cloneString(fSystemId);
            fInternalSubset = docImpl->cloneString(fInternalSubset);
            fName = docImpl->getPooledString(fName);
            
            fNode.setOwnerDocument(doc);
            fParent.setOwnerDocument(doc);

            DOMNamedNodeMap* entitiesTemp = ((DOMNamedNodeMapImpl *)fEntities)->cloneMap(this);
            DOMNamedNodeMap* notationsTemp = ((DOMNamedNodeMapImpl *)fNotations)->cloneMap(this);
            DOMNamedNodeMap* elementsTemp = ((DOMNamedNodeMapImpl *)fElements)->cloneMap(this);

            fEntities = entitiesTemp;
            fNotations = notationsTemp;
            fElements = elementsTemp;
        }
    }
}

const XMLCh * DOMDocumentTypeImpl::getNodeName() const
{
    return fName;
}


short DOMDocumentTypeImpl::getNodeType()  const {
    return DOMNode::DOCUMENT_TYPE_NODE;
}


DOMNamedNodeMap *DOMDocumentTypeImpl::getEntities() const
{
    return fEntities;
}



const XMLCh * DOMDocumentTypeImpl::getName() const
{
    return fName;
}


DOMNamedNodeMap *DOMDocumentTypeImpl::getNotations() const
{
    return fNotations;
}


DOMNamedNodeMap *DOMDocumentTypeImpl::getElements() const
{
    return fElements;
}


void DOMDocumentTypeImpl::setNodeValue(const XMLCh *val)
{
    fNode.setNodeValue(val);
}


void DOMDocumentTypeImpl::setReadOnly(bool readOnl, bool deep)
{
    fNode.setReadOnly(readOnl,deep);
    if (fEntities)
        ((DOMNamedNodeMapImpl *)fEntities)->setReadOnly(readOnl,true);
    if (fNotations)
        ((DOMNamedNodeMapImpl *)fNotations)->setReadOnly(readOnl,true);
}


//Introduced in DOM Level 2

const XMLCh * DOMDocumentTypeImpl::getPublicId() const
{
    return fPublicId;
}


const XMLCh * DOMDocumentTypeImpl::getSystemId() const
{
    return fSystemId;
}


const XMLCh * DOMDocumentTypeImpl::getInternalSubset() const
{
    return fInternalSubset;
}

bool DOMDocumentTypeImpl::isIntSubsetReading() const
{
    return fIntSubsetReading;
}


//set functions

void DOMDocumentTypeImpl::setPublicId(const XMLCh *value)
{
    // revist.  Why shouldn't 0 be assigned like any other value?
    if (value == 0)
        return;

    if ((DOMDocumentImpl *)castToNodeImpl(this)->getOwnerDocument())
        fPublicId = ((DOMDocumentImpl *)castToNodeImpl(this)->getOwnerDocument())->cloneString(value);
    else {
        fPublicId = ((DOMDocumentImpl *)&gDocTypeDocument())->cloneString(value);
    }
}

void DOMDocumentTypeImpl::setSystemId(const XMLCh *value)
{
    if ((DOMDocumentImpl *)castToNodeImpl(this)->getOwnerDocument())
        fSystemId = ((DOMDocumentImpl *)castToNodeImpl(this)->getOwnerDocument())->cloneString(value);
    else {
        fSystemId = ((DOMDocumentImpl *)&gDocTypeDocument())->cloneString(value);
    }
}

void DOMDocumentTypeImpl::setInternalSubset(const XMLCh *value)
{
    if ((DOMDocumentImpl *)castToNodeImpl(this)->getOwnerDocument())
        fInternalSubset = ((DOMDocumentImpl *)castToNodeImpl(this)->getOwnerDocument())->cloneString(value);
    else {
        fInternalSubset = ((DOMDocumentImpl *)&gDocTypeDocument())->cloneString(value);
    }
}

void DOMDocumentTypeImpl::release()
{
    if (fNode.isOwned()) {
        if (fNode.isToBeReleased()) {
            // we are calling from documnet.release() which will notify the user data handler
            if (fIsCreatedFromHeap) {
                DOMDocumentType* docType = this;
                delete docType;
            }
        }
        else
            throw DOMException(DOMException::INVALID_ACCESS_ERR,0);
    }
    else {
        if (fIsCreatedFromHeap) {
            fNode.callUserDataHandlers(DOMUserDataHandler::NODE_DELETED, 0, 0);
            DOMDocumentType* docType = this;
            delete docType;
        }
        else {
            DOMDocumentImpl* doc = (DOMDocumentImpl*) getOwnerDocument();
            if (doc) {
                fNode.callUserDataHandlers(DOMUserDataHandler::NODE_DELETED, 0, 0);
                doc->release(this, DOMDocumentImpl::DOCUMENT_TYPE_OBJECT);
            }
            else {
                // shouldn't reach here
                throw DOMException(DOMException::INVALID_ACCESS_ERR,0);
            }
        }
    }
}


//
// Delegation for functions inherited from Node
//

           DOMNode*         DOMDocumentTypeImpl::appendChild(DOMNode *newChild)          {return fParent.appendChild (newChild); }
           DOMNamedNodeMap* DOMDocumentTypeImpl::getAttributes() const                   {return fNode.getAttributes (); }
           DOMNodeList*     DOMDocumentTypeImpl::getChildNodes() const                   {return fParent.getChildNodes (); }
           DOMNode*         DOMDocumentTypeImpl::getFirstChild() const                   {return fParent.getFirstChild (); }
           DOMNode*         DOMDocumentTypeImpl::getLastChild() const                    {return fParent.getLastChild (); }
     const XMLCh*           DOMDocumentTypeImpl::getLocalName() const                    {return fNode.getLocalName (); }
     const XMLCh*           DOMDocumentTypeImpl::getNamespaceURI() const                 {return fNode.getNamespaceURI (); }
           DOMNode*         DOMDocumentTypeImpl::getNextSibling() const                  {return fChild.getNextSibling (); }
     const XMLCh*           DOMDocumentTypeImpl::getNodeValue() const                    {return fNode.getNodeValue (); }
           DOMDocument*     DOMDocumentTypeImpl::getOwnerDocument() const                {return fParent.fOwnerDocument; }
     const XMLCh*           DOMDocumentTypeImpl::getPrefix() const                       {return fNode.getPrefix (); }
           DOMNode*         DOMDocumentTypeImpl::getParentNode() const                   {return fChild.getParentNode (this); }
           DOMNode*         DOMDocumentTypeImpl::getPreviousSibling() const              {return fChild.getPreviousSibling (this); }
           bool             DOMDocumentTypeImpl::hasChildNodes() const                   {return fParent.hasChildNodes (); }
           DOMNode*         DOMDocumentTypeImpl::insertBefore(DOMNode *newChild, DOMNode *refChild)
                                                                                         {return fParent.insertBefore (newChild, refChild); }
           void             DOMDocumentTypeImpl::normalize()                             {fParent.normalize (); }
           DOMNode*         DOMDocumentTypeImpl::removeChild(DOMNode *oldChild)          {return fParent.removeChild (oldChild); }
           DOMNode*         DOMDocumentTypeImpl::replaceChild(DOMNode *newChild, DOMNode *oldChild)
                                                                                         {return fParent.replaceChild (newChild, oldChild); }
           bool             DOMDocumentTypeImpl::isSupported(const XMLCh *feature, const XMLCh *version) const
                                                                                         {return fNode.isSupported (feature, version); }
           void             DOMDocumentTypeImpl::setPrefix(const XMLCh  *prefix)         {fNode.setPrefix(prefix); }
           bool             DOMDocumentTypeImpl::hasAttributes() const                   {return fNode.hasAttributes(); }
           bool             DOMDocumentTypeImpl::isSameNode(const DOMNode* other) const  {return fNode.isSameNode(other); }
           void*            DOMDocumentTypeImpl::setUserData(const XMLCh* key, void* data, DOMUserDataHandler* handler)
                                                                                         {return fNode.setUserData(key, data, handler); }
           void*            DOMDocumentTypeImpl::getUserData(const XMLCh* key) const     {return fNode.getUserData(key); }
           const XMLCh*     DOMDocumentTypeImpl::getBaseURI() const                      {return fNode.getBaseURI(); }
           short            DOMDocumentTypeImpl::compareTreePosition(const DOMNode* other) const {return fNode.compareTreePosition(other); }
           const XMLCh*     DOMDocumentTypeImpl::getTextContent() const                  {return fNode.getTextContent(); }
           void             DOMDocumentTypeImpl::setTextContent(const XMLCh* textContent){fNode.setTextContent(textContent); }
           const XMLCh*     DOMDocumentTypeImpl::lookupNamespacePrefix(const XMLCh* namespaceURI, bool useDefault) const  {return fNode.lookupNamespacePrefix(namespaceURI, useDefault); }
           bool             DOMDocumentTypeImpl::isDefaultNamespace(const XMLCh* namespaceURI) const {return fNode.isDefaultNamespace(namespaceURI); }
           const XMLCh*     DOMDocumentTypeImpl::lookupNamespaceURI(const XMLCh* prefix) const  {return fNode.lookupNamespaceURI(prefix); }
           DOMNode*         DOMDocumentTypeImpl::getInterface(const XMLCh* feature)      {return fNode.getInterface(feature); }


bool DOMDocumentTypeImpl::isEqualNode(const DOMNode* arg) const
{
    if (isSameNode(arg)) {
        return true;
    }

    if (!fNode.isEqualNode(arg)) {
        return false;
    }

    DOMDocumentType* argDT = (DOMDocumentType*) arg;
    // check the string values
    if (!getPublicId()) {
        if (argDT->getPublicId()) {
            return false;
        }
    }
    else if (!XMLString::equals(getPublicId(), argDT->getPublicId())) {
        return false;
    }

    if (!getSystemId()) {
        if (argDT->getSystemId()) {
            return false;
        }
    }
    else if (!XMLString::equals(getSystemId(), argDT->getSystemId())) {
        return false;
    }

    if (!getInternalSubset()) {
        if (argDT->getInternalSubset()) {
            return false;
        }
    }
    else if (!XMLString::equals(getInternalSubset(), argDT->getInternalSubset())) {
        return false;
    }

    // check the notations
    if (getNotations()) {
        if (!argDT->getNotations())
            return false;

        DOMNamedNodeMap* map1 = getNotations();
        DOMNamedNodeMap* map2 = argDT->getNotations();

        XMLSize_t len = map1->getLength();
        if (len != map2->getLength()) {
            return false;
        }
        for (XMLSize_t i = 0; i < len; i++) {
            DOMNode* n1 = map1->item(i);
            DOMNode* n2 = map2->getNamedItem(n1->getNodeName());
            if (!n2 || !n1->isEqualNode(n2)) {
                return false;
            }
        }
    }
    else {
        if (argDT->getNotations())
            return false;
    }

    // check the entities
    if (getEntities()) {
        if (!argDT->getEntities())
            return false;

        DOMNamedNodeMap* map1 = getEntities();
        DOMNamedNodeMap* map2 = argDT->getEntities();

        XMLSize_t len = map1->getLength();
        if (len != map2->getLength()) {
            return false;
        }
        for (XMLSize_t i = 0; i < len; i++) {
            DOMNode* n1 = map1->item(i);
            DOMNode* n2 = map2->getNamedItem(n1->getNodeName());
            if (!n2 || !n1->isEqualNode(n2)) {
                return false;
            }
        }
    }
    else {
        if (argDT->getEntities())
            return false;
    }

    return fParent.isEqualNode(arg);
}

XERCES_CPP_NAMESPACE_END

