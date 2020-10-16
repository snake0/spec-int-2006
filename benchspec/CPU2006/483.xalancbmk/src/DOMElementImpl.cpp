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
 * $Id: DOMElementImpl.cpp,v 1.23 2004/01/29 11:44:26 cargilld Exp $
 */

#include "DOMElementImpl.hpp"

#include <xercesc/dom/DOMAttr.hpp>
#include <xercesc/dom/DOMDocument.hpp>
#include <xercesc/dom/DOMException.hpp>
#include <xercesc/util/XMLUniDefs.hpp>
#include <xercesc/util/XMLUri.hpp>

#include "DOMAttrMapImpl.hpp"
#include "DOMAttrImpl.hpp"
#include "DOMDocumentImpl.hpp"
#include "DOMParentNode.hpp"
#include "DOMStringPool.hpp"
#include "DOMCasts.hpp"
#include "DOMElementNSImpl.hpp"
#include "DOMTypeInfoImpl.hpp"

#include "DOMDeepNodeListImpl.hpp"
#include "DOMDocumentTypeImpl.hpp"
#include "DOMNamedNodeMapImpl.hpp"
#include <xercesc/util/OutOfMemoryException.hpp>

XERCES_CPP_NAMESPACE_BEGIN


class DOMAttr;

DOMElementImpl::DOMElementImpl(DOMDocument *ownerDoc, const XMLCh *eName)
    : fNode(ownerDoc), fParent(ownerDoc), fAttributes(0), fDefaultAttributes(0), fSchemaType(0)
{
    DOMDocumentImpl *docImpl = (DOMDocumentImpl *)ownerDoc;
    fName = docImpl->getPooledString(eName);
    setupDefaultAttributes();
    if (!fDefaultAttributes) {
        fDefaultAttributes = new (getOwnerDocument()) DOMAttrMapImpl(this);
        fAttributes = new (getOwnerDocument()) DOMAttrMapImpl(this);
    }
    else {
        fAttributes = new (getOwnerDocument()) DOMAttrMapImpl(this, fDefaultAttributes);
    }
}


DOMElementImpl::DOMElementImpl(const DOMElementImpl &other, bool deep)
    : fNode(other.getOwnerDocument()),
      fParent(other.getOwnerDocument()),
      fAttributes(0),
      fDefaultAttributes(0),
      fSchemaType(other.fSchemaType)      
{
    fName = other.fName;

    if (deep)
        fParent.cloneChildren(&other);

    if (other.getAttributes())
    {
        fAttributes = ((DOMAttrMapImpl *)other.getAttributes())->cloneAttrMap(this);
    }

    if (other.getDefaultAttributes())
    {
        fDefaultAttributes = ((DOMAttrMapImpl *)other.getDefaultAttributes())->cloneAttrMap(this);
    }

    if (!fDefaultAttributes)
        setupDefaultAttributes();

    if (!fDefaultAttributes)
        fDefaultAttributes = new (getOwnerDocument()) DOMAttrMapImpl(this);

    if (!fAttributes) {
        if (!fDefaultAttributes) {
            fAttributes = new (getOwnerDocument()) DOMAttrMapImpl(this);
        }
        else {
            fAttributes = new (getOwnerDocument()) DOMAttrMapImpl(this, fDefaultAttributes);
        }
    }

}


DOMElementImpl::~DOMElementImpl()
{
}


DOMNode *DOMElementImpl::cloneNode(bool deep) const
{
    DOMNode* newNode = new (getOwnerDocument(), DOMDocumentImpl::ELEMENT_OBJECT) DOMElementImpl(*this, deep);
    fNode.callUserDataHandlers(DOMUserDataHandler::NODE_CLONED, this, newNode);
    return newNode;
}




const XMLCh * DOMElementImpl::getNodeName() const {
    return fName;
}


short DOMElementImpl::getNodeType() const {
    return DOMNode::ELEMENT_NODE;
}


const XMLCh * DOMElementImpl::getAttribute(const XMLCh *nam) const
{
    DOMNode * attr = fAttributes->getNamedItem(nam);
    if (attr)
        return attr->getNodeValue();

    return XMLUni::fgZeroLenString;
}



DOMAttr *DOMElementImpl::getAttributeNode(const XMLCh *nam) const
{
    return  (DOMAttr *)fAttributes->getNamedItem(nam);
}


DOMNamedNodeMap *DOMElementImpl::getAttributes() const
{
    DOMElementImpl *ncThis = (DOMElementImpl *)this;   // cast off const
    return ncThis->fAttributes;
}



DOMNodeList *DOMElementImpl::getElementsByTagName(const XMLCh *tagname) const
{
    DOMDocumentImpl *docImpl = (DOMDocumentImpl *)getOwnerDocument();
    return docImpl->getDeepNodeList(this,tagname);
}


const XMLCh * DOMElementImpl::getTagName() const
{
    return fName;
}


void DOMElementImpl::removeAttribute(const XMLCh *nam)
{
    if (fNode.isReadOnly())
        throw DOMException(
             DOMException::NO_MODIFICATION_ALLOWED_ERR, 0);

    XMLSSize_t i = fAttributes->findNamePoint(nam);
    if (i >= 0)
    {
        DOMNode *att = fAttributes->removeNamedItemAt(i);
        ((DOMAttrImpl *)att)->removeAttrFromIDNodeMap();
        att->release();
    }
}



DOMAttr *DOMElementImpl::removeAttributeNode(DOMAttr *oldAttr)
{
    if (fNode.isReadOnly())
        throw DOMException(
        DOMException::NO_MODIFICATION_ALLOWED_ERR, 0);

    DOMNode* found = 0;

    // Since there is no removeAttributeNodeNS, check if this oldAttr has NS or not
    const XMLCh* localName = oldAttr->getLocalName();
    XMLSSize_t i = 0;
    if (localName)
        i = fAttributes->findNamePoint(oldAttr->getNamespaceURI(), localName);
    else
        i = fAttributes->findNamePoint(oldAttr->getName());

    if (i >= 0) {
        // If it is in fact the right object, remove it.
        found = fAttributes->item(i);
        if (found == oldAttr) {
            fAttributes->removeNamedItemAt(i);
            ((DOMAttrImpl *)oldAttr)->removeAttrFromIDNodeMap();
        }
        else
            throw DOMException(DOMException::NOT_FOUND_ERR, 0);

    }
    else
        throw DOMException(DOMException::NOT_FOUND_ERR, 0);

   return (DOMAttr *)found;
}



void DOMElementImpl::setAttribute(const XMLCh *nam, const XMLCh *val)
{
    if (fNode.isReadOnly())
        throw DOMException(
        DOMException::NO_MODIFICATION_ALLOWED_ERR, 0);

    DOMAttr* newAttr = getAttributeNode(nam);
    if (!newAttr)
    {
        newAttr = this->fNode.getOwnerDocument()->createAttribute(nam);
        fAttributes->setNamedItem(newAttr);
    }

    newAttr->setNodeValue(val);
}

void DOMElementImpl::setIdAttribute(const XMLCh* name)
{
    if (fNode.isReadOnly())
        throw DOMException(
        DOMException::NO_MODIFICATION_ALLOWED_ERR, 0);

    DOMAttr *attr = getAttributeNode(name);

    if (!attr) 
        throw DOMException(DOMException::NOT_FOUND_ERR, 0);

    ((DOMAttrImpl *)attr)->addAttrToIDNodeMap();
}

void DOMElementImpl::setIdAttributeNS(const XMLCh* namespaceURI, const XMLCh* localName) {

    if (fNode.isReadOnly())
        throw DOMException(
        DOMException::NO_MODIFICATION_ALLOWED_ERR, 0);

    DOMAttr *attr = getAttributeNodeNS(namespaceURI, localName);

    if (!attr) 
        throw DOMException(DOMException::NOT_FOUND_ERR, 0);

    ((DOMAttrImpl *)attr)->addAttrToIDNodeMap();

}


void DOMElementImpl::setIdAttributeNode(const DOMAttr *idAttr) {

    if (fNode.isReadOnly())
        throw DOMException(
        DOMException::NO_MODIFICATION_ALLOWED_ERR, 0);

    DOMAttr *attr;
    const XMLCh* localName = idAttr->getLocalName();
    if (localName)
        attr = getAttributeNodeNS(idAttr->getNamespaceURI(), idAttr->getLocalName());
    else 
        attr = getAttributeNode(idAttr->getName());
    
    if(!attr) 
        throw DOMException(DOMException::NOT_FOUND_ERR, 0);

    ((DOMAttrImpl *)attr)->addAttrToIDNodeMap();
}


DOMAttr * DOMElementImpl::setAttributeNode(DOMAttr *newAttr)
{
    if (fNode.isReadOnly())
        throw DOMException(
        DOMException::NO_MODIFICATION_ALLOWED_ERR, 0);

    if (newAttr->getNodeType() != DOMNode::ATTRIBUTE_NODE)
        throw DOMException(DOMException::WRONG_DOCUMENT_ERR, 0);
        // revisit.  Exception doesn't match test.

    // This will throw INUSE if necessary
    DOMAttr *oldAttr = (DOMAttr *) fAttributes->setNamedItem(newAttr);

    return oldAttr;
}


void DOMElementImpl::setNodeValue(const XMLCh *x)
{
    fNode.setNodeValue(x);
}



void DOMElementImpl::setReadOnly(bool readOnl, bool deep)
{
    fNode.setReadOnly(readOnl,deep);
    fAttributes->setReadOnly(readOnl,true);
}


//Introduced in DOM Level 2
const XMLCh * DOMElementImpl::getAttributeNS(const XMLCh *fNamespaceURI,
    const XMLCh *fLocalName) const
{
    DOMAttr * attr=
      (DOMAttr *)(fAttributes->getNamedItemNS(fNamespaceURI, fLocalName));
    return (attr==0) ? XMLUni::fgZeroLenString : attr->getValue();
}


void DOMElementImpl::setAttributeNS(const XMLCh *fNamespaceURI,
    const XMLCh *qualifiedName, const XMLCh *fValue)
{
    if (fNode.isReadOnly())
        throw DOMException(
        DOMException::NO_MODIFICATION_ALLOWED_ERR, 0);

    DOMAttr* newAttr = getAttributeNodeNS(fNamespaceURI, qualifiedName);

    if (!newAttr)
    {
        newAttr = this->fNode.getOwnerDocument()->createAttributeNS(fNamespaceURI, qualifiedName);
        fAttributes->setNamedItemNS(newAttr);
    }

    newAttr->setNodeValue(fValue);
}


void DOMElementImpl::removeAttributeNS(const XMLCh *fNamespaceURI,
    const XMLCh *fLocalName)
{
    if (fNode.isReadOnly())
        throw DOMException(
        DOMException::NO_MODIFICATION_ALLOWED_ERR, 0);

    XMLSSize_t i = fAttributes->findNamePoint(fNamespaceURI, fLocalName);
    if (i >= 0)
    {
        DOMNode *att = fAttributes->removeNamedItemAt(i);
        att->release();
    }
}


DOMAttr *DOMElementImpl::getAttributeNodeNS(const XMLCh *fNamespaceURI,
    const XMLCh *fLocalName) const
{
    return (DOMAttr *)fAttributes->getNamedItemNS(fNamespaceURI, fLocalName);
}


DOMAttr *DOMElementImpl::setAttributeNodeNS(DOMAttr *newAttr)
{
    if (fNode.isReadOnly())
        throw DOMException(
            DOMException::NO_MODIFICATION_ALLOWED_ERR, 0);

    if (newAttr -> getOwnerDocument() != this -> getOwnerDocument())
        throw DOMException(DOMException::WRONG_DOCUMENT_ERR, 0);

    // This will throw INUSE if necessary
    DOMAttr *oldAttr = (DOMAttr *) fAttributes->setNamedItemNS(newAttr);

    return oldAttr;
}


DOMNodeList *DOMElementImpl::getElementsByTagNameNS(const XMLCh *namespaceURI,
    const XMLCh *localName) const
{
    DOMDocumentImpl *docImpl = (DOMDocumentImpl *)getOwnerDocument();
    return docImpl->getDeepNodeList(this, namespaceURI, localName);
}

bool DOMElementImpl::hasAttributes() const
{
    return (fAttributes != 0 && fAttributes->getLength() != 0);
}


bool DOMElementImpl::hasAttribute(const XMLCh *name) const
{
    return (getAttributeNode(name) != 0);
}


bool DOMElementImpl::hasAttributeNS(const XMLCh *namespaceURI,
    const XMLCh *localName) const
{
    return (getAttributeNodeNS(namespaceURI, localName) != 0);
}


// util functions for default attributes
// returns the default attribute map for this node from the owner document
DOMAttrMapImpl *DOMElementImpl::getDefaultAttributes() const
{
    return fDefaultAttributes;
}

// initially set up the default attribute information based on doctype information
void DOMElementImpl::setupDefaultAttributes()
{
    DOMDocument *tmpdoc = getOwnerDocument();
    if ((fNode.fOwnerNode == 0) || (tmpdoc == 0) || (tmpdoc->getDoctype() == 0))
        return;

    DOMNode *eldef = ((DOMDocumentTypeImpl*)tmpdoc->getDoctype())->getElements()->getNamedItem(getNodeName());
    DOMAttrMapImpl* defAttrs = (eldef == 0) ? 0 : (DOMAttrMapImpl *)(eldef->getAttributes());

    if (defAttrs)
        fDefaultAttributes = new (getOwnerDocument()) DOMAttrMapImpl(this, defAttrs);
}

DOMAttr * DOMElementImpl::setDefaultAttributeNode(DOMAttr *newAttr)
{
    if (fNode.isReadOnly())
        throw DOMException(
        DOMException::NO_MODIFICATION_ALLOWED_ERR, 0);

    if (newAttr->getNodeType() != DOMNode::ATTRIBUTE_NODE)
        throw DOMException(DOMException::WRONG_DOCUMENT_ERR, 0);
        // revisit.  Exception doesn't match test.

    // This will throw INUSE if necessary
    DOMAttr *oldAttr = (DOMAttr *) fDefaultAttributes->setNamedItem(newAttr);
    fAttributes->hasDefaults(true);

    return oldAttr;
}


DOMAttr *DOMElementImpl::setDefaultAttributeNodeNS(DOMAttr *newAttr)
{
    if (fNode.isReadOnly())
        throw DOMException(
            DOMException::NO_MODIFICATION_ALLOWED_ERR, 0);

    if (newAttr -> getOwnerDocument() != this -> getOwnerDocument())
        throw DOMException(DOMException::WRONG_DOCUMENT_ERR, 0);

    // This will throw INUSE if necessary
    DOMAttr *oldAttr = (DOMAttr *) fDefaultAttributes->setNamedItemNS(newAttr);
    fAttributes->hasDefaults(true);

    return oldAttr;
}

void DOMElementImpl::release()
{
    if (fNode.isOwned() && !fNode.isToBeReleased())
        throw DOMException(DOMException::INVALID_ACCESS_ERR,0);

    DOMDocumentImpl* doc = (DOMDocumentImpl*) getOwnerDocument();
    if (doc) {
        fNode.callUserDataHandlers(DOMUserDataHandler::NODE_DELETED, 0, 0);
        fParent.release();
        doc->release(this, DOMDocumentImpl::ELEMENT_OBJECT);
    }
    else {
        // shouldn't reach here
        throw DOMException(DOMException::INVALID_ACCESS_ERR,0);
    }
}

const XMLCh* DOMElementImpl::getBaseURI() const
{
    const XMLCh* baseURI = fNode.fOwnerNode->getBaseURI();
    if (fAttributes) {
        const XMLCh xmlBaseString[] =
        {
            chLatin_x, chLatin_m, chLatin_l, chColon, chLatin_b, chLatin_a, chLatin_s, chLatin_e, chNull
        };
        DOMNode* attrNode = fAttributes->getNamedItem(xmlBaseString);
        if (attrNode) {
            const XMLCh* uri =  attrNode->getNodeValue();
            if (uri && *uri) {// attribute value is always empty string
                try {
                    XMLUri temp(baseURI, ((DOMDocumentImpl *)this->getOwnerDocument())->getMemoryManager());
                    XMLUri temp2(&temp, uri, ((DOMDocumentImpl *)this->getOwnerDocument())->getMemoryManager());
                    uri = ((DOMDocumentImpl *)this->getOwnerDocument())->cloneString(temp2.getUriText());
                }
                catch(const OutOfMemoryException&)
                {
                    throw;
                }
                catch (...){
                    // REVISIT: what should happen in this case?
                    return 0;
                }
                return uri;
            }
        }
    }
    return baseURI;
}



//
//   Functions inherited from Node
//
           DOMNode*         DOMElementImpl::appendChild(DOMNode *newChild)          {return fParent.appendChild (newChild); }
           DOMNodeList*     DOMElementImpl::getChildNodes() const                   {return fParent.getChildNodes (); }
           DOMNode*         DOMElementImpl::getFirstChild() const                   {return fParent.getFirstChild (); }
           DOMNode*         DOMElementImpl::getLastChild() const                    {return fParent.getLastChild (); }
     const XMLCh*           DOMElementImpl::getLocalName() const                    {return fNode.getLocalName (); }
     const XMLCh*           DOMElementImpl::getNamespaceURI() const                 {return fNode.getNamespaceURI (); }
           DOMNode*         DOMElementImpl::getNextSibling() const                  {return fChild.getNextSibling (); }
     const XMLCh*           DOMElementImpl::getNodeValue() const                    {return fNode.getNodeValue (); }
           DOMDocument*     DOMElementImpl::getOwnerDocument() const                {return fParent.fOwnerDocument; }
     const XMLCh*           DOMElementImpl::getPrefix() const                       {return fNode.getPrefix (); }
           DOMNode*         DOMElementImpl::getParentNode() const                   {return fChild.getParentNode (this); }
           DOMNode*         DOMElementImpl::getPreviousSibling() const              {return fChild.getPreviousSibling (this); }
           bool             DOMElementImpl::hasChildNodes() const                   {return fParent.hasChildNodes (); }
           DOMNode*         DOMElementImpl::insertBefore(DOMNode *newChild, DOMNode *refChild)
                                                                                    {return fParent.insertBefore (newChild, refChild); }
           void             DOMElementImpl::normalize()                             {fParent.normalize (); }
           DOMNode*         DOMElementImpl::removeChild(DOMNode *oldChild)          {return fParent.removeChild (oldChild); }
           DOMNode*         DOMElementImpl::replaceChild(DOMNode *newChild, DOMNode *oldChild)
                                                                                    {return fParent.replaceChild (newChild, oldChild); }
           bool             DOMElementImpl::isSupported(const XMLCh *feature, const XMLCh *version) const
                                                                                    {return fNode.isSupported (feature, version); }
           void             DOMElementImpl::setPrefix(const XMLCh  *prefix)         {fNode.setPrefix(prefix); }
           bool             DOMElementImpl::isSameNode(const DOMNode* other) const  {return fNode.isSameNode(other); }
           void*            DOMElementImpl::setUserData(const XMLCh* key, void* data, DOMUserDataHandler* handler)
                                                                                    {return fNode.setUserData(key, data, handler); }
           void*            DOMElementImpl::getUserData(const XMLCh* key) const     {return fNode.getUserData(key); }
           short            DOMElementImpl::compareTreePosition(const DOMNode* other) const {return fNode.compareTreePosition(other); }
           const XMLCh*     DOMElementImpl::getTextContent() const                  {return fNode.getTextContent(); }
           void             DOMElementImpl::setTextContent(const XMLCh* textContent){fNode.setTextContent(textContent); }
           const XMLCh*     DOMElementImpl::lookupNamespacePrefix(const XMLCh* namespaceURI, bool useDefault) const  {return fNode.lookupNamespacePrefix(namespaceURI, useDefault); }
           bool             DOMElementImpl::isDefaultNamespace(const XMLCh* namespaceURI) const {return fNode.isDefaultNamespace(namespaceURI); }
           const XMLCh*     DOMElementImpl::lookupNamespaceURI(const XMLCh* prefix) const  {return fNode.lookupNamespaceURI(prefix); }
           DOMNode*         DOMElementImpl::getInterface(const XMLCh* feature)      {return fNode.getInterface(feature); }



bool DOMElementImpl::isEqualNode(const DOMNode* arg) const
{
    if (isSameNode(arg)) {
        return true;
    }

    if (!fNode.isEqualNode(arg)) {
        return false;
    }

    bool hasAttrs = hasAttributes();

    if (hasAttrs != arg->hasAttributes()) {
        return false;
    }

    if (hasAttrs) {
        DOMNamedNodeMap* map1 = getAttributes();
        DOMNamedNodeMap* map2 = arg->getAttributes();

        XMLSize_t len = map1->getLength();
        if (len != map2->getLength()) {
            return false;
        }
        for (XMLSize_t i = 0; i < len; i++) {
            DOMNode* n1 = map1->item(i);
            if (!n1->getLocalName()) { // DOM Level 1 Node
                DOMNode* n2 = map2->getNamedItem(n1->getNodeName());
                if (!n2 || !n1->isEqualNode(n2)) {
                    return false;
                }
            }
            else {
                DOMNode* n2 = map2->getNamedItemNS(n1->getNamespaceURI(),
                                              n1->getLocalName());
                if (!n2 || !n1->isEqualNode(n2)) {
                    return false;
                }
            }
        }
    }

    return fParent.isEqualNode(arg);
}

DOMNode* DOMElementImpl::rename(const XMLCh* namespaceURI, const XMLCh* name)
{
    DOMDocumentImpl* doc = (DOMDocumentImpl*) getOwnerDocument();

    if (!namespaceURI || !*namespaceURI) {
        fName = doc->getPooledString(name);
        fAttributes->reconcileDefaultAttributes(getDefaultAttributes());

        return this;
    }
    else {

        // create a new ElementNS
        DOMElementNSImpl* newElem = (DOMElementNSImpl*)doc->createElementNS(namespaceURI, name);

        // transfer the userData
        doc->transferUserData(castToNodeImpl(this), castToNodeImpl(newElem));

        // remove old node from parent if any
        DOMNode* parent = getParentNode();
        DOMNode* nextSib = getNextSibling();
        if (parent) {
            parent->removeChild(this);
        }

        // move children to new node
        DOMNode* child = getFirstChild();
        while (child) {
            removeChild(child);
            newElem->appendChild(child);
            child = getFirstChild();
        }

        // insert new node where old one was
        if (parent) {
            parent->insertBefore(newElem, nextSib);
        }

        // move specified attributes to new node
        newElem->fAttributes->moveSpecifiedAttributes(fAttributes);

        // and fire user data NODE_RENAMED event
        castToNodeImpl(newElem)->callUserDataHandlers(DOMUserDataHandler::NODE_RENAMED, this, newElem);

        return newElem;
    }
}

const DOMTypeInfo *DOMElementImpl::getTypeInfo() const
{
    if(!fSchemaType) 
        ((DOMElementImpl *)(this))->fSchemaType = new (getOwnerDocument()) DOMTypeInfoImpl(0, 0, (DOMDocumentImpl *)getOwnerDocument());
    return fSchemaType;
}


void DOMElementImpl::setTypeInfo(const XMLCh* typeName, const XMLCh* typeURI) 
{
    fSchemaType = new (getOwnerDocument()) DOMTypeInfoImpl(typeName, typeURI, (DOMDocumentImpl *)getOwnerDocument());
}

XERCES_CPP_NAMESPACE_END

