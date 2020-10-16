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
 * $Id: DocumentImpl.cpp,v 1.8 2003/05/22 02:26:50 knoaman Exp $
 */

//
// file DocumentImpl.cpp
//

#include "DocumentImpl.hpp"
#include "DOM_DOMException.hpp"
#include "DOM_Node.hpp"

#include "DocumentTypeImpl.hpp"
#include "ElementImpl.hpp"
#include "ElementNSImpl.hpp"
#include "AttrImpl.hpp"
#include "AttrNSImpl.hpp"
#include "CDATASectionImpl.hpp"
#include "CommentImpl.hpp"
#include "DocumentFragmentImpl.hpp"
#include "EntityImpl.hpp"
#include "EntityReferenceImpl.hpp"
#include "NotationImpl.hpp"
#include "ProcessingInstructionImpl.hpp"
#include "TextImpl.hpp"
#include "DOM_DOMImplementation.hpp"
#include "DeepNodeListImpl.hpp"
#include "NamedNodeMapImpl.hpp"
#include "DStringPool.hpp"
#include <xercesc/internal/XMLReader.hpp>
#include "TreeWalkerImpl.hpp"
#include "NodeIteratorImpl.hpp"
#include "NodeIDMap.hpp"
#include "DOM_Document.hpp"
#include <xercesc/util/HashPtr.hpp>
#include "RangeImpl.hpp"
#include "DOM_Document.hpp"

XERCES_CPP_NAMESPACE_BEGIN


static DOMString *nam = 0;  // will be lazily initialized to "#document"
static XMLRegisterCleanup namCleanup;

DocumentImpl::DocumentImpl(MemoryManager* const manager)
    : ParentNode(this)
    , docType(0)
    , docElement(0)
    , namePool(0)
    , fNodeIDMap(0)
    , iterators(0)
    , treeWalkers(0)
    , userData(0)
    , ranges(0)
    , fChanges(0)
    , errorChecking(true)
    , fMemoryManager(manager)
{
    namePool    = new (fMemoryManager) DStringPool(257, fMemoryManager);
}   /* SPEC_CPU: removed extra ';' for C++98 standards compliance -- yag */


//DOM Level 2
DocumentImpl::DocumentImpl(const DOMString &fNamespaceURI,
                           const DOMString &qualifiedName,
                           DocumentTypeImpl *doctype,
                           MemoryManager* const manager)
    : ParentNode(this)
    , docType(0)
    , docElement(0)
    , namePool(0)
    , fNodeIDMap(0)
    , iterators(0)
    , treeWalkers(0)
    , userData(0)
    , ranges(0)
    , fChanges(0)
    , errorChecking(true)
    , fMemoryManager(manager)
{
	setDocumentType(doctype);
    namePool    = new (fMemoryManager) DStringPool(257, fMemoryManager);
    appendChild(createElementNS(fNamespaceURI, qualifiedName));  //root element
}

void DocumentImpl::setDocumentType(DocumentTypeImpl *doctype)
{
	if (!doctype)
		return;

	if (doctype->getOwnerDocument() != null)
        throw DOM_DOMException(	//one doctype can belong to only one DocumentImpl
        DOM_DOMException::WRONG_DOCUMENT_ERR, null);

    doctype->setOwnerDocument(this);
    doctype->getEntities()->ownerNode->setOwnerDocument(this);
    doctype->getNotations()->ownerNode->setOwnerDocument(this);
    doctype -> referenced();         // Warning, tricky!  An external (DOM_Node) reference
                                     //  to a node normally bumps the reference count to its
                                     //  document also.  But this could not happen when the
                                     //  user created the DOM_DocumentType because there was
                                     //  no document yet.  Now we have the document, and
                                     //  the docs ref count must be got back in sync.

				
	appendChild(doctype);

}

DocumentImpl::~DocumentImpl()
{
    if (iterators != 0L) {
        // The data in the vector is pointers owned by smart pointers, and will be cleaned up when they go away.
        delete iterators;
    }

    if (treeWalkers != 0L) {
        // The data in the vector is pointers owned by smart pointers, and will be cleaned up when they go away.
        delete treeWalkers;
    }

    if (ranges != 0L) {
        delete ranges;
        ranges = 0;
    }

    if (userData) {
        // make sure we won't access userData any further
        hasUserData(false);
        delete userData;
    }

    delete namePool;
    // Do not delete docType and docElement pointers here.
    //  These are also normal child nodes of the document,
    //  and refcounting will take them out in the usual way.

    delete fNodeIDMap;
}   /* SPEC_CPU: removed extra ';' for C++98 standards compliance -- yag */


NodeImpl *DocumentImpl::cloneNode(bool deep) {

    // clone the node itself
    DocumentImpl *newdoc = new (fMemoryManager) DocumentImpl(fMemoryManager);

    // then the children by _importing_ them
    if (deep) {
        for (ChildNode *n = firstChild; n != null; n = n->nextSibling) {
            newdoc->appendChild(newdoc->importNode(n, true));
	}
    }
    newdoc->setErrorChecking(errorChecking);
    return newdoc;
}   /* SPEC_CPU: removed extra ';' for C++98 standards compliance -- yag */


DOMString DocumentImpl::getNodeName() {
    return DStringPool::getStaticString("#document"
                                       , &nam
                                       , reinitDocumentImpl
                                       , namCleanup);
}


short DocumentImpl::getNodeType() {
    return DOM_Node::DOCUMENT_NODE;
}   /* SPEC_CPU: removed extra ';' for C++98 standards compliance -- yag */


// even though ownerDocument refers to this in this implementation
// the DOM Level 2 spec says it must be null, so make it appear so
DocumentImpl * DocumentImpl::getOwnerDocument() {
    return null;
}


bool DocumentImpl::isDocumentImpl() {
    return true;
}   /* SPEC_CPU: removed extra ';' for C++98 standards compliance -- yag */


AttrImpl *DocumentImpl::createAttribute(const DOMString &nam)
{
    if (errorChecking && !isXMLName(nam)) {
        throw DOM_DOMException(DOM_DOMException::INVALID_CHARACTER_ERR,null);
    }
    return new (fMemoryManager) AttrImpl(this,nam);
}   /* SPEC_CPU: removed extra ';' for C++98 standards compliance -- yag */



CDATASectionImpl *DocumentImpl::createCDATASection(const DOMString &data) {
    return new (fMemoryManager) CDATASectionImpl(this,data);
}   /* SPEC_CPU: removed extra ';' for C++98 standards compliance -- yag */



CommentImpl *DocumentImpl::createComment(const DOMString &data)
{
    return new (fMemoryManager) CommentImpl(this,data);
}   /* SPEC_CPU: removed extra ';' for C++98 standards compliance -- yag */



DocumentFragmentImpl *DocumentImpl::createDocumentFragment()
{
    return new (fMemoryManager) DocumentFragmentImpl(this);
}   /* SPEC_CPU: removed extra ';' for C++98 standards compliance -- yag */



DocumentTypeImpl *DocumentImpl::createDocumentType(const DOMString &nam)
{
    if (errorChecking && !isXMLName(nam)) {
        throw DOM_DOMException(DOM_DOMException::INVALID_CHARACTER_ERR, null);
    }
    return new (fMemoryManager) DocumentTypeImpl(this, nam);
}   /* SPEC_CPU: removed extra ';' for C++98 standards compliance -- yag */



DocumentTypeImpl *
    DocumentImpl::createDocumentType(const DOMString &qualifiedName,
                                     const DOMString &publicId,
                                     const DOMString &systemId)
{
    if (errorChecking && !isXMLName(qualifiedName)) {
        throw DOM_DOMException(DOM_DOMException::INVALID_CHARACTER_ERR, null);
    }
    return new (fMemoryManager) DocumentTypeImpl(this, qualifiedName, publicId, systemId);
}   /* SPEC_CPU: removed extra ';' for C++98 standards compliance -- yag */



ElementImpl *DocumentImpl::createElement(const DOMString &tagName)
{
    if (errorChecking && !isXMLName(tagName)) {
        throw DOM_DOMException(DOM_DOMException::INVALID_CHARACTER_ERR,null);
    }
    DOMString pooledTagName = this->namePool->getPooledString(tagName);
    return new (fMemoryManager) ElementImpl(this,pooledTagName);
}   /* SPEC_CPU: removed extra ';' for C++98 standards compliance -- yag */


ElementImpl *DocumentImpl::createElement(const XMLCh *tagName)
{
    DOMString pooledTagName = this->namePool->getPooledString(tagName);
    return new (fMemoryManager) ElementImpl(this,pooledTagName);
}   /* SPEC_CPU: removed extra ';' for C++98 standards compliance -- yag */




EntityImpl *DocumentImpl::createEntity(const DOMString &nam)
{
    if (errorChecking && !isXMLName(nam)) {
        throw DOM_DOMException(DOM_DOMException::INVALID_CHARACTER_ERR, null);
    }
    return new (fMemoryManager) EntityImpl(this, nam);
}   /* SPEC_CPU: removed extra ';' for C++98 standards compliance -- yag */



EntityReferenceImpl *DocumentImpl::createEntityReference(const DOMString &nam)
{
    if (errorChecking && !isXMLName(nam)) {
        throw DOM_DOMException(DOM_DOMException::INVALID_CHARACTER_ERR, null);
    }
    return new (fMemoryManager) EntityReferenceImpl(this, nam);
}   /* SPEC_CPU: removed extra ';' for C++98 standards compliance -- yag */



NotationImpl *DocumentImpl::createNotation(const DOMString &nam)
{
    if (errorChecking && !isXMLName(nam)) {
        throw DOM_DOMException(DOM_DOMException::INVALID_CHARACTER_ERR, null);
    }
    return new (fMemoryManager) NotationImpl(this, nam);
}   /* SPEC_CPU: removed extra ';' for C++98 standards compliance -- yag */



ProcessingInstructionImpl *DocumentImpl::createProcessingInstruction(
                                          const DOMString &target, const DOMString &data)
{
    if (errorChecking && !isXMLName(target)) {
        throw DOM_DOMException(DOM_DOMException::INVALID_CHARACTER_ERR,null);
    }
    return new (fMemoryManager) ProcessingInstructionImpl(this,target,data);
}   /* SPEC_CPU: removed extra ';' for C++98 standards compliance -- yag */




TextImpl *DocumentImpl::createTextNode(const DOMString &data)
{
    return new (fMemoryManager) TextImpl(this,data);
}   /* SPEC_CPU: removed extra ';' for C++98 standards compliance -- yag */


NodeIteratorImpl* DocumentImpl::createNodeIterator (DOM_Node root,
                                                    unsigned long whatToShow,
                                                    DOM_NodeFilter* filter,
                                                    bool entityReferenceExpansion,
                                                    MemoryManager* const manager)
{
		// Create the node iterator implementation object.
		//	Add it to the vector of iterators that must be synchronized when a node is deleted.
		//	The vector of iterators is kept in the "owner document" if there is one. If there isn't one, I assume that root is the
		//	owner document.

    NodeIteratorImpl* iter = new (manager) NodeIteratorImpl(root, whatToShow, filter, entityReferenceExpansion);
    DOM_Document doc = root.getOwnerDocument();
    DocumentImpl* impl;

    if (! doc.isNull()) {
        impl = (DocumentImpl *) doc.fImpl;
    }
    else
        impl = (DocumentImpl *) root.fImpl;

    if (impl->iterators == 0L) {
        impl->iterators = new (manager) NodeIterators(1, false, manager);
        impl->iterators->addElement(iter);
    }

    return iter;
}


TreeWalkerImpl* DocumentImpl::createTreeWalker (DOM_Node root, unsigned long whatToShow,
                                                DOM_NodeFilter* filter,
                                                bool entityReferenceExpansion,
                                                MemoryManager* const manager)
{
		// See notes for createNodeIterator...

    TreeWalkerImpl* twi = new (manager) TreeWalkerImpl(root, whatToShow, filter, entityReferenceExpansion);
    DOM_Document doc = root.getOwnerDocument();
    DocumentImpl* impl;

    if (! doc.isNull()) {
        impl = (DocumentImpl *) doc.fImpl;
    }
    else
        impl = (DocumentImpl *) root.fImpl;

    if (impl->treeWalkers == 0L) {
        impl->treeWalkers = new (manager) TreeWalkers(1, false, manager);
        impl->treeWalkers->addElement(twi);
    }

    return twi;
}




DocumentTypeImpl *DocumentImpl::getDoctype()
{
    return docType;
}   /* SPEC_CPU: removed extra ';' for C++98 standards compliance -- yag */



ElementImpl *DocumentImpl::getDocumentElement()
{
    return docElement;
}   /* SPEC_CPU: removed extra ';' for C++98 standards compliance -- yag */



DeepNodeListImpl *DocumentImpl::getElementsByTagName(const DOMString &tagname)
{
    return new (fMemoryManager) DeepNodeListImpl(this,tagname);
}   /* SPEC_CPU: removed extra ';' for C++98 standards compliance -- yag */



NodeImpl *DocumentImpl::insertBefore(NodeImpl *newChild, NodeImpl *refChild)
{
    // Only one such child permitted
    if (errorChecking &&
        ((newChild->isElementImpl() && docElement!=null) ||
         (newChild->isDocumentTypeImpl() && docType!=null))) {
        throw DOM_DOMException(DOM_DOMException::HIERARCHY_REQUEST_ERR,null);
    }
    ParentNode::insertBefore(newChild,refChild);

    // If insert succeeded, cache the kid appropriately
    if(newChild->isElementImpl())
        docElement=(ElementImpl *)newChild;
    else if(newChild->isDocumentTypeImpl())
        docType=(DocumentTypeImpl *)newChild;

    return newChild;
}   /* SPEC_CPU: removed extra ';' for C++98 standards compliance -- yag */



bool DocumentImpl::isXMLName(const DOMString &s)
{
    return XMLChar1_0::isValidName(s.rawBuffer(),s.length());
}   /* SPEC_CPU: removed extra ';' for C++98 standards compliance -- yag */


//  referenced().   Override this function here in class DocumentImpl because
//                  we don't want the action taken in NodeImpl, which is
//                  to add a reference to the node's owning document.
//
void DocumentImpl::referenced()
{
    // Intentionally empty.
}   /* SPEC_CPU: removed extra ';' for C++98 standards compliance -- yag */



NodeImpl *DocumentImpl::removeChild(NodeImpl *oldChild)
{
    ParentNode::removeChild(oldChild);

    // If remove succeeded, un-cache the kid appropriately
    if(oldChild->isElementImpl())
        docElement=null;
    else if(oldChild->isDocumentTypeImpl())
        docType=null;

    return oldChild;
}   /* SPEC_CPU: removed extra ';' for C++98 standards compliance -- yag */


//
//    unreferenced()  will be called whenever the refernce count on
//          this document goes from 1 to 0.  In all cases, when this
//          happens to a document node (which is the case here), it
//          is time to actually delete the document.
//
//          See also NodeImpl::referenced() and unreferenced(), which
//          update document node ref counts based on references coming
//          or going to nodes owned by the document.
//
void DocumentImpl::unreferenced()
{
    deleteIf(this);
}   /* SPEC_CPU: removed extra ';' for C++98 standards compliance -- yag */


//Introduced in DOM Level 2

NodeImpl *DocumentImpl::importNode(NodeImpl *source, bool deep)
{
    NodeImpl *newnode=null;
    bool oldErrorCheckingFlag = errorChecking;

    switch (source->getNodeType())
    {
        case DOM_Node::ELEMENT_NODE :
        {
            ElementImpl *newelement;
            if (source->getLocalName() == null)
                newelement = createElement(source->getNodeName());
            else
                newelement = createElementNS(source->getNamespaceURI(),
                                             source->getNodeName());
            NamedNodeMapImpl *srcattr=source->getAttributes();
            if (srcattr!=null)
                for(unsigned int i=0;i<srcattr->getLength();++i)
                {
                    AttrImpl *attr = (AttrImpl *) srcattr->item(i);
                    AttrImpl * pOldAttr = null;
                    if (attr -> getSpecified())	
                    { // not a default attribute
                        AttrImpl *nattr = (AttrImpl *) importNode(attr, true);
                        if (attr -> getLocalName() == null)
                            pOldAttr = newelement->setAttributeNode(nattr);
                        else
                            pOldAttr = newelement->setAttributeNodeNS(nattr);

                        if (pOldAttr)
                        {
                            if (pOldAttr->nodeRefCount == 0)
                                NodeImpl::deleteIf(pOldAttr);
                        }
                    }
                }

            newnode=newelement;
        }
        break;
    case DOM_Node::ATTRIBUTE_NODE :
        {
            if (source->getLocalName() == null)
                newnode = createAttribute(source->getNodeName());
            else
                newnode = createAttributeNS(source->getNamespaceURI(),
                                        source->getNodeName());
        // if source is an AttrImpl from this very same implementation
        // avoid creating the child nodes if possible
//        if (source instanceof AttrImpl) {
            AttrImpl *attr = (AttrImpl *) source;
            if (attr->hasStringValue()) {
                AttrImpl *newattr = (AttrImpl *) newnode;
                newattr->setValue(attr->getValue());
                deep = false;
            }
            else {
                deep = true;
            }
//        }
//        else {
//            // Kids carry value
//            deep = true;
//        }
        }
        break;
    case DOM_Node::TEXT_NODE :
        newnode = createTextNode(source->getNodeValue());
        break;
    case DOM_Node::CDATA_SECTION_NODE :
        newnode = createCDATASection(source->getNodeValue());
        break;
    case DOM_Node::ENTITY_REFERENCE_NODE :
        {
            EntityReferenceImpl* newentityRef = createEntityReference(source->getNodeName());
            newnode=newentityRef;
            errorChecking = false;
            newentityRef->setReadOnly(false, true); //allow deep import temporarily
        }
        break;
    case DOM_Node::ENTITY_NODE :
        {
            EntityImpl *srcentity=(EntityImpl *)source;
            EntityImpl *newentity = createEntity(source->getNodeName());
            newentity->setPublicId(srcentity->getPublicId());
            newentity->setSystemId(srcentity->getSystemId());
            newentity->setNotationName(srcentity->getNotationName());
            // Kids carry additional value
            newnode=newentity;
            newentity->setReadOnly(false, true);// allow deep import temporarily
        }
        break;
    case DOM_Node::PROCESSING_INSTRUCTION_NODE :
        newnode = createProcessingInstruction(source->getNodeName(),
                                              source->getNodeValue());
        break;
    case DOM_Node::COMMENT_NODE :
        newnode = createComment(source->getNodeValue());
        break;
    case DOM_Node::DOCUMENT_TYPE_NODE :
        {
            DocumentTypeImpl *srcdoctype = (DocumentTypeImpl *)source;
            DocumentTypeImpl *newdoctype = (DocumentTypeImpl *)
                createDocumentType(srcdoctype->getNodeName(),
                                   srcdoctype->getPublicId(),
                                   srcdoctype->getSystemId());
            // Values are on NamedNodeMaps
            NamedNodeMapImpl *smap = srcdoctype->getEntities();
            NamedNodeMapImpl *tmap = newdoctype->getEntities();
            if(smap != null) {
                for(unsigned int i = 0; i < smap->getLength(); i++) {
                    tmap->setNamedItem(importNode(smap->item(i), true));
                }
            }
            smap = srcdoctype->getNotations();
            tmap = newdoctype->getNotations();
            if (smap != null) {
                for(unsigned int i = 0; i < smap->getLength(); i++) {
                    tmap->setNamedItem(importNode(smap->item(i), true));
                }
            }
            // NOTE: At this time, the DOM definition of DocumentType
            // doesn't cover Elements and their Attributes. domimpl's
            // extentions in that area will not be preserved, even if
            // copying from domimpl to domimpl. We could special-case
            // that here. Arguably we should. Consider. ?????
            newnode = newdoctype;
        }
        break;
    case DOM_Node::DOCUMENT_FRAGMENT_NODE :
        newnode = createDocumentFragment();
        // No name, kids carry value
        break;
    case DOM_Node::NOTATION_NODE :
        {
            NotationImpl *srcnotation=(NotationImpl *)source;
            NotationImpl *newnotation = createNotation(source->getNodeName());
            newnotation->setPublicId(srcnotation->getPublicId());
            newnotation->setSystemId(srcnotation->getSystemId());
            // Kids carry additional value
            newnode=newnotation;
            // No name, no value
            break;
        }

    case DOM_Node::DOCUMENT_NODE : // Document can't be child of Document
    default:                       // Unknown node type
        throw DOM_DOMException(DOM_DOMException::NOT_SUPPORTED_ERR, null);
    }

    // If deep, replicate and attach the kids.
    if (deep)
        for (NodeImpl *srckid = source->getFirstChild();
             srckid != null;
             srckid = srckid->getNextSibling()) {
            newnode->appendChild(importNode(srckid, true));
        }
    if (newnode->getNodeType() == DOM_Node::ENTITY_REFERENCE_NODE
        || newnode->getNodeType() == DOM_Node::ENTITY_NODE) {
         ((EntityReferenceImpl*)newnode)->setReadOnly(true, true);
         errorChecking = oldErrorCheckingFlag;
    }

    return newnode;
}   /* SPEC_CPU: removed extra ';' for C++98 standards compliance -- yag */


ElementImpl *DocumentImpl::createElementNS(const DOMString &fNamespaceURI,
	const DOMString &qualifiedName)
{
    if (errorChecking && !isXMLName(qualifiedName)) {
        throw DOM_DOMException(DOM_DOMException::INVALID_CHARACTER_ERR,null);
    }
    //DOMString pooledTagName = this->namePool->getPooledString(qualifiedName);
    return new (fMemoryManager) ElementNSImpl(this, fNamespaceURI, qualifiedName);
}


AttrImpl *DocumentImpl::createAttributeNS(const DOMString &fNamespaceURI,
	const DOMString &qualifiedName)
{
    if (!isXMLName(qualifiedName)) {
        throw DOM_DOMException(DOM_DOMException::INVALID_CHARACTER_ERR,null);
    }
    return new (fMemoryManager) AttrNSImpl(this, fNamespaceURI, qualifiedName);
}


DeepNodeListImpl *DocumentImpl::getElementsByTagNameNS(const DOMString &fNamespaceURI,
	const DOMString &fLocalName)
{
    return new (fMemoryManager) DeepNodeListImpl(this, fNamespaceURI, fLocalName);
}


ElementImpl *DocumentImpl::getElementById(const DOMString &elementId)
{
    if (fNodeIDMap == 0)
        return null;

    AttrImpl *theAttr = fNodeIDMap->find(elementId);
    if (theAttr == null)
	    return null;

    return theAttr->getOwnerElement();
}


//Return the index > 0 of ':' in the given qualified name qName="prefix:localName".
//Return 0 if there is no ':', or -1 if qName is malformed such as ":abcd".
int DocumentImpl::indexofQualifiedName(const DOMString & qName)
{
    //Check if s = prefix:localName, name or malformed
    const XMLCh *qNameP = qName.rawBuffer();
    int qNameLen = qName.length();	//note: qName[qNameLen] may not be 0
    int index = -1, count = 0;
    for (int i = 0; i < qNameLen; ++i)
	if (*qNameP++ == chColon) {
	    index = i;
	    ++count;	//number of ':' found
	}
    if (qNameLen == 0 || count > 1 || index == 0 || index == qNameLen-1)
        return -1;
    return count == 0 ? 0 : index;
}


XMLDeclImpl* DocumentImpl::createXMLDecl(const DOMString& version, const DOMString& encoding, const DOMString& standalone)
{
    return new (fMemoryManager) XMLDeclImpl(this, version, encoding, standalone);
}

RangeImpl* DocumentImpl::createRange()
{

    RangeImpl* range = new (fMemoryManager) RangeImpl(DOM_Document(this));

    if (ranges == 0L) {
        ranges = new (fMemoryManager) RangeImpls(1, false, fMemoryManager);
    }
    ranges->addElement(range);
    return range;
}

RangeImpls* DocumentImpl::getRanges()
{
    return ranges;
}

void DocumentImpl::removeRange(RangeImpl* range)
{
    if (ranges != null) {
        unsigned int sz = ranges->size();
        if (sz !=0) {
            for (unsigned int i =0; i<sz; i++) {
                if (ranges->elementAt(i) == range) {
                    ranges->removeElementAt(i);
                    delete range;
                    break;
                }
            }
        }
    }
}

/** Uses the kidOK lookup table to check whether the proposed
    tree structure is legal.

    ????? It feels like there must be a more efficient solution,
    but for the life of me I can't think what it would be.
*/
bool DocumentImpl::isKidOK(NodeImpl *parent, NodeImpl *child)
{
      static int kidOK[14];

      if (kidOK[DOM_Node::DOCUMENT_NODE] == 0)
      {
          kidOK[DOM_Node::DOCUMENT_NODE] =
              1 << DOM_Node::ELEMENT_NODE |
              1 << DOM_Node::PROCESSING_INSTRUCTION_NODE |
              1 << DOM_Node::COMMENT_NODE |
              1 << DOM_Node::DOCUMENT_TYPE_NODE |
              1 << DOM_Node::XML_DECL_NODE;

          kidOK[DOM_Node::DOCUMENT_FRAGMENT_NODE] =
              kidOK[DOM_Node::ENTITY_NODE] =
              kidOK[DOM_Node::ENTITY_REFERENCE_NODE] =
              kidOK[DOM_Node::ELEMENT_NODE] =
              1 << DOM_Node::ELEMENT_NODE |
              1 << DOM_Node::PROCESSING_INSTRUCTION_NODE |
              1 << DOM_Node::COMMENT_NODE |
              1 << DOM_Node::TEXT_NODE |
              1 << DOM_Node::CDATA_SECTION_NODE |
              1 << DOM_Node::ENTITY_REFERENCE_NODE |
              1 << DOM_Node::XML_DECL_NODE;

          kidOK[DOM_Node::ATTRIBUTE_NODE] =
              1 << DOM_Node::TEXT_NODE |
              1 << DOM_Node::ENTITY_REFERENCE_NODE;

          kidOK[DOM_Node::PROCESSING_INSTRUCTION_NODE] =
              kidOK[DOM_Node::COMMENT_NODE] =
              kidOK[DOM_Node::TEXT_NODE] =
              kidOK[DOM_Node::CDATA_SECTION_NODE] =
              kidOK[DOM_Node::NOTATION_NODE] =
              0;
      };
      int p=parent->getNodeType();
      int ch = child->getNodeType();
      return (kidOK[p] & 1<<ch) != 0;
}

void DocumentImpl::setUserData(NodeImpl* n, void* data)
{
	if (!userData && data)
		userData = new (fMemoryManager) RefHashTableOf<void>
        (
            29
            , false
            , new (fMemoryManager) HashPtr()
            , fMemoryManager
        );
	if (!data && userData)
		userData->removeKey((void*)n);
	else
		userData->put((void*)n,data);
}

void* DocumentImpl::getUserData(NodeImpl* n)
{
	if (userData)
		return userData->get((void*)n);
	else
		return null;
}

void* DocumentImpl::getUserData()
{
	return (hasUserData()) ? getUserData(this) : null;
}

void DocumentImpl::setUserData(void* val)
{
	setUserData(this, val);
	if (val)
		hasUserData(true);
	else
		hasUserData(false);
}   /* SPEC_CPU: removed extra ';' for C++98 standards compliance -- yag */

/**
 * Denotes that this node has changed.
 */
void DocumentImpl::changed() {
    fChanges++;
}

/**
 * Returns the number of changes to this node.
 */
int DocumentImpl::changes() {
    return fChanges;
}

// -----------------------------------------------------------------------
//  Notification that lazy data has been deleted
// -----------------------------------------------------------------------
void DocumentImpl::reinitDocumentImpl() {
	delete nam;
	nam = 0;
}

XERCES_CPP_NAMESPACE_END

