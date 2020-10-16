#ifndef DOMDocumentImpl_HEADER_GUARD_
#define DOMDocumentImpl_HEADER_GUARD_

/*
 * The Apache Software License, Version 1.1
 *
 * Copyright (c) 2001-2003 The Apache Software Foundation.  All rights
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
 * $Id: DOMDocumentImpl.hpp,v 1.23 2004/01/29 11:44:26 cargilld Exp $
 */

//
//  This file is part of the internal implementation of the C++ XML DOM.
//  It should NOT be included or used directly by application programs.
//
//  Applications should include the file <xercesc/dom/DOM.hpp> for the entire
//  DOM API, or xercesc/dom/DOM*.hpp for individual DOM classes, where the class
//  name is substituded for the *.
//

#include <xercesc/util/RefArrayOf.hpp>
#include <xercesc/util/RefStackOf.hpp>
#include <xercesc/util/RefHashTableOf.hpp>
#include <xercesc/util/KeyRefPair.hpp>
#include <xercesc/dom/DOMDocument.hpp>
#include <xercesc/dom/DOMUserDataHandler.hpp>
#include "DOMNodeImpl.hpp"
#include "DOMParentNode.hpp"
#include "DOMDeepNodeListPool.hpp"

XERCES_CPP_NAMESPACE_BEGIN


class DOMAttrImpl;
class DOMCDATASectionImpl;
class DOMCommentImpl;
class DOMConfiguration;
class DOMDeepNodeListImpl;
class DOMDocumentFragmentImpl;
class DOMDocumentTypeImpl;
class DOMElementImpl;
class DOMEntityImpl;
class DOMEntityReferenceImpl;
class DOMNotationImpl;
class DOMProcessingInstructionImpl;
class DOMTextImpl;
class DOMNodeIteratorImpl;
class DOMNormalizer;
class DOMTreeWalkerImpl;
class DOMNodeFilter;
class DOMNodeFilterImpl;
class DOMImplementation;
class DOMNodeIDMap;
class DOMRangeImpl;
class DOMStringPool;
class DOMBuffer;
class MemoryManager;
class XPathNSResolver;
class XPathExpression;

typedef RefVectorOf<DOMRangeImpl>        Ranges;
typedef RefVectorOf<DOMNodeIteratorImpl>     NodeIterators;
typedef KeyRefPair<void, DOMUserDataHandler> DOMUserDataRecord;
typedef RefHashTableOf<DOMUserDataRecord> DOMNodeUserDataTable;
typedef RefStackOf<DOMNode>               DOMNodePtr;

class CDOM_EXPORT DOMDocumentImpl: public XMemory, public DOMDocument {
public:
    // -----------------------------------------------------------------------
    //  data types
    // -----------------------------------------------------------------------
    enum NodeObjectType {
        ATTR_OBJECT                   = 0,
        ATTR_NS_OBJECT                = 1,
        CDATA_SECTION_OBJECT          = 2,
        COMMENT_OBJECT                = 3,
        DOCUMENT_FRAGMENT_OBJECT      = 4,
        DOCUMENT_TYPE_OBJECT          = 5,
        ELEMENT_OBJECT                = 6,
        ELEMENT_NS_OBJECT             = 7,
        ENTITY_OBJECT                 = 8,
        ENTITY_REFERENCE_OBJECT       = 9,
        NOTATION_OBJECT               = 10,
        PROCESSING_INSTRUCTION_OBJECT = 11,
        TEXT_OBJECT                   = 12
    };


    // -----------------------------------------------------------------------
    //  data
    // -----------------------------------------------------------------------
    DOMNodeImpl           fNode;           // Implements common node functionality.
    DOMParentNode         fParent;         // Implements common parent node functionality
    DOMNodeIDMap*         fNodeIDMap;     // for use by GetElementsById().

public:
    DOMDocumentImpl(MemoryManager* const manager = XMLPlatformUtils::fgMemoryManager);
    DOMDocumentImpl(const XMLCh*     namespaceURI,     //DOM Level 2
                    const XMLCh*     qualifiedName,
                    DOMDocumentType* doctype,
                    MemoryManager* const manager = XMLPlatformUtils::fgMemoryManager);
    virtual ~DOMDocumentImpl();

    void                         setDocumentType(DOMDocumentType *doctype);

    // Add all functions that are pure virutal in DOMNODE
    DOMNODE_FUNCTIONS;

    // Add all functions that are pure virutal in DOMDocument
    virtual DOMAttr*             createAttribute(const XMLCh *name);
    virtual DOMCDATASection*     createCDATASection(const XMLCh *data);
    virtual DOMComment*          createComment(const XMLCh *data);
    virtual DOMDocumentFragment* createDocumentFragment();
    virtual DOMDocumentType*     createDocumentType(const XMLCh *name);
    virtual DOMDocumentType*     createDocumentType(const XMLCh *qName,
                                                    const XMLCh *publicId,
                                                    const XMLCh *systemId);
    virtual DOMElement*          createElement(const XMLCh * tagName);
    virtual DOMElement*          createElementNoCheck(const XMLCh *tagName);
    virtual DOMEntity*           createEntity(const XMLCh * name);
    virtual DOMEntityReference*  createEntityReference(const XMLCh * name);
    virtual DOMNotation*         createNotation(const XMLCh * name);
    virtual DOMProcessingInstruction* createProcessingInstruction(const XMLCh * target, const XMLCh * data);
    virtual DOMText*             createTextNode(const XMLCh * data);
    virtual DOMDocumentType*     getDoctype() const;
    virtual DOMElement*          getDocumentElement() const;
    virtual DOMNodeList*         getElementsByTagName(const XMLCh * tagname) const;
    virtual DOMImplementation*   getImplementation() const;
    bool                         isXMLName(const XMLCh * s);
    virtual DOMNodeIterator*     createNodeIterator(DOMNode *root,
                                                    unsigned long whatToShow,
                                                    DOMNodeFilter* filter,
                                                    bool entityReferenceExpansion);
    virtual DOMTreeWalker*       createTreeWalker(DOMNode *root,
                                                  unsigned long whatToShow,
                                                  DOMNodeFilter* filter,
                                                  bool entityReferenceExpansion);


    virtual DOMRange*            createRange();
    virtual Ranges*              getRanges() const;  //non-standard api
    virtual NodeIterators*       getNodeIterators() const;  //non-standard api
    virtual void                 removeRange(DOMRangeImpl* range); //non-standard api
    virtual void                 removeNodeIterator(DOMNodeIteratorImpl* nodeIterator); //non-standard api

    virtual const DOMXPathExpression*    createExpression(const XMLCh *expression, const DOMXPathNSResolver *resolver);
    virtual const DOMXPathNSResolver*    createNSResolver(DOMNode *nodeResolver);
    virtual void* evaluate(const XMLCh *expression, DOMNode *contextNode, const DOMXPathNSResolver *resolver, 
                           unsigned short type, void* result);


    // Extension to be called by the Parser
    DOMEntityReference*  createEntityReferenceByParser(const XMLCh * name);


    //
    // Functions to keep track of document mutations, so that node list chached
    //   information can be invalidated.  One global changes counter per document.
    //
    virtual void                 changed();
    virtual int                  changes() const;

    /**
     * Sets whether the DOM implementation performs error checking
     * upon operations. Turning off error checking only affects
     * the following DOM checks:
     * <ul>
     * <li>Checking strings to make sure that all characters are
     *     legal XML characters
     * <li>Hierarchy checking such as allowed children, checks for
     *     cycles, etc.
     * </ul>
     * <p>
     * Turning off error checking does <em>not</em> turn off the
     * following checks:
     * <ul>
     * <li>Read only checks
     * <li>Checks related to DOM events
     * </ul>
     */
    inline void setErrorChecking(bool check) {
        errorChecking = check;
    }

    /**
     * Returns true if the DOM implementation performs error checking.
     */
    inline bool getErrorChecking() const {
        return errorChecking;
    }

    //Introduced in DOM Level 2
    virtual DOMNode*             importNode(DOMNode *source, bool deep);
    virtual DOMElement*          createElementNS(const XMLCh *namespaceURI,
                                                 const XMLCh *qualifiedName);
    virtual DOMElement*          createElementNS(const XMLCh *namespaceURI,
                                                 const XMLCh *qualifiedName,
                                                 const XMLSSize_t lineNo,
                                                 const XMLSSize_t columnNo);
    virtual DOMAttr*             createAttributeNS(const XMLCh *namespaceURI,
                                                   const XMLCh *qualifiedName);
    virtual DOMNodeList*         getElementsByTagNameNS(const XMLCh *namespaceURI,
                                                        const XMLCh *localName) const;
    virtual DOMElement*          getElementById(const XMLCh *elementId) const;

    //Introduced in DOM Level 3
    virtual const XMLCh*         getActualEncoding() const;
    virtual void                 setActualEncoding(const XMLCh* actualEncoding);
    virtual const XMLCh*         getEncoding() const;
    virtual void                 setEncoding(const XMLCh* encoding);
    virtual bool                 getStandalone() const;
    virtual void                 setStandalone(bool standalone);
    virtual const XMLCh*         getVersion() const;
    virtual void                 setVersion(const XMLCh* version);
    virtual const XMLCh*         getDocumentURI() const;
    virtual void                 setDocumentURI(const XMLCh* documentURI);
    virtual bool                 getStrictErrorChecking() const;
    virtual void                 setStrictErrorChecking(bool strictErrorChecking);
    virtual DOMNode*             adoptNode(DOMNode* source);
    virtual void                 normalizeDocument();
    virtual DOMConfiguration*    getDOMConfiguration() const;
    virtual void                 setDOMConfiguration(DOMConfiguration *config);

    // helper functions to prevent storing userdata pointers on every node.
    void*                        setUserData(DOMNodeImpl* n,
                                            const XMLCh* key,
                                            void* data,
                                            DOMUserDataHandler* handler);
    void*                        getUserData(const DOMNodeImpl* n,
                                             const XMLCh* key) const;
    void                         callUserDataHandlers(const DOMNodeImpl* n,
                                                      DOMUserDataHandler::DOMOperationType operation,
                                                      const DOMNode* src,
                                                      const DOMNode* dst) const;
    void                         transferUserData(DOMNodeImpl* n1, DOMNodeImpl* n2);

    DOMNode*                     renameNode(DOMNode* n,
                                            const XMLCh* namespaceURI,
                                            const XMLCh* name);

    //Return the index > 0 of ':' in the given qualified name qName="prefix:localName".
    //Return 0 if there is no ':', or -1 if qName is malformed such as ":abcd".
    static  int                  indexofQualifiedName(const XMLCh * qName);
    static  bool                 isKidOK(DOMNode *parent, DOMNode *child);

    inline DOMNodeIDMap*         getNodeIDMap() {return fNodeIDMap;};


    //
    // Memory Management Functions.  All memory is allocated by and owned by
    //                               a document, and is not recovered until the
    //                               document itself is deleted.
    //
    void*                        allocate(size_t amount);
    void*                        allocate(size_t amount, NodeObjectType type);
    XMLCh*                       cloneString(const XMLCh *src);
    const XMLCh*                 getPooledString(const XMLCh *src);
    void                         deleteHeap();
    void                         release(DOMNode* object, NodeObjectType type);
    void                         releaseDocNotifyUserData(DOMNode* object);
    void                         releaseBuffer(DOMBuffer* buffer);
    DOMBuffer*                   popBuffer();
    MemoryManager*               getMemoryManager() const;

    // Factory methods for getting/creating node lists.
    // Because nothing is ever deleted, the implementation caches and recycles
    //  previously used instances of DOMDeepNodeList
    //
    DOMNodeList*                 getDeepNodeList(const DOMNode *rootNode, const XMLCh *tagName);
    DOMNodeList*                 getDeepNodeList(const DOMNode *rootNode,    //DOM Level 2
                                                 const XMLCh *namespaceURI,
                                                 const XMLCh *localName);

private:
    //Internal helper functions
    virtual DOMNode*             importNode(DOMNode *source, bool deep, bool cloningNode);

    // -----------------------------------------------------------------------
    // Unimplemented constructors and operators
    // -----------------------------------------------------------------------
    DOMDocumentImpl(const DOMDocumentImpl &);
    DOMDocumentImpl & operator = (const DOMDocumentImpl &);

private:
    // -----------------------------------------------------------------------
    //  data
    // -----------------------------------------------------------------------
    // New data introduced in DOM Level 3
    const XMLCh*          fActualEncoding;
    const XMLCh*          fEncoding;
    bool                  fStandalone;
    const XMLCh*          fVersion;
    const XMLCh*          fDocumentURI;
    DOMConfiguration*     fDOMConfiguration;
    
    RefHashTableOf<DOMNodeUserDataTable>* fUserDataTable;


    // Per-Document heap Variables.
    //   The heap consists of one or more biggish blocks which are
    //   sub-allocated for individual allocations of nodes, strings, etc.
    //   The big blocks form a linked list, allowing them to be located for deletion.
    //
    //   There is no provision for deleting suballocated blocks, other than
    //     deleting the entire heap when the document is deleted.
    //
    //   There is no header on individual sub-allocated blocks.
    //   The header on big blocks consists only of a single back pointer to
    //    the previously allocated big block (our linked list of big blocks)
    //
    //
    //   revisit - this heap should be encapsulated into its own
    //                  class, rather than hanging naked on Document.
    //
    void*                 fCurrentBlock;
    char*                 fFreePtr;
    XMLSize_t             fFreeBytesRemaining;

    // To recycle the DOMNode pointer
    RefArrayOf<DOMNodePtr>* fRecycleNodePtr;

    // To recycle DOMBuffer pointer
    RefStackOf<DOMBuffer>* fRecycleBufferPtr;

    // Pool of DOMNodeList for getElementsByTagName
    DOMDeepNodeListPool<DOMDeepNodeListImpl>* fNodeListPool;

    // Other data
    DOMDocumentType*      fDocType;
    DOMElement*           fDocElement;
    DOMStringPool*        fNamePool;
    DOMNormalizer*        fNormalizer;
    Ranges*               fRanges;
    NodeIterators*        fNodeIterators;
    MemoryManager*        fMemoryManager;   // configurable memory manager

    int                   fChanges;
    bool                  errorChecking;    // Bypass error checking.

};

inline MemoryManager* DOMDocumentImpl::getMemoryManager() const
{
    return fMemoryManager;
}

XERCES_CPP_NAMESPACE_END

// ---------------------------------------------------------------------------
//
//  Operator new.  Global overloaded version, lets any object be allocated on
//                 the heap owned by a document.
//
// ---------------------------------------------------------------------------
inline void * operator new(size_t amt, XERCES_CPP_NAMESPACE_QUALIFIER DOMDocument *doc, XERCES_CPP_NAMESPACE_QUALIFIER DOMDocumentImpl::NodeObjectType type)
{
    // revist.  Probably should be a checked cast.
    void *p = ((XERCES_CPP_NAMESPACE_QUALIFIER DOMDocumentImpl *)doc)->allocate(amt, type);
    return p;
}

inline void * operator new(size_t amt, XERCES_CPP_NAMESPACE_QUALIFIER DOMDocument *doc)
{
    // revist.  Probably should be a checked cast.
    void *p = ((XERCES_CPP_NAMESPACE_QUALIFIER DOMDocumentImpl *)doc)->allocate(amt);
    return p;
}

// ---------------------------------------------------------------------------
//  For DOM:
//  Bypass compiler warning:
//    no matching operator delete found; memory will not be freed if initialization throws an exception
// ---------------------------------------------------------------------------
#if _MSC_VER >= 1200 /* VC++ 6.0 */
inline void operator delete(void* ptr, XERCES_CPP_NAMESPACE_QUALIFIER DOMDocument *doc)
{
    return;
}
inline void operator delete(void* ptr, XERCES_CPP_NAMESPACE_QUALIFIER DOMDocument *doc, XERCES_CPP_NAMESPACE_QUALIFIER DOMDocumentImpl::NodeObjectType type)
{
    return;
}
#endif

#endif
