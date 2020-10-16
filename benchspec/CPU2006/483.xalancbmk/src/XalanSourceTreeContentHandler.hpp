/*
 * Copyright 1999-2004 The Apache Software Foundation.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

#if !defined(XALANSOURCETREECONTENTHANDLER_HEADER_GUARD_1357924680)
#define XALANSOURCETREECONTENTHANDLER_HEADER_GUARD_1357924680



#include <XalanSourceTreeDefinitions.hpp>



#include <vector>



#include <DocumentHandler.hpp>
#include <DTDHandler.hpp>
#include <ContentHandler.hpp>
#include <LexicalHandler.hpp>



#include <XalanDOMString.hpp>



XALAN_DECLARE_XERCES_CLASS(Attributes)
XALAN_DECLARE_XERCES_CLASS(AttributeList)
XALAN_DECLARE_XERCES_CLASS(ContentHandler)
XALAN_DECLARE_XERCES_CLASS(DTDHandler)
XALAN_DECLARE_XERCES_CLASS(LexicalHandler)
XALAN_DECLARE_XERCES_CLASS(Locator)



XALAN_CPP_NAMESPACE_BEGIN



typedef XERCES_CPP_NAMESPACE_QUALIFIER Attributes		AttributesType;
typedef XERCES_CPP_NAMESPACE_QUALIFIER AttributeList	AttributeListType;
typedef XERCES_CPP_NAMESPACE_QUALIFIER ContentHandler	ContentHandlerType;
typedef XERCES_CPP_NAMESPACE_QUALIFIER DTDHandler		DTDHandlerType;
typedef XERCES_CPP_NAMESPACE_QUALIFIER LexicalHandler	LexicalHandlerType;
typedef XERCES_CPP_NAMESPACE_QUALIFIER Locator			LocatorType;



class XalanNode;
class XalanSourceTreeDocument;
class XalanSourceTreeElement;



class XALAN_XALANSOURCETREE_EXPORT XalanSourceTreeContentHandler :
	public ContentHandlerType,
	public DTDHandlerType,
	public LexicalHandlerType
{
public:

#if defined(XALAN_NO_STD_NAMESPACE)
	typedef vector<XalanSourceTreeElement*> 		ElementStackType;
	typedef vector<XalanNode*> 						LastChildStackType;
#else
	typedef std::vector<XalanSourceTreeElement*>	ElementStackType;
	typedef std::vector<XalanNode*> 				LastChildStackType;
#endif

	enum { eDefaultStackSize = 50, eDefaultTextBufferSize = 100 };


	// Constructor
	explicit
	XalanSourceTreeContentHandler(
			XalanSourceTreeDocument*	theDocument = 0,
			bool						fAccumulateText = true);

	virtual
	~XalanSourceTreeContentHandler();


	// Inherited from ContentHandler...
	virtual void
	characters(
			const XMLCh* const	chars,
			const unsigned int	length);

	virtual void
	endDocument();

	virtual void
	endElement(
			const XMLCh* const	uri, 
			const XMLCh* const	localname, 
			const XMLCh* const	qname);

	virtual void
	ignorableWhitespace(
			const XMLCh* const	chars,
			const unsigned int	length);

	virtual void
	processingInstruction(
		const XMLCh* const	target,
		const XMLCh* const	data);

	virtual void
	setDocumentLocator(const LocatorType* const 	locator);

	virtual void
	startDocument();

	virtual void
	startElement(
			const XMLCh* const		uri,
			const XMLCh* const		localname,
			const XMLCh* const		qname,
			const AttributesType&	attrs);

	virtual void
	startPrefixMapping(
		const XMLCh* const	prefix,
		const XMLCh* const	uri);

	virtual void
	endPrefixMapping(const XMLCh* const 	prefix);


	virtual void
	skippedEntity(const XMLCh* const	name);


	// Inherited from DTDHandler...

	virtual void
	notationDecl(
			const XMLCh* const	  name,
			const XMLCh* const	  publicId,
			const XMLCh* const	  systemId);

	virtual void
	unparsedEntityDecl(
			const XMLCh* const	  name,
			const XMLCh* const	  publicId,
			const XMLCh* const	  systemId,
			const XMLCh* const	  notationName);

	virtual void
	resetDocType();


	// Inherited from LexicalHandler...

	virtual void
	comment(
			const XMLCh* const	chars,
			const unsigned int	length);

	virtual void
	endCDATA();

	virtual void
	endDTD();

	virtual void
	endEntity(const XMLCh* const	name);

	virtual void
	startCDATA();

	virtual void
	startDTD(
			const XMLCh* const	name,
			const XMLCh* const	publicId,
			const XMLCh* const	systemId);

	virtual void
	startEntity(const XMLCh* const	name);

	
	// New to XalanSourceTreeContentHandler...

	XalanSourceTreeDocument*
	getDocument() const
	{
		return m_document;
	}

	void
	setDocument(XalanSourceTreeDocument*	theDocument);

private:

	// Not implemented...
	XalanSourceTreeContentHandler(const XalanSourceTreeContentHandler&);

	XalanSourceTreeContentHandler&
	operator=(const XalanSourceTreeContentHandler&);

	bool
	operator==(const XalanSourceTreeContentHandler&) const;

	// Helper functions...
	XalanSourceTreeElement*
	createElement(
			const XMLCh* const			uri,
			const XMLCh* const			localname,
			const XMLCh* const			qname,
			const AttributesType&		attrs,
			XalanSourceTreeElement* 	theOwnerElement);

	void
	processAccumulatedText();

	void
	doCharacters(
			const XMLCh*				chars,
			XalanDOMString::size_type	length);

	// Data members...

	// The current document we're building...
	XalanSourceTreeDocument*	m_document;

	// The current element...
	XalanSourceTreeElement* 	m_currentElement;

	// Stack of elements...
	ElementStackType			m_elementStack;

	// The last child appended to the current element.  This is
	// an important optimization, because XalanSourceTreeElement
	// does not have a pointer to its last child.  Without this,
	// appending a child becomes a linear search.
	XalanNode* 					m_lastChild;

	// Stack of last children appended.  There is a ono-to-one
	// correspondance to the entries in m_elementStack.
	LastChildStackType			m_lastChildStack;

	// If true, the handler will accumulate text from calls to
	// characters() until another event triggers the creation
	// of the node.
	const bool					m_accumulateText;

	// A buffer to hold accumulated text.
	XalanDOMString				m_textBuffer;

	// A flag to determine if the DTD is being processed.
	bool						m_inDTD;
};



XALAN_CPP_NAMESPACE_END



#endif	// #if !defined(XALANSOURCETREECONTENTHANDLER_HEADER_GUARD_1357924680)
