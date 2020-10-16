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
#if !defined(XERCESDOCUMENTWRAPPER_HEADER_GUARD_1357924680)
#define XERCESDOCUMENTWRAPPER_HEADER_GUARD_1357924680



#include <XercesParserLiaisonDefinitions.hpp>



#include <deque>
#include <set>
#include <vector>



#include <XalanDocument.hpp>



#if defined(XALAN_AUTO_PTR_REQUIRES_DEFINITION)
#include <XalanDOMImplementation.hpp>
#include <XalanDOMStringPool.hpp>
#endif



#include <XalanAutoPtr.hpp>



#include <XercesDOMWalker.hpp>
#include <XercesWrapperNavigator.hpp>


#include <XercesWrapperToXalanNodeMap.hpp>
#include <XercesNodeListWrapper.hpp>
#include <XercesAttrWrapperAllocator.hpp>
#include <XercesElementWrapperAllocator.hpp>
#include <XercesTextWrapperAllocator.hpp>



#include <XercesWrapperTypes.hpp>



XALAN_CPP_NAMESPACE_BEGIN



class XalanDOMStringPool;
class XercesCommentWrapper;
class XercesCDATASectionWrapper;
class XercesDocumentFragmentWrapper;
class XercesDocumentTypeWrapper;
class XercesEntityWrapper;
class XercesEntityReferenceWrapper;
class XercesProcessingInstructionWrapper;
class XercesNotationWrapper;



class XALAN_XERCESPARSERLIAISON_EXPORT XercesDocumentWrapper : public XalanDocument
{
public:

	friend class XercesWrapperNavigator;

	/**
	 *
	 * Constructor for XercesDocumentWrapper.
	 *
	 * If the document will be shared amongst multiple threads of execution,
	 * the parameter buildWrapper must be true.  Otherwise, the bridge
	 * nodes will be built on demand, a process which is not synchronized.
	 * This could cause serious problems if multiple threads tried to visit
	 * an unbuilt node at the same time.
	 *
	 * @param theXercesDocument The Xerces document to bridge
	 * @param threadSafe If true, the tree can safely be shared amongst multiple threads.  (Also implies buildWrapper == true)
	 * @param buildWrapper If true, all of the bridge nodes will be built during construction.
	 * @param buildMaps If true, a map of Xerces to Xalan nodes will be built, even if the bridge is built
	 *
	 */
	XercesDocumentWrapper(
			const DOMDocument_Type*		theXercesDocument,
			bool						threadSafe = true,
			bool						buildWrapper = true,
			bool						buildMaps = false);

	virtual
	~XercesDocumentWrapper();

	// These interfaces are inherited from XalanNode...

	virtual const XalanDOMString&
	getNodeName() const;

	virtual const XalanDOMString&
	getNodeValue() const;

	virtual NodeType
	getNodeType() const;

	virtual XalanNode*
	getParentNode() const;

	virtual const XalanNodeList*
	getChildNodes() const;

	virtual XalanNode*
	getFirstChild() const;

	virtual XalanNode*
	getLastChild() const;

	virtual XalanNode*
	getPreviousSibling() const;

	virtual XalanNode*
	getNextSibling() const;

	virtual const XalanNamedNodeMap*
	getAttributes() const;

	virtual XalanDocument*
	getOwnerDocument() const;

#if defined(XALAN_NO_COVARIANT_RETURN_TYPE)
	virtual XalanNode*
#else
	virtual XercesDocumentWrapper*
#endif
	cloneNode(bool deep) const;

	virtual XalanNode*
	insertBefore(
			XalanNode*	newChild,
			XalanNode*	refChild);

	virtual XalanNode*
	replaceChild(
			XalanNode*	newChild,
			XalanNode*	oldChild);

	virtual XalanNode*
	removeChild(XalanNode*	oldChild);

	virtual XalanNode*
	appendChild(XalanNode*	newChild);

	virtual bool
	hasChildNodes() const;

	virtual void
	setNodeValue(const XalanDOMString&		nodeValue);

	virtual void
	normalize();

	virtual bool
	isSupported(
			const XalanDOMString&	feature,
			const XalanDOMString&	version) const;

	virtual const XalanDOMString&
	getNamespaceURI() const;

	virtual const XalanDOMString&
	getPrefix() const;

	virtual const XalanDOMString&
	getLocalName() const;

	virtual void
	setPrefix(const XalanDOMString& prefix);

	virtual bool
	isIndexed() const;

	virtual IndexType
	getIndex() const;

	virtual XalanElement*
	createElement(const XalanDOMString& tagName);

	virtual XalanDocumentFragment*
	createDocumentFragment();

	virtual XalanText*
	createTextNode(const XalanDOMString&	data);

	virtual XalanComment*
	createComment(const XalanDOMString& data);

	virtual XalanCDATASection*
	createCDATASection(const XalanDOMString&	data);

	virtual XalanProcessingInstruction*
	createProcessingInstruction(
			const XalanDOMString&	target,
			const XalanDOMString&	data);

	virtual XalanAttr*
	createAttribute(const XalanDOMString&	name);

	virtual XalanEntityReference*
	createEntityReference(const XalanDOMString& name);

	virtual XalanDocumentType*
	getDoctype() const;

	virtual XalanDOMImplementation*
	getImplementation() const;

	virtual XalanElement*
	getDocumentElement() const;

	virtual XalanNodeList*
	getElementsByTagName(const XalanDOMString&		tagname) const;

	virtual XalanNode*
	importNode(
			XalanNode*	importedNode,
			bool		deep);

	virtual XalanElement*
	createElementNS(
			const XalanDOMString&	namespaceURI,
			const XalanDOMString&	qualifiedName);

	virtual XalanAttr*
	createAttributeNS(
			const XalanDOMString& namespaceURI,
			const XalanDOMString& qualifiedName);

	virtual XalanNodeList*
	getElementsByTagNameNS(
			const XalanDOMString&	namespaceURI,
			const XalanDOMString&	localName) const;

	virtual XalanElement*
	getElementById(const XalanDOMString&	elementId) const;

	// These are some special interfaces to manage relationships between
	// our nodes and Xerces nodes.

	/**
	 * Destroy the entire bridge structure that connects
	 * the Xerces document to this XercesDocumentWrapper
	 * instance.  This will invalidate any pointers to
	 * any nodes in the document (except, of course, the
	 * document itself).
	 */
	void
	destroyWrapper();

	/**
	 * Rebuild the entire bridge structure that connects
	 * the Xerces document to this XercesDocumentWrapper
	 * instance.  This destroys the bridge before
	 * rebuilding.
	 */
	void
	rebuildWrapper();

	/**
	 * Map a Xerces node to the corresponding wrapper node.
	 * If the constructor for the instance was called with
	 * the threadSafe or buildWrapper parameter equal to
	 * true, this call will fail.
	 *
	 * @param theXercesNode The Xerces instance to map
	 *
	 * @return The pointer to the corresponding XalanNode instance, or 0 if the node could not be mapped.
	 */
	XalanNode*
	mapNode(const DOMNodeType* 	theXercesNode) const;

	/**
	 * Map a Xerces node to the corresponding wrapper node.
	 * If the constructor for the instance was called with
	 * the threadSafe or buildWrapper parameter equal to
	 * true, this call will fail.
	 *
	 * @param theXercesNode The Xerces instance to map
	 *
	 * @return The pointer to the corresponding XalanNode instance, or 0 if the node could not be mapped.
	 */
	XalanAttr*
	mapNode(const DOMAttrType* 	theXercesNode) const;

	/**
	 * Map a Xerces node to the corresponding wrapper node.
	 * If the constructor for the instance was called with
	 * the threadSafe or buildWrapper parameter equal to
	 * true, this call will fail.
	 *
	 * @param theXercesNode The Xerces instance to map
	 *
	 * @return The pointer to the corresponding XalanNode instance, or 0 if the node could not be mapped.
	 */
	XalanElement*
	mapNode(const DOMElementType* 	theXercesNode) const;

	/**
	 * Map a XalanNode to the corresponding Xerces node.
	 * If the node not owned by this document, the
	 * function will throw XalanDOMException with the code
	 * WRONG_DOCUMENT_ERR.
	 *
	 * @param theXalanNode The Xalan instance to map
	 *
	 * @return The pointer to the corresponding XalanNode instance, or 0 if the node could not be mapped.
	 */
	const DOMNodeType*
	mapNode(XalanNode* 	theXalanNode) const;

	/**
	 *
	 * Get the Xerces DOMDocument that this XercesDocument represents.
	 *
	 * @return the Xerces DOMDocument instance.
	 *
	 */
	const DOMDocument_Type*
	getXercesDocument() const
	{
		return m_xercesDocument;
	}

	/**
	 * Build the entire bridge structure.  This should be done before any
	 * processing begins, if the tree will be shared amongst multiple
	 * threads.
	 */
	void
	buildWrapperNodes();

#if defined(XALAN_NO_STD_NAMESPACE)
	typedef deque<XercesWrapperNavigator>	WrapperNavigatorVectorType;

	typedef deque<XalanNode*>				NodeVectorType;
#else
	typedef std::deque<XercesWrapperNavigator>	WrapperNavigatorVectorType;

	typedef std::deque<XalanNode*>				NodeVectorType;
#endif

	// Helper class to walk the tree and build everything...
	class BuildWrapperTreeWalker : public XercesDOMWalker
	{
	public:

		typedef XercesDOMWalker				ParentType;
		typedef WrapperNavigatorVectorType	WrapperNavigatorVectorInnerType;

		BuildWrapperTreeWalker(
				XercesDocumentWrapper*				theDocument,
				XercesWrapperNavigator*				theDocumentNavigator,
				WrapperNavigatorVectorInnerType&	theNavigators,
				IndexType							theStartIndex,
				bool								theBuildMapsFlag);

		virtual
		~BuildWrapperTreeWalker();

		struct NavigatorStackEntryType
		{
			NavigatorStackEntryType(
						XercesWrapperNavigator*	theNavigator = 0,
						XalanNode*				theNode = 0) :
				m_navigator(theNavigator),
				m_node(theNode)
			{
			}

			XercesWrapperNavigator*	m_navigator;

			XalanNode*				m_node;
		};

	#if defined(XALAN_NO_STD_NAMESPACE)
		typedef vector<NavigatorStackEntryType>			NavigatorStackType;
	#else
		typedef std::vector<NavigatorStackEntryType>	NavigatorStackType;
	#endif

	protected:

		virtual bool
		startNode(const DOMNodeType*	node);

		virtual bool
		endNode(const DOMNodeType*	node);

#if !defined(XALAN_NO_USING_DECLARATION)
		using ParentType::startNode;
		using ParentType::endNode;
#endif

	private:

		XercesDocumentWrapper*				m_document;

		WrapperNavigatorVectorInnerType&	m_navigators;

		IndexType							m_currentIndex;

		NavigatorStackType					m_parentNavigatorStack;

		NavigatorStackType					m_siblingNavigatorStack;

		const bool							m_buildMaps;
	};


	/**
	 * Get a pooled string.  If the string is not in the pool,
	 * add it.
	 *
	 * @param theString The string to pool.
	 * @return A const reference to the pooled string.
	 */
	const XalanDOMString&
	getPooledString(const XalanDOMString&	theString) const;

	/**
	 * Get a pooled string.  If the string is not in the pool,
	 * add it.
	 *
	 * @param theString The string to pool.
	 * @param theLength The length of the string.  If XalanDOMString::npos, the string is assumed to be null-terminated.
	 * @return A const reference to the pooled string.
	 */
	const XalanDOMString&
	getPooledString(
			const XalanDOMChar*			theString,
			XalanDOMString::size_type	theLength /* = XalanDOMString::npos */) const;

	bool
	getMappingMode() const
	{
		return m_mappingMode;
	}

private:

	// Destruction API...
	void
	destroyWrapperNode(XalanNode*	theNode);

	// Not implemented...
	XercesDocumentWrapper(const XercesDocumentWrapper&	theSource);

	XercesDocumentWrapper&
	operator=(const XercesDocumentWrapper&	theRHS);

	bool
	operator==(const XercesDocumentWrapper&	theRHS) const;

	// Private delete function...
	void
	destroyNode(XalanNode*	theNode);

	// More internal implementation stuff...

	// Factory methods for our implementation nodes...
	XalanNode*
	createWrapperNode(
			const DOMNodeType*	theXercesNode,
			IndexType			theIndex,
			bool				mapNode) const;

	XercesDocumentTypeWrapper*
	createWrapperNode(
			const DOMDocumentType_Type*		theDoctype,
			IndexType						theIndex,
			bool							mapNode) const;

	XercesElementWrapper*
	createWrapperNode(
			const DOMElementType* 	theXercesNode,
			IndexType				theIndex,
			bool					mapNode) const;

	XercesTextWrapper*
	createWrapperNode(
			const DOMTextType*	theXercesNode,
			IndexType			theIndex,
			bool				mapNode) const;

	XercesCommentWrapper*
	createWrapperNode(
			const DOMCommentType*	theXercesNode,
			IndexType				theIndex,
			bool					mapNode) const;

	XercesCDATASectionWrapper*
	createWrapperNode(
			const DOMCDATASectionType*	theXercesNode,
			IndexType					theIndex,
			bool						mapNode) const;

	XercesProcessingInstructionWrapper*
	createWrapperNode(
			const DOMProcessingInstructionType*		theXercesNode,
			IndexType								theIndex,
			bool									mapNode) const;

	XercesAttrWrapper*
	createWrapperNode(
			const DOMAttrType*	theXercesNode,
			IndexType			theIndex,
			bool				mapNode) const;

	XercesEntityWrapper*
	createWrapperNode(
			const DOMEntityType*	theXercesNode,
			IndexType				theIndex,
			bool					mapNode) const;

	XercesEntityReferenceWrapper*
	createWrapperNode(
			const DOMEntityReferenceType*	theXercesNode,
			IndexType						theIndex,
			bool							mapNode) const;

	XercesNotationWrapper*
	createWrapperNode(
			const DOMNotationType*	theXercesNode,
			IndexType				theIndex,
			bool					mapNode) const;

	XercesWrapperNavigator&
	pushNavigator() const;

	// This is a private helper class for building the tree...
	friend class BuildWrapperTreeWalker;

	const DOMDocument_Type* const			m_xercesDocument;

	XalanElement*							m_documentElement;

	mutable XercesWrapperToXalanNodeMap		m_nodeMap;

	XalanAutoPtr<XalanDOMImplementation>	m_domImplementation;

	mutable WrapperNavigatorVectorType		m_navigators;

	// Our navigator will be the first entry in m_navigators,
	// but we'll cache this so access is faster...
	XercesWrapperNavigator*					m_navigator;

	XercesNodeListWrapper					m_children;

	mutable NodeVectorType					m_nodes;

	mutable XercesDocumentTypeWrapper* 		m_doctype;

	bool									m_mappingMode;

	bool									m_indexValid;

	const bool								m_buildMaps;

	mutable XercesElementWrapperAllocator	m_elementAllocator;

	mutable XercesTextWrapperAllocator		m_textAllocator;

	mutable XercesAttrWrapperAllocator		m_attributeAllocator;

	const XalanAutoPtr<XalanDOMStringPool>	m_stringPool;
};



XALAN_CPP_NAMESPACE_END



#endif	// !defined(XERCESDOCUMENTWRAPPER_HEADER_GUARD_1357924680)
