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
#if !defined(STYLESHEETCONSTRUCTIONCONTEXTDEFAULT_HEADER_GUARD_1357924680)
#define STYLESHEETCONSTRUCTIONCONTEXTDEFAULT_HEADER_GUARD_1357924680



// Base include file.  Must be first.
#include <XSLTDefinitions.hpp>



#include <vector>



#include <ArenaAllocator.hpp>
#include <XalanArrayAllocator.hpp>
#include <XalanDOMStringPool.hpp>



#if defined(XALAN_AUTO_PTR_REQUIRES_DEFINITION) || (XALAN_ALLINONE_BUILD_DLL)
#include <XPathProcessor.hpp>
#endif



#include <xalanc/Include/XalanAutoPtr.hpp>



#include <XalanDOMStringCache.hpp>



#include <XalanQNameByReference.hpp>
#include <XalanQNameByValue.hpp>
#include <XalanQNameByValueAllocator.hpp>



// Base class header file...
#include <StylesheetConstructionContext.hpp>



#include <AVT.hpp>
#include <XalanAVTAllocator.hpp>
#include <AVTPartSimple.hpp>
#include <XalanAVTPartSimpleAllocator.hpp>
#include <AVTPartXPath.hpp>
#include <XalanAVTPartXPathAllocator.hpp>
#include <XalanElemApplyTemplatesAllocator.hpp>
#include <XalanElemAttributeAllocator.hpp>
#include <XalanElemAttributeSetAllocator.hpp>
#include <XalanElemCallTemplateAllocator.hpp>
#include <XalanElemElementAllocator.hpp>
#include <XalanElemLiteralResultAllocator.hpp>
#include <XalanElemTemplateAllocator.hpp>
#include <XalanElemTextLiteralAllocator.hpp>
#include <XalanElemValueOfAllocator.hpp>
#include <XalanElemVariableAllocator.hpp>



XALAN_CPP_NAMESPACE_BEGIN



class XPathEnvSupport;
class XPathFactory;
class XPathProcessor;
class XSLTEngineImpl;



/**
 *
 * An default implementation of an abtract class which provides support for
 * constructing the internal representation  of a stylesheet.
 *
 */
class XALAN_XSLT_EXPORT StylesheetConstructionContextDefault : public StylesheetConstructionContext
{
public:

	typedef XalanArrayAllocator<XalanDOMChar>			XalanDOMCharVectorAllocatorType;
	typedef XalanArrayAllocator<const void*>			PointerVectorAllocatorType;
	typedef XalanDOMCharVectorAllocatorType::size_type	VectorAllocatorSizeType;

    // Default size for vector allocation.
	enum {
			eDefaultXalanDOMCharVectorBlockSize = 1024,
			eDefaultAVTBlockSize = 128,
			eDefaultAVTPartSimpleBlockSize = 128,
			eDefaultAVTPartXPathBlockSize = 128,
			eDefaultXalanQNameByValueBlockSize = 32,
			eDefaultPointerVectorBlockSize = 512,
			eDefaultElemApplyTemplatesBlockSize = 10,
			eDefaultElemAttributeBlockSize = 10,
			eDefaultElemAttributeSetBlockSize = 10,
			eDefaultElemCallTemplateBlockSize = 10,
			eDefaultElemElementBlockSize = 10,
			eDefaultElemLiteralResultBlockSize = 20,
			eDefaultElemTemplateBlockSize = 10,
			eDefaultElemTextLiteralBlockSize = 20,
			eDefaultElemValueOfBlockSize = 10,
			eDefaultElemVariableBlockSize = 10 };

	/*
	 * Construct an instance.  If the stylesheet(s) constructed is/are meant to be reused (a.k.a. "compiled"),
	 * the XObjectFactory and XPathFactory instance must exist for the lifetime of the construction context
	 * and, therefore, for the lifetime of the stylesheet(s).  Otherwise, XObject and XPath instance will be
	 * destroyed when the corresponding factories are destryed, leaving pointers to destroyed objects in the.
	 * stylesheet(s).
	 *
	 * @param processor a reference to an XSLTEngineImpl instance.  Used for error reporting.
	 * @param xpathFactory a reference to an XPathFactory instance.  See comments above for important details.
	 * @param theXalanDOMCharVectorAllocatorBlockSize The block size to use for allocating vectors of XalanDOMChars
	 * @param theAVTAllocatorBlockSize The block size to use for allocating AVT instances.
	 * @param theAVTPartSimpleAllocatorBlockSize The block size to use for allocating AVTPartSimple instances.
	 * @param theAVTPartXPathAllocatorBlockSize The block size to use for allocating AVTPartXPath instances.
	 * @param theXalanQNameByValueAllocatorBlockSize The block size to use for allocating XalanQNameByValue instances.
	 * @param thePointerVectorAllocatorBlockSize The block size to use for allocating vectors of pointers.
	 */
	StylesheetConstructionContextDefault(
			XSLTEngineImpl&							processor,
			XPathFactory&							xpathFactory,
			VectorAllocatorSizeType					theXalanDOMCharVectorAllocatorBlockSize = eDefaultXalanDOMCharVectorBlockSize,
			XalanAVTAllocator::size_type			theAVTAllocatorBlockSize = eDefaultAVTBlockSize,
			XalanAVTPartSimpleAllocator::size_type	theAVTPartSimpleAllocatorBlockSize = eDefaultAVTPartSimpleBlockSize,
			XalanAVTPartXPathAllocator::size_type	theAVTPartXPathAllocatorBlockSize = eDefaultAVTPartXPathBlockSize,
			XalanQNameByValueAllocator::size_type	theXalanQNameByValueAllocatorBlockSize = eDefaultXalanQNameByValueBlockSize,
			VectorAllocatorSizeType					thePointerVectorAllocatorBlockSize = eDefaultPointerVectorBlockSize);

	virtual
	~StylesheetConstructionContextDefault();


	virtual void
	error(
			const XalanDOMString&		msg,
			const XalanNode* 			sourceNode = 0,
			const ElemTemplateElement*	styleNode = 0) const;

	virtual void
	error(
			const XalanDOMString&	msg,
			const XalanNode* 		sourceNode,
			const LocatorType* 		locator) const;

	virtual void
	error(
			const char*					msg,
			const XalanNode* 			sourceNode = 0,
			const ElemTemplateElement* 	styleNode = 0) const;

	virtual void
	error(
			const char*			msg,
			const XalanNode* 	sourceNode,
			const LocatorType* 	locator) const;

	virtual void
	warn(
			const XalanDOMString&		msg,
			const XalanNode* 			sourceNode = 0,
			const ElemTemplateElement* 	styleNode = 0) const;

	virtual void
	warn(
			const XalanDOMString&	msg,
			const XalanNode* 		sourceNode,
			const LocatorType* 		locator) const;

	virtual void
	warn(
			const char*					msg,
			const XalanNode* 			sourceNode = 0,
			const ElemTemplateElement* 	styleNode = 0) const;

	virtual void
	warn(
			const char*			msg,
			const XalanNode* 	sourceNode,
			const LocatorType* 	locator) const;

	virtual void
	message(
			const XalanDOMString&		msg,
			const XalanNode* 			sourceNode = 0,
			const ElemTemplateElement* 	styleNode = 0) const;

	virtual void
	message(
			const XalanDOMString&	msg,
			const XalanNode* 		sourceNode,
			const LocatorType*		locator) const;

	virtual void
	message(
			const char*					msg,
			const XalanNode* 			sourceNode = 0,
			const ElemTemplateElement* 	styleNode = 0) const;

	virtual void
	message(
			const char*			msg,
			const XalanNode* 	sourceNode,
			const LocatorType* 	locator) const;

	// These interfaces are inherited from StylesheetConstructionContext...

	virtual void
	reset();

	virtual StylesheetRoot*
	create(const XalanDOMString&	theBaseIdentifier);

	virtual StylesheetRoot*
	create(const XSLTInputSource&	theInputSource);

	virtual Stylesheet*
	create(
			StylesheetRoot&			theStylesheetRoot,
			const XalanDOMString&	theBaseIdentifier);

	virtual void
	destroy(StylesheetRoot*		theStylesheetRoot);

	virtual URLAutoPtrType
	getURLFromString(const XalanDOMString&	urlString);

	virtual XalanDOMString
	getURLStringFromString(const XalanDOMString&	urlString);

	virtual URLAutoPtrType
	getURLFromString(
			const XalanDOMString&	urlString,
			const XalanDOMString&	base);

	virtual XalanDOMString
	getURLStringFromString(
			const XalanDOMString&	urlString,
			const XalanDOMString&	base);

	virtual const XalanDOMString&
	getXSLTNamespaceURI() const;

	virtual XPath*
	createMatchPattern(
			const LocatorType*		locator,
			const XalanDOMString&	str,
			const PrefixResolver&	resolver);

	virtual XPath*
	createMatchPattern(
			const LocatorType*		locator,
			const XalanDOMChar*		str,
			const PrefixResolver&	resolver);

	virtual XPath*
	createXPath(
			const LocatorType*		locator,
			const XalanDOMString&	str,
			const PrefixResolver&	resolver);

	virtual XPath*
	createXPath(
			const LocatorType*			locator,
			const XalanDOMChar*			str,
			XalanDOMString::size_type	len,
			const PrefixResolver&		resolver);

	virtual XPath*
	createXPath(
			const LocatorType*		locator,
			const XalanDOMChar*		str,
			const PrefixResolver&	resolver);

	virtual const LocatorType*
	getLocatorFromStack() const;

	virtual void
	pushLocatorOnStack(const LocatorType*	locator);

	virtual void
	popLocatorStack();

	virtual const XalanDOMString&
	getXalanXSLNameSpaceURL() const;

	virtual XalanDocument*
	parseXML(
			const XalanDOMString&	urlString,
			DocumentHandlerType*	docHandler, 
			XalanDocument*			docToRegister);

	virtual bool
	isXMLSpaceAttribute(
			const XalanDOMChar*		theAttributeName,
			const Stylesheet&		theStylesheet,
			const LocatorType*		theLocator = 0);

	virtual bool
	isXSLUseAttributeSetsAttribute(
			const XalanDOMChar*		theAttributeName,
			const Stylesheet&		theStylesheet,
			const LocatorType*		theLocator = 0);

	virtual bool
	isValidQName(
			const XalanDOMChar*		theName,
			const Stylesheet&		theStylesheet,
			const LocatorType*		theLocator = 0);

	virtual eElementToken
	getElementToken(const XalanDOMString&	name) const;

	virtual double
	getXSLTVersionSupported() const;

	virtual const XalanDOMString&
	getPooledString(const XalanDOMString&	theString);

	virtual const XalanDOMString&
	getPooledString(
			const XalanDOMChar*			theString,
			XalanDOMString::size_type	theLength = XalanDOMString::npos);

	virtual XalanDOMString&
	getCachedString();

	virtual bool
	releaseCachedString(XalanDOMString&		theString);

	virtual XalanDOMChar*
	allocateXalanDOMCharVector(XalanDOMString::size_type	theLength);

	virtual XalanDOMChar*
	allocateXalanDOMCharVector(
			const XalanDOMChar*			theString,
			XalanDOMString::size_type	theLength = XalanDOMString::npos,
			bool						fTerminate = true);

	virtual const AVT*
	createAVT(
			const LocatorType*		locator,
			const XalanDOMChar*		name,
			const XalanDOMChar*		stringedValue,
			const PrefixResolver&	resolver);

	virtual const AVTPart*
	createAVTPart(
			const XalanDOMChar*			theString,
			XalanDOMString::size_type	theLength = XalanDOMString::npos);

	virtual const AVTPart*
	createAVTPart(
			const LocatorType*			locator,
			const XalanDOMChar*			str,
			XalanDOMString::size_type	len,
			const PrefixResolver&		resolver);

	virtual const AVT**
	allocateAVTPointerVector(size_type	theLength);

	virtual const AVTPart**
	allocateAVTPartPointerVector(size_type	theLength);

	virtual const XalanQName*
	createXalanQName(
			const XalanDOMString&		qname,
			const NamespacesStackType&	namespaces,
			const LocatorType*			locator = 0,
			bool						fUseDefault = false);

	virtual const XalanQName*
	createXalanQName(
			const XalanDOMChar*			qname,
			const NamespacesStackType&	namespaces,
			const LocatorType*			locator = 0,
			bool						fUseDefault = false);

	virtual const XalanQName**
	tokenizeQNames(
			size_type&					count,
			const XalanDOMChar*			qnameTokens,
			const NamespacesStackType&	namespaces,
			const LocatorType*			locator = 0,
			bool						fUseDefault = false);

	virtual ElemTemplateElement*
	createElement(
			int							token,
			Stylesheet&					stylesheetTree,
			const AttributeListType&	atts,
			const LocatorType*			locator = 0);

	virtual ElemTemplateElement*
	createElement(
			int							token,
			Stylesheet&					stylesheetTree,
			const XalanDOMChar*			name,
			const AttributeListType&	atts,
			const LocatorType*			locator = 0);

	virtual ElemTemplateElement*
	createElement(
			Stylesheet&					stylesheetTree,
            const XalanDOMChar*			chars,
			XalanDOMString::size_type	length,
			bool						preserveSpace,
            bool						disableOutputEscaping,
			const LocatorType*			locator = 0);

	virtual ElemTemplateElement*
	createElement(
			Stylesheet&					stylesheetTree,
			const XalanDOMChar*			name,
			const AttributeListType&	atts,
			ExtensionNSHandler&			handler,
			const LocatorType*			locator = 0);

	static eElementToken
	getElementNameToken(const XalanDOMString&	name);

#if defined(XALAN_NO_STD_NAMESPACE)
	typedef vector<StylesheetRoot*>			StylesheetVectorType;
	typedef vector<ElemTemplateElement*>	ElemTemplateElementVectorType;
#else
	typedef std::vector<StylesheetRoot*>		StylesheetVectorType;
	typedef std::vector<ElemTemplateElement*>	ElemTemplateElementVectorType;
#endif

private:

	const AVT**
	doAllocateAVTPointerVector(size_type	theSize)
	{
		assert(sizeof(AVT**) == sizeof(PointerVectorAllocatorType::value_type));

#if defined(XALAN_OLD_STYLE_CASTS)
		return (const AVT**)m_pointerVectorAllocator.allocate(theSize);
#else
		return reinterpret_cast<const AVT**>(m_pointerVectorAllocator.allocate(theSize));
#endif
	}

	const AVTPart**
	doAllocateAVTPartPointerVector(size_type	theSize)
	{
		assert(sizeof(AVTPart**) == sizeof(PointerVectorAllocatorType::value_type));

#if defined(XALAN_OLD_STYLE_CASTS)
		return (const AVTPart**)m_pointerVectorAllocator.allocate(theSize);
#else
		return reinterpret_cast<const AVTPart**>(m_pointerVectorAllocator.allocate(theSize));
#endif
	}

	const XalanQName**
	doAllocateXalanQNamePointerVector(size_type	theSize)
	{
		assert(sizeof(XalanQName**) == sizeof(PointerVectorAllocatorType::value_type));

#if defined(XALAN_OLD_STYLE_CASTS)
		return (const XalanQName**)m_pointerVectorAllocator.allocate(theSize);
#else
		return reinterpret_cast<const XalanQName**>(m_pointerVectorAllocator.allocate(theSize));
#endif
	}


	XSLTEngineImpl&							m_processor;

	XPathFactory&							m_xpathFactory;

	typedef XalanAutoPtr<XPathProcessor>	XPathProcessAutoPtr;

	XPathProcessAutoPtr						m_xpathProcessor;

	StylesheetVectorType					m_stylesheets;

	XalanDOMStringPool						m_stringPool;

	XalanDOMCharVectorAllocatorType			m_xalanDOMCharVectorAllocator;

	mutable XalanDOMString					m_tempBuffer;

	XalanQNameByValue						m_scratchQName;

	XalanDOMStringCache						m_stringCache;

	XalanAVTAllocator						m_avtAllocator;

	XalanAVTPartSimpleAllocator				m_avtPartSimpleAllocator;

	XalanAVTPartXPathAllocator				m_avtPartXPathAllocator;

	XalanQNameByValueAllocator				m_xalanQNameByValueAllocator;

	const XalanQNameByReference				m_useAttributeSetsQName;

	PointerVectorAllocatorType				m_pointerVectorAllocator;

	ElemTemplateElementVectorType			m_allocatedElements;

	XalanElemApplyTemplatesAllocator		m_elemApplyTemplatesAllocator;

	XalanElemAttributeAllocator				m_elemAttributeAllocator;

	XalanElemAttributeSetAllocator			m_elemAttributeSetAllocator;

	XalanElemCallTemplateAllocator			m_elemCallTemplateAllocator;

	XalanElemElementAllocator				m_elemElementAllocator;

	XalanElemLiteralResultAllocator			m_elemLiteralResultAllocator;

	XalanElemTemplateAllocator				m_elemTemplateAllocator;

	XalanElemTextLiteralAllocator			m_elemTextLiteralAllocator;

	XalanElemValueOfAllocator				m_elemValueOfAllocator;

	XalanElemVariableAllocator				m_elemVariableAllocator;

	const XalanQNameByReference				m_spaceAttrQName;

	// Static strings for stylesheet compilation...

	// The string "if"
	static const XalanDOMChar	s_if[];

	// The string "key"
	static const XalanDOMChar	s_key[];

	// The string "copy"
	static const XalanDOMChar	s_copy[];

	// The string "sort"
	static const XalanDOMChar	s_sort[];

	// The string "text"
	static const XalanDOMChar	s_text[];

	// The string "when"
	static const XalanDOMChar	s_when[];

	// The string "empty"
	static const XalanDOMChar	s_empty[];

	// The string "param"
	static const XalanDOMChar	s_param[];

	// The string "choose"
	static const XalanDOMChar	s_choose[];

	// The string "import"
	static const XalanDOMChar	s_import[];

	// The string "number"
	static const XalanDOMChar	s_number[];

	// The string "output"
	static const XalanDOMChar	s_output[];

	// The string "comment"
	static const XalanDOMChar	s_comment[];

	// The string "copy-of"
	static const XalanDOMChar	s_copyOf[];

	// The string "element"
	static const XalanDOMChar	s_element[];

	// The string "include"
	static const XalanDOMChar	s_include[];

	// The string "message"
	static const XalanDOMChar	s_message[];

	// The string "fallback"
	static const XalanDOMChar	s_fallback[];

	// The string "for-each"
	static const XalanDOMChar	s_forEach[];

	// The string "template"
	static const XalanDOMChar	s_template[];

	// The string "value-of"
	static const XalanDOMChar	s_valueOf[];

	// The string "variable"
	static const XalanDOMChar	s_variable[];

	// The string "attribute"
	static const XalanDOMChar	s_attribute[];

	// The string "otherwise"
	static const XalanDOMChar	s_otherwise[];

	// The string "transform"
	static const XalanDOMChar	s_transform[];

	// The string "stylesheet"
	static const XalanDOMChar	s_stylesheet[];

	// The string "with-param"
	static const XalanDOMChar	s_withParam[];

	// The string "strip-space"
	static const XalanDOMChar	s_stripSpace[];

	// The string "apply-imports"
	static const XalanDOMChar	s_applyImports[];

	// The string "attribute-set"
	static const XalanDOMChar	s_attributeSet[];

	// The string "call-template"
	static const XalanDOMChar	s_callTemplate[];

	// The string "decimal-format"
	static const XalanDOMChar	s_decimalFormat[];

	// The string "preserve-space"
	static const XalanDOMChar	s_preserveSpace[];

	// The string "apply-templates"
	static const XalanDOMChar	s_applyTemplates[];

	// The string "namespace-alias"
	static const XalanDOMChar	s_namespaceAlias[];

	// The string "processing-instruction"
	static const XalanDOMChar	s_processingInstruction[];

	// A struct for an array that maps stylesheet element names
	// to int tokens.
	struct ElementTokenTableEntry
	{
		const XalanDOMChar*		m_name;

		eElementToken			m_token;
	};

	static const ElementTokenTableEntry		s_elementTokenTable[];

	static const unsigned int				s_elementTokenTableSize;

	static const ElementTokenTableEntry&	s_elementTokenTableLast;

	static const ElementTokenTableEntry&	s_elementTokenTableDummy;
};



XALAN_CPP_NAMESPACE_END



#endif	// STYLESHEETCONSTRUCTIONCONTEXTDEFAULT_HEADER_GUARD_1357924680
