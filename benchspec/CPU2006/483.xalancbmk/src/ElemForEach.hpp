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
#if !defined(XALAN_ELEMFOREACH_HEADER_GUARD)
#define XALAN_ELEMFOREACH_HEADER_GUARD 



// Base include file.  Must be first.
#include "XSLTDefinitions.hpp"



#include <vector>



// Base class header file.
#include "ElemTemplateElement.hpp"



#include <NodeRefListBase.hpp>



#include "Constants.hpp"



XALAN_CPP_NAMESPACE_BEGIN



class ElemSort;
class ElemTemplate;
class NodeSorter;
class XPath;



class ElemForEach: public ElemTemplateElement
{
public:

	/**
	 * Construct an object corresponding to an "xsl:for-each" element
	 * 
	 * @param constructionContext context for construction of object
	 * @param stylesheetTree      stylesheet containing element
	 * @param atts                list of attributes for element
	 * @param lineNumber			line number in document
	 * @param columnNumber			column number in document
	 */
	ElemForEach(
			StylesheetConstructionContext&	constructionContext,
			Stylesheet&						stylesheetTree,
			const AttributeListType&		atts,
			int								lineNumber,
			int								columnNumber);

	virtual
	~ElemForEach();

#if defined(XALAN_NO_STD_NAMESPACE)
	typedef vector<ElemSort*>		SortElemsVectorType;
#else
	typedef std::vector<ElemSort*>	SortElemsVectorType;
#endif


	// These methods are inherited from ElemTemplateElement ...

	virtual const XalanDOMString&
	getElementName() const;

	virtual void
	processSortElement(
			StylesheetConstructionContext&	constructionContext,
			Stylesheet&						theStylesheet,
			const AttributeListType&		atts,
			const LocatorType*				locator = 0);

	virtual void
	postConstruction(
			StylesheetConstructionContext&	constructionContext,
			const NamespacesHandler&		theParentHandler);

	virtual void
	execute(StylesheetExecutionContext&		executionContext) const;
	
	virtual	const XPath*
	getXPath(unsigned int	index = 0) const;

protected:

	/**
	 * Construct an object derived from ElemForEach
	 * 
	 * @param constructionContext context for construction of object
	 * @param stylesheetTree      stylesheet containing element
	 * @param lineNumber			line number in document
	 * @param columnNumber			column number in document
	 * @param xslToken             an integer representing the type of instance.
	 */
	ElemForEach(
			StylesheetConstructionContext&	constructionContext,
			Stylesheet&						stylesheetTree,
			int								lineNumber,
			int								columnNumber,
			int								xslToken);

	/**
	 * Perform a query if needed, and call transformChild for each child.
	 * 
	 * @param executionContext  The current execution context
	 * @param template The owning template context.
	 * @param sourceNodeContext The current source node context.
	 */
	void
	transformSelectedChildren(
			StylesheetExecutionContext&		executionContext,
			const ElemTemplateElement*		theTemplate) const;

	/**
	 * Perform a query if needed, and call transformChild for each child.
	 * 
	 * @param executionContext The current execution context
	 * @param theTemplate The owning template context.
	 * @param sourceNodes The source nodes to transform.
	 * @param sourceNodesCount The count of source nodes to transform.
	 */
	void
	transformSelectedChildren(
			StylesheetExecutionContext& 	executionContext,
			const ElemTemplateElement*		theTemplate,
			const NodeRefListBase&			sourceNodes,
			NodeRefListBase::size_type		sourceNodesCount) const;

	/**
	 * Perform a query if needed, and call transformChild for each child.
	 * 
	 * @param executionContext	The current execution context
	 * @param template The owning template context.
	 * @param sorter The NodeSorter instance, if any.
	 * @param selectStackFrameIndex stack frame context for executing the
	 *								select statement
	 */
	virtual void
	selectAndSortChildren(
			StylesheetExecutionContext& 	executionContext,
			const ElemTemplateElement*		theTemplate,
			NodeSorter* 					sorter,
			int 							selectStackFrameIndex) const;

	const XPath*			m_selectPattern;

private:

	SortElemsVectorType				m_sortElems;

	SortElemsVectorType::size_type	m_sortElemsCount;

};



XALAN_CPP_NAMESPACE_END



#endif	// XALAN_ELEMFOREACH_HEADER_GUARD
