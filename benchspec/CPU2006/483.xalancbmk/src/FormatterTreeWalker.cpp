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
// Class header file.
#include "FormatterTreeWalker.hpp"



// Xerces header files...
#include <XalanElement.hpp>
#include <XalanNamedNodeMap.hpp>



#include <AttributeListImpl.hpp>
#include <DOMStringHelper.hpp>
#include <FormatterListener.hpp>
#include <NamedNodeMapAttributeList.hpp>



XALAN_CPP_NAMESPACE_BEGIN



FormatterTreeWalker::FormatterTreeWalker(FormatterListener& 	formatterListener) :
	TreeWalker(),
	m_formatterListener(formatterListener)
{
}



FormatterTreeWalker::~FormatterTreeWalker()
{
}



bool
FormatterTreeWalker::startNode(const XalanNode*		node)
{
	assert(node != 0);

	switch(node->getNodeType())
	{
	case XalanNode::COMMENT_NODE:
		{
			m_formatterListener.comment(c_wstr(node->getNodeValue()));
		}
		break;

	case XalanNode::DOCUMENT_FRAGMENT_NODE:
		// ??
		break;

	case XalanNode::DOCUMENT_NODE:
		m_formatterListener.startDocument();
		break;

	case XalanNode::ELEMENT_NODE:
		{
			const XalanElement*	theElementNode =
#if defined(XALAN_OLD_STYLE_CASTS)
				(const XalanElement*)node;
#else
				static_cast<const XalanElement*>(node);
#endif

			const XalanNamedNodeMap*	atts = theElementNode->getAttributes();
			assert(atts != 0);

			NamedNodeMapAttributeList	theAttributeList(*atts);

			m_formatterListener.startElement(c_wstr(theElementNode->getNodeName()),
											 theAttributeList);
		}
		break;

	case XalanNode::PROCESSING_INSTRUCTION_NODE:
		{
			m_formatterListener.processingInstruction(
				c_wstr(node->getNodeName()),
				c_wstr(node->getNodeValue()));
		}
		break;

	case XalanNode::CDATA_SECTION_NODE:
		{
			const XalanDOMString&	data = node->getNodeValue();

			assert(length(data) == FormatterListener::size_type(length(data)));

			m_formatterListener.cdata(c_wstr(data), FormatterListener::size_type(length(data)));
		}
		break;

	case XalanNode::TEXT_NODE:
		{
			const XalanDOMString&	data = node->getNodeValue();

			assert(length(data) == FormatterListener::size_type(length(data)));

			m_formatterListener.characters(c_wstr(data), FormatterListener::size_type(length(data)));
		}
		break;

	case XalanNode::ENTITY_REFERENCE_NODE:
		m_formatterListener.entityReference(c_wstr(node->getNodeName()));
		break;

	default:
		// Do nothing...
		break;
	}

	return false;
}



bool
FormatterTreeWalker::startNode(XalanNode*	node)
{
	assert(node != 0);

#if defined(XALAN_OLD_STYLE_CASTS)
	return startNode((const XalanNode*)node);
#else
	return startNode(const_cast<const XalanNode*>(node));
#endif
}



bool
FormatterTreeWalker::endNode(const XalanNode*	node)
{
	assert(node != 0);

	switch(node->getNodeType())
	{
	case XalanNode::DOCUMENT_NODE:
		m_formatterListener.endDocument();
		break;

	case XalanNode::ELEMENT_NODE:
		m_formatterListener.endElement(c_wstr(node->getNodeName()));
		break;

	default:
		// Do nothing
		break;
	}

	return false;
}



bool
FormatterTreeWalker::endNode(XalanNode*		node)
{
#if defined(XALAN_OLD_STYLE_CASTS)
	return endNode((const XalanNode*)node);
#else
	return endNode(const_cast<const XalanNode*>(node));
#endif
}



XALAN_CPP_NAMESPACE_END
