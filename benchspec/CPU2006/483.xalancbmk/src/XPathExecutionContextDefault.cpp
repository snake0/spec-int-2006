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

// Class header file...
#include "XPathExecutionContextDefault.hpp"



#include <Locator.hpp>



#include <xalanc/Include/STLHelper.hpp>



#include <PrefixResolver.hpp>
#include <XalanLocator.hpp>
#include <DoubleSupport.hpp>
#include <DOMStringHelper.hpp>
#include <XalanDecimalFormatSymbols.hpp>
#include <XalanMessageLoader.hpp>



#include <DOMSupport.hpp>



#include "XObjectFactory.hpp"
#include "XalanQName.hpp"
#include "XPathEnvSupport.hpp"



XALAN_CPP_NAMESPACE_BEGIN



const NodeRefList	XPathExecutionContextDefault::s_dummyList;



XPathExecutionContextDefault::XPathExecutionContextDefault(
			XPathEnvSupport&		theXPathEnvSupport,
			DOMSupport&				theDOMSupport,
			XObjectFactory&			theXObjectFactory,
			XalanNode*				theCurrentNode,
			const NodeRefListBase*	theContextNodeList,
			const PrefixResolver*	thePrefixResolver) :
	XPathExecutionContext(&theXObjectFactory),
	m_xpathEnvSupport(&theXPathEnvSupport),
	m_domSupport(&theDOMSupport),
	m_currentNodeStack(),
	m_contextNodeListStack(),
	m_prefixResolver(thePrefixResolver),
	m_nodeListCache(eNodeListCacheListSize),
	m_stringCache(),
	m_cachedPosition(),
	m_scratchQName()
{
	m_currentNodeStack.push_back(theCurrentNode);

	m_contextNodeListStack.push_back(theContextNodeList == 0 ? &s_dummyList : theContextNodeList);
}



XPathExecutionContextDefault::XPathExecutionContextDefault(
			XalanNode*				theCurrentNode,
			const NodeRefListBase*	theContextNodeList,
			const PrefixResolver*	thePrefixResolver) :
	XPathExecutionContext(),
	m_xpathEnvSupport(0),
	m_domSupport(0),
	m_currentNodeStack(),
	m_contextNodeListStack(),
	m_prefixResolver(thePrefixResolver),
	m_nodeListCache(eNodeListCacheListSize),
	m_stringCache(),
	m_cachedPosition(),
	m_scratchQName()
{
	m_currentNodeStack.push_back(theCurrentNode);

	m_contextNodeListStack.push_back(theContextNodeList == 0 ? &s_dummyList : theContextNodeList);
}




XPathExecutionContextDefault::~XPathExecutionContextDefault()
{
	reset();
}



void
XPathExecutionContextDefault::reset()
{
	if (m_xpathEnvSupport != 0)
	{
		m_xpathEnvSupport->reset();
	}

	if (m_domSupport != 0)
	{
		m_domSupport->reset();
	}

	if (m_xobjectFactory != 0)
	{
		m_xobjectFactory->reset();
	}

	m_currentNodeStack.clear();
	m_currentNodeStack.push_back(0);

	m_contextNodeListStack.clear();
	m_contextNodeListStack.push_back(&s_dummyList);

	m_prefixResolver = 0;

	m_nodeListCache.reset(),

	m_stringCache.reset();

	m_cachedPosition.clear();
}



XalanNode*
XPathExecutionContextDefault::getCurrentNode() const
{
    assert(m_currentNodeStack.empty() == false);

	return m_currentNodeStack.back();
}



void
XPathExecutionContextDefault::pushCurrentNode(XalanNode*	theCurrentNode)
{
	m_currentNodeStack.push_back(theCurrentNode);
}



void
XPathExecutionContextDefault::popCurrentNode()
{
    assert(m_currentNodeStack.empty() == false);

	m_currentNodeStack.pop_back();
}



bool
XPathExecutionContextDefault::isNodeAfter(
			const XalanNode&	node1,
			const XalanNode&	node2) const
{
	return m_domSupport->isNodeAfter(node1, node2);
}



void	
XPathExecutionContextDefault::pushContextNodeList(const NodeRefListBase&	theList)
{
	m_cachedPosition.clear();

	m_contextNodeListStack.push_back(&theList);
}



void	
XPathExecutionContextDefault::popContextNodeList()
{
	m_cachedPosition.clear();

	m_contextNodeListStack.pop_back();
}



const NodeRefListBase&
XPathExecutionContextDefault::getContextNodeList() const
{
	assert(m_contextNodeListStack.empty() == false);

	return *m_contextNodeListStack.back();
}



XPathExecutionContextDefault::size_type
XPathExecutionContextDefault::getContextNodeListLength() const
{
	assert(m_contextNodeListStack.empty() == false);

	return m_contextNodeListStack.back()->getLength();
}



XPathExecutionContextDefault::size_type
XPathExecutionContextDefault::getContextNodeListPosition(const XalanNode&	contextNode) const
{
	assert(m_contextNodeListStack.empty() == false);

	if (m_cachedPosition.m_node == &contextNode)
	{
		assert((m_cachedPosition.m_index == 0 && m_contextNodeListStack.back()->indexOf(&contextNode) == NodeRefListBase::npos) ||
			   (m_contextNodeListStack.back()->indexOf(&contextNode) + 1 == m_cachedPosition.m_index));
	}
	else
	{
		// Get the index of the node...
		const size_type		theIndex = m_contextNodeListStack.back()->indexOf(&contextNode);

		// If not found, it's 0.  Otherwise, it's the index + 1
#if defined(XALAN_NO_MUTABLE)
		((XPathExecutionContextDefault*)this)->m_cachedPosition.m_index = theIndex == NodeRefListBase::npos ? 0 : theIndex + 1;
		((XPathExecutionContextDefault*)this)->m_cachedPosition.m_node = &contextNode;
#else
		m_cachedPosition.m_index = theIndex == NodeRefListBase::npos ? 0 : theIndex + 1;
		m_cachedPosition.m_node = &contextNode;
#endif
	}

	return m_cachedPosition.m_index;
}



bool
XPathExecutionContextDefault::elementAvailable(const XalanQName&	theQName) const
{
	assert(m_xpathEnvSupport != 0);

	return m_xpathEnvSupport->elementAvailable(theQName.getNamespace(), theQName.getLocalPart());
}



bool
XPathExecutionContextDefault::elementAvailable(
			const XalanDOMString&	theName, 
			const LocatorType*		theLocator) const
{
	assert(m_xpathEnvSupport != 0);

	XalanQNameByValue&	theQName = getScratchQName();

	theQName.set(theName, m_prefixResolver, theLocator);

	return elementAvailable(m_scratchQName);
}



bool
XPathExecutionContextDefault::functionAvailable(const XalanQName&	theQName) const
{
	assert(m_xpathEnvSupport != 0);

	return m_xpathEnvSupport->functionAvailable(theQName.getNamespace(), theQName.getLocalPart());
}



bool
XPathExecutionContextDefault::functionAvailable(
			const XalanDOMString&	theName, 
			const LocatorType*		theLocator) const
{
	assert(m_xpathEnvSupport != 0);

	XalanQNameByValue&	theQName = getScratchQName();

	theQName.set(theName, m_prefixResolver, theLocator);

	return functionAvailable(theQName);
}



const XObjectPtr
XPathExecutionContextDefault::extFunction(
			const XalanDOMString&			theNamespace,
			const XalanDOMString&			functionName, 
			XalanNode*						context,
			const XObjectArgVectorType&		argVec,
			const LocatorType*				locator)
{
	assert(m_xpathEnvSupport != 0);

	return m_xpathEnvSupport->extFunction(*this, theNamespace, functionName, context, argVec, locator);
}



XalanDocument*
XPathExecutionContextDefault::parseXML(
			const XalanDOMString&	urlString,
			const XalanDOMString&	base) const
{
	assert(m_xpathEnvSupport != 0);

	return m_xpathEnvSupport->parseXML(urlString, base);
}



MutableNodeRefList*
XPathExecutionContextDefault::borrowMutableNodeRefList()
{
	return m_nodeListCache.get();
}



bool
XPathExecutionContextDefault::returnMutableNodeRefList(MutableNodeRefList*	theList)
{
	return m_nodeListCache.release(theList);
}



MutableNodeRefList*
XPathExecutionContextDefault::createMutableNodeRefList() const
{
	return new MutableNodeRefList;
}



XalanDOMString&
XPathExecutionContextDefault::getCachedString()
{
	return m_stringCache.get();
}



bool
XPathExecutionContextDefault::releaseCachedString(XalanDOMString&	theString)
{
	return m_stringCache.release(theString);
}



void
XPathExecutionContextDefault::getNodeSetByKey(
			XalanDocument*			/* doc */,
			const XalanQName&		/* qname */,
			const XalanDOMString&	/* ref */,
			MutableNodeRefList&		/* nodelist */)
{
}



void
XPathExecutionContextDefault::getNodeSetByKey(
			XalanDocument*			/* doc */,
			const XalanDOMString&	/* name */,
			const XalanDOMString&	/* ref */,
			const LocatorType*		/* locator */,
			MutableNodeRefList&		/* nodelist */)
{
}



const XObjectPtr
XPathExecutionContextDefault::getVariable(
			const XalanQName&		name,
			const LocatorType*		/* locator */)
{
	assert(m_xobjectFactory != 0);

	return m_xobjectFactory->createUnknown(name.getLocalPart());
}



const PrefixResolver*
XPathExecutionContextDefault::getPrefixResolver() const
{
	return m_prefixResolver;
}



void
XPathExecutionContextDefault::setPrefixResolver(const PrefixResolver*	thePrefixResolver)
{
	m_prefixResolver = thePrefixResolver;
}



const XalanDOMString*
XPathExecutionContextDefault::getNamespaceForPrefix(const XalanDOMString&	prefix) const
{
	assert(m_prefixResolver != 0);

	return m_prefixResolver->getNamespaceForPrefix(prefix);
}



XalanDOMString
XPathExecutionContextDefault::findURIFromDoc(const XalanDocument*	owner) const
{
	assert(m_xpathEnvSupport != 0);

	return m_xpathEnvSupport->findURIFromDoc(owner);
}



const XalanDOMString&
XPathExecutionContextDefault::getUnparsedEntityURI(
			const XalanDOMString&	theName,
			const XalanDocument&	theDocument) const
{
	return m_domSupport->getUnparsedEntityURI(theName, theDocument);
}



bool
XPathExecutionContextDefault::shouldStripSourceNode(const XalanText&	/* node */)
{
	return false;
}



void
XPathExecutionContextDefault::error(
			const XalanDOMString&	msg,
			const XalanNode*		sourceNode,
			const LocatorType*		locator) const
{
	assert(m_xpathEnvSupport != 0);

	XalanLocator::size_type		lineNumber = XalanLocator::getUnknownValue();
	XalanLocator::size_type		columnNumber = XalanLocator::getUnknownValue();

	XalanDOMString	uri;

	if (locator != 0)
	{
		lineNumber = locator->getLineNumber();
		columnNumber = locator->getColumnNumber();

		const XalanDOMChar*		id =
			locator->getPublicId();

		if (id != 0)
		{
			uri = id;
		}
		else
		{
			id = locator->getSystemId();

			if (id != 0)
			{
				uri = id;
			}
		}
	}

	if (m_xpathEnvSupport->problem(
			XPathEnvSupport::eXPATHProcessor, 
			XPathEnvSupport::eError,
			m_prefixResolver, 
			sourceNode,
			msg,
			c_wstr(uri),
			lineNumber,
			columnNumber) == true)
	{
		throw XalanXPathException(msg, uri, lineNumber, columnNumber);
	}
}



void
XPathExecutionContextDefault::error(
			const char*			msg,
			const XalanNode* 	sourceNode,
			const LocatorType* 	locator) const
{
	error(TranscodeFromLocalCodePage(msg), sourceNode, locator);
}



void
XPathExecutionContextDefault::warn(
			const XalanDOMString&	msg,
			const XalanNode*		sourceNode,
			const LocatorType* 		locator) const
{
	assert(m_xpathEnvSupport != 0);

	XalanLocator::size_type		lineNumber = XalanLocator::getUnknownValue();
	XalanLocator::size_type		columnNumber = XalanLocator::getUnknownValue();

	XalanDOMString	uri;

	if (locator != 0)
	{
		lineNumber = locator->getLineNumber();
		columnNumber = locator->getColumnNumber();

		const XalanDOMChar*		id =
			locator->getPublicId();

		if (id != 0)
		{
			uri = id;
		}
		else
		{
			id = locator->getSystemId();

			if (id != 0)
			{
				uri = id;
			}
		}
	}

	if (m_xpathEnvSupport->problem(
			XPathEnvSupport::eXPATHProcessor, 
			XPathEnvSupport::eWarning,
			m_prefixResolver, 
			sourceNode,
			msg,
			c_wstr(uri),
			lineNumber,
			columnNumber) == true)
	{
		throw XalanXPathException(msg, uri, lineNumber, columnNumber);
	}
}



void
XPathExecutionContextDefault::warn(
			const char*			msg,
			const XalanNode*	sourceNode,
			const LocatorType* 	locator) const
{
	warn(TranscodeFromLocalCodePage(msg), sourceNode, locator);
}



void
XPathExecutionContextDefault::message(
			const XalanDOMString&	msg,
			const XalanNode*		sourceNode,
			const LocatorType* 		locator) const
{
	assert(m_xpathEnvSupport != 0);

	XalanLocator::size_type		lineNumber = XalanLocator::getUnknownValue();
	XalanLocator::size_type		columnNumber = XalanLocator::getUnknownValue();

	XalanDOMString	uri;

	if (locator != 0)
	{
		lineNumber = locator->getLineNumber();
		columnNumber = locator->getColumnNumber();

		const XalanDOMChar*		id =
			locator->getPublicId();

		if (id != 0)
		{
			uri = id;
		}
		else
		{
			id = locator->getSystemId();

			if (id != 0)
			{
				uri = id;
			}
		}
	}

	if (m_xpathEnvSupport->problem(
			XPathEnvSupport::eXPATHProcessor, 
			XPathEnvSupport::eMessage,
			m_prefixResolver, 
			sourceNode,
			msg,
			c_wstr(uri),
			lineNumber,
			columnNumber) == true)
	{
		throw XalanXPathException(msg, uri, lineNumber, columnNumber);
	}
}



void
XPathExecutionContextDefault::message(
			const char*			msg,
			const XalanNode*	sourceNode,
			const LocatorType* 	locator) const
{
	message(TranscodeFromLocalCodePage(msg), sourceNode, locator);
}



XalanDocument*
XPathExecutionContextDefault::getSourceDocument(const XalanDOMString&	theURI) const
{
	assert(m_xpathEnvSupport != 0);

	return m_xpathEnvSupport->getSourceDocument(theURI);
}



void
XPathExecutionContextDefault::setSourceDocument(
			const XalanDOMString&	theURI,
			XalanDocument*			theDocument)
{
	assert(m_xpathEnvSupport != 0);

	m_xpathEnvSupport->setSourceDocument(theURI, theDocument);
}



void XPathExecutionContextDefault::formatNumber(
		double								number,
		const XalanDOMString&				pattern,
		XalanDOMString&						theResult,
		const XalanNode*					context,
		const LocatorType*					locator) 
{
	doFormatNumber(number, pattern, 0, theResult, context, locator);
}



void XPathExecutionContextDefault::formatNumber(
			double								number,
			const XalanDOMString&				pattern,
			const XalanDOMString&				/* dfsName */,
			XalanDOMString&						theResult,
			const XalanNode*					context,
			const LocatorType*					locator) 
{
	doFormatNumber(number, pattern, 0, theResult, context, locator);
}


	
void XPathExecutionContextDefault::doFormatNumber(
			double								number,
			const XalanDOMString&				/* pattern */,
			const XalanDecimalFormatSymbols*	theDFS,
			XalanDOMString&						theResult,
			const XalanNode*					context,
			const LocatorType*					locator) 
{
	if (DoubleSupport::isNaN(number) == true)
	{
		if (theDFS != 0)
		{
			theResult = theDFS->getNaN();
		}
		else
		{
			DoubleToDOMString(number, theResult);
		}
	}
	else if (DoubleSupport::isNegativeInfinity(number) == true)
	{
		if (theDFS != 0)
		{
			theResult = theDFS->getMinusSign();
			theResult += theDFS->getInfinity();
		}
		else
		{
			DoubleToDOMString(number, theResult);
		}
	}
	else if (DoubleSupport::isPositiveInfinity(number) == true )
	{
		if (theDFS != 0)
		{
			theResult = theDFS->getInfinity();
		}
		else
		{
			DoubleToDOMString(number, theResult);
		}
	}
	else
	{
	    warn( 
			XalanMessageLoader::getMessage(XalanMessages::FunctionIsNotImplemented_1Param,"format-number()"),
			context, 
			locator);

        DoubleToDOMString(number,theResult);
	}
}



XALAN_CPP_NAMESPACE_END
