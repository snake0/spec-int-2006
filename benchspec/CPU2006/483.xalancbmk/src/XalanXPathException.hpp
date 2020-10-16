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
#if !defined(XALANXPATHEXCEPTION_HEADER_GUARD_1357924680)
#define XALANXPATHEXCEPTION_HEADER_GUARD_1357924680



// Base header file.  Must be first.
#include <XPathDefinitions.hpp>



#include <XalanDOMString.hpp>
#include <XalanNode.hpp>



// Base class header file.
#include <XSLException.hpp>



XALAN_CPP_NAMESPACE_BEGIN



class XalanNode;



class XALAN_XPATH_EXPORT XalanXPathException : public XSLException
{
public:

	/**
	 * Construct an XPath exeption object.
	 * 
	 * @param message message explaining the problem. 
	 * @param theURI the URI of the related document, if known
	 * @param theLineNumber the line number of the related document.
	 * @param theColumnNumber the column number of the related document.
	 * @param styleNode the node in the stylesheet where the problem occurred
	 * @param theType type of exception, default is "XalanXPathException"
	 */
	XalanXPathException(
			const XalanDOMString&	message,
			const XalanDOMString&	theURI,
			int						theLineNumber,
			int						theColumnNumber,
			const XalanDOMString&	theType = XalanDOMString(XALAN_STATIC_UCODE_STRING("XalanXPathException")));

	/**
	 * Constructor
	 * 
	 * @param theLocator The locator instance for error reporting.
	 * @param theMessage message to write when exception thrown
	 * @param styleNode the node in the stylesheet where the problem occurred
	 * @param theType type of exception, default is "XalanXPathException"
	 */
	XalanXPathException(
			const LocatorType&		theLocator,
			const XalanDOMString&	theMessage,
			const XalanDOMString&	theType = XalanDOMString(XALAN_STATIC_UCODE_STRING("XalanXPathException")));

	/**
	 * Construct an XPath exeption object.
	 * 
	 * @param message message explaining the problem. 
	 * @param theType type of exception, default is "XalanXPathException"
	 */
	XalanXPathException(
			const XalanDOMString&	message,
			const XalanDOMString&	theType = XalanDOMString(XALAN_STATIC_UCODE_STRING("XalanXPathException")));

	virtual
	~XalanXPathException();

protected:

	/**
	 * Construct an XPath exeption object.
	 * 
	 */
	explicit
	XalanXPathException();

private:

	const XalanNode*	m_styleNode;

	static const XalanDOMString		s_emptyString;
};



XALAN_CPP_NAMESPACE_END



#endif	// XALANXPATHEXCEPTION_HEADER_GUARD_1357924680
