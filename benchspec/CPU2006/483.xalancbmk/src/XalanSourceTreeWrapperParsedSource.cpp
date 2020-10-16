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
#include "XalanSourceTreeWrapperParsedSource.hpp"



#include <URISupport.hpp>



#include <XalanSourceTreeParserLiaison.hpp>
#include <XalanSourceTreeDOMSupport.hpp>
#include <XalanSourceTreeDocument.hpp>



#include "XalanDefaultParsedSource.hpp"



XALAN_CPP_NAMESPACE_BEGIN



XalanSourceTreeWrapperParsedSource::XalanSourceTreeWrapperParsedSource(
			XalanSourceTreeDocument*		theDocument,
			XalanSourceTreeParserLiaison&	theParserLiaison,
			XalanSourceTreeDOMSupport&		theDOMSupport,
			const XalanDOMString&			theURI) :
	XalanParsedSource(),
	m_parserLiaison(theParserLiaison),
	m_domSupport(theDOMSupport),
	m_parsedSource(theDocument),
	m_uri(URISupport::NormalizeURIText(theURI))
{
	assert(m_parsedSource != 0);
}



XalanSourceTreeWrapperParsedSource::~XalanSourceTreeWrapperParsedSource()
{
}



XalanDocument*
XalanSourceTreeWrapperParsedSource::getDocument() const
{
	return m_parsedSource;
}



XalanParsedSourceHelper*
XalanSourceTreeWrapperParsedSource::createHelper() const
{
	return new XalanDefaultParsedSourceHelper(m_domSupport);
}



const XalanDOMString&
XalanSourceTreeWrapperParsedSource::getURI() const
{
	return m_uri;
}



XALAN_CPP_NAMESPACE_END
