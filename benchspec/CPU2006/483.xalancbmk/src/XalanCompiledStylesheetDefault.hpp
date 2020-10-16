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
#if !defined(XALANCOMPILEDSTYLESHEETDEFAULT_HEADER_GUARD)
#define XALANCOMPILEDSTYLESHEETDEFAULT_HEADER_GUARD



// Base include file.  Must be first.
#include <XalanTransformerDefinitions.hpp>



#include <XPathFactoryBlock.hpp>



#include <StylesheetConstructionContextDefault.hpp>
#include <StylesheetExecutionContextDefault.hpp>
#include <StylesheetRoot.hpp>
#include <XSLTEngineImpl.hpp>
#include <XSLTInputSource.hpp>
#include <XSLTProcessorEnvSupportDefault.hpp>



#include <XalanCompiledStylesheet.hpp>



XALAN_DECLARE_XERCES_CLASS(EntityResolver)
XALAN_DECLARE_XERCES_CLASS(ErrorHandler)



XALAN_CPP_NAMESPACE_BEGIN



typedef XERCES_CPP_NAMESPACE_QUALIFIER EntityResolver	EntityResolverType;
typedef XERCES_CPP_NAMESPACE_QUALIFIER ErrorHandler		ErrorHandlerType;



class XALAN_TRANSFORMER_EXPORT XalanCompiledStylesheetDefault : public XalanCompiledStylesheet
{
public:
	
	XalanCompiledStylesheetDefault(
			const XSLTInputSource&	theStylesheetSource,
			XSLTEngineImpl&			theProcessor,
			ErrorHandlerType*		theErrorHandler = 0,
			EntityResolverType*		theEntityResolver = 0);

	virtual
	~XalanCompiledStylesheetDefault();

	virtual const StylesheetRoot*
	getStylesheetRoot() const;

private:

	XPathFactoryBlock						m_stylesheetXPathFactory;

	StylesheetConstructionContextDefault	m_stylesheetConstructionContext;

	const StylesheetRoot* const				m_stylesheetRoot;
};



XALAN_CPP_NAMESPACE_END



#endif	// XALANCOMPILEDSTYLESHEETDEFAULT_HEADER_GUARD
