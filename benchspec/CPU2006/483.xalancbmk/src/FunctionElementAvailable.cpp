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
#include "FunctionElementAvailable.hpp"



#include <DOMStringHelper.hpp>
#include <XalanMessageLoader.hpp>



#include <XObjectFactory.hpp>



XALAN_CPP_NAMESPACE_BEGIN



FunctionElementAvailable::FunctionElementAvailable()
{
}



FunctionElementAvailable::~FunctionElementAvailable()
{
}



XObjectPtr
FunctionElementAvailable::execute(
			XPathExecutionContext&	executionContext,
			XalanNode*				/* context */,			
			const XObjectPtr		arg1,
			const LocatorType*		locator) const
{
	assert(arg1.null() == false);

	return executionContext.getXObjectFactory().createBoolean(executionContext.elementAvailable(arg1->str(), locator));
}



#if defined(XALAN_NO_COVARIANT_RETURN_TYPE)
Function*
#else
FunctionElementAvailable*
#endif
FunctionElementAvailable::clone() const
{
	return new FunctionElementAvailable(*this);
}



const XalanDOMString
FunctionElementAvailable::getError() const
{
	return XalanMessageLoader::getMessage(XalanMessages::FunctionAcceptsOneArgument_1Param,"function-available()");
}



XALAN_CPP_NAMESPACE_END
