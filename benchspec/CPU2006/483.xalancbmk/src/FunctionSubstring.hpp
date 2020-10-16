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
#if !defined(FUNCTIONSUBSTRING_HEADER_GUARD_1357924680)
#define FUNCTIONSUBSTRING_HEADER_GUARD_1357924680



// Base header file.  Must be first.
#include <XPathDefinitions.hpp>



#include <cfloat>



// Base class header file...
#include <Function.hpp>



#include <DoubleSupport.hpp>



XALAN_CPP_NAMESPACE_BEGIN



/**
 * XPath implementation of "substring" function.
 */
class XALAN_XPATH_EXPORT FunctionSubstring : public Function
{
public:

	typedef Function	ParentType;

	FunctionSubstring();

	virtual
	~FunctionSubstring();

	// These methods are inherited from Function ...

#if !defined(XALAN_NO_USING_DECLARATION)
	using ParentType::execute;
#endif

	virtual XObjectPtr
	execute(
			XPathExecutionContext&	executionContext,
			XalanNode*				context,			
			const XObjectPtr		arg1,
			const XObjectPtr		arg2,
			const LocatorType*		locator) const;

	virtual XObjectPtr
	execute(
			XPathExecutionContext&	executionContext,
			XalanNode*				context,			
			const XObjectPtr		arg1,
			const XObjectPtr		arg2,
			const XObjectPtr		arg3,
			const LocatorType*		locator) const;

#if defined(XALAN_NO_COVARIANT_RETURN_TYPE)
	virtual Function*
#else
	virtual FunctionSubstring*
#endif
	clone() const;

protected:

	const XalanDOMString
	getError() const;

private:

	// Not implemented...
	FunctionSubstring&
	operator=(const FunctionSubstring&);

	bool
	operator==(const FunctionSubstring&) const;

	static const XObjectPtr			s_nullXObjectPtr;
};



XALAN_CPP_NAMESPACE_END



#endif	// FUNCTIONSUBSTRING_HEADER_GUARD_1357924680
