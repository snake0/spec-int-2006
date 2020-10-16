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
#if !defined(FUNCTIONID_HEADER_GUARD_1357924680)
#define FUNCTIONID_HEADER_GUARD_1357924680



// Base header file.  Must be first.
#include <XPathDefinitions.hpp>



#include <set>



#include <XalanElement.hpp>
#include <XalanNode.hpp>
#include <XalanDocument.hpp>



// Base class header files...
#include <Function.hpp>
#include <XObjectTypeCallback.hpp>



#include <DOMStringHelper.hpp>
#include <StringTokenizer.hpp>



#include <MutableNodeRefList.hpp>
#include <NodeRefListBase.hpp>



XALAN_CPP_NAMESPACE_BEGIN



/**
 * XPath implementation of "id" function.
 */
class XALAN_XPATH_EXPORT FunctionID : public Function
{
public:

	typedef Function	ParentType;

	FunctionID();

	virtual
	~FunctionID();

	// These methods are inherited from Function ...

	virtual XObjectPtr
	execute(
			XPathExecutionContext&	executionContext,
			XalanNode*				context,
			const XObjectPtr		arg1,
			const LocatorType*		locator) const;

#if !defined(XALAN_NO_USING_DECLARATION)
	using ParentType::execute;
#endif

#if defined(XALAN_NO_COVARIANT_RETURN_TYPE)
	virtual Function*
#else
	virtual FunctionID*
#endif
	clone() const;

protected:

	const XalanDOMString
	getError() const;

private:

	class FunctionIDXObjectTypeCallback : public XObjectTypeCallback
	{
	public:

		FunctionIDXObjectTypeCallback(
				XPathExecutionContext&	theExecutionContext,
				XalanDOMString&			theString);

		void
		processCallback(const XObject&	theXObject);

		// These methods are inherited from XObjectTypeCallback ...

		virtual void
		Number(
			const XObject&	theXObject,
			double			/* theValue */);

		virtual void
		Boolean(
			const XObject&	theXObject,
			bool			/* theValue */);

		virtual void
		String(
			const XObject&			theXObject,
			const XalanDOMString&	/* theValue */);

		virtual void
		ResultTreeFragment(
			const XObject&					theXObject,
			const XalanDocumentFragment&	/* theValue */);

		virtual void
		ResultTreeFragment(
			const XObject&			theXObject,
			XalanDocumentFragment&	/* theValue */);

		virtual void
		NodeSet(
			const XObject&			/* theXObject */,
			const NodeRefListBase&	theValue);

		virtual void
		Unknown(
			const XObject&			/* theObject */,
			const XalanDOMString&	/* theName */);

		virtual void
		Null(const XObject&		/* theObject */);

	private:

		XalanDOMString&			m_resultString;

		XPathExecutionContext&	m_executionContext;
	};

	// Not implemented...
	FunctionID&
	operator=(const FunctionID&);

	bool
	operator==(const FunctionID&) const;
};



XALAN_CPP_NAMESPACE_END



#endif	// FUNCTIONID_HEADER_GUARD_1357924680
