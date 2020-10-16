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
#if !defined(XUKNOWN_HEADER_GUARD_1357924680)
#define XUKNOWN_HEADER_GUARD_1357924680



// Base header file.  Must be first.
#include <XPathDefinitions.hpp>



#include <XalanDOMString.hpp>



// Base class header file.
#include <XObject.hpp>



XALAN_CPP_NAMESPACE_BEGIN



class XALAN_XPATH_EXPORT XUnknown : public XObject
{
public:

	typedef XObject		ParentType;

	/**
	 * Perform static initialization.  See class XPathInit.
	 */
	static void
	initialize();

	/**
	 * Perform static shut down.  See class XPathInit.
	 */
	static void
	terminate();

	/**
	 * Construct an XUnknown object from a string.
	 * 
	 * @param name       source string
	 */
	XUnknown(const XalanDOMString&	name);

	XUnknown(const XUnknown&	source);

	virtual
	~XUnknown();

	// These methods are inherited from XObject ...

#if defined(XALAN_NO_COVARIANT_RETURN_TYPE)
	virtual XObject*
#else
	virtual XUnknown*
#endif
	clone(void*		theAddress = 0) const;

	virtual XalanDOMString
	getTypeString() const;
  
	virtual double
	num() const;

	virtual bool
	boolean() const;

	virtual const XalanDOMString&
	str() const;

	virtual void
	str(
			FormatterListener&	formatterListener,
			MemberFunctionPtr	function) const;

#if !defined(XALAN_NO_USING_DECLARATION)
	using ParentType::str;
#endif

	virtual double
	stringLength() const;

	virtual void
	ProcessXObjectTypeCallback(XObjectTypeCallback&		theCallbackObject);

	virtual void
	ProcessXObjectTypeCallback(XObjectTypeCallback&		theCallbackObject) const;

private:

	const XalanDOMString	m_value;

	static XalanDOMString	s_unknownString;
};



XALAN_CPP_NAMESPACE_END



#endif	// XUKNOWN_HEADER_GUARD_1357924680
