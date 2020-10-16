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
#if !defined(XRESULTTREEFRAG_HEADER_GUARD_1357924680)
#define XRESULTTREEFRAG_HEADER_GUARD_1357924680



// Base header file.  Must be first.
#include <XSLTDefinitions.hpp>



#include <XalanDOMString.hpp>



#if defined(XALAN_AUTO_PTR_REQUIRES_DEFINITION)
#include <ResultTreeFragBase.hpp>
#endif



#include <XalanAutoPtr.hpp>



// Base class header file.
#include <NodeRefListBase.hpp>
#include <XObject.hpp>



#include <StylesheetExecutionContext.hpp>



XALAN_CPP_NAMESPACE_BEGIN



class ResultTreeFrag;



class XALAN_XSLT_EXPORT XResultTreeFrag : public XObject
{
public:

	typedef XObject		ParentType;


	/**
	 * Construct an XResultTreeFrag object from a result tree fragment
	 * 
	 * @param val source result tree fragment.
	 */
	XResultTreeFrag(XalanDocumentFragment&		value);

	/**
	 * Construct an XResultTreeFrag object from another
	 * 
	 * @param source     source XResultTreeFrag
	 * @param deepClone  true to copy all subobjects, default is false
	 */
	XResultTreeFrag(
			const XResultTreeFrag&	source,
			bool					deepClone = false);

	virtual
	~XResultTreeFrag();


	void
	setExecutionContext(StylesheetExecutionContext*		theExecutionContext)
	{
		m_executionContext = theExecutionContext;
	}

	// These methods are inherited from XObject ...

#if defined(XALAN_NO_COVARIANT_RETURN_TYPE)
	virtual XObject*
#else
	virtual XResultTreeFrag*
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

	virtual void
	str(XalanDOMString&	theBuffer) const;

	virtual double
	stringLength() const;

	virtual const XalanDocumentFragment&
	rtree() const;

	virtual const NodeRefListBase&
	nodeset() const;

	virtual void
	ProcessXObjectTypeCallback(XObjectTypeCallback&		theCallbackObject);

	virtual void
	ProcessXObjectTypeCallback(XObjectTypeCallback&		theCallbackObject) const;

	/**
	 * Release the ResultTreeFrag held by the instance.
	 */
	XalanDocumentFragment*
	release();

	/**
	 * Change the value of an XResultTreeFrag
	 *
	 * @param theValue The new value.
	 */
	void
	set(XalanDocumentFragment&	theValue);

protected:

	virtual void 
	dereferenced();

private:

	// Data members...
	XalanDocumentFragment*			m_value;	

	const XalanDOMString*			m_singleTextChildValue;

	StylesheetExecutionContext*		m_executionContext;

	mutable XalanDOMString			m_cachedStringValue;

	mutable double					m_cachedNumberValue;
};



XALAN_CPP_NAMESPACE_END



#endif	// XRESULTTREEFRAG_HEADER_GUARD_1357924680
