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
#if !defined(NAMESPACE_HEADER_GUARD_1357924680)
#define NAMESPACE_HEADER_GUARD_1357924680



// Base header file.  Must be first.
#include <XPathDefinitions.hpp>



#include <DOMStringHelper.hpp>



XALAN_CPP_NAMESPACE_BEGIN



/**
 * A representation of a namespace.  One of these will be pushed on the
 * namespace stack for each element.
 */
class XALAN_XPATH_EXPORT NameSpace
{
public:

	explicit
	NameSpace() :
		m_prefix(),
		m_uri()
	{
	}

	/**
	 * Construct a namespace for placement on the 
	 * result tree namespace stack.
	 *
	 * @param prefix namespace prefix
	 * @param uri    URI of namespace
	 */
	NameSpace(
			const XalanDOMString&	prefix,
			const XalanDOMString&	uri) :
		m_prefix(prefix),
		m_uri(uri)
	{
	}

	~NameSpace()
	{
	}

	/**
	 * Retrieve the prefix for namespace
	 * 
	 * @return prefix string
	 */
	const XalanDOMString&
	getPrefix() const
	{
		return m_prefix;
	}

	/**
	 * Set the prefix for namespace
	 * 
	 * @param prefix The new prefix value
	 */
	void
	setPrefix(const XalanDOMString&		prefix)
	{
		m_prefix = prefix;
	}

	/**
	 * Retrieve the URI for namespace
	 * 
	 * @return URI string
	 */
	const XalanDOMString&
	getURI() const
	{
		return m_uri;
	}

	/**
	 * Set the URI for namespace
	 * 
	 * @param uri The new uri value
	 */
	void
	setURI(const XalanDOMString&	uri)
	{
		m_uri = uri;
	}

	/**
	 * Set the URI for namespace
	 * 
	 * @param uri The new uri value
	 */
	void
	setURI(const XalanDOMChar*	uri)
	{
		assert(uri != 0);

		m_uri = uri;
	}

	/**
	 * Set the URI for namespace
	 * 
	 * @param uri The new uri value
	 * @param len The length of the uri
	 */
	void
	setURI(
			const XalanDOMChar*			uri,
			XalanDOMString::size_type	len)
	{
		assert(uri != 0);

		m_uri.assign(uri, len);
	}

	bool
	empty() const
	{
		return m_prefix.empty() && m_uri.empty();
	}

	void
	clear()
	{
		m_prefix.clear();

		m_uri.clear();
	}

	/**
	 * Equality operator
	 *
	 * @param theRHS namespace to compare
	 */
	bool
	operator==(const NameSpace&		theRHS) const
	{
		return equals(m_prefix, theRHS.m_prefix) &&
			   equals(m_uri, theRHS.m_uri);
	}	

private:

	XalanDOMString	m_prefix;

	XalanDOMString	m_uri;
};



XALAN_CPP_NAMESPACE_END



#endif	// NAMESPACE_HEADER_GUARD_1357924680
