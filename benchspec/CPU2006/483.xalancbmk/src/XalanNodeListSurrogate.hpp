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
#if !defined(XALANNODELISTSURROGATE_HEADER_GUARD_1357924680)
#define XALANNODELISTSURROGATE_HEADER_GUARD_1357924680



#include <XalanDOMDefinitions.hpp>



#include <XalanNodeList.hpp>



XALAN_CPP_NAMESPACE_BEGIN



class XalanNode;



/*
 * <meta name="usage" content="experimental"/>
 *
 * Helper class for implementing the DOM NodeList interface.
 *
 * This class is experimental and subject to change!!
 */

class XALAN_DOM_EXPORT XalanNodeListSurrogate : public XalanNodeList
{
public:

	XalanNodeListSurrogate(const XalanNode&		theNode);

	XalanNodeListSurrogate(const XalanNodeListSurrogate&	theSource);

	virtual
	~XalanNodeListSurrogate();

	XalanNodeListSurrogate&
	operator=(const XalanNodeListSurrogate&	theSource)
	{
		m_node = theSource.m_node;

		XalanNodeList::operator=(theSource);

		return *this;
	}

	bool
	operator==(const XalanNodeListSurrogate& 	theRHS) const
	{
		return m_node == theRHS.m_node ? true : false;
	}

	virtual XalanNode*
	item(unsigned int	index) const;

	virtual unsigned int
	getLength() const;

private:

	const XalanNode*	m_node;
};



XALAN_CPP_NAMESPACE_END



#endif	// !defined(XALANNODELISTSURROGATE_HEADER_GUARD_1357924680)
