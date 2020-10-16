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
#if !defined(NAMEDNODEMAPATTRIBUTELIST_HEADER_GUARD_1357924680)
#define NAMEDNODEMAPATTRIBUTELIST_HEADER_GUARD_1357924680



// Base include file.  Must be first.
#include <PlatformSupportDefinitions.hpp>



#include <vector>



#include <xercesc/sax/AttributeList.hpp>



XALAN_CPP_NAMESPACE_BEGIN



class XalanNamedNodeMap;



class XALAN_PLATFORMSUPPORT_EXPORT NamedNodeMapAttributeList : public XERCES_CPP_NAMESPACE_QUALIFIER AttributeList
{
public:

	typedef XERCES_CPP_NAMESPACE_QUALIFIER AttributeList	ParentType;

	explicit
	NamedNodeMapAttributeList(const XalanNamedNodeMap&	theMap);

	virtual
	~NamedNodeMapAttributeList();

	// These are inherited from AttributeList
    virtual unsigned int
	getLength() const;

    virtual const XalanDOMChar*
	getName(const unsigned int index) const;

    virtual const XalanDOMChar*
	getType(const unsigned int index) const;

    virtual const XalanDOMChar*
	getValue(const unsigned int index) const;

    virtual const XalanDOMChar*
	getType(const XalanDOMChar* const name) const;

    virtual const XalanDOMChar*
	getValue(const XalanDOMChar* const name) const;

	virtual const XalanDOMChar* 
	getValue(const char* const name) const;

private:

	// Not implemented...
	NamedNodeMapAttributeList&
	operator=(const NamedNodeMapAttributeList&);

	bool
	operator==(const NamedNodeMapAttributeList&);

	// Data members...
	const XalanNamedNodeMap&	m_nodeMap;

	const unsigned int			m_lastIndex;

	static const XalanDOMChar	s_typeString[];
};



XALAN_CPP_NAMESPACE_END



#endif	// NAMEDNODEMAPATTRIBUTELIST_HEADER_GUARD_1357924680
