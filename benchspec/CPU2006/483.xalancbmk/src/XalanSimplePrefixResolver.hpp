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
#if !defined(XALANSIMPLEPREFIXRESOLVER_HEADER_GUARD_1357924680)
#define XALANSIMPLEPREFIXRESOLVER_HEADER_GUARD_1357924680



// Base include file.  Must be first.
#include <PlatformSupportDefinitions.hpp>



#include <PrefixResolver.hpp>



XALAN_CPP_NAMESPACE_BEGIN



class XALAN_PLATFORMSUPPORT_EXPORT XalanSimplePrefixResolver : public PrefixResolver
{
public:

	XalanSimplePrefixResolver(
			const XalanDOMString&	thePrefix,
			const XalanDOMString&	theNamespaceURI,
			const XalanDOMString&	theURI);

	virtual
	~XalanSimplePrefixResolver();


	virtual const XalanDOMString*
	getNamespaceForPrefix(const XalanDOMString&		prefix) const;

	virtual const XalanDOMString&
	getURI() const;

public:

	const XalanDOMString&	m_prefix;

	const XalanDOMString&	m_namespaceURI;

	const XalanDOMString&	m_uri;
};



XALAN_CPP_NAMESPACE_END



#endif	// XALANSIMPLEPREFIXRESOLVER_HEADER_GUARD_1357924680
