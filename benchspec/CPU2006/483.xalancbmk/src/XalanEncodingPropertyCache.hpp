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
#if !defined(XALANENCODINGPROPERTYCACHE_HEADER_GUARD_1357924680)
#define XALANENCODINGPROPERTYCACHE_HEADER_GUARD_1357924680



#include <PlatformSupportDefinitions.hpp>



#include <XalanDOMString.hpp>



#include <XalanBitmap.hpp>



XALAN_CPP_NAMESPACE_BEGIN



class XalanOutputTranscoder;




class XALAN_PLATFORMSUPPORT_EXPORT XalanEncodingPropertyCache
{
public:

	XalanEncodingPropertyCache(
			size_t								theCacheSize = XalanDOMChar(~0),
			const XalanOutputTranscoder*		theTranscoder = 0);

	~XalanEncodingPropertyCache();

	bool
	canEncodeCharacter(unsigned int		theChar) const;

	const XalanOutputTranscoder*
	getTranscoder() const
	{
		return m_transcoder;
	}

	void
	setTranscoder(const XalanOutputTranscoder*	theTranscoder)
	{
		m_transcoder = theTranscoder;
	}
private:

	const XalanOutputTranscoder*	m_transcoder;

	mutable XalanBitmap				m_presentBitmap;

	mutable XalanBitmap				m_valueBitmap;
};



XALAN_CPP_NAMESPACE_END



#endif	// XALANENCODINGPROPERTYCACHE_HEADER_GUARD_1357924680
