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
#if !defined(URISUPPORT_HEADER_GUARD_1357924680)
#define URISUPPORT_HEADER_GUARD_1357924680



// Base include file.  Must be first.
#include <PlatformSupportDefinitions.hpp>



#include <xercesc/util/XMLURL.hpp>



#include <XalanDOMString.hpp>



#include <XalanAutoPtr.hpp>



#include <XSLException.hpp>



XALAN_CPP_NAMESPACE_BEGIN



typedef XERCES_CPP_NAMESPACE_QUALIFIER XMLURL	XMLURLType;



class XALAN_PLATFORMSUPPORT_EXPORT URISupport
{
public:

	typedef XalanAutoPtr<XMLURLType>	URLAutoPtrType;

	/**
	 * Determine the fully qualified URI for a string.
	 *
	 * @param urlString string to qualify
	 * @return auto pointer to fully qualified URI
	 */
	static URLAutoPtrType
	getURLFromString(const XalanDOMString&	urlString)
	{
		return getURLFromString(urlString.c_str());
	}

	/**
	 * Determine the fully qualified URI for a string.
	 *
	 * @param urlString string to qualify
	 * @param url to update with the qualified string.
	 */
	static void
	getURLFromString(
			const XalanDOMString&	urlString,
			XMLURLType&				url)
	{
		getURLFromString(urlString.c_str(), url);
	}

	/**
	 * Determine the fully qualified URI for a string.
	 *
	 * @param urlString string to qualify
	 * @return auto pointer to fully qualified URI
	 */
	static URLAutoPtrType
	getURLFromString(const XalanDOMChar*	urlString);

	/**
	 * Determine the fully qualified URI for a string.
	 *
	 * @param urlString string to qualify
	 * @param url to update with the qualified string.
	 */
	static void
	getURLFromString(
			const XalanDOMChar*		urlString,
			XMLURLType&				url)
	{
		url.setURL(getURLStringFromString(urlString).c_str());
	}

	/**
	 * Determine the fully qualified URI for a string.
	 *
	 * @param urlString string to qualify
	 * @param base base location for URI
	 * @return auto pointer to fully qualified URI
	 */
	static URLAutoPtrType
	getURLFromString(
			const XalanDOMString&	urlString,
			const XalanDOMString&	base)
	{
		return getURLFromString(getURLStringFromString(urlString, base));
	}

	/**
	 * Determine the fully qualified URI for a string.
	 *
	 * @param urlString string to qualify
	 * @param base base location for URI
	 * @return auto pointer to fully qualified URI
	 */
	static URLAutoPtrType
	getURLFromString(
			const XalanDOMChar*		urlString,
			const XalanDOMChar*		base);

	/**
	 * Determine the fully qualified URI for a string.
	 *
	 * @param urlString string to qualify
	 * @return string to fully qualified URI
	 */
	static XalanDOMString
	getURLStringFromString(const XalanDOMString&	urlString)
	{
		XalanDOMString	result;

		getURLStringFromString(urlString.c_str(), urlString.length(), result);

		return result;
	}

	/**
	 * Determine the fully qualified URI for a string.
	 *
	 * @param urlString string to qualify
	 * @return string to fully qualified URI
	 */
	static void
	getURLStringFromString(
			const XalanDOMString&	urlString,
			XalanDOMString&			theNormalizedURI)
	{
		getURLStringFromString(urlString.c_str(), urlString.length(), theNormalizedURI);
	}

	/**
	 * Determine the fully qualified URI for a string.
	 *
	 * @param urlString string to qualify
	 * @return string to fully qualified URI
	 */
	static XalanDOMString
	getURLStringFromString(const XalanDOMChar*	urlString)
	{
		XalanDOMString	theNormalizedURI;

		getURLStringFromString(urlString, theNormalizedURI);

		return theNormalizedURI;
	}

	/**
	 * Determine the fully qualified URI for a string.
	 *
	 * @param urlString string to qualify
	 * @param theNormalizedURI fully qualified URI
	 */
	static void
	getURLStringFromString(
			const XalanDOMChar*			urlString,
			XalanDOMString&				theNormalizedURI)
	{
		assert(urlString != 0);

		getURLStringFromString(
			urlString,
			XalanDOMString::length(urlString),
			theNormalizedURI);
	}

	/**
	 * Determine the fully qualified URI for a string.
	 *
	 * @param urlString string to qualify
	 * @param len the length of urlString
	 * @param theNormalizedURI fully qualified URI
	 */
	static void
	getURLStringFromString(
			const XalanDOMChar*			urlString,
			XalanDOMString::size_type	len,
			XalanDOMString&				theNormalizedURI);

	/**
	 * Determine the fully qualified URI for a string.
	 *
	 * @param urlString string to qualify
	 * @param base base location for URI
	 * @return string to fully qualified URI
	 */
	static XalanDOMString
	getURLStringFromString(
			const XalanDOMString&	urlString,
			const XalanDOMString&	base)
	{
		XalanDOMString	theNormalizedURI;

		getURLStringFromString(
			urlString.c_str(),
			urlString.length(),
			base.c_str(),
			base.length(),
			theNormalizedURI);

		return theNormalizedURI;
	}

	/**
	 * Determine the fully qualified URI for a string.
	 *
	 * @param urlString string to qualify
	 * @param base base location for URI
	 * @param theNormalizedURI fully qualified URI
	 */
	static void
	getURLStringFromString(
			const XalanDOMString&	urlString,
			const XalanDOMString&	base,
			XalanDOMString&			theNormalizedURI)
	{
		getURLStringFromString(urlString.c_str(), base.c_str(), theNormalizedURI);
	}

	/**
	 * Determine the fully qualified URI for a string.
	 *
	 * @param urlString string to qualify
	 * @param base base location for URI
	 * @return string to fully qualified URI
	 */
	static XalanDOMString
	getURLStringFromString(
			const XalanDOMChar*		urlString,
			const XalanDOMChar*		base)
	{
		XalanDOMString	theNormalizedURI;

		getURLStringFromString(urlString, base, theNormalizedURI);

		return theNormalizedURI;
	}

	/**
	 * Determine the fully qualified URI for a string.
	 *
	 * @param urlString string to qualify
	 * @param base base location for URI
	 * @param theNormalizedURI fully qualified URI
	 */
	static void
	getURLStringFromString(
			const XalanDOMChar*		urlString,
			const XalanDOMChar*		base,
			XalanDOMString&			theNormalizedURI)
	{
		assert(urlString != 0 && base != 0);

		getURLStringFromString(
			urlString,
			XalanDOMString::length(urlString),
			base,
			XalanDOMString::length(base),
			theNormalizedURI);
	}

	/**
	 * Determine the fully qualified URI for a string.
	 *
	 * @param urlString string to qualify
	 * @param base base location for URI
	 * @param theNormalizedURI fully qualified URI
	 */
	static void
	getURLStringFromString(
			const XalanDOMChar*			urlString,
			XalanDOMString::size_type	urlStringLen,
			const XalanDOMChar*			base,
			XalanDOMString::size_type	baseLen,
			XalanDOMString&				theNormalizedURI);

	/**
	 * Normalizes the string passed in, replacing
	 * \ with /.
	 *
	 * @param urlString string to normalize
	 * @return a reference to the passed parameter
	 */
	static XalanDOMString&
	NormalizeURIText(XalanDOMString&	uriString);

	/**
	 * Normalizes the string passed in, replacing
	 * \ with /.
	 *
	 * @param urlString string to normalize
	 * @return a copy of the normalized URI
	 */
	static const XalanDOMString
	NormalizeURIText(const XalanDOMString&	uriString);


	class InvalidURIException : public XSLException
	{
	public:

		/**
		 * Construct an InvalidURIException.
		 *
		 * @param theMessage the error message
		 */
		InvalidURIException(const XalanDOMString&	theMessage);

		virtual
		~InvalidURIException();
	};


	static const XalanDOMChar	s_fileProtocolString1[];

	static const XalanDOMChar	s_fileProtocolString2[];
};



XALAN_CPP_NAMESPACE_END



#endif	// URISUPPORT_HEADER_GUARD_1357924680
