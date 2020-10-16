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
#if !defined(XALANFILEOUTPUTSTREAM_HEADER_GUARD_1357924680)
#define XALANFILEOUTPUTSTREAM_HEADER_GUARD_1357924680



// Base include file.  Must be first.
#include <PlatformSupportDefinitions.hpp>



#if defined(WIN32)
#include <windows.h>
#else
#include <cstdio>
#ifdef SPEC_CPU
using std::FILE;
using std::fopen;
using std::fclose;
using std::fflush;
using std::fwrite;
#endif
#endif


#include <vector>



// Base class header file.
#include <XalanOutputStream.hpp>



XALAN_CPP_NAMESPACE_BEGIN



class XALAN_PLATFORMSUPPORT_EXPORT XalanFileOutputStream : public XalanOutputStream
{
public :

	enum { eDefaultBufferSize = 8192 };

#if defined(WIN32)
	typedef HANDLE	HandleType;
#else
	typedef FILE*	HandleType;
#endif

	/**
	 * Construct an XalanFileOutputStream object.
	 * 
	 * @param theFileName name of file
	 * @param theBufferSize The size of the transcoding buffer
	 */
    XalanFileOutputStream(
			const XalanDOMString&	theFileName,
			unsigned int			theBufferSize = eDefaultBufferSize);

    virtual
	~XalanFileOutputStream();


	class XALAN_PLATFORMSUPPORT_EXPORT XalanFileOutputStreamOpenException : public XalanOutputStream::XalanOutputStreamException
	{
	public:

		/**
		 * Construct an XalanFileOutputStreamOpen exception object for an exception
		 * that occurred on opening a text file stream.
		 * 
		 * @param theFileName  name of file causing the exception
		 * @param theErrorCode number of error encountered
		 */
		XalanFileOutputStreamOpenException(
			const XalanDOMString&	theFileName,
			int						theErrorCode);

		virtual
		~XalanFileOutputStreamOpenException();
	};

	class XALAN_PLATFORMSUPPORT_EXPORT XalanFileOutputStreamWriteException : public XalanOutputStream::XalanOutputStreamException
	{
	public:

		/**
		 * Construct an XalanFileOutputStreamOpen exception object for an exception
		 * that occurred while writing to a text file stream.
		 * 
		 * @param theFileName  name of file causing the exception
		 * @param theErrorCode number of error encountered
		 */
		XalanFileOutputStreamWriteException(
			const XalanDOMString&	theFileName,
			int						theErrorCode);

		virtual
		~XalanFileOutputStreamWriteException();
	};

protected:

	virtual void
	writeData(
			const char*		theBuffer,
			size_type		theBufferLength);

	virtual void
	doFlush();

private:

    // These are not implemented...
    XalanFileOutputStream(const XalanFileOutputStream&);

    XalanFileOutputStream&
	operator=(const XalanFileOutputStream&);

    bool
	operator==(const XalanFileOutputStream&) const;


	// Data members...
	const XalanDOMString	m_fileName;

	const HandleType		m_handle;
};



XALAN_CPP_NAMESPACE_END



#endif	// XALANFILEOUTPUTSTREAM_HEADER_GUARD_1357924680
