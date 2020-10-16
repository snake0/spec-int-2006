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

#if !defined(XALANDOMSTRINGALLOCATOR_INCLUDE_GUARD_12455133)
#define XALANDOMSTRINGALLOCATOR_INCLUDE_GUARD_12455133



// Base include file.  Must be first.
#include <PlatformSupportDefinitions.hpp>



#include <ArenaAllocator.hpp>



#include <XalanDOMString.hpp>



XALAN_CPP_NAMESPACE_BEGIN



class XALAN_PLATFORMSUPPORT_EXPORT XalanDOMStringAllocator
{
public:

	typedef XalanDOMString					data_type;
	typedef data_type::size_type			data_type_size_type;

#if defined(XALAN_NO_DEFAULT_TEMPLATE_ARGUMENTS)
	typedef ArenaBlock<data_type>			ArenaBlockType;

	typedef ArenaAllocator<data_type,
						   ArenaBlockType>	ArenaAllocatorType;
#else
	typedef ArenaAllocator<data_type>		ArenaAllocatorType;
#endif

	typedef ArenaAllocatorType::size_type	size_type;

	enum { eDefaultBlockSize = 32 };


	/**
	 * Construct an instance that will allocate in blocks of the specified size.
	 *
	 * @param theBlockSize The block size.
	 */
	XalanDOMStringAllocator(size_type	theBlockCount);

	~XalanDOMStringAllocator();

	/**
	 * Create a XalanDOMString object.
	 * 
	 * @param theString A pointer to a character string
	 * @param theCount The number of characters in the string, or npos if the string is null-terminated.
	 *
	 * @return pointer to the new instance
	 */
	data_type*
	create(
			const char*				theString,
#if defined(_MSC_VER)
			// $$$ ToDo: Some strange bug in MSVC++ complains when using data_type::npos here.
			data_type_size_type		theCount = data_type_size_type(-1));
#else
	data_type_size_type		theCount = data_type_size_type(data_type::npos));
#endif

	/**
	 * Copy constructor
	 * 
	 * @param theSource The source string for the copy
	 * @param theStartPosition The position to start in the source string.
	 * @param theCount The number of characters to copy from the source string.
	 *
	 * @return pointer to the new instance
	 */
	data_type*
	create(
			const data_type&		theSource,
			data_type_size_type		theStartPosition = 0,
			data_type_size_type		theCount = data_type_size_type(data_type::npos));

	/**
	 * Create a XalanDOMString object.
	 * 
	 * @param theString A pointer to a wide character string
	 * @param theCount The number of characters in the string, or npos if the string is null-terminated.
	 *
	 * @return pointer to the new instance
	 */
	data_type*
	create(
			const XalanDOMChar*		theString,
			data_type_size_type		theCount = data_type_size_type(data_type::npos));

	/**
	 * Create a XalanDOMString object.
	 *
	 * @param theCount the size of the string
	 * @param theChar the character used to initialize the string
	 *
	 * @return pointer to the new instance
	 */
	data_type*
	create(
			data_type_size_type		theCount,
			XalanDOMChar			theChar);

	/**
	 * Determine if an object is owned by the allocator...
	 */
	bool
	ownsObject(const data_type*		theObject)
	{
		return m_allocator.ownsObject(theObject);
	}

	/**
	 * Delete all instance objects from allocator.	 
	 */	
	void
	reset()
	{
		m_allocator.reset();
	}

	/**
	 * Get the number of ArenaBlocks currently allocated.
	 *
	 * @return The number of blocks.
	 */
	size_type
	getBlockCount() const
	{
		return m_allocator.getBlockCount();
	}

	/**
	 * Get size of an ArenaBlock, that is, the number
	 * of objects in each block.
	 *
	 * @return The size of the block
	 */
	size_type
	getBlockSize() const
	{
		return m_allocator.getBlockSize();
	}

private:

	// Not implemented...
	XalanDOMStringAllocator(const XalanDOMStringAllocator&);

	XalanDOMStringAllocator&
	operator=(const XalanDOMStringAllocator&);

	// Data members...
	ArenaAllocatorType	m_allocator;
};



XALAN_CPP_NAMESPACE_END



#endif	// XALANDOMSTRINGALLOCATOR_INCLUDE_GUARD_12455133
