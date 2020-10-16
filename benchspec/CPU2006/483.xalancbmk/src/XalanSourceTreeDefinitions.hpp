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
#if !defined(XALANSOURCETREEDEFINITIONS_HEADER_GUARD_1357924680)
#define XALANSOURCETREEDEFINITIONS_HEADER_GUARD_1357924680



#include <PlatformDefinitions.hpp>



#if defined(XALAN_XALANSOURCETREE_BUILD_DLL)

#define XALAN_XALANSOURCETREE_EXPORT XALAN_PLATFORM_EXPORT

#define XALAN_XALANSOURCETREE_EXPORT_FUNCTION(T) XALAN_PLATFORM_EXPORT_FUNCTION(T)

#else

#define XALAN_XALANSOURCETREE_EXPORT XALAN_PLATFORM_IMPORT

#define XALAN_XALANSOURCETREE_EXPORT_FUNCTION(T) XALAN_PLATFORM_IMPORT_FUNCTION(T)

#endif



#endif	// XALANSOURCETREEDEFINITIONS_HEADER_GUARD_1357924680
