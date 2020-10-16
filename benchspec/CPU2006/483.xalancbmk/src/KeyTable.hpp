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
#if !defined(XALAN_KEYTABLE_HEADER_GUARD)
#define XALAN_KEYTABLE_HEADER_GUARD 

/**
 * $Id: KeyTable.hpp,v 1.4 2004/02/26 22:58:57 mhoyt Exp $
 * 
 * $State: Exp $
 * 
 */



// Base include file.  Must be first.
#include "XSLTDefinitions.hpp"



#if defined(XALAN_USE_HASH_MAP)
#include <hash_map>
#else
#include <map>
#endif
#include <vector>



#include <DOMStringHelper.hpp>



#include <MutableNodeRefList.hpp>
#include <XalanQNameByReference.hpp>



XALAN_CPP_NAMESPACE_BEGIN



class KeyDeclaration;
class NodeRefListBase;
class PrefixResolver;
class StylesheetExecutionContext;
class XalanElement;
class XalanDocument;
class XalanNode;




/**
 * Table of element keys, keyed by document node.  An instance of this 
 * class is keyed by a Document node that should be matched with the 
 * root of the current context.  It contains a table of name mappings 
 * to tables that contain mappings of identifier values to nodes.
 */
class KeyTable
{
public:

#if defined(XALAN_NO_STD_NAMESPACE)
	typedef vector<KeyDeclaration>			KeyDeclarationVectorType;

	typedef map<XalanDOMString,
				MutableNodeRefList,
				less<XalanDOMString> >		NodeListMapType;

	typedef map<XalanQNameByReference,
				NodeListMapType,
				less<XalanQNameByReference> >	KeysMapType;
#else
	typedef std::vector<KeyDeclaration>		KeyDeclarationVectorType;

#if defined(XALAN_USE_HASH_MAP)
	typedef std::hash_map<XalanDOMString,
						  MutableNodeRefList>	NodeListMapType;

	typedef std::hash_map<XalanQNameByReference,
						  NodeListMapType>		KeysMapType;
#else
	typedef std::map<XalanDOMString,
					 MutableNodeRefList>	NodeListMapType;

	typedef std::map<XalanQNameByReference,
					 NodeListMapType>		KeysMapType;
#endif
#endif


	/**
	 * Build a keys table.
	 *
	 * @param startNode        node to start iterating from to build the keys
	 *                         index
	 * @param nscontext        stylesheet's namespace context
	 * @param keyDeclarations  stylesheet's xsl:key declarations
	 * @param executionContext current execution context
	 */
	KeyTable(
			XalanNode*							startNode,
			const PrefixResolver&				resolver,
			const KeyDeclarationVectorType&		keyDeclarations,
			StylesheetExecutionContext&			executionContext);

	virtual
	~KeyTable();

	/**
	 * Given a valid element key, return the corresponding node list. If the
	 * name was not declared with xsl:key, this will return null, the
	 * identifier is not found, it will return an empty node set, otherwise it
	 * will return a nodeset of nodes.
	 *
	 * @param name name of the key, which must match the 'name' attribute on
	 *             xsl:key
	 * @param ref  value that must match the value found by the 'match'
	 *             attribute on xsl:key
	 * @return		pointer to nodeset for key 
	 */
	const MutableNodeRefList&
	getNodeSetByKey(
				  const XalanQName&			qname,
				  const XalanDOMString&		ref) const;

private:

	static void
	processKeyDeclaration(
			KeysMapType&					theKeys,
			const KeyDeclaration&			kd,
			XalanNode*						testNode,
			const PrefixResolver&			resolver,
			StylesheetExecutionContext&		executionContext);

	/**
	 * The document key.  This table should only be used with contexts
	 * whose Document roots match this key.
	 */
	const XalanDocument*	m_docKey;

	/**
	 * Table of element keys.  The table will be built on demand, 
	 * when a key is requested, or set by the XMLParserLiaison or 
	 * the caller.  The table is:
	 * a) keyed by name,
	 * b) each with a value of a hashtable, keyed by the value returned by 
	 *    the use attribute,
	 * c) each with a value that is a nodelist.
	 * Thus, for a given key or keyref, look up hashtable by name, 
	 * look up the nodelist by the given reference.
	 */

	KeysMapType							m_keys;

	static const MutableNodeRefList		s_dummyList;
};



XALAN_CPP_NAMESPACE_END



#endif	// XALAN_KEYTABLE_HEADER_GUARD
