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
#if !defined(NODENAMETREEWALKER_HEADER_GUARD_1357924680)
#define NODENAMETREEWALKER_HEADER_GUARD_1357924680


#include <vector>



#include <XalanDOMString.hpp>



// Base class include file.
#include <TreeWalker.hpp>



XALAN_CPP_NAMESPACE_BEGIN



class NodeNameTreeWalker : public TreeWalker
{
public:

#if defined(XALAN_NO_STD_NAMESPACE)
	typedef vector<const XalanNode*>		NodeVectorType;
#else
	typedef std::vector<const XalanNode*>	NodeVectorType;
#endif

	NodeNameTreeWalker();

	virtual
	~NodeNameTreeWalker();

	/**
	 * Find all nodes matching a specified node in a specified node tree.  The
	 * member m_matchingNodes is populated as a side effect.
	 *
	 * @param theNodeName  name of node sought
	 * @param theStartNode start node for search
	 */
	virtual void
	findMatchingNodes(
			const XalanDOMString&	theNodeName,
			const XalanNode*		theStartNode);

	virtual void
	findMatchingNodes(
			const XalanDOMString&	theNodeName,
			XalanNode*				theStartNode);

	/**
	 * Retrieve the matching nodes from the last search.
	 *
	 * @return vector of nodes
	 */
	const NodeVectorType&
	getMatchingNodes() const
	{
		return m_matchingNodes;
	}

protected:

	virtual bool
	startNode(XalanNode*	node);

	virtual bool
	endNode(XalanNode*	node);

	virtual bool
	startNode(const XalanNode*	node);

	virtual bool
	endNode(const XalanNode*	node);

private:

	XalanDOMString	m_nodeName;

	NodeVectorType	m_matchingNodes;
};



XALAN_CPP_NAMESPACE_END



#endif	// NODENAMETREEWALKER_HEADER_GUARD_1357924680
