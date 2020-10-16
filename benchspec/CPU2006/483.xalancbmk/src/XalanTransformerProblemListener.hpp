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
#if !defined(XALAN_XALANTRANSFORMERPROBLEMLISTENER_HEADER_GUARD)
#define XALAN_XALANTRANSFORMERPROBLEMLISTENER_HEADER_GUARD




// Base include file.  Must be first.
#include "XalanTransformerDefinitions.hpp"



#if defined(XALAN_CLASSIC_IOSTREAMS)
class ostream;
#else
#include <iosfwd>
#endif



// Xalan header files.
#include <ProblemListenerDefault.hpp>



XALAN_CPP_NAMESPACE_BEGIN



class XALAN_TRANSFORMER_EXPORT XalanTransformerProblemListener : public ProblemListener
{
public:

#if defined(XALAN_NO_STD_NAMESPACE)
	typedef ostream			StreamType;
#else
	typedef std::ostream	StreamType;
#endif

	XalanTransformerProblemListener(
			StreamType*		theStream,
			PrintWriter*	thePrintWriter);

	virtual
	~XalanTransformerProblemListener();

	// These methods are inherited from ProblemListener ...
	
	virtual void
	setPrintWriter(PrintWriter*		pw);

	virtual void
	problem(
			eProblemSource				where,
			eClassification				classification,
			const XalanNode*			sourceNode,
			const ElemTemplateElement*	styleNode,
			const XalanDOMString&		msg,
			const XalanDOMChar*			uri,
			int							lineNo,
			int							charOffset);

private:

	ProblemListenerDefault	m_problemListener;

	StreamType*		m_warningStream;
};



XALAN_CPP_NAMESPACE_END



#endif	// XALAN_XALANTRANSFORMERPROBLEMLISTENER_HEADER_GUARD
