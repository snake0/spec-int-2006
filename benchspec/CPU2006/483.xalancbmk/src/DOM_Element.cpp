/*
 * The Apache Software License, Version 1.1
 *
 * Copyright (c) 1999-2002 The Apache Software Foundation.  All rights
 * reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in
 *    the documentation and/or other materials provided with the
 *    distribution.
 *
 * 3. The end-user documentation included with the redistribution,
 *    if any, must include the following acknowledgment:
 *       "This product includes software developed by the
 *        Apache Software Foundation (http://www.apache.org/)."
 *    Alternately, this acknowledgment may appear in the software itself,
 *    if and wherever such third-party acknowledgments normally appear.
 *
 * 4. The names "Xerces" and "Apache Software Foundation" must
 *    not be used to endorse or promote products derived from this
 *    software without prior written permission. For written
 *    permission, please contact apache\@apache.org.
 *
 * 5. Products derived from this software may not be called "Apache",
 *    nor may "Apache" appear in their name, without prior written
 *    permission of the Apache Software Foundation.
 *
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND ANY EXPRESSED OR IMPLIED
 * WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED.  IN NO EVENT SHALL THE APACHE SOFTWARE FOUNDATION OR
 * ITS CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
 * USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 * OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
 * OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 * ====================================================================
 *
 * This software consists of voluntary contributions made by many
 * individuals on behalf of the Apache Software Foundation, and was
 * originally based on software copyright (c) 1999, International
 * Business Machines, Inc., http://www.ibm.com .  For more information
 * on the Apache Software Foundation, please see
 * <http://www.apache.org/>.
 */

/*
 * $Id: DOM_Element.cpp,v 1.3 2002/11/04 15:04:44 tng Exp $
 */

#include "DOM_Element.hpp"
#include "DOM_Attr.hpp"
#include "DOM_NodeList.hpp"
#include "ElementImpl.hpp"
#include "DeepNodeListImpl.hpp"

XERCES_CPP_NAMESPACE_BEGIN


DOM_Element::DOM_Element()
: DOM_Node(null)
{
}


DOM_Element::DOM_Element(const DOM_Element & other)
: DOM_Node(other)
{
}


DOM_Element::DOM_Element(ElementImpl *impl) :
        DOM_Node(impl)
{
}


DOM_Element::~DOM_Element()
{
}


DOM_Element & DOM_Element::operator = (const DOM_Element & other)
{
    return (DOM_Element &) DOM_Node::operator = (other);
}


DOM_Element & DOM_Element::operator = (const DOM_NullPtr *other)
{
    return (DOM_Element &) DOM_Node::operator = (other);
}


DOMString DOM_Element::getTagName() const
{
        return ((ElementImpl *)fImpl)->getTagName().clone();
}


DOMString DOM_Element::getAttribute(const DOMString &name) const
{
        return ((ElementImpl *)fImpl)->getAttribute(name).clone();
}


void      DOM_Element::setAttribute(const DOMString &name,
                                    const DOMString &value)
{
        ((ElementImpl *)fImpl)->setAttribute(name, value);
}



void      DOM_Element::removeAttribute(const DOMString &name)
{
        ((ElementImpl *)fImpl)->removeAttribute(name);
}


DOM_Attr  DOM_Element::getAttributeNode(const DOMString &name) const
{
        return DOM_Attr(((ElementImpl *)fImpl)->getAttributeNode(name));
}


DOM_Attr  DOM_Element::setAttributeNode(DOM_Attr newAttr)
{
        return DOM_Attr(((ElementImpl *)fImpl)->
                             setAttributeNode((AttrImpl *)newAttr.fImpl));
}


DOM_Attr  DOM_Element::removeAttributeNode(DOM_Attr oldAttr)
{
        return DOM_Attr(((ElementImpl *)fImpl)->
                                                removeAttributeNode((AttrImpl *)oldAttr.fImpl));
}


DOM_NodeList DOM_Element::getElementsByTagName(const DOMString &name) const
{
        return DOM_NodeList(((ElementImpl *)fImpl)->getElementsByTagName(name));

}


//Introduced in DOM Level 2

DOMString DOM_Element::getAttributeNS(const DOMString &namespaceURI,
	const DOMString &localName) const
{
    return ((ElementImpl *)fImpl)->getAttributeNS(namespaceURI, localName).clone();
}

void DOM_Element::setAttributeNS(const DOMString &namespaceURI,
	const DOMString &qualifiedName, const DOMString &value)
{
    ((ElementImpl *)fImpl)->setAttributeNS(namespaceURI, qualifiedName, value);
}


void DOM_Element::removeAttributeNS(const DOMString &namespaceURI,
	const DOMString &localName)
{
    ((ElementImpl *)fImpl)->removeAttributeNS(namespaceURI, localName);
}


DOM_Attr DOM_Element::getAttributeNodeNS(const DOMString &namespaceURI,
	const DOMString &localName) const
{
    return DOM_Attr(((ElementImpl *)fImpl)->getAttributeNodeNS(namespaceURI, localName));
}


DOM_Attr DOM_Element::setAttributeNodeNS(DOM_Attr newAttr)
{
    return DOM_Attr(((ElementImpl *)fImpl)->
                             setAttributeNodeNS((AttrImpl *)newAttr.fImpl));
}


DOM_NodeList DOM_Element::getElementsByTagNameNS(const DOMString &namespaceURI,
	const DOMString &localName) const
{
    return DOM_NodeList(((ElementImpl *)fImpl)->getElementsByTagNameNS(namespaceURI,
	localName));
}


bool DOM_Element::hasAttributes() const
{
        return ((ElementImpl *)fImpl)->hasAttributes();
}


bool DOM_Element::hasAttribute(const DOMString &name) const
{
        return ((ElementImpl *)fImpl)->hasAttribute(name);
}


bool DOM_Element::hasAttributeNS(const DOMString &namespaceURI,
	const DOMString &localName) const
{
    return ((ElementImpl *)fImpl)->hasAttributeNS(namespaceURI, localName);
}


XERCES_CPP_NAMESPACE_END

