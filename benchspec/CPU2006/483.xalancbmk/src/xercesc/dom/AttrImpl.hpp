#ifndef AttrImpl_HEADER_GUARD_
#define AttrImpl_HEADER_GUARD_

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

/**
 * Attribute represents an XML-style attribute of an
 * Element. Typically, the allowable values are controlled by its
 * declaration in the Document Type Definition (DTD) governing this
 * kind of document.
 * <P>
 * If the attribute has not been explicitly assigned a value, but has
 * been declared in the DTD, it will exist and have that default. Only
 * if neither the document nor the DTD specifies a value will the
 * Attribute really be considered absent and have no value; in that
 * case, querying the attribute will return null.
 * <P>
 * Attributes may have multiple children that contain their data. (XML
 * allows attributes to contain entity references, and tokenized
 * attribute types such as NMTOKENS may have a child for each token.)
 * For convenience, the Attribute object's getValue() method returns
 * the string version of the attribute's value.
 * <P>
 * Attributes are not children of the Elements they belong to, in the
 * usual sense, and have no valid Parent reference. However, the spec
 * says they _do_ belong to a specific Element, and an INUSE exception
 * is to be thrown if the user attempts to explicitly share them
 * between elements.
 * <P>
 * Note that Elements do not permit attributes to appear to be shared
 * (see the INUSE exception), so this object's mutability is
 * officially not an issue.
 * <p>
 * Note: The ownerNode attribute is used to store the Element the Attr
 * node is associated with. Attr nodes do not have parent nodes.
 * Besides, the getOwnerElement() method can be used to get the element node
 * this attribute is associated with.
 * <P>
 * AttrImpl does not support Namespaces. AttrNSImpl, which inherits from
 * it, does.
 *
 * <p>AttrImpl used to inherit from ParentNode. It now directly inherits from
 * NodeImpl and provide its own implementation of the ParentNode's behavior.
 * The reason is that we now try and avoid to always creating a Text node to
 * hold the value of an attribute. The DOM spec requires it, so we still have
 * to do it in case getFirstChild() is called for instance. The reason
 * attribute values are stored as a list of nodes is so that they can carry
 * more than a simple string. They can also contain EntityReference nodes.
 * However, most of the times people only have a single string that they only
 * set and get through Element.set/getAttribute or Attr.set/getValue. In this
 * new version, the Attr node has a value pointer which can either be the
 * String directly or a pointer to the first ChildNode. A flag tells which one
 * it currently is. Note that while we try to stick with the direct String as
 * much as possible once we've switched to a node there is no going back. This
 * is because we have no way to know whether the application keeps referring to
 * the node we once returned.
 * <p> The gain in memory varies on the density of attributes in the document.
 * But in the tests I've run I've seen up to 12% of memory gain. And the good
 * thing is that it also leads to a slight gain in speed because we allocate
 * fewer objects! I mean, that's until we have to actually create the node...
 * <p>
 * To avoid too much duplicated code, I got rid of ParentNode and renamed
 * ChildAndParentNode, which I never really liked, to ParentNode for
 * simplicity, this doesn't make much of a difference in memory usage because
 * there are only very objects that are only a Parent. This is only true now
 * because AttrImpl now inherits directly from NodeImpl and has its own
 * implementation of the ParentNode's node behavior. So there is still some
 * duplicated code there.
 *
 * <p><b>WARNING</b>: Some of the code here is partially duplicated in
 * ParentNode, be careful to keep these two classes in sync!
 *
 * $Id: AttrImpl.hpp,v 1.4 2003/05/24 23:56:35 neilg Exp $
 */

//
//  This file is part of the internal implementation of the C++ XML DOM.
//  It should NOT be included or used directly by application programs.
//
//  Applications should include the file <xercesc/dom/deprecated/DOM.hpp> for the entire
//  DOM API, or DOM_*.hpp for individual DOM classes, where the class
//  name is substituded for the *.
//



#include <xercesc/util/XercesDefs.hpp>
#include "ChildNode.hpp"
#include "DOM_Node.hpp"

XERCES_CPP_NAMESPACE_BEGIN


class ElementImpl;

class CDOM_EXPORT AttrImpl: public NodeImpl {

public:
    DOMString name;

    /** This can either be a DOMString or the first child node (ChildNode*). */
    union {
        ChildNode *child;
        DOMString *str;
    } value;

public:
    AttrImpl(DocumentImpl *ownerDocument, const DOMString &aName);
    AttrImpl(const AttrImpl &other, bool deep=false);
    virtual ~AttrImpl();
    virtual NodeImpl *cloneNode(bool deep=false);
    virtual DOMString getNodeName();
    virtual short getNodeType();
    virtual DOMString getName();
    virtual DOMString getNodeValue();
    virtual bool getSpecified();
    virtual DOMString getValue();
    virtual bool isAttrImpl();
    virtual void setNodeValue(const DOMString &value);
    virtual void setSpecified(bool arg);
    virtual void setValue(const DOMString &value);
    virtual DOMString toString();

    //Introduced in DOM Level 2
    ElementImpl *getOwnerElement();
    void setOwnerElement(ElementImpl *ownerElem);    //internal use only


    // ParentNode stuff
    virtual NodeListImpl *getChildNodes();
    virtual NodeImpl * getFirstChild();
    virtual NodeImpl * getLastChild();
    virtual unsigned int getLength();
    virtual bool        hasChildNodes();
    virtual NodeImpl    *insertBefore(NodeImpl *newChild, NodeImpl *refChild);
    virtual NodeImpl    *item(unsigned int index);
    virtual NodeImpl    *removeChild(NodeImpl *oldChild);
    virtual NodeImpl    *replaceChild(NodeImpl *newChild, NodeImpl *oldChild);
    virtual void        setReadOnly(bool isReadOnly, bool deep);

    //Introduced in DOM Level 2
    virtual void	normalize();

protected:
    void makeChildNode();
    void cloneChildren(const NodeImpl &other);
    ChildNode * lastChild();
    void lastChild(ChildNode *);
    inline DOMString* valueToDOMString();

};

XERCES_CPP_NAMESPACE_END

#endif
