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
 * $Id: DomMemDebug.cpp,v 1.3 2002/11/04 15:04:44 tng Exp $
 */


#include "DomMemDebug.hpp"
#include "DOMString.hpp"
#include "NodeImpl.hpp"
#include "NamedNodeMapImpl.hpp"
#include <stdio.h>

XERCES_CPP_NAMESPACE_BEGIN


DomMemDebug::DomMemDebug()
{
    liveStringHandles   = DOMString::gLiveStringHandleCount;
    totalStringHandles  = DOMString::gTotalStringHandleCount;
    liveStringBuffers   = DOMString::gLiveStringDataCount;
    totalStringBuffers  = DOMString::gTotalStringDataCount;
    liveNodeImpls       = NodeImpl::gLiveNodeImpls;
    totalNodeImpls      = NodeImpl::gTotalNodeImpls;
    liveNamedNodeMaps   = NamedNodeMapImpl::gLiveNamedNodeMaps;
    totalNamedNodeMaps  = NamedNodeMapImpl::gTotalNamedNodeMaps;
}   /* SPEC_CPU: removed extra ';' for C++98 standards compliance -- yag */


DomMemDebug::~DomMemDebug()
{
}   /* SPEC_CPU: removed extra ';' for C++98 standards compliance -- yag */


bool DomMemDebug::operator == (const DomMemDebug &other)
{
    bool    r =
        liveStringHandles   ==  other.liveStringHandles  &&
        liveStringBuffers   ==  other.liveStringBuffers  &&
        liveNodeImpls       ==  other.liveNodeImpls      &&
        liveNamedNodeMaps   ==  other.liveNamedNodeMaps;
    return r;
}   /* SPEC_CPU: removed extra ';' for C++98 standards compliance -- yag */


bool DomMemDebug::operator != (const DomMemDebug &other)
{
    return ! operator == (other);
}   /* SPEC_CPU: removed extra ';' for C++98 standards compliance -- yag */


void DomMemDebug::operator = (const DomMemDebug &other)
{
    liveStringHandles  = other.liveStringHandles;
    totalStringHandles = other.totalStringHandles;
    liveStringBuffers  = other.liveStringBuffers;
    totalStringBuffers = other.totalStringBuffers;
    liveNodeImpls      = other.liveNodeImpls;
    totalNodeImpls     = other.totalNodeImpls;
    liveNamedNodeMaps  = other.liveNamedNodeMaps;
    totalNamedNodeMaps = other.totalNamedNodeMaps;
}   /* SPEC_CPU: removed extra ';' for C++98 standards compliance -- yag */

void DomMemDebug::print()
{
    printf("DOM reference counted memory alloction statistics:\n"
        "    live  string handles:   %d\n"
        "    total string handles:   %d\n"
        "    live  string buffers:   %d\n"
        "    total string buffers:   %d\n"
        "    live  nodeImpls:        %d\n"
        "    total nodeImpls:        %d\n"
        "    live  NamedNodeMaps:    %d\n"
        "    total NamedNodeMaps:    %d\n",
            liveStringHandles ,
            totalStringHandles,
            liveStringBuffers  ,
            totalStringBuffers ,
            liveNodeImpls      ,
            totalNodeImpls     ,
            liveNamedNodeMaps  ,
            totalNamedNodeMaps);
}   /* SPEC_CPU: removed extra ';' for C++98 standards compliance -- yag */


void DomMemDebug::printDifference(const DomMemDebug &other)
{
    int d;

    d = liveStringHandles - other.liveStringHandles;
    if (d != 0)
        printf("   %d StringHandles.", d);

    d = liveStringBuffers - other.liveStringBuffers;
    if (d != 0)
        printf("   %d StringBuffers.", d);

    d = liveNodeImpls - other.liveNodeImpls;
    if (d != 0)
        printf("   %d NodeImpls.", d);

    d = liveNamedNodeMaps - other.liveNamedNodeMaps;
    if (d != 0)
        printf("   %d NamedNodeMaps.", d);

    printf("\n");
}   /* SPEC_CPU: removed extra ';' for C++98 standards compliance -- yag */

XERCES_CPP_NAMESPACE_END

