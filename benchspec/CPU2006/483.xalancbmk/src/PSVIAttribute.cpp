/*
 * The Apache Software License, Version 1.1
 *
 * Copyright (c) 2003 The Apache Software Foundation.  All rights
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
 * $Log: PSVIAttribute.cpp,v $
 * Revision 1.9  2004/01/29 11:46:30  cargilld
 * Code cleanup changes to get rid of various compiler diagnostic messages.
 *
 * Revision 1.8  2003/12/30 05:58:56  neilg
 * allow schema normalized values to be associated with a PSVIAttribute after it is reset
 *
 * Revision 1.7  2003/12/02 17:37:03  neilg
 * fix compilation problem
 *
 * Revision 1.6  2003/12/02 17:31:42  neilg
 * since there are certain things, such as schemaLocation attributes, that have a datatype and which we nonetheless do not validate, make canonical-value production dependent on validity being valid
 *
 * Revision 1.5  2003/11/28 22:41:04  neilg
 * fix compilation error
 *
 * Revision 1.4  2003/11/28 20:20:54  neilg
 * make use of canonical representation in PSVIAttribute implementation
 *
 * Revision 1.3  2003/11/27 06:10:32  neilg
 * PSVIAttribute implementation
 *
 * Revision 1.2  2003/11/06 21:50:33  neilg
 * fix compilation errors under gcc 3.3.
 *
 * Revision 1.1  2003/09/16 14:33:36  neilg
 * PSVI/schema component model classes, with Makefile/configuration changes necessary to build them
 *
 */

#include <xercesc/framework/psvi/PSVIAttribute.hpp>

XERCES_CPP_NAMESPACE_BEGIN

PSVIAttribute::PSVIAttribute( MemoryManager* const manager ):  
        PSVIItem(manager)
        , fAttributeDecl(0)
        , fDV(0)
{
}

void PSVIAttribute::reset(
            const XMLCh * const         valContext
            , PSVIItem::VALIDITY_STATE  state
            , PSVIItem::ASSESSMENT_TYPE assessmentType
            , XSSimpleTypeDefinition *  validatingType
            , XSSimpleTypeDefinition *  memberType
            , const XMLCh * const       defaultValue
            , const bool                isSpecified
            , XSAttributeDeclaration *  attrDecl
            , DatatypeValidator *dv
        )
{
    fValidationContext = valContext;
    fValidityState = state;
    fAssessmentType = assessmentType;
    fType = validatingType;
    fMemberType = memberType;
    fDefaultValue = defaultValue;
    fIsSpecified = isSpecified;
    fMemoryManager->deallocate((void *)fCanonicalValue);
    fCanonicalValue = 0;
    fNormalizedValue = 0;
    fAttributeDecl = attrDecl;
    fDV = dv;
}

void PSVIAttribute::setValue(const XMLCh * const       normalizedValue)
{ 
    if(normalizedValue)
    {
        fNormalizedValue = normalizedValue;
        if(fDV && fValidityState == PSVIItem::VALIDITY_VALID)
            fCanonicalValue = (XMLCh *)fDV->getCanonicalRepresentation(normalizedValue, fMemoryManager);
    }
}

XERCES_CPP_NAMESPACE_END


