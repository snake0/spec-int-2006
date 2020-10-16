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
 * $Log: XSSimpleTypeDefinition.cpp,v $
 * Revision 1.8  2003/12/15 17:23:48  cargilld
 * psvi updates; cleanup revisits and bug fixes
 *
 * Revision 1.7  2003/11/25 18:08:31  knoaman
 * Misc. PSVI updates. Thanks to David Cargill.
 *
 * Revision 1.6  2003/11/24 15:45:36  knoaman
 * PSVI: finish construction of XSSimpleTypeDefinition
 *
 * Revision 1.5  2003/11/21 17:34:04  knoaman
 * PSVI update
 *
 * Revision 1.4  2003/11/14 22:47:53  neilg
 * fix bogus log message from previous commit...
 *
 * Revision 1.3  2003/11/14 22:33:30  neilg
 * Second phase of schema component model implementation.  
 * Implement XSModel, XSNamespaceItem, and the plumbing necessary
 * to connect them to the other components.
 * Thanks to David Cargill.
 *
 * Revision 1.2  2003/11/06 15:30:04  neilg
 * first part of PSVI/schema component model implementation, thanks to David Cargill.  This covers setting the PSVIHandler on parser objects, as well as implementing XSNotation, XSSimpleTypeDefinition, XSIDCDefinition, and most of XSWildcard, XSComplexTypeDefinition, XSElementDeclaration, XSAttributeDeclaration and XSAttributeUse.
 *
 * Revision 1.1  2003/09/16 14:33:36  neilg
 * PSVI/schema component model classes, with Makefile/configuration changes necessary to build them
 *
 */

#include <xercesc/framework/psvi/XSSimpleTypeDefinition.hpp>
#include <xercesc/framework/psvi/XSFacet.hpp>
#include <xercesc/framework/psvi/XSMultiValueFacet.hpp>
#include <xercesc/framework/psvi/XSAnnotation.hpp>
#include <xercesc/framework/psvi/XSModel.hpp>
#include <xercesc/validators/datatype/DatatypeValidator.hpp>
#include <xercesc/validators/datatype/UnionDatatypeValidator.hpp>
#include <xercesc/util/XMLStringTokenizer.hpp>
#include <xercesc/util/XMLUniDefs.hpp>

XERCES_CPP_NAMESPACE_BEGIN

// ---------------------------------------------------------------------------
//  Local, static functions
// ---------------------------------------------------------------------------
static bool XSSimpleTypeDefinitionTestFlag(int flag) 
{
    if (flag)
        return true;
    return false;
}


// ---------------------------------------------------------------------------
//  XSSimpleTypeDefinition: Constructors and Destructors
// ---------------------------------------------------------------------------
XSSimpleTypeDefinition::XSSimpleTypeDefinition
(
    DatatypeValidator* const            datatypeValidator
    , VARIETY                           stVariety
    , XSTypeDefinition* const           xsBaseType
    , XSSimpleTypeDefinition* const     primitiveOrItemType
    , XSSimpleTypeDefinitionList* const memberTypes
    , XSAnnotation*                     headAnnot
    , XSModel* const                    xsModel
    , MemoryManager* const              manager
)
    : XSTypeDefinition(SIMPLE_TYPE, xsBaseType, xsModel, manager)
    , fDefinedFacets(0)
    , fFixedFacets(0)
    , fVariety(stVariety)
    , fDatatypeValidator(datatypeValidator)
    , fXSFacetList(0)
    , fXSMultiValueFacetList(0)
    , fPatternList(0)
    , fPrimitiveOrItemType(primitiveOrItemType)
    , fMemberTypes(memberTypes)
    , fXSAnnotationList(0)
{
    if (int finalset = fDatatypeValidator->getFinalSet()) 
    {
        if (finalset & SchemaSymbols::XSD_EXTENSION)
            fFinal |= XSConstants::DERIVATION_EXTENSION;

        if (finalset & SchemaSymbols::XSD_RESTRICTION)
            fFinal |= XSConstants::DERIVATION_RESTRICTION;

        if (finalset & SchemaSymbols::XSD_LIST)
            fFinal |= XSConstants::DERIVATION_LIST;

        if (finalset & SchemaSymbols::XSD_UNION)
            fFinal |= XSConstants::DERIVATION_UNION;
    }

    if (headAnnot)
    {
        XSAnnotation* annot = headAnnot;

        fXSAnnotationList = new (manager) RefVectorOf<XSAnnotation>(3, false, manager);
        do
        {
            fXSAnnotationList->addElement(annot);
            annot = annot->getNext();
        } while (annot);
    }
}

XSSimpleTypeDefinition::~XSSimpleTypeDefinition()
{
    if (fXSFacetList) 
        delete fXSFacetList;

    if (fXSMultiValueFacetList) 
        delete fXSMultiValueFacetList;

    if (fPatternList) 
        delete fPatternList;

    // don't delete fPrimitiveOrItemType -> deleted by XSModel
    if (fMemberTypes)
        delete fMemberTypes;

    if (fXSAnnotationList)
        delete fXSAnnotationList;
}


// ---------------------------------------------------------------------------
//  XSSimpleTypeDefinition: access methods
// ---------------------------------------------------------------------------
bool XSSimpleTypeDefinition::isDefinedFacet(FACET facetName)
{
    return XSSimpleTypeDefinitionTestFlag(fDefinedFacets & facetName);
}

bool XSSimpleTypeDefinition::isFixedFacet(FACET facetName)
{
    return XSSimpleTypeDefinitionTestFlag(fFixedFacets & facetName);
}

const XMLCh *XSSimpleTypeDefinition::getLexicalFacetValue(FACET facetName)
{ 
    unsigned int size = fXSFacetList->size();
    for (unsigned int i=0; i<size; i++) 
    {
        if (((fXSFacetList->elementAt(i))->getFacetKind()) == facetName)
            return (fXSFacetList->elementAt(i))->getLexicalFacetValue();
    }
    return 0;
}

StringList *XSSimpleTypeDefinition::getLexicalEnumeration()
{
    return (RefArrayVectorOf<XMLCh>*) fDatatypeValidator->getEnumString();
}

XSSimpleTypeDefinition::ORDERING XSSimpleTypeDefinition::getOrdered() const
{
    return fDatatypeValidator->getOrdered();
}

bool XSSimpleTypeDefinition::getFinite() const
{
    return fDatatypeValidator->getFinite();
}

bool XSSimpleTypeDefinition::getBounded() const
{
    return fDatatypeValidator->getBounded();
}

bool XSSimpleTypeDefinition::getNumeric() const
{
    return fDatatypeValidator->getNumeric();
}


// ---------------------------------------------------------------------------
//  XSSimpleTypeDefinition: virtual methods
// ---------------------------------------------------------------------------
const XMLCh *XSSimpleTypeDefinition::getName() 
{
    return fDatatypeValidator->getTypeLocalName();
}

const XMLCh *XSSimpleTypeDefinition::getNamespace() 
{
    return fDatatypeValidator->getTypeUri();
}

XSNamespaceItem *XSSimpleTypeDefinition::getNamespaceItem() 
{
    return fXSModel->getNamespaceItem(getNamespace());
}

bool XSSimpleTypeDefinition::getAnonymous() const
{
    return fDatatypeValidator->getAnonymous(); 
}

XSTypeDefinition *XSSimpleTypeDefinition::getBaseType() 
{
    return fBaseType;
}

bool XSSimpleTypeDefinition::derivedFromType(const XSTypeDefinition * const ancestorType)
{
    if (!ancestorType)
        return false;

    if (ancestorType->getTypeCategory() == XSTypeDefinition::COMPLEX_TYPE)
        return false;

    XSTypeDefinition* type = this;

    while (type && (type != ancestorType))
    {
        type = type->getBaseType();
    }

    return (type == ancestorType);
}

// ---------------------------------------------------------------------------
//  XSSimpleTypeDefinition: helper methods
// ---------------------------------------------------------------------------
void XSSimpleTypeDefinition::setFacetInfo
(
    int                            definedFacets
    , int                          fixedFacets
    , XSFacetList* const           xsFacetList
    , XSMultiValueFacetList* const xsMultiValueFacetList
    , StringList* const            patternList
)
{
    fDefinedFacets = definedFacets;
    fFixedFacets = fixedFacets;
    fXSFacetList = xsFacetList;
    fXSMultiValueFacetList = xsMultiValueFacetList;
    fPatternList = patternList;
}


XERCES_CPP_NAMESPACE_END
