/*
 * The Apache Software License, Version 1.1
 *
 * Copyright (c) 2001 The Apache Software Foundation.  All rights
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
 * originally based on software copyright (c) 2001, International
 * Business Machines, Inc., http://www.ibm.com .  For more information
 * on the Apache Software Foundation, please see
 * <http://www.apache.org/>.
 */

/*
 * $Log: QName.cpp,v $
 * Revision 1.11  2003/10/01 16:32:39  neilg
 * improve handling of out of memory conditions, bug #23415.  Thanks to David Cargill.
 *
 * Revision 1.10  2003/09/25 22:24:28  peiyongz
 * Using writeString/readString
 *
 * Revision 1.9  2003/09/25 15:22:34  peiyongz
 * Implementation of Serialization
 *
 * Revision 1.8  2003/05/16 06:01:52  knoaman
 * Partial implementation of the configurable memory manager.
 *
 * Revision 1.7  2003/05/15 19:04:35  knoaman
 * Partial implementation of the configurable memory manager.
 *
 * Revision 1.6  2002/11/04 15:22:04  tng
 * C++ Namespace Support.
 *
 * Revision 1.5  2002/10/30 21:52:00  tng
 * [Bug 13641] compiler-generated copy-constructor for QName doesn't do the right thing.
 *
 * Revision 1.4  2002/09/24 19:51:24  tng
 * Performance: use XMLString::equals instead of XMLString::compareString
 *
 * Revision 1.3  2002/09/05 16:06:41  tng
 * [Bug 12232] Make operator to be constant.
 *
 * Revision 1.2  2002/04/02 15:31:48  knoaman
 * Modiy QName comparison (operator=).
 *
 * Revision 1.1.1.1  2002/02/01 22:22:11  peiyongz
 * sane_include
 *
 * Revision 1.8  2001/12/06 17:48:36  tng
 * Performance Enhancement.  Added setNPrefix and setNLocalPart methods that allow code to take advantage of the fact that it knows the length of the prefix and local name, when possible.  That can avoid a copy of the prefix into a null-terminated temporary variable before copying into the fPrefix.
 * Also changed the getRawName method so that it would simply return the local part when there is no prefix, instead of allocating another buffer to copy the local part into the fRawName.
 * When there is a prefix, changed the getRawName to copy the prefix and local part into the fRawName using XMLString::moveChars instead of using XMLString::copyString and XMLString::catString.  The catString method has to loop past the prefix portion of the fRawName, which seems like a waste.
 * By Henry Zongaro.
 *
 * Revision 1.7  2001/07/24 18:31:47  knoaman
 * Added support for <group> + extra constraint checking for complexType
 *
 * Revision 1.6  2001/05/11 13:26:28  tng
 * Copyright update.
 *
 * Revision 1.5  2001/04/19 18:17:08  tng
 * Schema: SchemaValidator update, and use QName in Content Model
 *
 * Revision 1.4  2001/03/21 21:56:11  tng
 * Schema: Add Schema Grammar, Schema Validator, and split the DTDValidator into DTDValidator, DTDScanner, and DTDGrammar.
 *
 * Revision 1.3  2001/02/27 14:48:35  tng
 * Schema: Add CMAny and ContentLeafNameTypeVector, by Pei Yong Zhang
 *
 * Revision 1.2  2001/02/26 21:56:15  tng
 * Schema: QName can also be constructed with rawName.
 *
 * Revision 1.1  2001/02/26 19:44:25  tng
 * Schema: add utility class QName, by Pei Yong Zhang.
 *
 */

#include <xercesc/util/QName.hpp>
#include <xercesc/util/OutOfMemoryException.hpp>

XERCES_CPP_NAMESPACE_BEGIN

// ---------------------------------------------------------------------------
//  QName: Constructors and Destructor
// ---------------------------------------------------------------------------
QName::QName(MemoryManager* const manager) :

    fMemoryManager(manager)
    , fPrefix(0)
    , fPrefixBufSz(0)
    , fLocalPart(0)
    , fLocalPartBufSz(0)
    , fRawName(0)
    , fRawNameBufSz(0)
    , fURIId(0)
{
}

QName::QName( const XMLCh* const   prefix
            , const XMLCh* const   localPart
            , const unsigned int   uriId
            , MemoryManager* const manager) :

    fMemoryManager(manager)
    , fPrefix(0)
    , fPrefixBufSz(0)
    , fLocalPart(0)
    , fLocalPartBufSz(0)
    , fRawName(0)
    , fRawNameBufSz(0)
    , fURIId(0)
{
    try
    {
        //
        //  Just call the local setters to set up everything. Too much
        //  work is required to replicate that functionality here.
        //
        setName(prefix, localPart, uriId);
    }
    catch(const OutOfMemoryException&)
    {
        throw;
    }
    catch(...)
    {
        cleanUp();
    }
}

QName::QName( const XMLCh* const rawName
            , const unsigned int uriId
            , MemoryManager* const manager) :

    fMemoryManager(manager)
    , fPrefix(0)
    , fPrefixBufSz(0)
    , fLocalPart(0)
    , fLocalPartBufSz(0)
    , fRawName(0)
    , fRawNameBufSz(0)
    , fURIId(0)
{
    try
    {
        //
        //  Just call the local setters to set up everything. Too much
        //  work is required to replicate that functionality here.
        //
        setName(rawName, uriId);
    }
    catch(const OutOfMemoryException&)
    {
        throw;
    }
    catch(...)
    {
        cleanUp();
    }
}

QName::~QName()
{
	cleanUp();
}

// ---------------------------------------------------------------------------
//  QName: Copy Constructors
// ---------------------------------------------------------------------------
QName::QName(const QName& qname) :

    fMemoryManager(qname.fMemoryManager)
    , fPrefix(0)
    , fPrefixBufSz(0)
    , fLocalPart(0)
    , fLocalPartBufSz(0)
    , fRawName(0)
    , fRawNameBufSz(0)
    , fURIId(0)
{
    unsigned int newLen;

    newLen = XMLString::stringLen(qname.getLocalPart());
    fLocalPartBufSz = newLen + 8;
    fLocalPart = (XMLCh*) fMemoryManager->allocate
    (
        (fLocalPartBufSz + 1) * sizeof(XMLCh)
    ); //new XMLCh[fLocalPartBufSz + 1];
    XMLString::moveChars(fLocalPart, qname.getLocalPart(), newLen + 1);

    newLen = XMLString::stringLen(qname.getPrefix());
    fPrefixBufSz = newLen + 8;
    fPrefix = (XMLCh*) fMemoryManager->allocate
    (
        (fPrefixBufSz + 1) * sizeof(XMLCh)
    ); //new XMLCh[fPrefixBufSz + 1];
    XMLString::moveChars(fPrefix, qname.getPrefix(), newLen + 1);

    fURIId = qname.getURI();
}

// ---------------------------------------------------------------------------
//  QName: Getter methods
// ---------------------------------------------------------------------------
const XMLCh* QName::getRawName() const
{
    //
    //  If there is no buffer, or if there is but we've not faulted in the
    //  value yet, then we have to do that now.
    //
    if (!fRawName || !*fRawName)
    {
        //
        //  If we have a prefix, then do the prefix:name version. Else, its
        //  just the name.
        //
        if (*fPrefix)
        {
            //
            //  Calculate the worst case size buffer we will need. We use the
            //  current high water marks of the prefix and name buffers, so it
            //  might be a little wasteful of memory but we don't have to do
            //  string len operations on the two strings.
            //
            const unsigned int neededLen = fPrefixBufSz + fLocalPartBufSz + 1;

            //
            //  If no buffer, or the current one is too small, then allocate one
            //  and get rid of any old one.
            //
            if (!fRawName || (neededLen > fRawNameBufSz))
            {
                fMemoryManager->deallocate(fRawName); //delete [] fRawName;

                // We have to cast off the const'ness to do this
                ((QName*)this)->fRawNameBufSz = neededLen;
                ((QName*)this)->fRawName = (XMLCh*) fMemoryManager->allocate
                (
                    (neededLen + 1) * sizeof(XMLCh)
                ); //new XMLCh[neededLen + 1];

                // Make sure its initially empty
                *fRawName = 0;
            }

            const unsigned int prefixLen = XMLString::stringLen(fPrefix);

            XMLString::moveChars(fRawName, fPrefix, prefixLen);
            fRawName[prefixLen] = chColon;
            XMLString::copyString(&fRawName[prefixLen+1], fLocalPart);
        }
         else
        {
            return fLocalPart;
        }
    }
    return fRawName;
}

XMLCh* QName::getRawName()
{
    //
    //  If there is no buffer, or if there is but we've not faulted in the
    //  value yet, then we have to do that now.
    //
    if (!fRawName || !*fRawName)
    {
        //
        //  If we have a prefix, then do the prefix:name version. Else, its
        //  just the name.
        //
        if (*fPrefix)
        {
            //
            //  Calculate the worst case size buffer we will need. We use the
            //  current high water marks of the prefix and name buffers, so it
            //  might be a little wasteful of memory but we don't have to do
            //  string len operations on the two strings.
            //
            const unsigned int neededLen = fPrefixBufSz + fLocalPartBufSz + 1;

            //
            //  If no buffer, or the current one is too small, then allocate one
            //  and get rid of any old one.
            //
            if (!fRawName || (neededLen > fRawNameBufSz))
            {
                fMemoryManager->deallocate(fRawName); //delete [] fRawName;

                // We have to cast off the const'ness to do this
                ((QName*)this)->fRawNameBufSz = neededLen;
                ((QName*)this)->fRawName = (XMLCh*) fMemoryManager->allocate
                (
                    (neededLen + 1) * sizeof(XMLCh)
                ); //new XMLCh[neededLen + 1];

                // Make sure its initially empty
                *fRawName = 0;
            }


            const unsigned int prefixLen = XMLString::stringLen(fPrefix);

            XMLString::moveChars(fRawName, fPrefix, prefixLen);
            fRawName[prefixLen] = chColon;
            XMLString::copyString(&fRawName[prefixLen+1], fLocalPart);
        }
         else
        {
            return fLocalPart;
        }
    }
    return fRawName;
}

// ---------------------------------------------------------------------------
//  QName: Setter methods
// ---------------------------------------------------------------------------
void QName::setName(const XMLCh* const    prefix
                  , const XMLCh* const    localPart
                  , const unsigned int    uriId)
{
    setPrefix(prefix);
    setLocalPart(localPart);

    // And clean up any QName and leave it undone until/if asked for again
    if (fRawName)
        *fRawName = 0;

    // And finally store the URI id parameter
    fURIId = uriId;
}

void QName::setName(const XMLCh* const    rawName
                  , const unsigned int    uriId)
{
    //set the rawName
    unsigned int newLen;

    newLen = XMLString::stringLen(rawName);
    if (!fRawNameBufSz || (newLen > fRawNameBufSz))
    {
        fMemoryManager->deallocate(fRawName); //delete [] fRawName;
        fRawNameBufSz = newLen + 8;
        fRawName = (XMLCh*) fMemoryManager->allocate
        (
            (fRawNameBufSz + 1) * sizeof(XMLCh)
        ); //new XMLCh[fRawNameBufSz + 1];
    }
    XMLString::moveChars(fRawName, rawName, newLen + 1);

    //find out the prefix and localPart from the rawName
    const int colonInd = XMLString::indexOf(rawName, chColon);
    if (colonInd >= 0)
    {
        setNPrefix(rawName, colonInd);
    }
     else
    {
        // No colon, so we just have a name with no prefix
        setPrefix(XMLUni::fgZeroLenString);
    }

    setNLocalPart(&rawName[colonInd+1], newLen-colonInd-1);


    // And finally store the URI id parameter
    fURIId = uriId;
}

void QName::setPrefix(const XMLCh* prefix)
{
    unsigned int newLen;

    newLen = XMLString::stringLen(prefix);
    if (!fPrefixBufSz || (newLen > fPrefixBufSz))
    {
        fMemoryManager->deallocate(fPrefix); //delete [] fPrefix;
        fPrefixBufSz = newLen + 8;
        fPrefix = (XMLCh*) fMemoryManager->allocate
        (
            (fPrefixBufSz + 1) * sizeof(XMLCh)
        ); //new XMLCh[fPrefixBufSz + 1];
    }
    XMLString::moveChars(fPrefix, prefix, newLen + 1);
}

void QName::setNPrefix(const XMLCh* prefix, const unsigned int newLen)
{
    if (!fPrefixBufSz || (newLen > fPrefixBufSz))
    {
        fMemoryManager->deallocate(fPrefix); //delete [] fPrefix;
        fPrefixBufSz = newLen + 8;
        fPrefix = (XMLCh*) fMemoryManager->allocate
        (
            (fPrefixBufSz + 1) * sizeof(XMLCh)
        ); //new XMLCh[fPrefixBufSz + 1];
    }
    XMLString::moveChars(fPrefix, prefix, newLen);
    fPrefix[newLen] = chNull;
}

void QName::setLocalPart(const XMLCh* localPart)
{
    unsigned int newLen;

    newLen = XMLString::stringLen(localPart);
    if (!fLocalPartBufSz || (newLen > fLocalPartBufSz))
    {
        fMemoryManager->deallocate(fLocalPart); //delete [] fLocalPart;
        fLocalPartBufSz = newLen + 8;
        fLocalPart = (XMLCh*) fMemoryManager->allocate
        (
            (fLocalPartBufSz + 1) * sizeof(XMLCh)
        ); //new XMLCh[fLocalPartBufSz + 1];
    }
    XMLString::moveChars(fLocalPart, localPart, newLen + 1);
}

void QName::setNLocalPart(const XMLCh* localPart, const unsigned int newLen)
{
    if (!fLocalPartBufSz || (newLen > fLocalPartBufSz))
    {
        fMemoryManager->deallocate(fLocalPart); //delete [] fLocalPart;
        fLocalPartBufSz = newLen + 8;
        fLocalPart = (XMLCh*) fMemoryManager->allocate
        (
            (fLocalPartBufSz + 1) * sizeof(XMLCh)
        ); //new XMLCh[fLocalPartBufSz + 1];
    }
    XMLString::moveChars(fLocalPart, localPart, newLen);
    fLocalPart[newLen] = chNull;
}

void QName::setValues(const QName& qname)
{
    setPrefix(qname.getPrefix());
    setLocalPart(qname.getLocalPart());
    setURI(qname.getURI());
}

// -----------------------------------------------------------------------
//  comparison
// -----------------------------------------------------------------------
bool QName::operator==(const QName& qname) const
{
    if (fURIId == 0) // null URI
        return (XMLString::equals(getRawName(),qname.getRawName()));

    return ((fURIId == qname.getURI()) &&
           (XMLString::equals(fLocalPart, qname.getLocalPart())));
}

// ---------------------------------------------------------------------------
//  QName: Private, helper methods
// ---------------------------------------------------------------------------
void QName::cleanUp()
{
    fMemoryManager->deallocate(fLocalPart); //delete [] fLocalPart;
    fMemoryManager->deallocate(fPrefix); //delete [] fPrefix;
    fMemoryManager->deallocate(fRawName); //delete [] fRawName;
    fLocalPart = fPrefix = fRawName = 0;
}

/***
 * Support for Serialization/De-serialization
 ***/

IMPL_XSERIALIZABLE_TOCREATE(QName)

void QName::serialize(XSerializeEngine& serEng)
{

    if (serEng.isStoring())
    {
        serEng.writeString(fPrefix, fPrefixBufSz, XSerializeEngine::toWriteBufferLen);

        serEng.writeString(fLocalPart, fLocalPartBufSz, XSerializeEngine::toWriteBufferLen);

        //do not serialize rawName

        serEng<<fURIId;
    }
    else
    {
        int dataLen = 0;

        serEng.readString(fPrefix, (int&)fPrefixBufSz, dataLen, XSerializeEngine::toReadBufferLen);

        serEng.readString(fLocalPart, (int&)fLocalPartBufSz, dataLen, XSerializeEngine::toReadBufferLen);

        //force raw name rebuilt
        fRawNameBufSz = 0;        
        fRawName = 0;

        serEng>>fURIId;
    }

}

XERCES_CPP_NAMESPACE_END
