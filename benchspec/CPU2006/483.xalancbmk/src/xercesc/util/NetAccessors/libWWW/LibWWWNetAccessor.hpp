/*
 * The Apache Software License, Version 1.1
 *
 * Copyright (c) 1999-2000 The Apache Software Foundation.  All rights
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
 * $Log: LibWWWNetAccessor.hpp,v $
 * Revision 1.3  2003/03/07 18:15:50  tng
 * Return a reference instead of void for operator=
 *
 * Revision 1.2  2002/11/04 15:11:39  tng
 * C++ Namespace Support.
 *
 * Revision 1.1.1.1  2002/02/01 22:22:23  peiyongz
 * sane_include
 *
 * Revision 1.2  2001/03/02 14:39:27  tng
 * Enabling libWWW NetAccessor support under UNIX. Tested with latest tarball of libWWW
 * (w3c-libwww-5.3.2) under RedHat Linux 6.1.  Added by Martin Kalen.
 *
 * There is one MAJOR problem with the use of libwww and the patches
 * below, which someone with knowledge of libwww filters etc. might want
 * to look into. Default behavior for content-type text/xml is to consume
 * all xml data before it reaches the simple HTML presenter. Hence, only
 * files with content-type text/html will actually reach the xerces-c
 * library. If you have a *.xml file on the webbserver, processing of the
 * file will throw an exception stating "The main XML document cannot be
 * empty" (correct in a xerces point of view since if you enable debug
 * build you will see that libwww "eats" all text/xml).
 *
 * See "Diffs for enabling libWWW NetAccessor support under UNIX" posted in March 1, 2001
 * in the xerces-c-dev mailing list for further information.
 *
 * Revision 1.1  2000/02/17 22:06:19  rahulj
 * Moved the four LibWWW files to its own sub-directory in the
 * NetAccessor directory.
 *
 *
 * Revision 1.1  2000/01/15 01:08:04  rahulj
 * Added support for HTTP to the parser.
 * Error handling is not very good. Also cannot guarantee that
 * there are no memory leaks.
 * Only tested under NT 4.0 SP 5 using libWWW 5.2.8.
 *
 */


#if !defined(LIBWWWNETACCESSOR_HPP)
#define LIBWWWNETACCESSOR_HPP


#include <xercesc/util/XercesDefs.hpp>
#include <xercesc/util/XMLURL.hpp>
#include <xercesc/util/BinInputStream.hpp>
#include <xercesc/util/XMLNetAccessor.hpp>

XERCES_CPP_NAMESPACE_BEGIN

//
// This class is the wrapper for the libWWW library which provides
// support for HTTP and other network protocols, so that URL's using
// these protocols can be used in system id's in the XML decl clauses.
//

class XMLUTIL_EXPORT LibWWWNetAccessor : public XMLNetAccessor
{
public :
    LibWWWNetAccessor();
    ~LibWWWNetAccessor();

    BinInputStream* makeNew(const XMLURL&  urlSource);
    const XMLCh* getId() const;

private :
    static const XMLCh fgMyName[];

    LibWWWNetAccessor(const LibWWWNetAccessor&);
    LibWWWNetAccessor& operator=(const LibWWWNetAccessor&);

}; // LibWWWNetAccessor

inline const XMLCh* LibWWWNetAccessor::getId() const
{
    return fgMyName;
}

XERCES_CPP_NAMESPACE_END

#endif // LIBWWWNETACCESSOR_HPP
