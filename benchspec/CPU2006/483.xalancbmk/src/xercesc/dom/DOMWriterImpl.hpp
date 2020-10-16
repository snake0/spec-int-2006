/*
 * The Apache Software License, Version 1.1
 *
 * Copyright (c) 2002 The Apache Software Foundation.  All rights
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
 * $Id: DOMWriterImpl.hpp,v 1.18 2003/08/14 16:31:13 gareth Exp $
 * $Log: DOMWriterImpl.hpp,v $
 * Revision 1.18  2003/08/14 16:31:13  gareth
 * Method added to allow serilization of custom nodes from derived classes.
 *
 * Revision 1.17  2003/05/29 18:47:52  knoaman
 * Apply memory manager.
 *
 * Revision 1.16  2003/05/22 02:10:51  knoaman
 * Default the memory manager.
 *
 * Revision 1.15  2003/05/15 18:25:54  knoaman
 * Partial implementation of the configurable memory manager.
 *
 * Revision 1.14  2003/05/12 16:08:11  gareth
 * fix to #18832. Corrected serilization with regards to namespace nodes. Patch by Alby Massari.
 *
 * Revision 1.13  2003/03/16 05:42:04  peiyongz
 * Bug#17983 Formatter does not escape control characters
 *
 * Revision 1.12  2003/01/28 18:31:47  peiyongz
 * Bug#13694: Allow Xerces to write the BOM to XML files
 *
 * Revision 1.11  2003/01/20 16:50:13  tng
 * DOMWriter fix:
 * 1. wrong wrong nested cdata message
 * 2. pretty format the cdata section
 * 3. do not increment error count if warning was issued
 *
 * Revision 1.10  2002/12/10 21:01:32  tng
 * NLS: DOMWriter should use message loader to load message instead of using hardcoded static stirng
 *
 * Revision 1.9  2002/12/09 11:46:08  gareth
 * More pretty pretty print feature. Patch by Kevin King. Closes bug #13840.
 *
 * Revision 1.8  2002/11/04 15:07:35  tng
 * C++ Namespace Support.
 *
 * Revision 1.7  2002/06/25 16:17:16  tng
 * DOM L3: add release()
 *
 * Revision 1.6  2002/06/21 19:33:12  peiyongz
 * support for feature split_cdata_section and entities revised.
 *
 * Revision 1.5  2002/06/17 19:45:58  peiyongz
 * optimization on fFeatures and featureId introduced
 *
 * Revision 1.4  2002/06/14 15:39:02  peiyongz
 * Fix: Compilation error from ForteC on Solaris2.6
 *
 * Revision 1.3  2002/06/10 16:02:21  peiyongz
 * format-pretty-print partially supported
 * resolve encoding from DOMDocument Interface
 *
 * Revision 1.2  2002/06/05 16:03:03  peiyongz
 * delete[] used.
 *
 * Revision 1.1  2002/05/28 22:39:39  peiyongz
 * DOM3 Save Interface: DOMWriter/DOMWriterFilter
 *
 */

/**
 *  DOMWriterImpl provides an API for serializing (writing) a DOM document out in
 * an XML document. The XML data is written to an output stream, the type of
 * which depends on the specific language bindings in use. During
 * serialization of XML data, namespace fixup is done when possible.
 * <p> <code>DOMWriterImpl</code> accepts any node type for serialization. For
 * nodes of type <code>Document</code> or <code>Entity</code>, well formed
 * XML will be created if possible. The serialized output for these node
 * types is either as a Document or an External Entity, respectively, and is
 * acceptable input for an XML parser. For all other types of nodes the
 * serialized form is not specified, but should be something useful to a
 * human for debugging or diagnostic purposes. Note: rigorously designing an
 * external (source) form for stand-alone node types that don't already have
 * one defined in  seems a bit much to take on here.
 * <p>Within a Document or Entity being serialized, Nodes are processed as
 * follows Documents are written including an XML declaration and a DTD
 * subset, if one exists in the DOM. Writing a document node serializes the
 * entire document.  Entity nodes, when written directly by
 * <code>writeNode</code> defined in the <code>DOMWriterImpl</code> interface,
 * output the entity expansion but no namespace fixup is done. The resulting
 * output will be valid as an external entity.  Entity References nodes are
 * serializes as an entity reference of the form
 * <code>"&amp;entityName;"</code>) in the output. Child nodes (the
 * expansion) of the entity reference are ignored.  CDATA sections
 * containing content characters that can not be represented in the
 * specified output encoding are handled according to the
 * "split-cdata-sections" feature.If the feature is <code>true</code>, CDATA
 * sections are split, and the unrepresentable characters are serialized as
 * numeric character references in ordinary content. The exact position and
 * number of splits is not specified. If the feature is <code>false</code>,
 * unrepresentable characters in a CDATA section are reported as errors. The
 * error is not recoverable - there is no mechanism for supplying
 * alternative characters and continuing with the serialization. All other
 * node types (Element, Text, etc.) are serialized to their corresponding
 * XML source form.
 * <p> Within the character data of a document (outside of markup), any
 * characters that cannot be represented directly are replaced with
 * character references. Occurrences of '&lt;' and '&amp;' are replaced by
 * the predefined entities &amp;lt; and &amp;amp. The other predefined
 * entities (&amp;gt, &amp;apos, etc.) are not used; these characters can be
 * included directly. Any character that can not be represented directly in
 * the output character encoding is serialized as a numeric character
 * reference.
 * <p> Attributes not containing quotes are serialized in quotes. Attributes
 * containing quotes but no apostrophes are serialized in apostrophes
 * (single quotes). Attributes containing both forms of quotes are
 * serialized in quotes, with quotes within the value represented by the
 * predefined entity &amp;quot;. Any character that can not be represented
 * directly in the output character encoding is serialized as a numeric
 * character reference.
 * <p> Within markup, but outside of attributes, any occurrence of a character
 * that cannot be represented in the output character encoding is reported
 * as an error. An example would be serializing the element
 * &lt;LaCa�ada/&gt; with the encoding="us-ascii".
 * <p> When requested by setting the <code>normalize-characters</code> feature
 * on <code>DOMWriterImpl</code>, all data to be serialized, both markup and
 * character data, is W3C Text normalized according to the rules defined in
 * . The W3C Text normalization process affects only the data as it is being
 * written; it does not alter the DOM's view of the document after
 * serialization has completed.
 * <p>Namespaces are fixed up during serialization, the serialization process
 * will verify that namespace declarations, namespace prefixes and the
 * namespace URIs associated with Elements and Attributes are consistent. If
 * inconsistencies are found, the serialized form of the document will be
 * altered to remove them. The algorithm used for doing the namespace fixup
 * while seralizing a document is a combination of the algorithms used for
 * lookupNamespaceURI and lookupNamespacePrefix . previous paragraph to be
 * defined closer here.
 * <p>Any changes made affect only the namespace prefixes and declarations
 * appearing in the serialized data. The DOM's view of the document is not
 * altered by the serialization operation, and does not reflect any changes
 * made to namespace declarations or prefixes in the serialized output.
 * <p> While serializing a document the serializer will write out
 * non-specified values (such as attributes whose <code>specified</code> is
 * <code>false</code>) if the <code>output-default-values</code> feature is
 * set to <code>true</code>. If the <code>output-default-values</code> flag
 * is set to <code>false</code> and the <code>use-abstract-schema</code>
 * feature is set to <code>true</code> the abstract schema will be used to
 * determine if a value is specified or not, if
 * <code>use-abstract-schema</code> is not set the <code>specified</code>
 * flag on attribute nodes is used to determine if attribute values should
 * be written out.
 * <p> Ref to Core spec (1.1.9, XML namespaces, 5th paragraph) entity ref
 * description about warning about unbound entity refs. Entity refs are
 * always serialized as &amp;foo;, also mention this in the load part of
 * this spec.
 * <p> When serializing a document the DOMWriterImpl checks to see if the document
 * element in the document is a DOM Level 1 element or a DOM Level 2 (or
 * higher) element (this check is done by looking at the localName of the
 * root element). If the root element is a DOM Level 1 element then the
 * DOMWriterImpl will issue an error if a DOM Level 2 (or higher) element is
 * found while serializing. Likewise if the document element is a DOM Level
 * 2 (or higher) element and the DOMWriterImpl sees a DOM Level 1 element an
 * error is issued. Mixing DOM Level 1 elements with DOM Level 2 (or higher)
 * is not supported.
 * <p> <code>DOMWriterImpl</code>s have a number of named features that can be
 * queried or set. The name of <code>DOMWriterImpl</code> features must be valid
 * XML names. Implementation specific features (extensions) should choose an
 * implementation dependent prefix to avoid name collisions.
 * <p>Here is a list of properties that must be recognized by all
 * implementations.
 * <dl>
 * <dt><code>"normalize-characters"</code></dt>
 * <dd>
 * <dl>
 * <dt><code>true</code></dt>
 * <dd>[
 * optional] (default) Perform the W3C Text Normalization of the characters
 * in document as they are written out. Only the characters being written
 * are (potentially) altered. The DOM document itself is unchanged. </dd>
 * <dt>
 * <code>false</code></dt>
 * <dd>[required] do not perform character normalization. </dd>
 * </dl></dd>
 * <dt>
 * <code>"split-cdata-sections"</code></dt>
 * <dd>
 * <dl>
 * <dt><code>true</code></dt>
 * <dd>[required] (default)
 * Split CDATA sections containing the CDATA section termination marker
 * ']]&gt;' or characters that can not be represented in the output
 * encoding, and output the characters using numeric character references.
 * If a CDATA section is split a warning is issued. </dd>
 * <dt><code>false</code></dt>
 * <dd>[
 * required] Signal an error if a <code>CDATASection</code> contains an
 * unrepresentable character. </dd>
 * </dl></dd>
 * <dt><code>"validation"</code></dt>
 * <dd>
 * <dl>
 * <dt><code>true</code></dt>
 * <dd>[
 * optional] Use the abstract schema to validate the document as it is being
 * serialized. If validation errors are found the error handler is notified
 * about the error. Setting this state will also set the feature
 * <code>use-abstract-schema</code> to <code>true</code>. </dd>
 * <dt><code>false</code></dt>
 * <dd>[
 * required] (default) Don't validate the document as it is being
 * serialized. </dd>
 * </dl></dd>
 * <dt><code>"expand-entity-references"</code></dt>
 * <dd>
 * <dl>
 * <dt><code>true</code></dt>
 * <dd>[
 * optional] Expand <code>EntityReference</code> nodes when serializing. </dd>
 * <dt>
 * <code>false</code></dt>
 * <dd>[required] (default) Serialize all
 * <code>EntityReference</code> nodes as XML entity references. </dd>
 * </dl></dd>
 * <dt>
 * <code>"whitespace-in-element-content"</code></dt>
 * <dd>
 * <dl>
 * <dt><code>true</code></dt>
 * <dd>[required] (
 * default) Output all white spaces in the document. </dd>
 * <dt><code>false</code></dt>
 * <dd>[
 * optional] Only output white space that is not within element content. The
 * implementation is expected to use the
 * <code>isWhitespaceInElementContent</code> flag on <code>Text</code> nodes
 * to determine if a text node should be written out or not. </dd>
 * </dl></dd>
 * <dt>
 * <code>"discard-default-content"</code></dt>
 * <dd>
 * <dl>
 * <dt><code>true</code></dt>
 * <dd>[required] (default
 * ) Use whatever information available to the implementation (i.e. XML
 * schema, DTD, the <code>specified</code> flag on <code>Attr</code> nodes,
 * and so on) to decide what attributes and content should be serialized or
 * not. Note that the <code>specified</code> flag on <code>Attr</code> nodes
 * in itself is not always reliable, it is only reliable when it is set to
 * <code>false</code> since the only case where it can be set to
 * <code>false</code> is if the attribute was created by a Level 1
 * implementation. </dd>
 * <dt><code>false</code></dt>
 * <dd>[required] Output all attributes and
 * all content. </dd>
 * </dl></dd>
 * <dt><code>"format-canonical"</code></dt>
 * <dd>
 * <dl>
 * <dt><code>true</code></dt>
 * <dd>[optional]
 * This formatting writes the document according to the rules specified in .
 * Setting this feature to true will set the feature "format-pretty-print"
 * to false. </dd>
 * <dt><code>false</code></dt>
 * <dd>[required] (default) Don't canonicalize the
 * output. </dd>
 * </dl></dd>
 * <dt><code>"format-pretty-print"</code></dt>
 * <dd>
 * <dl>
 * <dt><code>true</code></dt>
 * <dd>[optional]
 * Formatting the output by adding whitespace to produce a pretty-printed,
 * indented, human-readable form. The exact form of the transformations is
 * not specified by this specification. Setting this feature to true will
 * set the feature "format-canonical" to false. </dd>
 * <dt><code>false</code></dt>
 * <dd>[required]
 * (default) Don't pretty-print the result. </dd>
 * </dl></dd>
 * </dl>
 * <p>See also the <a href='http://www.w3.org/TR/2001/WD-DOM-Level-3-ASLS-20011025'>Document Object Model (DOM) Level 3 Abstract Schemas and Load
 * and Save Specification</a>.
 */

#ifndef DOMWriterImpl_HEADER_GUARD_
#define DOMWriterImpl_HEADER_GUARD_

#include <xercesc/dom/DOM.hpp>
#include <xercesc/dom/DOMWriter.hpp>
#include <xercesc/util/XMLDOMMsg.hpp>
#include <xercesc/util/RefHashTableOf.hpp>
#include <xercesc/util/RefVectorOf.hpp>

XERCES_CPP_NAMESPACE_BEGIN


class CDOM_EXPORT DOMWriterImpl:public XMemory,
                                public DOMWriter {

public:

    /** @name Constructor and Destructor */
    //@{

    /**
     * Constructor.
     */
    DOMWriterImpl(MemoryManager* const manager = XMLPlatformUtils::fgMemoryManager);

    /**
     * Destructor.
     */
    ~DOMWriterImpl();
    //@}

    /** @name Inplementation of Abstract interface */

    virtual bool               canSetFeature(const XMLCh* const featName
                                           , bool               state) const;

    virtual void               setFeature(const XMLCh* const featName
                                        , bool               state);
    virtual bool               getFeature(const XMLCh* const featName) const;

    virtual void               setEncoding(const XMLCh* const encoding);
    virtual const XMLCh*       getEncoding() const;

    virtual void               setNewLine(const XMLCh* const newLine);
    virtual const XMLCh*       getNewLine() const;

    virtual void               setErrorHandler(DOMErrorHandler *errorHandler);
    virtual DOMErrorHandler*   getErrorHandler() const;

    virtual void               setFilter(DOMWriterFilter *filter);
    virtual DOMWriterFilter*   getFilter() const;

    virtual bool               writeNode(XMLFormatTarget* const destination
                                       , const DOMNode         &nodeToWrite);
    virtual void               release();

    /**
	  *  The caller is responsible for the release of the returned string
	  */

    virtual XMLCh*             writeToString(const DOMNode &nodeToWrite);
    //@}

private:

    /** unimplemented copy ctor and assignment operator */
    DOMWriterImpl(const DOMWriterImpl&);
    DOMWriterImpl & operator = (const DOMWriterImpl&);

    /** helper **/
    void                          initSession(const DOMNode* const);
    void                          processNode(const DOMNode* const);

    void                          procCdataSection(const XMLCh*   const nodeValue
                                                 , const DOMNode* const nodeToWrite
                                                 , int level);

    void                          procUnrepCharInCdataSection(const XMLCh*   const nodeValue
                                                            , const DOMNode* const nodeToWrite
                                                            , int level);

protected:
    /**
     * Overidden by derived classes to extend the abilities of the standard writer
     * always returns false in the default implementation
     * @return true if the method deals with nodeToWrite
     */
    virtual bool                          customNodeSerialize(const DOMNode* const nodeToWrite, int level);

    DOMNodeFilter::FilterAction   checkFilter(const DOMNode* const) const;

    bool                          checkFeature(const XMLCh* const featName
                                             , bool               state
                                             , int&               featureId) const;

    bool                          reportError(const DOMNode* const    errorNode
                                            , DOMError::ErrorSeverity errorType
                                            , const XMLCh*   const    errorMsg);
    bool                          reportError(const DOMNode* const    errorNode
                                            , DOMError::ErrorSeverity errorType
                                            , XMLDOMMsg::Codes        toEmit);

    bool                          canSetFeature(const int featureId
                                              , bool      val)     const;
    void                          setFeature(const int featureId
                                           , bool      val);
    bool                          getFeature(const int featureId) const;

    void                          printNewLine();
    void                          setURCharRef();


    void printIndent(int level) const;
    //does the actual work for processNode while keeping track of the level
    void processNode(const DOMNode* const nodeToWrite, int level);

    void processBOM();

    // -----------------------------------------------------------------------
    //  Private data members
    //
    //  fFeatures
    //
    //  fEncoding
    //      own it
    //
    //  fNewLine
    //      own it
    //
    //  fErrorHandler
    //      don't own it
    //
    //  fFilter
    //      don't own it
    //
    //  fDocumentVersion
    //      The XML Version of the document to be serialized.
    // 
    //  fEncodingUsed (session var)
    //      the actual encoding used in WriteNode(),
    //      it does not own any data(memory).
    //
    //  fNewLineUsed (session var)
    //      the actual "end of line" sequence used in WriteNode(),
    //      it does not own any data(memory).
    //
    //  fFormatter (session var)
    //      the formatter used in WriteNode()
    //
    //  fErrorCount
    //      the count of error encountered in the serialization,
    //      which neither the error handler, nor the serializer itself,
    //      treat as fatal. And the serializer will return true/false
    //      based on this value.
    //
    //  fCurrentLine
    //      the current line. Used to track the line number the current
    //      node begins on
    //
    // -----------------------------------------------------------------------

    int                           fFeatures;
    XMLCh                        *fEncoding;
    XMLCh                        *fNewLine;
    DOMErrorHandler              *fErrorHandler;
    DOMWriterFilter              *fFilter;
    const XMLCh                  *fDocumentVersion;

    //session vars
    const XMLCh                  *fEncodingUsed;
    const XMLCh                  *fNewLineUsed;
    XMLFormatter                 *fFormatter;
    int                           fErrorCount;
    int                           fCurrentLine;

    RefVectorOf< RefHashTableOf<XMLCh> >* fNamespaceStack;
    MemoryManager*               fMemoryManager;
};

inline void DOMWriterImpl::setFeature(const int featureId
                                    , bool      val)
{
    (val)? fFeatures |= (1<<featureId) : fFeatures &= ~(1<<featureId);
}   /* SPEC_CPU: removed extra ';' for C++98 standards compliance -- yag */

inline bool DOMWriterImpl::getFeature(const int featureId) const
{
    return ((fFeatures & ( 1<<featureId )) != 0) ? true : false;
}   /* SPEC_CPU: removed extra ';' for C++98 standards compliance -- yag */

inline void DOMWriterImpl::setURCharRef()
{
    fFormatter->setUnRepFlags(XMLFormatter::UnRep_CharRef);
}

XERCES_CPP_NAMESPACE_END

#endif
