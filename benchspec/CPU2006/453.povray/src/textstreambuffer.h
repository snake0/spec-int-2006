/****************************************************************************
 *               textstreambuffer.h
 *
 * This module contains all defines, typedefs, and prototypes for the
 * C++ interface version of textstreambuffer.cpp.
 *
 * from Persistence of Vision(tm) Ray Tracer version 3.6.
 * Copyright 1991-2003 Persistence of Vision Team
 * Copyright 2003-2004 Persistence of Vision Raytracer Pty. Ltd.
 *---------------------------------------------------------------------------
 * NOTICE: This source code file is provided so that users may experiment
 * with enhancements to POV-Ray and to port the software to platforms other
 * than those supported by the POV-Ray developers. There are strict rules
 * regarding how you are permitted to use this file. These rules are contained
 * in the distribution and derivative versions licenses which should have been
 * provided with this file.
 *
 * These licences may be found online, linked from the end-user license
 * agreement that is located at http://www.povray.org/povlegal.html
 *---------------------------------------------------------------------------
 * This program is based on the popular DKB raytracer version 2.12.
 * DKBTrace was originally written by David K. Buck.
 * DKBTrace Ver 2.0-2.12 were written by David K. Buck & Aaron A. Collins.
 *---------------------------------------------------------------------------
 * $File: //depot/povray/3.5/source/base/textstreambuffer.h $
 * $Revision: #7 $
 * $Change: 2929 $
 * $DateTime: 2004/07/01 14:01:40 $
 * $Author: calimet $
 * $Log$
 *****************************************************************************/

#ifndef TEXTSTREAMBUFFER_H
#define TEXTSTREAMBUFFER_H

#include <cstdarg>
#include <cstdio>
#include <cctype>

// must nuke these since everyone's favourite monopoly's cstdio still defines
// them for some reason (why not just use inlines like everyone else?)
#undef  getc
#undef  putc
#undef  getchar
#undef  putchar

#include "configbase.h"

BEGIN_POV_BASE_NAMESPACE

class TextStreamBuffer
{
	public:
		TextStreamBuffer(size_t buffersize = 1024*8, unsigned int wrapwidth = 80);
		virtual ~TextStreamBuffer();

		void printf(const char *format, ...);
		void print(const char *str);
		void puts(const char *str);
		void putc(int chr);
		void printfile(const char *filename, unsigned long offset, int lines);
		void printfile(FILE *file, int lines);
		void flush();
	protected:
		virtual void lineoutput(const char *str, unsigned int chars);
		virtual void directoutput(const char *str, unsigned int chars);
		virtual void rawoutput(const char *str, unsigned int chars);
	private:
		char *buffer;
		unsigned int boffset;
		unsigned int bsize;
		unsigned int wrap;
		unsigned int curline;

		void lineflush();
		void directflush(const char *str, unsigned int chars);
};

END_POV_BASE_NAMESPACE

#endif
