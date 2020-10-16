//=========================================================================
//
//  CENUM.CC - part of
//                          OMNeT++
//           Discrete System Simulation in C++
//
//   Member functions of
//    cEnum : effective integer-to-string mapping
//
//  Author: Andras Varga
//
//=========================================================================

/*--------------------------------------------------------------*
  Copyright (C) 1992-2003 Andras Varga

  This file is distributed WITHOUT ANY WARRANTY. See the file
  `license' for details on this and other legal matters.
*--------------------------------------------------------------*/

#include <stdio.h>           // sprintf, fprintf
#include <string.h>          // memcmp, memcpy, memset
#include "macros.h"
#include "ctypes.h"
#include "csimul.h"
#include "cenum.h"
#include "cexception.h"

//=== Registration
Register_Class(cEnum);

//==========================================================================
//=== cEnum - member functions

cEnum::cEnum(const cEnum& list) : cObject()
{
     vect=NULL;
     size=0;
     setName( list.name() );
     operator=(list);
}

cEnum::cEnum(const char *name, int siz) : cObject(name)
{
    size = Max(siz,0);
    items = 0;
    vect = new sEnum[size];
    for (int i=0; i<size; i++)
	vect[i].string=NULL;
}

cEnum::~cEnum()
{
    for (int i=0; i<size; i++)
	delete [] vect[i].string;
    delete [] vect;
}

cEnum& cEnum::operator=(const cEnum& list)
{
    int i;
    for (i=0; i<size; i++)
	delete [] vect[i].string;
    delete [] vect;

    cObject::operator=( list );

    size = list.size;
    items = list.items;
    vect = new sEnum[size];
    if (vect) memcpy( vect, list.vect, size * sizeof(sEnum *) );

    for (i=0; i<size; i++)
	 if (vect[i].string)
	     vect[i].string = opp_strdup(vect[i].string);
    return *this;
}

void cEnum::info(char *buf)
{
    cObject::info( buf );

    if (items==0)
        sprintf( buf+strlen(buf), " (empty)" );
    else
        sprintf( buf+strlen(buf), " (n=%d)", items);
}

void cEnum::insert(int key, const char *str)
{
    // if hashtable gets crowded, re-hash into bigger table
    if (items > 2*size/3)
    {
        // remember old table
        sEnum *oldvect = vect;
        int oldsize = size;

        // choose new size and allocate table
        static int sizes[] = {8,16,32,64,128,256,512,2048, 4096,8192,16384,32768,65536,0};
	int i;
	for (i=0; size >= sizes[i] && sizes[i]; i++);
        size=sizes[i];

	vect = new sEnum[size];
	for (i=0; i<size; i++)
	    vect[i].string=NULL;

        // copy over table contents
        for (i=0; i<oldsize; i++)
        {
            // find a slot...
            int key = oldvect[i].key;
            int k = (key<0 ? -key : key) % size;
	    while (vect[k].string)
		k = (k+1)%size;

	    // ...and insert there
	    vect[k].key = key;
	    vect[k].string = oldvect[i].string;
	}
	delete [] oldvect;
    }

    // find a slot...
    int k = (key<0 ? -key : key) % size;
    while (vect[k].string && vect[k].key!=key)
	k = (k+1)%size;

    // consistency check
#if defined(SPEC_CPU)
    // jray: conditional modified to fix a (definitely harmless) Insure message
    if (vect[k].string && vect[k].key == key && strcmp(vect[k].string, str))
#else
    if (vect[k].key == key && vect[k].string && strcmp(vect[k].string, str))
#endif
    {
        // oops! same keys but different strings!
        throw new cException("Key mismatch for enum %s: %s and %s have the same value (%d)",
                name(), vect[k].string, str, key);
    }
#if defined(SPEC_CPU)
    // jray: conditional modified to fix a (potentially harmful) Insure message
    else if (!(vect[k].string) || (vect[k].key != key))
#else
    else if (vect[k].key != key)
#endif
    {
        // ...and insert there
        vect[k].key = key;
        vect[k].string = opp_strdup(str);
        items++;
    }
}

const char *cEnum::stringFor(int key)
{
    int k = (key<0 ? -key : key) % size;
    while (vect[k].key!=key && vect[k].string)
	k = (k+1)%size;
    return vect[k].string;
}

int cEnum::lookup(const char *str, int fallback)
{
    for (int i=0; i<size; i++)
	if (vect[i].string && 0==strcmp(vect[i].string,str))
	    return vect[i].key;
    return fallback;
}

//==========================================================================
//=== sEnumBuilder - member functions

sEnumBuilder::sEnumBuilder(const char *name, ...)
{
    cEnum *e = findEnum(name);
    if (!e)
    {
	e = new cEnum(name);
        e->setOwner(&enums);
    }

    va_list va;
    va_start(va,name);
    for(;;)
    {
	int key = va_arg(va,int);
	const char *str = va_arg(va,const char *);
	if (!str)
	    break;
	e->insert(key,str);
    }
}


