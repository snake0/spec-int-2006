//----------------------------  function.cc  ---------------------------
//    $Id: memory_consumption.cc,v 1.1 2004/09/14 00:51:25 wolf Exp $
//    Version: $Name:  $
//
//    Copyright (C) 1998, 1999, 2000, 2001, 2002, 2003 by the deal.II authors
//
//    This file is subject to QPL and may not be  distributed
//    without copyright and license information. Please refer
//    to the file deal.II/doc/license.html for the  text  and
//    further information on this license.
//
//----------------------------  function.cc  ---------------------------


#include <base/memory_consumption.h>


namespace MemoryConsumption 
{
  unsigned int memory_consumption (const std::vector<std::string> &v)
  {
    unsigned int mem = sizeof(v);
    for (unsigned int i=0; i<v.size(); ++i)
      mem += memory_consumption(v[i]);
    return mem;
  }
  

}
