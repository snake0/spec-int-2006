//------------------------------------------------------------------------
//    $Id: sparse_matrix_ez.double.cc,v 1.1 2004/09/14 00:51:25 wolf Exp $
//    Version: $Name:  $
//
//    Copyright (C) 2002, 2003 by the deal.II authors
//
//    This file is subject to QPL and may not be  distributed
//    without copyright and license information. Please refer
//    to the file deal.II/doc/license.html for the  text  and
//    further information on this license.
//
//------------------------------------------------------------------------


#include <base/logstream.h>
#include <lac/sparse_matrix_ez.templates.h>
#include <iostream>

#define TYPEMAT double

template class SparseMatrixEZ<TYPEMAT>;

#define TYPEVEC float
#include "sparse_matrix_ez_vector.in.h"
#undef TYPEVEC

#define TYPEVEC double
#include "sparse_matrix_ez_vector.in.h"
#undef TYPEVEC

				 // a prerelease of gcc3.0 fails to
				 // compile this due to long double
//  #undef TYPE2
//  #define TYPE2 long double

//  #include <lac/sparse_matrix.2.templates>

#undef TYPEMAT
