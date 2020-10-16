//---------------------------------------------------------------------------
//    $Id: sparse_matrix.float.cc,v 1.1 2004/09/14 00:51:25 wolf Exp $
//    Version: $Name:  $
//
//    Copyright (C) 1998, 1999, 2000, 2001, 2002, 2003, 2004 by the deal.II authors
//
//    This file is subject to QPL and may not be  distributed
//    without copyright and license information. Please refer
//    to the file deal.II/doc/license.html for the  text  and
//    further information on this license.
//
//---------------------------------------------------------------------------


#include <lac/sparse_matrix.templates.h>
#include <lac/block_vector.h>

#define TYPEMAT float

template class SparseMatrix<TYPEMAT>;

#define TYPE2 float
#include "sparse_matrix_matrix.in.h"
#undef TYPE2

#define TYPE2 double
#include "sparse_matrix_matrix.in.h"
#undef TYPE2

#define TYPEVEC float
#include "sparse_matrix_vector.in.h"
#undef TYPEVEC

#define TYPEVEC double
#include "sparse_matrix_vector.in.h"
#undef TYPEVEC

				 // a prerelease of gcc3.0 fails to
				 // compile this due to long double
//  #undef TYPEVEC
//  #define TYPEVEC long double

//  #include <lac/sparse_matrix.2.templates>

#undef TYPEMAT
