//----------------------------------------------------------------------------
//    $Id: filtered_matrix.templates.h,v 1.1 2004/09/14 00:53:34 wolf Exp $
//    Version: $Name:  $
//
//    Copyright (C) 1998, 1999, 2000, 2001, 2002, 2003 by the deal.II authors
//
//    This file is subject to QPL and may not be  distributed
//    without copyright and license information. Please refer
//    to the file deal.II/doc/license.html for the  text  and
//    further information on this license.
//
//----------------------------------------------------------------------------
#ifndef __deal2__filtered_matrix_templates_h
#define __deal2__filtered_matrix_templates_h


#include <base/config.h>
#include <base/memory_consumption.h>
#include <lac/filtered_matrix.h>
#include <lac/sparse_matrix.h>
#include <lac/block_sparse_matrix.h>
#include <lac/vector.h>
#include <lac/block_vector.h>


template <class MATRIX, class VECTOR>
FilteredMatrix<MATRIX,VECTOR>::FilteredMatrix ()
{}



template <class MATRIX, class VECTOR>
FilteredMatrix<MATRIX,VECTOR>::
FilteredMatrix (const FilteredMatrix &fm)
		:
		Subscriptor (),
		constraints (fm.constraints)
{
  set_referenced_matrix (*fm.matrix);
}



template <class MATRIX, class VECTOR>
FilteredMatrix<MATRIX,VECTOR>::
FilteredMatrix (const MATRIX &m)
{
  set_referenced_matrix (m);
}



template <class MATRIX, class VECTOR>
FilteredMatrix<MATRIX,VECTOR> &
FilteredMatrix<MATRIX,VECTOR>::operator = (const FilteredMatrix &fm)
{
  set_referenced_matrix (*fm.matrix);
  constraints = fm.constraints;
  return *this;
}



template <class MATRIX, class VECTOR>
void
FilteredMatrix<MATRIX,VECTOR>::
set_referenced_matrix (const MATRIX &m)
{
  matrix = &m;
  allocate_tmp_vector ();
}



template <class MATRIX, class VECTOR>
void
FilteredMatrix<MATRIX,VECTOR>::clear_constraints ()
{
				   // swap vectors to release memory
  std::vector<IndexValuePair> empty;
  constraints.swap (empty);
}



template <class MATRIX, class VECTOR>
void
FilteredMatrix<MATRIX,VECTOR>::
apply_constraints (VECTOR     &v,
		   const bool  matrix_is_symmetric) const
{
				   // array that will hold the pairs
				   // of index/value of all nonzero
				   // entries in a given column
  std::vector<IndexValuePair> column_entries;
  
				   // iterate over all constraints and
				   // treat them one after the other
  const_index_value_iterator       i = constraints.begin();
  const const_index_value_iterator e = constraints.end();
  for (; i!=e; ++i)
    {
				       // define abbreviations
      const unsigned   index = i->first;
      const value_type value = i->second;
      
				       // check whether the value is
				       // zero, since in that case we do
				       // not have to modify other nodes
      if (value != 0)
	{
					   // first clear array of
					   // previous content
	  column_entries.clear ();
	  
					   // then get all entries in
					   // the present column
	  get_column_entries (index, column_entries, matrix_is_symmetric);
	  
					   // modify rhs for each entry
	  const_index_value_iterator       col     = column_entries.begin();
	  const const_index_value_iterator col_end = column_entries.end();
	  for (; col!=col_end; ++col)
	    v(col->first) -= col->second * value;
	};
    };

  
				       // finally set constrained
				       // entries themselves. we can't
				       // do it in the above loop
				       // since we might end up
				       // modifying an entry that we
				       // have already set if
				       // constrained dofs couple to
				       // each other
  for (i=constraints.begin(); i!=e; ++i)
    v(i->first) = i->second;
}



template <class MATRIX, class VECTOR>
void
FilteredMatrix<MATRIX,VECTOR>::pre_filter (VECTOR &v) const
{
				   // iterate over all constraints and
				   // zero out value
  const_index_value_iterator       i = constraints.begin();
  const const_index_value_iterator e = constraints.end();
  for (; i!=e; ++i)
    v(i->first) = 0;
}



template <class MATRIX, class VECTOR>
void
FilteredMatrix<MATRIX,VECTOR>::post_filter (const VECTOR &in,
					    VECTOR       &out) const
{
				   // iterate over all constraints and
				   // set value correctly
  const_index_value_iterator       i = constraints.begin();
  const const_index_value_iterator e = constraints.end();
  for (; i!=e; ++i)
    out(i->first) = in(i->first);
}



template <class MATRIX, class VECTOR>
void
FilteredMatrix<MATRIX,VECTOR>::vmult (VECTOR       &dst,
				      const VECTOR &src) const
{
  tmp_mutex.acquire ();
				   // first copy over src vector and
				   // pre-filter
  tmp_vector = src;
  pre_filter (tmp_vector);
				   // then let matrix do its work
  matrix->vmult (dst, tmp_vector);
				   // tmp_vector now no more needed
  tmp_mutex.release ();
				   // finally do post-filtering
  post_filter (src, dst);
}



template <class MATRIX, class VECTOR>
typename FilteredMatrix<MATRIX,VECTOR>::value_type
FilteredMatrix<MATRIX,VECTOR>::residual (VECTOR       &dst,
					 const VECTOR &x,
					 const VECTOR &b) const
{
  tmp_mutex.acquire ();
				   // first copy over x vector and
				   // pre-filter
  tmp_vector = x;
  pre_filter (tmp_vector);
				   // then let matrix do its work
  value_type res  = matrix->residual (dst, tmp_vector, b);
  value_type res2 = res*res;
				   // tmp_vector now no more needed
  tmp_mutex.release ();
				   // finally do post-filtering. here,
				   // we set constrained indices to
				   // zero, but have to subtract their
				   // contributions to the residual
  const_index_value_iterator       i = constraints.begin();
  const const_index_value_iterator e = constraints.end();
  for (; i!=e; ++i)
    {
      const value_type v = dst(i->first);
      res2 -= v*v;
      dst(i->first) = 0;
    };
  
  Assert (res2>=0, ExcInternalError());
  return std::sqrt (res2);
}



template <class MATRIX, class VECTOR>
void
FilteredMatrix<MATRIX,VECTOR>::Tvmult (VECTOR       &dst,
				       const VECTOR &src) const
{
  tmp_mutex.acquire ();
				   // first copy over src vector and
				   // pre-filter
  tmp_vector = src;
  pre_filter (tmp_vector);
				   // then let matrix do its work
  matrix->Tvmult (dst, tmp_vector);
				   // tmp_vector now no more needed
  tmp_mutex.release ();
				   // finally do post-filtering
  post_filter (src, dst);
}

  

template <class MATRIX, class VECTOR>
typename FilteredMatrix<MATRIX,VECTOR>::value_type
FilteredMatrix<MATRIX,VECTOR>::matrix_norm_square (const VECTOR &v) const
{
  tmp_mutex.acquire ();
  tmp_vector = v;

				   // zero out constrained entries and
				   // form matrix norm with original
				   // matrix. this is equivalent to
				   // forming the matrix norm of the
				   // original vector with the matrix
				   // where we have zeroed out rows
				   // and columns
  pre_filter (tmp_vector);
  const value_type ret = matrix->matrix_norm_square (tmp_vector);
  tmp_mutex.release ();
  return ret;
}



template <class MATRIX, class VECTOR>
void
FilteredMatrix<MATRIX,VECTOR>::
precondition_Jacobi (VECTOR           &dst,
		     const VECTOR     &src,
		     const value_type  omega) const
{
				   // first precondition as usual,
				   // using the fast algorithms of the
				   // matrix class
  matrix->precondition_Jacobi (dst, src, omega);

				   // then modify the constrained
				   // degree of freedom. as the
				   // diagonal entries of the filtered
				   // matrix would be 1.0, simply copy
				   // over old and new values
  const_index_value_iterator       i = constraints.begin();
  const const_index_value_iterator e = constraints.end();
  for (; i!=e; ++i)
    dst(i->first) = src(i->first);
}



template <class MATRIX, class VECTOR>
unsigned int
FilteredMatrix<MATRIX,VECTOR>::memory_consumption () const
{
  return (MemoryConsumption::memory_consumption (matrix) +
	  MemoryConsumption::memory_consumption (constraints) +
	  MemoryConsumption::memory_consumption (tmp_vector));
}



#endif
