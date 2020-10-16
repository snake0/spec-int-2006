//---------------------------------------------------------------------------
//    $Id: filtered_matrix.in.h,v 1.1 2004/09/14 00:52:54 wolf Exp $
//    Version: $Name:  $
//
//    Copyright (C) 2001, 2002, 2003 by the deal.II authors
//
//    This file is subject to QPL and may not be  distributed
//    without copyright and license information. Please refer
//    to the file deal.II/doc/license.html for the  text  and
//    further information on this license.
//
//---------------------------------------------------------------------------


// File included by driver file fitered_matrix.?.cc
// There, TYPEMAT and TYPEVEC must be set to the number types


//TODO: Check if this cannot be implemented with iterators
//      and applied to SparseMatrixEZ

template <>
void
FilteredMatrix<SparseMatrix<TYPEMAT>,Vector<TYPEVEC> >::
get_column_entries (const unsigned int           index,
		    std::vector<IndexValuePair> &column_entries,
		    const bool                   matrix_is_symmetric) const
{
				   // depending on whether the matrix
				   // can be assumed symmetric or not,
				   // either use a fast or a slow
				   // algorithm
  if (matrix_is_symmetric == true)
				     // ok, matrix is symmetric. we
				     // may determine the matrix
				     // entries in this column by
				     // looking at the matrix entries
				     // in this row which is
				     // significantly faster since we
				     // can traverse them linearly and
				     // do not have to check each row
				     // for the possible existence of
				     // a matrix entry
    {
      const unsigned int *
	col_nums   = &(matrix->get_sparsity_pattern().get_column_numbers()
		       [matrix->get_sparsity_pattern().get_rowstart_indices()[index]]);
      const unsigned int
	row_length = matrix->get_sparsity_pattern().row_length(index);

      for (unsigned int i=0; i<row_length; ++i)
	{
	  const unsigned int c = *(col_nums+i);

					   // if not diagonal entry,
					   // add to list
	  if (c != index)
	    column_entries.push_back (std::make_pair(c, (*matrix)(c,index)));
	};
    }
  else
    {
				       // otherwise check each row for
				       // occurrence of an entry in
				       // this column
      for (unsigned int row=0; row<n(); ++row)
	if (row != index)
	  {
	    const unsigned int
	      global_index = matrix->get_sparsity_pattern()(row,index);
	    if (global_index != SparsityPattern::invalid_entry)
	      column_entries.push_back (std::make_pair(row,
						       (*matrix)(row,index)));
	  };
    };
}



template <>
void
FilteredMatrix<BlockSparseMatrix<TYPEMAT>,BlockVector<TYPEVEC> >::
get_column_entries (const unsigned int           /*index*/,
		    std::vector<IndexValuePair> &/*column_entries*/,
		    const bool                   /*matrix_is_symmetric*/) const
{
				   // presently not implemented, but
				   // should be fairly simple to do
  Assert (false, ExcNotImplemented());
}




template <>
void
FilteredMatrix<SparseMatrix<TYPEMAT>,Vector<TYPEVEC> >::
allocate_tmp_vector () 
{
  Threads::ThreadMutex::ScopedLock lock (tmp_mutex);
  tmp_vector.reinit (matrix->n(), true);
}



template <>
void
FilteredMatrix<BlockSparseMatrix<TYPEMAT>,BlockVector<TYPEVEC> >::
allocate_tmp_vector () 
{
  std::vector<unsigned int> block_sizes (matrix->n_block_rows());
  for (unsigned int i=0; i<block_sizes.size(); ++i)
    block_sizes[i] = matrix->block(i,i).n();
  
  Threads::ThreadMutex::ScopedLock lock (tmp_mutex);
  tmp_vector.reinit (block_sizes, true);
}
