//----------------------------  petsc_sparse_matrix.h  ---------------------------
//    $Id: petsc_sparse_matrix.h,v 1.1 2004/09/14 00:53:34 wolf Exp $
//    Version: $Name:  $
//
//    Copyright (C) 2004 by the deal.II authors
//
//    This file is subject to QPL and may not be  distributed
//    without copyright and license information. Please refer
//    to the file deal.II/doc/license.html for the  text  and
//    further information on this license.
//
//----------------------------  petsc_sparse_matrix.h  ---------------------------
#ifndef __deal2__petsc_sparse_matrix_h
#define __deal2__petsc_sparse_matrix_h


#include <base/config.h>
#include <base/exceptions.h>

#ifdef DEAL_II_USE_PETSC

#include <lac/petsc_matrix_base.h>

#include <vector>

namespace PETScWrappers
{
/**
 * Implementation of a sequential sparse matrix class based on PETSC. All the
 * functionality is actually in the base class, except for the calls to
 * generate a sequential sparse matrix. This is possible since PETSc only works
 * on an abstract matrix type and internally distributes to functions that do
 * the actual work depending on the actual matrix type (much like using
 * virtual functions). Only the functions creating a matrix of specific type
 * differ, and are implemented in this particular class.
 *
 * @ingroup PETScWrappers
 * @author Wolfgang Bangerth, 2004
 */
  class SparseMatrix : public MatrixBase
  {
    public:
                                       /**
                                        * Default constructor. Create an empty
                                        * matrix.
                                        */
      SparseMatrix ();
      
                                       /**
                                        * Create a sparse matrix of dimensions
                                        * @p m times @p n, with an
                                        * initial guess of
                                        * @p n_nonzero_per_row nonzero
                                        * elements per row. PETSc is able to
                                        * cope with the situation that more
                                        * than this number of elements is
                                        * later allocated for a row, but this
                                        * involves copying data, and is thus
                                        * expensive.
                                        *
                                        * The @p is_symmetric flag determines
                                        * whether we should tell PETSc that
                                        * the matrix is going to be symmetric
                                        * (as indicated by the call
                                        * <tt>MatSetOption(mat,
                                        * MAT_SYMMETRIC)</tt>. Note that the
                                        * PETSc documentation states that one
                                        * cannot form an ILU decomposition of
                                        * a matrix for which this flag has
                                        * been set to @p true, only an
                                        * ICC. The default value of this flag
                                        * is @p false.
                                        */
      SparseMatrix (const unsigned int m,
                    const unsigned int n,
                    const unsigned int n_nonzero_per_row,
                    const bool         is_symmetric = false);

                                       /**
                                        * Initialize a rectangular matrix with
                                        * @p m rows and @p n
                                        * columns.  The maximal number of
                                        * nonzero entries for each row
                                        * separately is given by the
                                        * @p row_lengths array.
                                        *
                                        * Just as for the other constructors:
                                        * PETSc is able to cope with the
                                        * situation that more than this number
                                        * of elements is later allocated for a
                                        * row, but this involves copying data,
                                        * and is thus expensive.
                                        *
                                        * The @p is_symmetric flag determines
                                        * whether we should tell PETSc that
                                        * the matrix is going to be symmetric
                                        * (as indicated by the call
                                        * <tt>MatSetOption(mat,
                                        * MAT_SYMMETRIC)</tt>. Note that the
                                        * PETSc documentation states that one
                                        * cannot form an ILU decomposition of
                                        * a matrix for which this flag has
                                        * been set to @p true, only an
                                        * ICC. The default value of this flag
                                        * is @p false.
                                        */
      SparseMatrix (const unsigned int               m,
                    const unsigned int               n,
                    const std::vector<unsigned int> &row_lengths,
                    const bool                       is_symmetric = false);

                                       /**
                                        * This operator assigns a scalar to
                                        * a matrix. Since this does usually
                                        * not make much sense (should we set
                                        * all matrix entries to this value?
                                        * Only the nonzero entries of the
                                        * sparsity pattern?), this operation
                                        * is only allowed if the actual
                                        * value to be assigned is zero. This
                                        * operator only exists to allow for
                                        * the obvious notation
                                        * <tt>matrix=0</tt>, which sets all
                                        * elements of the matrix to zero,
                                        * but keep the sparsity pattern
                                        * previously used.
                                        */
      SparseMatrix & operator = (const double d);

                                       /**
                                        * Throw away the present matrix and
                                        * generate one that has the same
                                        * properties as if it were created by
                                        * the constructor of this class with
                                        * the same argument list as the
                                        * present function.
                                        */
      void reinit (const unsigned int m,
                   const unsigned int n,
                   const unsigned int n_nonzero_per_row,
                   const bool         is_symmetric = false);

                                       /**
                                        * Throw away the present matrix and
                                        * generate one that has the same
                                        * properties as if it were created by
                                        * the constructor of this class with
                                        * the same argument list as the
                                        * present function.
                                        */
      void reinit (const unsigned int               m,
                   const unsigned int               n,
                   const std::vector<unsigned int> &row_lengths,
                   const bool                       is_symmetric = false);

    private:

                                       /**
                                        * Do the actual work for the
                                        * respective reinit() function and the
                                        * matching constructor, i.e. create a
                                        * matrix. Getting rid of the previous
                                        * matrix is left to the caller.
                                        */
      void do_reinit (const unsigned int m,
                      const unsigned int n,
                      const unsigned int n_nonzero_per_row,
                      const bool         is_symmetric = false);

                                       /**
                                        * Same as previous function.
                                        */
      void do_reinit (const unsigned int               m,
                      const unsigned int               n,
                      const std::vector<unsigned int> &row_lengths,
                      const bool                       is_symmetric = false);
  };
}

#endif // DEAL_II_USE_PETSC

/*----------------------------   petsc_sparse_matrix.h     ---------------------------*/

#endif
/*----------------------------   petsc_sparse_matrix.h     ---------------------------*/
