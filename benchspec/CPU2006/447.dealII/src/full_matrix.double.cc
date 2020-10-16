//----------------------------  full_matrix.double.cc  ---------------------------
//    $Id: full_matrix.double.cc,v 1.1 2004/09/14 00:51:24 wolf Exp $
//    Version: $Name:  $
//
//    Copyright (C) 1998, 1999, 2000, 2001, 2002, 2003, 2004 by the deal.II authors
//
//    This file is subject to QPL and may not be  distributed
//    without copyright and license information. Please refer
//    to the file deal.II/doc/license.html for the  text  and
//    further information on this license.
//
//----------------------------  full_matrix.double.cc  ---------------------------


#include <lac/full_matrix.templates.h>
#include <base/logstream.h>

#define TYPEMAT double

template class FullMatrix<TYPEMAT>;

template FullMatrix<TYPEMAT>& FullMatrix<TYPEMAT>::operator =(
  const FullMatrix<float>&);

#define TYPEMAT2 double

template void FullMatrix<TYPEMAT>::fill<TYPEMAT2> (
  const FullMatrix<TYPEMAT2>&, unsigned, unsigned, unsigned, unsigned);
template void FullMatrix<TYPEMAT>::add<TYPEMAT2> (const TYPEMAT, const FullMatrix<TYPEMAT2>&);
template void FullMatrix<TYPEMAT>::add<TYPEMAT2> (
  const FullMatrix<TYPEMAT2>&, double, unsigned, unsigned, unsigned, unsigned);
template void FullMatrix<TYPEMAT>::add_scaled<TYPEMAT2> (const TYPEMAT, const FullMatrix<TYPEMAT2>&);
template void FullMatrix<TYPEMAT>::Tadd<TYPEMAT2> (const TYPEMAT, const FullMatrix<TYPEMAT2>&);
template void FullMatrix<TYPEMAT>::Tadd<TYPEMAT2> (
  const FullMatrix<TYPEMAT2>&, double, unsigned, unsigned, unsigned, unsigned);
template void FullMatrix<TYPEMAT>::mmult<TYPEMAT2> (FullMatrix<TYPEMAT2>&, const FullMatrix<TYPEMAT2>&, const bool) const;
template void FullMatrix<TYPEMAT>::Tmmult<TYPEMAT2> (FullMatrix<TYPEMAT2>&, const FullMatrix<TYPEMAT2>&, const bool) const;
template void FullMatrix<TYPEMAT>::add_diag<TYPEMAT2> (const TYPEMAT, const FullMatrix<TYPEMAT2>&);
template void FullMatrix<TYPEMAT>::invert<TYPEMAT2> (const FullMatrix<TYPEMAT2>&);

#define TYPEVEC double
#define TYPERES double

template void FullMatrix<TYPEMAT>::fill_permutation<TYPEVEC> (
  const FullMatrix<TYPEVEC>&,
  const std::vector<unsigned int>&,
  const std::vector<unsigned int>&);
template void FullMatrix<TYPEMAT>::vmult<TYPEVEC>(
  Vector<TYPEVEC>&, const Vector<TYPEVEC>&, bool) const;
template void FullMatrix<TYPEMAT>::Tvmult<TYPEVEC>(
  Vector<TYPEVEC>&, const Vector<TYPEVEC>&, bool) const;
template double FullMatrix<TYPEMAT>::residual<TYPEVEC>(
  Vector<TYPEVEC>&, const Vector<TYPEVEC>&, const Vector<TYPERES>&) const;
template TYPEVEC FullMatrix<TYPEMAT>::matrix_norm_square<TYPEVEC> (
  const Vector<TYPEVEC> &) const;
template TYPEVEC FullMatrix<TYPEMAT>::matrix_scalar_product<TYPEVEC>(
  const Vector<TYPEVEC>&, const Vector<TYPEVEC>&) const;
template void FullMatrix<TYPEMAT>::forward<TYPEVEC>(
  Vector<TYPEVEC>&, const Vector<TYPEVEC>&) const;
template void FullMatrix<TYPEMAT>::backward<TYPEVEC>(
  Vector<TYPEVEC>&, const Vector<TYPEVEC>&) const;
template void FullMatrix<TYPEMAT>::householder<TYPEVEC>(Vector<TYPEVEC>&);
template double FullMatrix<TYPEMAT>::least_squares<TYPEVEC>(
  Vector<TYPEVEC>&, Vector<TYPEVEC>&);

template
void FullMatrix<TYPEMAT>::precondition_Jacobi<TYPEVEC> (
  Vector<TYPEVEC> &, const Vector<TYPEVEC> &, const TYPEMAT) const;

#undef TYPEVEC
#define TYPEVEC float

template void FullMatrix<TYPEMAT>::fill_permutation<TYPEVEC> (
  const FullMatrix<TYPEVEC>&,
  const std::vector<unsigned int>&,
  const std::vector<unsigned int>&);
template void FullMatrix<TYPEMAT>::vmult<TYPEVEC>(
  Vector<TYPEVEC>&, const Vector<TYPEVEC>&, bool) const;
template void FullMatrix<TYPEMAT>::Tvmult<TYPEVEC>(
  Vector<TYPEVEC>&, const Vector<TYPEVEC>&, bool) const;
template double FullMatrix<TYPEMAT>::residual<TYPEVEC>(
  Vector<TYPEVEC>&, const Vector<TYPEVEC>&, const Vector<TYPERES>&) const;
template TYPEVEC FullMatrix<TYPEMAT>::matrix_norm_square<TYPEVEC> (
  const Vector<TYPEVEC> &) const;
template TYPEVEC FullMatrix<TYPEMAT>::matrix_scalar_product<TYPEVEC>(
  const Vector<TYPEVEC>&, const Vector<TYPEVEC>&) const;
template void FullMatrix<TYPEMAT>::forward<TYPEVEC>(
  Vector<TYPEVEC>&, const Vector<TYPEVEC>&) const;
template void FullMatrix<TYPEMAT>::backward<TYPEVEC>(
  Vector<TYPEVEC>&, const Vector<TYPEVEC>&) const;
template void FullMatrix<TYPEMAT>::householder<TYPEVEC>(Vector<TYPEVEC>&);
template double FullMatrix<TYPEMAT>::least_squares<TYPEVEC>(
  Vector<TYPEVEC>&, Vector<TYPEVEC>&);
template
void FullMatrix<TYPEMAT>::precondition_Jacobi<TYPEVEC> (
  Vector<TYPEVEC> &, const Vector<TYPEVEC> &, const TYPEMAT) const;


#undef TYPERES
#define TYPERES float

template
double
FullMatrix<TYPEMAT>::residual<TYPEVEC,TYPERES>(Vector<TYPEVEC>&,
					       const Vector<TYPEVEC>&,
					       const Vector<TYPERES>&) const;

// Experimental code

#ifdef HAVE_LIBLAPACK
extern "C" int dgels_ (const char* trans,
		       const unsigned int* M, const unsigned int* N,
		       const unsigned int* NRHS,
		       double* A, const unsigned int* LDA,
		       double* B, const unsigned int* LDB,
		       double* WORK, const unsigned int* LWORK,
		       int* INFO);
extern "C" int dgelss_ (const unsigned int* M, const unsigned int* N,
			const unsigned int* NRHS,
			double* A, const unsigned int* LDA,
			double* B, const unsigned int* LDB,
			double* S, const double* RCOND,
			int* RANK,
			double* WORK, const unsigned int* LWORK,
			int* INFO);


template<>
void
FullMatrix<double>::invert (const FullMatrix<double> &M)
{
  double* val = const_cast<double*> (data());

  Assert (val != 0, ExcEmptyMatrix());
  
  unsigned int dim_range = n_cols ();
  unsigned int dim_image = n_rows ();
  
  Assert (dim_range == dim_image, ExcNotQuadratic());
  Assert (dim_range == M.n_cols(),
          ExcDimensionMismatch(dim_range,M.n_cols()));
  Assert (dim_image == M.n_rows(),
	  ExcDimensionMismatch(dim_image,M.n_rows()));

  clear();
  diagadd(1.);

  const unsigned int lwork = 10*dim_range*dim_range;
  double* work = new double[lwork];
  int info;
  
//  const char* trans = "N";
  int rank;
  const double rcond=-1.;
  double* s = new double[dim_range];
  
  double* matrix = new double[dim_range*dim_range];
  std::copy (M.data(), M.data()+dim_image*dim_range, matrix);
  

  int erg = dgelss_ (&dim_range, &dim_range, &dim_range,
		     matrix, &dim_range,
		     val, &dim_range,
		     s, &rcond, &rank,
		     work, &lwork,
		     &info);
//    int erg = dgels_ (trans, &dim_range, &dim_range, &dim_range,
//  		    M.val, &dim_range,
//  		    val, &dim_range,
//  		    work, &lwork,
//  		    &info);

//  double condition = s[0]/s[dim_range-1];
  
  if (info!=0)
    deallog << "inverting error " << info << ' ' << erg << std::endl;
  if (rank<(int)dim_range)
    deallog << "rank deficiency " << rank << std::endl;
  delete[] work;
  delete[] s;
  delete[] matrix;
}

#endif
