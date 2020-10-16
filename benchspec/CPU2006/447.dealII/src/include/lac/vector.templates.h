//----------------------------  vector.templates.h  ---------------------------
//    $Id: vector.templates.h,v 1.1 2004/09/14 00:53:35 wolf Exp $
//    Version: $Name:  $
//
//    Copyright (C) 1998, 1999, 2000, 2001, 2002, 2003, 2004 by the deal.II authors
//
//    This file is subject to QPL and may not be  distributed
//    without copyright and license information. Please refer
//    to the file deal.II/doc/license.html for the  text  and
//    further information on this license.
//
//----------------------------  vector.templates.h  ---------------------------
#ifndef __deal2__vector_templates_h
#define __deal2__vector_templates_h


#include <lac/vector.h>

#ifdef DEAL_II_USE_PETSC
#  include <lac/petsc_vector.h>
#  include <lac/petsc_parallel_vector.h>
#endif

#include <cmath>
#include <algorithm>
#include <iostream>


/*
  Note that in this file, we use std::fabs, std::sqrt, etc
  everywhere. The reason is that we want to use those version of these
  functions that take a variable of the template type "Number", rather
  than the C standard function which accepts and returns a double. The
  C++ standard library therefore offers overloaded versions of these
  functions taking floats, or long doubles, with the importance on the
  additional accuracy when using long doubles.
 */

template <typename Number>
static inline Number local_sqr (const Number x)
{
  return x*x;
}



template <typename Number>
Vector<Number>::Vector (const Vector<Number>& v)
                :
		dim(v.size()),
		maxdim(v.size()),
		val(0)
{
  if (dim != 0)
    {
      val = new Number[maxdim];
      Assert (val != 0, ExcOutOfMemory());
      std::copy (v.begin(), v.end(), begin());
    }
}


// see the .h file for why this function was disabled
//
// template <typename Number>
// template <typename OtherNumber>
// Vector<Number>::Vector (const Vector<OtherNumber>& v) :
// 		dim(v.size()),
// 		maxdim(v.size()),
// 		val(0)
// {
//   if (dim != 0)
//     {
//       val = new Number[maxdim];
//       Assert (val != 0, ExcOutOfMemory());
//       copy (v.begin(), v.end(), begin());
//     }
// }


#ifdef DEAL_II_USE_PETSC

template <typename Number>
Vector<Number>::Vector (const PETScWrappers::Vector &v)
                :
		dim(v.size()),
		maxdim(v.size()),
		val(0)
{
  if (dim != 0)
    {
      val = new Number[maxdim];
      Assert (val != 0, ExcOutOfMemory());

                                       // get a representation of the vector
                                       // and copy it
      PetscScalar *start_ptr;
      int ierr = VecGetArray (static_cast<const Vec&>(v), &start_ptr);
      AssertThrow (ierr == 0, PETScWrappers::VectorBase::ExcPETScError(ierr));
      
      std::copy (start_ptr, start_ptr+dim, begin());

                                       // restore the representation of the
                                       // vector
      ierr = VecRestoreArray (static_cast<const Vec&>(v), &start_ptr);
      AssertThrow (ierr == 0, PETScWrappers::VectorBase::ExcPETScError(ierr));
    }
}



template <typename Number>
Vector<Number>::Vector (const PETScWrappers::MPI::Vector &v)
                :
		dim(0),
		maxdim(0),
		val(0)
{
  if (v.size() != 0)
    {
                                       // do this in a two-stage process:
                                       // first convert to a sequential petsc
                                       // vector, then copy that
      PETScWrappers::Vector seq (v);
      *this = seq;
    }
}

#endif


template <typename Number>
template <typename Number2>
void Vector<Number>::reinit (const Vector<Number2>& v, const bool fast)
{
  reinit (v.size(), fast);
}



template <typename Number>
void
Vector<Number>::swap (Vector<Number> &v)
{
  std::swap (dim,    v.dim);
  std::swap (maxdim, v.maxdim);
  std::swap (val,    v.val);
}



template <typename Number>
bool
Vector<Number>::all_zero () const
{
  Assert (dim!=0, ExcEmptyVector());
  
  const_iterator p = begin(),
		 e = end();
  while (p!=e)
    if (*p++ != 0.0)
      return false;
  return true;
}



template <typename Number>
bool
Vector<Number>::is_non_negative () const
{
  Assert (dim!=0, ExcEmptyVector());
  
  const_iterator p = begin(),
		 e = end();
  while (p!=e)
    if (*p++ < 0.0)
      return false;
  return true;
}



template <typename Number>
template <typename Number2>
Number Vector<Number>::operator * (const Vector<Number2>& v) const
{
  Assert (dim!=0, ExcEmptyVector());
  
  if (this == reinterpret_cast<const Vector<Number>*>(&v))
    return norm_sqr();
  
  Assert (dim == v.size(), ExcDimensionMismatch(dim, v.size()));
  
  Number sum0 = 0,
	 sum1 = 0,
	 sum2 = 0,
	 sum3 = 0;

				   // use modern processors better by
				   // allowing pipelined commands to be
				   // executed in parallel
  const_iterator ptr  = begin(),
		 eptr = ptr + (dim/4)*4;
  typename Vector<Number2>::const_iterator vptr = v.begin();
  while (ptr!=eptr)
    {
      sum0 += (*ptr++ * *vptr++);
      sum1 += (*ptr++ * *vptr++);
      sum2 += (*ptr++ * *vptr++);
      sum3 += (*ptr++ * *vptr++);
    };
				   // add up remaining elements
  while (ptr != end())
    sum0 += *ptr++ * *vptr++;
    
  return sum0+sum1+sum2+sum3;
}


template <typename Number>
Number Vector<Number>::norm_sqr () const
{
  Assert (dim!=0, ExcEmptyVector());

  Number sum0 = 0,
	 sum1 = 0,
	 sum2 = 0,
	 sum3 = 0;

				   // use modern processors better by
				   // allowing pipelined commands to be
				   // executed in parallel
  const_iterator ptr  = begin(),
		 eptr = ptr + (dim/4)*4;
  while (ptr!=eptr)
    {
      sum0 += ::local_sqr(*ptr++);
      sum1 += ::local_sqr(*ptr++);
      sum2 += ::local_sqr(*ptr++);
      sum3 += ::local_sqr(*ptr++);
    };
				   // add up remaining elements
  while (ptr != end())
    sum0 += ::local_sqr(*ptr++);
  
  return sum0+sum1+sum2+sum3;
}


template <typename Number>
Number Vector<Number>::mean_value () const
{
  Assert (dim!=0, ExcEmptyVector());

  Number sum0 = 0,
	 sum1 = 0,
	 sum2 = 0,
	 sum3 = 0;

				   // use modern processors better by
				   // allowing pipelined commands to be
				   // executed in parallel
  const_iterator ptr  = begin(),
		 eptr = ptr + (dim/4)*4;
  while (ptr!=eptr)
    {
      sum0 += *ptr++;
      sum1 += *ptr++;
      sum2 += *ptr++;
      sum3 += *ptr++;
    };
				   // add up remaining elements
  while (ptr != end())
    sum0 += *ptr++;
  
  return (sum0+sum1+sum2+sum3)/size();
}



template <typename Number>
Number Vector<Number>::l1_norm () const
{
  Assert (dim!=0, ExcEmptyVector());

  Number sum0 = 0,
	 sum1 = 0,
	 sum2 = 0,
	 sum3 = 0;

				   // use modern processors better by
				   // allowing pipelined commands to be
				   // executed in parallel
  const_iterator ptr  = begin(),
		 eptr = ptr + (dim/4)*4;
  while (ptr!=eptr)
    {
      sum0 += std::fabs(*ptr++);
      sum1 += std::fabs(*ptr++);
      sum2 += std::fabs(*ptr++);
      sum3 += std::fabs(*ptr++);
    };
				   // add up remaining elements
  while (ptr != end())
    sum0 += std::fabs(*ptr++);
  
  return sum0+sum1+sum2+sum3;
}


template <typename Number>
Number Vector<Number>::l2_norm () const
{
  return std::sqrt(norm_sqr());
}


template <typename Number>
Number Vector<Number>::lp_norm (const Number p) const
{
  Assert (dim!=0, ExcEmptyVector());

  Number sum0 = 0,
	 sum1 = 0,
	 sum2 = 0,
	 sum3 = 0;

				   // use modern processors better by
				   // allowing pipelined commands to be
				   // executed in parallel
  const_iterator ptr  = begin(),
		 eptr = ptr + (dim/4)*4;
  while (ptr!=eptr)
    {
      sum0 += std::pow(std::fabs(*ptr++), p);
      sum1 += std::pow(std::fabs(*ptr++), p);
      sum2 += std::pow(std::fabs(*ptr++), p);
      sum3 += std::pow(std::fabs(*ptr++), p);
    };
				   // add up remaining elements
  while (ptr != end())
    sum0 += std::pow(std::fabs(*ptr++), p);
  
  return std::pow(sum0+sum1+sum2+sum3,
		  static_cast<Number>(1./p));
}


template <typename Number>
Number Vector<Number>::linfty_norm () const
{
  Assert (dim!=0, ExcEmptyVector());

  Number max0=0.,
	 max1=0.,
	 max2=0.,
	 max3=0.;
  for (unsigned int i=0; i<(dim/4); ++i) 
    {
      if (max0<std::fabs(val[4*i]))   max0=std::fabs(val[4*i]);
      if (max1<std::fabs(val[4*i+1])) max1=std::fabs(val[4*i+1]);
      if (max2<std::fabs(val[4*i+2])) max2=std::fabs(val[4*i+2]);
      if (max3<std::fabs(val[4*i+3])) max3=std::fabs(val[4*i+3]);
    };
				   // add up remaining elements
  for (unsigned int i=(dim/4)*4; i<dim; ++i)
    if (max0<std::fabs(val[i]))
      max0 = std::fabs(val[i]);

  return std::max (std::max(max0, max1),
		   std::max(max2, max3));
}


template <typename Number>
Vector<Number>& Vector<Number>::operator += (const Vector<Number>& v)
{
  Assert (dim!=0, ExcEmptyVector());

  add (v);
  return *this;
}


template <typename Number>
Vector<Number>& Vector<Number>::operator -= (const Vector<Number>& v)
{
  Assert (dim!=0, ExcEmptyVector());
  Assert (dim == v.dim, ExcDimensionMismatch(dim, v.dim));

  iterator i_ptr = begin(),
	   i_end = end();
  const_iterator v_ptr = v.begin();
  while (i_ptr!=i_end)
    *i_ptr++ -= *v_ptr++;

  return *this;
}


template <typename Number>
void Vector<Number>::add (const Number v)
{
  Assert (dim!=0, ExcEmptyVector());

  iterator i_ptr = begin(),
	   i_end = end();
  while (i_ptr!=i_end)
    *i_ptr++ += v;
}


template <typename Number>
void Vector<Number>::add (const Vector<Number>& v)
{
  Assert (dim!=0, ExcEmptyVector());
  Assert (dim == v.dim, ExcDimensionMismatch(dim, v.dim));

  iterator i_ptr = begin(),
	   i_end = end();
  const_iterator v_ptr = v.begin();
  while (i_ptr!=i_end)
    *i_ptr++ += *v_ptr++;
}


template <typename Number>
void Vector<Number>::add (const Number a, const Vector<Number>& v)
{
  Assert (dim!=0, ExcEmptyVector());
  Assert (dim == v.dim, ExcDimensionMismatch(dim, v.dim));

  iterator i_ptr = begin(),
	   i_end = end();
  const_iterator v_ptr = v.begin();
  while (i_ptr!=i_end)
    *i_ptr++ += a * *v_ptr++;
}


template <typename Number>
void Vector<Number>::add (const Number a, const Vector<Number>& v,
			  const Number b, const Vector<Number>& w)
{
  Assert (dim!=0, ExcEmptyVector());
  Assert (dim == v.dim, ExcDimensionMismatch(dim, v.dim));
  Assert (dim == w.dim, ExcDimensionMismatch(dim, w.dim));
  iterator i_ptr = begin(),
	   i_end = end();
  const_iterator v_ptr = v.begin(),
		 w_ptr = w.begin();
  while (i_ptr!=i_end)
    *i_ptr++ += a * *v_ptr++ + b * *w_ptr++;
}


template <typename Number>
void Vector<Number>::sadd (const Number x, const Vector<Number>& v)
{
  Assert (dim!=0, ExcEmptyVector());
  Assert (dim == v.dim, ExcDimensionMismatch(dim, v.dim));
  iterator i_ptr = begin(),
	   i_end = end();
  const_iterator v_ptr = v.begin();
  for (; i_ptr!=i_end; ++i_ptr)
    *i_ptr = x * *i_ptr  + *v_ptr++;
}


template <typename Number>
void Vector<Number>::sadd (const Number x, const Number a,
                           const Vector<Number>& v)
{
  Assert (dim!=0, ExcEmptyVector());
  Assert (dim == v.dim, ExcDimensionMismatch(dim, v.dim));
  iterator i_ptr = begin(),
	   i_end = end();
  const_iterator v_ptr = v.begin();
  for (; i_ptr!=i_end; ++i_ptr)
    *i_ptr = x * *i_ptr  +  a * *v_ptr++;
}


template <typename Number>
void Vector<Number>::sadd (const Number x, const Number a,
			   const Vector<Number>& v, const Number b,
                           const Vector<Number>& w)
{
  Assert (dim!=0, ExcEmptyVector());
  Assert (dim == v.dim, ExcDimensionMismatch(dim, v.dim));
  Assert (dim == w.dim, ExcDimensionMismatch(dim, w.dim));
  iterator i_ptr = begin(),
	   i_end = end();
  const_iterator v_ptr = v.begin(),
		 w_ptr = w.begin();
  for (; i_ptr!=i_end; ++i_ptr)
    *i_ptr = x * *i_ptr  +  a * *v_ptr++  + b * *w_ptr++;
}


template <typename Number>
void Vector<Number>::sadd (const Number x, const Number a,
			   const Vector<Number>& v, const Number b,
			   const Vector<Number>& w, const Number c,
                           const Vector<Number>& y)
{
  Assert (dim!=0, ExcEmptyVector());
  Assert (dim == v.dim, ExcDimensionMismatch(dim, v.dim));
  Assert (dim == w.dim, ExcDimensionMismatch(dim, w.dim));
  Assert (dim == y.dim, ExcDimensionMismatch(dim, y.dim));
  iterator i_ptr = begin(),
	   i_end = end();
  const_iterator v_ptr = v.begin(),
		 w_ptr = w.begin(),
		 y_ptr = y.begin();
  
  for (; i_ptr!=i_end; ++i_ptr)
    *i_ptr = (x * *i_ptr)  +  (a * *v_ptr++)  +  (b * *w_ptr++)  + (c * *y_ptr++);
}



template <typename Number>
void Vector<Number>::scale (const Number factor)
{
  Assert (dim!=0, ExcEmptyVector());

  iterator             ptr  = begin();
  const const_iterator eptr = end();
  while (ptr!=eptr)
    *ptr++ *= factor;
}



template <typename Number>
template <typename Number2>
void Vector<Number>::scale (const Vector<Number2> &s)
{
  Assert (dim!=0, ExcEmptyVector());
  Assert (dim == s.dim, ExcDimensionMismatch(dim, s.dim));
  
  iterator             ptr  = begin();
  const const_iterator eptr = end();
  typename Vector<Number2>::const_iterator sptr = s.begin();
  while (ptr!=eptr)
    *ptr++ *= *sptr++;
}



template <typename Number>
void Vector<Number>::equ (const Number a, const Vector<Number>& u,
			  const Number b, const Vector<Number>& v)
{
  Assert (dim!=0, ExcEmptyVector());
  Assert (dim == u.dim, ExcDimensionMismatch(dim, u.dim));
  Assert (dim == v.dim, ExcDimensionMismatch(dim, v.dim));
  iterator i_ptr = begin(),
	   i_end = end();
  const_iterator u_ptr = u.begin(),
		 v_ptr = v.begin();
  while (i_ptr!=i_end)
    *i_ptr++ = a * *u_ptr++  + b * *v_ptr++;
}



template <typename Number>
template <typename Number2>
void Vector<Number>::equ (const Number a, const Vector<Number2>& u)
{
  Assert (dim!=0, ExcEmptyVector());
  Assert (dim == u.dim, ExcDimensionMismatch(dim, u.dim));
  iterator i_ptr = begin(),
	   i_end = end();
  typename Vector<Number2>::const_iterator u_ptr = u.begin();
  while (i_ptr!=i_end)
    *i_ptr++ = a * *u_ptr++;
}



template <typename Number>
void Vector<Number>::ratio (const Vector<Number> &a, const Vector<Number> &b)
{
  Assert (dim!=0, ExcEmptyVector());
  Assert (a.dim == b.dim, ExcDimensionMismatch (a.dim, b.dim));

				   // no need to reinit with zeros, since
				   // we overwrite them anyway
  reinit (a.size(), true);
  iterator i_ptr = begin(),
	   i_end = end();
  const_iterator a_ptr = a.begin(),
		 b_ptr = b.begin();
  while (i_ptr!=i_end)
    *i_ptr++ = *a_ptr++ / *b_ptr++;
}



template <typename Number>
Vector<Number>&
Vector<Number>::operator = (const Vector<Number>& v)
{
  if (v.dim != dim)
    reinit (v.dim, true);
  if (dim!=0)
    std::copy (v.begin(), v.end(), begin());
  
  return *this;
}



template <typename Number>
template <typename Number2>
Vector<Number>&
Vector<Number>::operator = (const Vector<Number2>& v)
{
  if (v.size() != dim)
    reinit (v.size(), true);
  if (dim!=0)
    std::copy (v.begin(), v.end(), begin());
  
  return *this;
}



#ifdef DEAL_II_USE_PETSC

template <typename Number>
Vector<Number> &
Vector<Number>::operator = (const PETScWrappers::Vector &v)
{
  if (v.size() != dim)
    reinit (v.size(), true);
  if (dim != 0)
    {
                                       // get a representation of the vector
                                       // and copy it
      PetscScalar *start_ptr;
      int ierr = VecGetArray (static_cast<const Vec&>(v), &start_ptr);
      AssertThrow (ierr == 0, PETScWrappers::VectorBase::ExcPETScError(ierr));
      
      std::copy (start_ptr, start_ptr+dim, begin());

                                       // restore the representation of the
                                       // vector
      ierr = VecRestoreArray (static_cast<const Vec&>(v), &start_ptr);
      AssertThrow (ierr == 0, PETScWrappers::VectorBase::ExcPETScError(ierr));
    }

  return *this;
}



template <typename Number>
Vector<Number> &
Vector<Number>::operator = (const PETScWrappers::MPI::Vector &v)
{
                                   // do this in a two-stage process:
                                   // first convert to a sequential petsc
                                   // vector, then copy that
  PETScWrappers::Vector seq (v);
  *this = seq;

  return *this;
}

#endif


template <typename Number>
template <typename Number2>
bool
Vector<Number>::operator == (const Vector<Number2>& v) const
{
  Assert (dim!=0, ExcEmptyVector());
  Assert (dim == v.size(), ExcDimensionMismatch(dim, v.size()));

  for (unsigned int i=0; i<dim; ++i)
    if (val[i] != v.val[i])
      return false;

  return true;
}



template <typename Number>
void Vector<Number>::print (const char* format) const
{
  Assert (dim!=0, ExcEmptyVector());
  if (!format) format = " %5.2f";
  for (unsigned int j=0;j<size();j++)
    std::printf (format, val[j]);
  std::printf ("\n");
}



template <typename Number>
void Vector<Number>::print (std::ostream      &out,
			    const unsigned int precision,
			    const bool         scientific,
			    const bool         across) const
{
  Assert (dim!=0, ExcEmptyVector());
  AssertThrow (out, ExcIO());

  out.precision (precision);
  if (scientific)
    out.setf (std::ios::scientific, std::ios::floatfield);
  else
    out.setf (std::ios::fixed, std::ios::floatfield);

  if (across)
    for (unsigned int i=0; i<size(); ++i)
      out << static_cast<double>(val[i]) << ' ';
  else
    for (unsigned int i=0; i<size(); ++i)
      out << static_cast<double>(val[i]) << std::endl;
  out << std::endl;
  
  AssertThrow (out, ExcIO());
}



template <typename Number>
void Vector<Number>::block_write (std::ostream &out) const
{
  AssertThrow (out, ExcIO());

				   // other version of the following
				   //  out << size() << std::endl << '[';
				   // reason: operator<< seems to use
				   // some resources that lead to
				   // problems in a multithreaded
				   // environment
  const unsigned int sz = size();
  char buf[16];
  
  std::sprintf(buf, "%d", sz);
  std::strcat(buf, "\n[");
  
  out.write(buf, std::strlen(buf));
  out.write (reinterpret_cast<const char*>(begin()),
	     reinterpret_cast<const char*>(end())
	     - reinterpret_cast<const char*>(begin()));
  
				   // out << ']';
  const char outro = ']';
  out.write (&outro, 1);
  
  AssertThrow (out, ExcIO());
}



template <typename Number>
void Vector<Number>::block_read (std::istream &in)
{
  AssertThrow (in, ExcIO());

  unsigned int sz;

  char buf[16];
  

  in.getline(buf,16,'\n');
  sz=std::atoi(buf);
  
				   // fast initialization, since the
				   // data elements are overwritten anyway
  reinit (sz, true);     

  char c;
				   //  in >> c;
  in.read (&c, 1);
  AssertThrow (c=='[', ExcIO());
  
  in.read (reinterpret_cast<char*>(begin()),
	   reinterpret_cast<const char*>(end())
	   - reinterpret_cast<const char*>(begin()));
  
				   //  in >> c;
  in.read (&c, 1);
  AssertThrow (c==']', ExcIO());
}



template <typename Number>
unsigned int
Vector<Number>::memory_consumption () const
{
  return sizeof(*this) + (maxdim * sizeof(Number));
}


#endif
