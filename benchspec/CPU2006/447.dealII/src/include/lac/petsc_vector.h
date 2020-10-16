//----------------------------  petsc_vector.h  ---------------------------
//    $Id: petsc_vector.h,v 1.1 2004/09/14 00:53:34 wolf Exp $
//    Version: $Name:  $
//
//    Copyright (C) 2004 by the deal.II authors
//
//    This file is subject to QPL and may not be  distributed
//    without copyright and license information. Please refer
//    to the file deal.II/doc/license.html for the  text  and
//    further information on this license.
//
//----------------------------  petsc_vector.h  ---------------------------
#ifndef __deal2__petsc_vector_h
#define __deal2__petsc_vector_h

#include <base/config.h>
#include <base/exceptions.h>
#include <base/subscriptor.h>

#include <lac/vector.h>

#ifdef DEAL_II_USE_PETSC

#include <lac/petsc_vector_base.h>
#include <lac/petsc_parallel_vector.h>


namespace PETScWrappers
{
/**
 * Implementation of a sequential vector class based on PETSC. All the
 * functionality is actually in the base class, except for the calls to
 * generate a sequential vector. This is possible since PETSc only works on an
 * abstract vector type and internally distributes to functions that do the
 * actual work depending on the actual vector type (much like using virtual
 * functions). Only the functions creating a vector of specific type differ,
 * and are implemented in this particular class.
 *
 * @ingroup PETScWrappers
 * @author Wolfgang Bangerth, 2004
 */
  class Vector : public VectorBase
  {
    public:
                                       /**
                                        * Default constructor. Initialize the
                                        * vector as empty.
                                        */
      Vector ();
      
                                       /**
                                        * Constructor. Set dimension to
                                        * @p n and initialize all
                                        * elements with zero.
                                        *
                                        * The constructor is made explicit to
                                        * avoid accidents like this:
                                        * <tt>v=0;</tt>. Presumably, the user wants
                                        * to set every element of the vector to
                                        * zero, but instead, what happens is
                                        * this call: <tt>v=Vector@<number@>(0);</tt>,
                                        * i.e. the vector is replaced by one of
                                        * length zero.
                                        */
      explicit Vector (const unsigned int n);
    
                                       /**
                                        * Copy-constructor from deal.II
                                        * vectors. Sets the dimension to that
                                        * of the given vector, and copies all
                                        * elements.
                                        */
      template <typename Number>
      explicit Vector (const ::Vector<Number> &v);

                                       /**
                                        * Copy-constructor the values from a
                                        * PETSc wrapper vector class.
                                        */
      explicit Vector (const Vector &v);

                                       /**
                                        * Copy-constructor the values from a
                                        * PETSc wrapper parallel vector class.
                                        */
      explicit Vector (const MPI::Vector &v);

                                       /**
                                        * Copy the given vector. Resize the
                                        * present vector if necessary.
                                        */
      Vector & operator = (const Vector &v);

                                       /**
                                        * Copy all the elements of the
                                        * parallel vector @arg v into this
                                        * local vector. Note that due to the
                                        * communication model of MPI, @em all
                                        * processes have to actually perform
                                        * this operation, even if they do not
                                        * use the result. It is not sufficient
                                        * if only one processor tries to copy
                                        * the elements from the other
                                        * processors over to its own process
                                        * space.
                                        */
      Vector & operator = (const MPI::Vector &v);

                                       /**
                                        * Set all components of the vector to
                                        * the given number @p s. Simply pass
                                        * this down to the base class, but we
                                        * still need to declare this function
                                        * to make the example given in the
                                        * discussion about making the
                                        * constructor explicit work.
                                        *
                                        * Since the semantics of assigning a
                                        * scalar to a vector are not
                                        * immediately clear, this operator
                                        * should really only be used if you
                                        * want to set the entire vector to
                                        * zero. This allows the intuitive
                                        * notation <tt>v=0</tt>. Assigning
                                        * other values is deprecated and may
                                        * be disallowed in the future.
                                        */
      Vector & operator = (const PetscScalar s);

                                       /**
                                        * Copy the values of a deal.II vector
                                        * (as opposed to those of the PETSc
                                        * vector wrapper class) into this
                                        * object.
                                        */
      template <typename number>
      Vector & operator = (const ::Vector<number> &v);

                                       /**
                                        * Change the dimension of the vector
                                        * to @p N. It is unspecified how
                                        * resizing the vector affects the
                                        * memory allocation of this object;
                                        * i.e., it is not guaranteed that
                                        * resizing it to a smaller size
                                        * actually also reduces memory
                                        * consumption, or if for efficiency
                                        * the same amount of memory is used
                                        * for less data.
                                        *
                                        * If @p fast is false, the vector is
                                        * filled by zeros. Otherwise, the
                                        * elements are left an unspecified
                                        * state.
                                        */ 
      void reinit (const unsigned int N,
                   const bool         fast = false);
    
                                       /**
                                        * Change the dimension to that of the
                                        * vector @p v. The same applies as
                                        * for the other reinit() function.
                                        *
                                        * The elements of @p v are not
                                        * copied, i.e.  this function is the
                                        * same as calling <tt>reinit (v.size(),
                                        * fast)</tt>.
                                        */
      void reinit (const Vector &v,
                   const bool    fast = false);

    protected:
                                       /**
                                        * Create a vector of length @p n. For
                                        * this class, we create a sequential
                                        * vector. @p n denotes the total
                                        * size of the vector to be
                                        * created.
                                        */
      void create_vector (const unsigned int n);
  };


/// @if NoDoc
// ------------------ template and inline functions -------------


  template <typename number>
  Vector::Vector (const ::Vector<number> &v)
  {
    Vector::create_vector (v.size());

    *this = v;
  }

  
  
  inline
  Vector &
  Vector::operator = (const PetscScalar s)
  {
    VectorBase::operator = (s);

    return *this;
  }
  

  inline
  Vector &
  Vector::operator = (const Vector &v)
  {
                                     // if the vectors have different sizes,
                                     // then first resize the present one
    if (size() != v.size())
      reinit (v.size(), true);
    
    const int ierr = VecCopy (v.vector, vector);
    AssertThrow (ierr == 0, ExcPETScError(ierr));
    
    return *this;
  }



  inline
  Vector &
  Vector::operator = (const MPI::Vector &v)
  {
                                     // the petsc function we call wants to
                                     // generate the vector itself, so destroy
                                     // the old one first
    int ierr = VecDestroy (vector);
    AssertThrow (ierr == 0, ExcPETScError(ierr));

                                     // then do the gather operation
    ierr = VecConvertMPIToSeqAll (static_cast<const Vec &>(v),
                                  &vector);
    AssertThrow (ierr == 0, ExcPETScError(ierr));
    
    return *this;
  }



  template <typename number>
  inline
  Vector &
  Vector::operator = (const ::Vector<number> &v) 
  {
    reinit (v.size(), true);
                                     // the following isn't necessarily fast,
                                     // but this is due to the fact that PETSc
                                     // doesn't offer an inlined access
                                     // operator.
                                     //
                                     // if someone wants to contribute some
                                     // code: to make this code faster, one
                                     // could either first convert all values
                                     // to PetscScalar, and then set them all
                                     // at once using VecSetValues. This has
                                     // the drawback that it could take quite
                                     // some memory, if the vector is large,
                                     // and it would in addition allocate
                                     // memory on the heap, which is
                                     // expensive. an alternative would be to
                                     // split the vector into chunks of, say,
                                     // 128 elements, convert a chunk at a
                                     // time and set it in the output vector
                                     // using VecSetValues. since 128 elements
                                     // is small enough, this could easily be
                                     // allocated on the stack (as a local
                                     // variable) which would make the whole
                                     // thing much more efficient.
                                     //
                                     // a second way to make things faster is
                                     // for the special case that
                                     // number==PetscScalar. we could then
                                     // declare a specialization of this
                                     // template, and omit the conversion. the
                                     // problem with this is that the best we
                                     // can do is to use VecSetValues, but
                                     // this isn't very efficient either: it
                                     // wants to see an array of indices,
                                     // which in this case a) again takes up a
                                     // whole lot of memory on the heap, and
                                     // b) is totally dumb since its content
                                     // would simply be the sequence
                                     // 0,1,2,3,...,n. the best of all worlds
                                     // would probably be a function in Petsc
                                     // that would take a pointer to an array
                                     // of PetscScalar values and simply copy
                                     // n elements verbatim into the vector...
    for (unsigned int i=0; i<v.size(); ++i)
      (*this)(i) = v(i);

    compress ();

    return *this;
  }
/// @endif

}


#endif // DEAL_II_USE_PETSC

/*----------------------------   petsc_vector.h     ---------------------------*/

#endif
/*----------------------------   petsc_vector.h     ---------------------------*/
