//----------------------------  memory_consumption.h  ---------------------------
//    $Id: memory_consumption.h,v 1.1 2004/09/14 00:53:31 wolf Exp $
//    Version: $Name:  $
//
//    Copyright (C) 1998, 1999, 2000, 2001, 2002, 2003, 2004 by the deal authors
//
//    This file is subject to QPL and may not be  distributed
//    without copyright and license information. Please refer
//    to the file deal.II/doc/license.html for the  text  and
//    further information on this license.
//
//----------------------------  memory_consumption.h  ---------------------------
#ifndef __deal2__memory_consumption_h
#define __deal2__memory_consumption_h


#include <base/config.h>
#include <string>
#include <vector>
#include <utility>



/**
 * This namespace provides functions helping to determine the amount
 * of memory used by objects. The goal is not necessarily to give the
 * amount of memory used up to the last bit (what is the memory used
 * by an STL <tt>std::map<></tt> object?), but rather to aid in the search for
 * memory bottlenecks.
 *
 * The functions in this namespace work basically by reducing each
 * object to its basics as far as they are known from this place. They
 * do not attempt to know what goes on in each object, if they are not
 * basic types (such as <tt>int</tt> or <tt>double</tt>) or STL containers (such
 * as <tt>vectors</tt>). The method goes as follows: if the object with
 * which a <tt>memory_consumption</tt> function from this namespace is an
 * atomic type, the return its size by applying the <tt>sizeof</tt>
 * operator to it. If this is not the case, then try to reduce it to
 * more basic types.
 *
 * For example, if it is a C-style array or a standard C++ <tt>std::vector</tt>,
 * then sum up the sizes of the array elements by calling
 * <tt>memory_consumption</tt> on each of them. This way, we can also
 * reduce objects of type <tt>std::vector<std::vector<double> ></tt> to its atomic
 * types, and can thus determine the memory used even if the sizes of
 * the elements of the outermost vector differ (e.g. the first
 * sub-vector has 3 and the second sub-vector has 10 elements).
 *
 * There are two exceptions to simply adding up the sizes of the
 * subobjects: for C++ <tt>std::vector</tt> objects, we also have to add the
 * size of the vector object, i.e. <tt>sizeof(vector<T>)</tt>, to the sizes
 * of the elements. Secondly, for the most common used vectors, such
 * as <tt>std::vector<double></tt> and <tt>std::vector<unsigned int></tt> we determine the
 * size without a loop but rather directly, since we know that the
 * sizes of the elements are constant.
 *
 * Finally, if we cannot reduce a type <tt>T</tt> further, because it is
 * neither atomic nor a known C++ data type, we call a member function
 * <tt>T::memory_consumption</tt> on it, which we assume to exist. Almost
 * all classes in the deal.II library have such a function. This way,
 * if we call <tt>memory_consumption(v)</tt> on a vector <tt>v</tt> of type
 * <tt>FullMatrix<double></tt>, we first reduce this to a loop in which we
 * call <tt>memory_consumption(v[i])</tt>, and because there is no such
 * function handling this explicitly, we try to call
 * <tt>v[i].memory_consumption()</tt>.
 *
 *
 * @section MeMoryConsumptionExt Extending this namespace
 *
 * The functions in this namespace and the functionality provided by
 * it live on the assumption that there is either a function
 * <tt>memory_consumption(T)</tt> in this namespace determining the amount
 * of memory use by objects of type <tt>T</tt>, or that the class <tt>T</tt> has
 * a function of that name as member function. While the latter is
 * true for almost all class in deal.II, we have only implemented the
 * first kind of functions for the most common data types, such as
 * atomic types, strings, C++ vectors, C-style arrays, and C++
 * pairs. These functions therefore do not cover, for example, C++
 * maps, lists, etc. If you need such functions feel free to implement
 * them and send them to us for inclusion.
 *
 * @author Wolfgang Bangerth, 2000
 */
namespace MemoryConsumption
{
				   /**
				    * Determine the amount of memory
				    * in bytes consumed by a <tt>bool</tt>
				    * variable.
				    */
  inline
  unsigned int memory_consumption (const bool);

				   /**
				    * Determine the amount of memory
				    * in bytes consumed by a <tt>char</tt>
				    * variable.
				    */
  inline
  unsigned int memory_consumption (const char);

				   /**
				    * Determine the amount of memory
				    * in bytes consumed by a
				    * <tt>short int</tt> variable.
				    */
  inline
  unsigned int memory_consumption (const short int);

				   /**
				    * Determine the amount of memory
				    * in bytes consumed by a
				    * <tt>short unsigned int</tt> variable.
				    */
  inline
  unsigned int memory_consumption (const short unsigned int);

				   /**
				    * Determine the amount of memory
				    * in bytes consumed by a <tt>int</tt>
				    * variable.
				    */
  inline
  unsigned int memory_consumption (const int);

				   /**
				    * Determine the amount of memory
				    * in bytes consumed by a <tt>unsigned int</tt>
				    * variable.
				    */
  inline
  unsigned int memory_consumption (const unsigned int);

				   /**
				    * Determine the amount of memory
				    * in bytes consumed by a <tt>float</tt>
				    * variable.
				    */
  inline
  unsigned int memory_consumption (const float);

				   /**
				    * Determine the amount of memory
				    * in bytes consumed by a <tt>double</tt>
				    * variable.
				    */
  inline
  unsigned int memory_consumption (const double);

				   /**
				    * Determine an estimate of the
				    * amount of memory in bytes
				    * consumed by a <tt>std::string</tt>
				    * variable.
				    */
  inline
  unsigned int memory_consumption (const std::string &s);

				   /**
				    * Determine an estimate of the
				    * amount of memory in bytes
				    * consumed by a <tt>std::vector</tt> of
				    * certain elements. It does so by
				    * looping over all elements of the
				    * vector and determining their
				    * sizes using the
				    * <tt>memory_consumption</tt>
				    * functions. If the elements are
				    * of constant size, there might be
				    * another global function
				    * <tt>memory_consumption</tt> for this
				    * data type or if there is a
				    * member function of that class of
				    * that names that returns a
				    * constant value and the compiler
				    * will unroll this loop so that
				    * the operation is fast. If the
				    * size of the data elements is
				    * variable, for example if they do
				    * memory allocation themselves,
				    * then the operation will
				    * necessarily be more expensive.
				    *
				    * Using the algorithm, in
				    * particular the loop over all
				    * elements, it is possible to also
				    * compute the memory consumption
				    * of vectors of vectors, vectors
				    * of strings, etc, where the
				    * individual elements may have
				    * vastly different sizes.
				    *
				    * Note that this algorithm also
				    * takes into account the size of
				    * elements that are allocated by
				    * this vector but not currently
				    * used.
				    *
				    * For the most commonly used
				    * vectors, there are special
				    * functions that compute the size
				    * without a loop. This also
				    * applies for the special case of
				    * vectors of bools.
				    */
  template <typename T>
  inline
  unsigned int memory_consumption (const std::vector<T> &v);

				   /**
				    * Estimate the amount of memory
				    * (in bytes) occupied by a
				    * C-style array. Since in this
				    * library we do not usually
				    * store simple data elements
				    * like <tt>double</tt>s in such
				    * arrays (but rather use STL
				    * <tt>std::vector</tt>s or deal.II
				    * <tt>Vector</tt> objects), we do not
				    * provide specializations like
				    * for the <tt>std::vector</tt> arrays, but
				    * always use the loop over all
				    * elements.
				    */
  template <typename T, int N>
  inline
  unsigned int memory_consumption (const T (&v)[N]);
  
				   /**
				    * Specialization of the
				    * determination of the memory
				    * consumption of a vector, here
				    * for a vector of <tt>bool</tt>s.
				    *
				    * This is a special case, as the
				    * bools are not stored
				    * one-by-one, but as a bit
				    * field.
				    */
  inline
  unsigned int memory_consumption (const std::vector<bool> &v);
    
				   /**
				    * Specialization of the
				    * determination of the memory
				    * consumption of a vector, here
				    * for a vector of <tt>int</tt>s.
				    */
  inline
  unsigned int memory_consumption (const std::vector<int> &v);
    
				   /**
				    * Specialization of the
				    * determination of the memory
				    * consumption of a vector, here
				    * for a vector of <tt>double</tt>s.
				    */
  inline
  unsigned int memory_consumption (const std::vector<double> &v);
    
				   /**
				    * Specialization of the
				    * determination of the memory
				    * consumption of a vector, here
				    * for a vector of <tt>float</tt>s.
				    */
  inline
  unsigned int memory_consumption (const std::vector<float> &v);
    
				   /**
				    * Specialization of the
				    * determination of the memory
				    * consumption of a vector, here
				    * for a vector of <tt>char</tt>s.
				    */
  inline
  unsigned int memory_consumption (const std::vector<char> &v);
    
				   /**
				    * Specialization of the
				    * determination of the memory
				    * consumption of a vector, here
				    * for a vector of <tt>unsigned char</tt>s.
				    */
  inline
  unsigned int memory_consumption (const std::vector<unsigned char> &v);
    
				   /**
				    * Specialization of the
				    * determination of the memory
				    * consumption of a vector, here
				    * for a vector of pointers.
				    */
  template <typename T>
  inline
  unsigned int memory_consumption (const std::vector<T *> &v);

				   /** 
				    * Specialization of the
				    * determination of the memory
				    * consumption of a vector, here
				    * for a vector of strings. This
				    * function is not necessary from a
				    * strict C++ viewpoint, since it
				    * could be generated, but is
				    * necessary for compatibility with
				    * IBM's xlC 5.0 compiler, and
				    * doesn't harm for other compilers
				    * as well.  
				    */
  unsigned int memory_consumption (const std::vector<std::string> &v);


				   /**
				    * Determine an estimate of the
				    * amount of memory in bytes
				    * consumed by a pair of values.
				    */
  template <typename A, typename B>
  inline
  unsigned int memory_consumption (const std::pair<A,B> &p);
    
				   /**
				    * Return the amount of memory
				    * used by a pointer. Make sure
				    * that you are really interested
				    * in this, and not the amount of
				    * memory required by the object
				    * pointed to.
				    */
  template <typename T>
  inline
  unsigned int memory_consumption (const T * const);

				   /**
				    * Return the amount of memory
				    * used by a pointer. Make sure
				    * that you are really interested
				    * in this, and not the amount of
				    * memory required by the object
				    * pointed to.
				    *
				    * This function is the same as
				    * above, but for non-const
				    * pointers
				    */
  template <typename T>
  inline
  unsigned int memory_consumption (T * const);

				   /**
				    * Return the amount of memory
				    * used by a void pointer. Make
				    * sure that you are really
				    * interested in this, and not
				    * the amount of memory required
				    * by the object pointed to.
				    *
				    * Note that we needed this
				    * function since <tt>void</tt> is no
				    * type and a <tt>void*</tt> is thus
				    * not caught by the general
				    * <tt>T*</tt> template function
				    * above.
				    */
  inline
  unsigned int memory_consumption (void * const);
    
    
				   /**
				    * For all other types which are
				    * not explicitly listed: try if
				    * there is a member function
				    * called
				    * <tt>memory_consumption</tt>. If
				    * this is not the case, then the
				    * compiler will in any case
				    * complain that this last exit
				    * does not work.
				    */
  template <typename T>
  inline
  unsigned int memory_consumption (const T &t);
}



// now comes the implementation of these functions

/// @if NoDoc

namespace MemoryConsumption
{
  inline
  unsigned int memory_consumption (const bool)
  {
    return sizeof(bool);
  }
  
  
  
  inline
  unsigned int memory_consumption (const char)
  {
    return sizeof(char);
  }
  


  inline
  unsigned int memory_consumption (const short int) 
  {
    return sizeof(short int);
  }
  


  inline
  unsigned int memory_consumption (const short unsigned int) 
  {
    return sizeof(short unsigned int);
  }



  inline
  unsigned int memory_consumption (const int) 
  {
    return sizeof(int);
  }
  


  inline
  unsigned int memory_consumption (const unsigned int) 
  {
    return sizeof(unsigned int);
  }



  inline
  unsigned int memory_consumption (const float)
  {
    return sizeof(float);
  }



  inline
  unsigned int memory_consumption (const double)
  {
    return sizeof(double);
  }


  
  inline
  unsigned int memory_consumption (const std::string &s)
  {
    return sizeof(s) + s.length();
  }



  template <typename T>
  unsigned int memory_consumption (const std::vector<T> &v)
  {
    unsigned int mem = sizeof(std::vector<T>);
    const unsigned int n = v.size();
    for (unsigned int i=0; i<n; ++i)
      mem += memory_consumption(v[i]);
    mem += (v.capacity() - n)*sizeof(T);
    return mem;
  }



  template <typename T, int N>
  unsigned int memory_consumption (const T (&v)[N])
  {
    unsigned int mem = 0;
    for (unsigned int i=0; i<N; ++i)
      mem += memory_consumption(v[i]);
    return mem;
  }



  inline
  unsigned int memory_consumption (const std::vector<bool> &v)
  {
    return v.capacity() / 8 + sizeof(v);
  }


  
  inline
  unsigned int memory_consumption (const std::vector<int> &v)
  {
    return (v.capacity() * sizeof(int) +
	    sizeof(v));
  }
    
    

  inline
  unsigned int memory_consumption (const std::vector<double> &v)
  {
    return (v.capacity() * sizeof(double) +
	    sizeof(v));
  }
    
    

  inline
  unsigned int memory_consumption (const std::vector<float> &v)
  {
    return (v.capacity() * sizeof(float) +
	    sizeof(v));
  }
    
    
	
  inline
  unsigned int memory_consumption (const std::vector<char> &v)
  {
    return (v.capacity() * sizeof(char) +
	    sizeof(v));
  }
    

    
  inline
  unsigned int memory_consumption (const std::vector<unsigned char> &v)
  {
    return (v.capacity() * sizeof(unsigned char) +
	    sizeof(v));
  }


    
  template <typename T>
  inline
  unsigned int memory_consumption (const std::vector<T *> &v)
  {
    return (v.capacity() * sizeof(T *) +
	    sizeof(v));
  }
    

				    
  template <typename A, typename B>
  inline
  unsigned int memory_consumption (const std::pair<A,B> &p)
  {
    return (memory_consumption(p.first) +
	    memory_consumption(p.second));
  }

  
		
  template <typename T>
  inline
  unsigned int
  memory_consumption (const T * const)
  {
    return sizeof(T*);
  }


		
  template <typename T>
  inline
  unsigned int
  memory_consumption (T * const)
  {
    return sizeof(T*);
  }

  
	
  inline
  unsigned int
  memory_consumption (void * const)
  {
    return sizeof(void*);
  }
    
    
	
  template <typename T>
  inline
  unsigned int
  memory_consumption (const T &t)
  {
    return t.memory_consumption();
  }
}

/// @endif

#endif
