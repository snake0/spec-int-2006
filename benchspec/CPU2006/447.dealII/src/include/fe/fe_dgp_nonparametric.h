//----------------------------------------------------------------
//    $Id: fe_dgp_nonparametric.h,v 1.1 2004/09/14 00:53:33 wolf Exp $
//    Version: $Name:  $
//
//    Copyright (C) 1998, 1999, 2000, 2001, 2002, 2003, 2004 by the deal.II authors
//
//    This file is subject to QPL and may not be  distributed
//    without copyright and license information. Please refer
//    to the file deal.II/doc/license.html for the  text  and
//    further information on this license.
//
//---------------------------------------------------------------
#ifndef __deal2__fe_dgp_nonparametric_h
#define __deal2__fe_dgp_nonparametric_h

#include <base/config.h>
#include <base/polynomial.h>
#include <base/polynomial_space.h>
#include <fe/fe.h>

template <int dim> class PolynomialSpace;
template <int dim> class MappingQ;


/*!@addtogroup fe */
/*@{*/

/**
 * Discontinuous finite elements evaluated at the mapped quadrature points.
 *
 * Warning: this class does not work properly, yet. Don't use it!
 *
 * This finite element implements complete polynomial spaces, that is,
 * $d$-dimensional polynomials of order $k$.
 *
 * The polynomials are not mapped. Therefore, they are constant,
 * linear, quadratic, etc. on any grid cell.
 *
 * Since the polynomials are evaluated at the quadrature points of the
 * actual grid cell, no grid transfer and interpolation matrices are
 * available.
 *
 * The purpose of this class is experimental, therefore the
 * implementation will remain incomplete.
 *
 * @author Guido Kanschat, 2002
 */
template <int dim>
class FE_DGPNonparametric : public FiniteElement<dim>
{
  public:
				     /**
				      * Constructor for tensor product
				      * polynomials of degree @p k.
				      */
    FE_DGPNonparametric (const unsigned int k);
    
				     /**
				      * Return a string that uniquely
				      * identifies a finite
				      * element. This class returns
				      * <tt>FE_DGPNonparametric<dim>(degree)</tt>,
				      * with @p dim and @p degree
				      * replaced by appropriate
				      * values.
				      */
    virtual std::string get_name () const;

				     /**
				      * Return the value of the
				      * @p ith shape function at the
				      * point @p p. See the
				      * FiniteElementBase base
				      * class for more information
				      * about the semantics of this
				      * function.
				      */
    virtual double shape_value (const unsigned int i,
			        const Point<dim> &p) const;

				     /**
				      * Return the value of the
				      * @p componentth vector
				      * component of the @p ith shape
				      * function at the point
				      * @p p. See the
				      * FiniteElementBase base
				      * class for more information
				      * about the semantics of this
				      * function.
				      *
				      * Since this element is scalar,
				      * the returned value is the same
				      * as if the function without the
				      * @p _component suffix were
				      * called, provided that the
				      * specified component is zero.
				      */
    virtual double shape_value_component (const unsigned int i,
					  const Point<dim> &p,
					  const unsigned int component) const;
    
				     /**
				      * Return the gradient of the
				      * @p ith shape function at the
				      * point @p p. See the
				      * FiniteElementBase base
				      * class for more information
				      * about the semantics of this
				      * function.
				      */
    virtual Tensor<1,dim> shape_grad (const unsigned int  i,
				      const Point<dim>   &p) const;

				     /**
				      * Return the gradient of the
				      * @p componentth vector
				      * component of the @p ith shape
				      * function at the point
				      * @p p. See the
				      * FiniteElementBase base
				      * class for more information
				      * about the semantics of this
				      * function.
				      *
				      * Since this element is scalar,
				      * the returned value is the same
				      * as if the function without the
				      * @p _component suffix were
				      * called, provided that the
				      * specified component is zero.
				      */
    virtual Tensor<1,dim> shape_grad_component (const unsigned int i,
						const Point<dim> &p,
						const unsigned int component) const;

				     /**
				      * Return the tensor of second
				      * derivatives of the @p ith
				      * shape function at point @p p
				      * on the unit cell.  See the
				      * FiniteElementBase base
				      * class for more information
				      * about the semantics of this
				      * function.
				      */
    virtual Tensor<2,dim> shape_grad_grad (const unsigned int  i,
					   const Point<dim> &p) const;
    
				     /**
				      * Return the second derivative
				      * of the @p componentth vector
				      * component of the @p ith shape
				      * function at the point
				      * @p p. See the
				      * FiniteElementBase base
				      * class for more information
				      * about the semantics of this
				      * function.
				      *
				      * Since this element is scalar,
				      * the returned value is the same
				      * as if the function without the
				      * @p _component suffix were
				      * called, provided that the
				      * specified component is zero.
				      */
    virtual Tensor<2,dim> shape_grad_grad_component (const unsigned int i,
						     const Point<dim> &p,
						     const unsigned int component) const;

				     /**
				      * Return the polynomial degree
				      * of this finite element,
				      * i.e. the value passed to the
				      * constructor.
				      */
    unsigned int get_degree () const;

				     /**
				      * Number of base elements in a
				      * mixed discretization. Since
				      * this is a scalar element,
				      * return one.
				      */
    virtual unsigned int n_base_elements () const;
    
				     /**
				      * Access to base element
				      * objects. Since this element is
				      * scalar, <tt>base_element(0)</tt> is
				      * @p this, and all other
				      * indices throw an error.
				      */
    virtual const FiniteElement<dim> &
    base_element (const unsigned int index) const;

                                     /**
                                      * Multiplicity of base element
                                      * @p index. Since this is a
                                      * scalar element,
                                      * <tt>element_multiplicity(0)</tt>
                                      * returns one, and all other
                                      * indices will throw an error.
                                      */
    virtual unsigned int element_multiplicity (const unsigned int index) const;
    
				     /**
				      * Check for non-zero values on a face.
				      *
				      * This function returns
				      * @p true, if the shape
				      * function @p shape_index has
				      * non-zero values on the face
				      * @p face_index.
				      *
				      * Implementation of the
				      * interface in
				      * FiniteElement
				      */
    virtual bool has_support_on_face (const unsigned int shape_index,
				      const unsigned int face_index) const;

				     /**
				      * Determine an estimate for the
				      * memory consumption (in bytes)
				      * of this object.
				      *
				      * This function is made virtual,
				      * since finite element objects
				      * are usually accessed through
				      * pointers to their base class,
				      * rather than the class itself.
				      */
    virtual unsigned int memory_consumption () const;


				     /**
				      * Declare a nested class which
				      * will hold static definitions of
				      * various matrices such as
				      * constraint and embedding
				      * matrices. The definition of
				      * the various static fields are
				      * in the files <tt>fe_dgp_[123]d.cc</tt>
				      * in the source directory.
				      */
    struct Matrices
    {
					 /**
					  * Pointers to the embedding
					  * matrices, one for each
					  * polynomial degree starting
					  * from constant elements
					  */
	static const double * const embedding[][GeometryInfo<dim>::children_per_cell];

					 /**
					  * Number of elements (first
					  * index) the above field
					  * has. Equals the highest
					  * polynomial degree plus one
					  * for which the embedding
					  * matrices have been
					  * computed.
					  */
	static const unsigned int n_embedding_matrices;

					 /**
					  * As @p embedding but for
					  * projection matrices.
					  */
	static const double * const projection_matrices[][GeometryInfo<dim>::children_per_cell];

					 /**
					  * As
					  * @p n_embedding_matrices
					  * but for projection
					  * matrices.
					  */
	static const unsigned int n_projection_matrices;
    };

  protected:

				     /**
				      * @p clone function instead of
				      * a copy constructor.
				      *
				      * This function is needed by the
				      * constructors of @p FESystem.
				      */
    virtual FiniteElement<dim> *clone() const;
  
				     /**
				      * Prepare internal data
				      * structures and fill in values
				      * independent of the cell.
				      */
    virtual
    typename Mapping<dim>::InternalDataBase *
    get_data (const UpdateFlags,
	      const Mapping<dim>& mapping,
	      const Quadrature<dim>& quadrature) const ;

				     /**
				      * Implementation of the same
				      * function in
				      * FiniteElement.
				      */
    virtual void
    fill_fe_values (const Mapping<dim> &mapping,
		    const typename DoFHandler<dim>::cell_iterator &cell,
		    const Quadrature<dim>                &quadrature,
		    typename Mapping<dim>::InternalDataBase      &mapping_internal,
		    typename Mapping<dim>::InternalDataBase      &fe_internal,
		    FEValuesData<dim>& data) const;
    
				     /**
				      * Implementation of the same
				      * function in
				      * FiniteElement.
				      */
    virtual void
    fill_fe_face_values (const Mapping<dim> &mapping,
			 const typename DoFHandler<dim>::cell_iterator &cell,
			 const unsigned int                    face_no,
			 const Quadrature<dim-1>                &quadrature,
			 typename Mapping<dim>::InternalDataBase      &mapping_internal,
			 typename Mapping<dim>::InternalDataBase      &fe_internal,
			 FEValuesData<dim>& data) const ;
    
				     /**
				      * Implementation of the same
				      * function in
				      * FiniteElement.
				      */
    virtual void
    fill_fe_subface_values (const Mapping<dim> &mapping,
			    const typename DoFHandler<dim>::cell_iterator &cell,
			    const unsigned int                    face_no,
			    const unsigned int                    sub_no,
			    const Quadrature<dim-1>                &quadrature,
			    typename Mapping<dim>::InternalDataBase      &mapping_internal,
			    typename Mapping<dim>::InternalDataBase      &fe_internal,
			    FEValuesData<dim>& data) const ;

  private:
    
				     /**
				      * Only for internal use. Its
				      * full name is
				      * @p get_dofs_per_object_vector
				      * function and it creates the
				      * @p dofs_per_object vector that is
				      * needed within the constructor to
				      * be passed to the constructor of
				      * @p FiniteElementData.
				      */
    static std::vector<unsigned int> get_dpo_vector(unsigned int degree);
    
				     /**
				      * Given a set of flags indicating
				      * what quantities are requested
				      * from a @p FEValues object,
				      * return which of these can be
				      * precomputed once and for
				      * all. Often, the values of
				      * shape function at quadrature
				      * points can be precomputed, for
				      * example, in which case the
				      * return value of this function
				      * would be the logical and of
				      * the input @p flags and
				      * @p update_values.
				      *
				      * For the present kind of finite
				      * element, this is exactly the
				      * case.
				      */
    virtual UpdateFlags update_once (const UpdateFlags flags) const;
  
				     /**
				      * This is the opposite to the
				      * above function: given a set of
				      * flags indicating what we want
				      * to know, return which of these
				      * need to be computed each time
				      * we visit a new cell.
				      *
				      * If for the computation of one
				      * quantity something else is
				      * also required (for example, we
				      * often need the covariant
				      * transformation when gradients
				      * need to be computed), include
				      * this in the result as well.
				      */
    virtual UpdateFlags update_each (const UpdateFlags flags) const;
  
				     /**
				      * Degree of the polynomials.
				      */  
    const unsigned int degree;

				     /**
				      * Pointer to an object
				      * representing the polynomial
				      * space used here.
				      */
    const PolynomialSpace<dim> polynomial_space;

				     /**
				      * Fields of cell-independent data.
				      *
				      * For information about the
				      * general purpose of this class,
				      * see the documentation of the
				      * base class.
				      */
    class InternalData : public FiniteElementBase<dim>::InternalDataBase
    {
      public:
				       // have some scratch arrays
      std::vector<double> values;
      std::vector<Tensor<1,dim> > grads;
      std::vector<Tensor<2,dim> > grad_grads;
    };
    
				     /**
				      * Allow access from other dimensions.
				      */
    template <int dim1> friend class FE_DGPNonparametric;

				     /**
				      * Allows @p MappingQ class to
				      * access to build_renumbering
				      * function.
				      */
    friend class MappingQ<dim>;
};

/*@}*/

/// @if NoDoc

// declaration of explicit specializations of member variables, if the
// compiler allows us to do that (the standard says we must)
#ifndef DEAL_II_MEMBER_VAR_SPECIALIZATION_BUG
template <> 
const double * const FE_DGPNonparametric<1>::Matrices::embedding[][GeometryInfo<1>::children_per_cell];

template <>
const unsigned int FE_DGPNonparametric<1>::Matrices::n_embedding_matrices;

template <>
const double * const FE_DGPNonparametric<1>::Matrices::projection_matrices[][GeometryInfo<1>::children_per_cell];

template <>
const unsigned int FE_DGPNonparametric<1>::Matrices::n_projection_matrices;

template <> 
const double * const FE_DGPNonparametric<2>::Matrices::embedding[][GeometryInfo<2>::children_per_cell];

template <>
const unsigned int FE_DGPNonparametric<2>::Matrices::n_embedding_matrices;

template <>
const double * const FE_DGPNonparametric<2>::Matrices::projection_matrices[][GeometryInfo<2>::children_per_cell];

template <>
const unsigned int FE_DGPNonparametric<2>::Matrices::n_projection_matrices;

template <> 
const double * const FE_DGPNonparametric<3>::Matrices::embedding[][GeometryInfo<3>::children_per_cell];

template <>
const unsigned int FE_DGPNonparametric<3>::Matrices::n_embedding_matrices;

template <>
const double * const FE_DGPNonparametric<3>::Matrices::projection_matrices[][GeometryInfo<3>::children_per_cell];

template <>
const unsigned int FE_DGPNonparametric<3>::Matrices::n_projection_matrices;
#endif

/// @endif

#endif
