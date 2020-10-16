//----------------------------  mapping_q.h  ---------------------------
//    $Id: mapping_q.h,v 1.1 2004/09/14 00:53:33 wolf Exp $
//    Version: $Name:  $
//
//    Copyright (C) 2000, 2001, 2002, 2003, 2004 by the deal.II authors
//
//    This file is subject to QPL and may not be  distributed
//    without copyright and license information. Please refer
//    to the file deal.II/doc/license.html for the  text  and
//    further information on this license.
//
//----------------------------  mapping_q.h  ---------------------------
#ifndef __deal2__mapping_q_h
#define __deal2__mapping_q_h


#include <base/config.h>
#include <base/table.h>
#include <fe/mapping_q1.h>

template <int dim> class TensorProductPolynomials;


/*!@addtogroup fe */
/*@{*/

/**
 * Mapping class that uses Qp-mappings on boundary cells. The mapping
 * shape functions make use of tensor product polynomials with
 * equidistant (on the unit cell) support points.
 *
 * For more details about Qp-mappings, see the `mapping' report at
 * <tt>deal.II/doc/reports/mapping_q/index.html</tt> in the `Reports'
 * section of `Documentation'.
 *
 * @author Ralf Hartmann, Guido Kanschat 2000, 2001
 */
template <int dim>
class MappingQ : public MappingQ1<dim>
{
  public:
				     /**
				      * Constructor.  @p p gives the
				      * degree of mapping polynomials
				      * on boundary cells.
				      */
    MappingQ (const unsigned int p);

				     /**
				      * Destructor.
				      */
    virtual ~MappingQ ();
    
				     /**
				      * Transforms the point @p p on
				      * the unit cell to the point
				      * @p p_real on the real cell
				      * @p cell and returns @p p_real.
				      */
    virtual Point<dim>
    transform_unit_to_real_cell (
      const typename Triangulation<dim>::cell_iterator &cell,
      const Point<dim>                                 &p) const;
    
				     /**
				      * Transforms the point @p p on
				      * the real cell to the point
				      * @p p_unit on the unit cell
				      * @p cell and returns @p p_unit.
				      *
				      * Uses Newton iteration and the
				      * @p transform_unit_to_real_cell
				      * function.
				      */
    virtual Point<dim>
    transform_real_to_unit_cell (
      const typename Triangulation<dim>::cell_iterator &cell,
      const Point<dim>                                 &p) const;

				     /**
				      * Implementation of the
				      * interface in Mapping.
				      */
    virtual void
    transform_covariant (Tensor<1,dim>          *begin,
			 Tensor<1,dim>          *end,
			 const Tensor<1,dim>    *src,
			 const typename Mapping<dim>::InternalDataBase &internal) const;
    
				     /**
				      * Implementation of the
				      * interface in Mapping.
				      */
    virtual void
    transform_covariant (Tensor<2,dim>          *begin,
			 Tensor<2,dim>          *end,
			 const Tensor<2,dim>    *src,
			 const typename Mapping<dim>::InternalDataBase &internal) const;
    
				     /**
				      * Implementation of the
				      * interface in Mapping.
				      */
    virtual void
    transform_contravariant (Tensor<1,dim>          *begin,
			     Tensor<1,dim>          *end,
			     const Tensor<1,dim>    *src,
			     const typename Mapping<dim>::InternalDataBase &internal) const;    

				     /**
				      * Implementation of the
				      * interface in Mapping.
				      */
    virtual void
    transform_contravariant (Tensor<2,dim>          *begin,
			     Tensor<2,dim>          *end,
			     const Tensor<2,dim>    *src,
			     const typename Mapping<dim>::InternalDataBase &internal) const;    
    
				     /**
				      * Return the degree of the
				      * mapping, i.e. the value which
				      * was passed to the constructor.
				      */
    unsigned int get_degree () const;
    
				     /** 
				      * Storage for internal data of
				      * Q_degree transformation.
				      */
    class InternalData : public MappingQ1<dim>::InternalData
    {
      public:
					 /**
					  * Constructor.
					  */
	InternalData (const unsigned int n_shape_functions);
	

					 /**
					  * Return an estimate (in
					  * bytes) or the memory
					  * consumption of this
					  * object.
					  */
	virtual unsigned int memory_consumption () const;

					 /**
					  * Unit normal vectors. Used
					  * for the alternative
					  * computation of the normal
					  * vectors. See doc of the
					  * @p alternative_normals_computation
					  * flag.
					  *
					  * Filled (hardcoded) once in
					  * @p get_face_data.
					  */
        std::vector<std::vector<Point<dim> > > unit_normals;

					 /**
					  * Flag that is set by the
					  * <tt>fill_fe_[[sub]face]_values</tt>
					  * function.
					  *
					  * If this flag is @p true
					  * we are on an interior cell
					  * and the
					  * @p mapping_q1_data is
					  * used.
					  */
	bool use_mapping_q1_on_current_cell;
	
					 /**
					  * On interior cells
					  * @p MappingQ1 is used.
					  */
	typename MappingQ1<dim>::InternalData mapping_q1_data;
    };

  protected:
				     /**
				      * Implementation of the interface in
				      * Mapping.
				      */
    virtual void
    fill_fe_values (const typename DoFHandler<dim>::cell_iterator &cell,
		    const Quadrature<dim>                &quadrature,
		    typename Mapping<dim>::InternalDataBase &mapping_data,
		    typename std::vector<Point<dim> >             &quadrature_points,
		    std::vector<double>                  &JxW_values) const ;

				     /**
				      * Implementation of the interface in
				      * Mapping.
				      */
    virtual void
    fill_fe_face_values (const typename DoFHandler<dim>::cell_iterator &cell,
			 const unsigned int face_no,
			 const Quadrature<dim-1>& quadrature,
			 typename Mapping<dim>::InternalDataBase &mapping_data,
			 typename std::vector<Point<dim> >        &quadrature_points,
			 std::vector<double>             &JxW_values,
			 typename std::vector<Tensor<1,dim> >        &exterior_form,
			 typename std::vector<Point<dim> >        &normal_vectors) const ;

				     /**
				      * Implementation of the interface in
				      * Mapping.
				      */
    virtual void
    fill_fe_subface_values (const typename DoFHandler<dim>::cell_iterator &cell,
			    const unsigned int face_no,
			    const unsigned int sub_no,
			    const Quadrature<dim-1>& quadrature,
			    typename Mapping<dim>::InternalDataBase &mapping_data,
			    typename std::vector<Point<dim> >        &quadrature_points,
			    std::vector<double>             &JxW_values,
			    typename std::vector<Tensor<1,dim> >        &exterior_form,
			    typename std::vector<Point<dim> >        &normal_vectors) const ;

				     /**
				      * For <tt>dim=2,3</tt>. Append the
				      * support points of all shape
				      * functions located on bounding
				      * lines to the vector
				      * @p a. Points located on the
				      * line but not on vertices are not
				      * included.
				      *
				      * Needed by the
				      * @p compute_support_points_laplace
				      * function . For <tt>dim=1</tt> this
				      * function is empty.
				      *
				      * This function is made virtual
				      * in order to allow derived
				      * classes to choose shape
				      * function support points
				      * differently than the present
				      * class, which chooses the
				      * points as interpolation points
				      * on the boundary.
				      */
    virtual void
    add_line_support_points (const typename Triangulation<dim>::cell_iterator &cell,
			     std::vector<Point<dim> > &a) const;

				     /**
				      * For <tt>dim=3</tt>. Append the
				      * support points of all shape
				      * functions located on bounding
				      * faces (quads in 3d) to the
				      * vector @p a. Points located
				      * on the quad but not on vertices
				      * are not included.
				      *
				      * Needed by the
				      * @p compute_support_points_laplace
				      * function. For <tt>dim=1</tt> and
				      * <tt>dim=2</tt> this function is
				      * empty.
				      *
				      * This function is made virtual
				      * in order to allow derived
				      * classes to choose shape
				      * function support points
				      * differently than the present
				      * class, which chooses the
				      * points as interpolation points
				      * on the boundary.
				      */
    virtual void
    add_quad_support_points(const typename Triangulation<dim>::cell_iterator &cell,
			    std::vector<Point<dim> > &a) const;
    
  private:
    
				     /**
				      * Implementation of the interface in
				      * Mapping.
				      */
    virtual
    typename Mapping<dim>::InternalDataBase *
    get_data (const UpdateFlags,
	      const Quadrature<dim>& quadrature) const;

				     /**
				      * Implementation of the interface in
				      * Mapping.
				      */
    virtual
    typename Mapping<dim>::InternalDataBase *
    get_face_data (const UpdateFlags flags,
		   const Quadrature<dim-1>& quadrature) const;

				     /**
				      * Implementation of the interface in
				      * Mapping.
				      */
    virtual
    typename Mapping<dim>::InternalDataBase *
    get_subface_data (const UpdateFlags flags,
		      const Quadrature<dim-1>& quadrature) const;
    
				     /**
				      * Compute shape values and/or
				      * derivatives.
				      */
    virtual void
    compute_shapes_virtual (const std::vector<Point<dim> > &unit_points,
			    typename MappingQ1<dim>::InternalData &data) const;

				     /**
				      * This function is needed by the
				      * constructor of <tt>MappingQ<dim></tt>
				      * for <tt>dim=</tt> 2 and 3.
				      *
				      * For <tt>degree<4</tt> this function
				      * sets the
				      * @p laplace_on_quad_vector to
				      * the hardcoded data. For
				      * <tt>degree>=4</tt> and MappingQ<2>
				      * this vector is computed.
				      *
				      * For the definition of the
				      * @p laplace_on_quad_vector
				      * please refer to equation (8)
				      * of the `mapping' report.
				      */
    void
    set_laplace_on_quad_vector(Table<2,double> &loqvs) const;
    
				     /**
				      * This function is needed by the
				      * constructor of <tt>MappingQ<3></tt>.
				      *
				      * For <tt>degree==2</tt> this function
				      * sets the
				      * @p laplace_on_hex_vector to
				      * the hardcoded data. For
				      * <tt>degree>2</tt> this vector is
				      * computed.
				      *
				      * For the definition of the
				      * @p laplace_on_hex_vector
				      * please refer to equation (8)
				      * of the `mapping' report.
				      */
    void set_laplace_on_hex_vector(Table<2,double> &lohvs) const;
    
				     /**
				      * Computes the
				      * <tt>laplace_on_quad(hex)_vector</tt>.
				      *
				      * Called by the
				      * <tt>set_laplace_on_quad(hex)_vector</tt>
				      * functions if the data is not
				      * yet hardcoded.
				      *
				      * For the definition of the
				      * <tt>laplace_on_quad(hex)_vector</tt>
				      * please refer to equation (8)
				      * of the `mapping' report.
				      */
    void compute_laplace_vector(Table<2,double> &lvs) const;

				     /**
				      * Takes a
				      * <tt>laplace_on_hex(quad)_vector</tt>
				      * and applies it to the vector
				      * @p a to compute the inner
				      * support points as a linear
				      * combination of the exterior
				      * points.
				      *
				      * The vector @p a initially
				      * contains the locations of the
				      * @p n_outer points, the
				      * @p n_inner computed inner
				      * points are appended.
				      *
				      * See equation (7) of the
				      * `mapping' report.
				      */
    void apply_laplace_vector(const Table<2,double>   &lvs,
			      std::vector<Point<dim> > &a) const;
    
				     /**
				      * Computes the support points of
				      * the mapping.
				      */
    virtual void compute_mapping_support_points(
      const typename Triangulation<dim>::cell_iterator &cell,
      std::vector<Point<dim> > &a) const;

				     /**
				      * Computes all support points of
				      * the mapping shape
				      * functions. The inner support
				      * points (ie. support points in
				      * quads for 2d, in hexes for 3d)
				      * are computed using the
				      * solution of a Laplace equation
				      * with the position of the outer
				      * support points as boundary
				      * values, in order to make the
				      * transformation as smooth as
				      * possible.
				      */
    void compute_support_points_laplace(
      const typename Triangulation<dim>::cell_iterator &cell,
      std::vector<Point<dim> > &a) const;
    
				     /**
				      * Needed by the
				      * @p laplace_on_quad function
				      * (for <tt>dim==2</tt>). Filled by the
				      * constructor.
				      *
				      * Sizes:
				      * laplace_on_quad_vector.size()=
				      *   number of inner
				      *   unit_support_points
				      * laplace_on_quad_vector[i].size()=
				      *   number of outer
				      *   unit_support_points, i.e.
				      *   unit_support_points on the
				      *   boundary of the quad
				      *
				      * For the definition of this
				      * vector see equation (8) of the
				      * `mapping' report.
				      */
    Table<2,double> laplace_on_quad_vector;
    
				     /**
				      * Needed by the
				      * @p laplace_on_hex function
				      * (for <tt>dim==3</tt>). Filled by the
				      * constructor.
				      *
				      * For the definition of this
				      * vector see equation (8) of the
				      * `mapping' report.
				      */
    Table<2,double> laplace_on_hex_vector;

				     /**
				      * Exception.
				      */
    DeclException1 (ExcLaplaceVectorNotSet,
		    int,
		    << "laplace_vector not set for degree=" << arg1 << ".");
     
				     /**
				      * Degree @p p of the
				      * polynomials used as shape
				      * functions for the Qp mapping
				      * of cells at the boundary.
				      */  
    const unsigned int degree;

				     /**
				      * Number of inner mapping shape
				      * functions.
				      */
    const unsigned int n_inner;

				     /**
				      * Number of mapping shape
				      * functions on the boundary.
				      */
    const unsigned int n_outer;
    
				     /**
				      * Pointer to the
				      * @p dim-dimensional tensor
				      * product polynomials used as
				      * shape functions for the Qp
				      * mapping of cells at the
				      * boundary.
				      */
    TensorProductPolynomials<dim> *tensor_pols;
    
    				     /**
				      * Number of the Qp tensor
				      * product shape functions.
				      */
    const unsigned int n_shape_functions;

				     /**
				      * Mapping from lexicographic to
				      * to the Qp shape function
				      * numbering. Its size is
				      * @p dofs_per_cell.
				      */
    std::vector<unsigned int> renumber;

				     /**
				      * If this flag is set @p true
				      * then @p MappingQ is used on
				      * all cells, not only on
				      * boundary cells.
				      *
				      * The default value is false.
				      *
				      * This flag is kept in the
				      * implementation to allow a fast
				      * switch between the two cases,
				      * as someone might want to do
				      * some comparative tests.
				      */
    static const bool use_mapping_q_on_all_cells = false;
};

/*@}*/

/* -------------- declaration of explicit specializations ------------- */

/// @if NoDoc

template<> MappingQ<1>::MappingQ (const unsigned int);
template<> MappingQ<1>::~MappingQ ();
template<> void MappingQ<1>::compute_shapes_virtual (
  const std::vector<Point<1> > &unit_points,
  MappingQ1<1>::InternalData   &data) const;
template <> void MappingQ<1>::set_laplace_on_quad_vector(
  Table<2,double> &) const;
template <> void MappingQ<3>::set_laplace_on_hex_vector(
  Table<2,double> &lohvs) const;
template <> void MappingQ<1>::compute_laplace_vector(
  Table<2,double> &) const;
template <> void MappingQ<1>::add_line_support_points (
  const Triangulation<1>::cell_iterator &,
  std::vector<Point<1> > &) const;
template<> void MappingQ<3>::add_quad_support_points(
  const Triangulation<3>::cell_iterator &cell,
  std::vector<Point<3> >                &a) const;

/// @endif

#endif
