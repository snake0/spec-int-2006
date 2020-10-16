//----------------------------  vectors.h  ---------------------------
//    $Id: vectors.h,v 1.1 2004/09/14 00:53:36 wolf Exp $
//    Version: $Name:  $
//
//    Copyright (C) 1998, 1999, 2000, 2001, 2002, 2003, 2004 by the deal.II authors
//
//    This file is subject to QPL and may not be  distributed
//    without copyright and license information. Please refer
//    to the file deal.II/doc/license.html for the  text  and
//    further information on this license.
//
//----------------------------  vectors.h  ---------------------------
#ifndef __deal2__vectors_h
#define __deal2__vectors_h


#include <base/config.h>
#include <base/exceptions.h>
#include <dofs/function_map.h>
#include <map>
#include <vector>
#include <set>

template <int dim> class Point;
template <int dim> class Function;
template <int dim> class FunctionMap;
template <int dim> class Quadrature;
template <int dim> class QGauss2;

template <typename number> class Vector;
template <typename number> class FullMatrix;
template <int dim> class Mapping;
template <int dim> class DoFHandler;
class ConstraintMatrix;


//TODO: Move documentation of functions to the functions!
//TODO: (Re)move the basic course on Sobolev spaces

/**
 * Provide a class which offers some operations on vectors. Amoung
 * these are assembling of standard vectors, integration of the
 * difference of a finite element solution and a continuous function,
 * interpolations and projections of continuous functions to the
 * finite element space and other operations.
 *
 * @note There exist two versions of almost each function. One with a
 * Mapping argument and one without. If a code uses a mapping
 * different from MappingQ1 the functions <b>with</b> mapping argument
 * should be used. Code that uses only MappingQ1 may also use the
 * functions without Mapping argument. Each of these latter functions
 * create a MappingQ1 object and just call the respective functions
 * with that object as mapping argument. The functions without Mapping
 * argument still exist to ensure backward compatibility. Nevertheless
 * it is advised to change the user's codes to store a specific
 * Mapping object and to use the functions that take this Mapping
 * object as argument. This gives the possibility to easily extend the
 * user codes to work also on mappings of higher degree, this just by
 * exchanging MappingQ1 by, for example, a MappingQ or another Mapping
 * object of interest.
 *
 * @section VectorTools1 Description of operations
 *
 * This collection of methods offers the following operations:
 * <ul>
 * <li> Interpolation: assign each degree of freedom in the vector to be
 *   the value of the function given as argument. This is identical to
 *   saying that the resulting finite element function (which is
 *   isomorphic to the output vector) has exact function values in all
 *   support points of trial functions. The support point of a trial
 *   function is the point where its value equals one, e.g. for linear
 *   trial functions the support points are four corners of an
 *   element. This function therefore relies on the assumption that a
 *   finite element is used for which the degrees of freedom are
 *   function values (Lagrange elements) rather than gradients, normal
 *   derivatives, second derivatives, etc (Hermite elements, quintic
 *   Argyris element, etc.).
 *
 *   It seems inevitable that some values of the vector to be created are set
 *   twice or even more than that. The reason is that we have to loop over
 *   all cells and get the function values for each of the trial functions
 *   located thereon. This applies also to the functions located on faces and
 *   corners which we thus visit more than once. While setting the value
 *   in the vector is not an expensive operation, the evaluation of the
 *   given function may be, taking into account that a virtual function has
 *   to be called.
 *
 * <li> Projection: compute the <i>L<sup>2</sup></i>-projection of the given function onto
 *   the finite element space. This is done through the solution of the
 *   linear system of equations $M v = f$ where $M$ is the mass matrix
 *   $m_{ij} = \int_\Omega \phi_i(x) \phi_j(x) dx$ and
 *   $f_i = \int_\Omega f(x) \phi_i(x) dx$. The solution vector $v$ then is
 *   the projection.
 *
 *   In order to get proper results, it may necessary to treat
 *   boundary conditions right. Below are listed some cases where this
 *   may be needed.  If needed, this is done by <i>L<sup>2</sup></i>-projection of
 *   the trace of the given function onto the finite element space
 *   restricted to the boundary of the domain, then taking this
 *   information and using it to eliminate the boundary nodes from the
 *   mass matrix of the whole domain, using the
 *   MatrixTools::apply_boundary_values() function. The projection of
 *   the trace of the function to the boundary is done with the
 *   VectorTools::project_boundary_values() (see below) function,
 *   which is called with a map of boundary functions FunctioMap in
 *   which all boundary indicators from zero to 254 (255 is used for
 *   other purposes, see the Triangulation class documentation) point
 *   to the function to be projected. The projection to the boundary
 *   takes place using a second quadrature formula on the boundary
 *   given to the project() function. The first quadrature formula is
 *   used to compute the right hand side and for numerical quadrature
 *   of the mass matrix.
 *
 *   The projection of the boundary values first, then eliminating
 *   them from the global system of equations is not needed
 *   usually. It may be necessary if you want to enforce special
 *   restrictions on the boundary values of the projected function,
 *   for example in time dependent problems: you may want to project
 *   the initial values but need consistency with the boundary values
 *   for later times. Since the latter are projected onto the boundary
 *   in each time step, it is necessary that we also project the
 *   boundary values of the initial values, before projecting them to
 *   the whole domain.
 *
 *   Obviously, the results of the two schemes for projection are
 *   different.  Usually, when projecting to the boundary first, the
 *   <i>L<sup>2</sup></i>-norm of the difference between original
 *   function and projection over the whole domain will be larger
 *   (factors of five have been observed) while the
 *   <i>L<sup>2</sup></i>-norm of the error integrated over the
 *   boundary should of course be less. The reverse should also hold
 *   if no projection to the boundary is performed.
 *
 *   The selection whether the projection to the boundary first is
 *   needed is done with the <tt>project_to_boundary_first</tt> flag
 *   passed to the function.  If @p false is given, the additional
 *   quadrature formula for faces is ignored.
 *
 *   You should be aware of the fact that if no projection to the boundary
 *   is requested, a function with zero boundary values may not have zero
 *   boundary values after projection. There is a flag for this especially
 *   important case, which tells the function to enforce zero boundary values
 *   on the respective boundary parts. Since enforced zero boundary values
 *   could also have been reached through projection, but are more economically
 *   obtain using other methods, the @p project_to_boundary_first flag is
 *   ignored if the @p enforce_zero_boundary flag is set.
 *
 *   The solution of the linear system is presently done using a simple CG
 *   method without preconditioning and without multigrid. This is clearly not
 *   too efficient, but sufficient in many cases and simple to implement. This
 *   detail may change in the future.
 *
 * <li> Creation of right hand side vectors:
 *   The @p create_right_hand_side function computes the vector
 *   $f_i = \int_\Omega f(x) \phi_i(x) dx$. This is the same as what the
 *   <tt>MatrixCreator::create_*</tt> functions which take a right hand side do,
 *   but without assembling a matrix.
 *
 * <li> Creation of boundary right hand side vectors: The
 *   @p create_boundary_right_hand_side function computes the vector
 *   $f_i = \int_{\partial\Omega} g(x) \phi_i(x) dx$. This is the
 *   right hand side contribution of boundary forces when having
 *   inhomogeneous Neumann boundary values in Laplace's equation or
 *   other second order operators. This function also takes an
 *   optional argument denoting over which parts of the boundary the
 *   integration shall extend.
 *
 * <li> Interpolation of boundary values:
 *   The MatrixTools@p ::apply_boundary_values function takes a list
 *   of boundary nodes and their values. You can get such a list by interpolation
 *   of a boundary function using the @p interpolate_boundary_values function.
 *   To use it, you have to
 *   specify a list of pairs of boundary indicators (of type <tt>unsigned char</tt>;
 *   see the section in the documentation of the Triangulation class for more
 *   details) and the according functions denoting the dirichlet boundary values
 *   of the nodes on boundary faces with this boundary indicator.
 *
 *   Usually, all other boundary conditions, such as inhomogeneous Neumann values
 *   or mixed boundary conditions are handled in the weak formulation. No attempt
 *   is made to include these into the process of matrix and vector assembly therefore.
 *
 *   Within this function, boundary values are interpolated, i.e. a node is given
 *   the point value of the boundary function. In some cases, it may be necessary
 *   to use the L2-projection of the boundary function or any other method. For
 *   this purpose we refer to the VectorTools@p ::project_boundary_values
 *   function below.
 *
 *   You should be aware that the boundary function may be evaluated at nodes
 *   on the interior of faces. These, however, need not be on the true
 *   boundary, but rather are on the approximation of the boundary represented
 *   by the mapping of the unit cell to the real cell. Since this mapping will
 *   in most cases not be the exact one at the face, the boundary function is
 *   evaluated at points which are not on the boundary and you should make
 *   sure that the returned values are reasonable in some sense anyway.
 *
 *   In 1d the situation is a bit different since there faces (i.e. vertices) have
 *   no boundary indicator. It is assumed that if the boundary indicator zero
 *   is given in the list of boundary functions, the left boundary point is to be
 *   interpolated while the right boundary point is associated with the boundary
 *   index 1 in the map. The respective boundary functions are then evaluated at
 *   the place of the respective boundary point.
 *
 * <li> Projection of boundary values:
 *   The @p project_boundary_values function acts similar to the
 *   @p interpolate_boundary_values function, apart from the fact that it does
 *   not get the nodal values of boundary nodes by interpolation but rather
 *   through the <i>L<sup>2</sup></i>-projection of the trace of the function to the boundary.
 *
 *   The projection takes place on all boundary parts with boundary
 *   indicators listed in the map (FunctioMap@p ::FunctionMap)
 *   of boundary functions. These boundary parts may or may not be
 *   continuous. For these boundary parts, the mass matrix is
 *   assembled using the
 *   MatrixTools@p ::create_boundary_mass_matrix function, as
 *   well as the appropriate right hand side. Then the resulting
 *   system of equations is solved using a simple CG method (without
 *   preconditioning), which is in most cases sufficient for the
 *   present purpose.
 *
 * <li> Computing errors:
 *   The function @p integrate_difference performs the calculation of the error
 *   between the finite element solution and a given (continuous) reference
 *   function in different norms. The integration is performed using a given
 *   quadrature formulae and assumes that the given finite element objects equals
 *   that used for the computation of the solution.
 * 
 *   The result is stored in a vector (named @p difference), where each entry
 *   equals the given norm of the difference on a cell. The order of entries
 *   is the same as a @p cell_iterator takes when started with @p begin_active and
 *   promoted with the <tt>++</tt> operator.
 * 
 *   You can use the @p distribute_cell_to_dof_vector function of the
 *   DoFHandler class to convert cell based data to a data
 *   vector with values on the degrees of freedom, which can then be
 *   added to a DataOut object to be printed. But also you can
 *   add a cell based data vector itself to a DataOut object,
 *   see the @p DataOut::add_data_vector functions.
 * 
 *   Presently, there is the possibility to compute the following values from the
 *   difference, on each cell: @p mean, @p L1_norm, @p L2_norm, @p Linfty_norm,
 *   @p H1_seminorm and @p H1_norm, see @p VectorTools::NormType.
 *   For the mean difference value, the reference function minus the numerical
 *   solution is computed, not the other way round.
 *
 *   The infinity norm of the difference on a given cell returns the maximum
 *   absolute value of the difference at the quadrature points given by the
 *   quadrature formula parameter. This will in some cases not be too good
 *   an approximation, since for example the Gauss quadrature formulae do
 *   not evaluate the difference at the end or corner points of the cells.
 *   You may want to choose a quadrature formula with more quadrature points
 *   or one with another distribution of the quadrature points in this case.
 *   You should also take into account the superconvergence properties of finite
 *   elements in some points: for example in 1D, the standard finite element
 *   method is a collocation method and should return the exact value at nodal
 *   points. Therefore, the trapezoidal rule should always return a vanishing
 *   L-infinity error. Conversely, in 2D the maximum L-infinity error should
 *   be located at the vertices or at the center of the cell, which would make
 *   it plausible to use the Simpson quadrature rule. On the other hand, there
 *   may be superconvergence at Gauss integration points. These examples are not
 *   intended as a rule of thumb, rather they are thought to illustrate that the
 *   use of the wrong quadrature formula may show a significantly wrong result
 *   and care should be taken to chose the right formula.
 *
 *   The <i>H<sup>1</sup></i> seminorm is the <i>L<sup>2</sup></i>
 *   norm of the gradient of the difference. The square of the full
 *   <i>H<sup>1</sup></i> norm is the sum of the square of seminorm
 *   and the square of the <i>L<sup>2</sup></i> norm.
 * 
 *   To get the global <i>L<sup>1</sup></i> error, you have to sum up the
 *   entries in @p difference, e.g. using
 *   <tt>Vector<double>::l1_norm</tt> function.  For the global <i>L<sup>2</sup></i>
 *   difference, you have to sum up the squares of the entries and
 *   take the root of the sum, e.g. using
 *   <tt>Vector<double>::l2_norm</tt>.  These two operations
 *   represent the $l_1$ and $l_2$ norms of the vectors, but you need
 *   not take the absolute value of each entry, since the cellwise
 *   norms are already positive.
 *  
 *   To get the global mean difference, simply sum up the elements as above.
 *   To get the $L_\infty$ norm, take the maximum of the vector elements, e.g.
 *   using the <tt>Vector<double>::linfty_norm</tt> function.
 *
 *   For the global <i>H<sup>1</sup></i> norm and seminorm, the same rule applies as for the
 *   <i>L<sup>2</sup></i> norm: compute the $l_2$ norm of the cell error vector.
 * </ul>
 *
 * All functions use the finite element given to the DoFHandler object the last
 * time that the degrees of freedom were distributed over the triangulation. Also,
 * if access to an object describing the exact form of the boundary is needed, the
 * pointer stored within the triangulation object is accessed.
 *
 * @author Wolfgang Bangerth, Ralf Hartmann, Guido Kanschat, 1998, 1999, 2000, 2001
 */
class VectorTools
{
  public:

				     /**
				      *  Denote which norm/integral is
				      *  to be computed by the
				      *  integrate_difference()
				      *  function of this class. The
				      *  following possibilities are
				      *  implemented:
				     */
    enum NormType {
					   /**
					    * The function or
					    * difference of functions
					    * is integrated on each
					    * cell.
					    */
	  mean,
					   /**
					    * The absolute value of
					    * the function is
					    * integrated.
					    */
	  L1_norm,
					   /**
					    * The square of the
					    * function is integrated
					    * and the the square root
					    * of the result is
					    * computed on each cell.
					    */
	  L2_norm,
					   /**
					    * The maximum absolute
					    * value of the function.
					    */
	  Linfty_norm,
					   /**
					    * #L2_norm of the gradient.
					    */
	  H1_seminorm,
					   /**
					    * The square of this norm
					    * is the square of the
					    * #L2_norm plus the square
					    * of the #H1_seminorm.
					    */
	  H1_norm,
					   /**
					    * The absolute value to
					    * the <i>p</i>th power is
					    * integrated and the pth
					    * root is computed on each
					    * cell. The exponent
					    * <i>p</i> is the last
					    * parameter of the
					    * function.
					    */
	  Lp_norm,
					   /**
					    * #Lp_norm of the gradient.
					    */
	  W1p_seminorm,
					   /**
					    * same as #H1_norm for
					    * <i>L<sup>p</sup></i>.
					    */
	  W1p_norm
    };
    
				     /**
				      * Compute the interpolation of
				      * @p function at the support
				      * points to the finite element
				      * space. It is assumed that the
				      * number of components of
				      * @p function matches that of
				      * the finite element used by
				      * @p dof.
				      *
				      * Note that you may have to call
				      * <tt>hanging_nodes.distribute(vec)</tt>
				      * with the hanging nodes from
				      * space @p dof afterwards, to
				      * make the result continuous
				      * again.
				      *
				      * See the general documentation
				      * of this class for further
				      * information.
				      */
    template <int dim, class VECTOR>
    static void interpolate (const Mapping<dim>    &mapping,
			     const DoFHandler<dim> &dof,
			     const Function<dim>   &function,
			     VECTOR                &vec);
    
				     /**
				      * Calls the @p interpolate
				      * function above with
				      * <tt>mapping=MappingQ1@<dim>@()</tt>.
				      */
    template <int dim, class VECTOR>
    static void interpolate (const DoFHandler<dim> &dof,
			     const Function<dim>   &function,
			     VECTOR                &vec);

				     /**
				      * Interpolate different finite
				      * element spaces. The
				      * interpolation of vector
				      * @p data_1 is executed from the
				      * FE space represented by
				      * @p dof_1 to the vector @p data_2
				      * on FE space @p dof_2. The
				      * interpolation on each cell is
				      * represented by the matrix
				      * @p transfer. Curved boundaries
				      * are neglected so far.
				      *
				      * Note that you may have to call
				      * <tt>hanging_nodes.distribute(data_2)</tt>
				      * with the hanging nodes from
				      * space @p dof_2 afterwards, to
				      * make the result continuous
				      * again.
				      */
    template <int dim, class InVector, class OutVector>
    static void interpolate (const DoFHandler<dim>    &dof_1,
			     const DoFHandler<dim>    &dof_2,
			     const FullMatrix<double> &transfer,
			     const InVector           &data_1,
			     OutVector                &data_2);
			  
				     /**
				      * Compute the projection of
				      * @p function to the finite element space.
				      *
				      * By default, projection to the boundary
				      * and enforcement of zero boundary values
				      * are disabled. The ordering of arguments
				      * to this function is such that you need
				      * not give a second quadrature formula if
				      * you don't want to project to the
				      * boundary first, but that you must if you
				      * want to do so.
				      *
				      * This function needs the mass
				      * matrix of the finite element
				      * space on the present grid. To
				      * this end, the mass matrix is
				      * assembled exactly using the
				      * @p create_mass_matrix
				      * function in the
				      * MatrixTools
				      * collection. This function
				      * performs numerical quadrature
				      * using the given quadrature
				      * rule; you should therefore
				      * make sure that the given
				      * quadrature formula is also
				      * sufficient for the integration
				      * of the mass matrix.
				      *
				      * See the general documentation of this
				      * class for further information.
				      */
    template <int dim>
    static void project (const Mapping<dim>       &mapping,
			 const DoFHandler<dim>    &dof,
			 const ConstraintMatrix   &constraints,
			 const Quadrature<dim>    &quadrature,
			 const Function<dim>      &function,
			 Vector<double>           &vec,
			 const bool                enforce_zero_boundary = false,
			 const Quadrature<dim-1>  &q_boundary = QGauss2<dim-1>(),
			 const bool                project_to_boundary_first = false);

				     /**
				      * Declaration of specialization
				      * of the previous function for
				      * 1d. At present, it is not
				      * implemented.
				      *
				      * The default value of the
				      * quadrature formula is an
				      * invalid object.
				      */
    static void project (const Mapping<1>         &mapping,
			 const DoFHandler<1>      &dof,
			 const ConstraintMatrix   &constraints,
			 const Quadrature<1>      &quadrature,
			 const Function<1>        &function,
			 Vector<double>           &vec,
			 const bool                enforce_zero_boundary = false,
			 const Quadrature<0>      &q_boundary = *invalid_face_quadrature,
			 const bool                project_to_boundary_first = false);
    
				     /**
				      * Calls the @p project
				      * function, see above, with
				      * <tt>mapping=MappingQ1@<dim@>()</tt>.
				      */
    template <int dim>
    static void project (const DoFHandler<dim>    &dof,
			 const ConstraintMatrix   &constraints,
			 const Quadrature<dim>    &quadrature,
			 const Function<dim>      &function,
			 Vector<double>           &vec,
			 const bool                enforce_zero_boundary = false,
			 const Quadrature<dim-1>  &q_boundary = QGauss2<dim-1>(),
			 const bool                project_to_boundary_first = false);

				     /**
				      * Create a right hand side
				      * vector. Prior content of the
				      * given @p rhs_vector vector is
				      * deleted.
				      *
				      * See the general documentation of this
				      * class for further information.
				      */				      
    template <int dim>
    static void create_right_hand_side (const Mapping<dim>    &mapping,
					const DoFHandler<dim> &dof,
					const Quadrature<dim> &q,
					const Function<dim>   &rhs,
					Vector<double>        &rhs_vector);
    
				     /**
				      * Calls the @p create_right_hand_side
				      * function, see above, with
				      * <tt>mapping=MappingQ1@<dim@>()</tt>.
				      */
    template <int dim>
    static void create_right_hand_side (const DoFHandler<dim> &dof,
					const Quadrature<dim> &q,
					const Function<dim>   &rhs,
					Vector<double>        &rhs_vector);

				     /**
				      * Create a right hand side
				      * vector from boundary
				      * forces. Prior content of the
				      * given @p rhs_vector vector is
				      * deleted.
				      *
				      * See the general documentation of this
				      * class for further information.
				      */
    template <int dim>
    static void create_boundary_right_hand_side (const Mapping<dim>      &mapping,
						 const DoFHandler<dim>   &dof,
						 const Quadrature<dim-1> &q,
						 const Function<dim>     &rhs,
						 Vector<double>          &rhs_vector,
						 const std::set<unsigned char> &boundary_indicators = std::set<unsigned char>());

				     /**
				      * Specialization of above
				      * function for 1d. Since the
				      * computation is not useful in
				      * 1d, this function simply
				      * throws an exception.
				      */
    static void create_boundary_right_hand_side (const Mapping<1>    &mapping,
						 const DoFHandler<1> &dof,
						 const Quadrature<0> &q,
						 const Function<1>   &rhs,
						 Vector<double>      &rhs_vector,
						 const std::set<unsigned char> &boundary_indicators = std::set<unsigned char>());
    
				     /**
				      * Calls the
				      * @p create_boundary_right_hand_side
				      * function, see above, with
				      * <tt>mapping=MappingQ1@<dim@>()</tt>.
				      */
    template <int dim>
    static void create_boundary_right_hand_side (const DoFHandler<dim>   &dof,
						 const Quadrature<dim-1> &q,
						 const Function<dim>     &rhs,
						 Vector<double>          &rhs_vector,
						 const std::set<unsigned char> &boundary_indicators = std::set<unsigned char>());
    
				     /**
				      * Prepare Dirichlet boundary
				      * conditions.  Make up the list
				      * of degrees of freedom subject
				      * to Dirichlet boundary
				      * conditions and the values to
				      * be assigned to them, by
				      * interpolation around the
				      * boundary. If the
				      * @p boundary_values contained
				      * values before, the new ones
				      * are added, or the old ones
				      * overwritten if a node of the
				      * boundary part to be used
				      * was already in the
				      * map of boundary values.
				      *
				      * The parameter
				      * @p boundary_component
				      * corresponds to the number
				      * @p boundary_indicator of the
				      * face.  255 is an illegal
				      * value, since it is reserved
				      * for interior faces.
				      *
				      * The flags in the last
				      * parameter, @p component_mask
				      * denote which components of the
				      * finite element space shall be
				      * interpolated. If it is left as
				      * specified by the default value
				      * (i.e. an empty array), all
				      * components are
				      * interpolated. If it is
				      * different from the default
				      * value, it is assumed that the
				      * number of entries equals the
				      * number of components in the
				      * boundary functions and the
				      * finite element, and those
				      * components in the given
				      * boundary function will be used
				      * for which the respective flag
				      * was set in the component mask.
				      *
				      * It is assumed that the number
				      * of components of the function
				      * in @p boundary_function matches that
				      * of the finite element used by
				      * @p dof.
				      *
				      * If the finite element used has
				      * shape functions that are
				      * non-zero in more than one
				      * component (in deal.II speak:
				      * they are non-primitive), then
				      * these components can presently
				      * not be used for interpolating
				      * boundary values. Thus, the
				      * elements in the component mask
				      * corresponding to the
				      * components of these
				      * non-primitive shape functions
				      * must be @p false.
				      *
				      * See the general doc for more
				      * information.
				      */
    template <int dim>
    static void interpolate_boundary_values (const Mapping<dim>            &mapping,
					     const DoFHandler<dim>         &dof,
					     const typename FunctionMap<dim>::type &function_map,
					     std::map<unsigned int,double> &boundary_values,
					     const std::vector<bool>       &component_mask = std::vector<bool>());

				     /**
				      * Declaration of specialization
				      * of the previous function for
				      * 1d.
				      */
    static void interpolate_boundary_values (const Mapping<1>              &mapping,
					     const DoFHandler<1>           &dof,
					     const FunctionMap<1>::type    &function_map,
					     std::map<unsigned int,double> &boundary_values,
					     const std::vector<bool>       &component_mask = std::vector<bool>());

				     /**
				      * Same function as above, but
				      * taking only one pair of
				      * boundary indicator and
				      * corresponding boundary
				      * function. Calls the other
				      * function with remapped
				      * arguments.
				      *
				      * This function is there mainly
				      * for backward compatibility.
				      */
    template <int dim>
    static void interpolate_boundary_values (const Mapping<dim>            &mapping,
					     const DoFHandler<dim>         &dof,
					     const unsigned char            boundary_component,
					     const Function<dim>           &boundary_function,
					     std::map<unsigned int,double> &boundary_values,
					     const std::vector<bool>       &component_mask = std::vector<bool>());

				     /**
				      * Declaration of specialization
				      * of the previous function for
				      * 1d.
				      */
    static void interpolate_boundary_values (const Mapping<1>              &mapping,
					     const DoFHandler<1>           &dof,
					     const unsigned char            boundary_component,
					     const Function<1>             &boundary_function,
					     std::map<unsigned int,double> &boundary_values,
					     const std::vector<bool>       &component_mask = std::vector<bool>());
    
				     /**
				      * Calls the other
				      * @p interpolate_boundary_values
				      * function, see above, with
				      * <tt>mapping=MappingQ1@<dim@>()</tt>.
				      */
    template <int dim>
    static void interpolate_boundary_values (const DoFHandler<dim>         &dof,
					     const unsigned char            boundary_component,
					     const Function<dim>           &boundary_function,
					     std::map<unsigned int,double> &boundary_values,
					     const std::vector<bool>       &component_mask = std::vector<bool>());

				     /**
				      * Calls the other
				      * @p interpolate_boundary_values
				      * function, see above, with
				      * <tt>mapping=MappingQ1@<dim@>()</tt>.
				      */
    template <int dim>
    static void interpolate_boundary_values (const DoFHandler<dim>         &dof,
					     const typename FunctionMap<dim>::type &function_map,
					     std::map<unsigned int,double> &boundary_values,
					     const std::vector<bool>       &component_mask = std::vector<bool>());

    
				     /**
				      * Project @p function to the boundary
				      * of the domain, using the given quadrature
				      * formula for the faces. If the
				      * @p boundary_values contained values
				      * before, the new ones are added, or
				      * the old one overwritten if a node
				      * of the boundary part to be projected
				      * on already was in the variable.
				      *
				      * It is assumed that the number
				      * of components of
				      * @p boundary_function
				      * matches that of the finite
				      * element used by @p dof.
				      *
				      * See the general documentation of this
				      * class for further information.
				      */
    template <int dim>
    static void project_boundary_values (const Mapping<dim>       &mapping,
					 const DoFHandler<dim>    &dof,
					 const typename FunctionMap<dim>::type &boundary_functions,
					 const Quadrature<dim-1>  &q,
					 std::map<unsigned int,double> &boundary_values);

				     /**
				      * Declaration of specialization
				      * of the previous function for
				      * 1d. Since in 1d projection
				      * equals interpolation, the
				      * interpolation function is
				      * called.
				      */
    static void project_boundary_values (const Mapping<1>           &mapping,
					 const DoFHandler<1>        &dof,
					 const FunctionMap<1>::type &boundary_functions,
					 const Quadrature<0>        &q,
					 std::map<unsigned int,double> &boundary_values);
    
				     /**
				      * Calls the @p project_boundary_values
				      * function, see above, with
				      * <tt>mapping=MappingQ1@<dim@>()</tt>.
				      */
    template <int dim>
    static void project_boundary_values (const DoFHandler<dim>    &dof,
					 const typename FunctionMap<dim>::type &boundary_function,
					 const Quadrature<dim-1>  &q,
					 std::map<unsigned int,double> &boundary_values);
    
    				     /**
				      * Compute the error of the finite element solution.
				      * Integrate the difference between
				      * a finite element function and
				      * a reference function, which
				      * is given as a continuous function
				      * object.
				      *
				      * The value of @p exponent is
				      * used for computing $L^p$-norms
				      * and $W^{1,p}$-norms.
				      *
				      * The additional argument
				      * @p weight allows to evaluate
				      * weighted norms.  The weight
				      * function may be
				      * one-dimensional, establishing
				      * a weight in the domain. It
				      * also may have as many
				      * components as the finite
				      * element function: Then,
				      * different components get
				      * different weights.  This can
				      * be applied for instant with
				      * the characteristic function of
				      * a subset of the domain or a
				      * weight function selecting only
				      * some components of the
				      * solution. The weight function
				      * is expected to be positive,
				      * but negative values are not
				      * filtered. By default, no
				      * weighting function is given,
				      * i.e. weight=1 in the whole
				      * domain.
				      *
				      * It is assumed that the number
				      * of components of the function
				      * @p exact_solution matches that
				      * of the finite element used by
				      * @p dof.
				      *
				      * See the general documentation of this
				      * class for more information.
				      */
    template <int dim, class InVector, class OutVector>
    static void integrate_difference (const Mapping<dim>    &mapping,
				      const DoFHandler<dim> &dof,
				      const InVector        &fe_function,
				      const Function<dim>   &exact_solution,
				      OutVector             &difference,
				      const Quadrature<dim> &q,
				      const NormType        &norm,
				      const Function<dim>   *weight=0,
				      const double exponent = 2.);

				     /**
				      * Calls the @p integrate_difference
				      * function, see above, with
				      * <tt>mapping=MappingQ1@<dim@>()</tt>.
				      */
    template <int dim, class InVector, class OutVector>
    static void integrate_difference (const DoFHandler<dim> &dof,
				      const InVector        &fe_function,
				      const Function<dim>   &exact_solution,
				      OutVector             &difference,
				      const Quadrature<dim> &q,
				      const NormType        &norm,
				      const Function<dim>   *weight=0,
				      const double exponent = 2.);

				     /**
				      * Point error evaluation. Find
				      * the first cell containing the
				      * given point and compute the
				      * difference of a (possibly
				      * vector-valued) finite element
				      * function and a continuous
				      * function (with as many vector
				      * components as the finite
				      * element) at this point.
				      *
				      * Since the function uses a
				      * simple test for checking
				      * whether a point is in a cell,
				      * it is only implemented for
				      * Q1-mapping yet.
				      */
    template <int dim, class InVector>
    static void point_difference (const DoFHandler<dim>& dof,
				  const InVector&        fe_function,
				  const Function<dim>&   exact_solution,
				  Vector<double>&        difference,
				  const Point<dim>&      point);
    
				     /**
				      * Mean-value filter for Stokes.
				      * The pressure in Stokes'
				      * equations with only Dirichlet
				      * boundaries for the velocities
				      * is determined up to a constant
				      * only. This function allows to
				      * subtract the mean value of the
				      * pressure. It is usually called
				      * in a preconditioner and
				      * generates updates with mean
				      * value zero. The mean value is
				      * understood in the l1-sense.
				      *
				      * Apart from the vector @p v to
				      * operate on, this function
				      * takes a bit vector. This has a
				      * true entry for every component
				      * for which the mean value shall
				      * be computed and later
				      * subtracted.
				      */
    static void subtract_mean_value(Vector<double>          &v,
				    const std::vector<bool> &p_select);
    
				     /**
				      * Compute the mean value of one
				      * component of the solution.
				      *
				      * This function integrates the
				      * chosen component over the
				      * whole domain and returns the
				      * result.
				      *
				      * Subtracting this mean value
				      * from the node vector does not
				      * generally yield the desired
				      * result of a finite element
				      * function with mean value
				      * zero. In fact, it only works
				      * for Lagrangian
				      * elements. Therefore, it is
				      * necessary to compute the mean
				      * value and subtract it in the
				      * evaluation routine.
				      *
				      * So far, this is needed only in
				      * the error evaluation for
				      * Stokes with complete Dirichlet
				      * boundaries for the velocities.
				      */
    template <int dim, class InVector>
    static double compute_mean_value (const Mapping<dim>    &mapping,
				      const DoFHandler<dim> &dof,
				      const Quadrature<dim> &quadrature,
				      const InVector        &v,
				      const unsigned int     component);
    
				     /**
				      * Calls the @p compute_mean_value
				      * function, see above, with
				      * <tt>mapping=MappingQ1@<dim@>()</tt>.
				      */
    template <int dim, class InVector>
    static double compute_mean_value (const DoFHandler<dim> &dof,
				      const Quadrature<dim> &quadrature,
				      const InVector        &v,
				      const unsigned int     component);

				     /**
				      * Exception
				      */
    DeclException0 (ExcNotUseful);
				     /**
				      * Exception
				      */
    DeclException0 (ExcInvalidFE);
				     /**
				      * Exception
				      */
    DeclException0 (ExcFENotPrimitive);
				     /**
				      * Exception
				      */
    DeclException0 (ExcInvalidBoundaryIndicator);
				     /**
				      * Exception
				      */
    DeclException0 (ExcComponentMismatch);
				     /**
				      * Exception
				      */
    DeclException0 (ExcNonInterpolatingFE);

  private:
				     /**
				      * Null pointer used to 
				      * denote invalid face 
				      * quadrature formulas in 1d.
 				      */
    static const Quadrature<0> * const invalid_face_quadrature;
};



#endif
