//----------------------------  mg_dof_accessor.h  ---------------------------
//    $Id: mg_dof_accessor.h,v 1.1 2004/09/14 00:53:35 wolf Exp $
//    Version: $Name:  $
//
//    Copyright (C) 1998, 1999, 2000, 2001, 2002, 2003, 2004 by the deal.II authors
//
//    This file is subject to QPL and may not be  distributed
//    without copyright and license information. Please refer
//    to the file deal.II/doc/license.html for the  text  and
//    further information on this license.
//
//----------------------------  mg_dof_accessor.h  ---------------------------
#ifndef __deal2__mg_dof_accessor_h
#define __deal2__mg_dof_accessor_h


#include <base/config.h>
#include <dofs/dof_accessor.h>

template <int dim> class MGDoFHandler;

template <int celldim, int dim> class MGDoFObjectAccessor;
template <int dim>              class MGDoFObjectAccessor<0, dim>;
template <int dim>              class MGDoFObjectAccessor<1, dim>;
template <int dim>              class MGDoFObjectAccessor<2, dim>;
template <int dim>              class MGDoFObjectAccessor<3, dim>;


/**
 * Define the basis for accessors to the degrees of freedom for
 * a multigrid DoF handler object.
 *
 * Note that it is allowed to construct an object of which the
 * @p mg_dof_handler pointer is a Null pointer. Such an object would
 * result in a strange kind of behaviour, though every reasonable
 * operating system should disallow access through that pointer.
 * The reason we do not check for the null pointer in the
 * constructor which gets passed the DoFHandler pointer is that
 * if we did we could not make dof iterators member of other classes
 * (like in the @p FEValues class) if we did not know about the
 * DoFHandler object to be used upon construction of that object.
 * Through the way this class is implemented here, we allow the
 * creation of a kind of virgin object which only gets useful if
 * assigned to from another object before first usage.
 *
 * Opposite to construction, it is not possible to copy an object
 * which has an invalid dof handler pointer. This is to guarantee
 * that every iterator which is once assigned to is a valid
 * object. However, this assertion only holds in debug mode, when
 * the @p Assert macro is switched on.
 *
 * @author Wolfgang Bangerth, 1998
 */
template <int dim>
class MGDoFAccessor {
  public:
				     /**
				      * Constructor
				      */
    MGDoFAccessor () : mg_dof_handler(0) {
      Assert (false, ExcInvalidObject());
    };

				     /**
				      * This should be the default constructor.
				      * We cast away the @p constness of the
				      * pointer which clearly is EVIL but
				      * we can't help without making all
				      * functions which could somehow use
				      * iterators (directly or indirectly) make
				      * non-const, even if they preserve
				      * constness.
				      */
    MGDoFAccessor (const MGDoFHandler<dim> *mg_dof_handler) :
		    mg_dof_handler(const_cast<MGDoFHandler<dim>*>(mg_dof_handler)) {};

				     /**
				      * Reset the DoF handler pointer.
				      */
    void set_mg_dof_handler (MGDoFHandler<dim> *dh) {
      Assert (dh != 0, typename DoFAccessor<dim>::ExcInvalidObject());
      mg_dof_handler = dh;
    };

    				     /**
				      * Exception for child classes
				      */
    DeclException0 (ExcInvalidObject);

				     /**
				      * Copy operator.
				      */
    MGDoFAccessor<dim> & operator = (const MGDoFAccessor<dim> &da) {
      set_dof_handler (da.mg_dof_handler);
      return *this;
    };

  protected:
				     /**
				      * Store the address of the @p MGDoFHandler object
				      * to be accessed.
				      */
    MGDoFHandler<dim> *mg_dof_handler;  
};


/* -------------------------------------------------------------------------- */

/**
 * This is a switch class which only declares a @p typdef. It is meant to
 * determine which class a @p MGDoFObjectAccessor class is to be derived
 * from. By default, <tt>MGDoFObjectAccessor<celldim,dim></tt> derives from
 * the @p typedef in the general <tt>MGDoFObjectAccessor_Inheritance<celldim,dim></tt>
 * class, which is <tt>DoFObjectAccessor<celldim,dim></tt>,
 * but if <tt>celldim==dim</tt>, then the specialization <tt>MGDoFObjectAccessor_Inheritance<dim,dim></tt>
 * is used which declares its local type to be <tt>DoFCellAccessor<dim></tt>. Therefore,
 * the inheritance is automatically chosen to be from @p DoFCellAccessor if the
 * object under consideration has full dimension, i.e. constitutes a cell.
 *
 * @author Wolfgang Bangerth, 1999
 */
template <int celldim, int dim>
class MGDoFObjectAccessor_Inheritance 
{
  public:
				     /**
				      * Declaration of the @p typedef.
				      * See the full documentation for
				      * more information.
				      */
    typedef DoFObjectAccessor<celldim,dim> BaseClass;
};


/**
 * This is a switch class which only declares a @p typdef. It is meant to
 * determine which class a @p DoFObjectAccessor class is to be derived
 * from. By default, <tt>DoFObjectAccessor<celldim,dim></tt> derives from
 * the @p typedef in the general <tt>DoFObjectAccessor_Inheritance<celldim,dim></tt>
 * class, which is <tt>TriaObjectAccessor<celldim,dim></tt>,
 * but if <tt>celldim==dim</tt>, then the specialization <tt>TriaObjectAccessor<dim,dim></tt>
 * is used which declares its local type to be <tt>CellAccessor<dim></tt>. Therefore,
 * the inheritance is automatically chosen to be from @p CellAccessor if the
 * object under consideration has full dimension, i.e. constitutes a cell.
 *
 * @author Wolfgang Bangerth, 1999
 */
template <int dim>
class MGDoFObjectAccessor_Inheritance<dim,dim>
{
  public:
				     /**
				      * Declaration of the @p typedef.
				      * See the full documentation for
				      * more information.
				      */
    typedef DoFCellAccessor<dim> BaseClass;
};


/* -------------------------------------------------------------------------- */


/**
 * Common template for line, quad, hex.
 *
 * Internal: inheritance is necessary for the general template due to
 * a compiler error.
 * @author Guido Kanschat, 1999
 */
template <int celldim, int dim>
class MGDoFObjectAccessor :  public MGDoFAccessor<dim>,
			     public MGDoFObjectAccessor_Inheritance<celldim, dim>::BaseClass
{
};


/**
 * Closure class.
 */
template<int dim>
class MGDoFObjectAccessor<0, dim>
{
  public:
    typedef void AccessorData;

				     /**
				      * Constructor. Should never be called
				      * and thus throws an error.
				      */
    MGDoFObjectAccessor (const Triangulation<dim> *,
			 const int,
			 const int,
			 const AccessorData *)
      {
	Assert (false, ExcInternalError());
      }
};


/**
 * Grant access to the degrees of freedom located on lines.
 * This class follows mainly the route laid out by the accessor library
 * declared in the triangulation library (TriaAccessor). It enables
 * the user to access the degrees of freedom on the lines (there are similar
 * versions for the DoFs on quads, etc), where the dimension of the underlying
 * triangulation does not really matter (i.e. this accessor works with the
 * lines in 1D-, 2D-, etc dimensions).
 *
 *
 * @sect3{Usage}
 *
 * The DoFDimensionInfo classes inherited by the DoFHandler classes
 * declare typedefs to iterators using the accessors declared in this class
 * hierarchy tree. Usage is best to happens through these typedefs, since they
 * are more secure to changes in the class naming and template interface as well
 * as they provide easier typing (much less complicated names!).
 * 
 * 
 * @sect3{Notes about the class hierarchy structure}
 *
 * Inheritance from <tt>MGDoFObjectAccessor_Inheritance<1,dim>::BaseClass</tt> yields
 * inheritance from <tt>DoFCellAccessor<1></tt> if <tt>dim==1</tt> and from
 * <tt>TriaObjectAccessor<1,dim></tt> for all other @p dim values. Thus, an object
 * of this class shares all features of cells in one dimension, but behaves
 * like an ordinary line in all other cases.
 *
 * @author Wolfgang Bangerth, 1998
 */
template <int dim>
class MGDoFObjectAccessor<1, dim> :  public MGDoFAccessor<dim>,
				     public MGDoFObjectAccessor_Inheritance<1, dim>::BaseClass
{
  public:
				     /**
				      * Declare the data type that this accessor
				      * class expects to get passed from the
				      * iterator classes.
				      */
    typedef MGDoFHandler<dim> AccessorData;
    
				     /**
				      * Default constructor, unused thus
				      * not implemented.
				      */
    MGDoFObjectAccessor ();
    
    				     /**
				      * Constructor. The @p local_data
				      * argument is assumed to be a pointer
				      * to a <tt>MGDoFHandler<dim></tt> object.
				      */
    MGDoFObjectAccessor (const Triangulation<dim> *tria,
			 const int                 level,
			 const int                 index,
			 const AccessorData       *local_data);
    
				     /**
				      * Return the index of the @p ith degree
				      * of freedom of this line on the level
				      * this line lives on.
				      */
    unsigned int mg_dof_index (const unsigned int i) const;

    				     /**
				      * Set the index of the @p ith degree
				      * of freedom of this line on the
				      * level this line lives on to @p index.
				      */
    void set_mg_dof_index (const unsigned int i,
			   const unsigned int index) const;

				     /**
				      * Return the index of the @p ith degree
				      * on the @p vertexth vertex for the
				      * level this line lives on.
				      */
    unsigned int mg_vertex_dof_index (const unsigned int vertex,
				      const unsigned int i) const;

				     /**
				      * Set the index of the @p ith degree
				      * on the @p vertexth vertex to @p index
				      * for the level this line lives on.
				      */
    void set_mg_vertex_dof_index (const unsigned int vertex,
				  const unsigned int i,
				  const unsigned int index) const;

    				     /**
				      * Return the indices of the dofs of this
				      * line in the standard ordering: dofs
				      * on vertex 0, dofs on vertex 1, 
				      * dofs on line.
				      *
				      * It is assumed that the vector already
				      * has the right size beforehand. The
				      * indices refer to the local numbering
				      * for the level this line lives on.
				      */
    void get_mg_dof_indices (std::vector<unsigned int> &dof_indices) const;

    				     /**
				      * Return the value of the given vector
				      * restricted to the dofs of this
				      * cell in the standard ordering: dofs
				      * on vertex 0, dofs on vertex 1, etc,
				      * dofs on line 0, dofs on line 1, etc,
				      * dofs on quad 0, etc.
				      *
				      * It is assumed that the vector already
				      * has the right size beforehand. The
				      * indices refer to the multilevel
				      * numbering local to the present
				      * level of this cell. The vector shall
				      * therefore have the same number of
				      * entries as there are degrees of
				      * freedom on this level.
				      */
    template <typename number>
    void get_mg_dof_values (const Vector<number> &values,
			    Vector<number>       &dof_values) const;

				     /**
				      * Return the @p ith child as a MGDoF line
				      * iterator. This function is needed since
				      * the child function of the base
				      * class returns a line accessor without
				      * access to the MG data.
				      */
    TriaIterator<dim,MGDoFObjectAccessor<1, dim> > child (const unsigned int) const;
    
				     /**
				      * Implement the copy operator needed
				      * for the iterator classes.
				      */
    void copy_from (const MGDoFObjectAccessor<1, dim> &a);
};


/**
 * Grant access to the multilevel degrees of freedom located on quads.
 *
 * DoFLineAccessor
 */
template <int dim>
class MGDoFObjectAccessor<2, dim> :  public MGDoFAccessor<dim>,
				     public MGDoFObjectAccessor_Inheritance<2, dim>::BaseClass
{
  public:
				     /**
				      * Declare the data type that this accessor
				      * class expects to get passed from the
				      * iterator classes.
				      */
    typedef MGDoFHandler<dim> AccessorData;

				     /**
				      * Default constructor, unused thus
				      * not implemented.
				      */
    MGDoFObjectAccessor ();

    				     /**
				      * Constructor. The @p local_data
				      * argument is assumed to be a pointer
				      * to a DoFHandler object.
				      */
    MGDoFObjectAccessor (const Triangulation<dim> *tria,
			 const int                 level,
			 const int                 index,
			 const AccessorData       *local_data);
    
				     /**
				      * Return the index of the @p ith degree
				      * of freedom of this quad on the level
				      * this quad lives on.
				      */
    unsigned int mg_dof_index (const unsigned int i) const;

    				     /**
				      * Set the index of the @p ith degree
				      * of freedom of this quad on the
				      * level this quad lives on to @p index.
				      */
    void set_mg_dof_index (const unsigned int i,
			   const unsigned int index) const;

				     /**
				      * Return the index of the @p ith degree
				      * on the @p vertexth vertex for the level
				      * this quad lives on.
				      */
    unsigned int mg_vertex_dof_index (const unsigned int vertex,
				      const unsigned int i) const;

				     /**
				      * Set the index of the @p ith degree
				      * on the @p vertexth vertex for the
				      * level this quad lives on to @p index.
				      */
    void set_mg_vertex_dof_index (const unsigned int vertex,
				  const unsigned int i,
				  const unsigned int index) const;

    				     /**
				      * Return the indices of the dofs of this
				      * quad in the standard ordering: dofs
				      * on vertex 0, dofs on vertex 1, etc,
				      * dofs on line 0, dofs on line 1, etc,
				      * dofs on quad 0, etc.
				      *
				      * It is assumed that the vector already
				      * has the right size beforehand. The
				      * indices refer to the local numbering
				      * for the level this quad lives on.
				      */
    void get_mg_dof_indices (std::vector<unsigned int> &dof_indices) const;
 
    				     /**
				      * Return the value of the given vector
				      * restricted to the dofs of this
				      * cell in the standard ordering: dofs
				      * on vertex 0, dofs on vertex 1, etc,
				      * dofs on line 0, dofs on line 1, etc,
				      * dofs on quad 0, etc.
				      *
				      * It is assumed that the vector already
				      * has the right size beforehand. The
				      * indices refer to the multilevel
				      * numbering local to the present
				      * level of this cell. The vector shall
				      * therefore have the same number of
				      * entries as there are degrees of
				      * freedom on this level.
				      */
    template <typename number>
    void get_mg_dof_values (const Vector<number> &values,
			    Vector<number>       &dof_values) const;

   				     /**
				      * Return a pointer to the @p ith line
				      * bounding this @p Quad.
				      */
    TriaIterator<dim,MGDoFObjectAccessor<1, dim> >
    line (const unsigned int i) const;

				     /**
				      * Return the @p ith child as a DoF quad
				      * iterator. This function is needed since
				      * the child function of the base
				      * class returns a quad accessor without
				      * access to the DoF data.
				      */
    TriaIterator<dim,MGDoFObjectAccessor<2, dim> >
    child (const unsigned int) const;
    
				     /**
				      * Implement the copy operator needed
				      * for the iterator classes.
				      */
    void copy_from (const MGDoFObjectAccessor<2, dim> &a);
};


/**
 * Grant access to the multilevel degrees of freedom located on hexhedra.
 *
 * DoFLineAccessor
 */
template <int dim>
class MGDoFObjectAccessor<3, dim> :  public MGDoFAccessor<dim>,
				     public MGDoFObjectAccessor_Inheritance<3, dim>::BaseClass
{
  public:
				     /**
				      * Declare the data type that this accessor
				      * class expects to get passed from the
				      * iterator classes.
				      */
    typedef MGDoFHandler<dim> AccessorData;

				     /**
				      * Default constructor, unused thus
				      * not implemented.
				      */
    MGDoFObjectAccessor ();

    				     /**
				      * Constructor. The @p local_data
				      * argument is assumed to be a pointer
				      * to a DoFHandler object.
				      */
    MGDoFObjectAccessor (const Triangulation<dim> *tria,
			 const int                 level,
			 const int                 index,
			 const AccessorData       *local_data);
    
				     /**
				      * Return the index of the @p ith degree
				      * of freedom of this hex on the level
				      * this quad lives on.
				      */
    unsigned int mg_dof_index (const unsigned int i) const;

    				     /**
				      * Set the index of the @p ith degree
				      * of freedom of this hex on the
				      * level this hex lives on to @p index.
				      */
    void set_mg_dof_index (const unsigned int i,
			   const unsigned int index) const;

				     /**
				      * Return the index of the @p ith degree
				      * on the @p vertexth vertex for the level
				      * this hex lives on.
				      */
    unsigned int mg_vertex_dof_index (const unsigned int vertex,
				      const unsigned int i) const;

				     /**
				      * Set the index of the @p ith degree
				      * on the @p vertexth vertex for the
				      * level this hex lives on to @p index.
				      */
    void set_mg_vertex_dof_index (const unsigned int vertex,
				  const unsigned int i,
				  const unsigned int index) const;

    				     /**
				      * Return the indices of the dofs of this
				      * hex in the standard ordering: dofs
				      * on vertex 0, dofs on vertex 1, etc,
				      * dofs on line 0, dofs on line 1, etc,
				      * dofs on quad 0, etc.
				      *
				      * It is assumed that the vector already
				      * has the right size beforehand. The
				      * indices refer to the local numbering
				      * for the level this hex lives on.
				      */
    void get_mg_dof_indices (std::vector<unsigned int> &dof_indices) const;

    				     /**
				      * Return the value of the given vector
				      * restricted to the dofs of this
				      * cell in the standard ordering: dofs
				      * on vertex 0, dofs on vertex 1, etc,
				      * dofs on line 0, dofs on line 1, etc,
				      * dofs on quad 0, etc.
				      *
				      * It is assumed that the vector already
				      * has the right size beforehand. The
				      * indices refer to the multilevel
				      * numbering local to the present
				      * level of this cell. The vector shall
				      * therefore have the same number of
				      * entries as there are degrees of
				      * freedom on this level.
				      */
    template <typename number>
    void get_mg_dof_values (const Vector<number> &values,
			    Vector<number>       &dof_values) const;

    				     /**
				      * Return a pointer to the @p ith line
				      * bounding this @p Hex.
				      */
    TriaIterator<dim,MGDoFObjectAccessor<1, dim> >
    line (const unsigned int i) const;

				     /**
				      * Return a pointer to the @p ith quad
				      * bounding this @p Hex.
				      */
    TriaIterator<dim,MGDoFObjectAccessor<2, dim> >
    quad (const unsigned int i) const;

				     /**
				      * Return the @p ith child as a DoF quad
				      * iterator. This function is needed since
				      * the child function of the base
				      * class returns a hex accessor without
				      * access to the DoF data.
				      */
    TriaIterator<dim,MGDoFObjectAccessor<3, dim> > child (const unsigned int) const;
    
				     /**
				      * Implement the copy operator needed
				      * for the iterator classes.
				      */
    void copy_from (const MGDoFObjectAccessor<3, dim> &a);
};


/**
 * Grant access to the degrees of freedom on a cell. In fact, since all
 * access to the degrees of freedom has been enabled by the classes
 * <tt>DoFObjectAccessor<1, 1></tt> and <tt>DoFObjectAccessor<2, 2></tt> for the space dimension
 * one and two, respectively, this class only collects the pieces
 * together by deriving from the appropriate <tt>DoF*Accessor</tt> and the
 * right <tt>CellAccessor<dim></tt> and finally adding two functions which give
 * access to the neighbors and children as @p DoFCellAccessor objects
 * rather than @p CellAccessor objects (the latter function was inherited
 * from the <tt>CellAccessor<dim></tt> class).
 *
 * @author Wolfgang Bangerth, 1998
 */
template <int dim>
class MGDoFCellAccessor :  public MGDoFObjectAccessor<dim, dim> {
  public:
				     /**
				      * Type of faces.
				      */
    typedef
    TriaIterator<dim, MGDoFObjectAccessor<dim-1, dim> >
    face_iterator;
				     /**
				      * Declare the data type that this accessor
				      * class expects to get passed from the
				      * iterator classes.
				      */
    typedef typename MGDoFObjectAccessor<dim, dim>::AccessorData  AccessorData;
    
    				     /**
				      * Constructor
				      */
    MGDoFCellAccessor (const Triangulation<dim> *tria,
		       const int                 level,
		       const int                 index,
		       const AccessorData       *local_data) :
		    MGDoFObjectAccessor<dim, dim> (tria,level,index,local_data) {};

				     /**
				      * Return the @p ith neighbor as a MGDoF cell
				      * iterator. This function is needed since
				      * the neighbor function of the base
				      * class returns a cell accessor without
				      * access to the MGDoF data.
				      */
    TriaIterator<dim,MGDoFCellAccessor<dim> > neighbor (const unsigned int) const;

    				     /**
				      * Return the @p ith child as a MGDoF cell
				      * iterator. This function is needed since
				      * the child function of the base
				      * class returns a cell accessor without
				      * access to the DoF data.
				      */
    TriaIterator<dim,MGDoFCellAccessor<dim> > child (const unsigned int) const;

    				     /**
				      * Return an iterator to the @p ith face
				      * of this cell.
				      *
				      * This function is not implemented in 1D,
				      * and maps to MGDoFObjectAccessor<2, dim>::line in 2D.
				      */
    face_iterator
    face (const unsigned int i) const;

				     /**
				      * Return the result of the
				      * @p neighbor_child_on_subface
				      * function of the base class,
				      * but convert it so that one can
				      * also access the MGDoF data (the
				      * function in the base class
				      * only returns an iterator with
				      * access to the triangulation
				      * data).
				      */
    TriaIterator<dim,MGDoFCellAccessor<dim> >
    neighbor_child_on_subface (const unsigned int face_no,
                               const unsigned int subface_no) const;

    				     /**
				      *  Exception
				      */
    DeclException0 (ExcNotUsefulForThisDimension);
};


template<> MGDoFCellAccessor<1>::face_iterator
MGDoFCellAccessor<1>::face (const unsigned int i) const;
template<> MGDoFCellAccessor<2>::face_iterator
MGDoFCellAccessor<2>::face (const unsigned int i) const;
template<> MGDoFCellAccessor<3>::face_iterator
MGDoFCellAccessor<3>::face (const unsigned int i) const;


#endif
