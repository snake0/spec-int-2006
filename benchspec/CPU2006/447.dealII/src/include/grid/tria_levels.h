//----------------------------  tria_levels.h  ---------------------------
//    $Id: tria_levels.h,v 1.1 2004/09/14 00:53:34 wolf Exp $
//    Version: $Name:  $
//
//    Copyright (C) 1998, 1999, 2000, 2001, 2002, 2003, 2004 by the deal.II authors
//
//    This file is subject to QPL and may not be  distributed
//    without copyright and license information. Please refer
//    to the file deal.II/doc/license.html for the  text  and
//    further information on this license.
//
//----------------------------  tria_levels.h  ---------------------------
#ifndef __deal2__tria_levels_h
#define __deal2__tria_levels_h


#include <base/config.h>
#include <vector>
#include <grid/tria_line.h>
#include <grid/tria_quad.h>
#include <grid/tria_hex.h>
#include <base/point.h>


/**
 *  General template for information belonging to one level of a multilevel
 *  hierarchy of a triangulation. This template is only declared to allow
 *  specializations for different dimensions.
 */
template <int dim>
class TriangulationLevel
{
};


/**
 *  Store all information which belongs to one level of the multilevel hierarchy.
 *
 *  In TriangulationLevel<0> all data is stored which is not
 *  dependent on the dimension, e.g. a field to store the
 *  refinement flag for the cells (what a cell actually is
 *  is declared elsewhere), etc. Actually, it is only cell-based
 *  data, like neighborship info or refinement flags. There is another
 *  field, which may fit in here, namely the material data (for cells)
 *  or the boundary indicators (for faces), but since we need for a line
 *  or quad either boundary information or material data, we store them
 *  with the lines and quads rather than with the common data. We may,
 *  however lose some memory in three dimensions, when we need the
 *  material data for cell, boundary data for the quads, but nothing
 *  for the lines. Since we only store one byte per line, quad or hex,
 *  this is a minor loss and we can live with that.
 *
 *  @author Wolfgang Bangerth, 1998
 */
template <>
class TriangulationLevel<0>
{
  public:
				     /**
				      *  Flags for the cells whether they are
				      *  to be refined or not. The meaning
				      *  what a cell is, is dimension specific,
				      *  therefore also the length of this
				      *  vector depends on the dimension: in
				      *  one dimension, the length of this
				      *  vector equals the length of the
				      *  @p lines vector, in two dimensions
				      *  that of the @p quads vector, etc.
				      */
    std::vector<bool> refine_flags;

				     /**
				      * Same meaning as the one above, but
				      * specifies whether a cell must be
				      * coarsened.
				      */
    std::vector<bool> coarsen_flags;
    
				     /**
				      *  Levels and indices of the neighbors
				      *  of the cells. Convention is, that the
				      *  neighbors of the cell with index @p i
				      *  are stored in the fields following
				      *  $i*(2*real_space_dimension)$, e.g. in
				      *  one spatial dimension, the neighbors
				      *  of cell 0 are stored in <tt>neighbors[0]</tt>
				      *  and <tt>neighbors[1]</tt>, the neighbors of
				      *  cell 1 are stored in <tt>neighbors[2]</tt>
				      *  and <tt>neighbors[3]</tt>, and so on.
				      *
				      *  In neighbors, <tt>neighbors[i].first</tt> is
				      *  the level, while <tt>neighbors[i].first</tt>
				      *  is the index of the neighbor.
				      *
				      *  If a neighbor does not exist (cell is
				      *  at the boundary), <tt>level=index=-1</tt>
				      *  is set.
				      *
				      *  <em>Conventions:</em> The
				      *  @p ith neighbor of a cell is
				      *  the one which shares the
				      *  @p ith face (@p Line in 2D,
				      *  @p Quad in 3D) of this cell.
				      *
				      *  The neighbor of a cell has at most the
				      *  same level as this cell, i.e. it may
				      *  or may not be refined.
				      *
				      *  In one dimension, a neighbor may have
				      *  any level less or equal the level of
				      *  this cell. If it has the same level,
				      *  it may be refined an arbitrary number
				      *  of times, but the neighbor pointer
				      *  still points to the cell on the same
				      *  level, while the neighbors of the
				      *  childs of the neighbor may point to
				      *  this cell or its children.
				      *
				      *  In two and more dimensions, the
				      *  neighbor is either on the same level
				      *  and refined (in which case its children
				      *  have neighbor pointers to this cell or
				      *  its direct children), unrefined on
				      *  the same level or one level down (in
				      *  which case its neighbor pointer points
				      *  to the mother cell of this cell).
				      */
    std::vector<std::pair<int,int> > neighbors;

				     /**
				      * One integer per cell to store
				      * which subdomain it belongs
				      * to. This field is most often
				      * used in parallel computations,
				      * where it denotes which
				      * processor shall work on the
				      * cells with a given subdomain
				      * number.
				      */
    std::vector<unsigned int> subdomain_ids;
    
				     /**
				      *  Reserve enough space to accomodate
				      *  @p total_cells cells on this level.
				      *  Since there are no @p used flags on this
				      *  level, you have to give to total number
				      *  of cells, not only the number of newly
				      *  to accomodate ones, like in the
				      *  <tt>TriangulationLevel<N>::reserve_space</tt>
				      *  functions, with <tt>N>0</tt>.
				      *
				      *  Since the
				      *  number of neighbors per cell depends
				      *  on the dimensions, you have to pass
				      *  that additionally.
				      */
    void reserve_space (const unsigned int total_cells,
			const unsigned int dimension);

				     /**
				      *  Check the memory consistency of the
				      *  different containers. Should only be
				      *  called with the prepro flag @p DEBUG
				      *  set. The function should be called from
				      *  the functions of the higher
				      *  TriangulationLevel classes.
				      */
    void monitor_memory (const unsigned int true_dimension) const;

				     /**
				      * Determine an estimate for the
				      * memory consumption (in bytes)
				      * of this object.
				      */
    unsigned int memory_consumption () const;

				     /**
				      *  Exception
				      */
    DeclException3 (ExcMemoryWasted,
		    char*, int, int,
		    << "The container " << arg1 << " contains "
		    << arg2 << " elements, but it`s capacity is "
		    << arg3 << ".");
				     /**
				      *  Exception
				      */
    DeclException2 (ExcMemoryInexact,
		    int, int,
		    << "The containers have sizes " << arg1 << " and "
		    << arg2 << ", which is not as expected.");
};



/**
 *  Store all information which belongs to one level of the multilevel hierarchy.
 *  
 *  In one dimension, this is a list of the lines associated with this level,
 *  as well as a list with the indices of the children of these lines.
 *  The TriangulationsLevel objects of higher dimensions are derived from
 *  this one.
 *
 *  @author Wolfgang Bangerth, 1998
 */
template <>
class TriangulationLevel<1> : public TriangulationLevel<0>
{
  private:

				     /**
				      *  This subclass groups together all that
				      *  is needed to describe the lines on one
				      *  level.
				      */
    struct LinesData
    {
					 /**
					  *  Vector of the lines belonging to
					  *  this level. The index of the line
					  *  on this level equals the index in
					  *  this container, while the global
					  *  index of a line is stored in the
					  *  line itself.
					  */
	std::vector<Line> lines;
					 /**
					  *  Index of the first child of a line
					  *  in the list on the next level.
					  *  Since when lines are refined, both
					  *  children are created at the same
					  *  time, they are appended to the list
					  *  on the next level after each other.
					  *  We therefore only store the index
					  *  of the first child, the second
					  *  follows immediately afterwards.
					  *
					  *  If a line has no children, -1 is
					  *  stored in this list. A line is
					  *  called active if it has no
					  *  children. The function
					  *  TriaAccessor::has_children()
					  *  tests for this.
					  */
	std::vector<int>  children;
	
					 /**
					  *  Vector storing whether a line is
					  *  used in the @p lines vector.
					  *
					  *  Since it is difficult to delete
					  *  elements in a @p vector, when an
					  *  element is not needed any more
					  *  (e.g. after derefinement), it is
					  *  not deleted from the list, but
					  *  rather the according @p used flag
					  *  is set to @p false.
					  */
	std::vector<bool> used;

					 /**
					  *  Make available a field for user data,
					  *  one bit per line. This field is usually
					  *  used when an operation runs over all
					  *  cells and needs information whether
					  *  another cell (e.g. a neighbor) has
					  *  already been processed.
					  *
					  *  You can clear all used flags using
					  *  Triangulation::clear_user_flags().
					  */
	std::vector<bool> user_flags;

					 /**
					  * Store boundary and material data. In
					  * one dimension, this field stores
					  * the material id of a line, which is a
					  * number between 0 and 254. In more
					  * than one dimension, lines have no
					  * material id, but they may be at the
					  * boundary; then, we store the
					  * boundary indicator in this field,
					  * which denotes to which part of the
					  * boundary this line belongs and which
					  * boundary conditions hold on this
					  * part. The boundary indicator also
					  * is a number between zero and 254;
					  * the id 255 is reserved for lines
					  * in the interior and may be used
					  * to check whether a line is at the
					  * boundary or not, which otherwise
					  * is not possible if you don't know
					  * which cell it belongs to.
					  */
	std::vector<unsigned char> material_id;

					 /**
					  * Pointer which is not used by the
					  * library but may be accessed an set
					  * by the user to handle data local to
					  * a line/quad/etc.
					  */
	std::vector<void*> user_pointers;
    };
    
  public:
    				     /**
				      *  Data about the lines.
				      */
    LinesData lines;

    				     /**
				      *  Assert that enough space is allocated
				      *  to accomodate @p new_lines new lines.
				      *  This function does not only call
				      *  <tt>vector::reserve()</tt>, but does really
				      *  append the needed elements.
				      *  There are pendants for higher
				      *  dimensions, which you have to call
				      *  explicitly (they can't hand down the
				      *  call because there is no easy relation
				      *  between the number of new quads and
				      *  the number of new lines, etc.). Also
				      *  don't forget to call the
				      *  TriangulationLevel<0>@p ::reserve_space
				      *  function.
				      */
    void reserve_space (const unsigned int new_lines);

				     /**
				      *  Check the memory consistency of the
				      *  different containers. Should only be
				      *  called with the prepro flag @p DEBUG
				      *  set. The function should be called from
				      *  the functions of the higher
				      *  TriangulationLevel classes.
				      */
    void monitor_memory (const unsigned int true_dimension) const;

				     /**
				      * Determine an estimate for the
				      * memory consumption (in bytes)
				      * of this object.
				      */
    unsigned int memory_consumption () const;
};



/**
 *  Store all information which belongs to one level of the multilevel hierarchy.
 *
 *  In 2D this is a vector of the lines and one of the
 *  quads on this levels, as well as a the two associated vectors holding
 *  information about the children of these lines and quads.
 *
 *  The vector of lines and their children is derived from
 *  TriangulationLevel<1>.
 *
 *  @author Wolfgang Bangerth, 1998
 */
template <>
class TriangulationLevel<2> :  public TriangulationLevel<1>
{
				     /**
				      *  This subclass groups together all that
				      *  is needed to describe the quads on one
				      *  level.
				      *
				      *  It is fully analogous to the
				      *  TriangulationLevel<1>::LinesData structure inherited from
				      *  Triangulation<tt><1></tt>.
				      */
    struct QuadsData
    {
					 /**
					  *  Same as for the @p lines array.
					  */
	std::vector<Quad> quads;
					 /**
					  *  Same as for the
					  *  TriangulationLevel<1>::LinesData@p ::chilren
					  *  array, but since there
					  *  are four children, the
					  *  index points to the first
					  *  while the other three are
					  *  following immediately
					  *  afterwards.
					  */
	std::vector<int>  children;

					 /**
					  *  Same as for
					  *  TriangulationLevel<1>::LinesData@p ::used.
					  */
	std::vector<bool> used;

					 /**
					  *  Same as for
					  *  TriangulationLevel<1>::LinesData@p ::used.
					  */
	std::vector<bool> user_flags;

					 /**
					  * Store boundary and material data. In
					  * two dimension, this field stores
					  * the material id of a quad, which is a
					  * number between 0 and 254. In more
					  * than two dimensions, quads have no
					  * material id, but they may be at the
					  * boundary; then, we store the
					  * boundary indicator in this field,
					  * which denotes to which part of the
					  * boundary this line belongs and which
					  * boundary conditions hold on this
					  * part. The boundary indicator also
					  * is a number between zero and 254;
					  * the id 255 is reserved for quads
					  * in the interior and may be used
					  * to check whether a quad is at the
					  * boundary or not, which otherwise
					  * is not possible if you don't know
					  * which cell it belongs to.
					  */
	std::vector<unsigned char> material_id;


					 /**
					  * Pointer which is not used by the
					  * library but may be accessed an set
					  * by the user to handle data local to
					  * a line/quad/etc.
					  */
	std::vector<void*> user_pointers;
    };
    
  public:
				     /**
				      *  Data about the quads.
				      */
    QuadsData quads;

    				     /**
				      *  Assert that enough space is allocated
				      *  to accomodate @p new_quads new quads.
				      */
    void reserve_space (const unsigned int new_quads);

				     /**
				      *  Check the memory consistency of the
				      *  different containers. Should only be
				      *  called with the prepro flag @p DEBUG
				      *  set. The function should be called from
				      *  the functions of the higher
				      *  TriangulationLevel classes.
				      */
    void monitor_memory (const unsigned int true_dimension) const;

				     /**
				      * Determine an estimate for the
				      * memory consumption (in bytes)
				      * of this object.
				      */
    unsigned int memory_consumption () const;
};



/**
 *  Store all information which belongs to one level of the multilevel hierarchy.
 *
 *  In 3D this is a vector of the lines, one of quads and one of the
 *  hexahedra on this levels, as well as a the associated vectors
 *  holding information about the children of these lines, quads and
 *  hexs. In addition, we store for each face of each hexahedron
 *  whether its normal points in the standard direction (see the
 *  Triangulation class) or in the opposite direction.
 *
 *  The vectors of lines and quads and their children are derived from
 *  TriangulationLevel<2>.
 *
 *  @author Wolfgang Bangerth, 1998, 2003
 */
template <>
class TriangulationLevel<3> :  public TriangulationLevel<2>
{
				     /**
				      *  This subclass groups together
				      *  all that is needed to
				      *  describe the hexes on one
				      *  level.
				      *
				      *  It is fully analogous to the
				      *  TriangulationLevel<1>::LinesData
				      *  structure inherited from
				      *  Triangulation<1>.
				      */
    struct HexesData
    {
					 /**
					  *  Same as for the @p lines array.
					  */
	std::vector<Hexahedron> hexes;
        
					 /**
					  *  Same as for the
					  *  TriangulationLevel<1>::LinesData@p ::chilren
					  *  array, but since there
					  *  are four children, the
					  *  index points to the first
					  *  while the other three are
					  *  following immediately
					  *  afterwards.
					  */
	std::vector<int>  children;

					 /**
					  *  Same as for
					  *  TriangulationLevel<1>::LinesData@p ::used.
					  */
	std::vector<bool> used;

					 /**
					  *  Same as for
					  *  TriangulationLevel<1>::LinesData@p ::used.
					  */
	std::vector<bool> user_flags;

					 /**
					  * Store boundary and
					  * material data. In two
					  * dimension, this field
					  * stores the material id of
					  * a hex, which is a number
					  * between 0 and 254. In more
					  * than three dimensions,
					  * hexes have no material id,
					  * but they may be at the
					  * boundary; then, we store
					  * the boundary indicator in
					  * this field, which denotes
					  * to which part of the
					  * boundary this line belongs
					  * and which boundary
					  * conditions hold on this
					  * part. The boundary
					  * indicator also is a number
					  * between zero and 254; the
					  * id 255 is reserved for
					  * hexes in the interior and
					  * may be used to check
					  * whether a hex is at the
					  * boundary or not, which
					  * otherwise is not possible
					  * if you don't know which
					  * cell it belongs to.
					  */
	std::vector<unsigned char> material_id;

					 /**
					  * Pointer which is not used
					  * by the library but may be
					  * accessed an set by the
					  * user to handle data local
					  * to a line/quad/etc.
					  */
	std::vector<void*> user_pointers;

                                         /**
                                          * For edges, we enforce a
                                          * standard convention that
                                          * opposite edges should be
                                          * parallel. Now, that's
                                          * enforcable in most cases,
                                          * and we have code that
                                          * makes sure that if a mesh
                                          * allows this to happen,
                                          * that we have this
                                          * convention. We also know
                                          * that it is always possible
                                          * to have opposite faces
                                          * have parallel normal
                                          * vectors. (For both things,
                                          * see the Agelek, Anderson,
                                          * Bangerth, Barth paper
                                          * mentioned in the
                                          * publications list.)
                                          *
                                          * The problem is that we
                                          * originally had another
                                          * condition, namely that
                                          * faces 0, 2 and 6 have
                                          * normals that point into
                                          * the cell, while the other
                                          * faces have normals that
                                          * point outward. It turns
                                          * out that this is not
                                          * always possible. In
                                          * effect, we have to store
                                          * whether the normal vector
                                          * of each face of each cell
                                          * follows this convention or
                                          * not. If this is so, then
                                          * this variable stores a
                                          * @p true value, otherwise
                                          * a @p false value.
                                          *
                                          * In effect, this field has
                                          * <tt>6*n_cells</tt> elements,
                                          * being the number of cells
                                          * times the six faces each
                                          * has.
                                          */
        std::vector<bool> face_orientations;
    };
    
  public:
				     /**
				      *  Data about the hexes.
				      */
    HexesData hexes;

    				     /**
				      *  Assert that enough space is allocated
				      *  to accomodate @p new_quads new quads.
				      */
    void reserve_space (const unsigned int new_quads);

				     /**
				      *  Check the memory consistency
				      *  of the different
				      *  containers. Should only be
				      *  called with the prepro flag
				      *  @p DEBUG set. The function
				      *  should be called from the
				      *  functions of the higher
				      *  TriangulationLevel
				      *  classes.
				      */
    void monitor_memory (const unsigned int true_dimension) const;

				     /**
				      * Determine an estimate for the
				      * memory consumption (in bytes)
				      * of this object.
				      */
    unsigned int memory_consumption () const;
};



#endif
