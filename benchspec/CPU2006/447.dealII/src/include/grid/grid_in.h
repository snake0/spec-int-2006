//----------------------------  grid_in.h  ---------------------------
//    $Id: grid_in.h,v 1.2 2004/09/19 20:15:53 wolf Exp $
//    Version: $Name:  $
//
//    Copyright (C) 1998, 1999, 2000, 2001, 2002, 2003, 2004 by the deal.II authors
//
//    This file is subject to QPL and may not be  distributed
//    without copyright and license information. Please refer
//    to the file deal.II/doc/license.html for the  text  and
//    further information on this license.
//
//----------------------------  grid_in.h  ---------------------------
#ifndef __deal2__grid_in_h
#define __deal2__grid_in_h


#include <base/config.h>
#include <base/exceptions.h>
#include <base/smartpointer.h>
#include <iostream>
#include <vector>
#include <string>

template <int dim> class Point;
template <int dim> class Triangulation;
template <int dim> struct CellData;
struct SubCellData;


/**
 * This class implements an input mechanism for grid data. It allows
 * to read a grid structure into a triangulation object. At present,
 * only UCD (unstructured cell data) and DB Mesh is supported as input
 * format for grid data. Any numerical data after the block of
 * topological information is ignored.
 *
 * Since the coarse mesh fed into a @p Triangulation object cannot
 * have hanging nodes, strange things will happen if the input file
 * has them. This is due to the fact that most mesh description
 * formats do not store neighborship information between cells, so the
 * grid reading functions have to regenerate it. They do so by
 * checking whether two cells have a common face. If there are hanging
 * nodes in a triangulation, adjacent cells have no common face, so
 * the grid reader concludes that the adjacent cells have no neighbors
 * along these faces and must therefore be at the boundary. In effect,
 * an internal crack of the domain is introduced this way. Since such
 * cases are very hard to detect, the library does not make any
 * attempt to abort on such situations, and you will get a
 * triangulation that probably does not do what you want.
 *
 * Note: if you experience unexpected problems with the use of this
 * class, be sure to read the documentation right until the end, and
 * also read the documentation of the GridReordering class.
 *
 * To read grid data, the triangulation to be fed with has to be empty.
 * When giving a file which does not contain the assumed information or
 * which does not keep to the right format, the state of the triangulation
 * will be undefined afterwards. Upon input, only lines in one dimension
 * and line and quads in two dimensions are accepted. All other cell types
 * (e.g. triangles in two dimensions, quads and hexes in 3d) are rejected.
 * The vertex and cell numbering in the input file, which
 * need not be consecutively, is lost upon transfer to the triangulation
 * object, since this one needs consecutively numbered elements.
 *
 * Material indicators are accepted to denote the material ID of cells and
 * to denote boundary part indication for lines in 2D. Read the according
 * sections in the documentation of the Triangulation class for
 * further details.
 *
 *
 * @sect3{Supported input formats}
 *
 * At present, the following input formats are supported:
 * <ul>
 * <li> @p UCD (unstructured cell data) format: this format is used
 * for grid input as well as data output. If there are data vectors in
 * the input file, they are ignored, as we are only interested in the
 * grid in this class. The exact description of the format can be
 * found in the AVS Explorer manual (see http://www.avs.com).
 * The @p UCD format can be read by the read_ucd() function.
 *
 * <li> <tt>DB mesh</tt> format: this format is used by the @p BAMG mesh
 * generator (see
 * http://www-rocq.inria.fr/gamma/cdrom/www/bamg/eng.htm. The
 * documentation of the format in the @p BAMG manual is very
 * incomplete, so we don't actually parse many of the fields of the
 * output since we don't know their meaning, but the data that is read
 * is enough to build up the mesh as intended by the mesh generator.
 * This format can be read by the read_dbmesh() function.
 *
 * <li> @p XDA format: this is a rather simple format used by the MGF
 * code. We don't have an exact specification of the format, but the reader
 * can read in several example files. If the reader does not grok your files,
 * it should be fairly simple to extend it.
 *
 * <li> <tt>Gmsh mesh</tt> format: this format is used by the @p GMSH
 * mesh generator (see http://www.geuz.org/gmsh/ ). The documentation
 * in the @p GMSH manual explains how to generate meshes compatible with
 * the deal.II library (i.e. quads rather than triangles).
 * </ul>
 *
 *
 * @sect3{Structure of input grid data. The GridReordering class}
 * 
 * It is your duty to use a correct numbering of vertices in the cell
 * list, i.e. for lines in 1d, you have to first give the vertex with
 * the lower coordinate value, then that with the higher coordinate
 * value. For quadrilaterals in two dimensions, the vertex indices in
 * the @p quad list have to be such that the vertices are numbered in
 * counter-clockwise sense.
 *
 * In two dimensions, another difficulty occurs, which has to do with the sense
 * of a quadrilateral. A quad consists of four lines which have a direction,
 * which is per definitionem as follows:
 * @verbatim
 *   3-->--2
 *   |     |
 *   ^     ^
 *   |     |
 *   0-->--1
 * @endverbatim
 * Now, two adjacent cells must have a vertex numbering such that the direction
 * of the common side is the same. For example, the following two quads
 * @verbatim
 *   3---4---5
 *   |   |   |
 *   0---1---2
 * @endverbatim
 * may be characterised by the vertex numbers <tt>(0 1 4 3)</tt> and
 * <tt>(1 2 5 4)</tt>, since the middle line would get the direction <tt>1->4</tt>
 * when viewed from both cells.  The numbering <tt>(0 1 4 3)</tt> and
 * <tt>(5 4 1 2)</tt> would not be allowed, since the left quad would give the
 * common line the direction <tt>1->4</tt>, while the right one would want
 * to use <tt>4->1</tt>, leading to an ambiguity. The Triangulation
 * object is capable of detecting this special case, which can be
 * eliminated by rotating the indices of the right quad by
 * two. However, it would not know what to do if you gave the vertex
 * indices <tt>(4 1 2 5)</tt>, since then it would have to rotate by one
 * element or three, the decision which to take is not yet
 * implemented.
 *
 * There are more ambiguous cases, where the triangulation may not
 * know what to do at all without the use of sophisticated
 * algorithms. Furthermore, similar problems exist in three space
 * dimensions, where faces and lines have orientations that need to be
 * taken care of.
 *
 * For this reason, the <tt>read_*</tt> functions of this class that read
 * in grids in various input formats call the GridReordering
 * class to bring the order of vertices that define the cells into an
 * ordering that satisfies the requiremenets of the
 * Triangulation class. Be sure to read the documentation of
 * that class if you experience unexpected problems when reading grids
 * through this class.
 *
 * @author Wolfgang Bangerth, 1998, 2000, Luca Heltai, 2004
 */
template <int dim>
class GridIn
{
  public:
				     /**
				      * Constructor.
				      */
    GridIn ();
    
				     /**
				      * Attach this triangulation
				      * to be fed with the grid data.
				      */
    void attach_triangulation (Triangulation<dim> &tria);

				     /**
				      * Read grid data from an ucd file.
				      * Numerical data is ignored.
				      */
    void read_ucd (std::istream &);

				     /**
				      * Read grid data from a file
				      * containing data in the DB mesh
				      * format.
				      */
    void read_dbmesh (std::istream &);
    
				     /**
				      * Read grid data from a file
				      * containing data in the XDA
				      * format.
				      */
    void read_xda (std::istream &);
    
    				     /**
				      * Read grid data from an msh file.
				      */
    void read_msh (std::istream &);

				     /**
				      * Exception
				      */
    DeclException1 (ExcUnknownIdentifier,
		    std::string,
		    << "The identifier <" << arg1 << "> as name of a "
		    << "part in an UCD input file is unknown or the "
		    << "respective input routine is not implemented.");
				     /**
				      * Exception
				      */
    DeclException0 (ExcNoTriangulationSelected);
				     /**
				      * Exception
				      */
    DeclException2 (ExcInvalidVertexIndex,
		    int, int,
		    << "Trying to access invalid vertex index " << arg2
		    << " while creating cell " << arg1);
				     /**
				      * Exception
				      */
    DeclException0 (ExcInvalidDBMeshFormat);
				     /**
				      * Exception
				      */
    DeclException1 (ExcInvalidDBMESHInput,
		    std::string,
		    << "The string <" << arg1 << "> is not recognized at the present"
		    << " position of a DB Mesh file.");
    
				     /**
				      * Exception
				      */
    DeclException1 (ExcDBMESHWrongDimension,
		    int,
		    << "The specified dimension " << arg1
		    << " is not the same as that of the triangulation to be created.");
				    
    DeclException1 (ExcInvalidGMSHInput,
		    std::string,
		    << "The string <" << arg1 << "> is not recognized at the present"
		    << " position of a Gmsh Mesh file.");

    DeclException1 (ExcGmshUnsupportedGeometry,
		    int,
		    << "The Element Identifier <" << arg1 << "> is not "
		    << "supported in the Deal.II Library.\n"
		    << "Supported elements are: \n"
		    << "ELM-TYPE\n"
		    << "1 Line (2 nodes, 1 edge).\n"
		    << "3 Quadrangle (4 nodes, 4 edges).\n"
		    << "5 Hexahedron (8 nodes, 12 edges, 6 faces).\n"
		    << "15 Point (1 node).");
  private:
				     /**
				      * Store address of the triangulation to
				      * be fed with the data read in.
				      */
    SmartPointer<Triangulation<dim> > tria;

				     /**
				      * Skip empty lines in the input
				      * stream, i.e. lines that
				      * contain either nothing or only
				      * whitespace.
				      */
    static void skip_empty_lines (std::istream &in);
    
				     /**
				      * Skip lines of comment that
				      * start with the indicated
				      * character (e.g. <tt>#</tt>)
				      * following the point where the
				      * given input stream presently
				      * is. After the call to this
				      * function, the stream is at the
				      * start of the first line after
				      * the comment lines, or at the
				      * same position as before if
				      * there were no lines of
				      * comments.
				      */
    static void skip_comment_lines (std::istream    &in,
				    const char  comment_start);

				     /**
				      * Remove vertices that are not
				      * referenced by any of the
				      * cells. This function is called
				      * by all <tt>read_*</tt> functions to
				      * eliminate vertices that are
				      * listed in the input files but
				      * are not used by the cells in
				      * the input file. While these
				      * vertices should not be in the
				      * input from the beginning, they
				      * sometimes are, most often when
				      * some cells have been removed
				      * by hand without wanting to
				      * update the vertex lists, as
				      * they might be lengthy.
				      *
				      * This function is called by all
				      * <tt>read_*</tt> functions as the
				      * triangulation class requires
				      * them to be called with used
				      * vertices only. This is so,
				      * since the vertices are copied
				      * verbatim by that class, so we
				      * have to eliminate unused
				      * vertices beforehand.
				      */
    static void delete_unused_vertices (std::vector<Point<dim> >    &vertices,
					std::vector<CellData<dim> > &cells,
					SubCellData                          &subcelldata);

				     /**
				      * This function can write the
				      * raw cell data objects created
				      * by the <tt>read_*</tt> functions in
				      * Gnuplot format to a
				      * stream. This is sometimes
				      * handy if one would like to see
				      * what actually was created, if
				      * it is known that the data is
				      * not correct in some way, but
				      * the Triangulation class
				      * refuses to generate a
				      * triangulation because of these
				      * errors. In particular, the
				      * output of this class writes
				      * out the cell numbers along
				      * with the direction of the
				      * faces of each cell. In
				      * particular the latter
				      * information is needed to
				      * verify whether the cell data
				      * objects follow the
				      * requirements of the ordering
				      * of cells and their faces,
				      * i.e. that all faces need to
				      * have unique directions and
				      * specified orientations with
				      * respect to neighboring cells
				      * (see the documentations to
				      * this class and the
				      * GridReordering class).
				      *
				      * The output of this function
				      * consists of vectors for each
				      * line bounding the cells
				      * indicating the direction it
				      * has with respect to the
				      * orientation of this cell, and
				      * the cell number. The whole
				      * output is in a form such that
				      * it can be read in by Gnuplot
				      * and generate the full plot
				      * without further ado by the
				      * user.
				      */
    static void debug_output_grid (const std::vector<CellData<dim> > &cells,
				   const std::vector<Point<dim> >    &vertices,
				   std::ostream                               &out);
};



/* -------------- declaration of explicit specializations ------------- */

/// @if NoDoc

template <>
void
GridIn<2>::debug_output_grid (const std::vector<CellData<2> > &cells,
			      const std::vector<Point<2> >    &vertices,
			      std::ostream                    &out);
template <>
void
GridIn<3>::debug_output_grid (const std::vector<CellData<3> > &cells,
			      const std::vector<Point<3> >    &vertices,
			      std::ostream                    &out);

/// @endif

#endif
