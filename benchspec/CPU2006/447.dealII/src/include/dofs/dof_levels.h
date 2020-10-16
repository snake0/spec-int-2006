//----------------------------  dof_levels.h  ---------------------------
//    $Id: dof_levels.h,v 1.1 2004/09/14 00:53:32 wolf Exp $
//    Version: $Name:  $
//
//    Copyright (C) 1998, 1999, 2000, 2001, 2002, 2003, 2004 by the deal.II authors
//
//    This file is subject to QPL and may not be  distributed
//    without copyright and license information. Please refer
//    to the file deal.II/doc/license.html for the  text  and
//    further information on this license.
//
//----------------------------  dof_levels.h  ---------------------------
#ifndef __deal2__dof_levels_h
#define __deal2__dof_levels_h


#include <base/config.h>
#include <vector>


/**
 * Store the indices of the degrees of freedom which are located on
 * objects of dimension @p N.  Declare this general template
 * class, but do not actually use it. Rather, only specializations of
 * this class are used.
 *
 * @author Wolfgang Bangerth, 1998
 */
template <int N>
class DoFLevel
{
  private:
				     /**
				      * Make the constructor private
				      * to avoid that someone uses
				      * this class.
				      */
    DoFLevel ();
};


/**
 * Store the indices of the degrees of freedom which are located on
 * the lines.
 *
 * @sect3{Information for all DoFLevel classes}
 *
 * The <tt>DoFLevel<N></tt> classes 
 * store the global indices of the degrees of freedom for each cell on a
 * certain level. The index or number of a degree of freedom is the zero-based
 * index of the according value in the solution vector and the row and column
 * index in the global matrix or the multigrid matrix for this level. These
 * indices refer to the unconstrained vectors and matrices, where we have not
 * taken account of the constraints introduced by hanging nodes. If more than
 * one value corresponds to one basis function, for example for vector equations
 * where the solution is vector valued and thus has several degrees of freedom
 * for each basis function, we nonetheless store only one index. This can then
 * be viewed as the index into a block vector, where each block contains the
 * different values according to a degree of freedom. It is left to the derived
 * classes, whether the values in a block are stored consecutively or distributed
 * (e.g. if the solution function is $u=(u_1, u_2)$, we could store the values
 * in the solution vector like
 * $\ldots, u_1^m, u_2^m, u_1^{m+1}, u_2^{m+1},\ldots$ with $m$ denoting the
 * $m$th basis function, or $\ldots, u_1^m, u_1^{m+1}, u_1^{m+2}, \ldots,
 * u_2^m, u_2^{m+1}, u_2^{m+2}, \ldots$, respectively). Likewise, the
 * constraint matrix returned by DoFHandler::make_hanging_node_constraints()
 * is then
 * to be understood as a block matrix.
 *
 * The storage format of the degrees of freedom indices (short: DoF
 * indices) is somewhat like a mirror of the data structures of the
 * triangulation classes.  There is a hierarchy of
 * DoFLevel<dim> classes for the different dimensions which
 * have objects named @p line_dofs, @p quad_dofs and so on, in which
 * the indices of DoFs located on lines and quads, respectively, are
 * stored. The indices are stored levelwise. The layout in these
 * arrays is as follows: if for a selected finite element (use
 * DoFHandler::distribute_dofs() to select a finite element)
 * the number of DoFs on each line (without those in the vertices) is
 * @p N, then the length of the @p line_dofs array is @p N times
 * the number of lines on this level. The DoF indices for the @p ith
 * line are at the positions <tt>N*i...(N+1)*i-1</tt>.
 *
 * The DoF indices for vertices are not stored this way, since they
 * need different treatment in multigrid environments. If no multigrid
 * is used, the indices are stored in the @p vertex_dofs array of the
 * DoFHandler class.
 *
 * @author Wolfgang Bangerth, 1998
 */
template <>
class DoFLevel<1>
{
  public:
				     /**
				      * Store the global indices of
				      * the degrees of freedom. See
				      * DoFLevel for detailed
				      * information.
				      */
    std::vector<unsigned int> line_dofs;

				     /**
				      * Determine an estimate for the
				      * memory consumption (in bytes)
				      * of this object.
				      */
    unsigned int memory_consumption () const;
};



/**
 * Store the indices of the degrees of freedom which are located on
 * quads.  See DoFLevel<1> for more information.
 *
 * @author Wolfgang Bangerth, 1998
 */
template <>
class DoFLevel<2> : public DoFLevel<1>
{
  public:
				     /**
				      * Store the global indices of
				      * the degrees of freedom. See
				      * DoFLevel for detailed
				      * information.
				      */
    std::vector<unsigned int> quad_dofs;

				     /**
				      * Determine an estimate for the
				      * memory consumption (in bytes)
				      * of this object.
				      */
    unsigned int memory_consumption () const;
};



/**
 * Store the indices of the degrees of freedom which are located on
 * hexhedra.  See DoFLevel<1> for more information.
 *
 * @author Wolfgang Bangerth, 1998
 */
template <>
class DoFLevel<3> : public DoFLevel<2>
{
  public:
				     /**
				      * Store the global indices of
				      * the degrees of freedom. See
				      * DoFLevel for detailed
				      * information.
				      */
    std::vector<unsigned int> hex_dofs;

				     /**
				      * Determine an estimate for the
				      * memory consumption (in bytes)
				      * of this object.
				      */
    unsigned int memory_consumption () const;
};


#endif
