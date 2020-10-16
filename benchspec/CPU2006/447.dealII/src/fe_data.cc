//----------------------------------------------------------------
//    $Id: fe_data.cc,v 1.1 2004/09/14 00:51:22 wolf Exp $
//    Version: $Name:  $
//
//    Copyright (C) 1998, 1999, 2000, 2001, 2002, 2003, 2004 by the deal.II authors
//
//    This file is subject to QPL and may not be  distributed
//    without copyright and license information. Please refer
//    to the file deal.II/doc/license.html for the  text  and
//    further information on this license.
//
//----------------------------------------------------------------


#include <fe/fe.h>
#include <grid/geometry_info.h>



template <int dim>
FiniteElementData<dim>::FiniteElementData ()
                :
		dofs_per_vertex(0),
		dofs_per_line(0),
		dofs_per_quad(0),
		dofs_per_hex(0),
		first_line_index(0),
		first_quad_index(0),
		first_hex_index(0),
		first_face_line_index(0),
		first_face_quad_index(0),
		dofs_per_face(0),
		dofs_per_cell (0),
		components(0),
		degree(0)
{}



template <int dim>
FiniteElementData<dim>::
FiniteElementData (const std::vector<unsigned int> &dofs_per_object,
                   const unsigned int n_components,
                   const unsigned int degree)
                :
		dofs_per_vertex(dofs_per_object[0]),
		dofs_per_line(dofs_per_object[1]),
		dofs_per_quad(dim>1? dofs_per_object[2]:0),
		dofs_per_hex(dim>2? dofs_per_object[3]:0),
		first_line_index(GeometryInfo<dim>::vertices_per_cell
				 * dofs_per_vertex),
		first_quad_index(first_line_index+
				 GeometryInfo<dim>::lines_per_cell
				 * dofs_per_line),
		first_hex_index(first_quad_index+
				GeometryInfo<dim>::quads_per_cell
				* dofs_per_quad),
		first_face_line_index(GeometryInfo<dim-1>::vertices_per_cell
				      * dofs_per_vertex),
		first_face_quad_index((dim==3 ?
				       GeometryInfo<dim-1>::vertices_per_cell
				       * dofs_per_vertex :
				       GeometryInfo<dim>::vertices_per_cell
				       * dofs_per_vertex) +
				      GeometryInfo<dim-1>::lines_per_cell
				      * dofs_per_line),
		dofs_per_face(GeometryInfo<dim>::vertices_per_face * dofs_per_vertex +
			      GeometryInfo<dim>::lines_per_face * dofs_per_line +
			      GeometryInfo<dim>::quads_per_face * dofs_per_quad),
		dofs_per_cell (GeometryInfo<dim>::vertices_per_cell * dofs_per_vertex +
			       GeometryInfo<dim>::lines_per_cell * dofs_per_line +
			       GeometryInfo<dim>::quads_per_cell * dofs_per_quad +
			       GeometryInfo<dim>::hexes_per_cell * dofs_per_hex),
		components(n_components),
		degree(degree)
{
  Assert(dofs_per_object.size()==dim+1, ExcDimensionMismatch(dofs_per_object.size()-1,dim));
}



template<int dim>
bool FiniteElementData<dim>::operator== (const FiniteElementData<dim> &f) const
{
  return ((dofs_per_vertex == f.dofs_per_vertex) &&
	  (dofs_per_line == f.dofs_per_line) &&
	  (dofs_per_quad == f.dofs_per_quad) &&
	  (dofs_per_hex == f.dofs_per_hex) &&
	  (components == f.components));
}


template class FiniteElementData<1>;
template class FiniteElementData<2>;
template class FiniteElementData<3>;
