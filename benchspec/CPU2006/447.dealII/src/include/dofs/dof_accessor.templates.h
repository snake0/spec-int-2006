//-------------------  dof_accessor.templates.h  ---------------------------
//    $Id: dof_accessor.templates.h,v 1.1 2004/09/14 00:53:32 wolf Exp $
//    Version: $Name:  $
//
//    Copyright (C) 1998, 1999, 2000, 2001, 2002, 2003, 2004 by the deal.II authors
//
//    This file is subject to QPL and may not be  distributed
//    without copyright and license information. Please refer
//    to the file deal.II/doc/license.html for the  text  and
//    further information on this license.
//
//-------------------  dof_accessor.templates.h  ---------------------------
#ifndef __deal2__dof_accessor_templates_h
#define __deal2__dof_accessor_templates_h


#include <base/config.h>
#include <dofs/dof_accessor.h>
#include <dofs/dof_handler.h>
#include <dofs/dof_levels.h>
#include <grid/tria_iterator.h>
#include <grid/tria_iterator.templates.h>

#include <vector>


/*------------------------- Functions: DoFAccessor ---------------------------*/


template <int dim>
DoFAccessor<dim>::DoFAccessor ()
                :
		dof_handler(0)
{
  Assert (false, ExcInvalidObject());
}



template <int dim>
inline
DoFAccessor<dim>::DoFAccessor (const DoFHandler<dim> *dof_handler) :
		dof_handler(const_cast<DoFHandler<dim>*>(dof_handler))
{}



template <int dim>
void
DoFAccessor<dim>::set_dof_handler (DoFHandler<dim> *dh)
{
  Assert (dh != 0, ExcInvalidObject());
  dof_handler = dh;
}



template <int dim>
inline
const DoFHandler<dim> &
DoFAccessor<dim>::get_dof_handler () const
{
  return *dof_handler;
}



template <int dim>
inline
const FiniteElement<dim> &
DoFAccessor<dim>::get_fe () const
{
  return *dof_handler->selected_fe;
}



template <int dim>
inline
DoFAccessor<dim> &
DoFAccessor<dim>::operator = (const DoFAccessor<dim> &da)
{
  this->set_dof_handler (da.dof_handler);
  return *this;
}




/*------------------------- Functions: DoFObjectAccessor<1,dim> -----------------------*/



template <int dim>
inline
DoFObjectAccessor<1,dim>::
DoFObjectAccessor (const Triangulation<dim> *tria,
                   const int                 level,
                   const int                 index,
                   const AccessorData       *local_data)
                :
                DoFAccessor<dim> (local_data),
                DoFObjectAccessor_Inheritance<1,dim>::BaseClass (tria,
                                                                 level,
                                                                 index)
{}



template <int dim>
inline
unsigned int
DoFObjectAccessor<1,dim>::dof_index (const unsigned int i) const
{
				   // since the exception classes are
				   // from a template dependent base
				   // class, we have to fully qualify
				   // them. to work around more
				   // trouble, typedef the template
				   // dependent base class to a
				   // non-template dependent name and
				   // use that to specify the
				   // qualified exception names
  typedef DoFAccessor<dim> BaseClass;
  
  Assert (this->dof_handler != 0, typename BaseClass::ExcInvalidObject());
				   // make sure a FE has been selected
				   // and enough room was reserved
  Assert (&this->get_fe() != 0, typename BaseClass::ExcInvalidObject());
  Assert (i<this->get_fe().dofs_per_line,
	  ExcIndexRange (i, 0, this->get_fe().dofs_per_line));

  return this->dof_handler->levels[this->present_level]
    ->line_dofs[this->present_index*this->get_fe().dofs_per_line+i];
}



template <int dim>
inline
unsigned int
DoFObjectAccessor<1,dim>::vertex_dof_index (const unsigned int vertex,
					    const unsigned int i) const
{
				   // since the exception classes are
				   // from a template dependent base
				   // class, we have to fully qualify
				   // them. to work around more
				   // trouble, typedef the template
				   // dependent base class to a
				   // non-template dependent name and
				   // use that to specify the
				   // qualified exception names
  typedef DoFAccessor<dim> BaseClass;
  
  Assert (this->dof_handler != 0, typename BaseClass::ExcInvalidObject());
  Assert (&this->get_fe() != 0, typename BaseClass::ExcInvalidObject());
  Assert (vertex<2, ExcIndexRange (i,0,2));
  Assert (i<this->get_fe().dofs_per_vertex,
	  ExcIndexRange (i, 0, this->get_fe().dofs_per_vertex));

  const unsigned int dof_number = (this->vertex_index(vertex) *
				   this->get_fe().dofs_per_vertex +
				   i);
  return this->dof_handler->vertex_dofs[dof_number];
}



template <int dim>
inline
void
DoFObjectAccessor<1,dim>::get_dof_indices (std::vector<unsigned int> &dof_indices) const
{
				   // since the exception classes are
				   // from a template dependent base
				   // class, we have to fully qualify
				   // them. to work around more
				   // trouble, typedef the template
				   // dependent base class to a
				   // non-template dependent name and
				   // use that to specify the
				   // qualified exception names
  typedef DoFAccessor<dim> BaseClass;
  
  Assert (this->dof_handler != 0, typename BaseClass::ExcInvalidObject());
  Assert (&this->get_fe() != 0, typename BaseClass::ExcInvalidObject());
  Assert (dof_indices.size() == (2*this->dof_handler->get_fe().dofs_per_vertex +
				 this->dof_handler->get_fe().dofs_per_line),
	  typename BaseClass::ExcVectorDoesNotMatch());

				   // this function really only makes
				   // sense on non-active objects if
				   // all degrees of freedom are
				   // located on vertices, since
				   // otherwise there are degrees of
				   // freedom on sub-objects which are
				   // not allocated for this
				   // non-active thing
  Assert (!this->has_children() ||
	  (this->dof_handler->get_fe().dofs_per_cell ==
	   2*this->dof_handler->get_fe().dofs_per_vertex),
	  typename DoFAccessor<dim>::ExcNotActive());
  
  const unsigned int dofs_per_vertex = this->dof_handler->get_fe().dofs_per_vertex,
		     dofs_per_line   = this->dof_handler->get_fe().dofs_per_line;
  std::vector<unsigned int>::iterator next = dof_indices.begin();
  for (unsigned int vertex=0; vertex<2; ++vertex)
    for (unsigned int d=0; d<dofs_per_vertex; ++d)
      *next++ = vertex_dof_index(vertex,d);
  for (unsigned int d=0; d<dofs_per_line; ++d)
    *next++ = dof_index(d);
}



template <int dim>
inline
TriaIterator<dim,DoFObjectAccessor<1,dim> >
DoFObjectAccessor<1,dim>::child (const unsigned int i) const
{
  TriaIterator<dim,DoFObjectAccessor<1,dim> > q (this->tria,
						 this->present_level+1,
						 this->child_index (i),
						 this->dof_handler);
  
#ifdef DEBUG
  if (q.state() != IteratorState::past_the_end)
    Assert (q->used(), typename TriaAccessor<dim>::ExcUnusedCellAsChild());
#endif
  return q;
}



template <int dim>
template <typename number, typename OutputVector>
inline
void
DoFObjectAccessor<1, dim>::
distribute_local_to_global (const Vector<number> &local_source,
			    OutputVector         &global_destination) const
{
				   // since the exception classes are
				   // from a template dependent base
				   // class, we have to fully qualify
				   // them. to work around more
				   // trouble, typedef the template
				   // dependent base class to a
				   // non-template dependent name and
				   // use that to specify the
				   // qualified exception names
  typedef DoFAccessor<dim> BaseClass;
  
  Assert (this->dof_handler != 0,
	  typename BaseClass::ExcInvalidObject());
  Assert (&this->get_fe() != 0,
	  typename BaseClass::ExcInvalidObject());
  Assert (local_source.size() == (2*this->get_fe().dofs_per_vertex +
				  this->get_fe().dofs_per_line),
	  typename BaseClass::ExcVectorDoesNotMatch());
  Assert (this->dof_handler->n_dofs() == global_destination.size(),
	  typename BaseClass::ExcVectorDoesNotMatch());

  const unsigned int n_dofs = local_source.size();

//TODO[WB]: This function could me made more efficient. First, it allocates memory, which could be avoided by passing in another argument as a scratch array. second, the elementwise access is really slow if we use PETSc vectors/matrices. This should be fixed eventually
  
				   // get indices of dofs
  std::vector<unsigned int> dofs (n_dofs);
  get_dof_indices (dofs);
  
				   // distribute cell vector
  for (unsigned int j=0; j<n_dofs; ++j)
    global_destination(dofs[j]) += local_source(j);
}



template <int dim>
template <typename number, typename OutputMatrix>
inline
void
DoFObjectAccessor<1, dim>::
distribute_local_to_global (const FullMatrix<number> &local_source,
			    OutputMatrix             &global_destination) const
{
				   // since the exception classes are
				   // from a template dependent base
				   // class, we have to fully qualify
				   // them. to work around more
				   // trouble, typedef the template
				   // dependent base class to a
				   // non-template dependent name and
				   // use that to specify the
				   // qualified exception names
  typedef DoFAccessor<dim> BaseClass;
  
  Assert (this->dof_handler != 0,
	  typename BaseClass::ExcInvalidObject());
  Assert (&this->get_fe() != 0,
	  typename BaseClass::ExcInvalidObject());
  Assert (local_source.m() == (2*this->get_fe().dofs_per_vertex +
                               this->get_fe().dofs_per_line),
	  typename BaseClass::ExcVectorDoesNotMatch());
  Assert (local_source.m() == local_source.n(),
	  typename BaseClass::ExcMatrixDoesNotMatch());
  Assert (this->dof_handler->n_dofs() == global_destination.m(),
	  typename BaseClass::ExcMatrixDoesNotMatch());
  Assert (global_destination.m() == global_destination.n(),
	  typename BaseClass::ExcMatrixDoesNotMatch());

  const unsigned int n_dofs = local_source.m();

//TODO[WB]: This function could me made more efficient. First, it allocates memory, which could be avoided by passing in another argument as a scratch array. second, the elementwise access is really slow if we use PETSc vectors/matrices. This should be fixed eventually

				   // get indices of dofs
  std::vector<unsigned int> dofs (n_dofs);
  get_dof_indices (dofs);
  
				   // distribute cell matrix
  for (unsigned int i=0; i<n_dofs; ++i)
    for (unsigned int j=0; j<n_dofs; ++j)
      global_destination.add(dofs[i], dofs[j], local_source(i,j));
}



template <int dim>
inline
void
DoFObjectAccessor<1,dim>::copy_from (const DoFObjectAccessor<1,dim> &a)
{
  BaseClass::copy_from (a);
  this->set_dof_handler (a.dof_handler);
}


/*------------------------- Functions: DoFObjectAccessor<2,dim> -----------------------*/

template <int dim>
inline
DoFObjectAccessor<2,dim>::
DoFObjectAccessor (const Triangulation<dim> *tria,
                   const int                 level,
                   const int                 index,
                   const AccessorData       *local_data)
                :
                DoFAccessor<dim> (local_data),
                DoFObjectAccessor_Inheritance<2,dim>::BaseClass (tria,
                                                                 level,
                                                                 index)
{}



template <int dim>
inline
unsigned int DoFObjectAccessor<2,dim>::dof_index (const unsigned int i) const
{
  Assert (this->dof_handler != 0,
	  typename DoFAccessor<dim>::ExcInvalidObject());
				   // make sure a FE has been selected
				   // and enough room was reserved
  Assert (&this->get_fe() != 0,
	  typename DoFAccessor<dim>::ExcInvalidObject());
  Assert (i<this->get_fe().dofs_per_quad,
	  ExcIndexRange (i, 0, this->get_fe().dofs_per_quad));

  return this->dof_handler->levels[this->present_level]
    ->quad_dofs[this->present_index*this->get_fe().dofs_per_quad+i];
}


template <int dim>
inline
unsigned int
DoFObjectAccessor<2,dim>::vertex_dof_index (const unsigned int vertex,
					    const unsigned int i) const
{
  Assert (this->dof_handler != 0,
	  typename DoFAccessor<dim>::ExcInvalidObject());
  Assert (&this->get_fe() != 0,
	  typename DoFAccessor<dim>::ExcInvalidObject());
  Assert (vertex<4, ExcIndexRange (i,0,4));
  Assert (i<this->get_fe().dofs_per_vertex,
	  ExcIndexRange (i, 0, this->get_fe().dofs_per_vertex));

  const unsigned int dof_number = (this->vertex_index(vertex) *
				   this->get_fe().dofs_per_vertex +
				   i);
  return this->dof_handler->vertex_dofs[dof_number];
}



template <int dim>
inline
void
DoFObjectAccessor<2,dim>::get_dof_indices (std::vector<unsigned int> &dof_indices) const
{
  Assert (this->dof_handler != 0,
	  typename DoFAccessor<dim>::ExcInvalidObject());
  Assert (&this->get_fe() != 0,
	  typename DoFAccessor<dim>::ExcInvalidObject());
  Assert (dof_indices.size() == (4*this->dof_handler->get_fe().dofs_per_vertex +
				 4*this->dof_handler->get_fe().dofs_per_line +
				 this->dof_handler->get_fe().dofs_per_quad),
	  typename DoFAccessor<dim>::ExcVectorDoesNotMatch());

				   // this function really only makes
				   // sense on non-active objects if
				   // all degrees of freedom are
				   // located on vertices, since
				   // otherwise there are degrees of
				   // freedom on sub-objects which are
				   // not allocated for this
				   // non-active thing
  Assert (!this->has_children() ||
	  (this->dof_handler->get_fe().dofs_per_cell ==
	   4*this->dof_handler->get_fe().dofs_per_vertex),
	  typename DoFAccessor<dim>::ExcNotActive());
  
  
  const unsigned int dofs_per_vertex = this->dof_handler->get_fe().dofs_per_vertex,
		     dofs_per_line   = this->dof_handler->get_fe().dofs_per_line,
		     dofs_per_quad   = this->dof_handler->get_fe().dofs_per_quad;
  std::vector<unsigned int>::iterator next = dof_indices.begin();
  for (unsigned int vertex=0; vertex<4; ++vertex)
    for (unsigned int d=0; d<dofs_per_vertex; ++d)
      *next++ = vertex_dof_index(vertex,d);
  for (unsigned int line=0; line<4; ++line)
    for (unsigned int d=0; d<dofs_per_line; ++d)
      *next++ = this->line(line)->dof_index(d);
  for (unsigned int d=0; d<dofs_per_quad; ++d)
    *next++ = dof_index(d);
}



template <int dim>
inline
TriaIterator<dim,DoFObjectAccessor<1,dim> >
DoFObjectAccessor<2,dim>::line (const unsigned int i) const
{
  Assert (i<4, ExcIndexRange (i, 0, 4));

  return TriaIterator<dim,DoFObjectAccessor<1,dim> >
    (
      this->tria,
      this->present_level,
      this->line_index (i),
      this->dof_handler
    );
}



template <int dim>
inline
TriaIterator<dim,DoFObjectAccessor<2,dim> >
DoFObjectAccessor<2,dim>::child (const unsigned int i) const
{
  TriaIterator<dim,DoFObjectAccessor<2,dim> > q (this->tria,
						 this->present_level+1,
						 this->child_index (i),
						 this->dof_handler);
  
#ifdef DEBUG
  if (q.state() != IteratorState::past_the_end)
    Assert (q->used(), typename TriaAccessor<dim>::ExcUnusedCellAsChild());
#endif
  return q;
}



template <int dim>
template <typename number, typename OutputVector>
inline
void
DoFObjectAccessor<2, dim>::
distribute_local_to_global (const Vector<number> &local_source,
			    OutputVector         &global_destination) const
{
				   // since the exception classes are
				   // from a template dependent base
				   // class, we have to fully qualify
				   // them. to work around more
				   // trouble, typedef the template
				   // dependent base class to a
				   // non-template dependent name and
				   // use that to specify the
				   // qualified exception names
  typedef DoFAccessor<dim> BaseClass;
  
  Assert (this->dof_handler != 0,
	  typename BaseClass::ExcInvalidObject());
  Assert (&this->get_fe() != 0,
	  typename BaseClass::ExcInvalidObject());
  Assert (local_source.size() == (4*this->dof_handler->get_fe().dofs_per_vertex +
				  4*this->dof_handler->get_fe().dofs_per_line +
				  this->dof_handler->get_fe().dofs_per_quad),
	  typename BaseClass::ExcVectorDoesNotMatch());
  Assert (this->dof_handler->n_dofs() == global_destination.size(),
	  typename BaseClass::ExcVectorDoesNotMatch());

  const unsigned int n_dofs = local_source.size();
  
//TODO[WB]: This function could me made more efficient. First, it allocates memory, which could be avoided by passing in another argument as a scratch array. second, the elementwise access is really slow if we use PETSc vectors/matrices. This should be fixed eventually

				   // get indices of dofs
  std::vector<unsigned int> dofs (n_dofs);
  get_dof_indices (dofs);
  
				   // distribute cell vector
  for (unsigned int j=0; j<n_dofs; ++j)
    global_destination(dofs[j]) += local_source(j);
}



template <int dim>
template <typename number, typename OutputMatrix>
inline
void
DoFObjectAccessor<2, dim>::
distribute_local_to_global (const FullMatrix<number> &local_source,
			    OutputMatrix             &global_destination) const
{
				   // since the exception classes are
				   // from a template dependent base
				   // class, we have to fully qualify
				   // them. to work around more
				   // trouble, typedef the template
				   // dependent base class to a
				   // non-template dependent name and
				   // use that to specify the
				   // qualified exception names
  typedef DoFAccessor<dim> BaseClass;
  
  Assert (this->dof_handler != 0,
	  typename BaseClass::ExcInvalidObject());
  Assert (&this->get_fe() != 0,
	  typename BaseClass::ExcInvalidObject());
  Assert (local_source.m() == (4*this->dof_handler->get_fe().dofs_per_vertex +
                               4*this->dof_handler->get_fe().dofs_per_line +
                               this->dof_handler->get_fe().dofs_per_quad),
	  typename BaseClass::ExcVectorDoesNotMatch());
  Assert (local_source.m() == local_source.n(),
	  typename BaseClass::ExcMatrixDoesNotMatch());
  Assert (this->dof_handler->n_dofs() == global_destination.m(),
	  typename BaseClass::ExcMatrixDoesNotMatch());
  Assert (global_destination.m() == global_destination.n(),
	  typename BaseClass::ExcMatrixDoesNotMatch());

  const unsigned int n_dofs = local_source.m();

//TODO[WB]: This function could me made more efficient. First, it allocates memory, which could be avoided by passing in another argument as a scratch array. second, the elementwise access is really slow if we use PETSc vectors/matrices. This should be fixed eventually

				   // get indices of dofs
  std::vector<unsigned int> dofs (n_dofs);
  get_dof_indices (dofs);
  
				   // distribute cell matrix
  for (unsigned int i=0; i<n_dofs; ++i)
    for (unsigned int j=0; j<n_dofs; ++j)
      global_destination.add(dofs[i], dofs[j], local_source(i,j));
}



template <int dim>
inline
void
DoFObjectAccessor<2,dim>::copy_from (const DoFObjectAccessor<2,dim> &a)
{
  BaseClass::copy_from (a);
  this->set_dof_handler (a.dof_handler);
}


/*------------------------- Functions: DoFObjectAccessor<3,dim> -----------------------*/


template <int dim>
inline
DoFObjectAccessor<3,dim>::
DoFObjectAccessor (const Triangulation<dim> *tria,
                   const int                 level,
                   const int                 index,
                   const AccessorData       *local_data)
                :
                DoFAccessor<dim> (local_data),
                DoFObjectAccessor_Inheritance<3,dim>::BaseClass (tria,
                                                                 level,
                                                                 index)
{}



template <int dim>
inline
unsigned int
DoFObjectAccessor<3,dim>::dof_index (const unsigned int i) const
{
  Assert (this->dof_handler != 0,
	  typename DoFAccessor<dim>::ExcInvalidObject());
				   // make sure a FE has been selected
				   // and enough room was reserved
  Assert (&this->get_fe() != 0,
	  typename DoFAccessor<dim>::ExcInvalidObject());
  Assert (i<this->get_fe().dofs_per_hex,
	  ExcIndexRange (i, 0, this->get_fe().dofs_per_hex));

  return this->dof_handler->levels[this->present_level]
    ->hex_dofs[this->present_index*this->get_fe().dofs_per_hex+i];
}



template <int dim>
inline
unsigned int
DoFObjectAccessor<3,dim>::vertex_dof_index (const unsigned int vertex,
					    const unsigned int i) const
{
  Assert (this->dof_handler != 0,
	  typename DoFAccessor<dim>::ExcInvalidObject());
  Assert (&this->get_fe() != 0,
	  typename DoFAccessor<dim>::ExcInvalidObject());
  Assert (vertex<8, ExcIndexRange (i,0,8));
  Assert (i<this->get_fe().dofs_per_vertex,
	  ExcIndexRange (i, 0, this->get_fe().dofs_per_vertex));

  const unsigned int dof_number = (this->vertex_index(vertex) *
				   this->get_fe().dofs_per_vertex +
				   i);
  return this->dof_handler->vertex_dofs[dof_number];
}



template <int dim>
inline
void
DoFObjectAccessor<3,dim>::get_dof_indices (std::vector<unsigned int> &dof_indices) const
{
  Assert (this->dof_handler != 0,
	  typename DoFAccessor<dim>::ExcInvalidObject());
  Assert (&this->get_fe() != 0,
	  typename DoFAccessor<dim>::ExcInvalidObject());
  Assert (dof_indices.size() == (8*this->dof_handler->get_fe().dofs_per_vertex +
				 12*this->dof_handler->get_fe().dofs_per_line +
				 6*this->dof_handler->get_fe().dofs_per_quad +
				 this->dof_handler->get_fe().dofs_per_hex),
	  typename DoFAccessor<dim>::ExcVectorDoesNotMatch());

				   // this function really only makes
				   // sense on non-active objects if
				   // all degrees of freedom are
				   // located on vertices, since
				   // otherwise there are degrees of
				   // freedom on sub-objects which are
				   // not allocated for this
				   // non-active thing
  Assert (!this->has_children() ||
	  (this->dof_handler->get_fe().dofs_per_cell ==
	   8*this->dof_handler->get_fe().dofs_per_vertex),
	  typename DoFAccessor<dim>::ExcNotActive());
  
  const unsigned int dofs_per_vertex = this->dof_handler->get_fe().dofs_per_vertex,
		     dofs_per_line   = this->dof_handler->get_fe().dofs_per_line,
		     dofs_per_quad   = this->dof_handler->get_fe().dofs_per_quad,
		     dofs_per_hex    = this->dof_handler->get_fe().dofs_per_hex;
  std::vector<unsigned int>::iterator next = dof_indices.begin();
  for (unsigned int vertex=0; vertex<8; ++vertex)
    for (unsigned int d=0; d<dofs_per_vertex; ++d)
      *next++ = vertex_dof_index(vertex,d);
  for (unsigned int line=0; line<12; ++line)
    for (unsigned int d=0; d<dofs_per_line; ++d)
      *next++ = this->line(line)->dof_index(d);
  for (unsigned int quad=0; quad<6; ++quad)
    for (unsigned int d=0; d<dofs_per_quad; ++d)
      *next++ = this->quad(quad)->dof_index(d);
  for (unsigned int d=0; d<dofs_per_hex; ++d)
    *next++ = dof_index(d);
}



template <int dim>
inline
TriaIterator<dim,DoFObjectAccessor<1,dim> >
DoFObjectAccessor<3,dim>::line (const unsigned int i) const
{
  TriaIterator<dim,TriaObjectAccessor<1,dim> > l = BaseClass::line(i);
  return TriaIterator<dim,DoFObjectAccessor<1,dim> >
    (
      this->tria,
      this->present_level,
      l->index(),
      this->dof_handler
    );
}



template <int dim>
inline
TriaIterator<dim,DoFObjectAccessor<2,dim> >
DoFObjectAccessor<3,dim>::quad (const unsigned int i) const
{
  Assert (i<6, ExcIndexRange (i, 0, 6));

  return TriaIterator<dim,DoFObjectAccessor<2,dim> >
    (
      this->tria,
      this->present_level,
      this->quad_index (i),
      this->dof_handler
    );
}



template <int dim>
inline
TriaIterator<dim,DoFObjectAccessor<3,dim> >
DoFObjectAccessor<3,dim>::child (const unsigned int i) const
{
  TriaIterator<dim,DoFObjectAccessor<3,dim> > q (this->tria,
						 this->present_level+1,
						 this->child_index (i),
						 this->dof_handler);
  
#ifdef DEBUG
  if (q.state() != IteratorState::past_the_end)
    Assert (q->used(), typename TriaAccessor<dim>::ExcUnusedCellAsChild());
#endif
  return q;
}



template <int dim>
template <typename number, typename OutputVector>
inline
void
DoFObjectAccessor<3, dim>::
distribute_local_to_global (const Vector<number> &local_source,
			    OutputVector         &global_destination) const
{
				   // since the exception classes are
				   // from a template dependent base
				   // class, we have to fully qualify
				   // them. to work around more
				   // trouble, typedef the template
				   // dependent base class to a
				   // non-template dependent name and
				   // use that to specify the
				   // qualified exception names
  typedef DoFAccessor<dim> BaseClass;
  
  Assert (this->dof_handler != 0,
	  typename BaseClass::ExcInvalidObject());
  Assert (&this->get_fe() != 0,
	  typename BaseClass::ExcInvalidObject());
  Assert (local_source.size() == (8*this->get_fe().dofs_per_vertex +
                                  12*this->get_fe().dofs_per_line +
                                  6*this->get_fe().dofs_per_quad +
                                  this->get_fe().dofs_per_hex),
	  typename BaseClass::ExcVectorDoesNotMatch());
  Assert (this->dof_handler->n_dofs() == global_destination.size(),
	  typename BaseClass::ExcVectorDoesNotMatch());

  const unsigned int n_dofs = local_source.size();
  
//TODO[WB]: This function could me made more efficient. First, it allocates memory, which could be avoided by passing in another argument as a scratch array. second, the elementwise access is really slow if we use PETSc vectors/matrices. This should be fixed eventually

				   // get indices of dofs
  std::vector<unsigned int> dofs (n_dofs);
  get_dof_indices (dofs);
  
				   // distribute cell vector
  for (unsigned int j=0; j<n_dofs; ++j)
    global_destination(dofs[j]) += local_source(j);
}



template <int dim>
template <typename number, typename OutputMatrix>
inline
void
DoFObjectAccessor<3, dim>::
distribute_local_to_global (const FullMatrix<number> &local_source,
			    OutputMatrix             &global_destination) const
{
				   // since the exception classes are
				   // from a template dependent base
				   // class, we have to fully qualify
				   // them. to work around more
				   // trouble, typedef the template
				   // dependent base class to a
				   // non-template dependent name and
				   // use that to specify the
				   // qualified exception names
  typedef DoFAccessor<dim> BaseClass;
  
  Assert (this->dof_handler != 0,
	  typename BaseClass::ExcInvalidObject());
  Assert (&this->get_fe() != 0,
	  typename BaseClass::ExcInvalidObject());
  Assert (local_source.m() == (8*this->get_fe().dofs_per_vertex +
                               12*this->get_fe().dofs_per_line +
                               6*this->get_fe().dofs_per_quad +
                               this->get_fe().dofs_per_hex),
	  typename BaseClass::ExcVectorDoesNotMatch());
  Assert (local_source.m() == local_source.n(),
	  typename BaseClass::ExcMatrixDoesNotMatch());
  Assert (this->dof_handler->n_dofs() == global_destination.m(),
	  typename BaseClass::ExcMatrixDoesNotMatch());
  Assert (global_destination.m() == global_destination.n(),
	  typename BaseClass::ExcMatrixDoesNotMatch());

  const unsigned int n_dofs = local_source.m();

//TODO[WB]: This function could me made more efficient. First, it allocates memory, which could be avoided by passing in another argument as a scratch array. second, the elementwise access is really slow if we use PETSc vectors/matrices. This should be fixed eventually

				   // get indices of dofs
  std::vector<unsigned int> dofs (n_dofs);
  get_dof_indices (dofs);
  
				   // distribute cell matrix
  for (unsigned int i=0; i<n_dofs; ++i)
    for (unsigned int j=0; j<n_dofs; ++j)
      global_destination.add(dofs[i], dofs[j], local_source(i,j));
}



template <int dim>
void DoFObjectAccessor<3,dim>::copy_from (const DoFObjectAccessor<3,dim> &a)
{
  BaseClass::copy_from (a);
  this->set_dof_handler (a.dof_handler);
}


/*------------------------- Functions: DoFCellAccessor -----------------------*/


template <int dim>
inline
DoFCellAccessor<dim>::
DoFCellAccessor (const Triangulation<dim> *tria,
                 const int                 level,
                 const int                 index,
                 const AccessorData       *local_data)
                :
                DoFObjectAccessor<dim, dim> (tria,level,index,local_data)
{}


template <int dim>
inline
TriaIterator<dim,DoFCellAccessor<dim> >
DoFCellAccessor<dim>::neighbor (const unsigned int i) const
{
  TriaIterator<dim,DoFCellAccessor<dim> > q (this->tria,
					     this->neighbor_level (i),
					     this->neighbor_index (i),
					     this->dof_handler);
  
#ifdef DEBUG
  if (q.state() != IteratorState::past_the_end)
    Assert (q->used(), typename TriaAccessor<dim>::ExcUnusedCellAsNeighbor());
#endif
  return q;
}


template <int dim>
inline
TriaIterator<dim,DoFCellAccessor<dim> >
DoFCellAccessor<dim>::child (const unsigned int i) const
{
  TriaIterator<dim,DoFCellAccessor<dim> > q (this->tria,
					     this->present_level+1,
					     this->child_index (i),
					     this->dof_handler);
  
#ifdef DEBUG
  if (q.state() != IteratorState::past_the_end)
    Assert (q->used(), typename TriaAccessor<dim>::ExcUnusedCellAsChild());
#endif
  return q;
}


#endif
