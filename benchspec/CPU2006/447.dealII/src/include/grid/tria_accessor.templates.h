//----------------------------  tria_accessor.templates.h  ---------------------------
//    $Id: tria_accessor.templates.h,v 1.1 2004/09/14 00:53:34 wolf Exp $
//    Version: $Name:  $
//
//    Copyright (C) 1998, 1999, 2000, 2001, 2002, 2003, 2004 by the deal.II authors
//
//    This file is subject to QPL and may not be  distributed
//    without copyright and license information. Please refer
//    to the file deal.II/doc/license.html for the  text  and
//    further information on this license.
//
//----------------------------  tria_accessor.templates.h  ---------------------------
#ifndef __deal2__tria_accessor_templates_h
#define __deal2__tria_accessor_templates_h


#include <base/config.h>
#include <grid/tria.h>
#include <grid/tria_levels.h>
#include <grid/tria_iterator.h>
#include <grid/tria_accessor.h>
#include <grid/tria_iterator.templates.h>
#include <grid/geometry_info.h>

#include <cmath>


//TODO[WB]: TriaObjectAccessor<dim,N> should never be used since we have specializations. Can we remove the implementations of functions, or what are they there for?

/*------------------------ Functions: TriaAccessor ---------------------------*/

template <int dim>
inline
TriaAccessor<dim>::TriaAccessor (const Triangulation<dim> *parent,
                                 const int                 level,
                                 const int                 index,
                                 const AccessorData       *)
                :
                present_level (level),
                present_index (index),
                tria (parent)
{}



template <int dim>
inline
void
TriaAccessor<dim>::copy_from (const TriaAccessor<dim> &a)
{
  present_level = a.present_level;
  present_index = a.present_index;
  tria = a.tria;
}



template <int dim>
inline
bool
TriaAccessor<dim>::operator == (const TriaAccessor<dim> &a) const
{
  Assert (tria == a.tria, ExcCantCompareIterators());
  return ((present_index == a.present_index) &&
	  (present_level == a.present_level));
}



template <int dim>
inline
bool
TriaAccessor<dim>::operator != (const TriaAccessor<dim> &a) const
{
  Assert (tria == a.tria, ExcCantCompareIterators());
  return ((present_index != a.present_index) ||
	  (present_level != a.present_level));
}



template <int dim>
inline
int
TriaAccessor<dim>::level () const
{
  return present_level;
}



template <int dim>
inline
int
TriaAccessor<dim>::index () const
{
  return present_index;
}



template <int dim>
inline
IteratorState::IteratorStates
TriaAccessor<dim>::state () const
{
  if ((present_level>=0) && (present_index>=0))
    return IteratorState::valid;
  else
    if ((present_level==-1) && (present_index==-1))
      return IteratorState::past_the_end;
    else
      return IteratorState::invalid;
}



template <int dim>
inline
const Triangulation<dim> &
TriaAccessor<dim>::get_triangulation () const
{
  return *tria;
}


/*------------------------ Functions: LineAccessor ---------------------------*/


template <int dim>
inline
TriaObjectAccessor<1,dim>::
TriaObjectAccessor (const Triangulation<dim> *parent,
                    const int                 level,
                    const int                 index,
                    const AccessorData       *local_data)
                :
                TriaAccessor<dim> (parent, level, index, local_data)
{}



template <int dim>
inline
bool
TriaObjectAccessor<1,dim>::used () const
{
  Assert (this->state() == IteratorState::valid,
	  typename TriaAccessor<dim>::ExcDereferenceInvalidObject());
  return this->tria->levels[this->present_level]->lines.used[this->present_index];
}



template <int dim>
inline
bool
TriaObjectAccessor<1,dim>::user_flag_set () const
{
  Assert (this->used(), typename TriaAccessor<dim>::ExcCellNotUsed());
  return this->tria->levels[this->present_level]->lines.user_flags[this->present_index];
}



template <int dim>
inline
void
TriaObjectAccessor<1,dim>::set_user_flag () const
{
  Assert (this->used(), typename TriaAccessor<dim>::ExcCellNotUsed());
  this->tria->levels[this->present_level]->lines.user_flags[this->present_index] = true;
}



template <int dim>
inline
void
TriaObjectAccessor<1,dim>::clear_user_flag () const
{
  Assert (this->used(), typename TriaAccessor<dim>::ExcCellNotUsed());
  this->tria->levels[this->present_level]->lines.user_flags[this->present_index] = false;
}



template <int dim>
inline
TriaIterator<dim,TriaObjectAccessor<1,dim> >
TriaObjectAccessor<1,dim>::child (const unsigned int i) const
{
  Assert (i<2, ExcIndexRange(i,0,2));
  
  TriaIterator<dim,TriaObjectAccessor<1,dim> >
    q (this->tria, this->present_level+1, child_index (i));
  
  Assert ((q.state() == IteratorState::past_the_end) || q->used(),
	  typename TriaAccessor<dim>::ExcUnusedCellAsChild());

  return q;
}



template <int dim>
inline
int
TriaObjectAccessor<1,dim>::child_index (unsigned const int i) const
{
  Assert (i<2, ExcIndexRange(i,0,2));
  return this->tria->levels[this->present_level]->lines.children[this->present_index]+i;
}



template <int dim>
inline
bool
TriaObjectAccessor<1,dim>::has_children () const
{
  Assert (this->state() == IteratorState::valid,
	  typename TriaAccessor<dim>::ExcDereferenceInvalidObject());
  return (this->tria->levels[this->present_level]->lines.children[this->present_index] != -1);
}



template <int dim>
inline
unsigned int
TriaObjectAccessor<1,dim>::max_refinement_depth () const
{
  if (!has_children())
    return 0;

  const unsigned int depths[2] = { child(0)->max_refinement_depth() + 1,
				   child(1)->max_refinement_depth() + 1  };
  return std::max (depths[0], depths[1]);
}



template <int dim>
inline
bool
TriaObjectAccessor<1,dim>::face_orientation (const unsigned int) const
{
  return true;
}



template <int dim>
inline
void
TriaObjectAccessor<1,dim>::operator ++ ()
{
  ++this->present_index;
				   // is index still in the range of
				   // the vector?
  while (this->present_index
	 >=
	 static_cast<int>(this->tria->levels[this->present_level]->lines.lines.size()))
    {
				       // no -> go one level up until we find
				       // one with more than zero cells
      ++this->present_level;
      this->present_index = 0;
				       // highest level reached?
      if (this->present_level >= static_cast<int>(this->tria->levels.size()))
	{
					   // return with past the end pointer
	  this->present_level = this->present_index = -1;
	  return;
	};
    };
}



template <int dim>
inline
void
TriaObjectAccessor<1,dim>::operator -- ()
{
  --this->present_index;
				   // is index still in the range of
				   // the vector?
  while (this->present_index < 0) 
    {
				       // no -> go one level down
      --this->present_level;
				       // lowest level reached?
      if (this->present_level == -1) 
	{
					   // return with past the end pointer
	  this->present_level = this->present_index = -1;
	  return;
	};
				       // else
      this->present_index = this->tria->levels[this->present_level]->lines.lines.size()-1;
    };
}


/*------------------------ Functions: QuadAccessor ---------------------------*/


template <int dim>
inline
TriaObjectAccessor<2,dim>::
TriaObjectAccessor (const Triangulation<dim> *parent,
                    const int                 level,
                    const int                 index,
                    const AccessorData       *local_data)
                :
                TriaAccessor<dim> (parent, level, index, local_data)
{}



template <int dim>
inline
bool
TriaObjectAccessor<2,dim>::used () const
{
  Assert (this->state() == IteratorState::valid,
	  typename TriaAccessor<dim>::ExcDereferenceInvalidObject());
  return this->tria->levels[this->present_level]->quads.used[this->present_index];
}



template <int dim>
inline
bool
TriaObjectAccessor<2,dim>::user_flag_set () const
{
  Assert (this->used(), typename TriaAccessor<dim>::ExcCellNotUsed());
  return this->tria->levels[this->present_level]->quads.user_flags[this->present_index];
}



template <int dim>
inline
void
TriaObjectAccessor<2,dim>::set_user_flag () const
{
  Assert (this->used(), typename TriaAccessor<dim>::ExcCellNotUsed());
  this->tria->levels[this->present_level]->quads.user_flags[this->present_index] = true;
}



template <int dim>
inline
void
TriaObjectAccessor<2,dim>::clear_user_flag () const
{
  Assert (this->used(), typename TriaAccessor<dim>::ExcCellNotUsed());
  this->tria->levels[this->present_level]->quads.user_flags[this->present_index] = false;
}



template <int dim>
inline
TriaIterator<dim,TriaObjectAccessor<1,dim> >
TriaObjectAccessor<2,dim>::line (const unsigned int i) const
{
  return
    TriaIterator<dim,TriaObjectAccessor<1,dim> >
    (
      this->tria,
      this->present_level,
      line_index (i)
    );
}



template <int dim>
inline
unsigned int
TriaObjectAccessor<2,dim>::line_index (const unsigned int i) const
{
  Assert (i<4, ExcIndexRange(i,0,4));

  return this->tria->levels[this->present_level]->quads.quads[this->present_index].line(i);
}



template <int dim>
inline
TriaIterator<dim,TriaObjectAccessor<2,dim> >
TriaObjectAccessor<2,dim>::child (const unsigned int i) const
{
  Assert (i<4, ExcIndexRange(i,0,4));
  
  TriaIterator<dim,TriaObjectAccessor<2,dim> >
    q (this->tria, this->present_level+1, child_index (i));
  
  Assert ((q.state() == IteratorState::past_the_end) || q->used(),
	  typename TriaAccessor<dim>::ExcUnusedCellAsChild());

  return q;
}



template <int dim>
inline
int TriaObjectAccessor<2,dim>::child_index (const unsigned int i) const
{
  Assert (i<4, ExcIndexRange(i,0,4));
  return this->tria->levels[this->present_level]->quads.children[this->present_index]+i;
}



template <int dim>
inline
bool
TriaObjectAccessor<2,dim>::has_children () const
{
  Assert (this->state() == IteratorState::valid,
	  typename TriaAccessor<dim>::ExcDereferenceInvalidObject());
  return (this->tria->levels[this->present_level]->quads.children[this->present_index] != -1);
}



template <int dim>
inline
unsigned int
TriaObjectAccessor<2,dim>::max_refinement_depth () const
{
  if (!has_children())
    return 0;

  const unsigned int depths[4] = { child(0)->max_refinement_depth() + 1,
				   child(1)->max_refinement_depth() + 1,
				   child(2)->max_refinement_depth() + 1,
				   child(3)->max_refinement_depth() + 1 };
  return std::max (std::max (depths[0], depths[1]),
		   std::max (depths[2], depths[3]));
}



template <int dim>
inline
bool
TriaObjectAccessor<2,dim>::face_orientation (const unsigned int) const
{
  return true;
}



template <int dim>
inline
void
TriaObjectAccessor<2,dim>::operator ++ ()
{
  ++this->present_index;
				   // is index still in the range of
				   // the vector?
  while (this->present_index
	 >=
	 static_cast<int>(this->tria->levels[this->present_level]->quads.quads.size()))
    {
				       // no -> go one level up
      ++this->present_level;
      this->present_index = 0;
				       // highest level reached?
      if (this->present_level >= static_cast<int>(this->tria->levels.size()))
	{
					   // return with past the end pointer
	  this->present_level = this->present_index = -1;
	  return;
	};
    };
}



template <int dim>
inline
void
TriaObjectAccessor<2,dim>::operator -- ()
{
  --this->present_index;
				   // is index still in the range of
				   // the vector?
  while (this->present_index < 0) 
    {
				       // no -> go one level down
      --this->present_level;
				       // lowest level reached?
      if (this->present_level == -1) 
	{
					   // return with past the end pointer
	  this->present_level = this->present_index = -1;
	  return;
	};
				       // else
      this->present_index = this->tria->levels[this->present_level]->quads.quads.size()-1;
    };
}


/*------------------------ Functions: HexAccessor ---------------------------*/


template <int dim>
inline
TriaObjectAccessor<3,dim>::
TriaObjectAccessor (const Triangulation<dim> *parent,
                    const int                 level,
                    const int                 index,
                    const AccessorData       *local_data)
                :
                TriaAccessor<dim> (parent, level, index, local_data)
{}



template <int dim>
inline
bool
TriaObjectAccessor<3,dim>::used () const
{
  Assert (this->state() == IteratorState::valid,
	  typename TriaAccessor<dim>::ExcDereferenceInvalidObject());
  return this->tria->levels[this->present_level]->hexes.used[this->present_index];
}



template <int dim>
inline
bool
TriaObjectAccessor<3,dim>::user_flag_set () const
{
  Assert (this->used(), typename TriaAccessor<dim>::ExcCellNotUsed());
  return this->tria->levels[this->present_level]->hexes.user_flags[this->present_index];
}



template <int dim>
inline
void
TriaObjectAccessor<3,dim>::set_user_flag () const
{
  Assert (this->used(), typename TriaAccessor<dim>::ExcCellNotUsed());
  this->tria->levels[this->present_level]->hexes.user_flags[this->present_index] = true;
}



template <int dim>
inline
void TriaObjectAccessor<3,dim>::clear_user_flag () const
{
  Assert (this->used(), typename TriaAccessor<dim>::ExcCellNotUsed());
  this->tria->levels[this->present_level]->hexes.user_flags[this->present_index] = false;
}



template <int dim>
inline
TriaIterator<dim,TriaObjectAccessor<1,dim> >
TriaObjectAccessor<3,dim>::line (const unsigned int i) const
{
  Assert (this->used(), typename TriaAccessor<dim>::ExcCellNotUsed());
  Assert (i<12, ExcIndexRange (i,0,12));

                                   // get the line index by asking the
                                   // quads. make sure we handle
                                   // reverted faces correctly
                                   //
                                   // so set up a table that for each
                                   // line describes a) from which
                                   // quad to take it, b) which line
                                   // therein it is if the face is
                                   // oriented correctly, and c) if in
                                   // the opposite direction
  static const unsigned int lookup_table[12][3] =
    { { 0, 0, 3 }, // take first four lines from front face
      { 0, 1, 2 },
      { 0, 2, 1 },
      { 0, 3, 0 },

      { 1, 0, 3 }, // second four lines from back face
      { 1, 1, 2 },
      { 1, 2, 1 },
      { 1, 3, 0 },

      { 2, 3, 0 }, // the rest randomly
      { 2, 1, 2 },
      { 4, 1, 2 },
      { 4, 3, 0 }};

  return (this->quad(lookup_table[i][0])
          ->line(face_orientation(lookup_table[i][0]) ?
                 lookup_table[i][1] :
                 lookup_table[i][2]));
}



template <int dim>
inline
TriaIterator<dim,TriaObjectAccessor<2,dim> >
TriaObjectAccessor<3,dim>::quad (const unsigned int i) const
{
  Assert (this->used(), typename TriaAccessor<dim>::ExcCellNotUsed());
  return
    TriaIterator<dim,TriaObjectAccessor<2,dim> >
    (
      this->tria,
      this->present_level,
      quad_index (i)
    );
}



template <int dim>
inline
unsigned int
TriaObjectAccessor<3,dim>::line_index (const unsigned int i) const
{
  Assert (i<12, ExcIndexRange(i,0,12));

                                   // get the line index by asking the
                                   // quads. make sure we handle
                                   // reverted faces correctly
                                   //
                                   // so set up a table that for each
                                   // line describes a) from which
                                   // quad to take it, b) which line
                                   // therein it is if the face is
                                   // oriented correctly, and c) if in
                                   // the opposite direction
  static const unsigned int lookup_table[12][3] =
    { { 0, 0, 3 }, // take first four lines from front face
      { 0, 1, 2 },
      { 0, 2, 1 },
      { 0, 3, 0 },

      { 1, 0, 3 }, // second four lines from back face
      { 1, 1, 2 },
      { 1, 2, 1 },
      { 1, 3, 0 },

      { 2, 3, 0 }, // the rest randomly
      { 2, 1, 2 },
      { 4, 1, 2 },
      { 4, 3, 0 }};

  return (this->quad(lookup_table[i][0])
          ->line_index(face_orientation(lookup_table[i][0]) ?
                       lookup_table[i][1] :
                       lookup_table[i][2]));
}



template <int dim>
inline
unsigned int
TriaObjectAccessor<3,dim>::quad_index (const unsigned int i) const
{
  Assert (i<6, ExcIndexRange(i,0,6));

  return this->tria->levels[this->present_level]->hexes.hexes[this->present_index].quad(i);
}



template <int dim>
inline
TriaIterator<dim,TriaObjectAccessor<3,dim> >
TriaObjectAccessor<3,dim>::child (const unsigned int i) const
{
  Assert (i<8, ExcIndexRange(i,0,8));
  
  TriaIterator<dim,TriaObjectAccessor<3,dim> > q (this->tria, this->present_level+1, child_index (i));
  
  Assert ((q.state() == IteratorState::past_the_end) || q->used(),
	  typename TriaAccessor<dim>::ExcUnusedCellAsChild());

  return q;
}



template <int dim>
inline
int TriaObjectAccessor<3,dim>::child_index (const unsigned int i) const
{
  Assert (i<8, ExcIndexRange(i,0,8));
  return this->tria->levels[this->present_level]->hexes.children[this->present_index]+i;
}



template <int dim>
bool TriaObjectAccessor<3,dim>::has_children () const
{
  Assert (this->state() == IteratorState::valid,
	  typename TriaAccessor<dim>::ExcDereferenceInvalidObject());
  return (this->tria->levels[this->present_level]->hexes.children[this->present_index] != -1);
}



template <int dim>
inline
unsigned int
TriaObjectAccessor<3,dim>::max_refinement_depth () const
{
  if (!has_children())
    return 0;

  const unsigned int depths[8] = { child(0)->max_refinement_depth() + 1,
				   child(1)->max_refinement_depth() + 1,
				   child(2)->max_refinement_depth() + 1,
				   child(3)->max_refinement_depth() + 1,
				   child(4)->max_refinement_depth() + 1,
				   child(5)->max_refinement_depth() + 1,
				   child(6)->max_refinement_depth() + 1,
				   child(7)->max_refinement_depth() + 1  };
  return std::max (std::max (std::max (depths[0], depths[1]),
			     std::max (depths[2], depths[3])),
		   std::max (std::max (depths[4], depths[5]),
			     std::max (depths[6], depths[7])));
}



template <int dim>
inline
bool
TriaObjectAccessor<3, dim>::
face_orientation (const unsigned int face) const
{
  Assert (used(), typename TriaAccessor<dim>::ExcCellNotUsed());
  Assert (face<GeometryInfo<3>::faces_per_cell,
          ExcIndexRange (face, 0, GeometryInfo<3>::faces_per_cell));
  Assert (this->present_index * GeometryInfo<3>::faces_per_cell + face
          < this->tria->levels[this->present_level]
          ->hexes.face_orientations.size(),
          ExcInternalError());
          
  return (this->tria->levels[this->present_level]
          ->hexes.face_orientations[this->present_index *
                                    GeometryInfo<3>::faces_per_cell
                                    +
                                    face]);
}



template <int dim>
inline
void
TriaObjectAccessor<3,dim>::operator ++ ()
{
  ++this->present_index;
				   // is index still in the range of
				   // the vector?
  while (this->present_index
	 >=
	 static_cast<int>(this->tria->levels[this->present_level]->hexes.hexes.size()))
    {
				       // no -> go one level up
      ++this->present_level;
      this->present_index = 0;
				       // highest level reached?
      if (this->present_level >= static_cast<int>(this->tria->levels.size()))
	{
					   // return with past the end pointer
	  this->present_level = this->present_index = -1;
	  return;
	};
    };
}



template <int dim>
inline
void
TriaObjectAccessor<3,dim>::operator -- ()
{
  --this->present_index;
				   // is index still in the range of
				   // the vector?
  while (this->present_index < 0) 
    {
				       // no -> go one level down
      --this->present_level;
				       // lowest level reached?
      if (this->present_level == -1) 
	{
					   // return with past the end pointer
	  this->present_level = this->present_index = -1;
	  return;
	};
				       // else
      this->present_index = this->tria->levels[this->present_level]->hexes.hexes.size()-1;
    };
}


/*------------------------ Functions: TriaObjectAccessor ---------------------------*/



/*------------------------ Functions: CellAccessor<dim> -----------------------*/


template <int dim>
inline
CellAccessor<dim>::
CellAccessor (const Triangulation<dim> *parent,
              const int                 level,
              const int                 index,
              const AccessorData       *local_data)
                :
                TriaObjectAccessor<dim,dim> (parent, level, index, local_data)
{}



template <>
inline
TriaIterator<1,TriaObjectAccessor<0, 1> >
CellAccessor<1>::face (const unsigned int) const 
{
  Assert (false, TriaAccessor<1>::ExcNotUsefulForThisDimension());
  return TriaIterator<1,TriaObjectAccessor<0, 1> >();
}



template <>
inline
Triangulation<2>::face_iterator
CellAccessor<2>::face (const unsigned int i) const 
{
  return this->line(i);
}



template <>
inline
Triangulation<3>::face_iterator
CellAccessor<3>::face (const unsigned int i) const 
{
  return this->quad(i);
}



template <int dim>
inline
int
CellAccessor<dim>::neighbor_index (const unsigned int i) const 
{
  Assert (i<GeometryInfo<dim>::faces_per_cell,
	  typename TriaAccessor<dim>::ExcInvalidNeighbor(i));
  return this->tria->levels[this->present_level]->
    neighbors[this->present_index*GeometryInfo<dim>::faces_per_cell+i].second;
}



template <int dim>
inline
int
CellAccessor<dim>::neighbor_level (const unsigned int i) const
{
  Assert (i<GeometryInfo<dim>::faces_per_cell,
	  typename TriaAccessor<dim>::ExcInvalidNeighbor(i));
  return this->tria->levels[this->present_level]->
    neighbors[this->present_index*GeometryInfo<dim>::faces_per_cell+i].first;
}



template <int dim>
inline
bool
CellAccessor<dim>::refine_flag_set () const
{
  Assert (this->used(), typename TriaAccessor<dim>::ExcCellNotUsed());
				   // cells flagged for refinement must be active
				   // (the @p set_refine_flag function checks this,
				   // but activity may change when refinement is
				   // executed and for some reason the refine
				   // flag is not cleared).
  Assert (this->active() ||  !this->tria->levels[this->present_level]->refine_flags[this->present_index],
	  ExcRefineCellNotActive());
  return this->tria->levels[this->present_level]->refine_flags[this->present_index];
}



template <int dim>
inline
void
CellAccessor<dim>::set_refine_flag () const
{
  Assert (this->used() && this->active(), ExcRefineCellNotActive());
  Assert (!coarsen_flag_set(),
	  ExcCellFlaggedForCoarsening());
  
  this->tria->levels[this->present_level]->refine_flags[this->present_index] = true;
}



template <int dim>
inline
void
CellAccessor<dim>::clear_refine_flag () const
{
  Assert (this->used() && this->active(), ExcRefineCellNotActive());
  this->tria->levels[this->present_level]->refine_flags[this->present_index] = false;
}



template <int dim>
inline
bool
CellAccessor<dim>::coarsen_flag_set () const
{
  Assert (this->used(), typename TriaAccessor<dim>::ExcCellNotUsed());
				   // cells flagged for coarsening must be active
				   // (the @p set_refine_flag function checks this,
				   // but activity may change when refinement is
				   // executed and for some reason the refine
				   // flag is not cleared).
  Assert (this->active() ||  !this->tria->levels[this->present_level]->coarsen_flags[this->present_index],
	  ExcRefineCellNotActive());
  return this->tria->levels[this->present_level]->coarsen_flags[this->present_index];
}



template <int dim>
inline
void
CellAccessor<dim>::set_coarsen_flag () const
{
  Assert (this->used() && this->active(), ExcRefineCellNotActive());
  Assert (!refine_flag_set(), ExcCellFlaggedForRefinement());
  
  this->tria->levels[this->present_level]->coarsen_flags[this->present_index] = true;
}



template <int dim>
inline
void
CellAccessor<dim>::clear_coarsen_flag () const
{
  Assert (this->used() && this->active(), ExcRefineCellNotActive());
  this->tria->levels[this->present_level]->coarsen_flags[this->present_index] = false;
}



template <int dim>
inline
TriaIterator<dim,CellAccessor<dim> >
CellAccessor<dim>::neighbor (const unsigned int i) const
{
  TriaIterator<dim,CellAccessor<dim> >
    q (this->tria, neighbor_level (i), neighbor_index (i));

  Assert ((q.state() == IteratorState::past_the_end) || q->used(),
	  typename TriaAccessor<dim>::ExcUnusedCellAsNeighbor());

  return q;
}



template <int dim>
inline
TriaIterator<dim,CellAccessor<dim> >
CellAccessor<dim>::child (const unsigned int i) const
{
  TriaIterator<dim,CellAccessor<dim> >
    q (this->tria, this->present_level+1, this->child_index (i));

  Assert ((q.state() == IteratorState::past_the_end) || q->used(),
	  typename TriaAccessor<dim>::ExcUnusedCellAsChild());

  return q;
}



template <int dim>
inline
bool
CellAccessor<dim>::active () const
{
  return !this->has_children();
}


#endif
