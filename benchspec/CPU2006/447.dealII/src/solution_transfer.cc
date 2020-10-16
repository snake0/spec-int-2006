//----------------------------  solution_transfer.cc  ---------------------------
//    $Id: solution_transfer.cc,v 1.1 2004/09/14 00:51:25 wolf Exp $
//    Version: $Name:  $
//
//    Copyright (C) 1998, 1999, 2000, 2001, 2002, 2003 by the deal.II authors
//
//    This file is subject to QPL and may not be  distributed
//    without copyright and license information. Please refer
//    to the file deal.II/doc/license.html for the  text  and
//    further information on this license.
//
//----------------------------  solution_transfer.cc  ---------------------------


#include <base/memory_consumption.h>
#include <grid/tria.h>
#include <dofs/dof_handler.h>
#include <grid/tria_accessor.h>
#include <dofs/dof_accessor.h>
#include <grid/tria_iterator.h>
#include <fe/fe.h>
#include <lac/vector.h>
#include <numerics/solution_transfer.h>




template<int dim, typename number>
SolutionTransfer<dim, number>::SolutionTransfer(const DoFHandler<dim> &dof):
		dof_handler(&dof),
		n_dofs_old(0),
		prepared_for(none)
{}


template<int dim, typename number>
SolutionTransfer<dim, number>::~SolutionTransfer()
{
  clear ();
}


template<int dim, typename number>
void SolutionTransfer<dim, number>::clear ()
{
  if (indices_on_cell.size())
    indices_on_cell.erase(indices_on_cell.begin(), indices_on_cell.end());  
  if (all_pointerstructs.size())
    all_pointerstructs.erase(all_pointerstructs.begin(), all_pointerstructs.end());
  if (dof_values_on_cell.size())
    dof_values_on_cell.erase(dof_values_on_cell.begin(), dof_values_on_cell.end());

  prepared_for=none;
}


template<int dim, typename number>
void SolutionTransfer<dim, number>::prepare_for_pure_refinement()
{ 
  Assert(prepared_for!=pure_refinement, ExcAlreadyPrepForRef());
  Assert(prepared_for!=coarsening_and_refinement, 
	 ExcAlreadyPrepForCoarseAndRef());

  clear();

  const unsigned int n_active_cells = dof_handler->get_tria().n_active_cells();
  const unsigned int dofs_per_cell  = dof_handler->get_fe().dofs_per_cell;
  n_dofs_old=dof_handler->n_dofs();

  indices_on_cell=std::vector<std::vector<unsigned int> > (n_active_cells,
						 std::vector<unsigned int> (dofs_per_cell));

  typename DoFHandler<dim>::cell_iterator cell = dof_handler->begin(),
					  endc = dof_handler->end();

  for (unsigned int i=0; cell!=endc; ++cell) 
    {
      if (cell->active())
	{
					   // on each cell store the indices
					   // of the dofs. after refining we
					   // get the values on the children
					   // by taking these indices, getting
					   // the respective values out of
					   // the data vectors and prolonging
					   // them to the children
	  cell->get_dof_indices(indices_on_cell[i]);
	  cell->set_user_pointer(&indices_on_cell[i]);

	  ++i;	  
	}
      else
	cell->clear_user_pointer();
    }
  prepared_for=pure_refinement;
}


template<int dim, typename number>  
void
SolutionTransfer<dim, number>::refine_interpolate(const Vector<number> &in,
						  Vector<number>       &out) const
{
  Assert(prepared_for==pure_refinement, ExcNotPrepared());
  Assert(in.size()==n_dofs_old, ExcWrongVectorSize(in.size(),n_dofs_old));
  Assert(out.size()==dof_handler->n_dofs(),
	 ExcWrongVectorSize(out.size(),dof_handler->n_dofs()));
  Assert(&in != &out,
         ExcMessage ("Vectors cannot be used as input and output"
                     " at the same time!"));

  unsigned int dofs_per_cell=dof_handler->get_fe().dofs_per_cell;  
  Vector<number> local_values(dofs_per_cell);

  typename DoFHandler<dim>::cell_iterator cell = dof_handler->begin(),
					  endc = dof_handler->end();

  std::vector<unsigned int> *indexptr;  

  for (; cell!=endc; ++cell) 
    {
      if (cell->user_pointer())
					 // this cell was refined or not
					 // touched at all, so we can get
					 // the new values by just setting
					 // or interpolating to the children,
					 // which is both done by one
					 // function
	{
	  indexptr=static_cast<std::vector<unsigned int> *>(cell->user_pointer());
	  for (unsigned int i=0; i<dofs_per_cell; ++i)
	    local_values(i)=in(indexptr->operator[](i));
	  cell->set_dof_values_by_interpolation(local_values, out);
	}
    }
}


template<int dim, typename number>  
void SolutionTransfer<dim, number>::refine_interpolate (Vector<number> &vec) const
{
  Assert(vec.size()==n_dofs_old, ExcWrongVectorSize(vec.size(),n_dofs_old));

  Vector<number> vec_old(vec);
  vec.reinit(dof_handler->n_dofs());
  
  refine_interpolate(vec_old, vec);
}


template<int dim, typename number>
void
SolutionTransfer<dim, number>::
prepare_for_coarsening_and_refinement(const std::vector<Vector<number> > &all_in)
{
  Assert(prepared_for!=pure_refinement, ExcAlreadyPrepForRef());
  Assert(!prepared_for!=coarsening_and_refinement, 
	 ExcAlreadyPrepForCoarseAndRef());
  
  const unsigned int in_size=all_in.size();
  Assert(in_size!=0, ExcNoInVectorsGiven());

  clear();

  const unsigned int n_active_cells = dof_handler->get_tria().n_active_cells();
  const unsigned int dofs_per_cell  = dof_handler->get_fe().dofs_per_cell;
  n_dofs_old=dof_handler->n_dofs();

  for (unsigned int i=0; i<in_size; ++i)
    {
      Assert(all_in[i].size()==n_dofs_old,
	     ExcWrongVectorSize(all_in[i].size(),n_dofs_old));
    }

				   // first count the number
				   // of cells that'll be coarsened
				   // and that'll stay or be refined
  unsigned int n_cells_to_coarsen=0;
  unsigned int n_cells_to_stay_or_refine=0;
  typename DoFHandler<dim>::active_cell_iterator
    act_cell = dof_handler->begin_active(),
    endc = dof_handler->end();
  for (; act_cell!=endc; ++act_cell) 
    {
      if (act_cell->coarsen_flag_set())
	++n_cells_to_coarsen;
      else
	++n_cells_to_stay_or_refine;
    }
  Assert((n_cells_to_coarsen+n_cells_to_stay_or_refine)==n_active_cells,
	 ExcInternalError());
  Assert(n_cells_to_coarsen%GeometryInfo<dim>::children_per_cell==0,
	 ExcInternalError());
  Assert(n_cells_to_coarsen%GeometryInfo<dim>::children_per_cell==0,
	 ExcTriaPrepCoarseningNotCalledBefore());
  
  const unsigned int n_coarsen_fathers = n_cells_to_coarsen /
					 GeometryInfo<dim>::children_per_cell;

				   // allocate the needed memory
  indices_on_cell=std::vector<std::vector<unsigned int> > (
    n_cells_to_stay_or_refine,
    std::vector<unsigned int> (dofs_per_cell));
  
  dof_values_on_cell=std::vector<std::vector<Vector<number> > > (
    n_coarsen_fathers,
    std::vector<Vector<number> > (in_size, Vector<number> (dofs_per_cell)));

  all_pointerstructs=std::vector<Pointerstruct> (
    n_cells_to_stay_or_refine+n_coarsen_fathers);


				   // we need counters for
				   // the 'to_stay_or_refine' cells 'n_sr',
				   // the 'coarsen_fathers' cells 'n_cf',
				   // and all the cells where a
				   // @p{Pointerstruct} is needed 'n'
  unsigned int n_sr=0, n_cf=0, n=0;
  typename DoFHandler<dim>::cell_iterator cell = dof_handler->begin();  
  for (; cell!=endc; ++cell) 
    {
      if (cell->active() && !cell->coarsen_flag_set())
	{
					   // cell will not be coarsened,
					   // so we get away by storing the
					   // dof indices and later
					   // interpolating to the children
	  cell->get_dof_indices(indices_on_cell[n_sr]);
	  all_pointerstructs[n].indices_ptr=&indices_on_cell[n_sr];
	  all_pointerstructs[n].dof_values_ptr=0;
	  cell->set_user_pointer(&all_pointerstructs[n]);
	  ++n_sr;
	  ++n;
	}
      else if (cell->has_children() && cell->child(0)->coarsen_flag_set())
	{
					   // note that if one child has the
					   // coaresen flag, then all should
					   // have if Tria::prepare_* has
					   // worked correctly
	  for (unsigned int i=1; i<GeometryInfo<dim>::children_per_cell; ++i)
	    Assert(cell->child(i)->coarsen_flag_set(),
		   ExcTriaPrepCoarseningNotCalledBefore());
	      
	  for (unsigned int j=0; j<in_size; ++j)
	    {
					       // store the data of each of
					       // the input vectors
	      cell->get_interpolated_dof_values(all_in[j],
						dof_values_on_cell[n_cf][j]);
	    }
	  all_pointerstructs[n].indices_ptr=0;
	  all_pointerstructs[n].dof_values_ptr=&dof_values_on_cell[n_cf];
	  cell->set_user_pointer(&all_pointerstructs[n]);    
	  ++n_cf;
	  ++n;
	}
      else
					 // some cell on the lower levels to
					 // which nothing will happen
	cell->clear_user_pointer();
    }
  Assert(n_sr==n_cells_to_stay_or_refine, ExcInternalError());
  Assert(n_cf==n_coarsen_fathers, ExcInternalError());

  prepared_for=coarsening_and_refinement;
}


template<int dim, typename number>
void
SolutionTransfer<dim, number>::prepare_for_coarsening_and_refinement(const Vector<number> &in)
{
  std::vector<Vector<number> > all_in=std::vector<Vector<number> >(1, in);
  prepare_for_coarsening_and_refinement(all_in);
}


template<int dim, typename number>
void SolutionTransfer<dim, number>::
interpolate (const std::vector<Vector<number> > &all_in,
	     std::vector<Vector<number> >       &all_out) const
{
  Assert(prepared_for==coarsening_and_refinement, ExcNotPrepared());
  const unsigned int size=all_in.size();
  Assert(all_out.size()==size, ExcDimensionMismatch(all_out.size(), size));
  for (unsigned int i=0; i<size; ++i)
    Assert (all_in[i].size() == n_dofs_old,
	    ExcWrongVectorSize(all_in[i].size(), n_dofs_old));
  for (unsigned int i=0; i<all_out.size(); ++i)
    Assert (all_out[i].size() == dof_handler->n_dofs(),
	    ExcWrongVectorSize(all_out[i].size(), dof_handler->n_dofs()));
  for (unsigned int i=0; i<size; ++i)
    for (unsigned int j=0; j<size; ++j)
      Assert(&all_in[i] != &all_out[j],
             ExcMessage ("Vectors cannot be used as input and output"
                         " at the same time!"));

  const unsigned int dofs_per_cell=dof_handler->get_fe().dofs_per_cell;  
  Vector<number> local_values(dofs_per_cell);
  std::vector<unsigned int> dofs(dofs_per_cell);

  typename DoFHandler<dim>::cell_iterator cell = dof_handler->begin(),
					  endc = dof_handler->end();
  for (; cell!=endc; ++cell) 
    {
      if (cell->user_pointer())
	{
	  const Pointerstruct * const structptr
	    =static_cast<Pointerstruct *>(cell->user_pointer());

	  const std::vector<unsigned int> * const indexptr
	    =structptr->indices_ptr;

	  const std::vector<Vector<number> > * const valuesptr
	    =structptr->dof_values_ptr;

					   // cell stayed or is
					   // refined
	  if (indexptr)
	    {
	      Assert (structptr->dof_values_ptr == 0,
		      ExcInternalError());
	      
					       // get the values of
					       // each of the input
					       // data vectors on this
					       // cell and prolong it
					       // to its children
	      for (unsigned int j=0; j<size; ++j)
		{
		  for (unsigned int i=0; i<dofs_per_cell; ++i)
		    local_values(i)=all_in[j](indexptr->operator[](i));
		  cell->set_dof_values_by_interpolation(local_values,
							all_out[j]);
		}
	    }
					   // children of cell were
					   // deleted
	  else if (valuesptr)
	    {
	      Assert (!cell->has_children(), ExcInternalError());
	      Assert (structptr->indices_ptr == 0,
		      ExcInternalError());

					       // get the local
					       // indices
	      cell->get_dof_indices(dofs);

					       // distribute the
					       // stored data to the
					       // new vectors
	      for (unsigned int j=0; j<size; ++j)
		for (unsigned int i=0; i<dofs_per_cell; ++i)
		  all_out[j](dofs[i])=valuesptr->operator[](j)(i);
	    }
					   // undefined status
	  else
	    Assert(false, ExcInternalError());
	}
    }
}



template<int dim, typename number>
void SolutionTransfer<dim, number>::interpolate(const Vector<number> &in,
						Vector<number>       &out) const
{
  Assert (in.size()==n_dofs_old,
	  ExcWrongVectorSize(in.size(), n_dofs_old));
  Assert (out.size()==dof_handler->n_dofs(),
	  ExcWrongVectorSize(out.size(), dof_handler->n_dofs()));

  std::vector<Vector<number> > all_in(1);
  all_in[0] = in;
  std::vector<Vector<number> > all_out(1);
  all_out[0] = out;
  interpolate(all_in,
	      all_out);
  out=all_out[0];
}



template<int dim, typename number>
unsigned int
SolutionTransfer<dim, number>::memory_consumption () const
{
  return (MemoryConsumption::memory_consumption (dof_handler) +
	  MemoryConsumption::memory_consumption (n_dofs_old) +
	  sizeof (prepared_for) +
	  MemoryConsumption::memory_consumption (indices_on_cell) +
	  MemoryConsumption::memory_consumption (all_pointerstructs) +
	  MemoryConsumption::memory_consumption (dof_values_on_cell));
}



template<int dim, typename number>
unsigned int
SolutionTransfer<dim, number>::Pointerstruct::memory_consumption () const
{
  return sizeof(*this);
}



template class SolutionTransfer<deal_II_dimension, float>;
template class SolutionTransfer<deal_II_dimension, double>;

/*----------------------------   solution_transfer.cc     ----------------------*/
