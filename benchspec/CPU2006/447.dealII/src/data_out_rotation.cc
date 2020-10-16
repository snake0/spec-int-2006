//----------------------------  data_out_rotation.cc  ---------------------------
//    $Id: data_out_rotation.cc,v 1.1 2004/09/14 00:51:21 wolf Exp $
//    Version: $Name:  $
//
//    Copyright (C) 2000, 2001, 2002, 2003, 2004 by the deal.II authors
//
//    This file is subject to QPL and may not be  distributed
//    without copyright and license information. Please refer
//    to the file deal.II/doc/license.html for the  text  and
//    further information on this license.
//
//----------------------------  data_out_rotation.cc  ---------------------------


#include <base/quadrature_lib.h>
#include <base/thread_management.h>
#include <lac/vector.h>
#include <lac/block_vector.h>
#include <grid/tria.h>
#include <dofs/dof_handler.h>
#include <dofs/dof_accessor.h>
#include <grid/tria_iterator.h>
#include <fe/fe.h>
#include <fe/fe_values.h>
#include <fe/mapping_q1.h>
#include <numerics/data_out_rotation.h>

#include <cmath>



template <int dim>
void DataOutRotation<dim>::build_some_patches (Data data)
{
  QTrapez<1>     q_trapez;
  QIterated<dim> patch_points (q_trapez, data.n_subdivisions);

				   // since most output formats can't
				   // handle cells that are not
				   // transformed using a Q1 mapping,
				   // we don't support anything else
				   // as well
  static const MappingQ1<dim> mapping;
  FEValues<dim> fe_patch_values (mapping, this->dofs->get_fe(),
				 patch_points, update_values);

  const unsigned int n_patches_per_circle = data.n_patches_per_circle;

				   // another abbreviation denoting
				   // the number of q_points in each
				   // direction
  const unsigned int n_points = data.n_subdivisions+1;
  
				   // set up an array that holds the
				   // directions in the plane of
				   // rotation in which we will put
				   // points in the whole domain (not
				   // the rotationally reduced one in
				   // which the computation took
				   // place. for simplicity add the
				   // initial direction at the end
				   // again
  std::vector<Point<dim+1> > angle_directions (n_patches_per_circle+1);
  for (unsigned int i=0; i<=n_patches_per_circle; ++i)
    {
      angle_directions[i][0] = std::cos(2*deal_II_numbers::PI *
                                        i/n_patches_per_circle);
      angle_directions[i][1] = std::sin(2*deal_II_numbers::PI *
                                        i/n_patches_per_circle);
    };
  
  
  unsigned int cell_number = 0;
  typename std::vector< ::DataOutBase::Patch<dim+1> >::iterator
    patch = this->patches.begin();
  typename DoFHandler<dim>::cell_iterator cell=first_cell();

				   // get first cell in this thread
  for (unsigned int i=0; (i<data.this_thread)&&(cell != this->dofs->end()); ++i)
    {
      std::advance (patch, n_patches_per_circle);
      ++cell_number;
      cell=next_cell(cell);
    }

  				   // now loop over all cells and
				   // actually create the patches
  for (; cell != this->dofs->end(); )
    {
      for (unsigned int angle=0; angle<n_patches_per_circle; ++angle, ++patch)
	{
	  Assert (patch != this->patches.end(), ExcInternalError());
	  

					   // first compute the
					   // vertices of the
					   // patch. note that they
					   // will have to be computed
					   // from the vertices of the
					   // cell, which has one
					   // dimension less, however.
	  switch (dim)
	    {
	      case 1:
	      {
		const double r1 = cell->vertex(0)(0),
			     r2 = cell->vertex(1)(0);
		Assert (r1 >= 0, ExcRadialVariableHasNegativeValues(r1));	
		Assert (r2 >= 0, ExcRadialVariableHasNegativeValues(r2));
		
		patch->vertices[0] = r1*angle_directions[angle];
		patch->vertices[1] = r2*angle_directions[angle];
		patch->vertices[2] = r2*angle_directions[angle+1];
		patch->vertices[3] = r1*angle_directions[angle+1];

		break;
	      };
	       
	      case 2:
	      {
		for (unsigned int vertex=0;
		     vertex<GeometryInfo<dim>::vertices_per_cell;
		     ++vertex)
		  {
		    const Point<dim> v = cell->vertex(vertex);
		    
						     // make sure that the
						     // radial variable does
						     // attain negative
						     // values
		    Assert (v(0) >= 0, ExcRadialVariableHasNegativeValues(v(0)));
		    
						     // now set the vertices
						     // of the patch
		    patch->vertices[vertex] = v(0) * angle_directions[angle];
		    patch->vertices[vertex][2] = v(1);
		    
		    patch->vertices[vertex+GeometryInfo<dim>::vertices_per_cell]
		      = v(0) * angle_directions[angle+1];
		    patch->vertices[vertex+GeometryInfo<dim>::vertices_per_cell][2]
		      = v(1);
		  };
		
		break;
	      };

	      default:
		    Assert (false, ExcNotImplemented());
	    };
	  
	       
					   // then fill in data
	  if (data.n_datasets > 0)
	    {
	      fe_patch_values.reinit (cell);
	      
					       // first fill dof_data
	      for (unsigned int dataset=0; dataset<this->dof_data.size(); ++dataset)
		{
		  if (data.n_components == 1)
		    {
                      this->dof_data[dataset]->get_function_values (fe_patch_values,
                                                                    data.patch_values);

		      switch (dim)
			{
			  case 1:
				for (unsigned int x=0; x<n_points; ++x)
				  for (unsigned int y=0; y<n_points; ++y)
				    patch->data(dataset,
						x*n_points + y)
					= data.patch_values[x];
				break;
				
			  case 2:
				for (unsigned int x=0; x<n_points; ++x)
				  for (unsigned int y=0; y<n_points; ++y)
				    for (unsigned int z=0; z<n_points; ++z)
				      patch->data(dataset,
						  x*n_points*n_points +
						  y*n_points +
						  z)
					= data.patch_values[x*n_points+z];
				break;
				
			  default:
				Assert (false, ExcNotImplemented());
			};
		    }
		  else
						     // system of components
		    {
                      this->dof_data[dataset]->get_function_values (fe_patch_values,
                                                                    data.patch_values_system);

		      for (unsigned int component=0; component<data.n_components;
			   ++component)
			{
			  switch (dim)
			    {
			      case 1:
				    for (unsigned int x=0; x<n_points; ++x)
				      for (unsigned int y=0; y<n_points; ++y)
					patch->data(dataset*data.n_components+component,
						    x*n_points + y)
					    = data.patch_values_system[x](component);
				    break;

			      case 2:
				    for (unsigned int x=0; x<n_points; ++x)
				      for (unsigned int y=0; y<n_points; ++y)
					for (unsigned int z=0; z<n_points; ++z)
					  patch->data(dataset*data.n_components+component,
						      x*n_points*n_points +
						      y*n_points +
						      z)
					    = data.patch_values_system[x*n_points+z](component);
				    break;

			      default:
				    Assert (false, ExcNotImplemented());
			    };
			};
		    };
		};
		  
					       // then do the cell data
	      for (unsigned int dataset=0; dataset<this->cell_data.size(); ++dataset)
		{
		  const double value
                    = this->cell_data[dataset]->get_cell_data_value (cell_number);
		  switch (dim)
		    {
		      case 1:
			    for (unsigned int x=0; x<n_points; ++x)
			      for (unsigned int y=0; y<n_points; ++y)
				patch->data(dataset+this->dof_data.size()*data.n_components,
					    x*n_points +
					    y)
				  = value;
			    break;
			    
		      case 2:
			    for (unsigned int x=0; x<n_points; ++x)
			      for (unsigned int y=0; y<n_points; ++y)
				for (unsigned int z=0; z<n_points; ++z)
				  patch->data(dataset+this->dof_data.size()*data.n_components,
					      x*n_points*n_points +
					      y*n_points +
					      z)
				    = value;
			    break;

		      default:
			    Assert (false, ExcNotImplemented());
		    };
		};
	    };
	};
      
				       // next cell (patch) in this
				       // thread. note that we have
				       // already advanced the patches
				       // for the present cell,
				       // i.e. we only have to skip
				       // the cells belonging to other
				       // threads, not the ones
				       // belonging to this thread.
      const int skip_threads = static_cast<signed int>(data.n_threads)-1;
      for (int i=0; (i<skip_threads) && (cell != this->dofs->end()); ++i)
	std::advance (patch, n_patches_per_circle);

				       // however, cell and cell
				       // number have not yet been
				       // increased
      for (unsigned int i=0; (i<data.n_threads) && (cell != this->dofs->end()); ++i)
	{
	  ++cell_number;
	  cell=next_cell(cell);
	};
    };
}



#if deal_II_dimension == 3

template <>
void DataOutRotation<3>::build_some_patches (Data)
{
				   // would this function make any
				   // sense after all? who would want
				   // to output/compute in four space
				   // dimensions?
  Assert (false, ExcNotImplemented());
}

#endif



template <int dim>
void DataOutRotation<dim>::build_patches (const unsigned int n_patches_per_circle,
					  const unsigned int n_subdivisions,
					  const unsigned int n_threads_) 
{
  Assert (n_subdivisions >= 1,
	  ExcInvalidNumberOfSubdivisions(n_subdivisions));

  typedef DataOut_DoFData<dim,dim+1> BaseClass;
  Assert (this->dofs != 0, typename BaseClass::ExcNoDoFHandlerSelected());

  const unsigned int n_threads = (DEAL_II_USE_MT ? n_threads_ : 1);

				   // before we start the loop:
				   // create a quadrature rule that
				   // actually has the points on this
				   // patch
  QTrapez<1>     q_trapez;
  QIterated<dim> patch_points (q_trapez, n_subdivisions);

  const unsigned int n_q_points     = patch_points.n_quadrature_points;
  const unsigned int n_components   = this->dofs->get_fe().n_components();
  const unsigned int n_datasets     = this->dof_data.size() * n_components +
				      this->cell_data.size();
  
				   // clear the patches array
  if (true)
    {
      std::vector< ::DataOutBase::Patch<dim+1> > dummy;
      this->patches.swap (dummy);
    };
  
				   // first count the cells we want to
				   // create patches of and make sure
				   // there is enough memory for that
  unsigned int n_patches = 0;
  for (typename DoFHandler<dim>::cell_iterator cell=first_cell();
       cell != this->dofs->end();
       cell = next_cell(cell))
    ++n_patches;
				   // then also take into account that
				   // we want more than one patch to
				   // come out of every cell, as they
				   // are repeated around the axis of
				   // rotation
  n_patches *= n_patches_per_circle;

  std::vector<Data> thread_data(n_threads);

				   // init data for the threads
  for (unsigned int i=0;i<n_threads;++i)
    {
      thread_data[i].n_threads            = n_threads;
      thread_data[i].this_thread          = i;
      thread_data[i].n_components         = n_components;
      thread_data[i].n_datasets           = n_datasets;
      thread_data[i].n_patches_per_circle = n_patches_per_circle;
      thread_data[i].n_subdivisions       = n_subdivisions;
      thread_data[i].patch_values.resize (n_q_points);
      thread_data[i].patch_values_system.resize (n_q_points);
      
      for (unsigned int k=0; k<n_q_points; ++k)
	thread_data[i].patch_values_system[k].reinit(n_components);
    }

				   // create the patches with default
				   // values. note that the evaluation
				   // points on the cell have to be
				   // repeated in angular direction
  ::DataOutBase::Patch<dim+1>  default_patch;
  default_patch.n_subdivisions = n_subdivisions;
  default_patch.data.reinit (n_datasets,
			     n_q_points * (n_subdivisions+1));
  this->patches.insert (this->patches.end(), n_patches, default_patch);

  if (DEAL_II_USE_MT)
    {
      void (DataOutRotation<dim>::*p) (Data)
        = &DataOutRotation<dim>::build_some_patches;

      Threads::ThreadGroup<> threads;
      for (unsigned int l=0;l<n_threads;++l)
        threads += Threads::spawn (*this, p) (thread_data[l]);
      threads.join_all();
    }
  else
                                     // just one thread
    build_some_patches(thread_data[0]);
}



template <int dim>
typename DoFHandler<dim>::cell_iterator
DataOutRotation<dim>::first_cell () 
{
  return this->dofs->begin_active ();
}


template <int dim>
typename DoFHandler<dim>::cell_iterator
DataOutRotation<dim>::next_cell (const typename DoFHandler<dim>::cell_iterator &cell) 
{
				   // convert the iterator to an
				   // active_iterator and advance
				   // this to the next active cell
  typename DoFHandler<dim>::active_cell_iterator active_cell = cell;
  ++active_cell;
  return active_cell;
}


// explicit instantiations
template class DataOutRotation<deal_II_dimension>;
