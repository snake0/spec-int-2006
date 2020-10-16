//----------------------------  data_out_base.cc  ---------------------------
//    $Id: data_out_base.cc,v 1.9 2006/01/24 01:06:25 wolf Exp $
//    Version: $Name:  $
//
//    Copyright (C) 1998, 1999, 2000, 2001, 2002, 2003, 2004 by the deal.II authors
//
//    This file is subject to QPL and may not be  distributed
//    without copyright and license information. Please refer
//    to the file deal.II/doc/license.html for the  text  and
//    further information on this license.
//
//----------------------------  data_out_base.cc  ---------------------------


#include <base/data_out_base.h>
#include <base/parameter_handler.h>
#include <base/thread_management.h>
#include <base/memory_consumption.h>

#include <algorithm>
#include <iomanip>
#include <cmath>
#include <set>


template <int dim, int spacedim>
const unsigned int DataOutBase::Patch<dim,spacedim>::no_neighbor;


template <int dim, int spacedim>
DataOutBase::Patch<dim,spacedim>::Patch ()
                :
		patch_index(no_neighbor),
		n_subdivisions (1)
				   // all the other data has a
				   // constructor of its own, except
				   // for the "neighbors" field, which
				   // we set to invalid values.
{
  for (unsigned int i=0;i<GeometryInfo<dim>::faces_per_cell;++i)
    neighbors[i] = no_neighbor;
  
  Assert (dim<=spacedim, ExcInvalidCombinationOfDimensions(dim,spacedim));
  Assert (spacedim<=3, ExcNotImplemented());
}



template <int dim, int spacedim>
unsigned int
DataOutBase::Patch<dim,spacedim>::memory_consumption () const
{
  return (sizeof(vertices) / sizeof(vertices[0]) * 
	  MemoryConsumption::memory_consumption(vertices[0])
	  +
	  MemoryConsumption::memory_consumption(n_subdivisions)
	  +
	  MemoryConsumption::memory_consumption(data));
}



DataOutBase::UcdFlags::UcdFlags (const bool write_preamble)
                :
		write_preamble (write_preamble)
{}



DataOutBase::PovrayFlags::PovrayFlags (const bool smooth,
				       const bool bicubic_patch,
				       const bool external_data)
                :
		smooth (smooth),
		bicubic_patch(bicubic_patch),
		external_data(external_data)
{}


DataOutBase::DXFlags::DXFlags (const bool write_multigrid,
			       const bool write_neighbors)
                :
		write_multigrid(write_multigrid),
		write_neighbors(write_neighbors)
{}


void DataOutBase::DXFlags::declare_parameters (ParameterHandler &prm)
{
  prm.declare_entry ("Write multigrid", "true", Patterns::Bool());
  prm.declare_entry ("Write neighbors", "true", Patterns::Bool());
}



void DataOutBase::DXFlags::parse_parameters (ParameterHandler &prm)
{
  write_multigrid = prm.get_bool ("Write multigrid");
  write_neighbors = prm.get_bool ("Write neighbors");
}



unsigned int
DataOutBase::DXFlags::memory_consumption () const
{
				   // only simple data elements, so
				   // use sizeof operator
  return sizeof (*this);
}




void DataOutBase::UcdFlags::declare_parameters (ParameterHandler &prm)
{
  prm.declare_entry ("Write preamble", "true", Patterns::Bool());
}



void DataOutBase::UcdFlags::parse_parameters (ParameterHandler &prm)
{
  write_preamble = prm.get_bool ("Write preamble");
}


unsigned int
DataOutBase::UcdFlags::memory_consumption () const
{
				   // only simple data elements, so
				   // use sizeof operator
  return sizeof (*this);
}



void DataOutBase::GnuplotFlags::declare_parameters (ParameterHandler &/*prm*/)
{}



void DataOutBase::GnuplotFlags::parse_parameters (ParameterHandler &/*prm*/)
{}



unsigned int
DataOutBase::GnuplotFlags::memory_consumption () const
{
				   // only simple data elements, so
				   // use sizeof operator
  return sizeof (*this);
}




void DataOutBase::PovrayFlags::declare_parameters (ParameterHandler &prm)
{
  prm.declare_entry ("Use smooth triangles", "false",
		     Patterns::Bool());
  prm.declare_entry ("Use bicubic patches", "false",
		     Patterns::Bool());
  prm.declare_entry ("Include external file", "true",
		     Patterns::Bool ());
}



void DataOutBase::PovrayFlags::parse_parameters (ParameterHandler &prm)
{
  smooth        = prm.get_bool ("Use smooth triangles");
  bicubic_patch = prm.get_bool ("Use bicubic patches");
  external_data = prm.get_bool ("Include external file");
}



unsigned int
DataOutBase::PovrayFlags::memory_consumption () const
{
				   // only simple data elements, so
				   // use sizeof operator
  return sizeof (*this);
}



DataOutBase::EpsFlags::EpsFlags (const unsigned int  height_vector,
				 const unsigned int  color_vector,
				 const SizeType      size_type,
				 const unsigned int  size,
				 const double        line_width,
				 const double        azimut_angle,
				 const double        turn_angle,
				 const double        z_scaling,
				 const bool          draw_mesh,
				 const bool          draw_cells,
				 const bool          shade_cells,
				 const ColorFunction color_function)
                :
		height_vector(height_vector),
		color_vector(color_vector),
		size_type(size_type),
		size(size),
		line_width(line_width),
		azimut_angle(azimut_angle),
		turn_angle(turn_angle),
		z_scaling(z_scaling),
		draw_mesh(draw_mesh),
		draw_cells(draw_cells),
		shade_cells(shade_cells),
		color_function(color_function)
{}



DataOutBase::EpsFlags::RgbValues
DataOutBase::EpsFlags::default_color_function (const double x,
					       const double xmin,
					       const double xmax)
{
  RgbValues rgb_values;
  
// A difficult color scale:
//     xmin          = black  (1)
// 3/4*xmin+1/4*xmax = blue   (2)
// 1/2*xmin+1/2*xmax = green  (3)
// 1/4*xmin+3/4*xmax = red    (4)
//              xmax = white  (5)
// Makes the following color functions:
//
// red      green    blue
//       __
//      /      /\  /  /\    /
// ____/    __/  \/  /  \__/

//     { 0                                (1) - (3)
// r = { ( 4*x-2*xmin+2*xmax)/(xmax-xmin) (3) - (4)
//     { 1                                (4) - (5)
//
//     { 0                                (1) - (2)
// g = { ( 4*x-3*xmin-  xmax)/(xmax-xmin) (2) - (3)
//     { (-4*x+  xmin+3*xmax)/(xmax-xmin) (3) - (4)
//     { ( 4*x-  xmin-3*xmax)/(xmax-xmin) (4) - (5)
//
//     { ( 4*x-4*xmin       )/(xmax-xmin) (1) - (2)
// b = { (-4*x+2*xmin+2*xmax)/(xmax-xmin) (2) - (3)
//     { 0                                (3) - (4)
//     { ( 4*x-  xmin-3*xmax)/(xmax-xmin) (4) - (5)

  double sum   =   xmax+  xmin;
  double sum13 =   xmin+3*xmax;
  double sum22 = 2*xmin+2*xmax;
  double sum31 = 3*xmin+  xmax;
  double dif = xmax-xmin;
  double rezdif = 1.0/dif;

  int where;

  if (x<(sum31)/4)
    where = 0;
  else if (x<(sum22)/4)
    where = 1;
  else if (x<(sum13)/4)
    where = 2;
  else
    where = 3;

  if (dif!=0)
    {
      switch (where)
	{
	  case 0:
		rgb_values.red   = 0;
		rgb_values.green = 0;
		rgb_values.blue  = (x-xmin)*4.*rezdif;
		break;
	  case 1:
		rgb_values.red   = 0;
		rgb_values.green = (4*x-3*xmin-xmax)*rezdif;
		rgb_values.blue  = (sum22-4.*x)*rezdif;
		break;
	  case 2:
		rgb_values.red   = (4*x-2*sum)*rezdif;
		rgb_values.green = (xmin+3*xmax-4*x)*rezdif;
		rgb_values.blue  = 0;
		break;
	  case 3:
		rgb_values.red   = 1;
		rgb_values.green = (4*x-xmin-3*xmax)*rezdif;
		rgb_values.blue  = (4.*x-sum13)*rezdif;
	  default:
		break;
	};
    }
  else // White 
    rgb_values.red = rgb_values.green = rgb_values.blue = 1;

  return rgb_values;
}



DataOutBase::EpsFlags::RgbValues
DataOutBase::EpsFlags::grey_scale_color_function (const double x,
						  const double xmin,
						  const double xmax)
{
  DataOutBase::EpsFlags::RgbValues rgb_values;
  rgb_values.red = rgb_values.blue = rgb_values.green
		 = (x-xmin)/(xmax-xmin);
  return rgb_values;
}



DataOutBase::EpsFlags::RgbValues
DataOutBase::EpsFlags::reverse_grey_scale_color_function (const double x,
							  const double xmin,
							  const double xmax)
{
  DataOutBase::EpsFlags::RgbValues rgb_values;
  rgb_values.red = rgb_values.blue = rgb_values.green
		 = 1-(x-xmin)/(xmax-xmin);
  return rgb_values;
}



bool DataOutBase::EpsCell2d::operator < (const EpsCell2d &e) const
{
				   // note the "wrong" order in
				   // which we sort the elements
  return depth > e.depth;
}



void DataOutBase::EpsFlags::declare_parameters (ParameterHandler &prm)
{
  prm.declare_entry ("Index of vector for height", "0",
		     Patterns::Integer());
  prm.declare_entry ("Index of vector for color", "0",
		     Patterns::Integer());
  prm.declare_entry ("Scale to width or height", "width",
		     Patterns::Selection ("width|height"));
  prm.declare_entry ("Size (width or height) in eps units", "300",
		     Patterns::Integer());
  prm.declare_entry ("Line widths in eps units", "0.5",
		     Patterns::Double());
  prm.declare_entry ("Azimut angle", "60",
		     Patterns::Double(0,180));
  prm.declare_entry ("Turn angle", "30",
		     Patterns::Double(0,360));
  prm.declare_entry ("Scaling for z-axis", "1",
		     Patterns::Double ());
  prm.declare_entry ("Draw mesh lines", "true",
		     Patterns::Bool());
  prm.declare_entry ("Fill interior of cells", "true",
		     Patterns::Bool());
  prm.declare_entry ("Color shading of interior of cells", "true",
		     Patterns::Bool());
  prm.declare_entry ("Color function", "default",
		     Patterns::Selection ("default|grey scale|reverse grey scale"));
}



void DataOutBase::EpsFlags::parse_parameters (ParameterHandler &prm)
{
  height_vector = prm.get_integer ("Index of vector for height");
  color_vector  = prm.get_integer ("Index of vector for color");
  if (prm.get ("Scale to width or height") == "width")
    size_type   = width;
  else
    size_type   = height;
  size          = prm.get_integer ("Size (width or height) in eps units");
  line_width    = prm.get_double ("Line widths in eps units");
  azimut_angle  = prm.get_double ("Azimut angle");
  turn_angle    = prm.get_double ("Turn angle");
  z_scaling     = prm.get_double ("Scaling for z-axis");
  draw_mesh     = prm.get_bool ("Draw mesh lines");
  draw_cells    = prm.get_bool ("Fill interior of cells");
  shade_cells   = prm.get_bool ("Color shading of interior of cells");
  if (prm.get("Color function") == "default")
    color_function = &default_color_function;
  else if (prm.get("Color function") == "grey scale")
    color_function = &grey_scale_color_function;
  else if (prm.get("Color function") == "reverse grey scale")
    color_function = &reverse_grey_scale_color_function;
  else
				     // we shouldn't get here, since
				     // the parameter object should
				     // already have checked that the
				     // given value is valid
    Assert (false, ExcInternalError());
}



unsigned int
DataOutBase::EpsFlags::memory_consumption () const
{
				   // only simple data elements, so
				   // use sizeof operator
  return sizeof (*this);
}



void DataOutBase::GmvFlags::declare_parameters (ParameterHandler &/*prm*/)
{}



void DataOutBase::GmvFlags::parse_parameters (ParameterHandler &/*prm*/)
{}


unsigned int
DataOutBase::GmvFlags::memory_consumption () const
{
				   // only simple data elements, so
				   // use sizeof operator
  return sizeof (*this);
}



DataOutBase::TecplotFlags::
TecplotFlags (const char* tecplot_binary_file_name)
                :
                tecplot_binary_file_name(tecplot_binary_file_name)
{}



void DataOutBase::TecplotFlags::declare_parameters (ParameterHandler &/*prm*/)
{}



void DataOutBase::TecplotFlags::parse_parameters (ParameterHandler &/*prm*/)
{}


unsigned int
DataOutBase::TecplotFlags::memory_consumption () const
{
				   // only simple data elements, so
				   // use sizeof operator
  return sizeof (*this);
}



void DataOutBase::VtkFlags::declare_parameters (ParameterHandler &/*prm*/)
{}



void DataOutBase::VtkFlags::parse_parameters (ParameterHandler &/*prm*/)
{}


unsigned int
DataOutBase::VtkFlags::memory_consumption () const
{
				   // only simple data elements, so
				   // use sizeof operator
  return sizeof (*this);
}



unsigned int DataOutBase::memory_consumption ()
{
  return 0;
}



template <int dim, int spacedim>
void DataOutBase::write_ucd (const std::vector<Patch<dim,spacedim> > &patches,
			     const std::vector<std::string>          &data_names,
			     const UcdFlags                          &flags,
			     std::ostream                            &out) 
{
  AssertThrow (out, ExcIO());

  Assert (patches.size() > 0, ExcNoPatches());
  
  const unsigned int n_data_sets = data_names.size();

				   // first count the number of cells
				   // and cells for later use
  unsigned int n_cells = 0,
	       n_nodes = 0;
  for (typename std::vector<Patch<dim,spacedim> >::const_iterator patch=patches.begin();
       patch!=patches.end(); ++patch)
    switch (dim)
      {
	case 1:
	      n_cells += patch->n_subdivisions;
	      n_nodes += patch->n_subdivisions+1;
	      break;
	case 2:
	      n_cells += patch->n_subdivisions *
			 patch->n_subdivisions;
	      n_nodes += (patch->n_subdivisions+1) *
			 (patch->n_subdivisions+1);
	      break;
	case 3:
	      n_cells += patch->n_subdivisions *
			 patch->n_subdivisions *
			 patch->n_subdivisions;
	      n_nodes += (patch->n_subdivisions+1) *
			 (patch->n_subdivisions+1) *
			 (patch->n_subdivisions+1);
	      break;
	default:
	      Assert (false, ExcNotImplemented());
      };

				   ///////////////////////
				   // preamble
				   // start with ucd data
  out << n_nodes << ' '
      << n_cells << ' '
      << n_data_sets << ' '
      << 0 << ' '                  // no cell data at present
      << 0                         // no model data
      << '\n';

				   ///////////////////////////////
				   // first make up the list of used
				   // nodes along with their
				   // coordinates. number them
				   // consecutively starting with 1
				   //
				   // note that we have to print
				   // d=1..3 dimensions
  if (true)
    {
      unsigned int present_node = 1;
      
      for (typename std::vector<Patch<dim,spacedim> >::const_iterator patch=patches.begin();
	   patch!=patches.end(); ++patch)
	{
	  const unsigned int n_subdivisions = patch->n_subdivisions;
	  
					   // if we have nonzero values for
					   // this coordinate
	  switch (dim)
	    {
	      case 1:
	      {
		for (unsigned int i=0; i<n_subdivisions+1; ++i, ++present_node)
		  {
		    out << present_node
			<< "   ";

		    const Point<spacedim>
		      node = ((patch->vertices[1] * i / n_subdivisions) +
			      (patch->vertices[0] * (n_subdivisions-i) / n_subdivisions));

						     // write out coordinates
		    for (unsigned int i=0; i<spacedim; ++i)
		      out << node(i) << ' ';
						     // fill with zeroes
		    for (unsigned int i=spacedim; i<3; ++i)
		      out << "0 ";
		    out << '\n';
		  };
		
		break;
	      };
	       
	      case 2:
	      {
		for (unsigned int i=0; i<n_subdivisions+1; ++i)
		  for (unsigned int j=0; j<n_subdivisions+1; ++j)
		    {
		      const double x_frac = i * 1./n_subdivisions,
				   y_frac = j * 1./n_subdivisions;
		      
		      out << present_node
			  << "   ";
		      
						       // compute coordinates for
						       // this patch point
		      const Point<spacedim>
			node = (((patch->vertices[1] * x_frac) +
				 (patch->vertices[0] * (1-x_frac))) * (1-y_frac) +
				((patch->vertices[2] * x_frac) +
				 (patch->vertices[3] * (1-x_frac))) * y_frac);
		      
						       // write out coordinates
		      for (unsigned int i=0; i<spacedim; ++i)
			out << node(i) << ' ';
						       // fill with zeroes
		      for (unsigned int i=spacedim; i<3; ++i)
			out << "0 ";
		      out << '\n';

		      ++present_node;
		    };
	      
		break;
	      };
	       
	      case 3:
	      {
		for (unsigned int i=0; i<n_subdivisions+1; ++i)
		  for (unsigned int j=0; j<n_subdivisions+1; ++j)
		    for (unsigned int k=0; k<n_subdivisions+1; ++k)
		      {
							 // note the broken
							 // design of hexahedra
							 // in deal.II, where
							 // first the z-component
							 // is counted up, before
							 // increasing the y-
							 // coordinate
			const double x_frac = i * 1./n_subdivisions,
				     y_frac = k * 1./n_subdivisions,
				     z_frac = j * 1./n_subdivisions;
			
							 // compute coordinates for
							 // this patch point
			out << present_node
			    << "   ";
			
							 // compute coordinates for
							 // this patch point
			const Point<spacedim>
			  node = ((((patch->vertices[1] * x_frac) +
				    (patch->vertices[0] * (1-x_frac))) * (1-y_frac) +
				   ((patch->vertices[2] * x_frac) +
				    (patch->vertices[3] * (1-x_frac))) * y_frac)   * (1-z_frac) +
				  (((patch->vertices[5] * x_frac) +
				    (patch->vertices[4] * (1-x_frac))) * (1-y_frac) +
				   ((patch->vertices[6] * x_frac) +
				    (patch->vertices[7] * (1-x_frac))) * y_frac)   * z_frac);
			
							 // write out coordinates
			for (unsigned int i=0; i<spacedim; ++i)
			  out << node(i) << ' ';
							 // fill with zeroes unnecessary here
			for (unsigned int i=spacedim; i<3; ++i)
			  out << "0 ";
			out << '\n';
			
			++present_node;
		      };
	      
		break;
	      };
	       
	      default:
		    Assert (false, ExcNotImplemented());
	    };
	};

				       // note that we number starting with 1!
      Assert (present_node == n_nodes+1,
	      ExcInternalError());
    };

				   /////////////////////////////////////////
				   // write cell. number them consecutively,
				   // starting with 1
  if (true)
    {
      unsigned int present_cell = 1;      
      unsigned int first_vertex_of_patch = 0;
      
      for (typename std::vector<Patch<dim,spacedim> >::const_iterator patch=patches.begin();
	   patch!=patches.end(); ++patch)
	{
	  const unsigned int n_subdivisions = patch->n_subdivisions;
	  
					   // write out the cells making
					   // up this patch
	  switch (dim)
	    {
	      case 1:
	      {
		for (unsigned int i=0; i<n_subdivisions; ++i, ++present_cell)
		  out << present_cell
		      << " 0  line  "        // set material id to 0
		      << first_vertex_of_patch+i+1 << ' '
		      << first_vertex_of_patch+i+1+1 << '\n';
		break;
	      };
	       
	      case 2:
	      {
		for (unsigned int i=0; i<n_subdivisions; ++i)
		  for (unsigned int j=0; j<n_subdivisions; ++j)
		    {
		      out << present_cell
			  << " 0  quad  "    // set material id to 0
			  << first_vertex_of_patch+i*(n_subdivisions+1)+j+1 << ' '
			  << first_vertex_of_patch+(i+1)*(n_subdivisions+1)+j+1 << ' '
			  << first_vertex_of_patch+(i+1)*(n_subdivisions+1)+j+1+1 << ' '
			  << first_vertex_of_patch+i*(n_subdivisions+1)+j+1+1
			  << '\n';
		      ++present_cell;
		    };
		break;
	      };
	       
	      case 3:
	      {
		for (unsigned int i=0; i<n_subdivisions; ++i)
		  for (unsigned int j=0; j<n_subdivisions; ++j)
		    for (unsigned int k=0; k<n_subdivisions; ++k)
		      {
			out << present_cell
			    << " 0  hex  "    // set material id to 0
							   // note: vertex indices start with 1!
			    << first_vertex_of_patch+(i*(n_subdivisions+1)+j      )*(n_subdivisions+1)+k  +1 << ' '
			    << first_vertex_of_patch+((i+1)*(n_subdivisions+1)+j  )*(n_subdivisions+1)+k  +1 << ' '
			    << first_vertex_of_patch+((i+1)*(n_subdivisions+1)+j+1)*(n_subdivisions+1)+k  +1 << ' '
			    << first_vertex_of_patch+(i*(n_subdivisions+1)+j+1    )*(n_subdivisions+1)+k  +1 << ' '
			    << first_vertex_of_patch+(i*(n_subdivisions+1)+j      )*(n_subdivisions+1)+k+1+1 << ' '
			    << first_vertex_of_patch+((i+1)*(n_subdivisions+1)+j  )*(n_subdivisions+1)+k+1+1 << ' '
			    << first_vertex_of_patch+((i+1)*(n_subdivisions+1)+j+1)*(n_subdivisions+1)+k+1+1 << ' '
			    << first_vertex_of_patch+(i*(n_subdivisions+1)+j+1    )*(n_subdivisions+1)+k+1+1 << ' '
			    << '\n';
			++present_cell;
		      };
		break;
	      };

	      default:
		    Assert (false, ExcNotImplemented());
	    };


					   // finally update the number
					   // of the first vertex of this patch
	  switch (dim)
	    {
	      case 1:
		    first_vertex_of_patch += n_subdivisions+1;
		    break;
	      case 2:
		    first_vertex_of_patch += (n_subdivisions+1) *
					     (n_subdivisions+1);
		    break;
	      case 3:
		    first_vertex_of_patch += (n_subdivisions+1) *
					     (n_subdivisions+1) *
					     (n_subdivisions+1);
		    break;
	      default:
		    Assert (false, ExcNotImplemented());
	    };
	};
      out << '\n';

				       // note that we number starting with 1!
      Assert (present_cell == n_cells+1,
	      ExcInternalError());
    };


				   /////////////////////////////
				   // now write data
  if (n_data_sets != 0)
    {      
      out << n_data_sets << "    ";    // number of vectors
      for (unsigned int i=0; i<n_data_sets; ++i)
	out << 1 << ' ';               // number of components;
				       // only 1 supported presently
      out << '\n';

      for (unsigned int data_set=0; data_set<n_data_sets; ++data_set)
	out << data_names[data_set]
	    << ",dimensionless"      // no units supported at present
	    << '\n';


				       // loop over all patches
      unsigned int present_node = 1;
      for (typename std::vector<Patch<dim,spacedim> >::const_iterator patch=patches.begin();
	   patch != patches.end(); ++patch)
	{
	  const unsigned int n_subdivisions = patch->n_subdivisions;
      
	  Assert (patch->data.n_rows() == n_data_sets,
		  ExcUnexpectedNumberOfDatasets (patch->data.n_rows(), n_data_sets));
	  Assert (patch->data.n_cols() == (dim==1 ?
				      n_subdivisions+1 :
				      (dim==2 ?
				       (n_subdivisions+1)*(n_subdivisions+1) :
				       (dim==3 ?
					(n_subdivisions+1)*(n_subdivisions+1)*(n_subdivisions+1) :
					0))),
		  ExcInvalidDatasetSize (patch->data.n_cols(), n_subdivisions+1));

	  switch (dim)
	    {
	      case 1:
	      {      
		for (unsigned int i=0; i<n_subdivisions+1; ++i, ++present_node) 
		  {
		    out << present_node
			<< "  ";
		    for (unsigned int data_set=0; data_set<n_data_sets; ++data_set)
		      out << patch->data(data_set,i) << ' ';

		    out << '\n';
		  };
	    
		break;
	      };
	   
	      case 2:
	      {
		for (unsigned int i=0; i<n_subdivisions+1; ++i)
		  for (unsigned int j=0; j<n_subdivisions+1; ++j)
		    {
		      out << present_node
			  << "  ";
		      for (unsigned int data_set=0; data_set<n_data_sets; ++data_set)
			out << patch->data(data_set,i*(n_subdivisions+1) + j) << ' ';

		      out << '\n';

		      ++present_node;
		    };

		break;
	      };

	      case 3:
	      {
		for (unsigned int i=0; i<n_subdivisions+1; ++i)
		  for (unsigned int j=0; j<n_subdivisions+1; ++j)
		    for (unsigned int k=0; k<n_subdivisions+1; ++k)
		      {
			out << present_node
			    << "  ";
			for (unsigned int data_set=0; data_set<n_data_sets; ++data_set)
			  out << patch->data(data_set,
					     (i*(n_subdivisions+1)+j)*(n_subdivisions+1)+k)
			      << ' ';
			
			out << '\n';
			
			++present_node;
		      };

		break;
	      };

	      default:
		    Assert (false, ExcNotImplemented());
	    };
	};  
    };

				   // no model data

				   // assert the stream is still ok
  AssertThrow (out, ExcIO());
}



template <int dim, int spacedim>
void DataOutBase::write_dx (const std::vector<Patch<dim,spacedim> > &patches,
			    const std::vector<std::string>          &data_names,
			    const DXFlags                           &flags,
			    std::ostream                            &out) 
{
  AssertThrow (out, ExcIO());

  Assert (patches.size() > 0, ExcNoPatches());
  
  const unsigned int n_data_sets = data_names.size();

				   // first count the number of cells
				   // and cells for later use
  unsigned int n_cells = 0,
	       n_nodes = 0;
  for (typename std::vector<Patch<dim,spacedim> >::const_iterator patch=patches.begin();
       patch!=patches.end(); ++patch)
    switch (dim)
      {
	case 1:
	      n_cells += patch->n_subdivisions;
	      n_nodes += patch->n_subdivisions+1;
	      break;
	case 2:
	      n_cells += patch->n_subdivisions *
			 patch->n_subdivisions;
	      n_nodes += (patch->n_subdivisions+1) *
			 (patch->n_subdivisions+1);
	      break;
	case 3:
	      n_cells += patch->n_subdivisions *
			 patch->n_subdivisions *
			 patch->n_subdivisions;
	      n_nodes += (patch->n_subdivisions+1) *
			 (patch->n_subdivisions+1) *
			 (patch->n_subdivisions+1);
	      break;
	default:
	      Assert (false, ExcNotImplemented());
      };

				   // start with vertices order is
				   // lexicographical, x varying
				   // fastest
  out << "object \"vertices\" class array type float rank 1 shape " << spacedim << " items " << n_nodes << " data follows"
      << '\n';

				   ///////////////////////////////
				   // first write the coordinates of all vertices
  if (true)
    {
      for (typename std::vector<Patch<dim,spacedim> >::const_iterator patch=patches.begin();
	   patch!=patches.end(); ++patch)
	{
	  const unsigned int n_subdivisions = patch->n_subdivisions;
	  
					   // if we have nonzero values for
					   // this coordinate
	  switch (dim)
	    {
	      case 1:
	      {
		for (unsigned int i=0; i<n_subdivisions+1; ++i)
		  {
		    const Point<spacedim>
		      node = ((patch->vertices[1] * i / n_subdivisions) +
			      (patch->vertices[0] * (n_subdivisions-i) / n_subdivisions));

						     // write out coordinates
		    for (unsigned int i=0; i<spacedim; ++i)
		      out << node(i) << '\t';
		    out << '\n';
		  };
		
		break;
	      };
	       
	      case 2:
	      {
		for (unsigned int i=0; i<n_subdivisions+1; ++i)
		  for (unsigned int j=0; j<n_subdivisions+1; ++j)
		    {
		      const double x_frac = i * 1./n_subdivisions,
				   y_frac = j * 1./n_subdivisions;
		      
						       // compute coordinates for
						       // this patch point
		      const Point<spacedim>
			node = (((patch->vertices[1] * x_frac) +
				 (patch->vertices[0] * (1-x_frac))) * (1-y_frac) +
				((patch->vertices[2] * x_frac) +
				 (patch->vertices[3] * (1-x_frac))) * y_frac);
		      
						       // write out coordinates
		      for (unsigned int i=0; i<spacedim; ++i)
			out << node(i) << '\t';
		      out << '\n';
		    };
	      
		break;
	      };
	       
	      case 3:
	      {
		for (unsigned int i=0; i<n_subdivisions+1; ++i)
		  for (unsigned int j=0; j<n_subdivisions+1; ++j)
		    for (unsigned int k=0; k<n_subdivisions+1; ++k)
		      {
							 // note the broken
							 // design of hexahedra
							 // in deal.II, where
							 // first the z-component
							 // is counted up, before
							 // increasing the y-
							 // coordinate
			const double x_frac = i * 1./n_subdivisions,
				     y_frac = k * 1./n_subdivisions,
				     z_frac = j * 1./n_subdivisions;
			
							 // compute coordinates for
							 // this patch point
			const Point<spacedim>
			  node = ((((patch->vertices[1] * x_frac) +
				    (patch->vertices[0] * (1-x_frac))) * (1-y_frac) +
				   ((patch->vertices[2] * x_frac) +
				    (patch->vertices[3] * (1-x_frac))) * y_frac)   * (1-z_frac) +
				  (((patch->vertices[5] * x_frac) +
				    (patch->vertices[4] * (1-x_frac))) * (1-y_frac) +
				   ((patch->vertices[6] * x_frac) +
				    (patch->vertices[7] * (1-x_frac))) * y_frac)   * z_frac);
			
							 // write out coordinates
			for (unsigned int i=0; i<spacedim; ++i)
			  out << node(i) << '\t';
			out << '\n';
		      };
	      
		break;
	      };
	       
	      default:
		    Assert (false, ExcNotImplemented());
	    };
	};
    };

				   /////////////////////////////////////////
				   // write cells
  out << "object \"cells\" class array type int rank 1 shape " << GeometryInfo<dim>::vertices_per_cell
      << " items " << n_cells << " data follows"
      << '\n';
  
  if (true)
    {
      unsigned int first_vertex_of_patch = 0;
      
      for (typename std::vector<Patch<dim,spacedim> >::const_iterator patch=patches.begin();
	   patch!=patches.end(); ++patch)
	{
	  const unsigned int n_subdivisions = patch->n_subdivisions;
	  
					   // write out the cells making
					   // up this patch
	  switch (dim)
	    {
	      case 1:
		if (true)
		  {
		    for (unsigned int i=0; i<n_subdivisions; ++i)
		      out << first_vertex_of_patch+i << '\t'
			  << first_vertex_of_patch+i+1 << '\n';
		  };
		break;
	      case 2:
		if (true)
		  {
		    for (unsigned int i=0; i<n_subdivisions; ++i)
		      for (unsigned int j=0; j<n_subdivisions; ++j)
			{
			  out << first_vertex_of_patch+i*(n_subdivisions+1)+j << '\t'
			      << first_vertex_of_patch+i*(n_subdivisions+1)+j+1 << '\t'
			      << first_vertex_of_patch+(i+1)*(n_subdivisions+1)+j << '\t'
			      << first_vertex_of_patch+(i+1)*(n_subdivisions+1)+j+1
			      << '\n';
			};
		  };
		break;
	      case 3:
		if (true)
		  {
		    const unsigned int nvt = n_subdivisions+1;
		    for (unsigned int i=0; i<n_subdivisions; ++i)
		      for (unsigned int j=0; j<n_subdivisions; ++j)
			for (unsigned int k=0; k<n_subdivisions; ++k)
			  {
//TODO:[GK] Put in correct order
			    out
			      << first_vertex_of_patch+((i  )*nvt+j  )*nvt+k   << '\t'
			      << first_vertex_of_patch+((i  )*nvt+j  )*nvt+k+1 << '\t'
			      << first_vertex_of_patch+((i  )*nvt+j+1)*nvt+k   << '\t'
			      << first_vertex_of_patch+((i  )*nvt+j+1)*nvt+k+1 << '\t'
			      << first_vertex_of_patch+((i+1)*nvt+j  )*nvt+k   << '\t'
			      << first_vertex_of_patch+((i+1)*nvt+j  )*nvt+k+1 << '\t'
			      << first_vertex_of_patch+((i+1)*nvt+j+1)*nvt+k   << '\t'
			      << first_vertex_of_patch+((i+1)*nvt+j+1)*nvt+k+1 << '\t'
			      ;
			
//  			  << first_vertex_of_patch+((i  )*(n_subdivisions+1)+j  )*(n_subdivisions+1)+k   << '\t'
//  			  << first_vertex_of_patch+((i+1)*(n_subdivisions+1)+j  )*(n_subdivisions+1)+k   << '\t'
//  			  << first_vertex_of_patch+((i  )*(n_subdivisions+1)+j+1)*(n_subdivisions+1)+k   << '\t'
//  			  << first_vertex_of_patch+((i+1)*(n_subdivisions+1)+j+1)*(n_subdivisions+1)+k   << '\t'
//  			  << first_vertex_of_patch+((i  )*(n_subdivisions+1)+j  )*(n_subdivisions+1)+k+1 << '\t'
//  			  << first_vertex_of_patch+((i+1)*(n_subdivisions+1)+j  )*(n_subdivisions+1)+k+1 << '\t'
//   			  << first_vertex_of_patch+((i  )*(n_subdivisions+1)+j+1)*(n_subdivisions+1)+k+1 << '\t'
//  			  << first_vertex_of_patch+((i+1)*(n_subdivisions+1)+j+1)*(n_subdivisions+1)+k+1 << '\t'
			    out << '\n';
			  }
		  }
		break;
	      default:
		    Assert (false, ExcNotImplemented());
	    };


					   // finally update the number
					   // of the first vertex of this patch
	  switch (dim)
	    {
	      case 1:
		    first_vertex_of_patch += n_subdivisions+1;
		    break;
	      case 2:
		    first_vertex_of_patch += (n_subdivisions+1) *
					     (n_subdivisions+1);
		    break;
	      case 3:
		    first_vertex_of_patch += (n_subdivisions+1) *
					     (n_subdivisions+1) *
					     (n_subdivisions+1);
		    break;
	      default:
		    Assert (false, ExcNotImplemented());
	    };
	};
      out << '\n';
    };

  out << "attribute \"element type\" string \"";
  if (dim==1) out << "lines";
  if (dim==2) out << "quads";
  if (dim==3) out << "cubes";
  out << "\"" << '\n'
      << "attribute \"ref\" string \"positions\"" << '\n';

//TODO:[GK] Patches must be of same size!
				   /////////////////////////////
				   // write neighbor information
  if (flags.write_neighbors)
    {
      out << "object \"neighbors\" class array type int rank 1 shape "
	  << GeometryInfo<dim>::faces_per_cell
	  << " items " << n_cells
	  << " data follows";

      for (typename std::vector<Patch<dim,spacedim> >::const_iterator
	     patch=patches.begin();
	   patch!=patches.end(); ++patch)
	{
	  const unsigned int n = patch->n_subdivisions;
	  unsigned int cells_per_patch = 1;
	  unsigned int dx = 1;
	  unsigned int dy = 1;
	  unsigned int dz = 1;
	  switch(dim)
	    {
	      case 3:
		dx *= n;
		dy *= n;
		cells_per_patch *= n;
	      case 2:
		dx *= n;
		cells_per_patch *= n;
	      case 1:
		cells_per_patch *= n;
	    }

	  const unsigned int patch_start = patch->patch_index * cells_per_patch;

	  for (unsigned int ix=0;ix<n;++ix)
	    for (unsigned int iy=0;iy<((dim>1) ? n : 1);++iy)
	      for (unsigned int iz=0;iz<((dim>2) ? n : 1);++iz)
		{
		  const unsigned int nx = ix*dx;
		  const unsigned int ny = iy*dy;
		  const unsigned int nz = iz*dz;

		  out << '\n';
						   // Direction -x
						   // Last cell in row
						   // of other patch
		  if (ix==0)
		    {
		      const unsigned int nn = patch->neighbors[0];
		      out << '\t';
		      if (nn != patch->no_neighbor)
			out << (nn*cells_per_patch+ny+nz+dx*(n-1));
		      else
			out << "-1";
		    } else {      
		      out << '\t'
			  << patch_start+nx-dx+ny+nz;
		    }
						       // Direction +x
						       // First cell in row
						       // of other patch
		  if (ix == n-1)
		    {
		      const unsigned int nn = patch->neighbors[1];
		      out << '\t';
		      if (nn != patch->no_neighbor)
			out << (nn*cells_per_patch+ny+nz);
		      else
			out << "-1";
		    } else {
		      out << '\t'
			  << patch_start+nx+dx+ny+nz;
		    }
		  if (dim<2)
		    continue;
						   // Direction -y
		  if (iy==0)
		    {
		      const unsigned int nn = patch->neighbors[2];
		      out << '\t';
		      if (nn != patch->no_neighbor)
			out << (nn*cells_per_patch+nx+nz+dy*(n-1));
		      else
			out << "-1";
		    } else {      
		      out << '\t'
			  << patch_start+nx+ny-dy+nz;
		    }
						   // Direction +y
		  if (iy == n-1)
		    {
		      const unsigned int nn = patch->neighbors[3];
		      out << '\t';
		      if (nn != patch->no_neighbor)
			out << (nn*cells_per_patch+nx+nz);
		      else
			out << "-1";
		    } else {
		      out << '\t'
			  << patch_start+nx+ny+dy+nz;
		    }
		  if (dim<3)
		    continue;
		  
						   // Direction -z
		  if (iz==0)
		    {
		      const unsigned int nn = patch->neighbors[4];
		      out << '\t';
		      if (nn != patch->no_neighbor)
			out << (nn*cells_per_patch+nx+ny+dz*(n-1));
		      else
			out << "-1";
		    } else {      
		      out << '\t'
			  << patch_start+nx+ny+nz-dz;
		    }
						   // Direction +z
		  if (iz == n-1)
		    {
		      const unsigned int nn = patch->neighbors[5];
		      out << '\t';
		      if (nn != patch->no_neighbor)
			out << (nn*cells_per_patch+nx+ny);
		      else
			out << "-1";
		    } else {
		      out << '\t'
			  << patch_start+nx+ny+nz+dz;
		    }
		}
	  out << '\n';	  
	}
    }
				   /////////////////////////////
				   // now write data
  if (n_data_sets != 0)
    {      
      out << "object \"data\" class array type float rank 1 shape "
	  << n_data_sets
	  << " items " << n_nodes << " data follows" << '\n';

				       // loop over all patches
      for (typename std::vector<Patch<dim,spacedim> >::const_iterator patch=patches.begin();
	   patch != patches.end(); ++patch)
	{
	  const unsigned int n_subdivisions = patch->n_subdivisions;
      
	  Assert (patch->data.n_rows() == n_data_sets,
		  ExcUnexpectedNumberOfDatasets (patch->data.n_rows(), n_data_sets));
	  Assert (patch->data.n_cols() == (dim==1 ?
				      n_subdivisions+1 :
				      (dim==2 ?
				       (n_subdivisions+1)*(n_subdivisions+1) :
				       (dim==3 ?
					(n_subdivisions+1)*(n_subdivisions+1)*(n_subdivisions+1) :
					0))),
		  ExcInvalidDatasetSize (patch->data.n_cols(), n_subdivisions+1));

	  switch (dim)
	    {
	      case 1:
	      {      
		for (unsigned int i=0; i<n_subdivisions+1; ++i) 
		  {
		    for (unsigned int data_set=0; data_set<n_data_sets; ++data_set)
		      out << patch->data(data_set,i) << '\t';
		    out << '\n';
		  };
	    
		break;
	      };
	   
	      case 2:
	      {
		for (unsigned int i=0; i<n_subdivisions+1; ++i)
		  for (unsigned int j=0; j<n_subdivisions+1; ++j)
		    {
		      for (unsigned int data_set=0; data_set<n_data_sets; ++data_set)
			out << patch->data(data_set,i*(n_subdivisions+1) + j) << '\t';
		      out << '\n';
		    };

		break;
	      };

	      case 3:
	      {
		for (unsigned int i=0; i<n_subdivisions+1; ++i)
		  for (unsigned int j=0; j<n_subdivisions+1; ++j)
		    for (unsigned int k=0; k<n_subdivisions+1; ++k)
		      {
			for (unsigned int data_set=0; data_set<n_data_sets; ++data_set)
			  out << patch->data(data_set,
					     (i*(n_subdivisions+1)+j)*(n_subdivisions+1)+k)
			      << '\t';
			out << '\n';
		      };

		break;
	      };

	      default:
		    Assert (false, ExcNotImplemented());
	    };
	};
      out << "attribute \"dep\" string \"positions\"" << '\n';
    } else {
      out << "object \"data\" class constantarray type float rank 0 items " << n_nodes << " data follows"
	  << '\n' << '0' << '\n';
    }
  
				   // no model data
  
  out << "object \"deal data\" class field" << '\n'
      << "component \"positions\" value \"vertices\"" << '\n'
      << "component \"connections\" value \"cells\"" << '\n'
      << "component \"data\" value \"data\"" << '\n';

  if (flags.write_neighbors)
    out << "component \"neighbors\" value \"neighbors\"" << '\n';

  out << "end" << '\n';
				   // assert the stream is still ok
  AssertThrow (out, ExcIO());
}



template <int dim, int spacedim>
void DataOutBase::write_gnuplot (const std::vector<Patch<dim,spacedim> > &patches,
				 const std::vector<std::string>          &data_names,
				 const GnuplotFlags                      &/*flags*/,
				 std::ostream                            &out) 
{
  AssertThrow (out, ExcIO());
  
  Assert (patches.size() > 0, ExcNoPatches());

  const unsigned int n_data_sets = data_names.size();
  

				   // loop over all patches
  for (typename std::vector<Patch<dim,spacedim> >::const_iterator patch=patches.begin();
       patch != patches.end(); ++patch)
    {
      const unsigned int n_subdivisions = patch->n_subdivisions;
      
      Assert (patch->data.n_rows() == n_data_sets,
	      ExcUnexpectedNumberOfDatasets (patch->data.n_rows(), n_data_sets));
      Assert (patch->data.n_cols() == (dim==1 ?
				  n_subdivisions+1 :
				  (dim==2 ?
				   (n_subdivisions+1)*(n_subdivisions+1) :
				   (dim==3 ?
				    (n_subdivisions+1)*(n_subdivisions+1)*(n_subdivisions+1) :
				    0))),
	      ExcInvalidDatasetSize (patch->data.n_cols(), n_subdivisions+1));

      switch (dim)
	{
	  case 1:
	  {      
	    for (unsigned int i=0; i<n_subdivisions+1; ++i) 
	      {
						 // compute coordinates for
						 // this patch point
		out << ((patch->vertices[1] * i / n_subdivisions) +
			(patch->vertices[0] * (n_subdivisions-i) / n_subdivisions))
		    << ' ';

		for (unsigned int data_set=0; data_set<n_data_sets; ++data_set)
		  out << patch->data(data_set,i) << ' ';

		out << '\n';
	      };

					     // end of patch
	    out << '\n'
		<< '\n';
	    
	    break;
	  };
	   
	  case 2:
	  {
	    for (unsigned int i=0; i<n_subdivisions+1; ++i)
	      {
		for (unsigned int j=0; j<n_subdivisions+1; ++j)
		  {
		    const double x_frac = i * 1./n_subdivisions,
				 y_frac = j * 1./n_subdivisions;
		    
						     // compute coordinates for
						     // this patch point
		    out << (((patch->vertices[1] * x_frac) +
			     (patch->vertices[0] * (1-x_frac))) * (1-y_frac) +
			    ((patch->vertices[2] * x_frac) +
			     (patch->vertices[3] * (1-x_frac))) * y_frac)
			<< ' ';
		    
		    for (unsigned int data_set=0; data_set<n_data_sets; ++data_set)
		      out << patch->data(data_set,i*(n_subdivisions+1) + j) << ' ';

		    out << '\n';
		  };

						 // end of row in patch
		out << '\n';
	      };

					     // end of patch
	    out << '\n';

	    break;
	  };

	  case 3:
	  {
					     // for all grid points: draw
					     // lines into all positive
					     // coordinate directions if
					     // there is another grid point
					     // there
	    for (unsigned int i=0; i<n_subdivisions+1; ++i)
	      for (unsigned int j=0; j<n_subdivisions+1; ++j)
		for (unsigned int k=0; k<n_subdivisions+1; ++k)
		  {
						     // note the broken
						     // design of hexahedra
						     // in deal.II, where
						     // first the z-component
						     // is counted up, before
						     // increasing the y-
						     // coordinate
		    const double x_frac = i * 1./n_subdivisions,
				 y_frac = k * 1./n_subdivisions,
				 z_frac = j * 1./n_subdivisions;

						     // compute coordinates for
						     // this patch point
		    const Point<spacedim> this_point
		      = ((((patch->vertices[1] * x_frac) +
			   (patch->vertices[0] * (1-x_frac))) * (1-y_frac) +
			  ((patch->vertices[2] * x_frac) +
			   (patch->vertices[3] * (1-x_frac))) * y_frac)   * (1-z_frac) +
			 (((patch->vertices[5] * x_frac) +
			   (patch->vertices[4] * (1-x_frac))) * (1-y_frac) +
			  ((patch->vertices[6] * x_frac) +
			   (patch->vertices[7] * (1-x_frac))) * y_frac)   * z_frac);

						     // line into positive x-direction
						     // if possible
		    if (i < n_subdivisions)
		      {
							 // write point here
							 // and its data
			out << this_point;
			for (unsigned int data_set=0; data_set<n_data_sets; ++data_set)
			  out  << ' '
			       << patch->data(data_set,
					      (i*(n_subdivisions+1) + j)*(n_subdivisions+1)+k);
			out << '\n';
			
							 // write point there
							 // and its data
			const double x_frac_new = x_frac + 1./n_subdivisions;
			out << ((((patch->vertices[1] * x_frac_new) +
				  (patch->vertices[0] * (1-x_frac_new))) * (1-y_frac) +
				 ((patch->vertices[2] * x_frac_new) +
				  (patch->vertices[3] * (1-x_frac_new))) * y_frac)   * (1-z_frac) +
				(((patch->vertices[5] * x_frac_new) +
				  (patch->vertices[4] * (1-x_frac_new))) * (1-y_frac) +
				 ((patch->vertices[6] * x_frac_new) +
				  (patch->vertices[7] * (1-x_frac_new))) * y_frac)   * z_frac);
			for (unsigned int data_set=0; data_set<n_data_sets; ++data_set)
			  out  << ' '
			       << patch->data(data_set,
					      ((i+1)*(n_subdivisions+1) + j)*(n_subdivisions+1)+k);
			out << '\n';

							 // end of line
			out << '\n'
			    << '\n';
		      };
		    
						     // line into positive y-direction
						     // if possible
		    if (j < n_subdivisions)
		      {
							 // write point here
							 // and its data
			out << this_point;
			for (unsigned int data_set=0; data_set<n_data_sets; ++data_set)
			  out  << ' '
			       << patch->data(data_set,
					      (i*(n_subdivisions+1) + j)*(n_subdivisions+1)+k);
			out << '\n';
			
							 // write point there
							 // and its data
			const double z_frac_new = z_frac + 1./n_subdivisions;
			out << ((((patch->vertices[1] * x_frac) +
				  (patch->vertices[0] * (1-x_frac))) * (1-y_frac) +
				 ((patch->vertices[2] * x_frac) +
				  (patch->vertices[3] * (1-x_frac))) * y_frac)   * (1-z_frac_new) +
				(((patch->vertices[5] * x_frac) +
				  (patch->vertices[4] * (1-x_frac))) * (1-y_frac) +
				 ((patch->vertices[6] * x_frac) +
				  (patch->vertices[7] * (1-x_frac))) * y_frac)   * z_frac_new);
			for (unsigned int data_set=0; data_set<n_data_sets; ++data_set)
			  out  << ' '
			       << patch->data(data_set,
					      (i*(n_subdivisions+1) + (j+1))*(n_subdivisions+1)+k);
			out << '\n';

							 // end of line
			out << '\n'
			    << '\n';
		      };

						     // line into positive z-direction
						     // if possible
		    if (k < n_subdivisions)
		      {
							 // write point here
							 // and its data
			out << this_point;
			for (unsigned int data_set=0; data_set<n_data_sets; ++data_set)
			  out  << ' '
			       << patch->data(data_set,
					      (i*(n_subdivisions+1) + j)*(n_subdivisions+1)+k);
			out << '\n';
			
							 // write point there
							 // and its data
			const double y_frac_new = y_frac + 1./n_subdivisions;
			out << ((((patch->vertices[1] * x_frac) +
				  (patch->vertices[0] * (1-x_frac))) * (1-y_frac_new) +
				 ((patch->vertices[2] * x_frac) +
				  (patch->vertices[3] * (1-x_frac))) * y_frac_new)   * (1-z_frac) +
				(((patch->vertices[5] * x_frac) +
				  (patch->vertices[4] * (1-x_frac))) * (1-y_frac_new) +
				 ((patch->vertices[6] * x_frac) +
				  (patch->vertices[7] * (1-x_frac))) * y_frac_new)   * z_frac);
			for (unsigned int data_set=0; data_set<n_data_sets; ++data_set)
			  out  << ' '
			       << patch->data(data_set,
					      (i*(n_subdivisions+1) + j)*(n_subdivisions+1)+k+1);
			out << '\n';

							 // end of line
			out << '\n'
			    << '\n';
		      };		    
			
		  };

	    break;
	  };

	  default:
		Assert (false, ExcNotImplemented());
	};
    };

  AssertThrow (out, ExcIO());
}



template <int dim, int spacedim>
void DataOutBase::write_povray (const std::vector<Patch<dim,spacedim> > &patches,
				const std::vector<std::string>          &data_names,
				const PovrayFlags                       &flags,
				std::ostream                            &out) 
{
  AssertThrow (out, ExcIO());
  
  Assert (patches.size() > 0, ExcNoPatches());
  Assert (dim==2, ExcNotImplemented());        // only for 2-D surfaces on a 2-D plane
  Assert (spacedim==2, ExcNotImplemented());

  const unsigned int n_data_sets = data_names.size();
  
				   // max. and min. heigth of solution 
  double hmin=0, hmax=0;

  for (typename std::vector<Patch<dim,spacedim> >::const_iterator patch=patches.begin();
       patch != patches.end(); ++patch)
    {
      const unsigned int n_subdivisions = patch->n_subdivisions;
      
      Assert (patch->data.n_rows() == n_data_sets,
	      ExcUnexpectedNumberOfDatasets (patch->data.n_rows(), n_data_sets));
      Assert (patch->data.n_cols() == (dim==1 ?
				  n_subdivisions+1 :
				  (dim==2 ?
				   (n_subdivisions+1)*(n_subdivisions+1) :
				   (dim==3 ?
				    (n_subdivisions+1)*(n_subdivisions+1)*(n_subdivisions+1) :
				    0))),
	      ExcInvalidDatasetSize (patch->data.n_cols(), n_subdivisions+1));
      
      for (unsigned int i=0; i<n_subdivisions; ++i)
	for (unsigned int j=0; j<n_subdivisions; ++j)
	  {
	    const int dl = i*(n_subdivisions+1)+j;
	    if ((hmin==0)||(patch->data(0,dl)<hmin)) hmin=patch->data(0,dl);
	    if ((hmax==0)||(patch->data(0,dl)>hmax)) hmax=patch->data(0,dl);
	  }
    }

  out << "#declare HMIN=" << hmin << ";" << '\n'
      << "#declare HMAX=" << hmax << ";" << '\n' << '\n';

  if (!flags.external_data)
    {
				       // texture with scaled niveau lines
				       // 10 lines in the surface
      out << "#declare Tex=texture{" << '\n'
	  << "  pigment {" << '\n'
	  << "    gradient y" << '\n'
	  << "    scale y*(HMAX-HMIN)*" << 0.1 << '\n'
	  << "    color_map {" << '\n'
	  << "      [0.00 color Light_Purple] " << '\n'
	  << "      [0.95 color Light_Purple] " << '\n'
	  << "      [1.00 color White]    " << '\n'
	  << "} } }" << '\n' << '\n';
    }

  if (!flags.bicubic_patch)
    {                                  // start of mesh header
      out << '\n'
	  << "mesh {" << '\n';
    }

				   // loop over all patches
  for (typename std::vector<Patch<dim,spacedim> >::const_iterator patch=patches.begin();
       patch != patches.end(); ++patch)
    {
      const unsigned int n_subdivisions = patch->n_subdivisions;
      
      Assert (patch->data.n_rows() == n_data_sets,
	      ExcUnexpectedNumberOfDatasets (patch->data.n_rows(), n_data_sets));
      Assert (patch->data.n_cols() == (dim==1 ?
				  n_subdivisions+1 :
				  (dim==2 ?
				   (n_subdivisions+1)*(n_subdivisions+1) :
				   (dim==3 ?
				    (n_subdivisions+1)*(n_subdivisions+1)*(n_subdivisions+1) :
				    0))),
	      ExcInvalidDatasetSize (patch->data.n_cols(), n_subdivisions+1));


      std::vector<Point<spacedim> > ver((n_subdivisions+1)*
					(n_subdivisions+1));
      
      for (unsigned int i=0; i<n_subdivisions+1; ++i)
	{
	  for (unsigned int j=0; j<n_subdivisions+1; ++j)
	    {
	      const double x_frac = i * 1./n_subdivisions,
			   y_frac = j * 1./n_subdivisions;
					       // compute coordinates for
					       // this patch point, storing in ver
	      ver[i*(n_subdivisions+1)+j]= (((patch->vertices[1] * x_frac) +
					     (patch->vertices[0] * (1-x_frac))) * (1-y_frac) +
					    ((patch->vertices[2] * x_frac) +
					     (patch->vertices[3] * (1-x_frac))) * y_frac);
	    };
	};
      
      if (!flags.bicubic_patch)
	{                                    // setting up triangles
	  for (unsigned int i=0; i<n_subdivisions; ++i)
	    {
	      for (unsigned int j=0; j<n_subdivisions; ++j)
		{
						   // down/left vertex of triangle
		  const int dl = i*(n_subdivisions+1)+j;
		  if (flags.smooth)               // only if smooth triangles are used
		    {
						       // aproximate normal
						       // vectors in patch
		      std::vector<Point<3> > nrml((n_subdivisions+1)*
						  (n_subdivisions+1));
		      Point<3> h1,h2;
		      for (unsigned int i=0; i<n_subdivisions+1;++i)
			{
			  for (unsigned int j=0; j<n_subdivisions+1;++j)
			    {
			      if (i==0)
				{
				  h1(0)=ver[(i+1)*(n_subdivisions+1)+j](0)-
					ver[i*(n_subdivisions+1)+j](0);
				  h1(1)=patch->data(0,(i+1)*(n_subdivisions+1)+j)-
					patch->data(0,i*(n_subdivisions+1)+j);
				  h1(2)=ver[(i+1)*(n_subdivisions+1)+j](1)-
					ver[i*(n_subdivisions+1)+j](1);
				}
			      else
				if (i==n_subdivisions)
				  {
				    h1(0)=ver[i*(n_subdivisions+1)+j](0)-
					  ver[(i-1)*(n_subdivisions+1)+j](0);
				    h1(1)=patch->data(0,i*(n_subdivisions+1)+j)-
					  patch->data(0,(i-1)*(n_subdivisions+1)+j);
				    h1(2)=ver[i*(n_subdivisions+1)+j](1)-
					  ver[(i-1)*(n_subdivisions+1)+j](1);
				  }
				else
				  {
				    h1(0)=ver[(i+1)*(n_subdivisions+1)+j](0)-
					  ver[(i-1)*(n_subdivisions+1)+j](0);
				    h1(1)=patch->data(0,(i+1)*(n_subdivisions+1)+j)-
					  patch->data(0,(i-1)*(n_subdivisions+1)+j);
				    h1(2)=ver[(i+1)*(n_subdivisions+1)+j](1)-
					  ver[(i-1)*(n_subdivisions+1)+j](1);
				  };
			      if (j==0)
				{
				  h2(0)=ver[i*(n_subdivisions+1)+j+1](0)-
					ver[i*(n_subdivisions+1)+j](0);
				  h2(1)=patch->data(0,i*(n_subdivisions+1)+j+1)-
					patch->data(0,i*(n_subdivisions+1)+j);
				  h2(2)=ver[i*(n_subdivisions+1)+j+1](1)-
					ver[i*(n_subdivisions+1)+j](1);
				}
			      else
				if (j==n_subdivisions)
				  {
				    h2(0)=ver[i*(n_subdivisions+1)+j](0)-
					  ver[i*(n_subdivisions+1)+j-1](0);
				    h2(1)=patch->data(0,i*(n_subdivisions+1)+j)-
					  patch->data(0,i*(n_subdivisions+1)+j-1);
				    h2(2)=ver[i*(n_subdivisions+1)+j](1)-
					  ver[i*(n_subdivisions+1)+j-1](1);
				  }
				else
				  {
				    h2(0)=ver[i*(n_subdivisions+1)+j+1](0)-
					  ver[i*(n_subdivisions+1)+j-1](0);
				    h2(1)=patch->data(0,i*(n_subdivisions+1)+j+1)-
					  patch->data(0,i*(n_subdivisions+1)+j-1);
				    h2(2)=ver[i*(n_subdivisions+1)+j+1](1)-
					  ver[i*(n_subdivisions+1)+j-1](1);
				  };
			      nrml[i*(n_subdivisions+1)+j](0)=h1(1)*h2(2)-h1(2)*h2(1);
			      nrml[i*(n_subdivisions+1)+j](1)=h1(2)*h2(0)-h1(0)*h2(2);
			      nrml[i*(n_subdivisions+1)+j](2)=h1(0)*h2(1)-h1(1)*h2(0);

							       // normalize Vector
			      double norm=std::sqrt(
				std::pow(nrml[i*(n_subdivisions+1)+j](0),2.)+
				std::pow(nrml[i*(n_subdivisions+1)+j](1),2.)+
				std::pow(nrml[i*(n_subdivisions+1)+j](2),2.));
			      
			      if (nrml[i*(n_subdivisions+1)+j](1)<0)
				norm*=-1.;
			      
			      for (unsigned int k=0;k<3;++k)
				nrml[i*(n_subdivisions+1)+j](k)/=norm;
			    }
			}
						       // writing smooth_triangles
		      
						       // down/right triangle
		      out << "smooth_triangle {" << '\n' << "\t<" 
			  << ver[dl](0) << ","   
			  << patch->data(0,dl) << ","
			  << ver[dl](1) << ">, <"
			  << nrml[dl](0) << ", " << nrml[dl](1) << ", " << nrml[dl](2)
			  << ">," << '\n';
		      out << " \t<" 
			  << ver[dl+n_subdivisions+1](0) << "," 
			  << patch->data(0,dl+n_subdivisions+1)  << ","
			  << ver[dl+n_subdivisions+1](1) << ">, <"
			  << nrml[dl+n_subdivisions+1](0) << ", "
			  << nrml[dl+n_subdivisions+1](1) << ", "
			  << nrml[dl+n_subdivisions+1](2)
			  << ">," << '\n';
		      out << "\t<" 
			  << ver[dl+n_subdivisions+2](0) << "," 
			  << patch->data(0,dl+n_subdivisions+2)  << ","
			  << ver[dl+n_subdivisions+2](1) << ">, <"
			  << nrml[dl+n_subdivisions+2](0) << ", "
			  << nrml[dl+n_subdivisions+2](1) << ", "
			  << nrml[dl+n_subdivisions+2](2) 
			  << ">}" << '\n'; 
		      
						       // upper/left triangle
		      out << "smooth_triangle {" << '\n' << "\t<" 
			  << ver[dl](0) << "," 
			  << patch->data(0,dl) << ","
			  << ver[dl](1) << ">, <"
			  << nrml[dl](0) << ", " << nrml[dl](1) << ", " << nrml[dl](2) 
			  << ">," << '\n';
		      out << "\t<" 
			  << ver[dl+n_subdivisions+2](0) << "," 
			  << patch->data(0,dl+n_subdivisions+2)  << ","
			  << ver[dl+n_subdivisions+2](1) << ">, <"
			  << nrml[dl+n_subdivisions+2](0) << ", "
			  << nrml[dl+n_subdivisions+2](1) << ", "
			  << nrml[dl+n_subdivisions+2](2)
			  << ">," << '\n';
		      out << "\t<" 
			  << ver[dl+1](0) << "," 
			  << patch->data(0,dl+1)  << ","
			  << ver[dl+1](1) << ">, <"
			  << nrml[dl+1](0) << ", " << nrml[dl+1](1) << ", " << nrml[dl+1](2)
			  << ">}" << '\n';
		    }
		  else
		    {		
						       // writing standard triangles
						       // down/right triangle
		      out << "triangle {" << '\n' << "\t<" 
			  << ver[dl](0) << "," 
			  << patch->data(0,dl) << ","
			  << ver[dl](1) << ">," << '\n';
		      out << "\t<" 
			  << ver[dl+n_subdivisions+1](0) << "," 
			  << patch->data(0,dl+n_subdivisions+1)  << ","
			  << ver[dl+n_subdivisions+1](1) << ">," << '\n';
		      out << "\t<" 
			  << ver[dl+n_subdivisions+2](0) << "," 
			  << patch->data(0,dl+n_subdivisions+2)  << ","
			  << ver[dl+n_subdivisions+2](1) << ">}" << '\n'; 
			    
						       // upper/left triangle
		      out << "triangle {" << '\n' << "\t<" 
			  << ver[dl](0) << "," 
			  << patch->data(0,dl) << ","
			  << ver[dl](1) << ">," << '\n';
		      out << "\t<"
			  << ver[dl+n_subdivisions+2](0) << "," 
			  << patch->data(0,dl+n_subdivisions+2)  << ","
			  << ver[dl+n_subdivisions+2](1) << ">," << '\n';
		      out << "\t<" 
			  << ver[dl+1](0) << ","
			  << patch->data(0,dl+1)  << ","
			   << ver[dl+1](1) << ">}" << '\n';
		        };
		};
	    };
	}
      else
	{                                    // writing bicubic_patch
	  Assert (n_subdivisions==3, ExcUnexpectedNumberOfSubdivisions(n_subdivisions,3));
	  out << '\n'
	      << "bicubic_patch {" << '\n'
	      << "  type 0" << '\n'
	      << "  flatness 0" << '\n'
	      << "  u_steps 0" << '\n'
	      << "  v_steps 0" << '\n';
	  for (int i=0;i<16;++i)
	    {
	      out << "\t<" << ver[i](0) << "," << patch->data(0,i) << "," << ver[i](1) << ">";
	      if (i!=15) out << ",";
	      out << '\n';
	    };
	  out << "  texture {Tex}" <<  '\n'
	      << "}" << '\n';
	};
    };
  
  if (!flags.bicubic_patch) 
    {                                   // the end of the mesh
      out << "  texture {Tex}" << '\n'
	  << "}" << '\n'
	  << '\n';
    }
  
  AssertThrow (out, ExcIO());
}



template <int dim, int spacedim>
void DataOutBase::write_eps (const std::vector<Patch<dim,spacedim> > &patches,
			     const std::vector<std::string>          &/*data_names*/,
			     const EpsFlags                          &flags,
			     std::ostream                            &out) 
{
  Assert (out, ExcIO());
  
  Assert (patches.size() > 0, ExcNoPatches());

				   // Do not allow volume rendering
  Assert (dim<=2, ExcNotImplemented());
  
  switch (dim) 
    {
      case 2:
      {
					 // set up an array of cells to be
					 // written later. this array holds the
					 // cells of all the patches as
					 // projected to the plane perpendicular
					 // to the line of sight.
					 //
					 // note that they are kept sorted by
					 // the set, where we chose the value
					 // of the center point of the cell
					 // along the line of sight as value
					 // for sorting
	std::multiset<EpsCell2d> cells;

					 // two variables in which we
					 // will store the minimum and
					 // maximum values of the field
					 // to be used for colorization
					 //
					 // preset them by 0 to calm down the
					 // compiler; they are initialized later
	double min_color_value=0, max_color_value=0;
	
					 // Array for z-coordinates of points.
					 // The elevation determined by a function if spacedim=2
					 // or the z-cooridate of the grid point if spacedim=3
	double heights[4];

					 // compute the cells for output and
					 // enter them into the set above
					 // note that since dim==2, we
					 // have exactly four vertices per
					 // patch and per cell
	for (typename std::vector<Patch<dim,spacedim> >::const_iterator patch=patches.begin();
	     patch!=patches.end(); ++patch)
	  {
	    const unsigned int n_subdivisions = patch->n_subdivisions;
	    for (unsigned int i=0; i<n_subdivisions; ++i)
	      for (unsigned int j=0; j<n_subdivisions; ++j)
		{
		  const double x_frac = i * 1./n_subdivisions,
			       y_frac = j * 1./n_subdivisions,
					
			      x_frac1 = (i+1) * 1./n_subdivisions,
			      y_frac1 = (j+1) * 1./n_subdivisions;
		  
		  const Point<spacedim> points[4]
		    = { (((patch->vertices[1] * x_frac) +
			  (patch->vertices[0] * (1-x_frac))) * (1-y_frac) +
			 ((patch->vertices[2] * x_frac) +
			  (patch->vertices[3] * (1-x_frac))) * y_frac),

			  (((patch->vertices[1] * x_frac1) +
			    (patch->vertices[0] * (1-x_frac1))) * (1-y_frac) +
			   ((patch->vertices[2] * x_frac1) +
			    (patch->vertices[3] * (1-x_frac1))) * y_frac),

			  (((patch->vertices[1] * x_frac1) +
			    (patch->vertices[0] * (1-x_frac1))) * (1-y_frac1) +
			   ((patch->vertices[2] * x_frac1) +
			    (patch->vertices[3] * (1-x_frac1))) * y_frac1),

			  (((patch->vertices[1] * x_frac) +
			    (patch->vertices[0] * (1-x_frac))) * (1-y_frac1) +
			   ((patch->vertices[2] * x_frac) +
			    (patch->vertices[3] * (1-x_frac))) * y_frac1) 
			  };

		  switch (spacedim)
		    {
		    case 2:
		      Assert ((flags.height_vector < patch->data.n_rows()) ||
			      patch->data.n_rows() == 0,
			      ExcInvalidVectorNumber (flags.height_vector,
						  patch->data.n_rows()));
		      heights[0] = patch->data.n_rows() != 0 ?
			patch->data(flags.height_vector,i*(n_subdivisions+1) + j) * flags.z_scaling
			: 0;
			
		      heights[1] = patch->data.n_rows() != 0 ?
			patch->data(flags.height_vector,(i+1)*(n_subdivisions+1) + j) * flags.z_scaling
			: 0;
			
		      heights[2] = patch->data.n_rows() != 0 ?
			patch->data(flags.height_vector,(i+1)*(n_subdivisions+1) + j+1) * flags.z_scaling
			: 0;
			
		      heights[3] = patch->data.n_rows() != 0 ?
			patch->data(flags.height_vector,i*(n_subdivisions+1) + j+1) * flags.z_scaling
			: 0;
		      break;
		    case 3:
						       // Copy z-coordinates into the height vector
		      for (unsigned int i=0;i<4;++i)
			heights[i] = points[i](2);
		      break;
		    default:
		      Assert(false, ExcNotImplemented());
		    }
		  

						   // now compute the projection of
						   // the bilinear cell given by the
						   // four vertices and their heights
						   // and write them to a proper
						   // cell object. note that we only
						   // need the first two components
						   // of the projected position for
						   // output, but we need the value
						   // along the line of sight for
						   // sorting the cells for back-to-
						   // front-output
						   //
						   // this computation was first written
						   // by Stefan Nauber. please no-one
						   // ask me why it works that way (or
						   // may be not), especially not about
						   // the angles and the sign of
						   // the height field, I don't know
						   // it.
		  EpsCell2d eps_cell;
		  const double pi = deal_II_numbers::PI;
		  const double cx = -std::cos(pi-flags.azimut_angle * 2*pi / 360.),
			       cz = -std::cos(flags.turn_angle * 2*pi / 360.),
			       sx = std::sin(pi-flags.azimut_angle * 2*pi / 360.),
			       sz = std::sin(flags.turn_angle * 2*pi / 360.);
		  for (unsigned int vertex=0; vertex<4; ++vertex)
		    {
		      const double x = points[vertex](0),
			y = points[vertex](1),
			z = -heights[vertex];
		      
		      eps_cell.vertices[vertex](0) = -   cz*x+   sz*y;
		      eps_cell.vertices[vertex](1) = -cx*sz*x-cx*cz*y-sx*z;

						       //      ( 1 0    0 )
						       // Dx = ( 0 cx -sx ) 
						       //      ( 0 sx  cx )

						       //      ( cy 0 sy )
						       // Dy = (  0 1  0 )
						       //      (-sy 0 cy )

						       //      ( cz -sz 0 )
						       // Dz = ( sz  cz 0 )
						       //      (  0   0 1 )

//       ( cz -sz 0 )( 1 0    0 )(x)   ( cz*x-sz*(cx*y-sx*z)+0*(sx*y+cx*z) )
// Dxz = ( sz  cz 0 )( 0 cx -sx )(y) = ( sz*x+cz*(cx*y-sx*z)+0*(sx*y+cx*z) )
// 	 (  0   0 1 )( 0 sx  cx )(z)   (  0*x+	*(cx*y-sx*z)+1*(sx*y+cx*z) )
		    };

						   // compute coordinates of
						   // center of cell
		  const Point<spacedim> center_point
		    = (points[0] + points[1] + points[2] + points[3]) / 4;
		  const double center_height
		    = -(heights[0] + heights[1] + heights[2] + heights[3]) / 4;

						   // compute the depth into
						   // the picture
		  eps_cell.depth = -sx*sz*center_point(0)
				   -sx*cz*center_point(1)
				   +cx*center_height;

		  if (flags.draw_cells && flags.shade_cells)
		    {
		      Assert ((flags.color_vector < patch->data.n_rows()) ||
			      patch->data.n_rows() == 0,
			      ExcInvalidVectorNumber (flags.color_vector,
						      patch->data.n_rows()));
		      const double color_values[4]
			= { patch->data.n_rows() != 0 ?
			    patch->data(flags.color_vector,i*(n_subdivisions+1) + j)       : 1,
			
			    patch->data.n_rows() != 0 ?
			    patch->data(flags.color_vector,(i+1)*(n_subdivisions+1) + j)   : 1,
			    
			    patch->data.n_rows() != 0 ?
			    patch->data(flags.color_vector,(i+1)*(n_subdivisions+1) + j+1) : 1,
			    
			    patch->data.n_rows() != 0 ?
			    patch->data(flags.color_vector,i*(n_subdivisions+1) + j+1)     : 1};

						       // set color value to average of the value
						       // at the vertices
		      eps_cell.color_value = (color_values[0] +
					      color_values[1] +
					      color_values[2] +
					      color_values[3]) / 4;

						       // update bounds of color
						       // field
		      if (patch == patches.begin())
			min_color_value = max_color_value = eps_cell.color_value;
		      else
			{
			  min_color_value = (min_color_value < eps_cell.color_value ?
					     min_color_value : eps_cell.color_value);
			  max_color_value = (max_color_value > eps_cell.color_value ?
					     max_color_value : eps_cell.color_value);
			};
		    };
		  
						   // finally add this cell
		  cells.insert (eps_cell);
		};
	  };

					 // find out minimum and maximum x and
					 // y coordinates to compute offsets
					 // and scaling factors
	double x_min = cells.begin()->vertices[0](0);
	double x_max = x_min;
	double y_min = cells.begin()->vertices[0](1);
	double y_max = y_min;
	
	for (typename std::multiset<EpsCell2d>::const_iterator
	       cell=cells.begin();
	     cell!=cells.end(); ++cell)
	  for (unsigned int vertex=0; vertex<4; ++vertex)
	    {
	      x_min = std::min (x_min, cell->vertices[vertex](0));
	      x_max = std::max (x_max, cell->vertices[vertex](0));
	      y_min = std::min (y_min, cell->vertices[vertex](1));
	      y_max = std::max (y_max, cell->vertices[vertex](1));
	    };
	
					 // scale in x-direction such that
					 // in the output 0 <= x <= 300.
					 // don't scale in y-direction to
					 // preserve the shape of the
					 // triangulation
	const double scale = (flags.size /
			      (flags.size_type==EpsFlags::width ?
			       x_max - x_min :
			       y_min - y_max));
	
	const Point<2> offset(x_min, y_min);


					 // now write preamble
	if (true) 
	  {
					     // block this to have local
					     // variables destroyed after
					     // use
	    out << "%!PS-Adobe-2.0 EPSF-1.2" << '\n'
		<< "%%Title: deal.II Output" << '\n'
		<< "%%Creator: the deal.II library" << '\n'
		<< "%%BoundingBox: "
					       // lower left corner
		<< "0 0 "
					       // upper right corner
		<< static_cast<unsigned int>( (x_max-x_min) * scale )
		<< ' '
		<< static_cast<unsigned int>( (y_max-y_min) * scale )
		<< '\n';
	    
					     // define some abbreviations to keep
					     // the output small:
					     // m=move turtle to
					     // l=define a line
					     // s=set rgb color
					     // sg=set gray value
					     // lx=close the line and plot the line
					     // lf=close the line and fill the interior
	    out << "/m {moveto} bind def"      << '\n'
		<< "/l {lineto} bind def"      << '\n'
		<< "/s {setrgbcolor} bind def" << '\n'
		<< "/sg {setgray} bind def"    << '\n'
		<< "/lx {lineto closepath stroke} bind def" << '\n'
		<< "/lf {lineto closepath fill} bind def"   << '\n';
	    
	    out << "%%EndProlog" << '\n'
		<< '\n';
					     // set fine lines
	    out << flags.line_width << " setlinewidth" << '\n';
					     // allow only five digits
					     // for output (instead of the
					     // default six); this should suffice
					     // even for fine grids, but reduces
					     // the file size significantly
	    out << std::setprecision (5);
	  };

					 // check if min and max
					 // values for the color are
					 // actually different. If
					 // that is not the case (such
					 // things happen, for
					 // example, in the very first
					 // time step of a time
					 // dependent problem, if the
					 // initial values are zero),
					 // all values are equal, and
					 // then we can draw
					 // everything in an arbitrary
					 // color. Thus, change one of
					 // the two values arbitrarily
	if (max_color_value == min_color_value)
	  max_color_value = min_color_value+1;

					 // now we've got all the information
					 // we need. write the cells.
					 // note: due to the ordering, we
					 // traverse the list of cells
					 // back-to-front
	for (typename std::multiset<EpsCell2d>::const_iterator
	       cell=cells.begin();
	     cell!=cells.end(); ++cell)
	  {
	    if (flags.draw_cells) 
	      {
		if (flags.shade_cells)
		  {
		    const EpsFlags::RgbValues rgb_values
		      = (*flags.color_function) (cell->color_value,
						 min_color_value,
						 max_color_value);

						     // write out color
		    if (rgb_values.is_grey())
		      out << rgb_values.red << " sg ";
		    else
		      out << rgb_values.red   << ' '
			  << rgb_values.green << ' '
			  << rgb_values.blue  << " s ";
		  }
		else
		  out << "1 sg ";

		out << (cell->vertices[0]-offset) * scale << " m "
		    << (cell->vertices[1]-offset) * scale << " l "
		    << (cell->vertices[2]-offset) * scale << " l "
		    << (cell->vertices[3]-offset) * scale << " lf"
		    << '\n';
	      };
	    
	    if (flags.draw_mesh)
	      out << "0 sg "      // draw lines in black
		  << (cell->vertices[0]-offset) * scale << " m "
		  << (cell->vertices[1]-offset) * scale << " l "
		  << (cell->vertices[2]-offset) * scale << " l "
		  << (cell->vertices[3]-offset) * scale << " lx"
		  << '\n';
	  };
	out << "showpage" << '\n';
	
	break;
      };
       
      default:
	    Assert (false, ExcNotImplemented());
    };
}



template <int dim, int spacedim>
void DataOutBase::write_gmv (const std::vector<Patch<dim,spacedim> > &patches,
			     const std::vector<std::string>          &data_names,
			     const GmvFlags                          &/*flags*/,
			     std::ostream                            &out) 
{
  AssertThrow (out, ExcIO());

  Assert (patches.size() > 0, ExcNoPatches());
 
  const unsigned int n_data_sets = data_names.size();
				   // check against # of data sets in
				   // first patch. checks against all
				   // other patches are made in
				   // write_gmv_reorder_data_vectors
  Assert (n_data_sets == patches[0].data.n_rows(),
	  ExcUnexpectedNumberOfDatasets (patches[0].data.n_rows(), n_data_sets));
  
  
				   ///////////////////////
				   // preamble
  out << "gmvinput ascii"
      << '\n'
      << '\n';

				   // first count the number of cells
				   // and cells for later use
  unsigned int n_cells = 0,
	       n_nodes = 0;
  for (typename std::vector<Patch<dim,spacedim> >::const_iterator patch=patches.begin();
       patch!=patches.end(); ++patch)
    switch (dim)
      {
	case 1:
	      n_cells += patch->n_subdivisions;
	      n_nodes += patch->n_subdivisions+1;
	      break;
	case 2:
	      n_cells += patch->n_subdivisions *
			 patch->n_subdivisions;
	      n_nodes += (patch->n_subdivisions+1) *
			 (patch->n_subdivisions+1);
	      break;
	case 3:
	      n_cells += patch->n_subdivisions *
			 patch->n_subdivisions *
			 patch->n_subdivisions;
	      n_nodes += (patch->n_subdivisions+1) *
			 (patch->n_subdivisions+1) *
			 (patch->n_subdivisions+1);
	      break;
	default:
	      Assert (false, ExcNotImplemented());
      };


				   // in gmv format the vertex
				   // coordinates and the data have an
				   // order that is a bit unpleasant
				   // (first all x coordinates, then
				   // all y coordinate, ...; first all
				   // data of variable 1, then
				   // variable 2, etc), so we have to
				   // copy the data vectors a bit around
				   //
				   // note that we copy vectors when
				   // looping over the patches since we
				   // have to write them one variable
				   // at a time and don't want to use
				   // more than one loop
				   //
				   // this copying of data vectors can
				   // be done while we already output
				   // the vertices, so do this on a
				   // separate thread and when wanting
				   // to write out the data, we wait
				   // for that thread to finish
  Table<2,double> data_vectors (n_data_sets, n_nodes);
  void (*fun_ptr) (const std::vector<Patch<dim,spacedim> > &,
		   Table<2,double> &)
    = &DataOutBase::template write_gmv_reorder_data_vectors<dim,spacedim>;
  Threads::Thread<> reorder_thread = Threads::spawn (fun_ptr)(patches, data_vectors);

				   ///////////////////////////////
				   // first make up a list of used
				   // vertices along with their
				   // coordinates
				   //
				   // note that we have to print
				   // d=1..3 dimensions
  out << "nodes " << n_nodes << '\n';
  for (unsigned int d=1; d<=3; ++d)
    {
      for (typename std::vector<Patch<dim,spacedim> >::const_iterator patch=patches.begin();
	   patch!=patches.end(); ++patch)
	{
	  const unsigned int n_subdivisions = patch->n_subdivisions;
	  
					   // if we have nonzero values for
					   // this coordinate
	  if (d<=spacedim)
	    {
	      switch (dim)
		{
		  case 1:
		  {
		    for (unsigned int i=0; i<n_subdivisions+1; ++i)
		      out << ((patch->vertices[1](0) * i / n_subdivisions) +
			      (patch->vertices[0](0) * (n_subdivisions-i) / n_subdivisions))
			  << '\n';
		    break;
		  };
		   
		  case 2:
		  {
		    for (unsigned int i=0; i<n_subdivisions+1; ++i)
		      for (unsigned int j=0; j<n_subdivisions+1; ++j)
			{
			  const double x_frac = i * 1./n_subdivisions,
				       y_frac = j * 1./n_subdivisions;
			  
							   // compute coordinates for
							   // this patch point
			  out << (((patch->vertices[1](d-1) * x_frac) +
				   (patch->vertices[0](d-1) * (1-x_frac))) * (1-y_frac) +
				  ((patch->vertices[2](d-1) * x_frac) +
				   (patch->vertices[3](d-1) * (1-x_frac))) * y_frac)
			      << '\n';
			};
		    break;
		  };
		   
		  case 3:
		  {
		    for (unsigned int i=0; i<n_subdivisions+1; ++i)
		      for (unsigned int j=0; j<n_subdivisions+1; ++j)
			for (unsigned int k=0; k<n_subdivisions+1; ++k)
			  {
							     // note the broken
							     // design of hexahedra
							     // in deal.II, where
							     // first the z-component
							     // is counted up, before
							     // increasing the y-
							     // coordinate
			    const double x_frac = i * 1./n_subdivisions,
					 y_frac = k * 1./n_subdivisions,
					 z_frac = j * 1./n_subdivisions;
			    
							     // compute coordinates for
							     // this patch point
			    out << ((((patch->vertices[1](d-1) * x_frac) +
				      (patch->vertices[0](d-1) * (1-x_frac))) * (1-y_frac) +
				     ((patch->vertices[2](d-1) * x_frac) +
				      (patch->vertices[3](d-1) * (1-x_frac))) * y_frac)   * (1-z_frac) +
				    (((patch->vertices[5](d-1) * x_frac) +
				      (patch->vertices[4](d-1) * (1-x_frac))) * (1-y_frac) +
				     ((patch->vertices[6](d-1) * x_frac) +
				      (patch->vertices[7](d-1) * (1-x_frac))) * y_frac)   * z_frac)
				<< '\n';
			  };
	      
		    break;
		  };
		   
		  default:
			Assert (false, ExcNotImplemented());
		};
	    }
	  else
					     // d>spacedim. write zeros instead
	    {
	      const unsigned int n_points
		= static_cast<unsigned int>(std::pow (static_cast<double>(n_subdivisions+1), dim));
	      for (unsigned int i=0; i<n_points; ++i)
		out << "0 ";
	    };
	};
      out << '\n';
    };

  out << '\n';

				   /////////////////////////////////
				   // now for the cells. note that
				   // vertices are counted from 1 onwards
  if (true)
    {
      out << "cells " << n_cells << '\n';


      unsigned int first_vertex_of_patch = 0;
      
      for (typename std::vector<Patch<dim,spacedim> >::const_iterator patch=patches.begin();
	   patch!=patches.end(); ++patch)
	{
	  const unsigned int n_subdivisions = patch->n_subdivisions;

					   // write out the cells making
					   // up this patch
	  switch (dim)
	    {
	      case 1:
	      {
		for (unsigned int i=0; i<n_subdivisions; ++i)
		  out << "line 2\n  "
		      << first_vertex_of_patch+i+1 << ' '
		      << first_vertex_of_patch+i+1+1 << '\n';
		break;
	      };
	       
	      case 2:
	      {
		for (unsigned int i=0; i<n_subdivisions; ++i)
		  for (unsigned int j=0; j<n_subdivisions; ++j)
		    out << "quad 4\n  "
			<< first_vertex_of_patch+i*(n_subdivisions+1)+j+1 << ' '
			<< first_vertex_of_patch+(i+1)*(n_subdivisions+1)+j+1 << ' '
			<< first_vertex_of_patch+(i+1)*(n_subdivisions+1)+j+1+1 << ' '
			<< first_vertex_of_patch+i*(n_subdivisions+1)+j+1+1
			<< '\n';
		break;
	      };
	       
	      case 3:
	      {
		for (unsigned int i=0; i<n_subdivisions; ++i)
		  for (unsigned int j=0; j<n_subdivisions; ++j)
		    for (unsigned int k=0; k<n_subdivisions; ++k)
		      {
			out << "hex 8\n   "
							   // note: vertex indices start with 1!
			    << first_vertex_of_patch+(i*(n_subdivisions+1)+j      )*(n_subdivisions+1)+k  +1 << ' '
			    << first_vertex_of_patch+((i+1)*(n_subdivisions+1)+j  )*(n_subdivisions+1)+k  +1 << ' '
			    << first_vertex_of_patch+((i+1)*(n_subdivisions+1)+j+1)*(n_subdivisions+1)+k  +1 << ' '
			    << first_vertex_of_patch+(i*(n_subdivisions+1)+j+1    )*(n_subdivisions+1)+k  +1 << ' '
			    << first_vertex_of_patch+(i*(n_subdivisions+1)+j      )*(n_subdivisions+1)+k+1+1 << ' '
			    << first_vertex_of_patch+((i+1)*(n_subdivisions+1)+j  )*(n_subdivisions+1)+k+1+1 << ' '
			    << first_vertex_of_patch+((i+1)*(n_subdivisions+1)+j+1)*(n_subdivisions+1)+k+1+1 << ' '
			    << first_vertex_of_patch+(i*(n_subdivisions+1)+j+1    )*(n_subdivisions+1)+k+1+1 << ' '
			    << '\n';
		      };
		break;
	      };

	      default:
		    Assert (false, ExcNotImplemented());
	    };


					   // finally update the number
					   // of the first vertex of this patch
	  switch (dim)
	    {
	      case 1:
		    first_vertex_of_patch += n_subdivisions+1;
		    break;
	      case 2:
		    first_vertex_of_patch += (n_subdivisions+1) *
					     (n_subdivisions+1);
		    break;
	      case 3:
		    first_vertex_of_patch += (n_subdivisions+1) *
					     (n_subdivisions+1) *
					     (n_subdivisions+1);
		    break;
	      default:
		    Assert (false, ExcNotImplemented());
	    };
	};
      out << '\n';
    };

				   ///////////////////////////////////////
				   // data output.
  out << "variable" << '\n';

				   // now write the data vectors to
				   // @p{out} first make sure that all
				   // data is in place
  reorder_thread.join ();

				   // then write data.
				   // the '1' means: node data (as opposed
				   // to cell data, which we do not
				   // support explicitly here)
  for (unsigned int data_set=0; data_set<n_data_sets; ++data_set)
    {
      out << data_names[data_set] << " 1" << '\n';
      std::copy (data_vectors[data_set].begin(),
		 data_vectors[data_set].end(),
		 std::ostream_iterator<double>(out, "\n"));
      out << '\n'
	  << '\n';
    };


  
				   // end of variable section
  out << "endvars" << '\n';
  
				   // end of output
  out << "endgmv"
      << '\n';
  
				   // assert the stream is still ok
  AssertThrow (out, ExcIO());
}



template <int dim, int spacedim>
void DataOutBase::write_tecplot (const std::vector<Patch<dim,spacedim> > &patches,
				 const std::vector<std::string>          &data_names,
				 const TecplotFlags                      &/*flags*/,
				 std::ostream                            &out)
{
  AssertThrow (out, ExcIO());

  Assert (patches.size() > 0, ExcNoPatches());
 
  const unsigned int n_data_sets = data_names.size();
				   // check against # of data sets in
				   // first patch. checks against all
				   // other patches are made in
				   // write_gmv_reorder_data_vectors
  Assert (n_data_sets == patches[0].data.n_rows(),
	  ExcUnexpectedNumberOfDatasets (patches[0].data.n_rows(), n_data_sets));
  

  
				   // first count the number of cells
				   // and cells for later use
  unsigned int n_cells = 0,
               n_nodes = 0;
  for (typename std::vector<Patch<dim,spacedim> >::const_iterator patch=patches.begin();
       patch!=patches.end(); ++patch)
    switch (dim)
      {
	case 1:
	      n_cells += patch->n_subdivisions;
	      n_nodes += patch->n_subdivisions+1;
	      break;
	case 2:
	      n_cells += patch->n_subdivisions *
			 patch->n_subdivisions;
	      n_nodes += (patch->n_subdivisions+1) *
			 (patch->n_subdivisions+1);
	      break;
	case 3:
	      n_cells += patch->n_subdivisions *
			 patch->n_subdivisions *
			 patch->n_subdivisions;
	      n_nodes += (patch->n_subdivisions+1) *
			 (patch->n_subdivisions+1) *
			 (patch->n_subdivisions+1);
	      break;
	default:
	      Assert (false, ExcNotImplemented());
      };

  


				   ///////////
				   // preamble
  {
    out << "Variables=";

    switch (dim)
      {
        case 1:
	      out << "\"x\"";
	      break;
        case 2:
	      out << "\"x\", \"y\"";
	      break;
        case 3:
	      out << "\"x\", \"y\", \"z\"";
	      break;
	default:
	      Assert (false, ExcNotImplemented());
      };
    
    for (unsigned int data_set=0; data_set<n_data_sets; ++data_set)
      {
	out << ", \"" << data_names[data_set] << "\"";
      };
    
    out << '\n';
    
    if (dim > 1)
      {
	out << "zone f=feblock, n=" << n_nodes << ", e=" << n_cells << ", et=";
	
	switch (dim)
	  {
	  case 2:
	    out << "quadrilateral" << '\n';
	    break;
	  case 3:
	    out << "brick" << '\n';
	    break;
	  default:
	    Assert (false, ExcNotImplemented());
	  };
      }
    else
      {
	out << "zone f=block, n=" << n_nodes << '\n';
      };
  };

  
                                   // in Tecplot FEBLOCK format the vertex
                                   // coordinates and the data have an
                                   // order that is a bit unpleasant
                                   // (first all x coordinates, then
                                   // all y coordinate, ...; first all
                                   // data of variable 1, then
                                   // variable 2, etc), so we have to
                                   // copy the data vectors a bit around
                                   //
                                   // note that we copy vectors when
                                   // looping over the patches since we
                                   // have to write them one variable
                                   // at a time and don't want to use
                                   // more than one loop
                                   //
                                   // this copying of data vectors can
                                   // be done while we already output
                                   // the vertices, so do this on a
                                   // separate thread and when wanting
                                   // to write out the data, we wait
                                   // for that thread to finish
  
  Table<2,double> data_vectors (n_data_sets, n_nodes);

  void (*fun_ptr) (const std::vector<Patch<dim,spacedim> > &,
                   Table<2,double> &)
    = &DataOutBase::template write_gmv_reorder_data_vectors<dim,spacedim>;
  Threads::Thread<> reorder_thread = Threads::spawn (fun_ptr)(patches, data_vectors);

                                   ///////////////////////////////
                                   // first make up a list of used
                                   // vertices along with their
                                   // coordinates

  
  for (unsigned int d=1; d<=spacedim; ++d)
    {       
          
      for (typename std::vector<Patch<dim,spacedim> >::const_iterator patch=patches.begin();
           patch!=patches.end(); ++patch)
        {
          const unsigned int n_subdivisions = patch->n_subdivisions;
	  
          switch (dim)
            {
              case 1:
              {
                for (unsigned int i=0; i<n_subdivisions+1; ++i)
                  out << ((patch->vertices[1](0) * i / n_subdivisions) +
                          (patch->vertices[0](0) * (n_subdivisions-i) / n_subdivisions))
                      << '\n';
                break;
              };
	      
              case 2:
              {
                for (unsigned int i=0; i<n_subdivisions+1; ++i)
                  for (unsigned int j=0; j<n_subdivisions+1; ++j)
                    {
                      const double x_frac = i * 1./n_subdivisions,
                                   y_frac = j * 1./n_subdivisions;
		      
                                                       // compute coordinates for
                                                       // this patch point

                      out << (((patch->vertices[1](d-1) * x_frac) +
                               (patch->vertices[0](d-1) * (1-x_frac))) * (1-y_frac) +
                              ((patch->vertices[2](d-1) * x_frac) +
                               (patch->vertices[3](d-1) * (1-x_frac))) * y_frac)
                          << '\n';
                    };
                break;
              };
	      
              case 3:
              {
                for (unsigned int i=0; i<n_subdivisions+1; ++i)
                  for (unsigned int j=0; j<n_subdivisions+1; ++j)
                    for (unsigned int k=0; k<n_subdivisions+1; ++k)
                      {
                                                         // note the broken
                                                         // design of hexahedra
                                                         // in deal.II, where
                                                         // first the z-component
                                                         // is counted up, before
                                                         // increasing the y-
                                                         // coordinate
                        const double x_frac = i * 1./n_subdivisions,
                                     y_frac = k * 1./n_subdivisions,
                                     z_frac = j * 1./n_subdivisions;
			
                                                         // compute coordinates for
                                                         // this patch point
			 
                        out << ((((patch->vertices[1](d-1) * x_frac) +
                                  (patch->vertices[0](d-1) * (1-x_frac))) * (1-y_frac) +
                                 ((patch->vertices[2](d-1) * x_frac) +
                                  (patch->vertices[3](d-1) * (1-x_frac))) * y_frac)   * (1-z_frac) +
                                (((patch->vertices[5](d-1) * x_frac) +
                                  (patch->vertices[4](d-1) * (1-x_frac))) * (1-y_frac) +
                                 ((patch->vertices[6](d-1) * x_frac) +
                                  (patch->vertices[7](d-1) * (1-x_frac))) * y_frac)   * z_frac)
                            << '\n';
                      };
                break;
              };
	      
              default:
                    Assert (false, ExcNotImplemented());
            };
        };
      out << '\n';
    };


                                   ///////////////////////////////////////
                                   // data output.
                                   //
                                   // now write the data vectors to
                                   // @p{out} first make sure that all
                                   // data is in place
  reorder_thread.join ();

                                   // then write data.
  for (unsigned int data_set=0; data_set<n_data_sets; ++data_set)
    {
       
      std::copy (data_vectors[data_set].begin(),
		 data_vectors[data_set].end(),
		 std::ostream_iterator<double>(out, "\n"));
      out << '\n';
    };

  
                                   /////////////////////////////////
                                   // now for the cells. note that
                                   // vertices are counted from 1 onwards

  unsigned int first_vertex_of_patch = 0;
      
  for (typename std::vector<Patch<dim,spacedim> >::const_iterator patch=patches.begin();
       patch!=patches.end(); ++patch)
    {
      const unsigned int n_subdivisions = patch->n_subdivisions;
      
                                       // write out the cells making
                                       // up this patch
      switch (dim)
        {
          case 1:
          {
            break;
          };
          case 2:
          {
            for (unsigned int i=0; i<n_subdivisions; ++i)
              for (unsigned int j=0; j<n_subdivisions; ++j)
                {

                  out << first_vertex_of_patch+i*(n_subdivisions+1)+j+1 << ' '
                      << first_vertex_of_patch+(i+1)*(n_subdivisions+1)+j+1 << ' '
                      << first_vertex_of_patch+(i+1)*(n_subdivisions+1)+j+1+1 << ' '
                      << first_vertex_of_patch+i*(n_subdivisions+1)+j+1+1
                      << '\n';
                };
            break;
          };
	      
          case 3:
          {
            for (unsigned int i=0; i<n_subdivisions; ++i)
              for (unsigned int j=0; j<n_subdivisions; ++j)
                for (unsigned int k=0; k<n_subdivisions; ++k)
                  {
                                                     // note: vertex indices start with 1!
		     
                    out << first_vertex_of_patch+(i*(n_subdivisions+1)+j      )*(n_subdivisions+1)+k  +1 << ' '
                        << first_vertex_of_patch+((i+1)*(n_subdivisions+1)+j  )*(n_subdivisions+1)+k  +1 << ' '
                        << first_vertex_of_patch+((i+1)*(n_subdivisions+1)+j+1)*(n_subdivisions+1)+k  +1 << ' '
                        << first_vertex_of_patch+(i*(n_subdivisions+1)+j+1    )*(n_subdivisions+1)+k  +1 << ' '
                        << first_vertex_of_patch+(i*(n_subdivisions+1)+j      )*(n_subdivisions+1)+k+1+1 << ' '
                        << first_vertex_of_patch+((i+1)*(n_subdivisions+1)+j  )*(n_subdivisions+1)+k+1+1 << ' '
                        << first_vertex_of_patch+((i+1)*(n_subdivisions+1)+j+1)*(n_subdivisions+1)+k+1+1 << ' '
                        << first_vertex_of_patch+(i*(n_subdivisions+1)+j+1    )*(n_subdivisions+1)+k+1+1 << ' '
                        << '\n';
                  };
            break;
          };

          default:
                Assert (false, ExcNotImplemented());
        };


                                       // finally update the number
                                       // of the first vertex of this patch
      switch (dim)
        {
          case 1:
                first_vertex_of_patch += n_subdivisions+1;
                break;
          case 2:
                first_vertex_of_patch += (n_subdivisions+1) *
                                         (n_subdivisions+1);
                break;
          case 3:
                first_vertex_of_patch += (n_subdivisions+1) *
                                         (n_subdivisions+1) *
                                         (n_subdivisions+1);
                break;
          default:
                Assert (false, ExcNotImplemented());
        };
       
    };

  
                                   // assert the stream is still ok
  AssertThrow (out, ExcIO());
}



//--------------------------------------------------------
// Macros for handling Tecplot API data

//--------------------------------------------------------



template <int dim, int spacedim>
void DataOutBase::write_tecplot_binary (const std::vector<Patch<dim,spacedim> > &patches,
					const std::vector<std::string>          &data_names,
					const TecplotFlags                      &flags,
					std::ostream                            &out)
{
  
                                   // simply call the ASCII output
                                   // function if the Tecplot API
                                   // isn't present
  write_tecplot (patches, data_names, flags, out);
  return;
}



template <int dim, int spacedim>
void DataOutBase::write_vtk (const std::vector<Patch<dim,spacedim> > &patches,
			     const std::vector<std::string>          &data_names,
			     const VtkFlags                          &/*flags*/,
			     std::ostream                            &out) 
{
  AssertThrow (out, ExcIO());

  Assert (patches.size() > 0, ExcNoPatches());
 
  const unsigned int n_data_sets = data_names.size();
				   // check against # of data sets in
				   // first patch. checks against all
				   // other patches are made in
				   // write_gmv_reorder_data_vectors
  Assert (n_data_sets == patches[0].data.n_rows(),
	  ExcUnexpectedNumberOfDatasets (patches[0].data.n_rows(), n_data_sets));
  
  

				   // first count the number of cells
				   // and cells for later use
  unsigned int n_cells = 0,
	       n_nodes = 0;
  for (typename std::vector<Patch<dim,spacedim> >::const_iterator patch=patches.begin();
       patch!=patches.end(); ++patch)
    switch (dim)
      {
	case 1:
	      n_cells += patch->n_subdivisions;
	      n_nodes += patch->n_subdivisions+1;
	      break;
	case 2:
	      n_cells += patch->n_subdivisions *
			 patch->n_subdivisions;
	      n_nodes += (patch->n_subdivisions+1) *
			 (patch->n_subdivisions+1);
	      break;
	case 3:
	      n_cells += patch->n_subdivisions *
			 patch->n_subdivisions *
			 patch->n_subdivisions;
	      n_nodes += (patch->n_subdivisions+1) *
			 (patch->n_subdivisions+1) *
			 (patch->n_subdivisions+1);
	      break;
	default:
	      Assert (false, ExcNotImplemented());
      };


				   // in gmv format the vertex
				   // coordinates and the data have an
				   // order that is a bit unpleasant
				   // (first all x coordinates, then
				   // all y coordinate, ...; first all
				   // data of variable 1, then
				   // variable 2, etc), so we have to
				   // copy the data vectors a bit around
				   //
				   // note that we copy vectors when
				   // looping over the patches since we
				   // have to write them one variable
				   // at a time and don't want to use
				   // more than one loop
				   //
				   // this copying of data vectors can
				   // be done while we already output
				   // the vertices, so do this on a
				   // separate thread and when wanting
				   // to write out the data, we wait
				   // for that thread to finish
  Table<2,double> data_vectors (n_data_sets, n_nodes);

  void (*fun_ptr) (const std::vector<Patch<dim,spacedim> > &,
		   Table<2,double> &)
    = &DataOutBase::template write_gmv_reorder_data_vectors<dim,spacedim>;
  Threads::Thread<> reorder_thread = Threads::spawn (fun_ptr)(patches, data_vectors);

				   ///////////////////////////////
				   // first make up a list of used
				   // vertices along with their
				   // coordinates
				   //
				   // note that we have to print
				   // d=1..3 dimensions
  out << "POINTS " << n_nodes << " double" << '\n';
  for (typename std::vector<Patch<dim,spacedim> >::const_iterator patch=patches.begin();
       patch!=patches.end(); ++patch)
    {
      const unsigned int n_subdivisions = patch->n_subdivisions;
      
      switch (dim)
	{
	  case 1:
	  {
	    for (unsigned int i=0; i<n_subdivisions+1; ++i)
	      out << ((patch->vertices[1](0) * i / n_subdivisions) +
		      (patch->vertices[0](0) * (n_subdivisions-i) / n_subdivisions))
		  << " 0 0\n";
	    break;
	  };
		   
	  case 2:
	  {
	    for (unsigned int i=0; i<n_subdivisions+1; ++i)
	      for (unsigned int j=0; j<n_subdivisions+1; ++j)
		{
		  const double x_frac = i * 1./n_subdivisions,
			       y_frac = j * 1./n_subdivisions;
		      
		  out << (((patch->vertices[1] * x_frac) +
			   (patch->vertices[0] * (1-x_frac))) * (1-y_frac) +
			  ((patch->vertices[2] * x_frac) +
			   (patch->vertices[3] * (1-x_frac))) * y_frac)
		      << " 0\n";
		};
	    break;
	  };
	       
	  case 3:
	  {
	    for (unsigned int i=0; i<n_subdivisions+1; ++i)
	      for (unsigned int j=0; j<n_subdivisions+1; ++j)
		for (unsigned int k=0; k<n_subdivisions+1; ++k)
		  {
						     // note the broken
						     // design of hexahedra
						     // in deal.II, where
						     // first the z-component
						     // is counted up, before
						     // increasing the y-
						     // coordinate
		    const double x_frac = i * 1./n_subdivisions,
				 y_frac = k * 1./n_subdivisions,
				 z_frac = j * 1./n_subdivisions;
			
						     // compute coordinates for
						     // this patch point
		    out << ((((patch->vertices[1] * x_frac) +
			      (patch->vertices[0] * (1-x_frac))) * (1-y_frac) +
			     ((patch->vertices[2] * x_frac) +
			      (patch->vertices[3] * (1-x_frac))) * y_frac)   * (1-z_frac) +
			    (((patch->vertices[5] * x_frac) +
			      (patch->vertices[4] * (1-x_frac))) * (1-y_frac) +
			     ((patch->vertices[6] * x_frac) +
			      (patch->vertices[7] * (1-x_frac))) * y_frac)   * z_frac)
			<< '\n';
		  };
		
	    break;
	  };
	       
	  default:
		Assert (false, ExcNotImplemented());
	};
    };

				   /////////////////////////////////
				   // now for the cells
  if (true)
    {
      out << "CELLS " << n_cells << ' '
	  << n_cells*(GeometryInfo<dim>::vertices_per_cell+1)
	  << '\n';


      unsigned int first_vertex_of_patch = 0;
      
      for (typename std::vector<Patch<dim,spacedim> >::const_iterator patch=patches.begin();
	   patch!=patches.end(); ++patch)
	{
	  const unsigned int n_subdivisions = patch->n_subdivisions;

					   // write out the cells making
					   // up this patch
	  switch (dim)
	    {
	      case 1:
	      {
		for (unsigned int i=0; i<n_subdivisions; ++i)
		  out << "2 "
		      << first_vertex_of_patch+i << ' '
		      << first_vertex_of_patch+i+1 << '\n';
		break;
	      };
	       
	      case 2:
	      {
		for (unsigned int i=0; i<n_subdivisions; ++i)
		  for (unsigned int j=0; j<n_subdivisions; ++j)
		    out << "4 "
			<< first_vertex_of_patch+i*(n_subdivisions+1)+j << ' '
			<< first_vertex_of_patch+(i+1)*(n_subdivisions+1)+j << ' '
			<< first_vertex_of_patch+(i+1)*(n_subdivisions+1)+j+1 << ' '
			<< first_vertex_of_patch+i*(n_subdivisions+1)+j+1
			<< '\n';
		break;
	      };
	       
	      case 3:
	      {
		for (unsigned int i=0; i<n_subdivisions; ++i)
		  for (unsigned int j=0; j<n_subdivisions; ++j)
		    for (unsigned int k=0; k<n_subdivisions; ++k)
		      {
			out << "8 "
							   // note: vertex indices start with 1!
			    << first_vertex_of_patch+(i*(n_subdivisions+1)+j      )*(n_subdivisions+1)+k   << ' '
			    << first_vertex_of_patch+((i+1)*(n_subdivisions+1)+j  )*(n_subdivisions+1)+k   << ' '
			    << first_vertex_of_patch+((i+1)*(n_subdivisions+1)+j+1)*(n_subdivisions+1)+k   << ' '
			    << first_vertex_of_patch+(i*(n_subdivisions+1)+j+1    )*(n_subdivisions+1)+k   << ' '
			    << first_vertex_of_patch+(i*(n_subdivisions+1)+j      )*(n_subdivisions+1)+k+1 << ' '
			    << first_vertex_of_patch+((i+1)*(n_subdivisions+1)+j  )*(n_subdivisions+1)+k+1 << ' '
			    << first_vertex_of_patch+((i+1)*(n_subdivisions+1)+j+1)*(n_subdivisions+1)+k+1 << ' '
			    << first_vertex_of_patch+(i*(n_subdivisions+1)+j+1    )*(n_subdivisions+1)+k+1 << ' '
			    << '\n';
		      };
		break;
	      };

	      default:
		    Assert (false, ExcNotImplemented());
	    };


					   // finally update the number
					   // of the first vertex of this patch
	  switch (dim)
	    {
	      case 1:
		    first_vertex_of_patch += n_subdivisions+1;
		    break;
	      case 2:
		    first_vertex_of_patch += (n_subdivisions+1) *
					     (n_subdivisions+1);
		    break;
	      case 3:
		    first_vertex_of_patch += (n_subdivisions+1) *
					     (n_subdivisions+1) *
					     (n_subdivisions+1);
		    break;
	      default:
		    Assert (false, ExcNotImplemented());
	    };
	};

				       // next output the types of the
				       // cells. since all cells are
				       // the same, this is simple
      out << "CELL_TYPES " << n_cells << '\n';
      for (unsigned int i=0; i<n_cells; ++i)
	switch (dim)
	  {
	    case 1:
		  out << "3\n";    // represents VTK_LINE
		  break;
	    case 2:
		  out << "9\n";    // represents VTK_QUAD
		  break;
	    case 3:
		  out << "12\n";    // represents VTK_HEXAHEDRON
		  break;
	    default:
		  Assert (false, ExcNotImplemented());
	  };
    };

				   ///////////////////////////////////////
				   // data output.

				   // now write the data vectors to
				   // @p{out} first make sure that all
				   // data is in place
  reorder_thread.join ();

				   // then write data.  the
				   // 'POINTD_DATA' means: node data
				   // (as opposed to cell data, which
				   // we do not support explicitly
				   // here). all following data sets
				   // are point data
  out << "POINT_DATA " << n_nodes
      << '\n';
  for (unsigned int data_set=0; data_set<n_data_sets; ++data_set)
    {
      out << "SCALARS "
	  << data_names[data_set]
	  << " double 1"
	  << '\n'
	  << "LOOKUP_TABLE default"
	  << '\n';
      std::copy (data_vectors[data_set].begin(),
		 data_vectors[data_set].end(),
		 std::ostream_iterator<double>(out, " "));
      out << '\n';
    };
  
				   // assert the stream is still ok
  AssertThrow (out, ExcIO());
}




template <int dim, int spacedim>
void
DataOutBase::write_gmv_reorder_data_vectors (const std::vector<Patch<dim,spacedim> > &patches,
					     Table<2,double>                         &data_vectors)
{
				   // unlike in the main function, we
				   // don't have here the data_names
				   // field, so we initialize it with
				   // the number of data sets in the
				   // first patch. the equivalence of
				   // these two definitions is checked
				   // in the main function.
  const unsigned int n_data_sets = patches[0].data.n_rows();

  Assert (data_vectors.size()[0] == n_data_sets,
	  ExcInternalError());
  
				   // loop over all patches
  unsigned int next_value = 0;
  for (typename std::vector<Patch<dim,spacedim> >::const_iterator patch=patches.begin();
       patch != patches.end(); ++patch)
    {
      const unsigned int n_subdivisions = patch->n_subdivisions;
	  
      Assert (patch->data.n_rows() == n_data_sets,
	      ExcUnexpectedNumberOfDatasets (patch->data.n_rows(), n_data_sets));
      Assert (patch->data.n_cols() == (dim==1 ?
				  n_subdivisions+1 :
				  (dim==2 ?
				   (n_subdivisions+1)*(n_subdivisions+1) :
				   (dim==3 ?
				    (n_subdivisions+1)*(n_subdivisions+1)*(n_subdivisions+1) :
				    0))),
	      ExcInvalidDatasetSize (patch->data.n_cols(), n_subdivisions+1));
	  
      switch (dim)
	{
	  case 1:
	  {      
	    for (unsigned int i=0; i<n_subdivisions+1; ++i, ++next_value) 
	      for (unsigned int data_set=0; data_set<n_data_sets; ++data_set)
		data_vectors[data_set][next_value] = patch->data(data_set,i);
		
	    break;
	  };
		     
	  case 2:
	  {
	    for (unsigned int i=0; i<n_subdivisions+1; ++i)
	      for (unsigned int j=0; j<n_subdivisions+1; ++j)
		{
		  for (unsigned int data_set=0; data_set<n_data_sets; ++data_set)
		    data_vectors[data_set][next_value]
		      = patch->data(data_set,i*(n_subdivisions+1) + j);
		  ++next_value;
		};
		
	    break;
	  };
	       
	  case 3:
	  {
	    for (unsigned int i=0; i<n_subdivisions+1; ++i)
	      for (unsigned int j=0; j<n_subdivisions+1; ++j)
		for (unsigned int k=0; k<n_subdivisions+1; ++k)
		  {
		    for (unsigned int data_set=0; data_set<n_data_sets; ++data_set)
		      data_vectors[data_set][next_value]
			= patch->data(data_set,
				      (i*(n_subdivisions+1)+j)*(n_subdivisions+1)+k);
		    ++next_value;
		  };

	    break;
	  };
	       
	  default:
		Assert (false, ExcNotImplemented());
	};
    };

  for (unsigned int data_set=0; data_set<n_data_sets; ++data_set)
    Assert (data_vectors[data_set].size() == next_value,
	    ExcInternalError());
}



/* --------------------------- class DataOutInterface ---------------------- */


template <int dim, int spacedim>
void DataOutInterface<dim,spacedim>::write_dx (std::ostream &out) const 
{
  DataOutBase::write_dx (get_patches(), get_dataset_names(),
			 dx_flags, out);
}



template <int dim, int spacedim>
void DataOutInterface<dim,spacedim>::write_ucd (std::ostream &out) const 
{
  DataOutBase::write_ucd (get_patches(), get_dataset_names(),
			  ucd_flags, out);
}



template <int dim, int spacedim>
void DataOutInterface<dim,spacedim>::write_gnuplot (std::ostream &out) const 
{
  DataOutBase::write_gnuplot (get_patches(), get_dataset_names(),
			      gnuplot_flags, out);
}



template <int dim, int spacedim>
void DataOutInterface<dim,spacedim>::write_povray (std::ostream &out) const 
{
  DataOutBase::write_povray (get_patches(), get_dataset_names(),
			     povray_flags, out);
}



template <int dim, int spacedim>
void DataOutInterface<dim,spacedim>::write_eps (std::ostream &out) const 
{
  DataOutBase::write_eps (get_patches(), get_dataset_names(),
			  eps_flags, out);
}



template <int dim, int spacedim>
void DataOutInterface<dim,spacedim>::write_gmv (std::ostream &out) const 
{
  DataOutBase::write_gmv (get_patches(), get_dataset_names(),
			  gmv_flags, out);
}



template <int dim, int spacedim>
void DataOutInterface<dim,spacedim>::write_tecplot (std::ostream &out) const 
{
  DataOutBase::write_tecplot (get_patches(), get_dataset_names(),
			      tecplot_flags, out);
}



template <int dim, int spacedim>
void DataOutInterface<dim,spacedim>::write_tecplot_binary (std::ostream &out) const 
{
  DataOutBase::write_tecplot_binary (get_patches(), get_dataset_names(),
				     tecplot_flags, out);
}



template <int dim, int spacedim>
void DataOutInterface<dim,spacedim>::write_vtk (std::ostream &out) const 
{
  DataOutBase::write_vtk (get_patches(), get_dataset_names(),
			  vtk_flags, out);
}



template <int dim, int spacedim>
void
DataOutInterface<dim,spacedim>::write (std::ostream &out,
				       const OutputFormat output_format_) const
{
  OutputFormat output_format = output_format_;
  if (output_format == default_format)
    output_format = default_fmt;
  
  switch (output_format) 
    {
      case dx:
	    write_dx (out);
	    break;
	    
      case ucd:
	    write_ucd (out);
	    break;
	    
      case gnuplot:
	    write_gnuplot (out);
	    break;
	    
      case povray:
	    write_povray (out);
	    break;
	    
      case eps:
	    write_eps(out);
	    break;
	    
      case gmv:
	    write_gmv (out);
	    break;
	    
      case tecplot:
	    write_tecplot (out);
	    break;
	    
      case tecplot_binary:
	    write_tecplot_binary (out);
	    break;
	    
      case vtk:
	    write_vtk (out);
	    break;
	    
      default:
	    Assert (false, ExcNotImplemented());
    };
}



template <int dim, int spacedim>
void
DataOutInterface<dim,spacedim>::set_default_format(const OutputFormat fmt)
{
  Assert (fmt != default_format, ExcNotImplemented());
  default_fmt = fmt;
}



template <int dim, int spacedim>
void
DataOutInterface<dim,spacedim>::set_flags (const DXFlags &flags) 
{
  dx_flags = flags;
}



template <int dim, int spacedim>
void
DataOutInterface<dim,spacedim>::set_flags (const UcdFlags &flags) 
{
  ucd_flags = flags;
}



template <int dim, int spacedim>
void
DataOutInterface<dim,spacedim>::set_flags (const GnuplotFlags &flags) 
{
  gnuplot_flags = flags;
}



template <int dim, int spacedim>
void
DataOutInterface<dim,spacedim>::set_flags (const PovrayFlags &flags) 
{
  povray_flags = flags;
}



template <int dim, int spacedim>
void
DataOutInterface<dim,spacedim>::set_flags (const EpsFlags &flags) 
{
  eps_flags = flags;
}



template <int dim, int spacedim>
void
DataOutInterface<dim,spacedim>::set_flags (const GmvFlags &flags) 
{
  gmv_flags = flags;
}



template <int dim, int spacedim>
void
DataOutInterface<dim,spacedim>::set_flags (const TecplotFlags &flags) 
{
  tecplot_flags = flags;
}



template <int dim, int spacedim>
void
DataOutInterface<dim,spacedim>::set_flags (const VtkFlags &flags) 
{
  vtk_flags = flags;
}



template <int dim, int spacedim>
std::string
DataOutInterface<dim,spacedim>::
default_suffix (const OutputFormat output_format_) const
{
  OutputFormat output_format = output_format_;
  if (output_format == default_format)
    output_format = default_fmt;
  
  switch (output_format) 
    {
      case dx:
	    return ".dx";
	    
      case ucd:
	    return ".inp";
	    
      case gnuplot: 
	    return ".gnuplot";
	    
      case povray: 
	    return ".pov";
	    
      case eps: 
	    return ".eps";

      case gmv:
	    return ".gmv";

      case tecplot:
	    return ".dat";
	    
      case tecplot_binary:
	    return ".plt";
	    
      case vtk:
	    return ".vtk";
	    
      default: 
	    Assert (false, ExcNotImplemented()); 
	    return "";
    };
}



template <int dim, int spacedim>
typename DataOutInterface<dim,spacedim>::OutputFormat
DataOutInterface<dim,spacedim>::
parse_output_format (const std::string &format_name)
{
  if (format_name == "dx")
    return dx;

  if (format_name == "ucd")
    return ucd;

  if (format_name == "gnuplot")
    return gnuplot;

  if (format_name == "povray")
    return povray;

  if (format_name == "eps")
    return eps;

  if (format_name == "gmv")
    return gmv;

  if (format_name == "tecplot")
    return tecplot;
  
  if (format_name == "tecplot_binary")
    return tecplot_binary;
  
  if (format_name == "vtk")
    return vtk;
  
  AssertThrow (false, ExcInvalidState ());

				   // return something invalid
  return OutputFormat(-1);
}



template <int dim, int spacedim>
std::string
DataOutInterface<dim,spacedim>::get_output_format_names ()
{
  return "dx|ucd|gnuplot|povray|eps|gmv|tecplot|vtk";
}



template <int dim, int spacedim>
void
DataOutInterface<dim,spacedim>::declare_parameters (ParameterHandler &prm) 
{
  prm.declare_entry ("Output format", "gnuplot",
		     Patterns::Selection (get_output_format_names ()));

  prm.enter_subsection ("DX output parameters");
  DXFlags::declare_parameters (prm);
  prm.leave_subsection ();
  
  prm.enter_subsection ("UCD output parameters");
  UcdFlags::declare_parameters (prm);
  prm.leave_subsection ();
  
  prm.enter_subsection ("Gnuplot output parameters");
  GnuplotFlags::declare_parameters (prm);
  prm.leave_subsection ();

  prm.enter_subsection ("Povray output parameters");
  PovrayFlags::declare_parameters (prm);
  prm.leave_subsection ();

  prm.enter_subsection ("Eps output parameters");
  EpsFlags::declare_parameters (prm);
  prm.leave_subsection ();

  prm.enter_subsection ("Gmv output parameters");
  GmvFlags::declare_parameters (prm);
  prm.leave_subsection ();

  prm.enter_subsection ("Tecplot output parameters");
  TecplotFlags::declare_parameters (prm);
  prm.leave_subsection ();

  prm.enter_subsection ("Vtk output parameters");
  VtkFlags::declare_parameters (prm);
  prm.leave_subsection ();
}



template <int dim, int spacedim>
void
DataOutInterface<dim,spacedim>::parse_parameters (ParameterHandler &prm) 
{
  const std::string& output_name = prm.get ("Output format");
  default_fmt = parse_output_format (output_name);

  prm.enter_subsection ("DX output parameters");
  dx_flags.parse_parameters (prm);
  prm.leave_subsection ();
  
  prm.enter_subsection ("UCD output parameters");
  ucd_flags.parse_parameters (prm);
  prm.leave_subsection ();
  
  prm.enter_subsection ("Gnuplot output parameters");
  gnuplot_flags.parse_parameters (prm);
  prm.leave_subsection ();

  prm.enter_subsection ("Povray output parameters");
  povray_flags.parse_parameters (prm);
  prm.leave_subsection ();

  prm.enter_subsection ("Eps output parameters");
  eps_flags.parse_parameters (prm);
  prm.leave_subsection ();

  prm.enter_subsection ("Gmv output parameters");
  gmv_flags.parse_parameters (prm);
  prm.leave_subsection ();

  prm.enter_subsection ("Tecplot output parameters");
  tecplot_flags.parse_parameters (prm);
  prm.leave_subsection ();

  prm.enter_subsection ("Vtk output parameters");
  vtk_flags.parse_parameters (prm);
  prm.leave_subsection ();
}



template <int dim, int spacedim>
unsigned int 
DataOutInterface<dim,spacedim>::memory_consumption () const
{
  return (sizeof (default_fmt) +
	  MemoryConsumption::memory_consumption (dx_flags) +
	  MemoryConsumption::memory_consumption (ucd_flags) +
	  MemoryConsumption::memory_consumption (gnuplot_flags) +
	  MemoryConsumption::memory_consumption (povray_flags) +
	  MemoryConsumption::memory_consumption (eps_flags) +
	  MemoryConsumption::memory_consumption (gmv_flags) +
	  MemoryConsumption::memory_consumption (tecplot_flags) +
	  MemoryConsumption::memory_consumption (vtk_flags));
}




// explicit instantiations
#ifdef __IBMCPP__
template const unsigned int DataOutBase::Patch<1,1>::no_neighbor;
template const unsigned int DataOutBase::Patch<2,2>::no_neighbor;
template const unsigned int DataOutBase::Patch<3,3>::no_neighbor;
template const unsigned int DataOutBase::Patch<4,4>::no_neighbor;
#endif


template class DataOutInterface<1,1>;
template class DataOutBase::Patch<1,1>;

template class DataOutInterface<2,2>;
template class DataOutBase::Patch<2,2>;

template class DataOutInterface<3,3>;
template class DataOutBase::Patch<3,3>;

template class DataOutInterface<4,4>;
template class DataOutBase::Patch<4,4>;

// plotting surfaces
template class DataOutInterface<1,2>;
template class DataOutBase::Patch<1,2>;

template class DataOutInterface<2,3>;
template class DataOutBase::Patch<2,3>;

template class DataOutInterface<3,4>;
template class DataOutBase::Patch<3,4>;

