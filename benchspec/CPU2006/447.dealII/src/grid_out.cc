//----------------------------  grid_out.cc  ---------------------------
//    $Id: grid_out.cc,v 1.4 2006/01/23 23:49:36 wolf Exp $
//    Version: $Name:  $
//
//    Copyright (C) 1998, 1999, 2000, 2001, 2002, 2003, 2004 by the deal.II authors
//
//    This file is subject to QPL and may not be  distributed
//    without copyright and license information. Please refer
//    to the file deal.II/doc/license.html for the  text  and
//    further information on this license.
//
//----------------------------  grid_out.cc  ---------------------------


#include <base/point.h>
#include <base/quadrature.h>
#include <grid/grid_out.h>
#include <grid/tria.h>
#include <grid/tria_accessor.h>
#include <grid/tria_iterator.h>
#include <fe/mapping.h>

#include <iomanip>
#include <algorithm>
#include <list>
#include <set>
#include <cmath>


#if deal_II_dimension == 1


template <int dim>
void GridOut::write_dx (const Triangulation<dim> &,
			std::ostream             &)
{
  Assert (false, ExcNotImplemented());
}

#else


template <int dim>
void GridOut::write_dx (const Triangulation<dim> &tria,
			std::ostream             &out) 
{
//TODO:[GK] allow for boundary faces only  
  Assert(dx_flags.write_all_faces, ExcNotImplemented());
  AssertThrow (out, ExcIO());
				   // Copied and adapted from write_ucd
  const std::vector<Point<dim> > &vertices    = tria.get_vertices();
  const std::vector<bool>        &vertex_used = tria.get_used_vertices();
  
  const unsigned int n_vertices = tria.n_used_vertices();

  typename Triangulation<dim>::active_cell_iterator       cell;
  const typename Triangulation<dim>::active_cell_iterator endc=tria.end();

  
				   // write the vertices
  out << "object \"vertices\" class array type float rank 1 shape " << dim
      << " items " << n_vertices << " data follows"
      << '\n';
  
  for (unsigned int i=0; i<vertices.size(); ++i)
    if (vertex_used[i])
      {
	out << '\t' << vertices[i] << '\n';
      };
  
				   // write cells or faces
  const bool write_cells = dx_flags.write_cells;
  const bool write_faces = (dim>1) ? dx_flags.write_faces : false;
  
  const unsigned int n_cells = tria.n_active_cells();
  const unsigned int n_faces = tria.n_active_cells()
			       * GeometryInfo<dim>::faces_per_cell;

  const unsigned int n_vertices_per_cell = GeometryInfo<dim>::vertices_per_cell;
  const unsigned int n_vertices_per_face = GeometryInfo<dim>::vertices_per_face;
  
  if (write_cells)
    {
      out << "object \"cells\" class array type int rank 1 shape "
	  << n_vertices_per_cell
	  << " items " << n_cells << " data follows" << '\n';
      
      for (cell = tria.begin_active(); cell != endc; ++cell)
	{
	  for (unsigned int v=0; v<GeometryInfo<dim>::vertices_per_cell; ++v)
	    out << '\t' << cell->vertex_index(GeometryInfo<dim>::dx_to_deal[v]);
	  out << '\n';
	}
      out << "attribute \"element type\" string \"";
      if (dim==1) out << "lines";
      if (dim==2) out << "quads";
      if (dim==3) out << "cubes";
      out << "\"" << '\n'
	  << "attribute \"ref\" string \"positions\"" << '\n' << '\n';

				       // Additional cell information
      
      out << "object \"material\" class array type int rank 0 items "
	  << n_cells << " data follows" << '\n';
      for (cell = tria.begin_active(); cell != endc; ++cell)
	out << ' ' << (unsigned int)cell->material_id();
      out  << '\n'
	   << "attribute \"dep\" string \"connections\"" << '\n' << '\n';
      
      out << "object \"level\" class array type int rank 0 items "
	  << n_cells << " data follows" << '\n';
      for (cell = tria.begin_active(); cell != endc; ++cell)
	out << ' ' << cell->level();
      out  << '\n'
	   << "attribute \"dep\" string \"connections\"" << '\n' << '\n';
      
      if (dx_flags.write_measure)
	{
	  out << "object \"measure\" class array type float rank 0 items "
	      << n_cells << " data follows" << '\n';
	  for (cell = tria.begin_active(); cell != endc; ++cell)
	    out << '\t' << cell->measure();
	  out  << '\n'
	       << "attribute \"dep\" string \"connections\"" << '\n' << '\n';
	}
      
      if (dx_flags.write_diameter)
	{
	  out << "object \"diameter\" class array type float rank 0 items "
	      << n_cells << " data follows" << '\n';
	  for (cell = tria.begin_active(); cell != endc; ++cell)
	    out << '\t' << cell->diameter();
	  out  << '\n'
	       << "attribute \"dep\" string \"connections\"" << '\n' << '\n';
	}
    }
  
  if (write_faces)
    {
      out << "object \"faces\" class array type int rank 1 shape "
	  << n_vertices_per_face
	  << " items " << n_faces << " data follows"
	  << '\n';

      for (cell = tria.begin_active(); cell != endc; ++cell)
	{
	  for (unsigned int f=0;f<GeometryInfo<dim>::faces_per_cell;++f)
	    {
	      typename Triangulation<dim>::face_iterator face = cell->face(f);
	      
	      for (unsigned int v=0; v<GeometryInfo<dim>::vertices_per_face; ++v)
		out << '\t' << face->vertex_index(GeometryInfo<dim-1>::dx_to_deal[v]);
	      out << '\n';
	    }
	}
      out << "attribute \"element type\" string \"";
      if (dim==2) out << "lines";
      if (dim==3) out << "quads";
      out << "\"" << '\n'
	  << "attribute \"ref\" string \"positions\"" << '\n' << '\n';
      

				       // Additional face information
      
      out << "object \"boundary\" class array type int rank 0 items "
	  << n_faces << " data follows" << '\n';
      for (cell = tria.begin_active(); cell != endc; ++cell)
	{
					   // Little trick to get -1
					   // for the interior
	  for (unsigned int f=0;f<GeometryInfo<dim>::faces_per_cell;++f)
	    out << ' ' << (int)(signed char)cell->face(f)->boundary_indicator();
	  out << '\n';
	}
      out << "attribute \"dep\" string \"connections\"" << '\n' << '\n';
      
      if (dx_flags.write_measure)
	{
	  out << "object \"face measure\" class array type float rank 0 items "
	      << n_faces << " data follows" << '\n';
	  for (cell = tria.begin_active(); cell != endc; ++cell)
	    {
	      for (unsigned int f=0;f<GeometryInfo<dim>::faces_per_cell;++f)
		out << ' ' << cell->face(f)->measure();
	      out << '\n';
	    }
	  out << "attribute \"dep\" string \"connections\"" << '\n' << '\n';
	}

      if (dx_flags.write_diameter)
	{
	  out << "object \"face diameter\" class array type float rank 0 items "
	      << n_faces << " data follows" << '\n';
	  for (cell = tria.begin_active(); cell != endc; ++cell)
	    {
	      for (unsigned int f=0;f<GeometryInfo<dim>::faces_per_cell;++f)
		out << ' ' << cell->face(f)->diameter();
	      out << '\n';
	    }
	  out << "attribute \"dep\" string \"connections\"" << '\n' << '\n';
	}

    }
  

				   // Write additional face information
  
  if (write_faces)
    {
      
    }
  else
    {
     }

				   // The wrapper
  out << "object \"deal data\" class field" << '\n'
      << "component \"positions\" value \"vertices\"" << '\n'
      << "component \"connections\" value \"cells\"" << '\n';

  if (write_cells)
    {
      out << "object \"cell data\" class field" << '\n'
	  << "component \"positions\" value \"vertices\"" << '\n'
	  << "component \"connections\" value \"cells\"" << '\n';
      out << "component \"material\" value \"material\"" << '\n';
      out << "component \"level\" value \"level\"" << '\n';
      if (dx_flags.write_measure)
	out << "component \"measure\" value \"measure\"" << '\n';
      if (dx_flags.write_diameter)
	out << "component \"diameter\" value \"diameter\"" << '\n';
    }

  if (write_faces)
    {
      out << "object \"face data\" class field" << '\n'
	  << "component \"positions\" value \"vertices\"" << '\n'
	  << "component \"connections\" value \"faces\"" << '\n';
      out << "component \"boundary\" value \"boundary\"" << '\n';
      if (dx_flags.write_measure)
	out << "component \"measure\" value \"face measure\"" << '\n';
      if (dx_flags.write_diameter)
	out << "component \"diameter\" value \"face diameter\"" << '\n';
    }
  
  out << '\n'
      << "object \"grid data\" class group" << '\n';
    if (write_cells)
      out << "member \"cells\" value \"cell data\"" << '\n';
    if (write_faces)
      out << "member \"faces\" value \"face data\"" << '\n';
  out << "end" << '\n';  
}


#endif


template <int dim>
void GridOut::write_ucd (const Triangulation<dim> &tria,
			 std::ostream             &out) 
{
  AssertThrow (out, ExcIO());

				   // get the positions of the
				   // vertices and whether they are
				   // used.
  const std::vector<Point<dim> > &vertices    = tria.get_vertices();
  const std::vector<bool>        &vertex_used = tria.get_used_vertices();
  
  const unsigned int n_vertices = tria.n_used_vertices();

  typename Triangulation<dim>::active_cell_iterator       cell=tria.begin_active();
  const typename Triangulation<dim>::active_cell_iterator endc=tria.end();

				   // write preamble
  if (ucd_flags.write_preamble)
    {
				       // block this to have local
				       // variables destroyed after
				       // use
      out << "# This file was generated by the deal.II library." << '\n'
	  << "#" << '\n'
	  << "# For a description of the UCD format see the AVS Developer's guide."
	  << '\n'
	  << "#" << '\n';
    };

				   // start with ucd data
  out << n_vertices << ' '
      << tria.n_active_cells() + (ucd_flags.write_faces ?
				  n_boundary_faces(tria) :
				  0)
      << " 0 0 0"                  // no data
      << '\n';

				   // actually write the vertices.
				   // note that we shall number them
				   // with first index 1 instead of 0
  for (unsigned int i=0; i<vertices.size(); ++i)
    if (vertex_used[i])
      {
	out << i+1                 // vertex index
	    << "  "
	    << vertices[i];
	for (unsigned int d=dim+1; d<=3; ++d)
	  out << " 0";             // fill with zeroes
	out << '\n';
      };
	
				   // write cells. Enumerate cells
				   // consecutively, starting with 1
  unsigned int cell_index=1;
  for (cell=tria.begin_active();
       cell!=endc; ++cell, ++cell_index)
    {
      out << cell_index << ' '
	  << static_cast<unsigned int>(cell->material_id())
	  << " ";
      switch (dim) 
	{
	  case 1:  out << "line    "; break;
	  case 2:  out << "quad    "; break;
	  case 3:  out << "hex     "; break;
	  default:
		Assert (false, ExcNotImplemented());
	};

				       // it follows a list of the
				       // vertices of each cell. in 1d
				       // this is simply a list of the
				       // two vertices, in 2d its counter
				       // clockwise, as usual in this
				       // library. in 3d, the same applies
				       // (special thanks to AVS for
				       // numbering their vertices in a
				       // way compatible to deal.II!)
				       //
				       // technical reference:
				       // AVS Developer's Guide, Release 4,
				       // May, 1992, p. E6
				       //
				       // note: vertex numbers are 1-base
      for (unsigned int vertex=0; vertex<GeometryInfo<dim>::vertices_per_cell;
	   ++vertex)
	out << cell->vertex_index(vertex)+1 << ' ';
      out << '\n';
    };

				   // write faces with non-zero boundary
				   // indicator
  if (ucd_flags.write_faces)
    write_ucd_faces (tria, cell_index, out);
    
  AssertThrow (out, ExcIO());
}


#if deal_II_dimension != 2

template <int dim>
void GridOut::write_xfig (const Triangulation<dim>&,
			  std::ostream&,
			  const Mapping<dim>*)
{
  Assert (false, ExcNotImplemented());
}

#else

//TODO:[GK] Obey parameters
//TODO:[GK] Flip y-axis?
template <int dim>
void GridOut::write_xfig (const Triangulation<dim>& tria,
			  std::ostream&             out,
			  const Mapping<dim>*       /*mapping*/)
{
  const unsigned int nv = GeometryInfo<dim>::vertices_per_cell;
  const unsigned int nf = GeometryInfo<dim>::faces_per_cell;
  const unsigned int nvf = GeometryInfo<dim>::vertices_per_face;

				   // The following text was copied
				   // from an existing XFig file.
  out << "#FIG 3.2\nLandscape\nCenter\nInches" << '\n'
      << "A4\n100.00\nSingle" << '\n'
				     // Background is transparent
      << "-3" << '\n'
      << "1200 2" << '\n';

				   // We write all cells and cells on
				   // coarser levels are behind cells
				   // on finer levels. Level 0
				   // corresponds to a depth of 900,
				   // each level subtracting 1
  typename Triangulation<dim>::cell_iterator cell = tria.begin();
  const typename Triangulation<dim>::cell_iterator end = tria.end();

  for (;cell != end; ++cell)
    {
	// If depth is not encoded, write finest level only
	if (!xfig_flags.level_depth && !cell->active())
	    continue;
				       // Code for polygon
      out << "2 3  "
					 // with black line thickness 1
	  << xfig_flags.line_style
	  <<" 1 0 ";
					 // Fill color
      if (xfig_flags.level_color)
	out << cell->level() + 8;
      else
	out << cell->material_id() + 1;
				       // Depth, unused, fill
      out << ' '
	  << (xfig_flags.level_depth
	      ? (900-cell->level())
	      : (900+cell->material_id()))
	  << " 0 "
	  << xfig_flags.fill_style << " 0.0 "
					 // some style parameters
	  << " 0 0 -1 0 0 "
					 // number of points
	  << nv+1 << '\n';

				       // For each point, write scaled
				       // and shifted coordinates
				       // multiplied by 1200
				       // (dots/inch)
      for (unsigned int k=0;k<=nv;++k)
	{
	  const Point<dim>& p = cell->vertex(k % nv);
	  for (unsigned int d=0;d<dim;++d)
	    {
	      int val = (int)(1200 * xfig_flags.scaling(d) *
			      (p(d)-xfig_flags.offset(d)));
	      out << '\t' << val;
	    }
	  out << '\n';
	}
				       // Now write boundary edges
      if (xfig_flags.draw_boundary)
	for (unsigned int f=0;f<nf;++f)
	  {
	    typename Triangulation<dim>::face_iterator
	      face = cell->face(f);
	    const unsigned char bi = face->boundary_indicator();
	    if (bi != 255)
	      {
						 // Code for polyline
		out << "2 1 "
						   // with line thickness 3
		    << xfig_flags.boundary_style
		    << " 3 ";
		out << (1 + (unsigned int) bi);
						 // Fill color
		out << " -1 ";
						 // Depth, unused, no fill
		out << (xfig_flags.level_depth
			? (800-cell->level())
			: 800+bi)	  
		    << " 0 -1 0.0 "
						   // some style parameters
		    << " 0 0 -1 0 0 "
						   // number of points
		    << nvf << '\n';
		
						 // For each point, write scaled
						 // and shifted coordinates
						 // multiplied by 1200
						 // (dots/inch)
		
		for (unsigned int k=0;k<nvf;++k)
		  {
		    const Point<dim>& p = face->vertex(k % nv);
		    for (unsigned int d=0;d<dim;++d)
		      {
			int val = (int)(1200 * xfig_flags.scaling(d) *
					(p(d)-xfig_flags.offset(d)));
			out << '\t' << val;
		      }
		    out << '\n';
		  } 
	      }
	  }
    }
}

#endif



#if deal_II_dimension == 1

unsigned int GridOut::n_boundary_faces (const Triangulation<1> &) const
{
  return 0;
}

#endif



template <int dim>
unsigned int GridOut::n_boundary_faces (const Triangulation<dim> &tria) const
{
  typename Triangulation<dim>::active_face_iterator face, endf;
  unsigned int n_faces = 0;

  for (face=tria.begin_active_face(), endf=tria.end_face();
       face != endf; ++face)
    if ((face->at_boundary()) &&
	(face->boundary_indicator() != 0))
      n_faces++;

  return n_faces;
}



#if deal_II_dimension == 1

void GridOut::write_ucd_faces (const Triangulation<1> &,
			       const unsigned int,
			       std::ostream &) const
{
  return;
}

#endif



template <int dim>
void GridOut::write_ucd_faces (const Triangulation<dim> &tria,
			       const unsigned int        starting_index,
			       std::ostream             &out) const
{
  typename Triangulation<dim>::active_face_iterator face, endf;
  unsigned int index=starting_index;

  for (face=tria.begin_active_face(), endf=tria.end_face();
       face != endf; ++face)
    if (face->at_boundary() &&
	(face->boundary_indicator() != 0)) 
      {
	out << index << "  "
	    << static_cast<unsigned int>(face->boundary_indicator())
	    << "  ";
	switch (dim) 
	  {
	    case 2: out << "line    ";  break;
	    case 3: out << "quad    ";  break;
	    default:
		  Assert (false, ExcNotImplemented());
	  };
				       // note: vertex numbers are 1-base
	for (unsigned int vertex=0; vertex<GeometryInfo<dim>::vertices_per_face; ++vertex)
	  out << face->vertex_index(vertex)+1 << ' ';
	out << '\n';

	++index;
      };	  
}



#if deal_II_dimension==1

void GridOut::write_gnuplot (const Triangulation<1> &tria,
			     std::ostream           &out,
			     const Mapping<1>       *) 
{
  AssertThrow (out, ExcIO());

  Triangulation<1>::active_cell_iterator        cell=tria.begin_active();
  const Triangulation<1>::active_cell_iterator  endc=tria.end();
  for (; cell!=endc; ++cell)
    {
      if (gnuplot_flags.write_cell_numbers)
	out << "# cell " << cell << '\n';

      out << cell->vertex(0)
	  << ' ' << cell->level()
	  << ' ' << static_cast<unsigned int>(cell->material_id()) << '\n'
	  << cell->vertex(1)
		    << ' ' << cell->level()
	  << ' ' << static_cast<unsigned int>(cell->material_id()) << '\n'
	  << '\n';
      break;
    }
  AssertThrow (out, ExcIO());
}


#else


template <int dim>
void GridOut::write_gnuplot (const Triangulation<dim> &tria,
			     std::ostream             &out,
			     const Mapping<dim>       *mapping) 
{
  AssertThrow (out, ExcIO());

  const unsigned int n_points=
    gnuplot_flags.n_boundary_face_points;

  typename Triangulation<dim>::active_cell_iterator        cell=tria.begin_active();
  const typename Triangulation<dim>::active_cell_iterator  endc=tria.end();

				   // if we are to treat curved
				   // boundaries, then generate a
				   // quadrature formula which will be
				   // used to probe boundary points at
				   // curved faces
  Quadrature<dim> *q_projector=0;
  if (mapping!=0)
    {
      std::vector<Point<dim-1> > boundary_points(n_points);
      for (unsigned int i=0; i<n_points; ++i)
	boundary_points[i](0)= 1.*(i+1)/(n_points+1);

      Quadrature<dim-1> quadrature(boundary_points);
      q_projector = new Quadrature<dim> (QProjector<dim>::project_to_all_faces(quadrature));
    }
  
  for (; cell!=endc; ++cell)
    {
      if (gnuplot_flags.write_cell_numbers)
	out << "# cell " << cell << '\n';

      switch (dim)
	{
	  case 1:
	  {
	    Assert(false, ExcInternalError());
	    break;
	  };
	   
	  case 2:
	  {
	    if (mapping==0 || !cell->at_boundary())
	      {
						 // write out the four
						 // sides of this cell
						 // by putting the
						 // four points (+ the
						 // initial point
						 // again) in a row
						 // and lifting the
						 // drawing pencil at
						 // the end
		out << cell->vertex(0)
		    << ' ' << cell->level()
		    << ' ' << static_cast<unsigned int>(cell->material_id()) << '\n'
		    << cell->vertex(1)
		    << ' ' << cell->level()
		    << ' ' << static_cast<unsigned int>(cell->material_id()) << '\n'
		    << cell->vertex(2)
		    << ' ' << cell->level()
		    << ' ' << static_cast<unsigned int>(cell->material_id()) << '\n'
		    << cell->vertex(3)
		    << ' ' << cell->level()
		    << ' ' << static_cast<unsigned int>(cell->material_id()) << '\n'
		    << cell->vertex(0)
		    << ' ' << cell->level()
		    << ' ' << static_cast<unsigned int>(cell->material_id()) << '\n'
		    << '\n'  // double new line for gnuplot 3d plots
		    << '\n';
	      }
	    else
					       // cell is at boundary
					       // and we are to treat
					       // curved
					       // boundaries. so loop
					       // over all faces and
					       // draw them as small
					       // pieces of lines
	      {
		for (unsigned int face_no=0;
		     face_no<GeometryInfo<dim>::faces_per_cell; ++face_no)
		  {
		    const typename Triangulation<dim>::face_iterator
		      face = cell->face(face_no);
		    if (face->at_boundary())
		      {
			out << face->vertex(0)
			    << ' ' << cell->level()
			    << ' ' << static_cast<unsigned int>(cell->material_id())
			    << '\n';

                                                         // compute
                                                         // offset of
                                                         // quadrature
                                                         // points
                                                         // within set
                                                         // of
                                                         // projected
                                                         // points. note
                                                         // that we
                                                         // need not
                                                         // care about
                                                         // reverted
                                                         // faces in
                                                         // 3d since
                                                         // boundary
                                                         // faces
                                                         // always
                                                         // have to be
                                                         // in
                                                         // standard
                                                         // orientation
                                                         // and the
                                                         // standard
                                                         // orientation
                                                         // quadrature
                                                         // points are
                                                         // first in
                                                         // the list
			const unsigned int offset=face_no*n_points;
			for (unsigned int i=0; i<n_points; ++i)
			  out << (mapping->transform_unit_to_real_cell
				  (cell, q_projector->point(offset+i)))
			      << ' ' << cell->level()
			      << ' ' << static_cast<unsigned int>(cell->material_id())
			      << '\n';
			
			out << face->vertex(1)
			    << ' ' << cell->level()
			    << ' ' << static_cast<unsigned int>(cell->material_id())
			    << '\n'
			    << '\n'
			    << '\n';
		      }
		    else
		      {
							 // if,
							 // however,
							 // the face
							 // is not at
							 // the
							 // boundary,
							 // then draw
							 // it as
							 // usual
			out << face->vertex(0)
			    << ' ' << cell->level()
			    << ' ' << static_cast<unsigned int>(cell->material_id())
			    << '\n'
			    << face->vertex(1)
			    << ' ' << cell->level()
			    << ' ' << static_cast<unsigned int>(cell->material_id())
			    << '\n'
			    << '\n'
			    << '\n';
		      };
		  };
	      };
	    
	    break;
	  };
	   
	  case 3:
	  {
//TODO:[RH] curved boundaries in 3d gnuplot not supported	    
	    Assert (mapping == 0, ExcNotImplemented());
	    
					     // front face
	    out << cell->vertex(0)
		<< ' ' << cell->level()
		<< ' ' << static_cast<unsigned int>(cell->material_id()) << '\n'
		<< cell->vertex(1)
		<< ' ' << cell->level()
		<< ' ' << static_cast<unsigned int>(cell->material_id()) << '\n'
		<< cell->vertex(2)
		<< ' ' << cell->level()
		<< ' ' << static_cast<unsigned int>(cell->material_id()) << '\n'
		<< cell->vertex(3)
		<< ' ' << cell->level()
		<< ' ' << static_cast<unsigned int>(cell->material_id()) << '\n'
		<< cell->vertex(0)
		<< ' ' << cell->level()
		<< ' ' << static_cast<unsigned int>(cell->material_id()) << '\n'
		<< '\n';
					     // back face
	    out << cell->vertex(4)
		<< ' ' << cell->level()
		<< ' ' << static_cast<unsigned int>(cell->material_id()) << '\n'
		<< cell->vertex(5)
		<< ' ' << cell->level()
		<< ' ' << static_cast<unsigned int>(cell->material_id()) << '\n'
		<< cell->vertex(6)
		<< ' ' << cell->level()
		<< ' ' << static_cast<unsigned int>(cell->material_id()) << '\n'
		<< cell->vertex(7)
		<< ' ' << cell->level()
		<< ' ' << static_cast<unsigned int>(cell->material_id()) << '\n'
		<< cell->vertex(4)
		<< ' ' << cell->level()
		<< ' ' << static_cast<unsigned int>(cell->material_id()) << '\n'
		<< '\n';
	    
					     // now for the four connecting lines
	    out << cell->vertex(0)
		<< ' ' << cell->level()
		<< ' ' << static_cast<unsigned int>(cell->material_id()) << '\n'
		<< cell->vertex(4)
		<< ' ' << cell->level()
		<< ' ' << static_cast<unsigned int>(cell->material_id()) << '\n'
		<< '\n';
	    out << cell->vertex(1)
		<< ' ' << cell->level()
		<< ' ' << static_cast<unsigned int>(cell->material_id()) << '\n'
		<< cell->vertex(5)
		<< ' ' << cell->level()
		<< ' ' << static_cast<unsigned int>(cell->material_id()) << '\n'
		<< '\n';
	    out << cell->vertex(2)
		<< ' ' << cell->level()
		<< ' ' << static_cast<unsigned int>(cell->material_id()) << '\n'
		<< cell->vertex(6)
		<< ' ' << cell->level()
		<< ' ' << static_cast<unsigned int>(cell->material_id()) << '\n'
		<< '\n';
	    out << cell->vertex(3)
		<< ' ' << cell->level()
		<< ' ' << static_cast<unsigned int>(cell->material_id()) << '\n'
		<< cell->vertex(7)
		<< ' ' << cell->level()
		<< ' ' << static_cast<unsigned int>(cell->material_id()) << '\n'
		<< '\n';
	    break;
	  };
	};
    };

  if (q_projector != 0)
    delete q_projector;
  
  
  AssertThrow (out, ExcIO());
}

#endif

struct LineEntry
{
    Point<2> first;
    Point<2> second;
    bool colorize;
    unsigned int level;
    LineEntry (const Point<2>    &f,
	       const Point<2>    &s,
	       const bool         c,
	       const unsigned int l)
		    :
		    first(f), second(s),
		    colorize(c), level(l)
      {}
};


#if deal_II_dimension==1

void GridOut::write_eps (const Triangulation<1> &,
			 std::ostream &,
			 const Mapping<1> *) 
{
  Assert(false, ExcNotImplemented());
}


#else

template <int dim>
void GridOut::write_eps (const Triangulation<dim> &tria,
			 std::ostream             &out,
			 const Mapping<dim>       *mapping) 
{
  
  typedef std::list<LineEntry> LineList;

				   // get a pointer to the flags
				   // common to all dimensions,
				   // in order to avoid the recurring
				   // distinctions between
				   // eps_flags_1, eps_flags_2, ...
  const GridOutFlags::EpsFlagsBase
    &eps_flags_base = (dim==2 ?
		       static_cast<GridOutFlags::EpsFlagsBase&>(eps_flags_2) :
		       (dim==3 ?
			static_cast<GridOutFlags::EpsFlagsBase&>(eps_flags_3) :
			*static_cast<GridOutFlags::EpsFlagsBase*>(0)));
  
  AssertThrow (out, ExcIO());
  const unsigned int n_points = eps_flags_base.n_boundary_face_points;

				   // make up a list of lines by which
				   // we will construct the triangulation
				   //
				   // this part unfortunately is a bit
				   // dimension dependent, so we have to
				   // treat every dimension different.
				   // however, by directly producing
				   // the lines to be printed, i.e. their
				   // 2d images, we can later do the
				   // actual output dimension independent
				   // again
  LineList line_list;

  switch (dim)
    {
      case 1:
      {
	Assert(false, ExcInternalError());
	break;
      };
       
      case 2:
      {
	typename Triangulation<dim>::active_line_iterator
	  line   =tria.begin_active_line (),
	  endline=tria.end_line ();

					 // first treat all interior
					 // lines and make up a list
					 // of them. if curved lines
					 // shall not be supported
					 // (i.e. no mapping is
					 // provided), then also treat
					 // all other lines
	for (; line!=endline; ++line)
	  if (mapping==0 || !line->at_boundary())
					     // one would expect
					     // make_pair(line->vertex(0),
					     //           line->vertex(1))
					     // here, but that is not
					     // dimension independent, since
					     // vertex(i) is Point<dim>,
					     // but we want a Point<2>.
					     // in fact, whenever we're here,
					     // the vertex is a Point<dim>,
					     // but the compiler does not
					     // know this. hopefully, the
					     // compiler will optimize away
					     // this little kludge
	    line_list.push_back (LineEntry(Point<2>(line->vertex(0)(0),
						    line->vertex(0)(1)),
					   Point<2>(line->vertex(1)(0),
						    line->vertex(1)(1)),
					   line->user_flag_set(),
					   line->level()));
	
					 // next if we are to treat
					 // curved boundaries
					 // specially, then add lines
					 // to the list consisting of
					 // pieces of the boundary
					 // lines
	if (mapping!=0)
	  {
					     // to do so, first
					     // generate a sequence of
					     // points on a face and
					     // project them onto the
					     // faces of a unit cell
	    std::vector<Point<dim-1> > boundary_points (n_points);
	    
	    for (unsigned int i=0; i<n_points; ++i)
	      boundary_points[i](0) = 1.*(i+1)/(n_points+1);
	    
	    Quadrature<dim-1> quadrature (boundary_points);
	    Quadrature<dim>   q_projector (QProjector<dim>::project_to_all_faces(quadrature));

					     // next loop over all
					     // boundary faces and
					     // generate the info from
					     // them
	    typename Triangulation<dim>::active_cell_iterator cell=tria.begin_active ();
	    const typename Triangulation<dim>::active_cell_iterator end=tria.end ();
	    for (; cell!=end; ++cell)
	      for (unsigned int face_no=0; face_no<GeometryInfo<dim>::faces_per_cell; ++face_no)
		{
		  const typename Triangulation<dim>::face_iterator
		    face = cell->face(face_no);
		  
		  if (face->at_boundary())
		    {
		      Point<dim> p0_dim(face->vertex(0));
		      Point<2>   p0    (p0_dim(0), p0_dim(1));

						       // loop over
						       // all pieces
						       // of the line
						       // and generate
						       // line-lets
		      const unsigned int offset=face_no*n_points;
		      for (unsigned int i=0; i<n_points; ++i)
			{
			  const Point<dim> p1_dim (mapping->transform_unit_to_real_cell
						   (cell, q_projector.point(offset+i)));
			  const Point<2>   p1     (p1_dim(0), p1_dim(1));
			  
			  line_list.push_back (LineEntry(p0, p1,
							 face->user_flag_set(),
							 face->level() ));
			  p0=p1;
			}

						       // generate last piece
		      const Point<dim> p1_dim (face->vertex(1));
		      const Point<2>   p1     (p1_dim(0), p1_dim(1));
		      line_list.push_back (LineEntry(p0, p1,
						     face->user_flag_set(),
						     face->level()));
		    };
		};
	  };
	
	break;
      };
       
      case 3:
      {
					 // curved boundary output
					 // presently not supported
//TODO:[RH] curved boundaries in eps for 3d	
	Assert (mapping == 0, ExcNotImplemented());
	
	typename Triangulation<dim>::active_line_iterator
	  line   =tria.begin_active_line (),
	  endline=tria.end_line ();
	
					 // loop over all lines and compute their
					 // projection on the plane perpendicular
					 // to the direction of sight

					 // direction of view equals the unit 
					 // vector of the position of the
					 // spectator to the origin.
					 //
					 // we chose here the viewpoint as in
					 // gnuplot as default.
					 //
//TODO:[WB] Fix a potential problem with viewing angles in 3d Eps GridOut
					 // note: the following might be wrong
					 // if one of the base vectors below
					 // is in direction of the viewer, but
					 // I am too tired at present to fix
					 // this
	const double pi = deal_II_numbers::PI;
	const double z_angle    = eps_flags_3.azimut_angle;
	const double turn_angle = eps_flags_3.turn_angle;
	const Point<dim> view_direction(-std::sin(z_angle * 2.*pi / 360.) * std::sin(turn_angle * 2.*pi / 360.),
					+std::sin(z_angle * 2.*pi / 360.) * std::cos(turn_angle * 2.*pi / 360.),
					-std::cos(z_angle * 2.*pi / 360.));
	
					 // decide about the two unit vectors
					 // in this plane. we chose the first one
					 // to be the projection of the z-axis
					 // to this plane
	const Point<dim> vector1
	  = Point<dim>(0,0,1) - ((Point<dim>(0,0,1) * view_direction) * view_direction);
	const Point<dim> unit_vector1 = vector1 / std::sqrt(vector1.square());
	
					 // now the third vector is fixed. we
					 // chose the projection of a more or
					 // less arbitrary vector to the plane
					 // perpendicular to the first one
	const Point<dim> vector2
	  = (Point<dim>(1,0,0)
	     - ((Point<dim>(1,0,0) * view_direction) * view_direction)
	     - ((Point<dim>(1,0,0) * unit_vector1)   * unit_vector1));
	const Point<dim> unit_vector2 = vector2 / std::sqrt(vector2.square());
	
	for (; line!=endline; ++line) 
	  line_list.push_back (LineEntry(Point<2>(line->vertex(0) * unit_vector2,
						  line->vertex(0) * unit_vector1),
					 Point<2>(line->vertex(1) * unit_vector2,
						  line->vertex(1) * unit_vector1),
					 line->user_flag_set(),
					 line->level()));

	break;
      };

      default:
	    Assert (false, ExcNotImplemented());
    };



				   // find out minimum and maximum x and
				   // y coordinates to compute offsets
				   // and scaling factors
  double x_min = tria.begin_active_line()->vertex(0)(0);
  double x_max = x_min;
  double y_min = tria.begin_active_line()->vertex(0)(1);
  double y_max = y_min;
  unsigned int  max_level = line_list.begin()->level;

  for (LineList::const_iterator line=line_list.begin();
       line!=line_list.end(); ++line)
    {
      x_min = std::min (x_min, line->first(0));
      x_min = std::min (x_min, line->second(0));

      x_max = std::max (x_max, line->first(0));
      x_max = std::max (x_max, line->second(0));

      y_min = std::min (y_min, line->first(1));
      y_min = std::min (y_min, line->second(1));

      y_max = std::max (y_max, line->first(1));
      y_max = std::max (y_max, line->second(1));
      
      max_level = std::max (max_level,  line->level);      
    };

				   // scale in x-direction such that
				   // in the output 0 <= x <= 300.
				   // don't scale in y-direction to
				   // preserve the shape of the
				   // triangulation
  const double scale = (eps_flags_base.size /
			(eps_flags_base.size_type==GridOutFlags::EpsFlagsBase::width ?
			 x_max - x_min :
			 y_min - y_max));


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
	  << static_cast<unsigned int>(std::floor(( (x_max-x_min) * scale )+1))
	  << ' '
          << static_cast<unsigned int>(std::floor(( (y_max-y_min) * scale )+1))
	  << '\n';

				       // define some abbreviations to keep
				       // the output small:
				       // m=move turtle to
				       // x=execute line stroke
				       // b=black pen
				       // r=red pen
      out << "/m {moveto} bind def" << '\n'
	  << "/x {lineto stroke} bind def" << '\n'
	  << "/b {0 0 0 setrgbcolor} def" << '\n'
	  << "/r {1 0 0 setrgbcolor} def" << '\n';

				       // calculate colors for level
				       // coloring; level 0 is black,
				       // other levels are blue
				       // ... red
      if (eps_flags_base.color_lines_level)
	out  << "/l  { neg "
	     << (max_level)
	     << " add "
	     << (0.66666/std::max(1U,(max_level-1)))
	     << " mul 1 0.8 sethsbcolor} def" << '\n';

				       // in 2d, we can also plot cell
				       // and vertex numbers, but this
				       // requires a somewhat more
				       // lengthy preamble. please
				       // don't ask me what most of
				       // this means, it is reverse
				       // engineered from what GNUPLOT
				       // uses in its output
      if ((dim == 2) && (eps_flags_2.write_cell_numbers ||
			 eps_flags_2.write_vertex_numbers))
	{
	  out << ("/R {rmoveto} bind def\n"
		  "/Symbol-Oblique /Symbol findfont [1 0 .167 1 0 0] makefont\n"
		  "dup length dict begin {1 index /FID eq {pop pop} {def} ifelse} forall\n"
		  "currentdict end definefont\n"
		  "/MFshow {{dup dup 0 get findfont exch 1 get scalefont setfont\n"
		  "[ currentpoint ] exch dup 2 get 0 exch rmoveto dup dup 5 get exch 4 get\n"
		  "{show} {stringwidth pop 0 rmoveto}ifelse dup 3 get\n"
		  "{2 get neg 0 exch rmoveto pop} {pop aload pop moveto}ifelse} forall} bind def\n"
		  "/MFwidth {0 exch {dup 3 get{dup dup 0 get findfont exch 1 get scalefont setfont\n"
		  "5 get stringwidth pop add}\n"
		  "{pop} ifelse} forall} bind def\n"
		  "/MCshow { currentpoint stroke m\n"
		  "exch dup MFwidth -2 div 3 -1 roll R MFshow } def\n")
	      << '\n';
	};
      
      out << "%%EndProlog" << '\n'
	  << '\n';

				       // set fine lines
      out << eps_flags_base.line_width << " setlinewidth" << '\n';
    };

				   // now write the lines
  const Point<2> offset(x_min, y_min);
  
  for (LineList::const_iterator line=line_list.begin();
       line!=line_list.end(); ++line)
    if (eps_flags_base.color_lines_level && (line->level > 0))
				       // lines colored according to
				       // refinement level,
				       // contributed by J�rg
				       // R. Weimar
      out << line->level
	  << " l "
	  << (line->first  - offset) * scale << " m "
	  << (line->second - offset) * scale << " x" << '\n';
    else
      out << ((line->colorize && eps_flags_base.color_lines_on_user_flag) ? "r " : "b ")
	  << (line->first  - offset) * scale << " m "
	  << (line->second - offset) * scale << " x" << '\n';
    
				   // finally write the cell numbers
				   // in 2d, if that is desired
  if ((dim == 2) && (eps_flags_2.write_cell_numbers == true))
    {
      out << "(Helvetica) findfont 140 scalefont setfont"
	  << '\n';
      
      typename Triangulation<dim>::active_cell_iterator
	cell = tria.begin_active (),
	endc = tria.end ();
      for (; cell!=endc; ++cell)
	{
	  out << (cell->center()(0)-offset(0))*scale << ' '
	      << (cell->center()(1)-offset(1))*scale
	      << " m" << '\n'
	      << "[ [(Helvetica) 12.0 0.0 true true (";
	  if (eps_flags_2.write_cell_number_level)
	    out << cell;
	  else
	    out << cell->index();

	  out << ")] "
	      << "] -6 MCshow"
	      << '\n';
	};
    };

				   // and the vertex numbers
  if ((dim == 2) && (eps_flags_2.write_vertex_numbers == true))
    {
      out << "(Helvetica) findfont 140 scalefont setfont"
	  << '\n';

				       // have a list of those
				       // vertices which we have
				       // already tracked, to avoid
				       // doing this multiply
      std::set<unsigned int> treated_vertices;
      typename Triangulation<dim>::active_cell_iterator
	cell = tria.begin_active (),
	endc = tria.end ();
      for (; cell!=endc; ++cell)
	for (unsigned int vertex=0;
	     vertex<GeometryInfo<dim>::vertices_per_cell;
	     ++vertex)
	  if (treated_vertices.find(cell->vertex_index(vertex))
	      ==
	      treated_vertices.end())
	    {
	      treated_vertices.insert (cell->vertex_index(vertex));
	      
	      out << (cell->vertex(vertex)(0)-offset(0))*scale << ' '
		  << (cell->vertex(vertex)(1)-offset(1))*scale
		  << " m" << '\n'
		  << "[ [(Helvetica) 10.0 0.0 true true ("
		  << cell->vertex_index(vertex)
		  << ")] "
		  << "] -6 MCshow"
		  << '\n';
	    };
    };
  
  out << "showpage" << '\n';
  
  AssertThrow (out, ExcIO());
}

#endif


template <int dim>
void GridOut::write (const Triangulation<dim> &tria,
		     std::ostream             &out,
		     const OutputFormat        output_format,
		     const Mapping<dim>       *mapping)
{
  switch (output_format)
    {
      case dx:
	    write_dx (tria, out);
	    return;

      case ucd:
	    write_ucd (tria, out);
	    return;

      case gnuplot:
	    write_gnuplot (tria, out, mapping);
	    return;

      case eps:
	    write_eps (tria, out, mapping);
	    return;
 
      case xfig:
	    write_xfig (tria, out, mapping);
	    return;
   };

  Assert (false, ExcInternalError());
}



// explicit instantiations
template void GridOut::write_ucd<deal_II_dimension>
(const Triangulation<deal_II_dimension> &,
 std::ostream &);
#if deal_II_dimension != 1
template void GridOut::write_gnuplot<deal_II_dimension>
(const Triangulation<deal_II_dimension> &,
 std::ostream &,
 const Mapping<deal_II_dimension> *);
template void GridOut::write_eps<deal_II_dimension>
(const Triangulation<deal_II_dimension> &,
 std::ostream &,
 const Mapping<deal_II_dimension> *);
#endif
template void GridOut::write<deal_II_dimension>
(const Triangulation<deal_II_dimension> &,
 std::ostream &,
 const OutputFormat,
 const Mapping<deal_II_dimension> *);
