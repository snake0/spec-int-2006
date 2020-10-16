//----------------------------  grid_generator.cc  ---------------------------
//    $Id: grid_generator.cc,v 1.1 2004/09/14 00:51:24 wolf Exp $
//    Version: $Name:  $
//
//    Copyright (C) 1998, 1999, 2000, 2001, 2002, 2003, 2004 by the deal.II authors
//
//    This file is subject to QPL and may not be  distributed
//    without copyright and license information. Please refer
//    to the file deal.II/doc/license.html for the  text  and
//    further information on this license.
//
//----------------------------  grid_generator.cc  ---------------------------


#include <base/quadrature_lib.h>
#include <base/thread_management.h>
#include <lac/vector.h>
#include <lac/vector_memory.h>
#include <lac/filtered_matrix.h>
#include <lac/precondition.h>
#include <lac/solver_cg.h>
#include <lac/sparse_matrix.h>
#include <grid/grid_generator.h>
#include <grid/grid_reordering.h>
#include <grid/tria.h>
#include <grid/tria_accessor.h>
#include <grid/tria_iterator.h>
#include <dofs/dof_handler.h>
#include <dofs/dof_accessor.h>
#include <dofs/dof_tools.h>
#include <fe/mapping_q1.h>
#include <fe/fe_q.h>
#include <numerics/matrices.h>

#include <cmath>


template <int dim>
void
GridGenerator::hyper_rectangle (Triangulation<dim> &tria,
				const Point<dim>   &p_1,
				const Point<dim>   &p_2,
				const bool          colorize)
{
				   // First, normalize input such that
				   // p1 is lower in all coordinate directions.
  Point<dim> p1(p_1);
  Point<dim> p2(p_2);
  
  for (unsigned int i=0;i<dim;++i)
    if (p1(i) > p2(i))
      std::swap (p1(i), p2(i));
  
  std::vector<Point<dim> > vertices (GeometryInfo<dim>::vertices_per_cell);
  switch (dim)
    {
      case 1:
	    vertices[0] = p1;
	    vertices[1] = p2;
	    break;
      case 2:
	    vertices[0] = p1;
	    vertices[2] = p2;
	    
	    vertices[1](0) = p2(0);
	    vertices[3](0) = p1(0);
	    vertices[1](1) = p1(1);
	    vertices[3](1) = p2(1);
	    break;
      case 3:
	    vertices[0] = vertices[1] = vertices[2] = vertices[3] = p1;
	    vertices[4] = vertices[5] = vertices[6] = vertices[7] = p2;
	    
	    vertices[1](0) = p2(0);
	    vertices[3](2) = p2(2);
	    vertices[2](0) = p2(0);
	    vertices[2](2) = p2(2);

	    vertices[5](2) = p1(2);
	    vertices[7](0) = p1(0);
	    vertices[4](0) = p1(0);
	    vertices[4](2) = p1(2);
	    
	    break;
      default:
	    Assert (false, ExcNotImplemented ());
    }

				   // Prepare cell data
  std::vector<CellData<dim> > cells (1);
  for (unsigned int i=0;i<GeometryInfo<dim>::vertices_per_cell;++i)
    cells[0].vertices[i] = i;
  cells[0].material_id = 0;

  tria.create_triangulation (vertices, cells, SubCellData());

				   // Assign boundary indicators
  if (colorize)
    colorize_hyper_rectangle (tria);
}



#if deal_II_dimension == 1

void
GridGenerator::colorize_hyper_rectangle (Triangulation<1> &)
{
				   // nothing to do in 1d
}


#else

template <int dim>
void
GridGenerator::colorize_hyper_rectangle (Triangulation<dim> &tria)
{
				   // there is only one cell, so
				   // simple task
  const typename Triangulation<dim>::cell_iterator cell = tria.begin();
  switch(dim)
    {
      case 2:
	    cell->face(0)->set_boundary_indicator (2);
	    cell->face(1)->set_boundary_indicator (1);
	    cell->face(2)->set_boundary_indicator (3);
	    cell->face(3)->set_boundary_indicator (0);
	    break;
      case 3:
	    cell->face(0)->set_boundary_indicator (2);
	    cell->face(1)->set_boundary_indicator (3);
	    cell->face(2)->set_boundary_indicator (4);
	    cell->face(3)->set_boundary_indicator (1);
	    cell->face(4)->set_boundary_indicator (5);
	    cell->face(5)->set_boundary_indicator (0);
	    break;
      default:
	    Assert(false, ExcNotImplemented());
    };
}

#endif


template <int dim>
void GridGenerator::hyper_cube (Triangulation<dim> &tria,
			        const double        left,
			        const double        right)
{
  Point<dim> p1;
  Point<dim> p2;
  for (unsigned int i=0;i<dim;++i)
    {
      p1(i) = left;
      p2(i) = right;
    }
  hyper_rectangle (tria, p1, p2);
}


template <int dim>
void
GridGenerator::subdivided_hyper_cube (Triangulation<dim> &tria,
                                      const unsigned int  repetitions,
                                      const double        left,
                                      const double        right)
{
  Assert (repetitions >= 1, ExcInvalidRepetitions(repetitions));
  
                                   // first generate the necessary
                                   // points
  const double delta = (right-left)/repetitions;
  std::vector<Point<dim> > points;
  switch (dim)
    {
      case 1:
            for (unsigned int x=0; x<=repetitions; ++x)
              points.push_back (Point<dim> (left+x*delta));
            break;

      case 2:
            for (unsigned int y=0; y<=repetitions; ++y)
              for (unsigned int x=0; x<=repetitions; ++x)
                points.push_back (Point<dim> (left+x*delta,
                                              left+y*delta));
            break;

      case 3:
            for (unsigned int z=0; z<=repetitions; ++z)
              for (unsigned int y=0; y<=repetitions; ++y)
                for (unsigned int x=0; x<=repetitions; ++x)
                  points.push_back (Point<dim> (left+x*delta,
                                                left+y*delta,
                                                left+z*delta));
            break;

      default:
            Assert (false, ExcNotImplemented());
    }

                                   // next create the cells
				   // Prepare cell data
  std::vector<CellData<dim> > cells;
  switch (dim)
    {
      case 1:
            cells.resize (repetitions);
            for (unsigned int x=0; x<repetitions; ++x)
              {
                cells[x].vertices[0] = x;
                cells[x].vertices[1] = x+1;
                cells[x].material_id = 0;
              }
            break;

      case 2:
            cells.resize (repetitions*repetitions);
            for (unsigned int y=0; y<repetitions; ++y)
              for (unsigned int x=0; x<repetitions; ++x)
                {
                  const unsigned int c = x+y*repetitions;
                  cells[c].vertices[0] = y*(repetitions+1)+x;
                  cells[c].vertices[1] = y*(repetitions+1)+x+1;
                  cells[c].vertices[2] = (y+1)*(repetitions+1)+x+1;
                  cells[c].vertices[3] = (y+1)*(repetitions+1)+x;
                  cells[c].material_id = 0;
                }
            break;

      default:
                                             // should be trivial to
                                             // do for 3d as well, but
                                             // am too tired at this
                                             // point of the night to
                                             // do that...
                                             //
                                             // contributions are welcome!
            Assert (false, ExcNotImplemented());
    }

  tria.create_triangulation (points, cells, SubCellData());  
}



template <int dim>
void
GridGenerator::
subdivided_hyper_rectangle (Triangulation<dim>              &tria,
                            const std::vector<unsigned int> &repetitions,
                            const Point<dim>                &p_1,
                            const Point<dim>                &p_2,
                            const bool                       colorize)
{
				   // contributed by Joerg R. Weimar
				   // (j.weimar@jweimar.de) 2003
  Assert(repetitions.size() == dim, 
	 ExcInvalidRepetitionsDimension(dim));
				   // First, normalize input such that
				   // p1 is lower in all coordinate
				   // directions.
  Point<dim> p1(p_1);
  Point<dim> p2(p_2);
  
  for (unsigned int i=0;i<dim;++i)
    if (p1(i) > p2(i))
      std::swap (p1(i), p2(i));

				   // then check that all repetitions
				   // are >= 1, and calculate deltas
				   // convert repetitions from double
				   // to int by taking the ceiling.
  double delta[dim];
  
  for (unsigned int i=0; i<dim; ++i)
    {
      Assert (repetitions[i] >= 1, ExcInvalidRepetitions(repetitions[i]));
    		
      delta[i] = (p2[i]-p1[i])/repetitions[i];
    }
 
                                   // then generate the necessary
                                   // points
  std::vector<Point<dim> > points;
  switch (dim)
    {
      case 1:
            for (unsigned int x=0; x<=repetitions[0]; ++x)
              points.push_back (Point<dim> (p1[0]+x*delta[0]));
            break;

      case 2:
            for (unsigned int y=0; y<=repetitions[1]; ++y)
              for (unsigned int x=0; x<=repetitions[0]; ++x)
                points.push_back (Point<dim> (p1[0]+x*delta[0],
                                              p1[1]+y*delta[1]));
            break;

      case 3:
            for (unsigned int z=0; z<=repetitions[2]; ++z)
              for (unsigned int y=0; y<=repetitions[1]; ++y)
                for (unsigned int x=0; x<=repetitions[0]; ++x)
                  points.push_back (Point<dim> (p1[0]+x*delta[0],
                                                p1[1]+y*delta[1],
                                                p1[2]+z*delta[2]));
            break;

      default:
            Assert (false, ExcNotImplemented());
    }

                                   // next create the cells
				   // Prepare cell data
  std::vector<CellData<dim> > cells;
  switch (dim)
    {
      case 1:
      {
        cells.resize (repetitions[0]);
        for (unsigned int x=0; x<repetitions[0]; ++x)
          {
            cells[x].vertices[0] = x;
            cells[x].vertices[1] = x+1;
            cells[x].material_id = 0;
          }
        break;
      }
      
      case 2:
      {
        cells.resize (repetitions[1]*repetitions[0]);
        for (unsigned int y=0; y<repetitions[1]; ++y)
          for (unsigned int x=0; x<repetitions[0]; ++x)
            {
              const unsigned int c = x+y*repetitions[0];
              cells[c].vertices[0] = y*(repetitions[0]+1)+x;
              cells[c].vertices[1] = y*(repetitions[0]+1)+x+1;
              cells[c].vertices[2] = (y+1)*(repetitions[0]+1)+x+1;
              cells[c].vertices[3] = (y+1)*(repetitions[0]+1)+x;
              cells[c].material_id = 0;
            }
        break;
      }
      
      case 3:
      {
        const unsigned int n_x  = (repetitions[0]+1);
        const unsigned int n_xy = (repetitions[0]+1)*(repetitions[1]+1);
        
        cells.resize (repetitions[2]*repetitions[1]*repetitions[0]);
        for (unsigned int z=0; z<repetitions[2]; ++z)
          for (unsigned int y=0; y<repetitions[1]; ++y)
            for (unsigned int x=0; x<repetitions[0]; ++x)
              {
                const unsigned int c = x+y*repetitions[0] +
                                       z*repetitions[0]*repetitions[1];
                cells[c].vertices[0] = z*n_xy + y*n_x + x;
                cells[c].vertices[1] = z*n_xy + y*n_x + x+1;
                cells[c].vertices[2] = (z+1)*n_xy + y*n_x + x+1;
                cells[c].vertices[3] = (z+1)*n_xy + y*n_x + x;
                cells[c].vertices[4] = z*n_xy + (y+1)*n_x + x;
                cells[c].vertices[5] = z*n_xy + (y+1)*n_x + x+1;
                cells[c].vertices[6] = (z+1)*n_xy + (y+1)*n_x + x+1;
                cells[c].vertices[7] = (z+1)*n_xy + (y+1)*n_x + x;
                cells[c].material_id = 0;
              }
        break;
        
      }

      default:
            Assert (false, ExcNotImplemented());
    }

  tria.create_triangulation (points, cells, SubCellData());  

  if (colorize)
    {
				       // to colorize, run through all
				       // faces of all cells and set
				       // boundary indicator to the
				       // correct value if it was 0.
     
				       // use a large epsilon to
				       // compare numbers to avoid
				       // roundoff problems.
      const double epsilon
        = 0.01 * *std::min_element (&delta[0], &delta[dim]);
    
                                       // actual code is external since
                                       // 1-D is different from 2/3D.
      colorize_subdivided_hyper_rectangle (tria, p1, p2, epsilon);
    }
}


#if deal_II_dimension == 1

void
GridGenerator::colorize_subdivided_hyper_rectangle (Triangulation<1> &,
						    const Point<1>   &,
						    const Point<1>   &,
						    const double      )
{
				   // nothing to do in 1d
				   // boundary indicators are set to
				   // 0 (left) and 1 (right) by default.
}

#else

template <int dim>
void
GridGenerator::colorize_subdivided_hyper_rectangle (Triangulation<dim> &tria,
						    const Point<dim>   &p1,
						    const Point<dim>   &p2,
						    const double        epsilon)
{
	
				   // run through all faces and check
				   // if one of their center coordinates matches
				   // one of the corner points. Comparisons
				   // are made using an epsilon which
				   // should be smaller than the smallest cell
				   // diameter.
				    
  typename Triangulation<dim>::raw_face_iterator face = tria.begin_raw_face(),
					      endface = tria.end_face();
  for (; face!=endface; ++face)
    {
      if (face->boundary_indicator() == 0)
	{
	  const Point<dim> center (face->center());
	  if (std::abs(center(0)-p1[0]) < epsilon)
	    face->set_boundary_indicator(0);
	  else if (std::abs(center(0) - p2[0]) < epsilon)
	    face->set_boundary_indicator(1);
	  else if (dim > 1 && std::abs(center(1) - p1[1]) < epsilon)
	    face->set_boundary_indicator(2);
	  else if (dim > 1 && std::abs(center(1) - p2[1]) < epsilon)
	    face->set_boundary_indicator(3);
	  else if (dim > 2 && std::abs(center(2) - p1[2]) < epsilon)
	    face->set_boundary_indicator(4);
	  else if (dim > 2 && std::abs(center(2) - p2[2]) < epsilon)
	    face->set_boundary_indicator(5);
	  else
					     // triangulation says it
					     // is on the boundary,
					     // but we could not find
					     // on which boundary.
	    Assert (false, ExcInternalError());
	  
	}
    }
}

#endif



#if deal_II_dimension == 1

void GridGenerator::hyper_cube_slit (Triangulation<1> &,
				     const double,
				     const double,
				     const bool)
{
  Assert (false, ExcNotImplemented());
}



void GridGenerator::hyper_L (Triangulation<1> &,
			     const double,
			     const double)
{
  Assert (false, ExcNotImplemented());
}



void GridGenerator::hyper_ball (Triangulation<1> &,
				const Point<1> &,
				const double)
{
  Assert (false, ExcNotImplemented());
}



void GridGenerator::cylinder (Triangulation<1> &,
			      const double,
			      const double)
{
  Assert (false, ExcNotImplemented());
}



void GridGenerator::hyper_shell (Triangulation<1> &,
				 const Point<1> &,
				 const double,
				 const double,
				 const unsigned int)
{
  Assert (false, ExcNotImplemented());
}

#endif



#if deal_II_dimension == 2

void GridGenerator::enclosed_hyper_cube (Triangulation<2> &tria,
					 const double      l,
					 const double      r,
					 const double      d,
					 const bool        colorize)
{
  std::vector<Point<2> > vertices(16);
  double coords[4];
  coords[0] = l-d;
  coords[1] = l;
  coords[2] = r;
  coords[3] = r+d;

  unsigned int k=0;
  for (unsigned int i0=0;i0<4;++i0)
    for (unsigned int i1=0;i1<4;++i1)
      vertices[k++] = Point<2>(coords[i1], coords[i0]);

  const unsigned char materials[9] = { 5, 4, 6,
				       1, 0, 2,
				       9, 8,10
  };
  
  std::vector<CellData<2> > cells(9);
  k = 0;
  for (unsigned int i0=0;i0<3;++i0)
    for (unsigned int i1=0;i1<3;++i1)
      {
	cells[k].vertices[0] = i1+4*i0;
	cells[k].vertices[1] = i1+4*i0+1;
	cells[k].vertices[2] = i1+4*i0+5;
	cells[k].vertices[3] = i1+4*i0+4;
	if (colorize)
	  cells[k].material_id = materials[k];
	++k;
      }
  tria.create_triangulation (vertices,
			     cells,
			     SubCellData());       // no boundary information
}



void
GridGenerator::hyper_cube_slit (Triangulation<2> &tria,
				const double left,
				const double right,
				const bool colorize)
{
  const double rl2=(right+left)/2;
  const Point<2> vertices[10] = { Point<2>(left, left ),
				  Point<2>(rl2,  left ),
				  Point<2>(rl2,  rl2  ),
				  Point<2>(left, rl2  ),
				  Point<2>(right,left ),
				  Point<2>(right,rl2  ),
				  Point<2>(rl2,  right),
				  Point<2>(left, right),
				  Point<2>(right,right),
				  Point<2>(rl2,  left ) };
  const int cell_vertices[4][4] = { { 0,1,2,3 },
				    { 9,4,5,2 },
				    { 3,2,6,7 },
				    { 2,5,8,6 } };
  std::vector<CellData<2> > cells (4, CellData<2>());
  for (unsigned int i=0; i<4; ++i)
    {
      for (unsigned int j=0; j<4; ++j)
	cells[i].vertices[j] = cell_vertices[i][j];
      cells[i].material_id = 0;
    };
  tria.create_triangulation (std::vector<Point<2> >(&vertices[0], &vertices[10]),
			     cells,
			     SubCellData());       // no boundary information

  if (colorize)
    {
      Triangulation<2>::cell_iterator cell = tria.begin();
      cell->face(1)->set_boundary_indicator(1);
      ++cell;
      cell->face(3)->set_boundary_indicator(2);
    }
}



void
GridGenerator::hyper_L (Triangulation<2> &tria,
			const double a,
			const double b)
{
  const unsigned int dim=2;
  const Point<dim> vertices[8] = { Point<dim> (a,a),
				   Point<dim> ((a+b)/2,a),
				   Point<dim> (b,a),
				   Point<dim> (a,(a+b)/2),
				   Point<dim> ((a+b)/2,(a+b)/2),
				   Point<dim> (b,(a+b)/2),
				   Point<dim> (a,b),
				   Point<dim> ((a+b)/2,b)  };
  const int cell_vertices[3][4] = {{0, 1, 4, 3},
				   {1, 2, 5, 4},
				   {3, 4, 7, 6}};

  std::vector<CellData<2> > cells (3, CellData<2>());
  
  for (unsigned int i=0; i<3; ++i) 
    {
      for (unsigned int j=0; j<4; ++j)
	cells[i].vertices[j] = cell_vertices[i][j];
      cells[i].material_id = 0;
    };
  
  tria.create_triangulation (std::vector<Point<dim> >(&vertices[0], &vertices[8]),
			     cells,
			     SubCellData());       // no boundary information
}



void
GridGenerator::hyper_ball (Triangulation<2> &tria,
			   const Point<2>   &p,
			   const double      radius)
{
				   // equilibrate cell sizes at
				   // transition from the inner part
				   // to the radial cells
  const double a = 1./(1+std::sqrt(2.0));
  const Point<2> vertices[8] = { p+Point<2>(-1,-1)*(radius/std::sqrt(2.0)),
				 p+Point<2>(+1,-1)*(radius/std::sqrt(2.0)),
				 p+Point<2>(-1,-1)*(radius/std::sqrt(2.0)*a),
				 p+Point<2>(+1,-1)*(radius/std::sqrt(2.0)*a),
				 p+Point<2>(-1,+1)*(radius/std::sqrt(2.0)*a),
				 p+Point<2>(+1,+1)*(radius/std::sqrt(2.0)*a),
				 p+Point<2>(-1,+1)*(radius/std::sqrt(2.0)),
				 p+Point<2>(+1,+1)*(radius/std::sqrt(2.0)) };
  
  const int cell_vertices[5][4] = {{0, 1, 3, 2},
				   {0, 2, 4, 6},
				   {2, 3, 5, 4},
				   {1, 7, 5, 3},
				   {6, 4, 5, 7}};

  std::vector<CellData<2> > cells (5, CellData<2>());
  
  for (unsigned int i=0; i<5; ++i) 
    {
      for (unsigned int j=0; j<4; ++j)
	cells[i].vertices[j] = cell_vertices[i][j];
      cells[i].material_id = 0;
    };
  
  tria.create_triangulation (std::vector<Point<2> >(&vertices[0], &vertices[8]),
			     cells,
			     SubCellData());       // no boundary information
}



void GridGenerator::hyper_shell (Triangulation<2>   &tria,
				 const Point<2>     &center,
				 const double        inner_radius,
				 const double        outer_radius,
				 const unsigned int  n_cells)
{
  Assert ((inner_radius > 0) && (inner_radius < outer_radius),
	  ExcInvalidRadii ());
  
  const double pi = deal_II_numbers::PI;
  
				   // determine the number of cells
				   // for the grid. if not provided by
				   // the user determine it such that
				   // the length of each cell on the
				   // median (in the middle between
				   // the two circles) is equal to its
				   // radial extent (which is the
				   // difference between the two
				   // radii)
  const unsigned int N = (n_cells == 0 ?
			  static_cast<unsigned int>
			  (std::ceil((2*pi* (outer_radius + inner_radius)/2) /
				     (outer_radius - inner_radius))) :
			  n_cells);

				   // set up N vertices on the
				   // outer and N vertices on
				   // the inner circle. the
				   // first N ones are on the
				   // outer one, and all are
				   // numbered counter-clockwise
  std::vector<Point<2> > vertices(2*N);
  for (unsigned int i=0; i<N; ++i)
    {
      vertices[i] = Point<2>( std::cos(2*pi * i/N),
			      std::sin(2*pi * i/N)) * outer_radius;
      vertices[i+N] = vertices[i] * (inner_radius/outer_radius);

      vertices[i]   += center;
      vertices[i+N] += center;
    };

  std::vector<CellData<2> > cells (N, CellData<2>());
	
  for (unsigned int i=0; i<N; ++i) 
    {
      cells[i].vertices[0] = i;
      cells[i].vertices[1] = (i+1)%N;
      cells[i].vertices[2] = N+((i+1)%N);
      cells[i].vertices[3] = N+i;
	    
      cells[i].material_id = 0;
    };
  
  tria.create_triangulation (vertices, cells, SubCellData());
}



void
GridGenerator::cylinder (Triangulation<2> &tria,
			 const double radius,
			 const double half_length)
{
  Point<2> p1 (-half_length, -radius);
  Point<2> p2 (half_length, radius);

  hyper_rectangle(tria, p1, p2, true);

  Triangulation<2>::face_iterator f = tria.begin_face();
  Triangulation<2>::face_iterator end = tria.end_face();
  while (f != end)
    {
      switch (f->boundary_indicator())
	{
	  case 0:
		f->set_boundary_indicator(1);
		break;
	  case 1:
		f->set_boundary_indicator(2);
		break;
	  default:
		f->set_boundary_indicator(0);
		break;	    
	}
      ++f;
    }
}



void
GridGenerator::half_hyper_ball (Triangulation<2> &tria,
				const Point<2>   &p,
				const double      radius)
{
				   // equilibrate cell sizes at
				   // transition from the inner part
				   // to the radial cells
  const double a = 1./(1+std::sqrt(2.0));
  const Point<2> vertices[8] = { p+Point<2>(0,-1)*radius,
				 p+Point<2>(+1,-1)*(radius/std::sqrt(2.0)),
				 p+Point<2>(0,-1)*(radius/std::sqrt(2.0)*a),
				 p+Point<2>(+1,-1)*(radius/std::sqrt(2.0)*a),
				 p+Point<2>(0,+1)*(radius/std::sqrt(2.0)*a),
				 p+Point<2>(+1,+1)*(radius/std::sqrt(2.0)*a),
				 p+Point<2>(0,+1)*radius,
				 p+Point<2>(+1,+1)*(radius/std::sqrt(2.0)) };
  
  const int cell_vertices[5][4] = {{0, 1, 3, 2},
				   {2, 3, 5, 4},
				   {1, 7, 5, 3},
				   {6, 4, 5, 7}};

  std::vector<CellData<2> > cells (4, CellData<2>());
  
  for (unsigned int i=0; i<4; ++i) 
    {
      for (unsigned int j=0; j<4; ++j)
	cells[i].vertices[j] = cell_vertices[i][j];
      cells[i].material_id = 0;
    };
  
  tria.create_triangulation (std::vector<Point<2> >(&vertices[0], &vertices[8]),
			     cells,
			     SubCellData());       // no boundary information
}



void
GridGenerator::half_hyper_shell (Triangulation<2>   &tria,
				 const Point<2>     &center,
				 const double        inner_radius,
				 const double        outer_radius,
				 const unsigned int  n_cells)
{
  Assert ((inner_radius > 0) && (inner_radius < outer_radius),
	  ExcInvalidRadii ());
  
  const double pi     = deal_II_numbers::PI;
				   // determine the number of cells
				   // for the grid. if not provided by
				   // the user determine it such that
				   // the length of each cell on the
				   // median (in the middle between
				   // the two circles) is equal to its
				   // radial extent (which is the
				   // difference between the two
				   // radii)
  const unsigned int N = (n_cells == 0 ?
			  static_cast<unsigned int>
			  (std::ceil((pi* (outer_radius + inner_radius)/2) /
				     (outer_radius - inner_radius))) :
			  n_cells);

				   // set up N+1 vertices on the
				   // outer and N+1 vertices on
				   // the inner circle. the
				   // first N+1 ones are on the
				   // outer one, and all are
				   // numbered counter-clockwise
  std::vector<Point<2> > vertices(2*(N+1));
  for (unsigned int i=0; i<=N; ++i)
    {
				       // enforce that the x-coordinates
				       // of the first and last point of
				       // each half-circle are exactly
				       // zero (contrary to what we may
				       // compute using the imprecise
				       // value of pi)
      vertices[i] =  Point<2>( ( (i==0) || (i==N) ?
				 0 :
				 std::cos(pi * i/N - pi/2) ),
			       std::sin(pi * i/N - pi/2)) * outer_radius;
      vertices[i+N+1] = vertices[i] * (inner_radius/outer_radius);

      vertices[i]     += center;
      vertices[i+N+1] += center;
    };


  std::vector<CellData<2> > cells (N, CellData<2>());
	
  for (unsigned int i=0; i<N; ++i) 
    {
      cells[i].vertices[0] = i;
      cells[i].vertices[1] = (i+1)%(N+1);
      cells[i].vertices[2] = N+1+((i+1)%(N+1));
      cells[i].vertices[3] = N+1+i;
	    
      cells[i].material_id = 0;
    };
  
  tria.create_triangulation (vertices, cells, SubCellData());
}



#endif


#if deal_II_dimension == 3


void GridGenerator::hyper_cube_slit (Triangulation<3> &,
				     const double,
				     const double,
				     const bool)
{
  Assert (false, ExcNotImplemented());
}



void GridGenerator::enclosed_hyper_cube (Triangulation<3> &tria,
					 const double      l,
					 const double      r,
					 const double      d,
					 const bool        colorize)
{
  std::vector<Point<3> > vertices(64);
  double coords[4];
  coords[0] = l-d;
  coords[1] = l;
  coords[2] = r;
  coords[3] = r+d;

  unsigned int k=0;
  for (unsigned int i0=0;i0<4;++i0)
    for (unsigned int i1=0;i1<4;++i1)
      for (unsigned int i2=0;i2<4;++i2)
	vertices[k++] = Point<3>(coords[i2], coords[i1], coords[i0]);

  const unsigned char materials[27] = {
	21,20,22,
	17,16,18,
	25,24,26,
	5 , 4, 6,
	1 , 0, 2,
	9 , 8,10,
	37,36,38,
	33,32,34,
	41,40,42
  };
  
  std::vector<CellData<3> > cells(27);
  k = 0;
  for (unsigned int i0=0;i0<3;++i0)
    for (unsigned int i1=0;i1<3;++i1)
      for (unsigned int i2=0;i2<3;++i2)
	{
	  cells[k].vertices[0] = i2+4*i1+16*i0;
	  cells[k].vertices[1] = i2+4*i1+16*i0+1;
	  cells[k].vertices[2] = i2+4*i1+16*i0+5;
	  cells[k].vertices[3] = i2+4*i1+16*i0+4;
	  cells[k].vertices[4] = i2+4*i1+16*i0+16;
	  cells[k].vertices[5] = i2+4*i1+16*i0+17;
	  cells[k].vertices[6] = i2+4*i1+16*i0+21;
	  cells[k].vertices[7] = i2+4*i1+16*i0+20;
	  if (colorize)
	    cells[k].material_id = materials[k];
	  ++k;
	}
  tria.create_triangulation (vertices,
			     cells,
			     SubCellData());       // no boundary information
}



void
GridGenerator::hyper_L (Triangulation<3> &tria,
			const double      a,
			const double      b)
{
  const unsigned int dim=3;
				   // we slice out the top back right
				   // part of the cube
  const Point<dim> vertices[26]
    = {
					   // front face of the big cube
	  Point<dim> (a,      a,a),
	  Point<dim> ((a+b)/2,a,a),
	  Point<dim> (b,      a,a),
	  Point<dim> (a,      a,(a+b)/2),
	  Point<dim> ((a+b)/2,a,(a+b)/2),
	  Point<dim> (b,      a,(a+b)/2),
	  Point<dim> (a,      a,b),
	  Point<dim> ((a+b)/2,a,b),
	  Point<dim> (b,      a,b),
					   // middle face of the big cube
	  Point<dim> (a,      (a+b)/2,a),
	  Point<dim> ((a+b)/2,(a+b)/2,a),
	  Point<dim> (b,      (a+b)/2,a),
	  Point<dim> (a,      (a+b)/2,(a+b)/2),
	  Point<dim> ((a+b)/2,(a+b)/2,(a+b)/2),
	  Point<dim> (b,      (a+b)/2,(a+b)/2),
	  Point<dim> (a,      (a+b)/2,b),
	  Point<dim> ((a+b)/2,(a+b)/2,b),
	  Point<dim> (b,      (a+b)/2,b),
					   // back face of the big cube
					   // last (top right) point is missing
	  Point<dim> (a,      b,a),
	  Point<dim> ((a+b)/2,b,a),
	  Point<dim> (b,      b,a),
	  Point<dim> (a,      b,(a+b)/2),
	  Point<dim> ((a+b)/2,b,(a+b)/2),
	  Point<dim> (b,      b,(a+b)/2),
	  Point<dim> (a,      b,b),
	  Point<dim> ((a+b)/2,b,b)
    };
  const int cell_vertices[7][8] = {{0, 1, 4, 3, 9, 10, 13, 12},
				   {1, 2, 5, 4, 10, 11, 14, 13},
				   {3, 4, 7, 6, 12, 13, 16, 15},
				   {4, 5, 8, 7, 13, 14, 17, 16},
				   {9, 10, 13, 12, 18, 19, 22, 21},
				   {10, 11, 14, 13, 19, 20, 23, 22},
				   {12, 13, 16, 15, 21, 22, 25, 24}};

  std::vector<CellData<3> > cells (7, CellData<3>());
  
  for (unsigned int i=0; i<7; ++i) 
    {
      for (unsigned int j=0; j<8; ++j)
	cells[i].vertices[j] = cell_vertices[i][j];
      cells[i].material_id = 0;
    };
  
  tria.create_triangulation (std::vector<Point<dim> >(&vertices[0], &vertices[26]),
			     cells,
			     SubCellData());       // no boundary information
}



void
GridGenerator::hyper_ball (Triangulation<3> &tria,
			   const Point<3>   &p,
			   const double radius)
{
  const double a = 1./(1+std::sqrt(3.0)); // equilibrate cell sizes at transition
				          // from the inner part to the radial
				          // cells
  const unsigned int n_vertices = 16;
  const Point<3> vertices[n_vertices]
    = {
					   // first the vertices of the inner
					   // cell
	  p+Point<3>(-1,-1,-1)*(radius/std::sqrt(3.0)*a),
	  p+Point<3>(+1,-1,-1)*(radius/std::sqrt(3.0)*a),
	  p+Point<3>(+1,-1,+1)*(radius/std::sqrt(3.0)*a),
	  p+Point<3>(-1,-1,+1)*(radius/std::sqrt(3.0)*a),
	  p+Point<3>(-1,+1,-1)*(radius/std::sqrt(3.0)*a),
	  p+Point<3>(+1,+1,-1)*(radius/std::sqrt(3.0)*a),
	  p+Point<3>(+1,+1,+1)*(radius/std::sqrt(3.0)*a),
	  p+Point<3>(-1,+1,+1)*(radius/std::sqrt(3.0)*a),
					   // now the eight vertices at
					   // the outer sphere
	  p+Point<3>(-1,-1,-1)*(radius/std::sqrt(3.0)),
	  p+Point<3>(+1,-1,-1)*(radius/std::sqrt(3.0)),
	  p+Point<3>(+1,-1,+1)*(radius/std::sqrt(3.0)),
	  p+Point<3>(-1,-1,+1)*(radius/std::sqrt(3.0)),
	  p+Point<3>(-1,+1,-1)*(radius/std::sqrt(3.0)),
	  p+Point<3>(+1,+1,-1)*(radius/std::sqrt(3.0)),
	  p+Point<3>(+1,+1,+1)*(radius/std::sqrt(3.0)),
	  p+Point<3>(-1,+1,+1)*(radius/std::sqrt(3.0)),
    };

				   // one needs to draw the seven cubes to
				   // understand what's going on here
  const unsigned int n_cells = 7;
  const int cell_vertices[n_cells][8] = {{0, 1, 2, 3, 4, 5, 6, 7}, // center
					 {8, 9, 1, 0, 12, 13, 5, 4}, // bottom
					 {9, 13, 14, 10, 1, 5, 6, 2}, // right
					 {11, 10, 14, 15, 3, 2, 6, 7}, // top
					 {8, 0, 3, 11, 12, 4, 7, 15}, // left
					 {8, 9, 10, 11, 0, 1, 2, 3}, // front
					 {12, 4, 7, 15, 13, 5, 6, 14}}; // back
  
  std::vector<CellData<3> > cells (n_cells, CellData<3>());
  
  for (unsigned int i=0; i<n_cells; ++i) 
    {
      for (unsigned int j=0; j<GeometryInfo<3>::vertices_per_cell; ++j)
	cells[i].vertices[j] = cell_vertices[i][j];
      cells[i].material_id = 0;
    };

  GridReordering<3>::reorder_cells (cells);
  
  tria.create_triangulation (std::vector<Point<3> >(&vertices[0], &vertices[n_vertices]),
			     cells,
			     SubCellData());       // no boundary information
}



void
GridGenerator::cylinder (Triangulation<3> &tria,
			 const double radius,
			 const double half_length)
{
				   // Copy the base from hyper_ball<2>
				   // and transform it to yz
  const double d = radius/std::sqrt(2.0);
  const double a = d/(1+std::sqrt(2.0));
  Point<3> vertices[24] = {
	Point<3>(-d, -half_length,-d),
	Point<3>( d, -half_length,-d),
	Point<3>(-a, -half_length,-a),
	Point<3>( a, -half_length,-a),
	Point<3>(-a, -half_length, a),
	Point<3>( a, -half_length, a),
	Point<3>(-d, -half_length, d),
	Point<3>( d, -half_length, d),
	Point<3>(-d, 0,-d),
	Point<3>( d, 0,-d),
	Point<3>(-a, 0,-a),
	Point<3>( a, 0,-a),
	Point<3>(-a, 0, a),
	Point<3>( a, 0, a),
	Point<3>(-d, 0, d),
	Point<3>( d, 0, d),
	Point<3>(-d, half_length,-d),
	Point<3>( d, half_length,-d),
	Point<3>(-a, half_length,-a),
	Point<3>( a, half_length,-a),
	Point<3>(-a, half_length, a),
	Point<3>( a, half_length, a),
	Point<3>(-d, half_length, d),
	Point<3>( d, half_length, d),
  };
				   // Turn cylinder such that y->x
  for (unsigned int i=0;i<24;++i)
    {
      const double h = vertices[i](1);
      vertices[i](1) = -vertices[i](0);
      vertices[i](0) = h;
    }
  
  int cell_vertices[10][8] = {
	{0,1,3,2,8,9,11,10},
	{0,2,4,6,8,10,12,14},
	{2,3,5,4,10,11,13,12},
	{1,7,5,3,9,15,13,11},
	{6,4,5,7,14,12,13,15}
  };
  for (unsigned int i=0;i<5;++i)
    for (unsigned int j=0;j<8;++j)
      cell_vertices[i+5][j] = cell_vertices[i][j]+8;
  
  std::vector<CellData<3> > cells (10, CellData<3>());
  
  for (unsigned int i=0; i<10; ++i) 
    {
      for (unsigned int j=0; j<8; ++j)
	cells[i].vertices[j] = cell_vertices[i][j];
      cells[i].material_id = 0;
    };
  
  GridReordering<3>::reorder_cells (cells);
  
  tria.create_triangulation (std::vector<Point<3> >(&vertices[0], &vertices[24]),
			     cells,
			     SubCellData());       // no boundary information

  Triangulation<3>::cell_iterator cell = tria.begin();
  Triangulation<3>::cell_iterator end = tria.end();
  
  while (cell != end)
    {
      for (unsigned int i=0;i<GeometryInfo<3>::faces_per_cell;++i)
	{
	  if (cell->face(i)->boundary_indicator() == 255)
	    continue;
	  
	  if (cell->face(i)->center()(0) > 1.-1.e-5)
	    cell->face(i)->set_boundary_indicator(2);
	  else if (cell->face(i)->center()(0) < -1.+1.e-5)
	    cell->face(i)->set_boundary_indicator(1);
	}
      ++cell;
    }
}



void GridGenerator::hyper_shell (Triangulation<3>   &,
				 const Point<3>     &,
				 const double        ,
				 const double        ,
				 const unsigned int  )
{
  Assert (false, ExcNotImplemented());
}



void GridGenerator::cylinder_shell (Triangulation<3>   &tria,
                                    const double        length,
                                    const double        inner_radius,
                                    const double        outer_radius,
                                    const unsigned int  n_radial_cells,
                                    const unsigned int  n_axial_cells)
{
  Assert ((inner_radius > 0) && (inner_radius < outer_radius),
	  ExcInvalidRadii ());
  
  const double pi = deal_II_numbers::PI;
  
				   // determine the number of cells
				   // for the grid. if not provided by
				   // the user determine it such that
				   // the length of each cell on the
				   // median (in the middle between
				   // the two circles) is equal to its
				   // radial extent (which is the
				   // difference between the two
				   // radii)
  const unsigned int N_r = (n_radial_cells == 0 ?
                            static_cast<unsigned int>
                            (std::ceil((2*pi* (outer_radius + inner_radius)/2) /
                                       (outer_radius - inner_radius))) :
                            n_radial_cells);
  const unsigned int N_z = (n_axial_cells == 0 ?
                            static_cast<unsigned int>
                            (std::ceil (length /
                                        (2*pi*(outer_radius + inner_radius)/2/N_r))) :
                            n_axial_cells);

				   // set up N vertices on the
				   // outer and N vertices on
				   // the inner circle. the
				   // first N ones are on the
				   // outer one, and all are
				   // numbered counter-clockwise
  std::vector<Point<2> > vertices_2d(2*N_r);
  for (unsigned int i=0; i<N_r; ++i)
    {
      vertices_2d[i] = Point<2>( std::cos(2*pi * i/N_r),
                                 std::sin(2*pi * i/N_r)) * outer_radius;
      vertices_2d[i+N_r] = vertices_2d[i] * (inner_radius/outer_radius);
    };

  std::vector<Point<3> > vertices_3d;
  vertices_3d.reserve (2*N_r*(N_z+1));
  for (unsigned int j=0; j<=N_z; ++j)
    for (unsigned int i=0; i<2*N_r; ++i)
      {
        const Point<3> v (vertices_2d[i][0],
                          vertices_2d[i][1],
                          j*length/N_z);
        vertices_3d.push_back (v);
      }
                            
  std::vector<CellData<3> > cells (N_r*N_z, CellData<3>());
  
  for (unsigned int j=0; j<N_z; ++j)
    for (unsigned int i=0; i<N_r; ++i) 
      {
        cells[i+j*N_r].vertices[0] = i + j*2*N_r;
        cells[i+j*N_r].vertices[1] = (i+1)%N_r + j*2*N_r;
        cells[i+j*N_r].vertices[2] = N_r+((i+1)%N_r) + j*2*N_r;
        cells[i+j*N_r].vertices[3] = N_r+i + j*2*N_r;
        
        cells[i+j*N_r].vertices[4] = i + (j+1)*2*N_r;
        cells[i+j*N_r].vertices[5] = (i+1)%N_r + (j+1)*2*N_r;
        cells[i+j*N_r].vertices[6] = N_r+((i+1)%N_r) + (j+1)*2*N_r;
        cells[i+j*N_r].vertices[7] = N_r+i + (j+1)*2*N_r;
        
        cells[i+j*N_r].material_id = 0;
      }
  
  tria.create_triangulation (vertices_3d, cells, SubCellData());
}


#endif


#if deal_II_dimension == 1

void GridGenerator::laplace_transformation (Triangulation<1> &,
					    const std::map<unsigned int,Point<1> > &)
{
  Assert(false, ExcNotImplemented());
}

#else

template <int dim>
void GridGenerator::laplace_transformation (Triangulation<dim> &tria,
					    const std::map<unsigned int,Point<dim> > &new_points)
{
				   // first provide everything that is
				   // needed for solving a Laplace
				   // equation.  
  MappingQ1<dim> mapping_q1;  
  FE_Q<dim> q1(1);
  
  DoFHandler<dim> dof_handler(tria);
  dof_handler.distribute_dofs(q1);
  SparsityPattern sparsity_pattern (dof_handler.n_dofs (), dof_handler.n_dofs (),
				    dof_handler.max_couplings_between_dofs());
  DoFTools::make_sparsity_pattern(dof_handler, sparsity_pattern);
  sparsity_pattern.compress ();
  
  SparseMatrix<double> S(sparsity_pattern);
  
  QGauss4<dim> quadrature;
  
  MatrixCreator::create_laplace_matrix(mapping_q1, dof_handler, quadrature, S);

				   // set up the boundary values for
				   // the laplace problem
  std::vector<std::map<unsigned int,double> > m(dim);
  typename std::map<unsigned int,Point<dim> >::const_iterator map_iter;
  typename std::map<unsigned int,Point<dim> >::const_iterator map_end=new_points.end();

				   // fill these maps using the data
				   // given by new_points
  typename DoFHandler<dim>::cell_iterator cell=dof_handler.begin_active(),
					  endc=dof_handler.end();
  typename DoFHandler<dim>::face_iterator face;
  for (; cell!=endc; ++cell)
    {
      if (cell->at_boundary())
	for (unsigned int face_no=0; face_no<GeometryInfo<dim>::faces_per_cell; ++face_no)
	  {
	    face=cell->face(face_no);
	    if (face->at_boundary())
	      for (unsigned int vertex_no=0;
		   vertex_no<GeometryInfo<dim>::vertices_per_face; ++vertex_no)
		{
		  const unsigned int vertex_index=face->vertex_index(vertex_no);
		  map_iter=new_points.find(vertex_index);
		  Assert(map_iter!=map_end, ExcInternalError());
		  
		  for (unsigned int i=0; i<dim; ++i)
		    m[i].insert(std::pair<unsigned int,double> (
				  face->vertex_dof_index(vertex_no, 0), map_iter->second(i)));
		}
	  }
    }
  
				   // solve the dim problems with
				   // different right hand sides.
  Vector<double> us[dim];
  for (unsigned int i=0; i<dim; ++i)
    us[i].reinit (dof_handler.n_dofs());
  
				   // solve linear systems in parallel
  Threads::ThreadGroup<> threads;
  for (unsigned int i=0; i<dim; ++i)
    threads += Threads::spawn (&GridGenerator::laplace_solve)(S, m[i], us[i]);
  threads.join_all ();
  
				   // change the coordinates of the
				   // points of the triangulation
				   // according to the computed values
  for (cell=dof_handler.begin_active(); cell!=endc; ++cell)
    for (unsigned int vertex_no=0;
	 vertex_no<GeometryInfo<dim>::vertices_per_cell; ++vertex_no)
      {
	Point<dim> &v=cell->vertex(vertex_no);
	const unsigned int dof_index=cell->vertex_dof_index(vertex_no, 0);
	for (unsigned int i=0; i<dim; ++i)
	  v(i)=us[i](dof_index);
      }
}


#endif


// make the following function inline. this is so that it becomes marked
// internal/weak for the linker and we don't get multiply defined errors
// when linking with more than one dimension at a time. Usually we used
// the trick of putting these functions in a .all_dimensions.cc file, but
// this is not necessary here as this is an internal only function.
inline
void GridGenerator::laplace_solve (const SparseMatrix<double> &S,
				   const std::map<unsigned int,double> &m,
				   Vector<double> &u)
{
  const unsigned int n_dofs=S.n();
  FilteredMatrix<SparseMatrix<double> > SF (S);
  SolverControl control (1000, 1.e-10, false, false);
  PrimitiveVectorMemory<Vector<double> > mem;
  SolverCG<Vector<double> > solver (control, mem);
  PreconditionJacobi<FilteredMatrix<SparseMatrix<double> > > prec;
  Vector<double> f(n_dofs);
  
  SF.add_constraints(m);
  prec.initialize (SF);
  SF.apply_constraints (f, true);
  solver.solve(SF, u, f, prec);
}


// explicit instantiations
template void
GridGenerator::hyper_rectangle<deal_II_dimension> (Triangulation<deal_II_dimension> &,
						   const Point<deal_II_dimension>&,
						   const Point<deal_II_dimension>&,
						   const bool);
template void
GridGenerator::hyper_cube<deal_II_dimension> (Triangulation<deal_II_dimension> &,
					      const double,
					      const double);

template void
GridGenerator::subdivided_hyper_cube<deal_II_dimension> (Triangulation<deal_II_dimension> &,
                                                         const unsigned int,
                                                         const double,
                                                         const double);

template void
GridGenerator::subdivided_hyper_rectangle<deal_II_dimension> 
(Triangulation<deal_II_dimension> &,
 const std::vector<unsigned int>&,
 const Point<deal_II_dimension>&,
 const Point<deal_II_dimension>&,
 const bool);


#if deal_II_dimension != 1
template void
GridGenerator::
laplace_transformation<deal_II_dimension> (Triangulation<deal_II_dimension> &,
					   const std::map<unsigned int,Point<deal_II_dimension> > &);

#endif
