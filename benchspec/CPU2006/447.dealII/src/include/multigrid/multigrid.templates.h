//----------------------------  multigrid.templates.h  ---------------------------
//    $Id: multigrid.templates.h,v 1.1 2004/09/14 00:53:35 wolf Exp $
//    Version: $Name:  $
//
//    Copyright (C) 1998, 1999, 2000, 2001, 2002, 2003, 2004 by the deal.II authors
//
//    This file is subject to QPL and may not be  distributed
//    without copyright and license information. Please refer
//    to the file deal.II/doc/license.html for the  text  and
//    further information on this license.
//
//----------------------------  multigrid.templates.h  ---------------------------
#ifndef __deal2__multigrid_templates_h
#define __deal2__multigrid_templates_h
#include <multigrid/multigrid.h>


template <class VECTOR>
void
Multigrid<VECTOR>::set_edge_matrices (const MGMatrixBase<VECTOR>& down,
				      const MGMatrixBase<VECTOR>& up)
{
  edge_down = &down;
  edge_up = &up;
}




template <class VECTOR>
void
Multigrid<VECTOR>::level_mgstep(const unsigned int level)
{
  solution[level] = 0.;
  
  if (level == minlevel)
    {
      (*coarse)(level, solution[level], defect[level]);
      return;
    }

				   // smoothing of the residual by
				   // modifying s
  pre_smooth->smooth(level, solution[level], defect[level]);

				   // t = A*solution[level]
  matrix->vmult(level, t[level], solution[level]);
  
				   // make t rhs of lower level The
				   // non-refined parts of the
				   // coarse-level defect already
				   // contain the global defect, the
				   // refined parts its restriction.
  for (unsigned int l = level;l>minlevel;--l)
    {
      t[l-1] = 0.;
      if (l==level && edge_down != 0)
	{
	  edge_down->vmult(level, t[level-1], solution[level]);
	}
      transfer->restrict_and_add (l, t[l-1], t[l]);
      defect[l-1] -= t[l-1];
    }

				   // do recursion
  level_mgstep(level-1);

				   // reset size of the auxiliary
				   // vector, since it has been
				   // resized in the recursive call to
				   // level_mgstep directly above
  t[level] = 0.;

				   // do coarse grid correction

  transfer->prolongate(level, t[level], solution[level-1]);
  
  solution[level] += t[level];

  if (edge_up != 0)
    {
      edge_up->Tvmult(level, t[level], solution[level-1]);
      defect[level] -= t[level];
    }
  
				   // post-smoothing
  post_smooth->smooth(level, solution[level], defect[level]);
}


template <class VECTOR>
void
Multigrid<VECTOR>::vcycle()
{
				   // The defect vector has been
				   // initialized by copy_to_mg. Now
				   // adjust the other vectors.
  solution.resize(minlevel, maxlevel);
  t.resize(minlevel, maxlevel);
  
  for (unsigned int level=minlevel; level<=maxlevel;++level)
    {
      solution[level].reinit(defect[level]);
      t[level].reinit(defect[level]);
    }

  level_mgstep (maxlevel);
//  abort ();
}




#endif
