//----------------------------  multigrid.h  ---------------------------
//    $Id: multigrid.h,v 1.1 2004/09/14 00:53:35 wolf Exp $
//    Version: $Name:  $
//
//    Copyright (C) 1998, 1999, 2000, 2001, 2002, 2003, 2004 by the deal.II authors
//
//    This file is subject to QPL and may not be  distributed
//    without copyright and license information. Please refer
//    to the file deal.II/doc/license.html for the  text  and
//    further information on this license.
//
//----------------------------  multigrid.h  ---------------------------
#ifndef __deal2__multigrid_h
#define __deal2__multigrid_h


#include <base/config.h>
#include <base/subscriptor.h>
#include <base/smartpointer.h>
#include <lac/sparse_matrix.h>
#include <lac/vector.h>
#include <multigrid/mg_base.h>
#include <multigrid/mg_level_object.h>
#include <multigrid/mg_dof_handler.h>

#include <vector>


//TODO:[GK] Cleanup
//  move definitions of virtual destructors to mg_base.templates.h
//

/**
 * Implementation of the multigrid method.
 *
 * Warning: multigrid on locally refined meshes only works with
 * <b>discontinuous finite elements</b> right now. It is not clear,
 * whether the paradigm of local smoothing we use is applicable to
 * continuous elements with hanging nodes; in fact, most people you
 * meet on conferences seem to deny this.
 *
 * The function actually performing a multi-level cycle,
 * @p level_mgstep, as well as the function @p vcycle, calling it,
 * require several helper classes handed over as template parameters.
 * These classes have to meet the following requirements:
 *
 * <tt>MATRIX</tt> is a matrix as specified for LAC-solvers, that is, it
 * has a function
 * @code
 *   void MATRIX::vmult(VECTOR& x, const VECTOR& b)
 * @endcode
 * performing the matrix vector product <i>x=Ab</i>.
 *
 * <tt>SMOOTH</tt> is a class with a function
 * @code
 *   void SMOOTH::smooth (unsigned int level, VECTOR& x, const VECTOR& b)
 * @endcode
 * modifying the vector <i>x</i> such that the residual <i>b-Ax</i>
 * on level <i>l</i> is smoothened. Refer to MGSmootherRelaxation
 * for an example.
 *
 * <tt>COARSE</tt> has an
 * @code
 *   void COARSE::operator() (VECTOR& x, const VECTOR& b)
 * @endcode
 * returning the solution to the system <tt>Ax=b</tt> on the coarsest level
 * in <i>x</i>.
 * 
 * @author Guido Kanschat, 1999 - 2004
 */
template <class VECTOR>
class Multigrid : public Subscriptor
{
  public:
  typedef VECTOR vector_type;
  typedef const VECTOR const_vector_type;
  
				     /**
				      * Constructor. The
				      * @p MGDoFHandler is used to
				      * determine the highest possible
				      * level. @p transfer is an
				      * object performing prolongation
				      * and restriction.
				      *
				      * The V-cycle will start on
				      * level @p maxlevel and goes
				      * down to level @p minlevel,
				      * where the coarse grid solver
				      * will be used.
				      *
				      * This function already
				      * initializes the vectors which
				      * will be used later on in the
				      * course of the
				      * computations. You should
				      * therefore create objects of
				      * this type as late as possible.
				      */
  template <int dim>
    Multigrid(const MGDoFHandler<dim>& mg_dof_handler,
	      const MGMatrixBase<VECTOR>& matrix,
	      const MGCoarseGridBase<VECTOR>& coarse,
	      const MGTransferBase<VECTOR>& transfer,
	      const MGSmootherBase<VECTOR>& pre_smooth,
	      const MGSmootherBase<VECTOR>& post_smooth,
	      const unsigned int            minlevel = 0,
	      const unsigned int            maxlevel = 1000000);


				     /**
				      * Reinit this class according to
				      * @p minlevel and @p maxlevel.
				      */
    void reinit (const unsigned int minlevel,
		 const unsigned int maxlevel);

				     /**
				      * Execute one step of the
				      * v-cycle algorithm.  This
				      * function assumes, that the
				      * vector @p defect is properly
				      * filled with the residual in
				      * the outer defect correction
				      * scheme (usually performed by
				      * PreconditionMG). After
				      * execution of <tt>vcycle()</tt>, the
				      * result is in the vector
				      * @p solution. See
				      * <tt>copy_*_mg</tt> in class
				      * @p MGTools if you want to use
				      * these vectors yourself.
				      *
				      * The actual work for this
				      * function is done in
				      * @p level_mgstep.
				      */
    void vcycle();

				     /**
				      * Set additional matrices to
				      * correct residual computation
				      * at refinement edges.
				      */
    void set_edge_matrices (const MGMatrixBase<VECTOR>& edge_down,
			    const MGMatrixBase<VECTOR>& edge_up);

  private:
    
				     /**
				      * The actual v-cycle multigrid method.
				      * @p level is the level the
				      * function should work on. It
				      * will usually be called for the
				      * highest level from outside,
				      * but will then call itself
				      * recursively for @p level-1,
				      * unless we are on @p minlevel
				      * where instead of recursing
				      * deeper, the coarse grid solver
				      * is used to solve the matrix of
				      * this level.
				      */
    void level_mgstep (const unsigned int level);

				     /**
				      * Level for coarse grid solution.
				      */
    unsigned int minlevel;

				     /**
				      * Highest level of cells.
				      */
    unsigned int maxlevel;
    
				     /**
				      * Auxiliary vector. Contains the
				      * defect to be corrected before
				      * the multigrid step.
				      */
    MGLevelObject<VECTOR> defect;

				     /**
				      * Auxiliary vector. Contains the
				      * updated solution after the
				      * multigrid step.
				      */
    MGLevelObject<VECTOR> solution;
    
				     /**
				      * Auxiliary vector.
				      */
    MGLevelObject<VECTOR> t;    


  private:
				     /**
				      * The matrix for each level.
				      */
    SmartPointer<const MGMatrixBase<VECTOR> > matrix;
    
				     /**
				      * The matrix for each level.
				      */
    SmartPointer<const MGCoarseGridBase<VECTOR> > coarse;
    
				     /**
				      * Object for grid tranfer.
				      */
    SmartPointer<const MGTransferBase<VECTOR> > transfer;

				     /**
				      * The pre-smoothing object.
				      */
    SmartPointer<const MGSmootherBase<VECTOR> > pre_smooth;

				     /**
				      * The post-smoothing object.
				      */
    SmartPointer<const MGSmootherBase<VECTOR> > post_smooth;
    
				     /**
				      * Edge matrix from fine to coarse.
				      */
    SmartPointer<const MGMatrixBase<VECTOR> > edge_down;

				     /**
				      * Transpose edge matrix from coarse to fine.
				      */
    SmartPointer<const MGMatrixBase<VECTOR> > edge_up;

				     /**
				      * Exception.
				      */
    DeclException2(ExcSwitchedLevels, int, int,
		   << "minlevel and maxlevel switched, should be: "
		   << arg1 << "<=" << arg2);
    template<int dim, class VECTOR2, class TRANSFER> friend class PreconditionMG;
};


/**
 * Multi-level preconditioner.
 * Here, we collect all information needed for multi-level preconditioning
 * and provide the standard interface for LAC iterative methods.
 *
 * The template parameter class @p MG is required to inherit @p MGBase.
 * Furthermore, it needs functions <tt>void copy_to_mg(const VECTOR&)</tt>
 * to store @p src in the right hand side of the multi-level method and
 * <tt>void copy_from_mg(VECTOR&)</tt> to store the result of the v-cycle in @p dst.
 *
 * @author Guido Kanschat, 1999, 2000, 2001, 2002
 */
template<int dim, class VECTOR, class TRANSFER>
class PreconditionMG : public Subscriptor
{
  public:
				     /**
				      * Constructor.
				      * Arguments are the multigrid object,
				      * pre-smoother, post-smoother and
				      * coarse grid solver.
				      */
    PreconditionMG(const MGDoFHandler<dim>&     mg_dof,
		   Multigrid<VECTOR>&           mg,
		   const TRANSFER& transfer);

				     /**
				      * Dummy function needed by other classes.
				      */
    bool empty () const;
    
				     /**
				      * Preconditioning operator.
				      * Calls the @p vcycle function
				      * of the @p MG object passed to
				      * the constructor.
				      *
				      * This is the operator used by
				      * LAC iterative solvers.
				      */
    template<class VECTOR2>
    void vmult (VECTOR2       &dst,
		const VECTOR2 &src) const;
    
				     /**
				      * Preconditioning operator.
				      * Calls the @p vcycle function
				      * of the @p MG object passed to
				      * the constructor.
				      */
    template<class VECTOR2>
    void vmult_add (VECTOR2       &dst,
		    const VECTOR2 &src) const;
    
				     /**
				      * Tranposed preconditioning operator.
				      *
				      * Not implemented, but the
				      * definition may be needed.
				      */
    template<class VECTOR2>
    void Tvmult (VECTOR2       &dst,
		const VECTOR2 &src) const;
    
				     /**
				      * Tranposed preconditioning operator.
				      *
				      * Not implemented, but the
				      * definition may be needed.
				      */
    template<class VECTOR2>
    void Tvmult_add (VECTOR2       &dst,
		     const VECTOR2 &src) const;
    
  private:
				     /**
				      * Associated @p MGDoFHandler.
				      */
    SmartPointer<const MGDoFHandler<dim> > mg_dof_handler;

				     /**
				      * The multigrid object.
				      */
    SmartPointer<Multigrid<VECTOR> > multigrid;
    
				   /**
				    * Object for grid tranfer.
				    */
    SmartPointer<const TRANSFER> transfer;  
};


/* --------------------------- inline functions --------------------- */


template <class VECTOR>
template <int dim>
Multigrid<VECTOR>::Multigrid (const MGDoFHandler<dim>& mg_dof_handler,
			      const MGMatrixBase<VECTOR>& matrix,
			      const MGCoarseGridBase<VECTOR>& coarse,
			      const MGTransferBase<VECTOR>& transfer,
			      const MGSmootherBase<VECTOR>& pre_smooth,
			      const MGSmootherBase<VECTOR>& post_smooth,
			      const unsigned int min_level,
			      const unsigned int max_level)
		:
		minlevel(min_level),
		maxlevel(std::min(mg_dof_handler.get_tria().n_levels()-1,
				  max_level)),
		defect(minlevel,maxlevel),
		solution(minlevel,maxlevel),
		t(minlevel,maxlevel),
		matrix(&matrix),
		coarse(&coarse),
		transfer(&transfer),
		pre_smooth(&pre_smooth),
		post_smooth(&post_smooth),
		edge_down(0),
		edge_up(0)
{}


template <class VECTOR>
void
Multigrid<VECTOR>::reinit(const unsigned int min_level,
			  const unsigned int max_level)
{
  minlevel=min_level;
  maxlevel=max_level;
  defect.resize(minlevel, maxlevel);
  solution.resize(minlevel, maxlevel);
  t.resize(minlevel, maxlevel);
}

  
/* --------------------------- inline functions --------------------- */


template<int dim, class VECTOR, class TRANSFER>
PreconditionMG<dim, VECTOR, TRANSFER>
::PreconditionMG(const MGDoFHandler<dim>&      mg_dof_handler,
		 Multigrid<VECTOR>& mg,
		 const TRANSFER&              transfer)
		:
  mg_dof_handler(&mg_dof_handler),
  multigrid(&mg),
  transfer(&transfer)
{}

template<int dim, class VECTOR, class TRANSFER>
inline bool
PreconditionMG<dim, VECTOR, TRANSFER>::empty () const
{
  return false;
}

template<int dim, class VECTOR, class TRANSFER>
template<class VECTOR2>
void
PreconditionMG<dim, VECTOR, TRANSFER>::vmult (
  VECTOR2& dst,
  const VECTOR2& src) const
{
  transfer->copy_to_mg(*mg_dof_handler,
		       multigrid->defect,
		       src);
  
  multigrid->vcycle();

  transfer->copy_from_mg(*mg_dof_handler,
			 dst,
			 multigrid->solution);
}


template<int dim, class VECTOR, class TRANSFER>
template<class VECTOR2>
void
PreconditionMG<dim, VECTOR, TRANSFER>::vmult_add (
  VECTOR2& dst,
  const VECTOR2& src) const
{
  transfer->copy_to_mg(*mg_dof_handler,
		       multigrid->defect,
		       src);
  
  multigrid->vcycle();

  transfer->copy_from_mg_add(*mg_dof_handler,
			     dst,
			     multigrid->solution);
}


template<int dim, class VECTOR, class TRANSFER>
template<class VECTOR2>
void
PreconditionMG<dim, VECTOR, TRANSFER>::Tvmult (
  VECTOR2&,
  const VECTOR2&) const
{
  Assert(false, ExcNotImplemented());
}


template<int dim, class VECTOR, class TRANSFER>
template<class VECTOR2>
void
PreconditionMG<dim, VECTOR, TRANSFER>::Tvmult_add (
  VECTOR2&,
  const VECTOR2&) const
{
  Assert(false, ExcNotImplemented());
}


#endif
