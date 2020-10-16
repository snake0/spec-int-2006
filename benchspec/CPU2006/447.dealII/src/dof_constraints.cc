//----------------------------  dof_constraints.cc  ---------------------------
//    $Id: dof_constraints.cc,v 1.1 2004/09/14 00:51:21 wolf Exp $
//    Version: $Name:  $
//
//    Copyright (C) 1998, 1999, 2000, 2001, 2002, 2003, 2004 by the deal.II authors
//
//    This file is subject to QPL and may not be  distributed
//    without copyright and license information. Please refer
//    to the file deal.II/doc/license.html for the  text  and
//    further information on this license.
//
//----------------------------  dof_constraints.cc  ---------------------------


#include <dofs/dof_constraints.h>
#include <dofs/dof_constraints.templates.h>

#include <base/memory_consumption.h>
#include <lac/sparsity_pattern.h>
#include <lac/compressed_sparsity_pattern.h>
#include <lac/vector.h>
#include <lac/petsc_vector.h>
#include <lac/petsc_sparse_matrix.h>
#include <lac/petsc_parallel_vector.h>
#include <lac/petsc_parallel_sparse_matrix.h>
#include <lac/block_vector.h>
#include <lac/sparse_matrix.h>
#include <lac/block_sparse_matrix.h>

#include <algorithm>
#include <numeric>
#include <set>

// we only need output streams, but older compilers did not provide
// them in a separate include file
#ifdef HAVE_STD_OSTREAM_HEADER
#  include <ostream>
#else
#  include <iostream>
#endif


inline
bool
ConstraintMatrix::check_zero_weight (const std::pair<unsigned int, double> &p)
{
  return (p.second == 0);
}



inline
bool
ConstraintMatrix::ConstraintLine::operator < (const ConstraintLine &a) const
{
  return line < a.line;
}



inline
bool
ConstraintMatrix::ConstraintLine::operator == (const ConstraintLine &a) const
{
  return line == a.line;
}



unsigned int
ConstraintMatrix::ConstraintLine::memory_consumption () const
{
  return (MemoryConsumption::memory_consumption (line) +
	  MemoryConsumption::memory_consumption (entries));
}



ConstraintMatrix::ConstraintMatrix () :
		lines(),
		sorted(false)
{}


void ConstraintMatrix::add_line (const unsigned int line)
{
  Assert (sorted==false, ExcMatrixIsClosed());

				   // check whether line already exists;
				   // it may, but then we need to quit
  for (unsigned int i=0; i!=lines.size(); ++i)
    if (lines[i].line == line)
      return;

				   // push a new line to the end of the
				   // list
  lines.push_back (ConstraintLine());
  lines.back().line = line;
}



void
ConstraintMatrix::add_entry (const unsigned int line,
                             const unsigned int column,
                             const double       value)
{
  Assert (sorted==false, ExcMatrixIsClosed());

  std::vector<ConstraintLine>::iterator line_ptr;
  const std::vector<ConstraintLine>::const_iterator start=lines.begin();
				   // the usual case is that the line where
				   // a value is entered is the one we
				   // added last, so we search backward
  for (line_ptr=(lines.end()-1); line_ptr!=start; --line_ptr)
    if (line_ptr->line == line)
      break;

				   // if the loop didn't break, then
				   // line_ptr must be begin().
				   // we have an error if that doesn't
				   // point to 'line' then
  Assert (line_ptr->line==line, ExcLineInexistant(line));

				   // if in debug mode, check whether an
				   // entry for this column already
				   // exists and if its the same as
				   // the one entered at present
				   //
				   // in any case: exit the function if an
				   // entry for this column already exists,
				   // since we don't want to enter it twice
  for (std::vector<std::pair<unsigned int,double> >::const_iterator
         p=line_ptr->entries.begin();
       p != line_ptr->entries.end(); ++p)
    if (p->first == column)
      {
	Assert (p->second == value,
		ExcEntryAlreadyExists(line, column, p->second, value));
	return;
      };
  
  line_ptr->entries.push_back (std::make_pair(column,value));
}



void
ConstraintMatrix::add_entries (const unsigned int                        line,
                               const std::vector<std::pair<unsigned int,double> > &col_val_pairs)
{
  Assert (sorted==false, ExcMatrixIsClosed());

  std::vector<ConstraintLine>::iterator line_ptr;
  const std::vector<ConstraintLine>::const_iterator start=lines.begin();
				   // the usual case is that the line where
				   // a value is entered is the one we
				   // added last, so we search backward
  for (line_ptr=(lines.end()-1); line_ptr!=start; --line_ptr)
    if (line_ptr->line == line)
      break;

				   // if the loop didn't break, then
				   // line_ptr must be begin().
				   // we have an error if that doesn't
				   // point to 'line' then
  Assert (line_ptr->line==line, ExcLineInexistant(line));

				   // if in debug mode, check whether an
				   // entry for this column already
				   // exists and if its the same as
				   // the one entered at present
				   //
				   // in any case: skip this entry if
				   // an entry for this column already
				   // exists, since we don't want to
				   // enter it twice
  for (std::vector<std::pair<unsigned int,double> >::const_iterator
         col_val_pair = col_val_pairs.begin();
       col_val_pair!=col_val_pairs.end(); ++col_val_pair)
    {
      for (std::vector<std::pair<unsigned int,double> >::const_iterator
             p=line_ptr->entries.begin();
	   p != line_ptr->entries.end(); ++p)
	if (p->first == col_val_pair->first)
	  {
					     // entry exists, break
					     // innermost loop
	    Assert (p->second == col_val_pair->second,
		    ExcEntryAlreadyExists(line, col_val_pair->first,
					  p->second, col_val_pair->second));
	    break;
	  };
      
      line_ptr->entries.push_back (*col_val_pair);
    };
}



void ConstraintMatrix::close ()
{
  if (sorted == true)
    return;
  
				   // sort the entries in the different lines
				   // and strip zero entries
  std::vector<ConstraintLine>::iterator line = lines.begin(),
					endl = lines.end();
  for (; line!=endl; ++line)
    {
				       // first remove zero
				       // entries. that would mean
				       // that in the linear
				       // constraint for a node,
				       // x_i = ax_1 + bx_2 + ...,
				       // another node times 0
				       // appears. obviously,
				       // 0*something can be omitted
      line->entries.erase (std::remove_if (line->entries.begin(),
                                           line->entries.end(),
                                           &check_zero_weight),
                           line->entries.end());

				       // now sort the remainder
      std::sort (line->entries.begin(), line->entries.end());

                                       // finally do the following check: if
                                       // the sum of weights for the
                                       // constraints is close to one, but not
                                       // exactly one, then rescale all the
                                       // weights so that they sum up to
                                       // 1. this adds a little numerical
                                       // stability and avoids all sorts of
                                       // problems where the actual value is
                                       // close to, but not quite what we
                                       // expected
                                       //
                                       // the case where the weights don't
                                       // quite sum up happens when we compute
                                       // the interpolation weights "on the
                                       // fly", i.e. not from precomputed
                                       // tables. in this case, the
                                       // interpolation weights are also
                                       // subject to round-off
      double sum = 0;
      for (unsigned int i=0; i<line->entries.size(); ++i)
        sum += line->entries[i].second;
      if ((sum != 1.0) && (std::fabs (sum-1.) < 1.e-13))
        for (unsigned int i=0; i<line->entries.size(); ++i)
          line->entries[i].second /= sum;
    };
  
				   // sort the lines
  std::sort (lines.begin(), lines.end());

#ifdef DEBUG
				   // if in debug mode: check that no
				   // dof is constraint to another dof
				   // that is also constrained
  for (std::vector<ConstraintLine>::const_iterator line=lines.begin();
       line!=lines.end(); ++line)
    for (std::vector<std::pair<unsigned int,double> >::const_iterator entry=line->entries.begin();
	 entry!=line->entries.end(); ++entry)
      {
					 // make sure that
					 // entry->first is not the
					 // index of a line itself
	ConstraintLine test_line;
	test_line.line = entry->first;
	const std::vector<ConstraintLine>::const_iterator
	  test_line_position = std::lower_bound (lines.begin(),
						 lines.end(),
						 test_line);
	Assert ((test_line_position == lines.end())
		||
		(test_line_position->line != entry->first),
		ExcDoFConstrainedToConstrainedDoF(line->line, entry->first));
      };
#endif
  
  sorted = true;
}



void ConstraintMatrix::merge (const ConstraintMatrix &other_constraints)
{
				   // first check whether the
				   // constraints in the two objects
				   // are for different degrees of
				   // freedom
  if (true)
    {
				       // first insert all dofs in
				       // this object into a list...
      std::set<unsigned int> this_dofs;
      for (std::vector<ConstraintLine>::const_iterator line=lines.begin();
	   line!=lines.end(); ++line)
	this_dofs.insert (line->line);

				       // ...then check whether it
				       // appears in the other object
				       // as well. note that we have
				       // to do this in a somewhat
				       // complicated style since the
				       // two objects may not be
				       // sorted
      for (std::vector<ConstraintLine>::const_iterator
	     line=other_constraints.lines.begin();
	   line!=other_constraints.lines.end(); ++line)
	AssertThrow (this_dofs.find (line->line) == this_dofs.end(),
		     ExcDoFIsConstrainedFromBothObjects (line->line));

				       // finally check the following:
				       // while we allow that in this
				       // object nodes are constrained
				       // to other nodes that are
				       // constrained in the given
				       // argument, we do not allow
				       // the reverse, i.e. the nodes
				       // to which the constraints in
				       // the other object hold may
				       // not be constrained here
      for (std::vector<ConstraintLine>::const_iterator
	     line=other_constraints.lines.begin();
	   line!=other_constraints.lines.end(); ++line)
	for (std::vector<std::pair<unsigned int,double> >::const_iterator
	     e=line->entries.begin();
	   e!=line->entries.end(); ++e)
	  AssertThrow (this_dofs.find (e->first) == this_dofs.end(),
		       ExcDoFIsConstrainedToConstrainedDoF (e->first));
    };

				   // store the previous state with
				   // respect to sorting
  const bool object_was_sorted = sorted;
  sorted = false;


				   // first action is to fold into the
				   // present object possible
				   // constraints in the second
				   // object. for this, loop over all
				   // constraints and replace the
				   // constraint lines with a new one
				   // where constraints are replaced
				   // if necessary. use the same tmp
				   // object over again to avoid
				   // excessive memory allocation
  std::vector<std::pair<unsigned int,double> > tmp;
  std::vector<std::vector<ConstraintLine>::const_iterator> tmp_other_lines;
  for (std::vector<ConstraintLine>::iterator line=lines.begin();
       line!=lines.end(); ++line) 
    {
				       // copy the line of old object
				       // modulo dofs constrained in
				       // the second object. for this
				       // purpose, first search the
				       // respective constraint line
				       // (if any, otherwise a null
				       // pointer) in the other object
				       // for each of the entries in
				       // this line
				       //
				       // store whether we have to
				       // resolve entries, since if
				       // not there is no need to copy
				       // the line one-to-one
      tmp.clear ();
      tmp_other_lines.clear ();
      tmp_other_lines.reserve (line->entries.size());

      
      bool entries_to_resolve = false;
      
      for (unsigned int i=0; i<line->entries.size(); ++i)
	{
	  if (other_constraints.sorted == true)
	    {
					       // as the array is
					       // sorted, use a
					       // bindary find to
					       // check for the
					       // existence of the
					       // element. if it does
					       // not exist, then the
					       // pointer may still
					       // point into tha
					       // array, but to an
					       // element of which the
					       // indices do not
					       // match, so return the
					       // end iterator
	      ConstraintLine index_comparison;
	      index_comparison.line = line->entries[i].first;

	      std::vector<ConstraintLine>::const_iterator
		it = std::lower_bound (other_constraints.lines.begin (),
				       other_constraints.lines.end (),
				       index_comparison);
	      if ((it != other_constraints.lines.end ()) &&
		  (it->line != index_comparison.line))
		it = other_constraints.lines.end ();

	      tmp_other_lines.push_back (it);
	    }
	  else
	    {
	      std::vector<ConstraintLine>::const_iterator
		it = other_constraints.lines.end ();
	      
	      for (std::vector<ConstraintLine>::const_iterator
		     p=other_constraints.lines.begin();
		   p!=other_constraints.lines.end(); ++p)
		if (p->line == line->entries[i].first)
		  {
		    it = p;
		    break;
		  }

	      tmp_other_lines.push_back (it);
	    };
	  
	  if (tmp_other_lines.back() != other_constraints.lines.end ())
	    entries_to_resolve = true;
	};
      Assert (tmp_other_lines.size() == line->entries.size(),
	      ExcInternalError());

				       // now we have for each entry
				       // in the present line whether
				       // it needs to be resolved
				       // using the new object, and if
				       // so which constraint line to
				       // use. first check whether we
				       // have to resolve anything at
				       // all, otherwise leave the
				       // line as is
      if (entries_to_resolve == false)
	continue;

				       // something to resolve, so go
				       // about it
      for (unsigned int i=0; i<line->entries.size(); ++i)
	{
					   // if the present dof is not
					   // constrained, then simply
					   // copy it over
	  if (tmp_other_lines[i] == other_constraints.lines.end())
	    tmp.push_back(line->entries[i]);
	  else
					     // otherwise resolve
					     // further constraints by
					     // replacing the old
					     // entry by a sequence of
					     // new entries taken from
					     // the other object, but
					     // with multiplied
					     // weights
	    {
	      Assert (tmp_other_lines[i]->line == line->entries[i].first,
		      ExcInternalError());
	      
	      const double weight = line->entries[i].second;
	      for (std::vector<std::pair<unsigned int, double> >::const_iterator
		     j=tmp_other_lines[i]->entries.begin();
		   j!=tmp_other_lines[i]->entries.end(); ++j)
		tmp.push_back (std::make_pair(j->first, j->second*weight));
	    };
	};
				       // finally exchange old and
				       // newly resolved line
      line->entries.swap (tmp);
    };
  
      
  
				   // next action: append new lines at
				   // the end
  lines.insert (lines.end(),
		other_constraints.lines.begin(),
		other_constraints.lines.end());

				   // if the object was sorted before,
				   // then make sure it is so
				   // afterwards as well. otherwise
				   // leave everything in the unsorted
				   // state
  if (object_was_sorted == true)
    close ();
}



void ConstraintMatrix::shift (const unsigned int offset)
{
  for (std::vector<ConstraintLine>::iterator i = lines.begin();
       i != lines.end(); i++)
    {
      i->line += offset;
      for (std::vector<std::pair<unsigned int,double> >::iterator 
	     j = i->entries.begin();
	   j != i->entries.end(); j++)
	j->first += offset;
    }
}



void ConstraintMatrix::clear ()
{
  std::vector<ConstraintLine> tmp;
  lines.swap (tmp);
  sorted = false;
}



void ConstraintMatrix::condense (const SparsityPattern &uncondensed,
				 SparsityPattern       &condensed) const
{
  Assert (sorted == true, ExcMatrixNotClosed());
  Assert (uncondensed.is_compressed() == true, ExcMatrixNotClosed());
  Assert (uncondensed.n_rows() == uncondensed.n_cols(),
	  ExcMatrixNotSquare());


				   // store for each line of the matrix
				   // its new line number
				   // after compression. If the shift is
				   // -1, this line will be condensed away
  std::vector<int> new_line;

  new_line.reserve (uncondensed.n_rows());

  std::vector<ConstraintLine>::const_iterator next_constraint = lines.begin();
  unsigned int                                shift           = 0;
  unsigned int n_rows = uncondensed.n_rows();

  if (next_constraint == lines.end()) 
				     // if no constraint is to be handled
    for (unsigned int row=0; row!=n_rows; ++row)
      new_line.push_back (row);
  else
    for (unsigned int row=0; row!=n_rows; ++row) 
      if (row == next_constraint->line)
	{
					   // this line is constrained
	  new_line.push_back (-1);
					   // note that @p{lines} is ordered	  
	  ++shift;
	  ++next_constraint;
	  if (next_constraint == lines.end())
					     // nothing more to do; finish rest
					     // of loop
	    {
	      for (unsigned int i=row+1; i<n_rows; ++i)
		new_line.push_back (i-shift);
	      break;
	    };
	}
      else
	new_line.push_back (row-shift);


  next_constraint = lines.begin();
				   // note: in this loop we need not check
				   // whether @p{next_constraint} is a valid
				   // iterator, since @p{next_constraint} is
				   // only evaluated so often as there are
				   // entries in new_line[*] which tells us
				   // which constraints exist
  for (unsigned int row=0; row<uncondensed.n_rows(); ++row)
    if (new_line[row] != -1)
				       // line not constrained
				       // copy entries if column will not
				       // be condensed away, distribute
				       // otherwise
      for (unsigned int j=uncondensed.get_rowstart_indices()[row];
	   j<uncondensed.get_rowstart_indices()[row+1]; ++j)
	if (new_line[uncondensed.get_column_numbers()[j]] != -1)
	  condensed.add (new_line[row], new_line[uncondensed.get_column_numbers()[j]]);
	else 
	  {
					     // let c point to the constraint
					     // of this column
	    std::vector<ConstraintLine>::const_iterator c = lines.begin();
	    while (c->line != uncondensed.get_column_numbers()[j])
	      ++c;

	    for (unsigned int q=0; q!=c->entries.size(); ++q) 
	      condensed.add (new_line[row], new_line[c->entries[q].first]);
	  }
    else
				       // line must be distributed
      {
	for (unsigned int j=uncondensed.get_rowstart_indices()[row];
	     j<uncondensed.get_rowstart_indices()[row+1]; ++j)
					   // for each entry: distribute
	  if (new_line[uncondensed.get_column_numbers()[j]] != -1)
					     // column is not constrained
	    for (unsigned int q=0; q!=next_constraint->entries.size(); ++q) 
	      condensed.add (new_line[next_constraint->entries[q].first],
			     new_line[uncondensed.get_column_numbers()[j]]);
	
	  else
					     // not only this line but
					     // also this col is constrained
	    {
					       // let c point to the constraint
					       // of this column
	      std::vector<ConstraintLine>::const_iterator c = lines.begin();
	      while (c->line != uncondensed.get_column_numbers()[j]) ++c;
	      
	      for (unsigned int p=0; p!=c->entries.size(); ++p)
		for (unsigned int q=0; q!=next_constraint->entries.size(); ++q)
		  condensed.add (new_line[next_constraint->entries[q].first],
				 new_line[c->entries[p].first]);
	    };
	
	++next_constraint;
      };

  condensed.compress();
}



void ConstraintMatrix::condense (SparsityPattern &sparsity) const
{
  Assert (sorted == true, ExcMatrixNotClosed());
  Assert (sparsity.is_compressed() == false, ExcMatrixIsClosed());
  Assert (sparsity.n_rows() == sparsity.n_cols(),
	  ExcMatrixNotSquare());
  
				   // store for each index whether it must be
				   // distributed or not. If entry is
				   // deal_II_numbers::invalid_unsigned_int,
				   // no distribution is necessary.
				   // otherwise, the number states which line
				   // in the constraint matrix handles this
				   // index
  std::vector<unsigned int> distribute(sparsity.n_rows(),
                                       deal_II_numbers::invalid_unsigned_int);
  
  for (unsigned int c=0; c<lines.size(); ++c)
    distribute[lines[c].line] = c;

  const unsigned int n_rows = sparsity.n_rows();
  for (unsigned int row=0; row<n_rows; ++row)
    {
      if (distribute[row] == deal_II_numbers::invalid_unsigned_int)
	{
					   // regular line. loop over cols all
					   // valid cols. note that this
					   // changes the line we are
					   // presently working on: we add
					   // additional entries. these are
					   // put to the end of the
					   // row. however, as constrained
					   // nodes cannot be constrained to
					   // other constrained nodes, nothing
					   // will happen if we run into these
					   // added nodes, as they can't be
					   // distributed further. we might
					   // store the position of the last
					   // old entry and stop work there,
					   // but since operating on the newly
					   // added ones only takes two
					   // comparisons (column index valid,
					   // distribute[column] necessarily
					   // ==deal_II_numbers::invalid_unsigned_int),
					   // it is cheaper to not do so and
					   // run right until the end of the
					   // line
          for (SparsityPattern::iterator entry = sparsity.begin(row);
               ((entry != sparsity.end(row)) &&
                entry->is_valid_entry());
               ++entry)
	    {
	      const unsigned int column = entry->column();

              if (distribute[column] != deal_II_numbers::invalid_unsigned_int)
                {
                                                   // distribute entry
                                                   // at regular row
                                                   // @p{row} and
                                                   // irregular column
                                                   // sparsity.colnums[j]
                  for (unsigned int q=0;
                       q!=lines[distribute[column]].entries.size();
                       ++q) 
                    sparsity.add (row,
                                  lines[distribute[column]].entries[q].first);
                }
	    }
	}
      else
					 // row must be
					 // distributed. note that
					 // here the present row is
					 // not touched (unlike above)
	{
          for (SparsityPattern::iterator entry = sparsity.begin(row);
               (entry != sparsity.end(row)) && entry->is_valid_entry(); ++entry)
            {
              const unsigned int column = entry->column();
              if (distribute[column] == deal_II_numbers::invalid_unsigned_int)
                                                 // distribute entry at irregular
                                                 // row @p{row} and regular column
                                                 // sparsity.colnums[j]
                for (unsigned int q=0;
                     q!=lines[distribute[row]].entries.size(); ++q) 
                  sparsity.add (lines[distribute[row]].entries[q].first,
                                column);
              else
                                                 // distribute entry at irregular
                                                 // row @p{row} and irregular column
                                                 // sparsity.get_column_numbers()[j]
                for (unsigned int p=0; p!=lines[distribute[row]].entries.size(); ++p)
                  for (unsigned int q=0;
                       q!=lines[distribute[column]].entries.size(); ++q)
                    sparsity.add (lines[distribute[row]].entries[p].first,
                                  lines[distribute[column]].entries[q].first);
            }
	}
    }
  
  sparsity.compress();
}



void ConstraintMatrix::condense (CompressedSparsityPattern &sparsity) const
{
  Assert (sorted == true, ExcMatrixNotClosed());
  Assert (sparsity.n_rows() == sparsity.n_cols(),
	  ExcMatrixNotSquare());
  
				   // store for each index whether it must be
				   // distributed or not. If entry is
				   // deal_II_numbers::invalid_unsigned_int,
				   // no distribution is necessary.
				   // otherwise, the number states which line
				   // in the constraint matrix handles this
				   // index
  std::vector<unsigned int> distribute(sparsity.n_rows(),
                                       deal_II_numbers::invalid_unsigned_int);
  
  for (unsigned int c=0; c<lines.size(); ++c)
    distribute[lines[c].line] = c;

  const unsigned int n_rows = sparsity.n_rows();
  for (unsigned int row=0; row<n_rows; ++row)
    {
      if (distribute[row] == deal_II_numbers::invalid_unsigned_int)
					 // regular line. loop over
					 // cols. note that as we
					 // proceed to distribute
					 // cols, the loop may get
					 // longer
	for (unsigned int j=0; j<sparsity.row_length(row); ++j)
	  {
	    const unsigned int column = sparsity.column_number(row,j);

 	    if (distribute[column] != deal_II_numbers::invalid_unsigned_int)
	      {
						 // distribute entry
						 // at regular row
						 // @p{row} and
						 // irregular column
						 // column. note that
						 // this changes the
						 // line we are
						 // presently working
						 // on: we add
						 // additional
						 // entries. if we add
						 // another entry at a
						 // column behind the
						 // present one, we
						 // will encounter it
						 // later on (but
						 // since it can't be
						 // further
						 // constrained, won't
						 // have to do
						 // anything about
						 // it). if we add it
						 // up front of the
						 // present column, we
						 // will find the
						 // present column
						 // later on again as
						 // it was shifted
						 // back (again
						 // nothing happens,
						 // in particular no
						 // endless loop, as
						 // when we encounter
						 // it the second time
						 // we won't be able
						 // to add more
						 // entries as they
						 // all already exist,
						 // but we do the same
						 // work more often
						 // than necessary,
						 // and the loop gets
						 // longer), so move
						 // the cursor one to
						 // the right in the
						 // case that we add
						 // an entry up front
						 // that did not exist
						 // before. check
						 // whether it existed
						 // before by tracking
						 // the length of this
						 // row
		unsigned int old_rowlength = sparsity.row_length(row);
		for (unsigned int q=0;
		     q!=lines[distribute[column]].entries.size();
		     ++q) 
		  {
		    const unsigned int
		      new_col = lines[distribute[column]].entries[q].first;
		    
		    sparsity.add (row, new_col);

		    const unsigned int new_rowlength = sparsity.row_length(row);
		    if ((new_col < column) && (old_rowlength != new_rowlength))
		      ++j;
		    old_rowlength = new_rowlength;
		  };
	      };
	  }
      else
					 // row must be distributed
	for (unsigned int j=0; j<sparsity.row_length(row); ++j)
	  {
	    const unsigned int column = sparsity.column_number(row,j);

	    if (distribute[column] == deal_II_numbers::invalid_unsigned_int)
					       // distribute entry at irregular
					       // row @p{row} and regular column
					       // sparsity.colnums[j]
	      for (unsigned int q=0;
		   q!=lines[distribute[row]].entries.size(); ++q) 
		sparsity.add (lines[distribute[row]].entries[q].first,
			      column);
	    else
					       // distribute entry at irregular
					       // row @p{row} and irregular column
					       // sparsity.get_column_numbers()[j]
	      for (unsigned int p=0; p!=lines[distribute[row]].entries.size(); ++p)
		for (unsigned int q=0;
		     q!=lines[distribute[sparsity.column_number(row,j)]]
				    .entries.size(); ++q)
		  sparsity.add (lines[distribute[row]].entries[p].first,
				lines[distribute[sparsity.column_number(row,j)]]
				.entries[q].first);
	  };
    };
}



void ConstraintMatrix::condense (BlockSparsityPattern &sparsity) const
{
  Assert (sorted == true, ExcMatrixNotClosed());
  Assert (sparsity.is_compressed() == false, ExcMatrixIsClosed());
  Assert (sparsity.n_rows() == sparsity.n_cols(),
	  ExcMatrixNotSquare());
  Assert (sparsity.n_block_rows() == sparsity.n_block_cols(),
	  ExcMatrixNotSquare());
  Assert (sparsity.get_column_indices() == sparsity.get_row_indices(),
	  ExcMatrixNotSquare());
  
  const BlockIndices &
    index_mapping = sparsity.get_column_indices();

  const unsigned int n_blocks = sparsity.n_block_rows();
  
				   // store for each index whether it must be
				   // distributed or not. If entry is
				   // deal_II_numbers::invalid_unsigned_int,
				   // no distribution is necessary.
				   // otherwise, the number states which line
				   // in the constraint matrix handles this
				   // index
  std::vector<unsigned int> distribute (sparsity.n_rows(),
                                        deal_II_numbers::invalid_unsigned_int);
  
  for (unsigned int c=0; c<lines.size(); ++c)
    distribute[lines[c].line] = c;

  const unsigned int n_rows = sparsity.n_rows();
  for (unsigned int row=0; row<n_rows; ++row)
    {
				       // get index of this row
				       // within the blocks
      const std::pair<unsigned int,unsigned int>
	block_index = index_mapping.global_to_local(row);
      const unsigned int block_row = block_index.first;
      
      if (distribute[row] == deal_II_numbers::invalid_unsigned_int)
					 // regular line. loop over
					 // all columns and see
					 // whether this column must
					 // be distributed
	{

					   // to loop over all entries
					   // in this row, we have to
					   // loop over all blocks in
					   // this blockrow and the
					   // corresponding row
					   // therein
	  for (unsigned int block_col=0; block_col<n_blocks; ++block_col)
	    {
	      const SparsityPattern &
		block_sparsity = sparsity.block(block_row, block_col);

              for (SparsityPattern::const_iterator
                     entry = block_sparsity.begin(block_index.second);
                   (entry != block_sparsity.end(block_index.second)) &&
                     entry->is_valid_entry();
                   ++entry)
                {
                  const unsigned int global_col
                    = index_mapping.local_to_global(block_col, entry->column());
                  
                  if (distribute[global_col] != deal_II_numbers::invalid_unsigned_int)
                                                     // distribute entry at regular
                                                     // row @p{row} and irregular column
                                                     // global_col
                    {
                      for (unsigned int q=0;
                           q!=lines[distribute[global_col]].entries.size(); ++q)
                        sparsity.add (row,
                                      lines[distribute[global_col]].entries[q].first);
                    }
                }
	    }
	}
      else
	{
					   // row must be
					   // distributed. split the
					   // whole row into the
					   // chunks defined by the
					   // blocks
	  for (unsigned int block_col=0; block_col<n_blocks; ++block_col)
	    {
	      const SparsityPattern &
		block_sparsity = sparsity.block(block_row,block_col);
	      
              for (SparsityPattern::const_iterator
                     entry = block_sparsity.begin(block_index.second);
                   (entry != block_sparsity.end(block_index.second)) &&
                     entry->is_valid_entry();
                   ++entry)
                {
                  const unsigned int global_col
                    = index_mapping.local_to_global (block_col, entry->column());
                  
                  if (distribute[global_col] == deal_II_numbers::invalid_unsigned_int)
                                                     // distribute entry at irregular
                                                     // row @p{row} and regular column
                                                     // global_col.
                    {
                      for (unsigned int q=0; q!=lines[distribute[row]].entries.size(); ++q) 
                        sparsity.add (lines[distribute[row]].entries[q].first, global_col);
                    }
                  else
                                                     // distribute entry at irregular
                                                     // row @p{row} and irregular column
                                                     // @p{global_col}
                    {
                      for (unsigned int p=0; p!=lines[distribute[row]].entries.size(); ++p)
                        for (unsigned int q=0; q!=lines[distribute[global_col]].entries.size(); ++q)
                          sparsity.add (lines[distribute[row]].entries[p].first,
                                        lines[distribute[global_col]].entries[q].first);
                    }
                }
	    }
	}
    }
  
  sparsity.compress();
}



void ConstraintMatrix::condense (CompressedBlockSparsityPattern &sparsity) const
{
  Assert (sorted == true, ExcMatrixNotClosed());
  Assert (sparsity.n_rows() == sparsity.n_cols(),
	  ExcMatrixNotSquare());
  Assert (sparsity.n_block_rows() == sparsity.n_block_cols(),
	  ExcMatrixNotSquare());
  Assert (sparsity.get_column_indices() == sparsity.get_row_indices(),
	  ExcMatrixNotSquare());
  
  const BlockIndices &
    index_mapping = sparsity.get_column_indices();

  const unsigned int n_blocks = sparsity.n_block_rows();
  
				   // store for each index whether it must be
				   // distributed or not. If entry is
				   // deal_II_numbers::invalid_unsigned_int,
				   // no distribution is necessary.
				   // otherwise, the number states which line
				   // in the constraint matrix handles this
				   // index
  std::vector<unsigned int> distribute (sparsity.n_rows(),
                                        deal_II_numbers::invalid_unsigned_int);
  
  for (unsigned int c=0; c<lines.size(); ++c)
    distribute[lines[c].line] = static_cast<signed int>(c);

  const unsigned int n_rows = sparsity.n_rows();
  for (unsigned int row=0; row<n_rows; ++row)
    {
				       // get index of this row
				       // within the blocks
      const std::pair<unsigned int,unsigned int>
	block_index = index_mapping.global_to_local(row);
      const unsigned int block_row = block_index.first;
      const unsigned int local_row = block_index.second;
      
      if (distribute[row] == deal_II_numbers::invalid_unsigned_int)
					 // regular line. loop over
					 // all columns and see
					 // whether this column must
					 // be distributed. note that
					 // as we proceed to
					 // distribute cols, the loop
					 // over cols may get longer.
					 //
					 // don't try to be clever
					 // here as in the algorithm
					 // for the
					 // CompressedSparsityPattern,
					 // as that would be much more
					 // complicated here. after
					 // all, we know that
					 // compressed patterns are
					 // inefficient...
	{

					   // to loop over all entries
					   // in this row, we have to
					   // loop over all blocks in
					   // this blockrow and the
					   // corresponding row
					   // therein
	  for (unsigned int block_col=0; block_col<n_blocks; ++block_col)
	    {
	      const CompressedSparsityPattern &
		block_sparsity = sparsity.block(block_row, block_col);

	      for (unsigned int j=0; j<block_sparsity.row_length(local_row); ++j)
		{
		  const unsigned int global_col
		    = index_mapping.local_to_global(block_col,
						    block_sparsity.column_number(local_row,j));
		    
		  if (distribute[global_col] != deal_II_numbers::invalid_unsigned_int)
						     // distribute entry at regular
						     // row @p{row} and irregular column
						     // global_col
		    {
		      for (unsigned int q=0;
			   q!=lines[distribute[global_col]]
					  .entries.size(); ++q)
			sparsity.add (row,
				      lines[distribute[global_col]].entries[q].first);
		    };
		};
	    };
	}
      else
	{
					   // row must be
					   // distributed. split the
					   // whole row into the
					   // chunks defined by the
					   // blocks
	  for (unsigned int block_col=0; block_col<n_blocks; ++block_col)
	    {
	      const CompressedSparsityPattern &
		block_sparsity = sparsity.block(block_row,block_col);
      
	      for (unsigned int j=0; j<block_sparsity.row_length(local_row); ++j)
		{
		  const unsigned int global_col
		    = index_mapping.local_to_global (block_col,
						     block_sparsity.column_number(local_row,j));
		    
		  if (distribute[global_col] == deal_II_numbers::invalid_unsigned_int)
						     // distribute entry at irregular
						     // row @p{row} and regular column
						     // global_col.
		    {
		      for (unsigned int q=0; q!=lines[distribute[row]].entries.size(); ++q) 
			sparsity.add (lines[distribute[row]].entries[q].first,
				      global_col);
		    }
		  else
						     // distribute entry at irregular
						     // row @p{row} and irregular column
						     // @p{global_col}
		    {
		      for (unsigned int p=0; p!=lines[distribute[row]].entries.size(); ++p)
			for (unsigned int q=0; q!=lines[distribute[global_col]].entries.size(); ++q)
			  sparsity.add (lines[distribute[row]].entries[p].first,
					lines[distribute[global_col]].entries[q].first);
		    };
		};
	    };
	};
    };
}



unsigned int ConstraintMatrix::n_constraints () const
{
  return lines.size();
}



bool ConstraintMatrix::is_constrained (const unsigned int index) const 
{
  if (lines.size() == 0)
    return false;
  
  if (sorted == true)
    {
      ConstraintLine index_comparison;
      index_comparison.line = index;
      
      return std::binary_search (lines.begin (),
				 lines.end (),
				 index_comparison);
    }
  else
    {
      for (std::vector<ConstraintLine>::const_iterator i=lines.begin();
	   i!=lines.end(); ++i)
	if (i->line == index)
	  return true;

      return false;
    };
}



bool ConstraintMatrix::is_identity_constrained (const unsigned int index) const 
{
  if (lines.size() == 0)
    return false;
  
  if (sorted == true)
    {
      ConstraintLine index_comparison;
      index_comparison.line = index;

      const std::vector<ConstraintLine>::const_iterator
	p = std::lower_bound (lines.begin (),
			      lines.end (),
			      index_comparison);
				       // return if an entry for this
				       // line was found and if it has
				       // only one entry equal to 1.0
				       //
				       // note that lower_bound only
				       // returns a valid iterator if
				       // 'index' is less than the
				       // largest line index in out
				       // constraints list
      return ((p != lines.end()) &&
	      (p->line == index) &&
	      (p->entries.size() == 1) &&
	      (p->entries[0].second == 1.0));
    }
  else
    {
      for (std::vector<ConstraintLine>::const_iterator i=lines.begin();
	   i!=lines.end(); ++i)
	if (i->line == index)
	  return ((i->entries.size() == 1) &&
		  (i->entries[0].second == 1.0));

      return false;
    }
}



unsigned int ConstraintMatrix::max_constraint_indirections () const 
{
  unsigned int return_value = 0;
  for (std::vector<ConstraintLine>::const_iterator i=lines.begin();
       i!=lines.end(); ++i)
				     // use static cast, since
				     // typeof(size)==size_t, which is
				     // != unsigned int on AIX
    return_value = std::max(return_value,
			    static_cast<unsigned int>(i->entries.size()));

  return return_value;
}

    

void ConstraintMatrix::print (std::ostream &out) const
{
  for (unsigned int i=0; i!=lines.size(); ++i)
    for (unsigned int j=0; j!=lines[i].entries.size(); ++j)
      out << "    " << lines[i].line
	  << " " << lines[i].entries[j].first
	  << ":  " << lines[i].entries[j].second << std::endl;

  AssertThrow (out, ExcIO());
}



unsigned int
ConstraintMatrix::memory_consumption () const
{
  return (MemoryConsumption::memory_consumption (lines) +
	  MemoryConsumption::memory_consumption (sorted));
}





// explicit instantiations
//
// define a list of functions for vectors and matrices, respectively, where
// the vector/matrix can be replaced using a preprocessor variable
// VectorType/MatrixType. note that we need a space between "VectorType" and
// ">" to disambiguate ">>" when VectorType trails in an angle bracket

#define VECTOR_FUNCTIONS(VectorType) \
  template void ConstraintMatrix::condense<VectorType >(const VectorType &uncondensed,\
					               VectorType       &condensed) const;\
  template void ConstraintMatrix::condense<VectorType >(VectorType &vec) const;\
  template void ConstraintMatrix::set_zero<VectorType >(VectorType &vec) const;\
  template void ConstraintMatrix:: \
    distribute_local_to_global<VectorType > (const Vector<double>            &, \
                                             const std::vector<unsigned int> &, \
                                             VectorType                      &) const; \
  template void ConstraintMatrix::distribute<VectorType >(const VectorType &condensed,\
					                 VectorType       &uncondensed) const;\
  template void ConstraintMatrix::distribute<VectorType >(VectorType &vec) const


VECTOR_FUNCTIONS(Vector<float>);
VECTOR_FUNCTIONS(Vector<double>);
VECTOR_FUNCTIONS(BlockVector<double>);
VECTOR_FUNCTIONS(BlockVector<float>);

#ifdef DEAL_II_USE_PETSC
VECTOR_FUNCTIONS(PETScWrappers::Vector);
VECTOR_FUNCTIONS(PETScWrappers::MPI::Vector);
#endif


template
void
ConstraintMatrix::condense<float>(const SparseMatrix<float> &uncondensed,
                                  SparseMatrix<float> &condensed) const;
template
void
ConstraintMatrix::condense<float>(SparseMatrix<float> &uncondensed) const;

template
void
ConstraintMatrix::condense<double>(const SparseMatrix<double> &uncondensed,
                                   SparseMatrix<double> &condensed) const;

template
void
ConstraintMatrix::condense<double>(SparseMatrix<double> &uncondensed) const;


// block sparse matrices are only implemented for one of the two matrix
// functions (the single-argument, in-place function)
template
void
ConstraintMatrix::condense<double>(BlockSparseMatrix<double> &uncondensed) const;

template
void
ConstraintMatrix::condense<float>(BlockSparseMatrix<float> &uncondensed) const;


#define MATRIX_FUNCTIONS(MatrixType) \
template void ConstraintMatrix:: \
distribute_local_to_global<MatrixType > (const FullMatrix<double>        &, \
                                         const std::vector<unsigned int> &, \
                                         MatrixType                      &) const

MATRIX_FUNCTIONS(SparseMatrix<double>);
MATRIX_FUNCTIONS(SparseMatrix<float>);

MATRIX_FUNCTIONS(BlockSparseMatrix<double>);
MATRIX_FUNCTIONS(BlockSparseMatrix<float>);

#ifdef DEAL_II_USE_PETSC
MATRIX_FUNCTIONS(PETScWrappers::SparseMatrix);
MATRIX_FUNCTIONS(PETScWrappers::MPI::SparseMatrix);
#endif
