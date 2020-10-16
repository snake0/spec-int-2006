//----------------------------  block_indices.h  ---------------------------
//    $Id: block_indices.h,v 1.4 2006/01/23 23:53:16 wolf Exp $
//    Version: $Name:  $
//
//    Copyright (C) 2000, 2001, 2002, 2003, 2004 by the deal.II authors
//
//    This file is subject to QPL and may not be  distributed
//    without copyright and license information. Please refer
//    to the file deal.II/doc/license.html for the  text  and
//    further information on this license.
//
//----------------------------  block_indices.h  ---------------------------
#ifndef __deal2__block_indices_h
#define __deal2__block_indices_h


#include <base/config.h>
#include <base/exceptions.h>
#include <vector>


/**
 * Class that manages the conversion of global indices into a block
 * vector or matrix to the local indices within this block. This is
 * required when you address a global element in a block vector and
 * want to know which element within which block this is. It is also
 * useful if a matrix is composed of several blocks, where you have to
 * translate global row and column indices to local ones.
 *
 * @author Wolfgang Bangerth, Guido Kanschat, 2000
 */
class BlockIndices
{
  public:

				     /**
				      * Default
				      * constructor. Initialize for
				      * @p n_blocks blocks and set
				      * all block sizes to zero.
				      */
    BlockIndices (const unsigned int n_blocks = 0);

				     /**
				      * Constructor. Initialize the
				      * number of entries in each
				      * block @p i as <tt>n[i]</tt>. The
				      * number of blocks will be the
				      * size of the vector
				      */
    BlockIndices (const std::vector<unsigned int> &n);

				     /**
				      * Reinitialize the number of
				      * blocks and assign each block
				      * the same number of elements.
				      */
    void reinit (const unsigned int n_blocks,
		 const unsigned int n_elements_per_block);
    
				     /**
				      * Reinitialize the number of
				      * indices within each block from
				      * the given argument. The number
				      * of blocks will be adjusted to
				      * the size of @p n and the size
				      * of block @p i is set to
				      * <tt>n[i]</tt>.
				      */
    inline void reinit (const std::vector<unsigned int> &n);
    
				     /**
				      * Return the block and the
				      * index within that block
				      * for the global index @p i. The
				      * first element of the pair is
				      * the block, the second the
				      * index within it.
				      */
    std::pair<unsigned int,unsigned int>
    global_to_local (const unsigned int i) const;

				     /**
				      * Return the global index of
				      * @p index in block @p block.
				      */
    unsigned int local_to_global (const unsigned int block,
				  const unsigned int index) const;

				     /**
				      * Number of blocks in index field.
				      */
    unsigned int size () const;
  
				     /**
				      * Return the total number of
				      * indices accumulated over all
				      * blocks, that is, the dimension
				      * of the vector space of the
				      * block vector.
				      */
    inline unsigned int total_size () const;

				     /**
				      * Return the size of the @p ith
				      * block.
				      */
    unsigned int block_size (const unsigned int i) const;
      
				     /**
				      * Copy operator.
				      */
    BlockIndices & operator = (const BlockIndices &b);

				     /**
				      * Compare whether two objects
				      * are the same, i.e. whether the
				      * starting indices of all blocks
				      * are equal.
				      */
    bool operator == (const BlockIndices &b) const;
    
				     /**
				      * Swap the contents of these two
				      * objects.
				      */
    void swap (BlockIndices &b);

				     /**
				      * Determine an estimate for the
				      * memory consumption (in bytes)
				      * of this object.
				      */
    unsigned int memory_consumption () const;
    
  private:
				     /**
				      * Number of blocks. This is made
				      * constant to avoid accidental
				      * changes during lifetime.
				      */
    unsigned int n_blocks;

                                     /**
				      * Global starting index of each
				      * vector. The last and redundant
				      * value is the total number of
				      * entries.
				      */
    std::vector<unsigned int> start_indices;
};



/* ---------------------- template and inline functions ------------------- */

inline
BlockIndices::BlockIndices (const unsigned int n_blocks_)
		:
		n_blocks(n_blocks_),
		start_indices(n_blocks_+1)
{
  for (unsigned int i=0; i<=n_blocks; ++i)
    start_indices[i] = 0;
}



inline
BlockIndices::BlockIndices (const std::vector<unsigned int> &n)
		:
		n_blocks(n.size()),
		start_indices(n.size()+1)
{
  reinit (n);
}



inline
void
BlockIndices::reinit (const unsigned int n_blocks,
		      const unsigned int n_elements_per_block)
{
  const std::vector<unsigned int> v(n_blocks, n_elements_per_block);
  reinit (v);
}



inline
void
BlockIndices::reinit (const std::vector<unsigned int> &n)
{
  if (start_indices.size() != n.size()+1)
    {
      n_blocks = n.size();
      start_indices.resize(n_blocks+1);
    }
  start_indices[0] = 0;
  for (unsigned int i=1; i<=n_blocks; ++i)
    start_indices[i] = start_indices[i-1] + n[i-1];
}



inline
std::pair<unsigned int,unsigned int>
BlockIndices::global_to_local (const unsigned int i) const 
{
  Assert (i<total_size(), ExcIndexRange(i, 0, total_size()));

  int block = n_blocks-1;
  while (i < start_indices[block])
    --block;

  return std::make_pair<unsigned int>(block, i-start_indices[block]);
}


inline
unsigned int
BlockIndices::local_to_global (const unsigned int block,
			       const unsigned int index) const 
{
  Assert (block < n_blocks, ExcIndexRange(block, 0, n_blocks));
  Assert (index < start_indices[block+1]-start_indices[block],
	  ExcIndexRange (index, 0, start_indices[block+1]-start_indices[block]));

  return start_indices[block]+index;
}


inline
unsigned int
BlockIndices::size () const 
{
  return n_blocks;
}



inline
unsigned int
BlockIndices::total_size () const 
{
  return start_indices[n_blocks];
}



inline
unsigned int
BlockIndices::block_size (const unsigned int block) const 
{
  Assert (block < n_blocks, ExcIndexRange(block, 0, n_blocks));
  return start_indices[block+1]-start_indices[block];
}



inline
BlockIndices &
BlockIndices::operator = (const BlockIndices &b)
{
  start_indices = b.start_indices;
  n_blocks = b.n_blocks;
  return *this;
}



inline
bool
BlockIndices::operator == (const BlockIndices &b) const 
{
  if (n_blocks != b.n_blocks)
    return false;
  
  for (unsigned int i=0; i<=n_blocks; ++i)
    if (start_indices[i] != b.start_indices[i])
      return false;
  
  return true;
}



inline
void
BlockIndices::swap (BlockIndices &b) 
{
  Assert (n_blocks == b.n_blocks,
	  ExcDimensionMismatch(n_blocks, b.n_blocks));  

  for (unsigned int i=0; i<=n_blocks; ++i)
    std::swap (start_indices[i], b.start_indices[i]);
}



inline
unsigned int
BlockIndices::memory_consumption () const 
{
  return (sizeof(*this) + 
	  start_indices.size() * sizeof(start_indices[0]));
}



/* ----------------- global functions ---------------------------- */


/**
 * Global function @p swap which overloads the default implementation
 * of the C++ standard library which uses a temporary object. The
 * function simply exchanges the data of the two objects.
 *
 * @relates BlockIndices
 * @author Wolfgang Bangerth, 2000
 */
inline
void swap (BlockIndices &u, BlockIndices &v) 
{
  u.swap (v);
}




#endif
