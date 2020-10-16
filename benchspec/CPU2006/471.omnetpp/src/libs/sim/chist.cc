//=========================================================================
//
//  CHIST.CC - part of
//                          OMNeT++
//           Discrete System Simulation in C++
//
//   Member functions of
//    cHistogramBase    : common base class for histogram classes
//    cEqdHistogramBase : Equi-distant histograms
//    cLongHistogram    : long integer histogram
//    cDoubleHistogram  : double histogram
//
//   Authors: Andras Varga, Gabor Lencse
//
//=========================================================================

/*--------------------------------------------------------------*
  Copyright (C) 1992-2003 Andras Varga

  This file is distributed WITHOUT ANY WARRANTY. See the file
  `license' for details on this and other legal matters.
*--------------------------------------------------------------*/

#include <stdio.h>
#if defined(SPEC_CPU)
#include <cstdlib>
#else
#include <stdlib.h>
#endif
#include <string.h>
#if defined(SPEC_CPU)
#include <cmath>
#else
#include <math.h>
#endif
#include <assert.h>
#include "random.h"
#include "distrib.h"
#include "macros.h"
#include "cdetect.h"
#include "chist.h"
#include "cexception.h"

#define MIN(a,b) ((a)<(b) ? (a) : (b))
#define MAX(a,b) ((a)>(b) ? (a) : (b))

//=========================================================================
//=== Registration

Register_Class(cLongHistogram);
Register_Class(cDoubleHistogram);

//==========================================================================
// cHistogramBase - member functions

cHistogramBase::cHistogramBase(const char *name, int numcells) :
cDensityEstBase(name)
{
    cellv = NULL;
    num_cells = numcells;
}

cHistogramBase::~cHistogramBase()
{
    delete [] cellv;
}

cHistogramBase& cHistogramBase::operator=(const cHistogramBase& res)
{
    if (this==&res) return *this;

    cDensityEstBase::operator=(res);

    num_cells = res.num_cells;
    delete [] cellv;  cellv = NULL;
    if (res.cellv)
    {
        cellv = new unsigned[num_cells];
        memcpy(cellv, res.cellv, num_cells*sizeof(unsigned));
    }
    return *this;
}

void cHistogramBase::clearResult ()
{
    cDensityEstBase::clearResult();

    delete [] cellv;
    cellv=NULL;
}

void cHistogramBase::transform()
{
    int i;
    cellv = new unsigned [num_cells];
    for (i=0; i<num_cells; i++) cellv[i]=0;

    setupRange();
    for (i=0; i<num_samples; i++) collectTransformed( firstvals[i] );
    transfd = true;
}

int cHistogramBase::cells() const
{
    if (!transformed()) return 0;

    return num_cells;
}

void cHistogramBase::saveToFile(FILE *f) const
{
    cDensityEstBase::saveToFile(f);
    fprintf(f,"%d\t #= num_cells\n", num_cells);
    fprintf(f,"%d\t #= cellv[] exists\n", cellv!=NULL);
    if (cellv) for (int i=0; i<num_cells; i++) fprintf(f," %u\n",cellv[i]);
}

void cHistogramBase::loadFromFile(FILE *f)
{
    cDensityEstBase::loadFromFile(f);
    freadvarsf(f,"%d\t #= num_cells", &num_cells);

    int cellv_exists;
    freadvarsf(f,"%d\t #= cellv[] exists", &cellv_exists);
    delete [] cellv; cellv = NULL;
    if (cellv_exists)
    {
        cellv = new unsigned[num_cells];
        for (int i=0; i<num_cells; i++) freadvarsf(f," %u",cellv+i);
    }
}

void cHistogramBase::setNumCells(int numcells)
{
    if (cellv)
        throw new cException(this,"setNumCells(): too late, cells already set up");
    num_cells = numcells;
}


//=========================================================================
// cEqdHistogramBase - member functions

cEqdHistogramBase::cEqdHistogramBase(const char *name, int numcells) :
cHistogramBase(name,numcells)
{
    cellsize=0;
}

cEqdHistogramBase& cEqdHistogramBase::operator=(const cEqdHistogramBase& res)
{
    if (this==&res) return *this;

    cHistogramBase::operator=(res);
    cellsize = res.cellsize;
    return *this;
}

void cEqdHistogramBase::setupRange()
{
    cHistogramBase::setupRange();

    cellsize = (rangemax - rangemin) / num_cells;
}

void cEqdHistogramBase::collectTransformed (double val)
{
    int k = (int)std::floor((val-rangemin)/cellsize);
    if (k<0 || val<rangemin)
        cell_under++;
    else if (k>=num_cells || val>=rangemax)
        cell_over++;
    else
        cellv[k]++;
}

double cEqdHistogramBase::pdf(double x) const
{
    if (!transformed())
        throw new cException(this,"pdf(x) cannot be called before histogram is transformed");

    int k = (int)std::floor((x-rangemin)/cellsize);
    if (k<0 || x<rangemin || k>=num_cells || x>=rangemax)
        return 0.0;

    return cellv[k] / cellsize / num_samples;
}

double cEqdHistogramBase::cdf(double) const
{
    throw new cException(this,"cdf() not implemented");
}

// return kth basepoint
double cEqdHistogramBase::basepoint(int k) const
{
    //   k=0           : rangemin
    //   k=1,2,...     : rangemin + k*cellsize
    //   k=num_cells   : rangemax

    if (k<0 || k>num_cells)
        throw new cException(this,"invalid basepoint index %u",k);

    if (k==num_cells)
        return rangemax;
    else
        return rangemin + k*cellsize;
}

double cEqdHistogramBase::cell(int k) const
{
    if (k<0 || k>num_cells)
        throw new cException(this,"invalid cell index %u",k);
    return cellv[k];
}

void cEqdHistogramBase::saveToFile(FILE *f) const
{
    cHistogramBase::saveToFile(f);
    fprintf(f,"%g\t #= cellsize\n", cellsize);
}

void cEqdHistogramBase::loadFromFile(FILE *f)
{
    cHistogramBase::loadFromFile(f);
    freadvarsf(f,"%g\t #= cellsize", &cellsize);
}

//=========================================================================
// cLongHistogram - member functions

cLongHistogram::cLongHistogram(const char *name, int numcells) :
cEqdHistogramBase(name,numcells)
{
}

cLongHistogram::~cLongHistogram()
{
}

void cLongHistogram::collect(double val)
{
     cEqdHistogramBase::collect( std::floor(val) );
}

void cLongHistogram::setupRange()
{
    cHistogramBase::setupRange();

    // adjust range so that cellsize be an integer
    rangemin = std::floor(rangemin)-0.5;
    rangemax = std::ceil(rangemax)+0.5;

    cellsize = (rangemax-rangemin)/num_cells;
    cellsize = std::ceil(cellsize);
    double newrange = cellsize*num_cells;
    double rangediff = newrange - (rangemax-rangemin);

    switch (range_mode)
    {
       case RANGE_AUTO:
         rangemin -= std::floor(rangediff/2);
         rangemax += rangediff - std::floor(rangediff/2);
         break;
       case RANGE_AUTOLOWER:
         rangemin = rangemax - newrange;
         break;
       case RANGE_AUTOUPPER:
         rangemax = rangemin + newrange;
         break;
    }
}

double cLongHistogram::random() const
{
    if( num_samples==0 )
        return 0L;
    else if( num_samples<num_firstvals )
    {
        // randomly select a sample from the stored ones
        return firstvals[genk_intrand(genk,num_samples)];
    }
    else
    {
        // min_samples, max_samples: integer-valued doubles (e.g.: -3.0, 5.0)
        // rangemin, rangemax: doubles like -1.5, 4.5 (integer+0.5)
        // cellsize: integer-valued double, >0
        double m = genk_intrand(genk,num_samples-cell_under-cell_over);

        // select a random interval (k-1) and return a random number from
        // that interval generated according to uniform distribution.
        int k;
        for (k=0; m>=0; k++)
            m -= cellv[k];

        return std::ceil(rangemin) + (k-1)*(long)cellsize + genk_intrand(genk, (long)cellsize);
    }
}

//==========================================================================
// cDoubleHistogram - member functions

cDoubleHistogram::cDoubleHistogram(const char *name, int numcells) :
cEqdHistogramBase(name,numcells)
{
}

cDoubleHistogram::~cDoubleHistogram()
{
}

double cDoubleHistogram::random() const
{
    if( num_samples==0 )
        return 0L;
    else if( num_samples<num_firstvals )
    {
        // randomly select a sample from the stored ones
        return firstvals[genk_intrand(genk,num_samples)];
    }
    else
    {
        double m = genk_intrand(genk,num_samples-cell_under-cell_over);

        // select a random interval (k-1) and return a random number from
        // that interval generated according to uniform distribution.
        int k;
        for (k=0; m>=0; k++)
           m -= cellv[k];
        return rangemin + (k-1)*cellsize + genk_dblrand(genk)*cellsize;
    }
}

