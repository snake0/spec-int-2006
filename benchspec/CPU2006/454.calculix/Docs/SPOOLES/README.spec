This directory is derived from "SPOOLES 2.2", the SParse Object Oriented 
Linear Equations Solver which "is entirely within the public domain; 
there are no licensing restrictions" according to 
    http://www.netlib.org/linalg/spooles/spooles.2.2.html
(last retrieved 4 Dec 2003).  The contributions of Cleve Ashcraft et. al. 
are gratefully acknowledged.

The original SPOOLES 2.2 makefile structure would cause commands such as 
these to be executed:

   cd A2/src
   cc -c  IO.c     -o A2_IO.o
   cc -c  basics.c -o A2_basics.o
   cc -c  init.c   -o A2_init.o
   .
   .
   .
   ar rv ../../spooles.a A2_*.o

SPEC uses an entirely different makefile structure, and does not rely on 
linking from archive libraries (a concept that may not be portable across 
all operating systems of interest).

So, for SPEC purposes, the original SPOOLES makefile structure has been 
entirely removed.  If you'd like to see what it used to be, please 
expand "spooles.2.2.tgz", available at 
    http://www.netlib.org/linalg/spooles/spooles.2.2.html
or on your CPU2006 CD in the "original.src/454.calculix" directory.

For SPEC purposes, it is convenient if source module names have the same 
name as object names.  Therefore, the modules used by CalculiX have been 
renamed from the original SPOOLES, using a procedure similar to this:

    mv "A2/src/IO.c"           "A2/src/A2_IO.c"
    mv "A2/src/basics.c"       "A2/src/A2_basics.c"
    mv "A2/src/init.c"         "A2/src/A2_init.c"
    .
    .
    .

Only the modules actually linked into CalculiX have been renamed in this 
fashion.

Multi-thread notes:
   SPEC CPU has traditionally been single-threaded, leaving 
   multi-threaded programs to the SPEC "HPG" group.  If this benchmark 
   should be considered for adoption by SPEC HPG at a later date, it 
   may be desirable to restore the multi-thread capabilities in the 
   original package.  But for now, Lock/Lock.h has been set to TT_NONE 
   (see installation guide section 3), the MT and MPI directories have 
   been removed, as have MT.h and MPI.h from the top directory.

The original SPOOLES documentation is distributed as gzip-compressed 
postscript.  For convenience, a pdf version has been generated, and
placed in the 454.calculix/Docs/SPOOLES/documentation directory.

- John Henning
  Sun Microsystems, 4 Dec 2003
