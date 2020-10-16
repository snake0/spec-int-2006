//==========================================================================
//  HEAP.CPP
//
//    A safety pool heap management module with debugging features
//
//    !
//    !  This file DOES CONTAIN user servicable parts!
//    !
//
//    Contents:
//       MemManager  - memory management class
//       new, delete - allocation operators
//
//==========================================================================

/*--------------------------------------------------------------*
  Copyright (C) 1992-2003 Andras Varga

  This file is distributed WITHOUT ANY WARRANTY. See the file
  `license' for details on this and other legal matters.
*--------------------------------------------------------------*/

#include <ctype.h>
#include <stdio.h>
#if defined(SPEC_CPU)
#include <cstdlib>   // malloc
#else
#include <stdlib.h>   // malloc
#endif
#if defined(__MSDOS__) || defined(__CONSOLE__)
#include <conio.h>
#include <alloc.h>
#endif

//=================================================================
//
//       **** Defines for memory allocation debugging ****
//
//  GO AHEAD and EDIT the following #defines!!!
//
//  They may help if you have problems with heap allocations.
//  o  #defining HEAPCHECK, COUNTBLOCKS and ALLOCTABLE is recommended
//     while you are developing.
//  o  For running a tested simulation, you can turn them off.
//
//=================================================================

//#define HEAPCHECK      /*check heap on new/delete*/
//#define COUNTBLOCKS    /*count blocks on heap & dislay if none left*/
//#define ALLOCTABLE     /*remember pointers & display heap LASTN times*/
//#define DISPLAYALL     /*report every new/delete*/
//#define DISPSTRAYS     /*report deleting of ptrs not in the table*/
//#define BKPT           /*break at new/delete #bkpt*/

#define LASTN          0  /* list heap contents when less than LASTN */
                          /* blocks are allocated */
#define TABLESIZE   8192  /* your program should not have more than */
                          /* this many blocks allocated at the same time */

#ifdef BKPT
unsigned breakat = 0; //!!!! PUT HERE THE NUMBER OF BLOCK IN QUESTION !!!!

void breakpoint()
{
     ; //!!!!!!!!!!!!!!! PUT YOUR BREAKPOINT HERE !!!!!!!!!!!!!!!!!!

    /* PURPOSE: use this function to find out
     *   o  which block was the one that was never deleted, or
     *   o  when is block #n deleted!
     * Set the 'breakat' variable or the BREAKAT #define
     * for this function to be called at a given allocation/deallocation
     * and put a breakpoint here!
     */
}
#endif

/*----------*
 PORTABILITY NOTE
 Here are the symbols Borland C++ 4.5 #defines for different platforms:

 DOS (real mode) : __MSDOS__
 DOS 16 bit DPMI : __MSDOS__, _Windows (!), __DPMI16__
 DOS 32 bit DPMI : __WIN32__ (!), _Windows (!), __DPMI32__, __FLAT__, __CONSOLE__
 Windows 16bit   : __MSDOS__ (!), _Windows
 Win32 Console   : __WIN32__, _Windows, __FLAT__, __CONSOLE__
 Win32 GUI       : __WIN32__, _Windows, __FLAT__
  --VA
 *----------*/


//==========================================================================
// MemManager
//  States:
//   safetypool      lowmem
//     NULL           false     -> before construction or after destruction
//   NOT NULL         false     -> ok, size of safety pool = maxpoolsize
//     NULL           true      -> low memory, no safety pool
//   NOT NULL         true      -> never occurs
//
class MemManager
{
   private:
      void *safetypool;
      int maxpoolsize;
      bool lowmem;
   public:
      MemManager(int mps=16384);
      ~MemManager();
      void makeRoom();
      void restorePool();
      bool lowMemory() {return lowmem;}
};

MemManager::MemManager(int mps)
{
    maxpoolsize = mps;
    safetypool = std::malloc(maxpoolsize);
    lowmem = (safetypool==NULL);
}

MemManager::~MemManager()
{
    std::free(safetypool);
    safetypool = NULL;
    maxpoolsize = 0;
    lowmem = false;
}

void MemManager::makeRoom()
{
    lowmem = true;
    std::free(safetypool);
    safetypool = NULL;
}

void MemManager::restorePool()
{
    if (lowmem)
    {
        safetypool = std::malloc(maxpoolsize);
        if(safetypool)
            lowmem = false;
    }
}

//==========================================================================
// static instance of MemManager and a function accessible from outside
static MemManager memManager;

bool cmdenvMemoryIsLow()
{
    return memManager.lowMemory();
}

//==========================================================================
//=== The allocation control functions: NEW,DELETE
// Defines to help debugging:
//  HEAPCHECK, COUNTBLOCKS, ALLOCTABLE, DISPLAYALL, BKPT (switches)
//  TABLESIZE, LASTN(=1,2,...) (parameters)

#if defined(__BORLANDC__)
#if (defined(__WIN32__) && !defined(__DPMI32__)) || /*Win32*/ \
   (defined(__MSDOS__) && !defined(_Windows))     /*real-mode DOS*/
#define HEAPCHECK_OK
#endif
#endif

#if defined(BKPT) || defined(DISPSTRAYS)
#define ALLOCTABLE
#endif

#if defined(DISPLAYALL) && !defined(ALLOCTABLE)
#define COUNTBLOCKS
#endif

#if defined(COUNTBLOCKS)
static int blocksonheap = 0;
#endif

#if defined(ALLOCTABLE)
struct Allocation { unsigned long id; void *ptr; };
static Allocation *alloctable;
static int lastused = -1;  // index into alloctable[]; will not decrease!
static int table_off = 0; // if table ever got full: not reliable
static int blocksintable = 0; // #blocks in table
static unsigned long last_id = 0;   // id of last allocation
#endif

#if defined(BKPT)
void brk(char *s)
{
        printf(" [HEAP.CC-debug:%s#%u: BREAKPOINT HIT - Press ENTER.]",s,breakat);
        while (getchar()!='\n');
        breakpoint();
}
#endif

#if defined(HEAPCHECK) || defined(ALLOCTABLE) || defined(COUNTBLOCKS)
int askyesno()
{
        char c;
        while (c=getchar(), toupper(c)!='Y' && toupper(c)!='N');
        while (getchar()!='\n');
        return toupper(c)=='Y';
}
#endif

#if defined(HEAPCHECK)
void checkheap(char *s)
{
#ifdef HEAPCHECK_OK
        if (heapcheck()<0) {
            printf("\n[HEAP.CC-debug:%s:HEAP CORRUPTED,continue? (y/n)]",s);
            if(!askyesno()) abort();
        }
#endif
}

int checkheapnode(char *s,void *p)
{
#ifdef HEAPCHECK_OK
        //if (p != MK_FP( FP_SEG(p), 0x0004))
        //    printf(" [HEAP.CC-debug:%s:PTR WITH BAD OFFSET]",s);
        int a = heapchecknode(p);
        if(a==_BADNODE)
            printf(" [HEAP.CC-debug:%s:NOT A VALID NODE]",s);
        else if(a==_FREEENTRY)
            printf(" [HEAP.CC-debug:%s:ALREADY FREED]",s);
        return a==_USEDENTRY;
#else
        return true;
#endif
}
#endif

#if defined(ALLOCTABLE)
void insertintotable(void *p)
{
        if (table_off) return;

        if (alloctable==NULL)
            alloctable = (Allocation *)std::malloc(TABLESIZE*sizeof(Allocation));

        int i;
        for (i=lastused; i>=0; i--)   // find a hole in used area
          if (alloctable[i].ptr==NULL)
             break;
        if (i==-1 && lastused<TABLESIZE-1) // expand used area
             i = lastused = lastused+1;
        if (i!=-1)
        {
            blocksintable++;
            alloctable[i].id = ++last_id;
            alloctable[i].ptr = p;
        }
        else // table really full
        {
            table_off = 1;
            printf(" [HEAP.CC-debug:NEW:ALLOCTABLE FULL,checks turned off!]");
        }
}

int delfromtable(void *p)
{
        if (table_off) return -1;

        if (alloctable==NULL)
            alloctable = (Allocation *)std::malloc(TABLESIZE*sizeof(Allocation));

        int i;
        for(i=0; i<=lastused; i++)
           if (alloctable[i].ptr==p) break;
        if(i>lastused) {
#ifdef DISPSTRAYS
             printf(" [HEAP.CC-debug:DELETE:BLOCK NOT IN ALLOCTABLE!]");
#endif
             return -1;
        } else {
             blocksintable--;
             alloctable[i].ptr = NULL;
             return alloctable[i].id;
        }
}
void dispheap()
{
        if(blocksintable!=0) {
             printf("\n[HEAP.CC-debug:%u blocks left - display heap? (y/n)]", blocksintable);
             if(!askyesno()) return;
             printf("\n------%u blocks still on heap------\n",blocksintable);
             for(int i=0; i<=lastused; i++)
               if(alloctable[i].ptr)
                  printf(" #%lu %p [%.20s]\n",alloctable[i].id,alloctable[i].ptr,alloctable[i].ptr);
             printf("----------end heap contents-----------\n");
        }
}
#endif

void *operator new(size_t m)
{
#ifdef HEAPCHECK
      checkheap("NEW");
#endif
      if (m==0) return NULL;
      void *p;
      if ((p=std::malloc(m)) != NULL)   // allocation successful
      {
#ifdef COUNTBLOCKS
         blocksonheap++;
#endif
#ifdef ALLOCTABLE
         insertintotable(p);
#endif
#ifdef DISPLAYALL
#ifdef ALLOCTABLE
         if (table_off)
            printf(" [HEAP.CC-debug:NEW(%lu),alloctable full]", (long)m );
         else
            printf(" [HEAP.CC-debug:NEW#%lu (%lu),%u blocks%s]", last_id,
                    (long)m, blocksintable, memManager.lowMemory() ? " LOWMEM!":"" );
#else
         printf(" [HEAP.CC-debug:NEW(%lu),%u blocks%s]",
                    (long)m, blocksonheap, memManager.lowMemory() ? " LOWMEM!":"" );
#endif
#endif
#ifdef BKPT
         if (last_id==breakat)
            brk("NEW");
#endif
         return p;
      }
      else if (!memManager.lowMemory())   // still we have the safety pool
      {
         // try allocation again after freeing pool
         memManager.makeRoom();
         return (void *)new char[m];  // recursive call
      }
      else   // really no more memory, must call exit()
      {
#ifdef COUNTBLOCKS
#ifdef ALLOCTABLE
         if (table_off)
            printf("\n[HEAP.CC-debug:NEW(%lu) FAILED,alloctable full,about to exit!]",
                   (long)m );
         else
            printf("\n[HEAP.CC-debug: NEW#%lu (%lu) FAILED,%u blocks%s,about to exit!]",
                   last_id, (long)m, blocksintable, memManager.lowMemory() ? " LOWMEM!":"" );
         dispheap();
#else
         printf("\n[HEAP.CC-debug:NEW (%lu) FAILED,%u blocks%s,exiting!]",
                 (long)m, blocksonheap, memManager.lowMemory() ? " LOWMEM!":"" );
#endif
#endif
         printf("\n[NEW (%lu) FAILED,exiting!]\n", (long)m );
         std::exit(1);
         return p;   // to suppress compiler warning
      }
}

void operator delete(void *p)
{
#ifdef HEAPCHECK
    checkheap("DELETE");
#endif
    if (p==NULL) return;
#ifdef HEAPCHECK
    if(!checkheapnode("DELETE",p)) return;
#endif
#ifdef COUNTBLOCKS
    bool heapempty=false;
    if (blocksonheap==0)
           printf(" [HEAP.CC-debug:DELETE:DELETE AFTER HEAP EMPTY ???]");
    else {
           blocksonheap--;
           if (blocksonheap==0)  heapempty=true;
    }
#endif
#ifdef ALLOCTABLE
    unsigned id = delfromtable(p);
#endif
#ifdef DISPLAYALL
    char *s = (char *)p;
    s[11]=0;       // ok, BC++: smallest block is 16-4 = 12 bytes long
#ifdef ALLOCTABLE
    if (id!=-1)
       printf(" [HEAP.CC-debug:DELETE#%lu ('%s'),%u blocks]",id,s,blocksintable);
#else
       printf(" [HEAP.CC-debug:DELETE ('%s'),%u blocks]",s,blocksonheap);
#endif
#endif
#ifdef ALLOCTABLE
    if (blocksintable<=LASTN) dispheap();
#endif
#ifdef COUNTBLOCKS
    if(heapempty) printf(" [HEAP.CC-debug:DELETE:ALL BLOCKS FREED OK] ");
#endif
    std::free(p);
    if (memManager.lowMemory())
           memManager.restorePool();
#ifdef BKPT
    if(id==breakat) brk("DELETE");
#endif
}
