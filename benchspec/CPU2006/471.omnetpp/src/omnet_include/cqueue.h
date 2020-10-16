//==========================================================================
//   CQUEUE.H  - header for
//                             OMNeT++
//            Discrete System Simulation in C++
//
//
//  Declaration of the following classes:
//    cQueue        : (optionally) sorted queue of cObjects
//    cQueueIterator: walks along a queue
//
//==========================================================================

/*--------------------------------------------------------------*
  Copyright (C) 1992-2003 Andras Varga

  This file is distributed WITHOUT ANY WARRANTY. See the file
  `license' for details on this and other legal matters.
*--------------------------------------------------------------*/

#ifndef __CQUEUE_H
#define __CQUEUE_H

#include "cobject.h"


/**
 * Queue class. cQueue is a container class that can hold objects derived
 * from cObject. cQueue acts as a priority queue.
 * The user must provide a function that can compare two objects.
 * If no such function is given, cQueue implements a FIFO.
 * Order (ascending or descending) can be specified.
 *
 * Ownership of contained objects (responsibility of deletion) can
 * be specified per-object basis (see cObject::takeOwnership()).
 * Default is that cQueue takes the ownership of each object
 * inserted (that is, takeOwnership(true)).
 *
 * The sorting function should look like:
 * int CompareFunc(cObject* a, cObject* b);
 *
 * They must return a negative value if a&lt;b, 0 if a==b and a positive value
 * if a&gt;b.
 *
 * @see Iterator
 * @ingroup Containers
 */
class SIM_API cQueue : public cObject
{

/* see osgcpu-10407 */
#if defined(SPEC_CPU_NO_NESTED_CLASS_ACCESS)
    public:
#else
    private:
#endif

    struct QElem
    {
        cObject *obj;
        QElem *prev, *next;
    };

  public:
    /**
     * Walks along a cQueue.
     */
    class Iterator
    {
      private:
        QElem *p;

      public:
        /**
         * Constructor. cQueueIterator will walk on the queue passed
         * as argument. The current object will be the first (if athead==true) or
         * the last (athead==false) object in the queue.
         */
        Iterator(const cQueue& q, bool athead=true)
                {p=&q ? (athead ? q.headp : q.tailp) : NULL;}

        /**
         * Reinitializes the iterator object.
         */
        void init(const cQueue& q, bool athead=true)
                {p=&q ? (athead ? q.headp : q.tailp) : NULL;}

        /**
         * DEPRECATED. Use operator () instead.
         */
        cObject& operator[](int)  {return p ? *(p->obj) : *(cObject *)NULL;}

        /**
         * Returns the current object.
         */
        cObject *operator()()  {return p ? p->obj : NULL;}

        /**
         * Returns true if the iterator has reached either end of the queue.
         */
        bool end() const   {return (bool)(p==NULL);}

        /**
         * Returns the current object, then moves the iterator to the next item.
         * If the iterator has reached either end of the queue, nothing happens;
         * you have to call init() again to restart iterating.
         */
        cObject *operator++(int)  {if (!p) return NULL; cObject *r=p->obj; p=p->next; return r;}

        /**
         * Returns the current object, then moves the iterator to the previous item.
         * If the iterator has reached either end of the queue, nothing happens;
         * you have to call init() again to restart iterating.
         */
        cObject *operator--(int)  {if (!p) return NULL; cObject *r=p->obj; p=p->prev; return r;}
    };

    friend class Iterator;

  private:
    QElem *headp, *tailp;           // inserting at head, removal at tail
    int n;                          // number of items in queue
    CompareFunc compare;            // compare function
    bool asc;                       // order: true=ascending

  protected:
    // internal functions
    QElem *find_qelem(cObject *obj) const;
    void insbefore_qelem(QElem *p, cObject *obj);
    void insafter_qelem(QElem *p, cObject *obj);
    cObject *remove_qelem(QElem *p);

  public:
    /** @name Constructors, destructor, assignment. */
    //@{

    /**
     * Copy constructor. Contained objects that are owned by the queue
     * will be duplicated so that the new queue will have its own copy
     * of them.
     */
    cQueue(const cQueue& queue);

    /**
     * Constructor. It accepts the object name, the address of the comparing
     * function and the sorting order (ascending=true, descending=false).
     */
    explicit cQueue(const char *name=NULL, CompareFunc cmp=NULL, bool a=false);

    /**
     * Destructor. Deletes all contained objects that were owned by it.
     */
    virtual ~cQueue();

    /**
     * Assignment operator. Duplication and assignment work all right with cQueue.
     * Contained objects that are owned by the queue will be duplicated
     * so that the new queue will have its own copy of them.
     *
     * The name member doesn't get copied; see cObject's operator=() for more details.
     */
    cQueue& operator=(const cQueue& queue);
    //@}

    /** @name Redefined cObject member functions. */
    //@{

    /**
     * Duplication and assignment work all right with cQueue.
     * Contained objects that are owned by the queue will be duplicated
     * so that the new queue will have its own copy of them.
     */
    virtual cObject *dup() const  {return new cQueue(*this);}

    /**
     * Produces a one-line description of object contents into the buffer passed as argument.
     * See cObject for more details.
     */
    virtual void info(char *buf);

    /**
     * Calls the given function for each contained
     * object.
     */
    virtual void forEach(ForeachFunc f);

    /**
     * Serializes the object into a PVM or MPI send buffer.
     * Used by the simulation kernel for parallel execution.
     * See cObject for more details.
     */
    virtual int netPack();

    /**
     * Deserializes the object from a PVM or MPI receive buffer
     * Used by the simulation kernel for parallel execution.
     * See cObject for more details.
     */
    virtual int netUnpack();
    //@}

    /** @name Setup, insertion and removal functions. */
    //@{

    /**
     * Changes the sort function and the sorting order. Doesn't re-sort
     * the contents of the queue!
     */
    virtual void setup(CompareFunc cmp, bool a=false);

    /**
     * Inserts the given object into the queue, maintaining the sorting
     * order. Trying to insert a NULL pointer is an error (throws cException).
     */
    virtual void insert(cObject *obj);

    /**
     * Inserts exactly before the given object. If the given position
     * does not exist or if you try to insert a NULL pointer,
     * cException is thrown.
     */
    virtual void insertBefore(cObject *where, cObject *obj);

    /**
     * Inserts exactly after the given object. If the given position
     * does not exist or if you try to insert a NULL pointer,
     * cException is thrown.
     */
    virtual void insertAfter(cObject *where, cObject *obj);

    /**
     * Unlinks and returns the object given. If the object is not in the
     * queue, NULL pointer is returned.
     */
    virtual cObject *remove(cObject *obj);

    /**
     * Unlinks and returns the last (tail) object in the queue. If the queue
     * was empty, cException is thrown.
     */
    virtual cObject *pop();

    /**
     * As a result, the container will be empty. Contained objects that
     * were owned by the queue will be deleted.
     */
    virtual void clear();
    //@}

    /** @name Query functions. */
    //@{

    /**
     * Returns pointer to the object at the head of the queue.
     * Returns NULL if the queue is empty.
     */
    virtual cObject *head() const;

    /**
     * Returns pointer to the last (tail) object in the queue.
     * Returns NULL if the queue is empty.
     */
    virtual cObject *tail() const;

    /**
     * Returns the number of objects contained in the queue.
     */
    virtual int length() const;

    /**
     * Returns true if the queue is empty.
     */
    bool empty() const {return length()==0;}

    /**
     * Returns true if the queue contains the passed object.
     */
    virtual bool contains(cObject *obj) const;
    //@}
};

#endif

