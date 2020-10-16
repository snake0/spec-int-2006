#ifndef XMALLOC_H
#define XMALLOC_H

/* Prototypes for functions defined in xmalloc.c.  */
void *xmalloc (size_t n);
void *xcalloc (size_t n, size_t s);
void *xrealloc (void *p, size_t n);

#endif /* XMALLOC_H */
