#ifndef __LIST_P_H__
#define __LIST_P_H__

extern _VOID_ read_and _P_((void (*do_something)(void)));
extern _VOID_ list_archive _P_((void));
extern int read_header _P_((void));
extern _VOID_ decode_header _P_((register union record *header,
                                 register struct stat *st, int *stdp,
                                 int wantug));
extern long from_oct _P_((register int digs, register char *where));
extern _VOID_ print_header _P_((void));
extern _VOID_ pr_mkdir _P_((char *pathname, int length, int mode));
extern _VOID_ skip_file _P_((register long size));
extern _VOID_ skip_extended_headers _P_((void));
extern _VOID_ demode _P_((register unsigned mode, register char *string));

#endif
