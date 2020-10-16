#ifndef __CREATE_P_H__
#define __CREATE_P_H__

extern _VOID_ create_archive _P_((void));
extern _VOID_ dump_file _P_((char *p, int curdev, int toplevel));
extern int finish_sparse_file _P_((int fd, long *sizeleft, long fullsize,
                                   char *name));
extern _VOID_ init_sparsearray _P_((void));
extern int deal_with_sparse _P_((char *name, union record *header,
                                 int nulls_at_end));
extern _VOID_ clear_buffer _P_((char *buf));
extern int zero_record _P_((char *buffer));
extern _VOID_ find_new_file_size _P_((int *filesize, int highest_index));
extern union record * start_header _P_((char *name, register struct stat *st));
extern _VOID_ finish_header _P_((register union record *header));
extern _VOID_ to_oct _P_((register long value, register int digs,
                          register char *where));
extern _VOID_ write_eot _P_((void));
extern _VOID_ write_long _P_((char *p, char type));

#endif
