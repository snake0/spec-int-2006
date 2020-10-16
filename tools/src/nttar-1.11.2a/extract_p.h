#ifndef __EXTRACT_P_H__
#define __EXTRACT_P_H__

extern _VOID_ extr_init _P_((void));
extern _VOID_ extract_archive _P_((void));
extern int make_dirs _P_((char *pathname));
extern _VOID_ extract_sparse_file _P_((int fd, long *sizeleft, long totalsize,
                                       char *name));
extern _VOID_ restore_saved_dir_info _P_((void));

#endif
