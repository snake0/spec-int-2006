#ifndef __DIFFARCH_P_H__
#define __DIFFARCH_P_H__

extern _VOID_ diff_init _P_((void));
extern _VOID_ diff_archive _P_((void));
extern int compare_chunk _P_((long bytes, char *buffer));
extern int compare_dir _P_((long bytes, char *buffer));
extern _VOID_ sigh _P_((char *what));
extern _VOID_ verify_volume _P_((void));
extern int do_stat _P_((struct stat *statp));
extern _VOID_ diff_sparse_files _P_((int filesize));
extern _VOID_ fill_in_sparse_array _P_((void));

#endif
