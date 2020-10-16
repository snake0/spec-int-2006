#ifndef __UPDATE_P_H__
#define __UPDATE_P_H__

extern _VOID_ update_archive _P_((void));
extern _VOID_ append_file _P_((char *p));
extern _VOID_ junk_archive _P_((void));
extern _VOID_ write_block _P_((int f));
extern int move_arch _P_((int n));

#endif
