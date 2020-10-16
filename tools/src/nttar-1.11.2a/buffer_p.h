#ifndef __BUFFER_P_H__
#define __BUFFER_P_H__

extern _VOID_ reset_eof _P_((void));
extern union record * findrec _P_((void));
extern _VOID_ userec _P_((union record *rec));
extern union record * endofrecs _P_((void));
extern _VOID_ dupto _P_((int from, int to, char *msg));
extern _VOID_ child_open _P_((void));
extern int isfile _P_((char *p));
extern _VOID_ open_archive _P_((int reading));
extern _VOID_ saverec _P_((union record **pointer));
extern _VOID_ fl_write _P_((void));
extern _VOID_ writeerror _P_((int err));
extern _VOID_ readerror _P_((void));
extern _VOID_ fl_read _P_((void));
extern _VOID_ flush_archive _P_((void));
extern int backspace_output _P_((void));
extern _VOID_ close_archive _P_((void));
extern _VOID_ anno _P_((FILE *stream, char *prefix, int savedp));
extern _VOID_ init_volume_number _P_((void));
extern _VOID_ closeout_volume_number _P_((void));
extern int new_volume _P_((int type));
extern int no_op _P_((int size, char *data));
extern int wantbytes _P_((long size, int (*func)(long data_size, char *data)));

#endif
