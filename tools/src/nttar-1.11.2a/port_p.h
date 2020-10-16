#ifndef __PORT_P_H__
#define __PORT_P_H__

extern char * valloc _P_((unsigned size));
extern _VOID_ panic _P_((char *s));
extern PTR ck_malloc _P_((size_t size));
extern char * xmalloc _P_((size_t size));
extern PTR ck_realloc _P_((PTR ptr, size_t size));
extern char * init_buffer _P_((void));
extern _VOID_ flush_buffer _P_((char *bb));
extern _VOID_ add_buffer _P_((char *bb, char *p, int n));
extern char * get_buffer _P_((char *bb));
extern char * merge_sort _P_((PTR list, unsigned n, int off,
                              int (*cmp)(PTR a, PTR b)));
extern _VOID_ ck_close _P_((int fd));
extern char * quote_copy_string _P_((char *string));
extern char * un_quote_string _P_((char *string));
extern _VOID_ ck_pipe _P_((int *pipes));
#ifdef HAVE_VPRINTF
extern _VOID_ msg _P_((char *str, ...));
extern _VOID_ msg_perror _P_((char *str, ...));
#else
extern _VOID_ msg ();
extern _VOID_ msg_perror ();
#endif

#ifndef HAVE_MKDIR
extern int mkdir _P_((char *dpath, int dmode));
extern int rmdir _P_((char *dpath));
#endif
#ifndef HAVE_RENAME
extern int rename _P_((char *from, char *to));
#endif
#ifdef minix
extern _VOID_ bzero _P_((register char *s1, register int n));
extern int bcmp _P_((register char *s1, register char *s2, register int n));
extern int execlp _P_((char *filename, char *arg0));
#endif
#ifdef EMUL_OPEN3
extern int open3 _P_((char *path, int flags, int mode));
#endif
#ifndef HAVE_MKNOD
extern int mknod _P_((char *path, unsigned short mode, dev_t dev));
extern int link _P_((char *path1, char *path2));
extern int chown _P_((char *path, int uid, int gid));
extern int geteuid _P_((void));
#endif
#ifdef __TURBOC__
extern int utime _P_((char *filename, struct utimbuf *utb));
#endif
#ifndef HAVE_STRSTR
extern char * strstr _P_((char *s, char *wanted));
#endif
#ifndef HAVE_FTRUNCATE
extern int ftruncate _P_((int fd, off_t length));
#endif

#endif
