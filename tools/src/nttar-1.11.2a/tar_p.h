#ifndef __TAR_P_H__
#define __TAR_P_H__

extern _VOID_ blank_name_list _P_((void));
extern _VOID_ name_add _P_((char *name));
extern _VOID_ name_init _P_((int argc, char **argv));
extern char * read_name_from_file _P_((char *buffer, size_t *pbuffer_size,
                                       FILE *stream));
extern char * name_next _P_((int change_dirs));
extern _VOID_ name_close _P_((void));
extern _VOID_ name_gather _P_((void));
extern _VOID_ addname _P_((char *name));
extern int name_match _P_((register char *p));
extern _VOID_ names_notfound _P_((void));
extern _VOID_ name_expand _P_((void));
extern struct name * name_scan _P_((register char *p));
extern char * name_from_list _P_((void));
extern char * new_name _P_((char *path, char *name));
extern int confirm _P_((char *action, char *file));
extern _VOID_ add_exclude _P_((char *name));
extern _VOID_ add_exclude_file _P_((char *file));
extern int is_regex _P_((char *str));
extern int check_exclude _P_((char *name));
extern _VOID_ describe _P_((void));
extern _VOID_ options _P_((int argc, char **argv));

#endif
