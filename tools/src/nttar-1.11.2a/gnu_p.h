#ifndef __GNU_P_H__
#define __GNU_P_H__

extern _VOID_ add_dir _P_((char *name, dev_t dev, ino_t ino, char *text));
extern _VOID_ read_dir_file _P_((void));
extern _VOID_ write_dir_file _P_((void));
extern struct dirname * get_dir _P_((char *name));
extern _VOID_ collect_and_sort_names _P_((void));
extern int name_cmp _P_((struct name *n1, struct name *n2));
extern int dirent_cmp _P_((const PTR p1, const PTR p2));
extern char * get_dir_contents _P_((char *p, int device));
static void add_dir_name _P_((char *p, int device));
extern int is_dot_or_dotdot _P_((char *p));
extern _VOID_ gnu_restore _P_((int skipcrud));
extern int recursively_delete _P_((char *path));

#endif
