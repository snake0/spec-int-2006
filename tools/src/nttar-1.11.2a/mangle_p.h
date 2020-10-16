#ifndef __MANGLE_P_H__
#define __MANGLE_P_H__

extern _VOID_ extract_mangle _P_((union record *head));
extern _VOID_ add_mangle _P_((char *name, char *buffer));
extern _VOID_ write_mangled _P_((void));

#ifdef S_ISLNK
extern _VOID_ add_symlink_mangle _P_((char *symlink, char *linkto,
                                      char *buffer));
#endif

#endif
