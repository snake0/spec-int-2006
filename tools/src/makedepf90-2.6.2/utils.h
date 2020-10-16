/* 
 * Copyright (C) 2000, 2001 Erik Edelmann <eedelman@beam.helsinki.fi>
 *
 *     This program is free software;  you  can  redistribute  it
 *     and/or modify it under the terms of the GNU General Public
 *     License version 2 as published  by  the  Free  Software  
 *     Foundation.
 *
 *     This program is distributed in the hope that  it  will  be
 *     useful, but WITHOUT ANY WARRANTY; without even the implied
 *     warranty of MERCHANTABILITY or FITNESS  FOR  A  PARTICULAR
 *     PURPOSE.   See  the  GNU  General  Public License for more
 *     details.
 *
 *     You should have received a copy of the GNU General  Public
 *     License along with this program; if not, write to the Free
 *     Software Foundation, Inc., 59  Temple  Place,  Suite  330,
 *     Boston, MA  02111-1307  USA
 */
#ifndef UTILS_H_
#define UTILS_H_

char *replace_suffix (const char *filename, const char *new_suffix);
char *set_path (const char *filename, const char *path);
char *remove_citation (const char *s);
char *expand_rule(const char *rule, const char *srcfile);
char *get_suffix (const char *filename);

#endif /* UTILS_H_ */
