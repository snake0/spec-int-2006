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

#ifndef XMALLOC_H_
#define XMALLOC_H_

void *xmalloc (size_t bytes);
char *xstrdup (const char *str);
char *xstrndup (const char *str, size_t maxl);

#endif /* XMALLOC_H_ */
