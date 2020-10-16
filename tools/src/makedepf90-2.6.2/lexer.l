%{
/* 
 * Copyright (C) 2000,2001 Erik Edelmann <eedelman@beam.helsinki.fi>
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


#include <string.h>
#include <errno.h>
#include <assert.h>
#include "global.h"
#include "errormesg.h"
#include "find_dep.h"
#include "xmalloc.h"

void yy_push_state(int new_state);
void yy_pop_state();
void pop_filestack ();

static bool in_interface = false;

static YY_BUFFER_STATE include_stack[INCLUDE_RECURSION_LIMIT];
static int line_num_stack[INCLUDE_RECURSION_LIMIT];
static int incl_buff = 0;

static char string_buf[MAX_STRING_LEN];
static char *string_buf_ptr;

static int old_startcond;


%}

%option yylineno

%s free_fmt fixed_fmt
%x str_sq str_dq

%%


\"              { string_buf_ptr = string_buf; old_startcond = YY_START; 
                  BEGIN(str_dq); }
'               { string_buf_ptr = string_buf; old_startcond = YY_START; 
                  BEGIN(str_sq); }

<str_dq>\" |
<str_sq>'       { 
                    BEGIN(old_startcond);
                    *string_buf_ptr = '\0';
                    yylval.string = xstrdup (string_buf);
                    DEBUG_PRINT("string: '%s'\n", yylval.string);
                    return STRING;
                }

<str_dq,str_sq>&[ \t]*\n |
<str_dq,str_sq>&[ \t]*\n[ \t]*&  /* Ignore (continued strings, free fmt) */

<fixed_fmt,str_dq,str_sq>\n[ ]{5}[^ \t\n] /*Ignore (cont. strings, fixed fmt) */

<str_dq,str_sq>\n { 
                    unput ('\n');
                    BEGIN(old_startcond);
                    yylval.number = yylineno;
                    return UNTERMINATED_STRING;
                }

<str_sq,str_dq>. { *string_buf_ptr++ = yytext[0]; }




!.*\n                   { return EOSTMT; } /* Treat comments like */
<fixed_fmt>^[cC*dD].*\n { return EOSTMT; } /* empty lines */

#[ \t]*include  { return CPP_INCLUDE; }
\$[ \t]*include { return F90PPR_INCLUDE; }
\?\?[ \t]*include { return COCO_INCLUDE; }
INCLUDE         { return F_INCLUDE; }
USE             { return USE; }

END" "*INTERFACE { in_interface = false; }
INTERFACE       { in_interface = true; }

END" "*MODULE   /* Ignore */
MODULE          { if (!in_interface) return MODULE; }

#[ \t]*define   { return CPP_DEFINE; }
\$[ \t]*DEFINE   { return F90PPR_DEFINE; }

#[ \t]*undef    { return CPP_UNDEF; }
\$[ \t]*UNDEF   { return F90PPR_UNDEF; }

#[ \t]*ifdef    { return CPP_IFDEF; }
#[ \t]*ifndef   { return CPP_IFNDEF; }
#[ \t]*if       { return CPP_IF; }
#[ \t]*elif     { return CPP_ELIF; }
#[ \t]*else     { return CPP_ELSE; }
#[ \t]*endif    { return CPP_ENDIF; }

$[ \t]*ifdef    { return F90PPR_IFDEF; }
$[ \t]*ifndef   { return F90PPR_IFNDEF; }
$[ \t]*if       { return F90PPR_IF; }
$[ \t]*elif     { return F90PPR_ELIF; }
$[ \t]*else     { return F90PPR_ELSE; }
$[ \t]*endif    { return F90PPR_ENDIF; }

&[ \t]*\n |
&[ \t]*\n[ \t]*& /* Ignore */


[^ \t\n\r;,!'"]+ { yylval.string = xstrdup(yytext); return WORD; }

;|\n          { return EOSTMT; }

[ \t\r,]         /* Ignore */

.               { return *yytext; }


<<EOF>>         { 
                    DEBUG_PRINT("EOF reached %i\n", incl_buff);
                    incl_buff--;
                    if (incl_buff < 0) {
                        incl_buff = 0;
                        yyterminate ();
                    } else {
                        yy_delete_buffer (YY_CURRENT_BUFFER);
                        yy_switch_to_buffer (include_stack[incl_buff]);
                        yylineno = line_num_stack[incl_buff];
                        pop_filestack ();
                    }
                }

%%


bool lex_include_file (const char *incfile)
/* "Include" file 'incfile' here.  Return false for failure, true for success */
{
    FILE *bakup;

    DEBUG_PRINT("'%s'\n", incfile);

    if (incl_buff >= INCLUDE_RECURSION_LIMIT) {
        warning ("Recursion limit reached in file '%s'", incfile);
        return false;
    }

    bakup = yyin;
    yyin = fopen (incfile, "r");
    if (yyin == NULL) {
        if (errno == ENOENT) {
            if (options.warn)  warning ("Include file '%s' not found", incfile);
        } else
            warning ("Error reading file '%s', skipping", incfile);

        yyin = bakup;
        return false;
    }

    include_stack[incl_buff] = YY_CURRENT_BUFFER;
    yy_switch_to_buffer (yy_create_buffer (yyin, YY_BUF_SIZE));

    line_num_stack[incl_buff++] = yylineno;
    yylineno = 1;

    return true;
}


void lex_set_format (SourceFmt fmt)
{
    switch (fmt) {
        case UNKNOWN:
        case FREE: 
            BEGIN(free_fmt); break;
        case FIXED: 
            BEGIN(fixed_fmt); break;
        default: 
            assert(0);
    }
}


int yywrap ()
{
    return 1;
}
