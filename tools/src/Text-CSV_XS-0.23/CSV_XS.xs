/* -*- C -*-
 *
 *  Copyright (c) 1998 Jochen Wiedmann. All rights reserved.
 *  This program is free software; you can redistribute it and/or
 *  modify it under the same terms as Perl itself.
 *
 *
 **************************************************************************/

#include <EXTERN.h>
#include <perl.h>
#include <XSUB.h>
#include "ppport.h"


#define CSV_XS_TYPE_PV 0
#define CSV_XS_TYPE_IV 1
#define CSV_XS_TYPE_NV 2

#define CSV_XS_SELF                                 \
    if (!self  ||  !SvOK(self)  ||  !SvROK(self)    \
	||  SvTYPE(SvRV(self)) != SVt_PVHV) {       \
        croak("self is not a hash ref");            \
    }                                               \
    hv = (HV*) SvRV(self);


typedef struct {
  HV* self;
  unsigned char quoteChar;
  unsigned char escapeChar;
  unsigned char sepChar;
  int binary;
  int alwaysQuote;
  char buffer[1024];
  STRLEN used;
  STRLEN size;
  char* bptr;
  int useIO;
  SV* tmp;
  char* types;
  STRLEN types_len;
} csv_t;


static void SetupCsv(csv_t* csv, HV* self) {
  SV** svp;
  STRLEN len;
  char* ptr;

  csv->quoteChar = '"';
  if ((svp = hv_fetch(self, "quote_char", 10, 0))  &&  *svp) {
    if (!SvOK(*svp)) {
      csv->quoteChar = '\0';
    } else {
      ptr = SvPV(*svp, len);
      csv->quoteChar = len ? *ptr : '\0';
    }
  }
  csv->escapeChar = '"';
  if ((svp = hv_fetch(self, "escape_char", 11, 0))  &&  *svp) {
    if (!SvOK(*svp)) {
      csv->escapeChar = '\0';
    } else {
      ptr = SvPV(*svp, len);
      csv->escapeChar = len ? *ptr : '\0';
    }
  }
  csv->sepChar = ',';
  if ((svp = hv_fetch(self, "sep_char", 8, 0))  &&  *svp  &&	SvOK(*svp)) {
    ptr = SvPV(*svp, len);
    if (len) {
      csv->sepChar = *ptr;
    }
  }
  csv->types = NULL;
  if ((svp = hv_fetch(self, "_types", 6, 0))  &&  *svp  &&  SvOK(*svp)) {
    STRLEN len;
    csv->types = SvPV(*svp, len);
    csv->types_len = len;
  }
  csv->binary = 0;
  if ((svp = hv_fetch(self, "binary", 6, 0))  &&  *svp) {
    csv->binary = SvTRUE(*svp);
  }
  csv->alwaysQuote = 0;
  if ((svp = hv_fetch(self, "always_quote", 12, 0))  &&  *svp) {
    csv->alwaysQuote = SvTRUE(*svp);
  }
  csv->self = self;
  csv->used = 0;
}


static
int Print(csv_t* csv, SV* dst) {
  int result;

  if (csv->useIO) {
    SV* tmp = newSVpv(csv->buffer, csv->used);
    dSP;                                              
    PUSHMARK(sp);
    EXTEND(sp, 2);
    PUSHs((dst));
    PUSHs(tmp);
    PUTBACK;
    result = perl_call_method("print", G_SCALAR);
    SPAGAIN;
    if (result) {
      result = POPi;
    }
    PUTBACK;
    SvREFCNT_dec(tmp);
  } else {
    sv_catpvn(SvRV(dst), csv->buffer, csv->used);
    result = TRUE;
  }
  csv->used = 0;
  return result;
}


#define CSV_PUT(csv, dst, c)                                \
    if ((csv)->used == sizeof((csv)->buffer)-1) {           \
        Print((csv), (dst));                                \
    }                                                       \
    (csv)->buffer[(csv)->used++] = (c);


static int Encode(csv_t* csv, SV* dst, AV* fields, SV* eol) {
  int i;
  for (i = 0;  i <= av_len(fields);  i++) {
    SV** svp;
    if (i > 0) {
      CSV_PUT(csv, dst, csv->sepChar);
    }
    if ((svp = av_fetch(fields, i, 0))  &&  *svp  &&  SvOK(*svp)) {
      STRLEN len;
      char* ptr = SvPV(*svp, len);
      int quoteMe = csv->alwaysQuote;
      /*
       *  Do we need quoting? We do quote, if the user requested
       *  (alwaysQuote), if binary or blank characters are found
       *  and if the string contains quote or escape characters.
       */
      if (!quoteMe  &&
	  (quoteMe = (!SvIOK(*svp)  &&  !SvNOK(*svp)  &&
		      csv->quoteChar))) {
	char* ptr2, *ptr3;
	STRLEN l;
	for (ptr2 = ptr, l = len;  l;  ++ptr2, --l) {
	  unsigned char c = *ptr2;
	  if (c <= 0x20  ||  (c >= 0x7f  &&  c <= 0xa0)  ||
	      (csv->quoteChar && c == csv->quoteChar)  ||
	      (csv->sepChar && c == csv->sepChar)  ||
	      (csv->escapeChar  &&  c == csv->escapeChar)) {
	    /* Binary character */
	    break;
	  }
	}
	quoteMe = (l>0);
      }
      if (quoteMe) {
	CSV_PUT(csv, dst, csv->quoteChar);
      }
      while (len-- > 0) {
	char c = *ptr++;
	int e = 0;
	if (!csv->binary  &&
	    (c != '\t'  &&  (c < '\040'  ||  c > '\176'))) {
	  SvREFCNT_inc(*svp);
	  if (!hv_store(csv->self, "_ERROR_INPUT", 12, *svp, 0)) {
	    SvREFCNT_dec(*svp);
	  }
	  return FALSE;
	}
	if (csv->quoteChar  &&  c == csv->quoteChar) {
	  e = 1;
	} else if (csv->escapeChar  &&  c == csv->escapeChar) {
	  e = 1;
	} else if (c == '\0') {
	  e = 1;
	  c = '0';
	}
	if (e  &&  csv->escapeChar) {
	  CSV_PUT(csv, dst, csv->escapeChar);
	}
	CSV_PUT(csv, dst, c);
      }
      if (quoteMe) {
	CSV_PUT(csv, dst, csv->quoteChar);
      }
    }
  }
  if (eol && SvOK(eol)) {
    STRLEN len;
    char* ptr = SvPV(eol, len);
    while (len--) {
      CSV_PUT(csv, dst, *ptr++);
    }
  }
  if (csv->used) {
    Print(csv, dst);
  }
  return TRUE;
}


static void DecodeError(csv_t* csv) {
  if(csv->tmp) {
    if (hv_store(csv->self, "_ERROR_INPUT", 12, csv->tmp, 0)) {
      SvREFCNT_inc(csv->tmp);
    }
  }
}

static int CsvGet(csv_t* csv, SV* src) {
  if (!csv->useIO) {
    return EOF;
  }
  {
    int result;
    dSP;
    PUSHMARK(sp);
    EXTEND(sp, 1);
    PUSHs(src);
    PUTBACK;
    result = perl_call_method("getline", G_SCALAR);
    SPAGAIN;
    if (result) {
      csv->tmp = POPs;
    } else {
      csv->tmp = NULL;
    }
    PUTBACK;
  }
  if (csv->tmp  &&  SvOK(csv->tmp)) {
    csv->bptr = SvPV(csv->tmp, csv->size);
    csv->used = 0;
    if (csv->size) {
      return ((unsigned char) csv->bptr[csv->used++]);
    }
  }
  return EOF;
}

#define ERROR_INSIDE_QUOTES                                        \
    SvREFCNT_dec(insideQuotes);                                    \
    DecodeError(csv);                                              \
    return FALSE;
#define ERROR_INSIDE_FIELD                                         \
    SvREFCNT_dec(insideField);                                     \
    DecodeError(csv);                                              \
    return FALSE;

#define CSV_PUT_SV(sv, c)                                          \
    len = SvCUR((sv));                                             \
    SvGROW((sv), len+2);                                           \
    *SvEND((sv)) = c;                                              \
    SvCUR_set((sv), len+1)

#define CSV_GET                                                    \
    ((c_ungetc != EOF) ? c_ungetc :                                \
     ((csv->used < csv->size) ?                                    \
      ((unsigned char) csv->bptr[(csv)->used++]) : CsvGet(csv, src)))

#define AV_PUSH(fields, sv)                                        \
    *SvEND(sv) = '\0';                                             \
    av_push(fields, sv);

static int Decode(csv_t* csv, SV* src, AV* fields) {
  int c;
  int c_ungetc = EOF;
  int waitingForField = 1;
  SV* insideQuotes = NULL;
  SV* insideField = NULL;
  STRLEN len;
  int seenSomething = FALSE;
  
  while ((c = CSV_GET)  !=  EOF) {
    seenSomething = TRUE;
restart:
    if (c == csv->sepChar) {
      if (waitingForField) {
	av_push(fields, newSVpv("", 0));
      } else if (insideQuotes) {
	CSV_PUT_SV(insideQuotes, c);
      } else {
	AV_PUSH(fields, insideField);
	insideField = NULL;
	waitingForField = 1;
      }
    } else if (c == '\012') {
      if (waitingForField) {
	av_push(fields, newSVpv("", 0));
	return TRUE;
      } else if (insideQuotes) {
	if (!csv->binary) {
	  ERROR_INSIDE_QUOTES;
	}
	CSV_PUT_SV(insideQuotes, c);
      } else {
	AV_PUSH(fields, insideField);
	return TRUE;
      }
    } else if (c == '\015') {
      if (waitingForField) {
	int c2 = CSV_GET;
	if (c2 == EOF) {
	  insideField = newSVpv("", 0);
	  waitingForField = 0;
	  goto restart;
	} else if (c2 == '\012') {
	  c = '\012';
	  goto restart;
	} else {
	  c_ungetc = c2;
	  insideField = newSVpv("", 0);
	  waitingForField = 0;
	  goto restart;
	}
      } else if (insideQuotes) {
	if (!csv->binary) {
	  ERROR_INSIDE_QUOTES;
	}
	CSV_PUT_SV(insideQuotes, c);
      } else {
	int c2 = CSV_GET;
	if (c2 == '\012') {
	  AV_PUSH(fields, insideField);
	  return TRUE;
	} else {
	  ERROR_INSIDE_FIELD;
	}
      }
    } else if (c == csv->quoteChar) {
      if (waitingForField) {
	insideQuotes = newSVpv("", 0);
	waitingForField = 0;
      } else if (insideQuotes) {
	int c2;
	if (!csv->escapeChar  ||  c != csv->escapeChar) {
	  /* Field is terminated */
	  AV_PUSH(fields, insideQuotes);
	  insideQuotes = NULL;
	  waitingForField = 1;
	  c2 = CSV_GET;
	  if (c2 == csv->sepChar) {
	    continue;
	  } else if (c2 == EOF) {
	    return TRUE;
	  } else if (c2 == '\015') {
	    int c3 = CSV_GET;
	    if (c3 == '\012') {
	      return TRUE;
	    }
	    DecodeError(csv);
	    return FALSE;
	  } else if (c2 == '\012') {
	    return TRUE;
	  } else {
	    DecodeError(csv);
	    return FALSE;
	  }
	}
	c2 = CSV_GET;
	if (c2 == EOF) {
	  AV_PUSH(fields, insideQuotes);
	  return TRUE;
	} else if (c2 == csv->sepChar) {
	  AV_PUSH(fields, insideQuotes);
	  insideQuotes = NULL;
	  waitingForField = 1;
	} else if (c2 == '0') {
	  CSV_PUT_SV(insideQuotes, (int) '\0');
	} else if (c2 == csv->quoteChar  ||  c2 == csv->sepChar) {
	  CSV_PUT_SV(insideQuotes, c2);
	} else if (c2 == '\012') {
	  AV_PUSH(fields, insideQuotes);
	  return TRUE;
	} else if (c2 == '\015') {
	  int c3 = CSV_GET;
	  if (c3 == '\012') {
	    AV_PUSH(fields, insideQuotes);
	    return TRUE;
	  }
	  ERROR_INSIDE_QUOTES;
	} else {
	  ERROR_INSIDE_QUOTES;
	}
      } else if (csv->quoteChar  &&  csv->quoteChar != csv->escapeChar) {
	if (!csv->binary  &&
	    (c != '\011'  &&  (c < '\040'  ||  c > '\176'))) {
	  ERROR_INSIDE_FIELD;
	}
	CSV_PUT_SV(insideField, c);
      } else {
	ERROR_INSIDE_FIELD;
      }
    } else if (csv->escapeChar  &&  c == csv->escapeChar) {
      /*  This means quoteChar != escapeChar  */
      if (waitingForField) {
	insideField = newSVpv("", 0);
	waitingForField = 0;
      } else if (insideQuotes) {
	int c2 = CSV_GET;
	if (c2 == EOF) {
	  ERROR_INSIDE_QUOTES;
	} else if (c2 == '0') {
	  CSV_PUT_SV(insideQuotes, (int) '\0');
	} else if (c2 == csv->quoteChar  ||  c2 == csv->sepChar  ||
		   c2 == csv->escapeChar) {
	  /* c2 == csv->escapeChar added 28-06-1999,
	   * Pavel Kotala <pkotala@logis.cz>
	   */
	  CSV_PUT_SV(insideQuotes, c2);
	} else {
	  ERROR_INSIDE_QUOTES;
	}
      } else if (insideField) {
	int c2 = CSV_GET;
	if (c2 == EOF) {
	  ERROR_INSIDE_FIELD;
	} else {
	  CSV_PUT_SV(insideField, c2);
	}
      } else {
	ERROR_INSIDE_FIELD;
      }
    } else {
      if (waitingForField) {
	insideField = newSVpv("", 0);
	waitingForField = 0;
	goto restart;
      } else if (insideQuotes) {
	if (!csv->binary  &&
	    (c != '\011'  &&  (c < '\040'  ||  c > '\176'))) {
	  ERROR_INSIDE_QUOTES;
	}
	CSV_PUT_SV(insideQuotes, c);
      } else {
	if (!csv->binary  &&
	    (c != '\011'  &&  (c < '\040'  ||  c > '\176'))) {
	  ERROR_INSIDE_FIELD;
	}
	CSV_PUT_SV(insideField, c);
      }
    }
  }

  if (waitingForField) {
    if (seenSomething) {
      av_push(fields, newSVpv("", 0));
    }
  } else if (insideQuotes) {
    ERROR_INSIDE_QUOTES;
  } else if (insideField) {
    AV_PUSH(fields, insideField);
  }
  return TRUE;
}


static int xsDecode(HV* hv, AV* av, SV* src, bool useIO) {
  csv_t csv;
  int result;

  SetupCsv(&csv, hv);
  if ((csv.useIO = useIO)) {
    csv.tmp = NULL;
    csv.size = 0;
  } else {
    STRLEN size;
    csv.tmp = src;
    csv.bptr = SvPV(src, size);
    csv.size = size;
  }
  result = Decode(&csv, src, av);
  if (result  &&  csv.types) {
    I32 i, len = av_len(av);
    SV** svp;
    
    for (i = 0;  i <= len  &&  i <= csv.types_len;  i++) {
      if ((svp = av_fetch(av, i, 0))  &&  *svp  &&  SvOK(*svp)) {
	switch (csv.types[i]) {
	case CSV_XS_TYPE_IV:
	  sv_setiv(*svp, SvIV(*svp));
	  break;
	case CSV_XS_TYPE_NV:
	  sv_setnv(*svp, SvIV(*svp));
	  break;
	}
      }
    }
  }
  return result;
}


static int xsEncode(HV* hv, AV* av, SV* io, bool useIO, SV* eol) {
  csv_t csv;
  SetupCsv(&csv, hv);
  csv.useIO = useIO;
  return Encode(&csv, io, av, eol);
}


MODULE = Text::CSV_XS		PACKAGE = Text::CSV_XS

PROTOTYPES: ENABLE


SV*
Encode(self, dst, fields, useIO, eol)
    SV* self
    SV* dst
    SV* fields
    bool useIO
    SV* eol
  PROTOTYPE: $$$$
  PPCODE:
    {
      HV* hv;
      AV* av;

      CSV_XS_SELF;
      if (!fields  ||  !SvOK(fields)  ||  !SvROK(fields)
	  ||  SvTYPE(SvRV(fields)) != SVt_PVAV) {
	croak("fields is not an array ref");
      } else {
	av = (AV*) SvRV(fields);
      }
      
      ST(0) = xsEncode(hv, av, dst, useIO, eol) ? &PL_sv_yes : &PL_sv_undef;
      XSRETURN(1);
    }


SV*
Decode(self, src, fields, useIO)
    SV* self
    SV* src
    SV* fields
    bool useIO
  PROTOTYPE: $$$$
  PPCODE:
    {
      HV* hv;
      AV* av;
      int result;

      CSV_XS_SELF;
      if (!fields  ||  !SvOK(fields)  ||  !SvROK(fields)
	  ||  SvTYPE(SvRV(fields)) != SVt_PVAV) {
	croak("fields is not an array ref");
      } else {
	av = (AV*) SvRV(fields);
      }

      ST(0) = xsDecode(hv, av, src, useIO) ? &PL_sv_yes : &PL_sv_no;
      XSRETURN(1);
    }


void
print(self, io, fields)
    SV* self
    SV* io
    SV* fields
  PROTOTYPE: $$$
  PPCODE:
    {
      HV* hv;
      AV* av;
      SV* eol;
      SV** svp;

      CSV_XS_SELF;
      if (!fields  ||  !SvOK(fields)  ||  !SvROK(fields)  ||
	  SvTYPE(SvRV(fields)) != SVt_PVAV) {
	croak("Expected fields to be an array ref");
      }
      av = (AV*) SvRV(fields);
      if ((svp = hv_fetch(hv, "eol", 3, FALSE))) {
	eol = *svp;
      } else {
	eol = &PL_sv_undef;
      }
      ST(0) = xsEncode(hv, av, io, 1, eol) ? &PL_sv_yes : &PL_sv_no;
      XSRETURN(1);
    }


void
getline(self, io)
    SV* self
    SV* io
  PROTOTYPE: $;$
  PPCODE:
    {
      HV* hv;
      AV* av;
      SV* rv;

      CSV_XS_SELF;
      hv_delete(hv, "_ERROR_INPUT", 12, G_DISCARD);
      av = newAV();
      ST(0) = xsDecode(hv, av, io, 1) ?
	sv_2mortal(newRV_noinc((SV*) av)) : &PL_sv_undef;
      XSRETURN(1);
    }
