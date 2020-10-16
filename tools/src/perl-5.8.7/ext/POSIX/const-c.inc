#define PERL_constant_NOTFOUND	1
#define PERL_constant_NOTDEF	2
#define PERL_constant_ISIV	3
#define PERL_constant_ISNO	4
#define PERL_constant_ISNV	5
#define PERL_constant_ISPV	6
#define PERL_constant_ISPVN	7
#define PERL_constant_ISSV	8
#define PERL_constant_ISUNDEF	9
#define PERL_constant_ISUV	10
#define PERL_constant_ISYES	11

#ifndef NVTYPE
typedef double NV; /* 5.6 and later define NVTYPE, and typedef NV to it.  */
#endif
#ifndef aTHX_
#define aTHX_ /* 5.6 or later define this for threading support.  */
#endif
#ifndef pTHX_
#define pTHX_ /* 5.6 or later define this for threading support.  */
#endif

static int
constant_3 (pTHX_ const char *name, IV *iv_return) {
  /* When generated this function returned values for the list of names given
     here.  However, subsequent manual editing may have added or removed some.
     B50 B75 CS5 CS6 CS7 CS8 EIO EOF */
  /* Offset 2 gives the best switch position.  */
  switch (name[2]) {
  case '0':
    if (name[0] == 'B' && name[1] == '5') {
#ifdef B50
      *iv_return = B50;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case '5':
    if (name[0] == 'B' && name[1] == '7') {
#ifdef B75
      *iv_return = B75;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (name[0] == 'C' && name[1] == 'S') {
#ifdef CS5
      *iv_return = CS5;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case '6':
    if (name[0] == 'C' && name[1] == 'S') {
#ifdef CS6
      *iv_return = CS6;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case '7':
    if (name[0] == 'C' && name[1] == 'S') {
#ifdef CS7
      *iv_return = CS7;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case '8':
    if (name[0] == 'C' && name[1] == 'S') {
#ifdef CS8
      *iv_return = CS8;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'F':
    if (name[0] == 'E' && name[1] == 'O') {
#ifdef EOF
      *iv_return = EOF;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'O':
    if (name[0] == 'E' && name[1] == 'I') {
#ifdef EIO
      *iv_return = EIO;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  }
  return PERL_constant_NOTFOUND;
}

static int
constant_4 (pTHX_ const char *name, IV *iv_return) {
  /* When generated this function returned values for the list of names given
     here.  However, subsequent manual editing may have added or removed some.
     B110 B134 B150 B200 B300 B600 ECHO EDOM F_OK ISIG IXON NCCS NULL R_OK VEOF
     VEOL VMIN W_OK X_OK */
  /* Offset 1 gives the best switch position.  */
  switch (name[1]) {
  case '1':
    if (memEQ(name, "B110", 4)) {
    /*                ^        */
#ifdef B110
      *iv_return = B110;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "B134", 4)) {
    /*                ^        */
#ifdef B134
      *iv_return = B134;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "B150", 4)) {
    /*                ^        */
#ifdef B150
      *iv_return = B150;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case '2':
    if (memEQ(name, "B200", 4)) {
    /*                ^        */
#ifdef B200
      *iv_return = B200;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case '3':
    if (memEQ(name, "B300", 4)) {
    /*                ^        */
#ifdef B300
      *iv_return = B300;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case '6':
    if (memEQ(name, "B600", 4)) {
    /*                ^        */
#ifdef B600
      *iv_return = B600;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'C':
    if (memEQ(name, "ECHO", 4)) {
    /*                ^        */
#ifdef ECHO
      *iv_return = ECHO;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "NCCS", 4)) {
    /*                ^        */
#ifdef NCCS
      *iv_return = NCCS;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'D':
    if (memEQ(name, "EDOM", 4)) {
    /*                ^        */
#ifdef EDOM
      *iv_return = EDOM;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'E':
    if (memEQ(name, "VEOF", 4)) {
    /*                ^        */
#ifdef VEOF
      *iv_return = VEOF;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "VEOL", 4)) {
    /*                ^        */
#ifdef VEOL
      *iv_return = VEOL;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'M':
    if (memEQ(name, "VMIN", 4)) {
    /*                ^        */
#ifdef VMIN
      *iv_return = VMIN;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'S':
    if (memEQ(name, "ISIG", 4)) {
    /*                ^        */
#ifdef ISIG
      *iv_return = ISIG;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'U':
    if (memEQ(name, "NULL", 4)) {
    /*                ^        */
#ifdef NULL
      *iv_return = 0;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'X':
    if (memEQ(name, "IXON", 4)) {
    /*                ^        */
#ifdef IXON
      *iv_return = IXON;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case '_':
    if (memEQ(name, "F_OK", 4)) {
    /*                ^        */
#ifdef F_OK
      *iv_return = F_OK;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "R_OK", 4)) {
    /*                ^        */
#ifdef R_OK
      *iv_return = R_OK;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "W_OK", 4)) {
    /*                ^        */
#ifdef W_OK
      *iv_return = W_OK;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "X_OK", 4)) {
    /*                ^        */
#ifdef X_OK
      *iv_return = X_OK;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  }
  return PERL_constant_NOTFOUND;
}

static int
constant_5 (pTHX_ const char *name, IV *iv_return) {
  /* When generated this function returned values for the list of names given
     here.  However, subsequent manual editing may have added or removed some.
     B1200 B1800 B2400 B4800 B9600 CREAD CSIZE E2BIG EBADF EBUSY ECHOE ECHOK
     EFBIG EINTR ELOOP ENXIO EPERM EPIPE EROFS ESRCH EXDEV HUPCL ICRNL IGNCR
     INLCR INPCK IXOFF OPOST TCION TCOON VINTR VKILL VQUIT VSTOP VSUSP VTIME */
  /* Offset 1 gives the best switch position.  */
  switch (name[1]) {
  case '1':
    if (memEQ(name, "B1200", 5)) {
    /*                ^         */
#ifdef B1200
      *iv_return = B1200;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "B1800", 5)) {
    /*                ^         */
#ifdef B1800
      *iv_return = B1800;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case '2':
    if (memEQ(name, "B2400", 5)) {
    /*                ^         */
#ifdef B2400
      *iv_return = B2400;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "E2BIG", 5)) {
    /*                ^         */
#ifdef E2BIG
      *iv_return = E2BIG;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case '4':
    if (memEQ(name, "B4800", 5)) {
    /*                ^         */
#ifdef B4800
      *iv_return = B4800;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case '9':
    if (memEQ(name, "B9600", 5)) {
    /*                ^         */
#ifdef B9600
      *iv_return = B9600;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'B':
    if (memEQ(name, "EBADF", 5)) {
    /*                ^         */
#ifdef EBADF
      *iv_return = EBADF;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "EBUSY", 5)) {
    /*                ^         */
#ifdef EBUSY
      *iv_return = EBUSY;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'C':
    if (memEQ(name, "ECHOE", 5)) {
    /*                ^         */
#ifdef ECHOE
      *iv_return = ECHOE;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "ECHOK", 5)) {
    /*                ^         */
#ifdef ECHOK
      *iv_return = ECHOK;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "ICRNL", 5)) {
    /*                ^         */
#ifdef ICRNL
      *iv_return = ICRNL;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "TCION", 5)) {
    /*                ^         */
#ifdef TCION
      *iv_return = TCION;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "TCOON", 5)) {
    /*                ^         */
#ifdef TCOON
      *iv_return = TCOON;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'F':
    if (memEQ(name, "EFBIG", 5)) {
    /*                ^         */
#ifdef EFBIG
      *iv_return = EFBIG;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'G':
    if (memEQ(name, "IGNCR", 5)) {
    /*                ^         */
#ifdef IGNCR
      *iv_return = IGNCR;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'I':
    if (memEQ(name, "EINTR", 5)) {
    /*                ^         */
#ifdef EINTR
      *iv_return = EINTR;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "VINTR", 5)) {
    /*                ^         */
#ifdef VINTR
      *iv_return = VINTR;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'K':
    if (memEQ(name, "VKILL", 5)) {
    /*                ^         */
#ifdef VKILL
      *iv_return = VKILL;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'L':
    if (memEQ(name, "ELOOP", 5)) {
    /*                ^         */
#ifdef ELOOP
      *iv_return = ELOOP;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'N':
    if (memEQ(name, "ENXIO", 5)) {
    /*                ^         */
#ifdef ENXIO
      *iv_return = ENXIO;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "INLCR", 5)) {
    /*                ^         */
#ifdef INLCR
      *iv_return = INLCR;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "INPCK", 5)) {
    /*                ^         */
#ifdef INPCK
      *iv_return = INPCK;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'P':
    if (memEQ(name, "EPERM", 5)) {
    /*                ^         */
#ifdef EPERM
      *iv_return = EPERM;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "EPIPE", 5)) {
    /*                ^         */
#ifdef EPIPE
      *iv_return = EPIPE;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "OPOST", 5)) {
    /*                ^         */
#ifdef OPOST
      *iv_return = OPOST;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'Q':
    if (memEQ(name, "VQUIT", 5)) {
    /*                ^         */
#ifdef VQUIT
      *iv_return = VQUIT;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'R':
    if (memEQ(name, "CREAD", 5)) {
    /*                ^         */
#ifdef CREAD
      *iv_return = CREAD;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "EROFS", 5)) {
    /*                ^         */
#ifdef EROFS
      *iv_return = EROFS;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'S':
    if (memEQ(name, "CSIZE", 5)) {
    /*                ^         */
#ifdef CSIZE
      *iv_return = CSIZE;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "ESRCH", 5)) {
    /*                ^         */
#ifdef ESRCH
      *iv_return = ESRCH;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "VSTOP", 5)) {
    /*                ^         */
#ifdef VSTOP
      *iv_return = VSTOP;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "VSUSP", 5)) {
    /*                ^         */
#ifdef VSUSP
      *iv_return = VSUSP;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'T':
    if (memEQ(name, "VTIME", 5)) {
    /*                ^         */
#ifdef VTIME
      *iv_return = VTIME;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'U':
    if (memEQ(name, "HUPCL", 5)) {
    /*                ^         */
#ifdef HUPCL
      *iv_return = HUPCL;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'X':
    if (memEQ(name, "EXDEV", 5)) {
    /*                ^         */
#ifdef EXDEV
      *iv_return = EXDEV;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "IXOFF", 5)) {
    /*                ^         */
#ifdef IXOFF
      *iv_return = IXOFF;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  }
  return PERL_constant_NOTFOUND;
}

static int
constant_6 (pTHX_ const char *name, IV *iv_return) {
  /* When generated this function returned values for the list of names given
     here.  However, subsequent manual editing may have added or removed some.
     B19200 B38400 BRKINT BUFSIZ CLOCAL CSTOPB EACCES EAGAIN ECHILD ECHONL
     EDQUOT EEXIST EFAULT EINVAL EISDIR EMFILE EMLINK ENFILE ENODEV ENOENT
     ENOLCK ENOMEM ENOSPC ENOSYS ENOTTY ERANGE ESPIPE ESTALE EUSERS ICANON
     IEXTEN IGNBRK IGNPAR ISTRIP LC_ALL NOFLSH O_EXCL O_RDWR PARENB PARMRK
     PARODD SIGFPE SIGHUP SIGILL SIGINT TCIOFF TCOOFF TOSTOP VERASE VSTART */
  /* Offset 3 gives the best switch position.  */
  switch (name[3]) {
  case '2':
    if (memEQ(name, "B19200", 6)) {
    /*                  ^        */
#ifdef B19200
      *iv_return = B19200;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case '4':
    if (memEQ(name, "B38400", 6)) {
    /*                  ^        */
#ifdef B38400
      *iv_return = B38400;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'A':
    if (memEQ(name, "EAGAIN", 6)) {
    /*                  ^        */
#ifdef EAGAIN
      *iv_return = EAGAIN;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "ESTALE", 6)) {
    /*                  ^        */
#ifdef ESTALE
      *iv_return = ESTALE;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "LC_ALL", 6)) {
    /*                  ^        */
#ifdef LC_ALL
      *iv_return = LC_ALL;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "VERASE", 6)) {
    /*                  ^        */
#ifdef VERASE
      *iv_return = VERASE;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "VSTART", 6)) {
    /*                  ^        */
#ifdef VSTART
      *iv_return = VSTART;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'B':
    if (memEQ(name, "IGNBRK", 6)) {
    /*                  ^        */
#ifdef IGNBRK
      *iv_return = IGNBRK;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'C':
    if (memEQ(name, "CLOCAL", 6)) {
    /*                  ^        */
#ifdef CLOCAL
      *iv_return = CLOCAL;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "EACCES", 6)) {
    /*                  ^        */
#ifdef EACCES
      *iv_return = EACCES;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'D':
    if (memEQ(name, "EISDIR", 6)) {
    /*                  ^        */
#ifdef EISDIR
      *iv_return = EISDIR;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "ENODEV", 6)) {
    /*                  ^        */
#ifdef ENODEV
      *iv_return = ENODEV;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "O_RDWR", 6)) {
    /*                  ^        */
#ifdef O_RDWR
      *iv_return = O_RDWR;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'E':
    if (memEQ(name, "ENOENT", 6)) {
    /*                  ^        */
#ifdef ENOENT
      *iv_return = ENOENT;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "EUSERS", 6)) {
    /*                  ^        */
#ifdef EUSERS
      *iv_return = EUSERS;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "PARENB", 6)) {
    /*                  ^        */
#ifdef PARENB
      *iv_return = PARENB;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'F':
    if (memEQ(name, "SIGFPE", 6)) {
    /*                  ^        */
#ifdef SIGFPE
      *iv_return = SIGFPE;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'H':
    if (memEQ(name, "SIGHUP", 6)) {
    /*                  ^        */
#ifdef SIGHUP
      *iv_return = SIGHUP;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'I':
    if (memEQ(name, "BRKINT", 6)) {
    /*                  ^        */
#ifdef BRKINT
      *iv_return = BRKINT;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "ECHILD", 6)) {
    /*                  ^        */
#ifdef ECHILD
      *iv_return = ECHILD;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "EEXIST", 6)) {
    /*                  ^        */
#ifdef EEXIST
      *iv_return = EEXIST;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "EMFILE", 6)) {
    /*                  ^        */
#ifdef EMFILE
      *iv_return = EMFILE;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "EMLINK", 6)) {
    /*                  ^        */
#ifdef EMLINK
      *iv_return = EMLINK;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "ENFILE", 6)) {
    /*                  ^        */
#ifdef ENFILE
      *iv_return = ENFILE;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "ESPIPE", 6)) {
    /*                  ^        */
#ifdef ESPIPE
      *iv_return = ESPIPE;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "SIGILL", 6)) {
    /*                  ^        */
#ifdef SIGILL
      *iv_return = SIGILL;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "SIGINT", 6)) {
    /*                  ^        */
#ifdef SIGINT
      *iv_return = SIGINT;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'L':
    if (memEQ(name, "ENOLCK", 6)) {
    /*                  ^        */
#ifdef ENOLCK
      *iv_return = ENOLCK;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "NOFLSH", 6)) {
    /*                  ^        */
#ifdef NOFLSH
      *iv_return = NOFLSH;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'M':
    if (memEQ(name, "ENOMEM", 6)) {
    /*                  ^        */
#ifdef ENOMEM
      *iv_return = ENOMEM;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "PARMRK", 6)) {
    /*                  ^        */
#ifdef PARMRK
      *iv_return = PARMRK;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'N':
    if (memEQ(name, "ERANGE", 6)) {
    /*                  ^        */
#ifdef ERANGE
      *iv_return = ERANGE;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "ICANON", 6)) {
    /*                  ^        */
#ifdef ICANON
      *iv_return = ICANON;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'O':
    if (memEQ(name, "CSTOPB", 6)) {
    /*                  ^        */
#ifdef CSTOPB
      *iv_return = CSTOPB;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "ECHONL", 6)) {
    /*                  ^        */
#ifdef ECHONL
      *iv_return = ECHONL;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "PARODD", 6)) {
    /*                  ^        */
#ifdef PARODD
      *iv_return = PARODD;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "TCIOFF", 6)) {
    /*                  ^        */
#ifdef TCIOFF
      *iv_return = TCIOFF;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "TCOOFF", 6)) {
    /*                  ^        */
#ifdef TCOOFF
      *iv_return = TCOOFF;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'P':
    if (memEQ(name, "IGNPAR", 6)) {
    /*                  ^        */
#ifdef IGNPAR
      *iv_return = IGNPAR;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'R':
    if (memEQ(name, "ISTRIP", 6)) {
    /*                  ^        */
#ifdef ISTRIP
      *iv_return = ISTRIP;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'S':
    if (memEQ(name, "BUFSIZ", 6)) {
    /*                  ^        */
#ifdef BUFSIZ
      *iv_return = BUFSIZ;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "ENOSPC", 6)) {
    /*                  ^        */
#ifdef ENOSPC
      *iv_return = ENOSPC;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "ENOSYS", 6)) {
    /*                  ^        */
#ifdef ENOSYS
      *iv_return = ENOSYS;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'T':
    if (memEQ(name, "ENOTTY", 6)) {
    /*                  ^        */
#ifdef ENOTTY
      *iv_return = ENOTTY;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "IEXTEN", 6)) {
    /*                  ^        */
#ifdef IEXTEN
      *iv_return = IEXTEN;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "TOSTOP", 6)) {
    /*                  ^        */
#ifdef TOSTOP
      *iv_return = TOSTOP;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'U':
    if (memEQ(name, "EDQUOT", 6)) {
    /*                  ^        */
#ifdef EDQUOT
      *iv_return = EDQUOT;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "EFAULT", 6)) {
    /*                  ^        */
#ifdef EFAULT
      *iv_return = EFAULT;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'V':
    if (memEQ(name, "EINVAL", 6)) {
    /*                  ^        */
#ifdef EINVAL
      *iv_return = EINVAL;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'X':
    if (memEQ(name, "O_EXCL", 6)) {
    /*                  ^        */
#ifdef O_EXCL
      *iv_return = O_EXCL;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  }
  return PERL_constant_NOTFOUND;
}

static int
constant_7 (pTHX_ const char *name, IV *iv_return, NV *nv_return) {
  /* When generated this function returned values for the list of names given
     here.  However, subsequent manual editing may have added or removed some.
     ARG_MAX CLK_TCK DBL_DIG DBL_MAX DBL_MIN EDEADLK EISCONN ENOBUFS ENOEXEC
     ENOTBLK ENOTDIR EREMOTE ETXTBSY FLT_DIG FLT_MAX FLT_MIN F_DUPFD F_GETFD
     F_GETFL F_GETLK F_RDLCK F_SETFD F_SETFL F_SETLK F_UNLCK F_WRLCK INT_MAX
     INT_MIN LC_TIME O_CREAT O_TRUNC SIGABRT SIGALRM SIGCHLD SIGCONT SIGKILL
     SIGPIPE SIGQUIT SIGSEGV SIGSTOP SIGTERM SIGTSTP SIGTTIN SIGTTOU SIGUSR1
     SIGUSR2 SIG_DFL SIG_ERR SIG_IGN S_IRGRP S_IROTH S_IRUSR S_IRWXG S_IRWXO
     S_IRWXU S_ISGID S_ISUID S_IWGRP S_IWOTH S_IWUSR S_IXGRP S_IXOTH S_IXUSR
     TCSANOW TMP_MAX WNOHANG */
  /* Offset 6 gives the best switch position.  */
  switch (name[6]) {
  case '1':
    if (memEQ(name, "SIGUSR", 6)) {
    /*                     1     */
#ifdef SIGUSR1
      *iv_return = SIGUSR1;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case '2':
    if (memEQ(name, "SIGUSR", 6)) {
    /*                     2     */
#ifdef SIGUSR2
      *iv_return = SIGUSR2;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'C':
    if (memEQ(name, "ENOEXE", 6)) {
    /*                     C     */
#ifdef ENOEXEC
      *iv_return = ENOEXEC;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "O_TRUN", 6)) {
    /*                     C     */
#ifdef O_TRUNC
      *iv_return = O_TRUNC;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'D':
    if (memEQ(name, "F_DUPF", 6)) {
    /*                     D     */
#ifdef F_DUPFD
      *iv_return = F_DUPFD;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "F_GETF", 6)) {
    /*                     D     */
#ifdef F_GETFD
      *iv_return = F_GETFD;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "F_SETF", 6)) {
    /*                     D     */
#ifdef F_SETFD
      *iv_return = F_SETFD;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "SIGCHL", 6)) {
    /*                     D     */
#ifdef SIGCHLD
      *iv_return = SIGCHLD;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "S_ISGI", 6)) {
    /*                     D     */
#ifdef S_ISGID
      *iv_return = S_ISGID;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "S_ISUI", 6)) {
    /*                     D     */
#ifdef S_ISUID
      *iv_return = S_ISUID;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'E':
    if (memEQ(name, "EREMOT", 6)) {
    /*                     E     */
#ifdef EREMOTE
      *iv_return = EREMOTE;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "LC_TIM", 6)) {
    /*                     E     */
#ifdef LC_TIME
      *iv_return = LC_TIME;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "SIGPIP", 6)) {
    /*                     E     */
#ifdef SIGPIPE
      *iv_return = SIGPIPE;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'G':
    if (memEQ(name, "DBL_DI", 6)) {
    /*                     G     */
#ifdef DBL_DIG
      *nv_return = DBL_DIG;
      return PERL_constant_ISNV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "FLT_DI", 6)) {
    /*                     G     */
#ifdef FLT_DIG
      *nv_return = FLT_DIG;
      return PERL_constant_ISNV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "S_IRWX", 6)) {
    /*                     G     */
#ifdef S_IRWXG
      *iv_return = S_IRWXG;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "WNOHAN", 6)) {
    /*                     G     */
#ifdef WNOHANG
      *iv_return = WNOHANG;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'H':
    if (memEQ(name, "S_IROT", 6)) {
    /*                     H     */
#ifdef S_IROTH
      *iv_return = S_IROTH;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "S_IWOT", 6)) {
    /*                     H     */
#ifdef S_IWOTH
      *iv_return = S_IWOTH;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "S_IXOT", 6)) {
    /*                     H     */
#ifdef S_IXOTH
      *iv_return = S_IXOTH;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'K':
    if (memEQ(name, "CLK_TC", 6)) {
    /*                     K     */
#ifdef CLK_TCK
      *iv_return = CLK_TCK;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "EDEADL", 6)) {
    /*                     K     */
#ifdef EDEADLK
      *iv_return = EDEADLK;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "ENOTBL", 6)) {
    /*                     K     */
#ifdef ENOTBLK
      *iv_return = ENOTBLK;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "F_GETL", 6)) {
    /*                     K     */
#ifdef F_GETLK
      *iv_return = F_GETLK;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "F_RDLC", 6)) {
    /*                     K     */
#ifdef F_RDLCK
      *iv_return = F_RDLCK;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "F_SETL", 6)) {
    /*                     K     */
#ifdef F_SETLK
      *iv_return = F_SETLK;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "F_UNLC", 6)) {
    /*                     K     */
#ifdef F_UNLCK
      *iv_return = F_UNLCK;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "F_WRLC", 6)) {
    /*                     K     */
#ifdef F_WRLCK
      *iv_return = F_WRLCK;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'L':
    if (memEQ(name, "F_GETF", 6)) {
    /*                     L     */
#ifdef F_GETFL
      *iv_return = F_GETFL;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "F_SETF", 6)) {
    /*                     L     */
#ifdef F_SETFL
      *iv_return = F_SETFL;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "SIGKIL", 6)) {
    /*                     L     */
#ifdef SIGKILL
      *iv_return = SIGKILL;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "SIG_DF", 6)) {
    /*                     L     */
#ifdef SIG_DFL
      *iv_return = (IV)SIG_DFL;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'M':
    if (memEQ(name, "SIGALR", 6)) {
    /*                     M     */
#ifdef SIGALRM
      *iv_return = SIGALRM;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "SIGTER", 6)) {
    /*                     M     */
#ifdef SIGTERM
      *iv_return = SIGTERM;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'N':
    if (memEQ(name, "DBL_MI", 6)) {
    /*                     N     */
#ifdef DBL_MIN
      *nv_return = DBL_MIN;
      return PERL_constant_ISNV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "EISCON", 6)) {
    /*                     N     */
#ifdef EISCONN
      *iv_return = EISCONN;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "FLT_MI", 6)) {
    /*                     N     */
#ifdef FLT_MIN
      *nv_return = FLT_MIN;
      return PERL_constant_ISNV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "INT_MI", 6)) {
    /*                     N     */
#ifdef INT_MIN
      *iv_return = INT_MIN;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "SIGTTI", 6)) {
    /*                     N     */
#ifdef SIGTTIN
      *iv_return = SIGTTIN;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "SIG_IG", 6)) {
    /*                     N     */
#ifdef SIG_IGN
      *iv_return = (IV)SIG_IGN;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'O':
    if (memEQ(name, "S_IRWX", 6)) {
    /*                     O     */
#ifdef S_IRWXO
      *iv_return = S_IRWXO;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'P':
    if (memEQ(name, "SIGSTO", 6)) {
    /*                     P     */
#ifdef SIGSTOP
      *iv_return = SIGSTOP;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "SIGTST", 6)) {
    /*                     P     */
#ifdef SIGTSTP
      *iv_return = SIGTSTP;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "S_IRGR", 6)) {
    /*                     P     */
#ifdef S_IRGRP
      *iv_return = S_IRGRP;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "S_IWGR", 6)) {
    /*                     P     */
#ifdef S_IWGRP
      *iv_return = S_IWGRP;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "S_IXGR", 6)) {
    /*                     P     */
#ifdef S_IXGRP
      *iv_return = S_IXGRP;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'R':
    if (memEQ(name, "ENOTDI", 6)) {
    /*                     R     */
#ifdef ENOTDIR
      *iv_return = ENOTDIR;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "SIG_ER", 6)) {
    /*                     R     */
#ifdef SIG_ERR
      *iv_return = (IV)SIG_ERR;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "S_IRUS", 6)) {
    /*                     R     */
#ifdef S_IRUSR
      *iv_return = S_IRUSR;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "S_IWUS", 6)) {
    /*                     R     */
#ifdef S_IWUSR
      *iv_return = S_IWUSR;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "S_IXUS", 6)) {
    /*                     R     */
#ifdef S_IXUSR
      *iv_return = S_IXUSR;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'S':
    if (memEQ(name, "ENOBUF", 6)) {
    /*                     S     */
#ifdef ENOBUFS
      *iv_return = ENOBUFS;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'T':
    if (memEQ(name, "O_CREA", 6)) {
    /*                     T     */
#ifdef O_CREAT
      *iv_return = O_CREAT;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "SIGABR", 6)) {
    /*                     T     */
#ifdef SIGABRT
      *iv_return = SIGABRT;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "SIGCON", 6)) {
    /*                     T     */
#ifdef SIGCONT
      *iv_return = SIGCONT;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "SIGQUI", 6)) {
    /*                     T     */
#ifdef SIGQUIT
      *iv_return = SIGQUIT;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'U':
    if (memEQ(name, "SIGTTO", 6)) {
    /*                     U     */
#ifdef SIGTTOU
      *iv_return = SIGTTOU;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "S_IRWX", 6)) {
    /*                     U     */
#ifdef S_IRWXU
      *iv_return = S_IRWXU;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'V':
    if (memEQ(name, "SIGSEG", 6)) {
    /*                     V     */
#ifdef SIGSEGV
      *iv_return = SIGSEGV;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'W':
    if (memEQ(name, "TCSANO", 6)) {
    /*                     W     */
#ifdef TCSANOW
      *iv_return = TCSANOW;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'X':
    if (memEQ(name, "ARG_MA", 6)) {
    /*                     X     */
#ifdef ARG_MAX
      *iv_return = ARG_MAX;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "DBL_MA", 6)) {
    /*                     X     */
#ifdef DBL_MAX
      *nv_return = DBL_MAX;
      return PERL_constant_ISNV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "FLT_MA", 6)) {
    /*                     X     */
#ifdef FLT_MAX
      *nv_return = FLT_MAX;
      return PERL_constant_ISNV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "INT_MA", 6)) {
    /*                     X     */
#ifdef INT_MAX
      *iv_return = INT_MAX;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "TMP_MA", 6)) {
    /*                     X     */
#ifdef TMP_MAX
      *iv_return = TMP_MAX;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'Y':
    if (memEQ(name, "ETXTBS", 6)) {
    /*                     Y     */
#ifdef ETXTBSY
      *iv_return = ETXTBSY;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  }
  return PERL_constant_NOTFOUND;
}

static int
constant_8 (pTHX_ const char *name, IV *iv_return, NV *nv_return) {
  /* When generated this function returned values for the list of names given
     here.  However, subsequent manual editing may have added or removed some.
     CHAR_BIT CHAR_MAX CHAR_MIN EALREADY EMSGSIZE ENETDOWN ENOTCONN ENOTSOCK
     EPROCLIM ERESTART F_SETLKW HUGE_VAL LC_CTYPE LDBL_DIG LDBL_MAX LDBL_MIN
     LINK_MAX LONG_MAX LONG_MIN L_tmpnam NAME_MAX OPEN_MAX O_APPEND O_NOCTTY
     O_RDONLY O_WRONLY PATH_MAX PIPE_BUF RAND_MAX SEEK_CUR SEEK_END SEEK_SET
     SHRT_MAX SHRT_MIN TCIFLUSH TCOFLUSH UINT_MAX */
  /* Offset 2 gives the best switch position.  */
  switch (name[2]) {
  case 'A':
    if (memEQ(name, "CHAR_BIT", 8)) {
    /*                 ^           */
#ifdef CHAR_BIT
      *iv_return = CHAR_BIT;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "CHAR_MAX", 8)) {
    /*                 ^           */
#ifdef CHAR_MAX
      *iv_return = CHAR_MAX;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "CHAR_MIN", 8)) {
    /*                 ^           */
#ifdef CHAR_MIN
      *iv_return = CHAR_MIN;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "O_APPEND", 8)) {
    /*                 ^           */
#ifdef O_APPEND
      *iv_return = O_APPEND;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'B':
    if (memEQ(name, "LDBL_DIG", 8)) {
    /*                 ^           */
#ifdef LDBL_DIG
      *nv_return = LDBL_DIG;
      return PERL_constant_ISNV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "LDBL_MAX", 8)) {
    /*                 ^           */
#ifdef LDBL_MAX
      *nv_return = LDBL_MAX;
      return PERL_constant_ISNV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "LDBL_MIN", 8)) {
    /*                 ^           */
#ifdef LDBL_MIN
      *nv_return = LDBL_MIN;
      return PERL_constant_ISNV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'E':
    if (memEQ(name, "ENETDOWN", 8)) {
    /*                 ^           */
#ifdef ENETDOWN
      *iv_return = ENETDOWN;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "ERESTART", 8)) {
    /*                 ^           */
#ifdef ERESTART
      *iv_return = ERESTART;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "OPEN_MAX", 8)) {
    /*                 ^           */
#ifdef OPEN_MAX
      *iv_return = OPEN_MAX;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "SEEK_CUR", 8)) {
    /*                 ^           */
#ifdef SEEK_CUR
      *iv_return = SEEK_CUR;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "SEEK_END", 8)) {
    /*                 ^           */
#ifdef SEEK_END
      *iv_return = SEEK_END;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "SEEK_SET", 8)) {
    /*                 ^           */
#ifdef SEEK_SET
      *iv_return = SEEK_SET;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'G':
    if (memEQ(name, "HUGE_VAL", 8)) {
    /*                 ^           */
#if (defined(USE_LONG_DOUBLE) && defined(HUGE_VALL)) || defined(HUGE_VAL)
	/* HUGE_VALL is admittedly non-POSIX but if we are using long doubles
	 * we might as well use long doubles. --jhi */
      *nv_return = 
#if defined(USE_LONG_DOUBLE) && defined(HUGE_VALL)
                   HUGE_VALL
#else
                   HUGE_VAL
#endif
                           ;
      return PERL_constant_ISNV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'I':
    if (memEQ(name, "TCIFLUSH", 8)) {
    /*                 ^           */
#ifdef TCIFLUSH
      *iv_return = TCIFLUSH;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'L':
    if (memEQ(name, "EALREADY", 8)) {
    /*                 ^           */
#ifdef EALREADY
      *iv_return = EALREADY;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'M':
    if (memEQ(name, "NAME_MAX", 8)) {
    /*                 ^           */
#ifdef NAME_MAX
      *iv_return = NAME_MAX;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'N':
    if (memEQ(name, "LINK_MAX", 8)) {
    /*                 ^           */
#ifdef LINK_MAX
      *iv_return = LINK_MAX;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "LONG_MAX", 8)) {
    /*                 ^           */
#ifdef LONG_MAX
      *iv_return = LONG_MAX;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "LONG_MIN", 8)) {
    /*                 ^           */
#ifdef LONG_MIN
      *iv_return = LONG_MIN;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "O_NOCTTY", 8)) {
    /*                 ^           */
#ifdef O_NOCTTY
      *iv_return = O_NOCTTY;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "RAND_MAX", 8)) {
    /*                 ^           */
#ifdef RAND_MAX
      *iv_return = RAND_MAX;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "UINT_MAX", 8)) {
    /*                 ^           */
#ifdef UINT_MAX
      *iv_return = (IV)UINT_MAX;
      return PERL_constant_ISUV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'O':
    if (memEQ(name, "ENOTCONN", 8)) {
    /*                 ^           */
#ifdef ENOTCONN
      *iv_return = ENOTCONN;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "ENOTSOCK", 8)) {
    /*                 ^           */
#ifdef ENOTSOCK
      *iv_return = ENOTSOCK;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "TCOFLUSH", 8)) {
    /*                 ^           */
#ifdef TCOFLUSH
      *iv_return = TCOFLUSH;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'P':
    if (memEQ(name, "PIPE_BUF", 8)) {
    /*                 ^           */
#ifdef PIPE_BUF
      *iv_return = PIPE_BUF;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'R':
    if (memEQ(name, "EPROCLIM", 8)) {
    /*                 ^           */
#ifdef EPROCLIM
      *iv_return = EPROCLIM;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "O_RDONLY", 8)) {
    /*                 ^           */
#ifdef O_RDONLY
      *iv_return = O_RDONLY;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "SHRT_MAX", 8)) {
    /*                 ^           */
#ifdef SHRT_MAX
      *iv_return = SHRT_MAX;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "SHRT_MIN", 8)) {
    /*                 ^           */
#ifdef SHRT_MIN
      *iv_return = SHRT_MIN;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'S':
    if (memEQ(name, "EMSGSIZE", 8)) {
    /*                 ^           */
#ifdef EMSGSIZE
      *iv_return = EMSGSIZE;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "F_SETLKW", 8)) {
    /*                 ^           */
#ifdef F_SETLKW
      *iv_return = F_SETLKW;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'T':
    if (memEQ(name, "PATH_MAX", 8)) {
    /*                 ^           */
#ifdef PATH_MAX
      *iv_return = PATH_MAX;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'W':
    if (memEQ(name, "O_WRONLY", 8)) {
    /*                 ^           */
#ifdef O_WRONLY
      *iv_return = O_WRONLY;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case '_':
    if (memEQ(name, "LC_CTYPE", 8)) {
    /*                 ^           */
#ifdef LC_CTYPE
      *iv_return = LC_CTYPE;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 't':
    if (memEQ(name, "L_tmpnam", 8)) {
    /*                 ^           */
#ifdef L_tmpnam
      *iv_return = L_tmpnam;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  }
  return PERL_constant_NOTFOUND;
}

static int
constant_9 (pTHX_ const char *name, IV *iv_return, NV *nv_return) {
  /* When generated this function returned values for the list of names given
     here.  However, subsequent manual editing may have added or removed some.
     CHILD_MAX EHOSTDOWN ENETRESET ENOTEMPTY ESHUTDOWN ETIMEDOUT FLT_RADIX
     L_ctermid L_cuserid L_tmpname MAX_CANON MAX_INPUT O_ACCMODE SCHAR_MAX
     SCHAR_MIN SIG_BLOCK SSIZE_MAX TCIOFLUSH TCSADRAIN TCSAFLUSH UCHAR_MAX
     ULONG_MAX USHRT_MAX WUNTRACED */
  /* Offset 3 gives the best switch position.  */
  switch (name[3]) {
  case 'A':
    if (memEQ(name, "SCHAR_MAX", 9)) {
    /*                  ^           */
#ifdef SCHAR_MAX
      *iv_return = SCHAR_MAX;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "SCHAR_MIN", 9)) {
    /*                  ^           */
#ifdef SCHAR_MIN
      *iv_return = SCHAR_MIN;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "TCSADRAIN", 9)) {
    /*                  ^           */
#ifdef TCSADRAIN
      *iv_return = TCSADRAIN;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "TCSAFLUSH", 9)) {
    /*                  ^           */
#ifdef TCSAFLUSH
      *iv_return = TCSAFLUSH;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "UCHAR_MAX", 9)) {
    /*                  ^           */
#ifdef UCHAR_MAX
      *iv_return = (IV)UCHAR_MAX;
      return PERL_constant_ISUV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'C':
    if (memEQ(name, "O_ACCMODE", 9)) {
    /*                  ^           */
#ifdef O_ACCMODE
      *iv_return = O_ACCMODE;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'L':
    if (memEQ(name, "CHILD_MAX", 9)) {
    /*                  ^           */
#ifdef CHILD_MAX
      *iv_return = CHILD_MAX;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'M':
    if (memEQ(name, "ETIMEDOUT", 9)) {
    /*                  ^           */
#ifdef ETIMEDOUT
      *iv_return = ETIMEDOUT;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'N':
    if (memEQ(name, "ULONG_MAX", 9)) {
    /*                  ^           */
#ifdef ULONG_MAX
      *iv_return = (IV)ULONG_MAX;
      return PERL_constant_ISUV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'O':
    if (memEQ(name, "TCIOFLUSH", 9)) {
    /*                  ^           */
#ifdef TCIOFLUSH
      *iv_return = TCIOFLUSH;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'R':
    if (memEQ(name, "USHRT_MAX", 9)) {
    /*                  ^           */
#ifdef USHRT_MAX
      *iv_return = (IV)USHRT_MAX;
      return PERL_constant_ISUV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'S':
    if (memEQ(name, "EHOSTDOWN", 9)) {
    /*                  ^           */
#ifdef EHOSTDOWN
      *iv_return = EHOSTDOWN;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'T':
    if (memEQ(name, "ENETRESET", 9)) {
    /*                  ^           */
#ifdef ENETRESET
      *iv_return = ENETRESET;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "ENOTEMPTY", 9)) {
    /*                  ^           */
#ifdef ENOTEMPTY
      *iv_return = ENOTEMPTY;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "WUNTRACED", 9)) {
    /*                  ^           */
#ifdef WUNTRACED
      *iv_return = WUNTRACED;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'U':
    if (memEQ(name, "ESHUTDOWN", 9)) {
    /*                  ^           */
#ifdef ESHUTDOWN
      *iv_return = ESHUTDOWN;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'Z':
    if (memEQ(name, "SSIZE_MAX", 9)) {
    /*                  ^           */
#ifdef SSIZE_MAX
      *iv_return = SSIZE_MAX;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case '_':
    if (memEQ(name, "FLT_RADIX", 9)) {
    /*                  ^           */
#ifdef FLT_RADIX
      *nv_return = FLT_RADIX;
      return PERL_constant_ISNV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "MAX_CANON", 9)) {
    /*                  ^           */
#ifdef MAX_CANON
      *iv_return = MAX_CANON;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "MAX_INPUT", 9)) {
    /*                  ^           */
#ifdef MAX_INPUT
      *iv_return = MAX_INPUT;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "SIG_BLOCK", 9)) {
    /*                  ^           */
#ifdef SIG_BLOCK
      *iv_return = SIG_BLOCK;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'm':
    if (memEQ(name, "L_tmpname", 9)) {
    /*                  ^           */
#ifdef L_tmpname
      *iv_return = L_tmpnam;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 't':
    if (memEQ(name, "L_ctermid", 9)) {
    /*                  ^           */
#ifdef L_ctermid
      *iv_return = L_ctermid;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'u':
    if (memEQ(name, "L_cuserid", 9)) {
    /*                  ^           */
#ifdef L_cuserid
      *iv_return = L_cuserid;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  }
  return PERL_constant_NOTFOUND;
}

static int
constant_10 (pTHX_ const char *name, IV *iv_return, NV *nv_return) {
  /* When generated this function returned values for the list of names given
     here.  However, subsequent manual editing may have added or removed some.
     EADDRINUSE ECONNRESET EOPNOTSUPP EPROTOTYPE FD_CLOEXEC FLT_ROUNDS
     LC_COLLATE LC_NUMERIC MB_CUR_MAX MB_LEN_MAX O_NONBLOCK SA_NODEFER
     SA_ONSTACK SA_RESTART SA_SIGINFO STREAM_MAX TZNAME_MAX */
  /* Offset 5 gives the best switch position.  */
  switch (name[5]) {
  case 'B':
    if (memEQ(name, "O_NONBLOCK", 10)) {
    /*                    ^           */
#ifdef O_NONBLOCK
      *iv_return = O_NONBLOCK;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'D':
    if (memEQ(name, "SA_NODEFER", 10)) {
    /*                    ^           */
#ifdef SA_NODEFER
      *iv_return = (IV)SA_NODEFER;
      return PERL_constant_ISUV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'E':
    if (memEQ(name, "TZNAME_MAX", 10)) {
    /*                    ^           */
#ifdef TZNAME_MAX
      *iv_return = TZNAME_MAX;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'G':
    if (memEQ(name, "SA_SIGINFO", 10)) {
    /*                    ^           */
#ifdef SA_SIGINFO
      *iv_return = (IV)SA_SIGINFO;
      return PERL_constant_ISUV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'I':
    if (memEQ(name, "EADDRINUSE", 10)) {
    /*                    ^           */
#ifdef EADDRINUSE
      *iv_return = EADDRINUSE;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'L':
    if (memEQ(name, "LC_COLLATE", 10)) {
    /*                    ^           */
#ifdef LC_COLLATE
      *iv_return = LC_COLLATE;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'M':
    if (memEQ(name, "LC_NUMERIC", 10)) {
    /*                    ^           */
#ifdef LC_NUMERIC
      *iv_return = LC_NUMERIC;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "STREAM_MAX", 10)) {
    /*                    ^           */
#ifdef STREAM_MAX
      *iv_return = STREAM_MAX;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'N':
    if (memEQ(name, "MB_LEN_MAX", 10)) {
    /*                    ^           */
#ifdef MB_LEN_MAX
      *iv_return = MB_LEN_MAX;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'O':
    if (memEQ(name, "EPROTOTYPE", 10)) {
    /*                    ^           */
#ifdef EPROTOTYPE
      *iv_return = EPROTOTYPE;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "FD_CLOEXEC", 10)) {
    /*                    ^           */
#ifdef FD_CLOEXEC
      *iv_return = FD_CLOEXEC;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "FLT_ROUNDS", 10)) {
    /*                    ^           */
#ifdef FLT_ROUNDS
      *nv_return = FLT_ROUNDS;
      return PERL_constant_ISNV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'R':
    if (memEQ(name, "ECONNRESET", 10)) {
    /*                    ^           */
#ifdef ECONNRESET
      *iv_return = ECONNRESET;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "MB_CUR_MAX", 10)) {
    /*                    ^           */
#ifdef MB_CUR_MAX
      *iv_return = MB_CUR_MAX;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'S':
    if (memEQ(name, "SA_ONSTACK", 10)) {
    /*                    ^           */
#ifdef SA_ONSTACK
      *iv_return = (IV)SA_ONSTACK;
      return PERL_constant_ISUV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "SA_RESTART", 10)) {
    /*                    ^           */
#ifdef SA_RESTART
      *iv_return = (IV)SA_RESTART;
      return PERL_constant_ISUV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'T':
    if (memEQ(name, "EOPNOTSUPP", 10)) {
    /*                    ^           */
#ifdef EOPNOTSUPP
      *iv_return = EOPNOTSUPP;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  }
  return PERL_constant_NOTFOUND;
}

static int
constant_11 (pTHX_ const char *name, IV *iv_return, NV *nv_return) {
  /* When generated this function returned values for the list of names given
     here.  However, subsequent manual editing may have added or removed some.
     DBL_EPSILON DBL_MAX_EXP DBL_MIN_EXP EINPROGRESS ENETUNREACH ENOPROTOOPT
     EWOULDBLOCK FLT_EPSILON FLT_MAX_EXP FLT_MIN_EXP LC_MESSAGES LC_MONETARY
     NGROUPS_MAX SIG_SETMASK SIG_UNBLOCK _SC_ARG_MAX _SC_CLK_TCK _SC_VERSION */
  /* Offset 5 gives the best switch position.  */
  switch (name[5]) {
  case 'A':
    if (memEQ(name, "DBL_MAX_EXP", 11)) {
    /*                    ^            */
#ifdef DBL_MAX_EXP
      *nv_return = DBL_MAX_EXP;
      return PERL_constant_ISNV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "FLT_MAX_EXP", 11)) {
    /*                    ^            */
#ifdef FLT_MAX_EXP
      *nv_return = FLT_MAX_EXP;
      return PERL_constant_ISNV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'D':
    if (memEQ(name, "EWOULDBLOCK", 11)) {
    /*                    ^            */
#ifdef EWOULDBLOCK
      *iv_return = EWOULDBLOCK;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'E':
    if (memEQ(name, "SIG_SETMASK", 11)) {
    /*                    ^            */
#ifdef SIG_SETMASK
      *iv_return = SIG_SETMASK;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "_SC_VERSION", 11)) {
    /*                    ^            */
#ifdef _SC_VERSION
      *iv_return = _SC_VERSION;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'I':
    if (memEQ(name, "DBL_MIN_EXP", 11)) {
    /*                    ^            */
#ifdef DBL_MIN_EXP
      *nv_return = DBL_MIN_EXP;
      return PERL_constant_ISNV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "FLT_MIN_EXP", 11)) {
    /*                    ^            */
#ifdef FLT_MIN_EXP
      *nv_return = FLT_MIN_EXP;
      return PERL_constant_ISNV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'L':
    if (memEQ(name, "_SC_CLK_TCK", 11)) {
    /*                    ^            */
#ifdef _SC_CLK_TCK
      *iv_return = _SC_CLK_TCK;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'N':
    if (memEQ(name, "ENETUNREACH", 11)) {
    /*                    ^            */
#ifdef ENETUNREACH
      *iv_return = ENETUNREACH;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "LC_MONETARY", 11)) {
    /*                    ^            */
#ifdef LC_MONETARY
      *iv_return = LC_MONETARY;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "SIG_UNBLOCK", 11)) {
    /*                    ^            */
#ifdef SIG_UNBLOCK
      *iv_return = SIG_UNBLOCK;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'O':
    if (memEQ(name, "EINPROGRESS", 11)) {
    /*                    ^            */
#ifdef EINPROGRESS
      *iv_return = EINPROGRESS;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "ENOPROTOOPT", 11)) {
    /*                    ^            */
#ifdef ENOPROTOOPT
      *iv_return = ENOPROTOOPT;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'P':
    if (memEQ(name, "DBL_EPSILON", 11)) {
    /*                    ^            */
#ifdef DBL_EPSILON
      *nv_return = DBL_EPSILON;
      return PERL_constant_ISNV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "FLT_EPSILON", 11)) {
    /*                    ^            */
#ifdef FLT_EPSILON
      *nv_return = FLT_EPSILON;
      return PERL_constant_ISNV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "NGROUPS_MAX", 11)) {
    /*                    ^            */
#ifdef NGROUPS_MAX
      *iv_return = NGROUPS_MAX;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'R':
    if (memEQ(name, "_SC_ARG_MAX", 11)) {
    /*                    ^            */
#ifdef _SC_ARG_MAX
      *iv_return = _SC_ARG_MAX;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'S':
    if (memEQ(name, "LC_MESSAGES", 11)) {
    /*                    ^            */
#ifdef LC_MESSAGES
      *iv_return = LC_MESSAGES;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  }
  return PERL_constant_NOTFOUND;
}

static int
constant_12 (pTHX_ const char *name, IV *iv_return, NV *nv_return) {
  /* When generated this function returned values for the list of names given
     here.  However, subsequent manual editing may have added or removed some.
     DBL_MANT_DIG EAFNOSUPPORT ECONNABORTED ECONNREFUSED EDESTADDRREQ
     EHOSTUNREACH ENAMETOOLONG EPFNOSUPPORT ETOOMANYREFS EXIT_FAILURE
     EXIT_SUCCESS FILENAME_MAX FLT_MANT_DIG LDBL_EPSILON LDBL_MAX_EXP
     LDBL_MIN_EXP SA_NOCLDSTOP SA_NOCLDWAIT SA_RESETHAND STDIN_FILENO
     _PC_LINK_MAX _PC_NAME_MAX _PC_NO_TRUNC _PC_PATH_MAX _PC_PIPE_BUF
     _PC_VDISABLE _SC_OPEN_MAX _SC_PAGESIZE */
  /* Offset 7 gives the best switch position.  */
  switch (name[7]) {
  case 'C':
    if (memEQ(name, "EXIT_SUCCESS", 12)) {
    /*                      ^           */
#ifdef EXIT_SUCCESS
      *iv_return = EXIT_SUCCESS;
      return PERL_constant_ISIV;
#else
      *iv_return = 0;
      return PERL_constant_ISIV;
#endif
    }
    break;
  case 'D':
    if (memEQ(name, "EDESTADDRREQ", 12)) {
    /*                      ^           */
#ifdef EDESTADDRREQ
      *iv_return = EDESTADDRREQ;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "SA_NOCLDSTOP", 12)) {
    /*                      ^           */
#ifdef SA_NOCLDSTOP
      *iv_return = (IV)SA_NOCLDSTOP;
      return PERL_constant_ISUV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "SA_NOCLDWAIT", 12)) {
    /*                      ^           */
#ifdef SA_NOCLDWAIT
      *iv_return = (IV)SA_NOCLDWAIT;
      return PERL_constant_ISUV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'E':
    if (memEQ(name, "FILENAME_MAX", 12)) {
    /*                      ^           */
#ifdef FILENAME_MAX
      *iv_return = FILENAME_MAX;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "_PC_NAME_MAX", 12)) {
    /*                      ^           */
#ifdef _PC_NAME_MAX
      *iv_return = _PC_NAME_MAX;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "_PC_PIPE_BUF", 12)) {
    /*                      ^           */
#ifdef _PC_PIPE_BUF
      *iv_return = _PC_PIPE_BUF;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "_SC_PAGESIZE", 12)) {
    /*                      ^           */
#ifdef _SC_PAGESIZE
      *iv_return = _SC_PAGESIZE;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'F':
    if (memEQ(name, "ECONNREFUSED", 12)) {
    /*                      ^           */
#ifdef ECONNREFUSED
      *iv_return = ECONNREFUSED;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'H':
    if (memEQ(name, "_PC_PATH_MAX", 12)) {
    /*                      ^           */
#ifdef _PC_PATH_MAX
      *iv_return = _PC_PATH_MAX;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'I':
    if (memEQ(name, "EXIT_FAILURE", 12)) {
    /*                      ^           */
#ifdef EXIT_FAILURE
      *iv_return = EXIT_FAILURE;
      return PERL_constant_ISIV;
#else
      *iv_return = 1;
      return PERL_constant_ISIV;
#endif
    }
    if (memEQ(name, "STDIN_FILENO", 12)) {
    /*                      ^           */
#ifdef STDIN_FILENO
      *iv_return = STDIN_FILENO;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'K':
    if (memEQ(name, "_PC_LINK_MAX", 12)) {
    /*                      ^           */
#ifdef _PC_LINK_MAX
      *iv_return = _PC_LINK_MAX;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'N':
    if (memEQ(name, "LDBL_MIN_EXP", 12)) {
    /*                      ^           */
#ifdef LDBL_MIN_EXP
      *nv_return = LDBL_MIN_EXP;
      return PERL_constant_ISNV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "_SC_OPEN_MAX", 12)) {
    /*                      ^           */
#ifdef _SC_OPEN_MAX
      *iv_return = _SC_OPEN_MAX;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'O':
    if (memEQ(name, "ECONNABORTED", 12)) {
    /*                      ^           */
#ifdef ECONNABORTED
      *iv_return = ECONNABORTED;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "ENAMETOOLONG", 12)) {
    /*                      ^           */
#ifdef ENAMETOOLONG
      *iv_return = ENAMETOOLONG;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'P':
    if (memEQ(name, "EAFNOSUPPORT", 12)) {
    /*                      ^           */
#ifdef EAFNOSUPPORT
      *iv_return = EAFNOSUPPORT;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "EPFNOSUPPORT", 12)) {
    /*                      ^           */
#ifdef EPFNOSUPPORT
      *iv_return = EPFNOSUPPORT;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'R':
    if (memEQ(name, "EHOSTUNREACH", 12)) {
    /*                      ^           */
#ifdef EHOSTUNREACH
      *iv_return = EHOSTUNREACH;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'S':
    if (memEQ(name, "LDBL_EPSILON", 12)) {
    /*                      ^           */
#ifdef LDBL_EPSILON
      *nv_return = LDBL_EPSILON;
      return PERL_constant_ISNV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "_PC_VDISABLE", 12)) {
    /*                      ^           */
#ifdef _PC_VDISABLE
      *iv_return = _PC_VDISABLE;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'T':
    if (memEQ(name, "DBL_MANT_DIG", 12)) {
    /*                      ^           */
#ifdef DBL_MANT_DIG
      *nv_return = DBL_MANT_DIG;
      return PERL_constant_ISNV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "FLT_MANT_DIG", 12)) {
    /*                      ^           */
#ifdef FLT_MANT_DIG
      *nv_return = FLT_MANT_DIG;
      return PERL_constant_ISNV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "SA_RESETHAND", 12)) {
    /*                      ^           */
#ifdef SA_RESETHAND
      *iv_return = (IV)SA_RESETHAND;
      return PERL_constant_ISUV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "_PC_NO_TRUNC", 12)) {
    /*                      ^           */
#ifdef _PC_NO_TRUNC
      *iv_return = _PC_NO_TRUNC;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'X':
    if (memEQ(name, "LDBL_MAX_EXP", 12)) {
    /*                      ^           */
#ifdef LDBL_MAX_EXP
      *nv_return = LDBL_MAX_EXP;
      return PERL_constant_ISNV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'Y':
    if (memEQ(name, "ETOOMANYREFS", 12)) {
    /*                      ^           */
#ifdef ETOOMANYREFS
      *iv_return = ETOOMANYREFS;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  }
  return PERL_constant_NOTFOUND;
}

static int
constant_13 (pTHX_ const char *name, IV *iv_return, NV *nv_return) {
  /* When generated this function returned values for the list of names given
     here.  However, subsequent manual editing may have added or removed some.
     EADDRNOTAVAIL LDBL_MANT_DIG STDERR_FILENO STDOUT_FILENO _PC_MAX_CANON
     _PC_MAX_INPUT _SC_CHILD_MAX _SC_SAVED_IDS */
  /* Offset 10 gives the best switch position.  */
  switch (name[10]) {
  case 'A':
    if (memEQ(name, "EADDRNOTAVAIL", 13)) {
    /*                         ^         */
#ifdef EADDRNOTAVAIL
      *iv_return = EADDRNOTAVAIL;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'D':
    if (memEQ(name, "LDBL_MANT_DIG", 13)) {
    /*                         ^         */
#ifdef LDBL_MANT_DIG
      *nv_return = LDBL_MANT_DIG;
      return PERL_constant_ISNV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'E':
    if (memEQ(name, "STDERR_FILENO", 13)) {
    /*                         ^         */
#ifdef STDERR_FILENO
      *iv_return = STDERR_FILENO;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "STDOUT_FILENO", 13)) {
    /*                         ^         */
#ifdef STDOUT_FILENO
      *iv_return = STDOUT_FILENO;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'I':
    if (memEQ(name, "_SC_SAVED_IDS", 13)) {
    /*                         ^         */
#ifdef _SC_SAVED_IDS
      *iv_return = _SC_SAVED_IDS;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'M':
    if (memEQ(name, "_SC_CHILD_MAX", 13)) {
    /*                         ^         */
#ifdef _SC_CHILD_MAX
      *iv_return = _SC_CHILD_MAX;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'N':
    if (memEQ(name, "_PC_MAX_CANON", 13)) {
    /*                         ^         */
#ifdef _PC_MAX_CANON
      *iv_return = _PC_MAX_CANON;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'P':
    if (memEQ(name, "_PC_MAX_INPUT", 13)) {
    /*                         ^         */
#ifdef _PC_MAX_INPUT
      *iv_return = _PC_MAX_INPUT;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  }
  return PERL_constant_NOTFOUND;
}

static int
constant_14 (pTHX_ const char *name, IV *iv_return, NV *nv_return) {
  /* When generated this function returned values for the list of names given
     here.  However, subsequent manual editing may have added or removed some.
     CLOCKS_PER_SEC DBL_MAX_10_EXP DBL_MIN_10_EXP FLT_MAX_10_EXP FLT_MIN_10_EXP
     _POSIX_ARG_MAX _POSIX_VERSION _SC_STREAM_MAX _SC_TZNAME_MAX */
  /* Offset 5 gives the best switch position.  */
  switch (name[5]) {
  case 'A':
    if (memEQ(name, "DBL_MAX_10_EXP", 14)) {
    /*                    ^               */
#ifdef DBL_MAX_10_EXP
      *nv_return = DBL_MAX_10_EXP;
      return PERL_constant_ISNV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "FLT_MAX_10_EXP", 14)) {
    /*                    ^               */
#ifdef FLT_MAX_10_EXP
      *nv_return = FLT_MAX_10_EXP;
      return PERL_constant_ISNV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'I':
    if (memEQ(name, "DBL_MIN_10_EXP", 14)) {
    /*                    ^               */
#ifdef DBL_MIN_10_EXP
      *nv_return = DBL_MIN_10_EXP;
      return PERL_constant_ISNV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "FLT_MIN_10_EXP", 14)) {
    /*                    ^               */
#ifdef FLT_MIN_10_EXP
      *nv_return = FLT_MIN_10_EXP;
      return PERL_constant_ISNV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'S':
    if (memEQ(name, "CLOCKS_PER_SEC", 14)) {
    /*                    ^               */
#ifdef CLOCKS_PER_SEC
      *iv_return = CLOCKS_PER_SEC;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'T':
    if (memEQ(name, "_SC_STREAM_MAX", 14)) {
    /*                    ^               */
#ifdef _SC_STREAM_MAX
      *iv_return = _SC_STREAM_MAX;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'X':
    if (memEQ(name, "_POSIX_ARG_MAX", 14)) {
    /*                    ^               */
#ifdef _POSIX_ARG_MAX
      *iv_return = _POSIX_ARG_MAX;
      return PERL_constant_ISIV;
#else
      *iv_return = 0;
      return PERL_constant_ISIV;
#endif
    }
    if (memEQ(name, "_POSIX_VERSION", 14)) {
    /*                    ^               */
#ifdef _POSIX_VERSION
      *iv_return = _POSIX_VERSION;
      return PERL_constant_ISIV;
#else
      *iv_return = 0;
      return PERL_constant_ISIV;
#endif
    }
    break;
  case 'Z':
    if (memEQ(name, "_SC_TZNAME_MAX", 14)) {
    /*                    ^               */
#ifdef _SC_TZNAME_MAX
      *iv_return = _SC_TZNAME_MAX;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  }
  return PERL_constant_NOTFOUND;
}

static int
constant_15 (pTHX_ const char *name, IV *iv_return, NV *nv_return) {
  /* When generated this function returned values for the list of names given
     here.  However, subsequent manual editing may have added or removed some.
     EPROTONOSUPPORT ESOCKTNOSUPPORT LDBL_MAX_10_EXP LDBL_MIN_10_EXP
     _POSIX_LINK_MAX _POSIX_NAME_MAX _POSIX_NO_TRUNC _POSIX_OPEN_MAX
     _POSIX_PATH_MAX _POSIX_PIPE_BUF _POSIX_VDISABLE _SC_JOB_CONTROL
     _SC_NGROUPS_MAX */
  /* Offset 9 gives the best switch position.  */
  switch (name[9]) {
  case '1':
    if (memEQ(name, "LDBL_MAX_10_EXP", 15)) {
    /*                        ^            */
#ifdef LDBL_MAX_10_EXP
      *nv_return = LDBL_MAX_10_EXP;
      return PERL_constant_ISNV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "LDBL_MIN_10_EXP", 15)) {
    /*                        ^            */
#ifdef LDBL_MIN_10_EXP
      *nv_return = LDBL_MIN_10_EXP;
      return PERL_constant_ISNV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'E':
    if (memEQ(name, "_POSIX_OPEN_MAX", 15)) {
    /*                        ^            */
#ifdef _POSIX_OPEN_MAX
      *iv_return = _POSIX_OPEN_MAX;
      return PERL_constant_ISIV;
#else
      *iv_return = 0;
      return PERL_constant_ISIV;
#endif
    }
    break;
  case 'I':
    if (memEQ(name, "_POSIX_VDISABLE", 15)) {
    /*                        ^            */
#ifdef _POSIX_VDISABLE
      *iv_return = _POSIX_VDISABLE;
      return PERL_constant_ISIV;
#else
      *iv_return = 0;
      return PERL_constant_ISIV;
#endif
    }
    break;
  case 'M':
    if (memEQ(name, "_POSIX_NAME_MAX", 15)) {
    /*                        ^            */
#ifdef _POSIX_NAME_MAX
      *iv_return = _POSIX_NAME_MAX;
      return PERL_constant_ISIV;
#else
      *iv_return = 0;
      return PERL_constant_ISIV;
#endif
    }
    break;
  case 'N':
    if (memEQ(name, "_POSIX_LINK_MAX", 15)) {
    /*                        ^            */
#ifdef _POSIX_LINK_MAX
      *iv_return = _POSIX_LINK_MAX;
      return PERL_constant_ISIV;
#else
      *iv_return = 0;
      return PERL_constant_ISIV;
#endif
    }
    break;
  case 'O':
    if (memEQ(name, "_SC_JOB_CONTROL", 15)) {
    /*                        ^            */
#ifdef _SC_JOB_CONTROL
      *iv_return = _SC_JOB_CONTROL;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'P':
    if (memEQ(name, "_POSIX_PIPE_BUF", 15)) {
    /*                        ^            */
#ifdef _POSIX_PIPE_BUF
      *iv_return = _POSIX_PIPE_BUF;
      return PERL_constant_ISIV;
#else
      *iv_return = 0;
      return PERL_constant_ISIV;
#endif
    }
    if (memEQ(name, "_SC_NGROUPS_MAX", 15)) {
    /*                        ^            */
#ifdef _SC_NGROUPS_MAX
      *iv_return = _SC_NGROUPS_MAX;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 'T':
    if (memEQ(name, "_POSIX_PATH_MAX", 15)) {
    /*                        ^            */
#ifdef _POSIX_PATH_MAX
      *iv_return = _POSIX_PATH_MAX;
      return PERL_constant_ISIV;
#else
      *iv_return = 0;
      return PERL_constant_ISIV;
#endif
    }
    break;
  case 'U':
    if (memEQ(name, "EPROTONOSUPPORT", 15)) {
    /*                        ^            */
#ifdef EPROTONOSUPPORT
      *iv_return = EPROTONOSUPPORT;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    if (memEQ(name, "ESOCKTNOSUPPORT", 15)) {
    /*                        ^            */
#ifdef ESOCKTNOSUPPORT
      *iv_return = ESOCKTNOSUPPORT;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case '_':
    if (memEQ(name, "_POSIX_NO_TRUNC", 15)) {
    /*                        ^            */
#ifdef _POSIX_NO_TRUNC
      *iv_return = _POSIX_NO_TRUNC;
      return PERL_constant_ISIV;
#else
      *iv_return = 0;
      return PERL_constant_ISIV;
#endif
    }
    break;
  }
  return PERL_constant_NOTFOUND;
}

static int
constant_16 (pTHX_ const char *name, IV *iv_return) {
  /* When generated this function returned values for the list of names given
     here.  However, subsequent manual editing may have added or removed some.
     _POSIX_CHILD_MAX _POSIX_MAX_CANON _POSIX_MAX_INPUT _POSIX_SAVED_IDS
     _POSIX_SSIZE_MAX */
  /* Offset 11 gives the best switch position.  */
  switch (name[11]) {
  case 'C':
    if (memEQ(name, "_POSIX_MAX_CANON", 16)) {
    /*                          ^           */
#ifdef _POSIX_MAX_CANON
      *iv_return = _POSIX_MAX_CANON;
      return PERL_constant_ISIV;
#else
      *iv_return = 0;
      return PERL_constant_ISIV;
#endif
    }
    break;
  case 'D':
    if (memEQ(name, "_POSIX_CHILD_MAX", 16)) {
    /*                          ^           */
#ifdef _POSIX_CHILD_MAX
      *iv_return = _POSIX_CHILD_MAX;
      return PERL_constant_ISIV;
#else
      *iv_return = 0;
      return PERL_constant_ISIV;
#endif
    }
    if (memEQ(name, "_POSIX_SAVED_IDS", 16)) {
    /*                          ^           */
#ifdef _POSIX_SAVED_IDS
      return PERL_constant_ISYES;
#else
      *iv_return = 0;
      return PERL_constant_ISIV;
#endif
    }
    break;
  case 'E':
    if (memEQ(name, "_POSIX_SSIZE_MAX", 16)) {
    /*                          ^           */
#ifdef _POSIX_SSIZE_MAX
      *iv_return = _POSIX_SSIZE_MAX;
      return PERL_constant_ISIV;
#else
      *iv_return = 0;
      return PERL_constant_ISIV;
#endif
    }
    break;
  case 'I':
    if (memEQ(name, "_POSIX_MAX_INPUT", 16)) {
    /*                          ^           */
#ifdef _POSIX_MAX_INPUT
      *iv_return = _POSIX_MAX_INPUT;
      return PERL_constant_ISIV;
#else
      *iv_return = 0;
      return PERL_constant_ISIV;
#endif
    }
    break;
  }
  return PERL_constant_NOTFOUND;
}

static int
constant (pTHX_ const char *name, STRLEN len, IV *iv_return, NV *nv_return) {
  /* Initially switch on the length of the name.  */
  /* When generated this function returned values for the list of names given
     in this section of perl code.  Rather than manually editing these functions
     to add or remove constants, which would result in this comment and section
     of code becoming inaccurate, we recommend that you edit this section of
     code, and use it to regenerate a new set of constant functions which you
     then use to replace the originals.

     Regenerate these constant functions by feeding this entire source file to
     perl -x

#!/var/services/homes/spparmar/spec2006/tools/src/perl-5.8.7/miniperl -w
use ExtUtils::Constant qw (constant_types C_constant XS_constant);

my $types = {map {($_, 1)} qw(IV NV UV YES)};
my @names = (qw(ARG_MAX B0 B110 B1200 B134 B150 B1800 B19200 B200 B2400 B300
	       B38400 B4800 B50 B600 B75 B9600 BRKINT BUFSIZ CHAR_BIT CHAR_MAX
	       CHAR_MIN CHILD_MAX CLK_TCK CLOCAL CLOCKS_PER_SEC CREAD CS5 CS6
	       CS7 CS8 CSIZE CSTOPB E2BIG EACCES EADDRINUSE EADDRNOTAVAIL
	       EAFNOSUPPORT EAGAIN EALREADY EBADF EBUSY ECHILD ECHO ECHOE ECHOK
	       ECHONL ECONNABORTED ECONNREFUSED ECONNRESET EDEADLK EDESTADDRREQ
	       EDOM EDQUOT EEXIST EFAULT EFBIG EHOSTDOWN EHOSTUNREACH
	       EINPROGRESS EINTR EINVAL EIO EISCONN EISDIR ELOOP EMFILE EMLINK
	       EMSGSIZE ENAMETOOLONG ENETDOWN ENETRESET ENETUNREACH ENFILE
	       ENOBUFS ENODEV ENOENT ENOEXEC ENOLCK ENOMEM ENOPROTOOPT ENOSPC
	       ENOSYS ENOTBLK ENOTCONN ENOTDIR ENOTEMPTY ENOTSOCK ENOTTY ENXIO
	       EOF EOPNOTSUPP EPERM EPFNOSUPPORT EPIPE EPROCLIM EPROTONOSUPPORT
	       EPROTOTYPE ERANGE EREMOTE ERESTART EROFS ESHUTDOWN
	       ESOCKTNOSUPPORT ESPIPE ESRCH ESTALE ETIMEDOUT ETOOMANYREFS
	       ETXTBSY EUSERS EWOULDBLOCK EXDEV FD_CLOEXEC FILENAME_MAX F_DUPFD
	       F_GETFD F_GETFL F_GETLK F_OK F_RDLCK F_SETFD F_SETFL F_SETLK
	       F_SETLKW F_UNLCK F_WRLCK HUPCL ICANON ICRNL IEXTEN IGNBRK IGNCR
	       IGNPAR INLCR INPCK INT_MAX INT_MIN ISIG ISTRIP IXOFF IXON LC_ALL
	       LC_COLLATE LC_CTYPE LC_MESSAGES LC_MONETARY LC_NUMERIC LC_TIME
	       LINK_MAX LONG_MAX LONG_MIN L_ctermid L_cuserid L_tmpnam
	       MAX_CANON MAX_INPUT MB_CUR_MAX MB_LEN_MAX NAME_MAX NCCS
	       NGROUPS_MAX NOFLSH OPEN_MAX OPOST O_ACCMODE O_APPEND O_CREAT
	       O_EXCL O_NOCTTY O_NONBLOCK O_RDONLY O_RDWR O_TRUNC O_WRONLY
	       PARENB PARMRK PARODD PATH_MAX PIPE_BUF RAND_MAX R_OK SCHAR_MAX
	       SCHAR_MIN SEEK_CUR SEEK_END SEEK_SET SHRT_MAX SHRT_MIN SIGABRT
	       SIGALRM SIGCHLD SIGCONT SIGFPE SIGHUP SIGILL SIGINT SIGKILL
	       SIGPIPE SIGQUIT SIGSEGV SIGSTOP SIGTERM SIGTSTP SIGTTIN SIGTTOU
	       SIGUSR1 SIGUSR2 SIG_BLOCK SIG_SETMASK SIG_UNBLOCK SSIZE_MAX
	       STDERR_FILENO STDIN_FILENO STDOUT_FILENO STREAM_MAX S_IRGRP
	       S_IROTH S_IRUSR S_IRWXG S_IRWXO S_IRWXU S_ISGID S_ISUID S_IWGRP
	       S_IWOTH S_IWUSR S_IXGRP S_IXOTH S_IXUSR TCIFLUSH TCIOFF
	       TCIOFLUSH TCION TCOFLUSH TCOOFF TCOON TCSADRAIN TCSAFLUSH
	       TCSANOW TMP_MAX TOSTOP TZNAME_MAX VEOF VEOL VERASE VINTR VKILL
	       VMIN VQUIT VSTART VSTOP VSUSP VTIME WNOHANG WUNTRACED W_OK X_OK
	       _PC_CHOWN_RESTRICTED _PC_LINK_MAX _PC_MAX_CANON _PC_MAX_INPUT
	       _PC_NAME_MAX _PC_NO_TRUNC _PC_PATH_MAX _PC_PIPE_BUF _PC_VDISABLE
	       _SC_ARG_MAX _SC_CHILD_MAX _SC_CLK_TCK _SC_JOB_CONTROL
	       _SC_NGROUPS_MAX _SC_OPEN_MAX _SC_PAGESIZE _SC_SAVED_IDS
	       _SC_STREAM_MAX _SC_TZNAME_MAX _SC_VERSION),
            {name=>"DBL_DIG", type=>"NV"},
            {name=>"DBL_EPSILON", type=>"NV"},
            {name=>"DBL_MANT_DIG", type=>"NV"},
            {name=>"DBL_MAX", type=>"NV"},
            {name=>"DBL_MAX_10_EXP", type=>"NV"},
            {name=>"DBL_MAX_EXP", type=>"NV"},
            {name=>"DBL_MIN", type=>"NV"},
            {name=>"DBL_MIN_10_EXP", type=>"NV"},
            {name=>"DBL_MIN_EXP", type=>"NV"},
            {name=>"EXIT_FAILURE", type=>"IV", default=>["IV", "1"]},
            {name=>"EXIT_SUCCESS", type=>"IV", default=>["IV", "0"]},
            {name=>"FLT_DIG", type=>"NV"},
            {name=>"FLT_EPSILON", type=>"NV"},
            {name=>"FLT_MANT_DIG", type=>"NV"},
            {name=>"FLT_MAX", type=>"NV"},
            {name=>"FLT_MAX_10_EXP", type=>"NV"},
            {name=>"FLT_MAX_EXP", type=>"NV"},
            {name=>"FLT_MIN", type=>"NV"},
            {name=>"FLT_MIN_10_EXP", type=>"NV"},
            {name=>"FLT_MIN_EXP", type=>"NV"},
            {name=>"FLT_RADIX", type=>"NV"},
            {name=>"FLT_ROUNDS", type=>"NV"},
            {name=>"HUGE_VAL", type=>"NV", macro=>["#if (defined(USE_LONG_DOUBLE) && defined(HUGE_VALL)) || defined(HUGE_VAL)\n\t/" . "* HUGE_VALL is admittedly non-POSIX but if we are using long doubles\n\t * we might as well use long doubles. --jhi *" . "/\n", "#endif\n"], value=>"\n#if defined(USE_LONG_DOUBLE) && defined(HUGE_VALL)\n                   HUGE_VALL\n#else\n                   HUGE_VAL\n#endif\n                           "},
            {name=>"LDBL_DIG", type=>"NV"},
            {name=>"LDBL_EPSILON", type=>"NV"},
            {name=>"LDBL_MANT_DIG", type=>"NV"},
            {name=>"LDBL_MAX", type=>"NV"},
            {name=>"LDBL_MAX_10_EXP", type=>"NV"},
            {name=>"LDBL_MAX_EXP", type=>"NV"},
            {name=>"LDBL_MIN", type=>"NV"},
            {name=>"LDBL_MIN_10_EXP", type=>"NV"},
            {name=>"LDBL_MIN_EXP", type=>"NV"},
            {name=>"L_tmpname", type=>"IV", value=>"L_tmpnam"},
            {name=>"NULL", type=>"IV", value=>"0"},
            {name=>"SA_NOCLDSTOP", type=>"UV"},
            {name=>"SA_NOCLDWAIT", type=>"UV"},
            {name=>"SA_NODEFER", type=>"UV"},
            {name=>"SA_ONSTACK", type=>"UV"},
            {name=>"SA_RESETHAND", type=>"UV"},
            {name=>"SA_RESTART", type=>"UV"},
            {name=>"SA_SIGINFO", type=>"UV"},
            {name=>"SIG_DFL", type=>"IV", value=>"(IV)SIG_DFL"},
            {name=>"SIG_ERR", type=>"IV", value=>"(IV)SIG_ERR"},
            {name=>"SIG_IGN", type=>"IV", value=>"(IV)SIG_IGN"},
            {name=>"UCHAR_MAX", type=>"UV"},
            {name=>"UINT_MAX", type=>"UV"},
            {name=>"ULONG_MAX", type=>"UV"},
            {name=>"USHRT_MAX", type=>"UV"},
            {name=>"_POSIX_ARG_MAX", type=>"IV", default=>["IV", "0"]},
            {name=>"_POSIX_CHILD_MAX", type=>"IV", default=>["IV", "0"]},
            {name=>"_POSIX_CHOWN_RESTRICTED", type=>"IV", default=>["IV", "0"]},
            {name=>"_POSIX_JOB_CONTROL", type=>"YES", default=>["IV", "0"]},
            {name=>"_POSIX_LINK_MAX", type=>"IV", default=>["IV", "0"]},
            {name=>"_POSIX_MAX_CANON", type=>"IV", default=>["IV", "0"]},
            {name=>"_POSIX_MAX_INPUT", type=>"IV", default=>["IV", "0"]},
            {name=>"_POSIX_NAME_MAX", type=>"IV", default=>["IV", "0"]},
            {name=>"_POSIX_NGROUPS_MAX", type=>"IV", default=>["IV", "0"]},
            {name=>"_POSIX_NO_TRUNC", type=>"IV", default=>["IV", "0"]},
            {name=>"_POSIX_OPEN_MAX", type=>"IV", default=>["IV", "0"]},
            {name=>"_POSIX_PATH_MAX", type=>"IV", default=>["IV", "0"]},
            {name=>"_POSIX_PIPE_BUF", type=>"IV", default=>["IV", "0"]},
            {name=>"_POSIX_SAVED_IDS", type=>"YES", default=>["IV", "0"]},
            {name=>"_POSIX_SSIZE_MAX", type=>"IV", default=>["IV", "0"]},
            {name=>"_POSIX_STREAM_MAX", type=>"IV", default=>["IV", "0"]},
            {name=>"_POSIX_TZNAME_MAX", type=>"IV", default=>["IV", "0"]},
            {name=>"_POSIX_VDISABLE", type=>"IV", default=>["IV", "0"]},
            {name=>"_POSIX_VERSION", type=>"IV", default=>["IV", "0"]});

print constant_types(); # macro defs
foreach (C_constant ("POSIX", 'constant', 'IV', $types, undef, 3, @names) ) {
    print $_, "\n"; # C constant subs
}
print "#### XS Section:\n";
print XS_constant ("POSIX", $types);
__END__
   */

  switch (len) {
  case 2:
    if (name[0] == 'B' && name[1] == '0') {
#ifdef B0
      *iv_return = B0;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 3:
    return constant_3 (aTHX_ name, iv_return);
    break;
  case 4:
    return constant_4 (aTHX_ name, iv_return);
    break;
  case 5:
    return constant_5 (aTHX_ name, iv_return);
    break;
  case 6:
    return constant_6 (aTHX_ name, iv_return);
    break;
  case 7:
    return constant_7 (aTHX_ name, iv_return, nv_return);
    break;
  case 8:
    return constant_8 (aTHX_ name, iv_return, nv_return);
    break;
  case 9:
    return constant_9 (aTHX_ name, iv_return, nv_return);
    break;
  case 10:
    return constant_10 (aTHX_ name, iv_return, nv_return);
    break;
  case 11:
    return constant_11 (aTHX_ name, iv_return, nv_return);
    break;
  case 12:
    return constant_12 (aTHX_ name, iv_return, nv_return);
    break;
  case 13:
    return constant_13 (aTHX_ name, iv_return, nv_return);
    break;
  case 14:
    return constant_14 (aTHX_ name, iv_return, nv_return);
    break;
  case 15:
    return constant_15 (aTHX_ name, iv_return, nv_return);
    break;
  case 16:
    return constant_16 (aTHX_ name, iv_return);
    break;
  case 17:
    /* Names all of length 17.  */
    /* _POSIX_STREAM_MAX _POSIX_TZNAME_MAX */
    /* Offset 7 gives the best switch position.  */
    switch (name[7]) {
    case 'S':
      if (memEQ(name, "_POSIX_STREAM_MAX", 17)) {
      /*                      ^                */
#ifdef _POSIX_STREAM_MAX
        *iv_return = _POSIX_STREAM_MAX;
        return PERL_constant_ISIV;
#else
        *iv_return = 0;
        return PERL_constant_ISIV;
#endif
      }
      break;
    case 'T':
      if (memEQ(name, "_POSIX_TZNAME_MAX", 17)) {
      /*                      ^                */
#ifdef _POSIX_TZNAME_MAX
        *iv_return = _POSIX_TZNAME_MAX;
        return PERL_constant_ISIV;
#else
        *iv_return = 0;
        return PERL_constant_ISIV;
#endif
      }
      break;
    }
    break;
  case 18:
    /* Names all of length 18.  */
    /* _POSIX_JOB_CONTROL _POSIX_NGROUPS_MAX */
    /* Offset 12 gives the best switch position.  */
    switch (name[12]) {
    case 'O':
      if (memEQ(name, "_POSIX_JOB_CONTROL", 18)) {
      /*                           ^            */
#ifdef _POSIX_JOB_CONTROL
        return PERL_constant_ISYES;
#else
        *iv_return = 0;
        return PERL_constant_ISIV;
#endif
      }
      break;
    case 'P':
      if (memEQ(name, "_POSIX_NGROUPS_MAX", 18)) {
      /*                           ^            */
#ifdef _POSIX_NGROUPS_MAX
        *iv_return = _POSIX_NGROUPS_MAX;
        return PERL_constant_ISIV;
#else
        *iv_return = 0;
        return PERL_constant_ISIV;
#endif
      }
      break;
    }
    break;
  case 20:
    if (memEQ(name, "_PC_CHOWN_RESTRICTED", 20)) {
#ifdef _PC_CHOWN_RESTRICTED
      *iv_return = _PC_CHOWN_RESTRICTED;
      return PERL_constant_ISIV;
#else
      return PERL_constant_NOTDEF;
#endif
    }
    break;
  case 23:
    if (memEQ(name, "_POSIX_CHOWN_RESTRICTED", 23)) {
#ifdef _POSIX_CHOWN_RESTRICTED
      *iv_return = _POSIX_CHOWN_RESTRICTED;
      return PERL_constant_ISIV;
#else
      *iv_return = 0;
      return PERL_constant_ISIV;
#endif
    }
    break;
  }
  return PERL_constant_NOTFOUND;
}

