      subroutine near2d(xo,yo,x,y,nx,ny,xp,yp,n,node)
C
c     determines the closest node out of n with coordinates in
c     xo,yo to the point with coordinates (xp,yp);
c
c     order xo and yo before the first call to near2d and
c     store the results with the corresponding permutation array
c     in x,y and nx,ny, respectively
c
      IMPLICIT none
C
      integer node,nx(*),ny(*),ni(5),n,m,idx,idy,it,j,iz,kflag,i
c
      real*8 x(*),y(*),xo(*),yo(*),r(5),xr,yr,aan,aas,aaw,aao,
     &  aano,aaso,aasw,aanw,aamin,xx,yy,rr,xp,yp
c
      CALL IDENT(X,XP,N,IDX)
      CALL IDENT(Y,YP,N,IDY)
C
      kflag=2
      i=1
C
      XR=XO(1)-XP
      YR=YO(1)-YP
      R(1)=dSQRT(XR*XR+YR*YR)
      NI(1)=1
C
      loop: do
c
         IZ=0
C
C     WEST
C
         M=IDX-i+1
         IF(M.LE.0) THEN
            AAW=R(1)
            GO TO 4
         END IF
C
         XX=XO(NX(M))
         YY=YO(NX(M))
         AAW=XX-XP
         YR=YY-YP
         RR=SQRT(AAW*AAW+YR*YR)
C
         IF(RR.GE.R(1)) GO TO 4
         IT=IZ+1
         DO 8 J=1,IT
            IF(NX(M).EQ.NI(J)) GO TO 4
 8       CONTINUE
         IZ=IZ+1
         R(IZ+1)=RR
         NI(IZ+1)=NX(M)
C
C     EAST
C
 4       M=IDX+i
         IF(M.GT.N) THEN
            AAO=R(1)
            GO TO 6
         END IF
C
         XX=XO(NX(M))
         YY=YO(NX(M))
         AAO=XX-XP
         YR=YY-YP
         RR=SQRT(AAO*AAO+YR*YR)
C
         IF(RR.GE.R(1)) GO TO 6
         IT=IZ+1
         DO 10 J=1,IT
            IF(NX(M).EQ.NI(J)) GO TO 6
 10      CONTINUE
         IZ=IZ+1
         R(IZ+1)=RR
         NI(IZ+1)=NX(M)
C
C     SOUTH
C
 6       M=IDY-i+1
         IF(M.LE.0) THEN
            AAS=R(1)
            GO TO 5
         END IF
C
         XX=XO(NY(M))
         YY=YO(NY(M))
         XR=XX-XP
         AAS=YY-YP
         RR=SQRT(XR*XR+AAS*AAS)
C
         IF(RR.GE.R(1)) GO TO 5
         IT=IZ+1
         DO 9 J=1,IT
            IF(NY(M).EQ.NI(J)) GO TO 5
 9       CONTINUE
         IZ=IZ+1
         R(IZ+1)=RR
         NI(IZ+1)=NY(M)
C
C     NORTH
C
 5       M=IDY+i
         IF(M.GT.N) THEN
            AAN=R(1)
            GO TO 7
         END IF
C
         XX=XO(NY(M))
         YY=YO(NY(M))
         XR=XX-XP
         AAN=YY-YP
         RR=SQRT(XR*XR+AAN*AAN)
C
         IF(RR.GE.R(1)) GO TO 7
         IT=IZ+1
         DO 11 J=1,IT
            IF(NY(M).EQ.NI(J)) GO TO 7
 11      CONTINUE
         IZ=IZ+1
         R(IZ+1)=RR
         NI(IZ+1)=NY(M)
C
 7       AANO=SQRT(AAN*AAN+AAO*AAO)
         AASO=SQRT(AAS*AAS+AAO*AAO)
         AASW=SQRT(AAS*AAS+AAW*AAW)
         AANW=SQRT(AAN*AAN+AAW*AAW)
         AAMIN=MIN(AANO,AASO,AASW,AANW)
C
         IF(IZ.NE.0) THEN
            IZ=IZ+1
            CALL dsort(R,NI,IZ,kflag)
         ENDIF
C
         IF(R(1).LE.AAMIN) THEN
            exit loop
         ELSE
            i=i+1
            cycle loop
         ENDIF
C
      enddo loop
C
      node=ni(1)
c
      RETURN
      END
