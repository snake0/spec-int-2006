!
!     CalculiX - A 3-dimensional finite element program
!              Copyright (C) 1998 Guido Dhondt
!
!     This program is free software; you can redistribute it and/or
!     modify it under the terms of the GNU General Public License as
!     published by the Free Software Foundation(version 2);
!     
!
!     This program is distributed in the hope that it will be useful,
!     but WITHOUT ANY WARRANTY; without even the implied warranty of 
!     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the 
!     GNU General Public License for more details.
!
!     You should have received a copy of the GNU General Public License
!     along with this program; if not, write to the Free Software
!     Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
!
      subroutine frd(co,nk,kon,ipkon,lakon,ne,v,stn,inum,nmethod,
     &  kode,nodeflab,een,t1,fn,time,epn,ielmat,matname,enern,xstaten,
     &  nstate_,istep,iinc)
!
!     stores the results in frd format
!
      implicit none
!
      character*1 c
      character*3 m1,m2,m3,m4,m5
      character*4 nodeflab(*)
      character*5 p0,p1,p2,p3,p4,p5,p6
      character*8 lakon(*)
      character*20 matname(*)
      character*132 text
!
      integer kon(*),inum(*),nk,ne,nmethod,kode,i,j,ipkon(*),indexe,
     &  one,ielmat(*),lb,nterms,nstate_,l,istep,iinc
!
      real*8 co(3,*),v(0:3,*),stn(6,*),een(6,*),t1(*),fn(0:3,*),time,
     &  epn(*),enern(*),xstaten(nstate_,*)
!
#ifdef SPEC_CPU
! Don't bother with IO that is not needed; the ".dat" 
! file will be used for validation
      return
#endif

      c='C'
!
      m1=' -1'
      m2=' -2'
      m3=' -3'
      m4=' -4'
      m5=' -5'
!
      p0='    0'
      p1='    1'
      p2='    2'
      p3='    3'
      p4='    4'
      p5='    5'
      p6='    6'
!
      one=1
!
      if(kode.eq.1) then
!
        write(7,'(a5,a1)') p1,c
!
!       storing the coordinates of the nodes
!
        write(7,'(a5,a1,67x,i1)') p2,c,one
!
        if(nmethod.ne.0) then
          do i=1,nk
             if(inum(i).eq.0) cycle
             write(7,100) m1,i,(co(j,i),j=1,3)
          enddo
        else
          do i=1,nk
             write(7,100) m1,i,(co(j,i),j=1,3)
          enddo
        endif
!
        write(7,'(a3)') m3
!
!       storing the element topology (for 8-node 3-D hexa elements)
!
        write(7,'(a5,a1,67x,i1)') p3,c,one
!
        do i=1,ne
!
           if(ipkon(i).lt.0) cycle
           indexe=ipkon(i)
           if(lakon(i)(4:4).eq.'2') then
              write(7,'(a3,i10,3a5)') m1,i,p4,p0,matname(ielmat(i))(1:5)
              write(7,'(a3,10i10)') m2,(kon(indexe+j),j=1,10)
              write(7,'(a3,10i10)') m2,(kon(indexe+j),j=11,12),
     &             (kon(indexe+j),j=17,19),kon(indexe+20),
     &             (kon(indexe+j),j=13,16)
           elseif(lakon(i)(4:4).eq.'8') then
              write(7,'(a3,i10,3a5)') m1,i,p1,p0,matname(ielmat(i))(1:5)
              write(7,'(a3,8i10)') m2,(kon(indexe+j),j=1,8)
           elseif(lakon(i)(4:5).eq.'10') then
              write(7,'(a3,i10,3a5)') m1,i,p6,p0,matname(ielmat(i))(1:5)
              write(7,'(a3,10i10)') m2,(kon(indexe+j),j=1,10)
           elseif(lakon(i)(4:4).eq.'4') then
              write(7,'(a3,i10,3a5)') m1,i,p3,p0,matname(ielmat(i))(1:5)
              write(7,'(a3,4i10)') m2,(kon(indexe+j),j=1,4)
           elseif(lakon(i)(4:5).eq.'15') then
              write(7,'(a3,i10,3a5)') m1,i,p5,p0,matname(ielmat(i))(1:5)
              write(7,'(a3,10i10)') m2,(kon(indexe+j),j=1,9),
     &          kon(indexe+13)
              write(7,'(a3,5i10)') m2,(kon(indexe+j),j=14,15),
     &          (kon(indexe+j),j=10,12)
           elseif(lakon(i)(4:4).eq.'6') then
              write(7,'(a3,i10,3a5)') m1,i,p2,p0,matname(ielmat(i))(1:5)
              write(7,'(a3,6i10)') m2,(kon(indexe+j),j=1,6)
           endif
!
        enddo
!
        write(7,'(a3)') m3
!
        if(nmethod.eq.0) return
      endif
!
!     storing the displacements of the nodes
!
      if(nodeflab(1).eq.'U   ') then
         text=
     & '  100CL       .00000E+00            Beschreibung         3    1'
         text(74:74)='1'
         write(text(8:10),'(i3)') 100+kode
         write(text(13:24),'(e12.5)') time
         write(text(59:63),'(i5)') kode
         write(7,'(a132)') text
         text=' -4  DISP        4    1'
         write(7,'(a132)') text
         text=' -5  D1          1    2    1    0'
         write(7,'(a132)') text
         text=' -5  D2          1    2    2    0'
         write(7,'(a132)') text
         text=' -5  D3          1    2    3    0'
         write(7,'(a132)') text
         text=' -5  ALL         1    2    0    0    1ALL'
         write(7,'(a132)') text
!
         do i=1,nk
            if(inum(i).eq.0) cycle
            write(7,100) m1,i,(v(j,i),j=1,3)
         enddo
!
         write(7,'(a3)') m3
      endif
!
!     storing the temperatures in the nodes
!
      if(nodeflab(2).eq.'NT  ') then
         text=
     & '  100CL       .00000E+00            Beschreibung         3    1'
         text(74:74)='1'
         write(text(8:10),'(i3)') 100+kode
         write(text(13:24),'(e12.5)') time
         write(text(59:63),'(i5)') kode
         write(7,'(a132)') text
         text=' -4  NDTEMP      1    1'
         write(7,'(a132)') text
         text=' -5  T           1    1    0    0'
         write(7,'(a132)') text
!
         do i=1,nk
            if(inum(i).eq.0) cycle
            write(7,100) m1,i,t1(i)
         enddo
!
         write(7,'(a3)') m3
      endif
!
!     storing the stresses in the nodes
!
      if(nodeflab(3).eq.'S   ') then
         text=
     & '  100CL       .00000E+00            Beschreibung         3    1'
         text(74:74)='1'
         write(text(8:10),'(i3)') 100+kode      
         write(text(13:24),'(e12.5)') time
         write(text(59:63),'(i5)') kode
         write(7,'(a132)') text
         text=' -4  STRESS      6    1'
         write(7,'(a132)') text
         text=' -5  SXX         1    4    1    1'
         write(7,'(a132)') text
         text=' -5  SYY         1    4    2    2'
         write(7,'(a132)') text
         text=' -5  SZZ         1    4    3    3'
         write(7,'(a132)') text
         text=' -5  SXY         1    4    1    2'
         write(7,'(a132)') text
         text=' -5  SYZ         1    4    2    3'
         write(7,'(a132)') text
         text=' -5  SZX         1    4    3    1'
         write(7,'(a132)') text
!
         do i=1,nk
            if(inum(i).eq.0) cycle
            write(7,101) m1,i,(stn(j,i),j=1,4),
     &           stn(6,i),stn(5,i)
         enddo
!
         write(7,'(a3)') m3
      endif
!
!     storing the strains in the nodes
!
      if(nodeflab(4).eq.'E   ') then
         text=
     & '  100CL       .00000E+00            Beschreibung         3    1'
         text(74:74)='1'
         write(text(8:10),'(i3)') 100+kode      
         write(text(13:24),'(e12.5)') time
         write(text(59:63),'(i5)') kode
         write(7,'(a132)') text
         text=' -4  STRAIN      6    1'
         write(7,'(a132)') text
         text=' -5  EXX         1    4    1    1'
         write(7,'(a132)') text
         text=' -5  EYY         1    4    2    2'
         write(7,'(a132)') text
         text=' -5  EZZ         1    4    3    3'
         write(7,'(a132)') text
         text=' -5  EXY         1    4    1    2'
         write(7,'(a132)') text
         text=' -5  EYZ         1    4    2    3'
         write(7,'(a132)') text
         text=' -5  EZX         1    4    3    1'
         write(7,'(a132)') text
!
         do i=1,nk
            if(inum(i).eq.0) cycle
            write(7,101) m1,i,(een(j,i),j=1,4),
     &           een(6,i),een(5,i)
         enddo
!
         write(7,'(a3)') m3
      endif
!
!     storing the forces in the nodes
!
      if(nodeflab(5).eq.'RF  ') then
         text=
     & '  100CL       .00000E+00            Beschreibung         3    1'
         text(74:74)='1'
         write(text(8:10),'(i3)') 100+kode
         write(text(13:24),'(e12.5)') time
         write(text(59:63),'(i5)') kode
         write(7,'(a132)') text
         text=' -4  FORC        4    1'
         write(7,'(a132)') text
         text=' -5  F1          1    2    1    0'
         write(7,'(a132)') text
         text=' -5  F2          1    2    2    0'
         write(7,'(a132)') text
         text=' -5  F3          1    2    3    0'
         write(7,'(a132)') text
         text=' -5  ALL         1    2    0    0    1ALL'
         write(7,'(a132)') text
!
         do i=1,nk
            if(inum(i).eq.0) cycle
            write(7,100) m1,i,(fn(j,i),j=1,3)
         enddo
!
         write(7,'(a3)') m3
      endif
!
!     storing the equivalent plastic strains in the nodes
!
      if(nodeflab(6).eq.'PE  ') then
         text=
     & '  100CL       .00000E+00            Beschreibung         3    1'
         text(74:74)='1'
         write(text(8:10),'(i3)') 100+kode
         write(text(13:24),'(e12.5)') time
         write(text(59:63),'(i5)') kode
         write(7,'(a132)') text
         text=' -4  PE          1    1'
         write(7,'(a132)') text
         text=' -5  PE          1    1    0    0'
         write(7,'(a132)') text
!
         do i=1,nk
            if(inum(i).eq.0) cycle
            write(7,100) m1,i,epn(i)
         enddo
!
         write(7,'(a3)') m3
      endif
!
!     storing the energy in the nodes
!
      if(nodeflab(7).eq.'ENER') then
         text=
     & '  100CL       .00000E+00            Beschreibung         3    1'
         text(74:74)='1'
         write(text(8:10),'(i3)') 100+kode
         write(text(13:24),'(e12.5)') time
         write(text(59:63),'(i5)') kode
         write(7,'(a132)') text
         text=' -4  ENER        1    1'
         write(7,'(a132)') text
         text=' -5  ENER        1    1    0    0'
         write(7,'(a132)') text
!
         do i=1,nk
            if(inum(i).eq.0) cycle
            write(7,100) m1,i,enern(i)
         enddo
!
         write(7,'(a3)') m3
      endif
!
!     storing the internal state variables in the nodes
!
      if(nodeflab(8).eq.'SDV ') then
         do l=1,(nstate_+5)/6
            lb=(l-1)*6
            text=
     & '  100CL       .00000E+00            Beschreibung         3    1'
            text(74:74)='1'
            write(text(8:10),'(i3)') 100+kode      
            write(text(13:24),'(e12.5)') time
            write(text(59:63),'(i5)') kode
            write(7,'(a132)') text
            if(l.eq.(nstate_+5)/6) then
               nterms=nstate_-lb
            else
               nterms=6
            endif
            text=' -4  SDV         6    1'
            write(text(18:18),'(i1)') nterms
            if(lb+1.le.9) then
               write(text(9:9),'(i1)') lb+1
            else
               write(text(9:10),'(i2)') lb+1
            endif
            write(7,'(a132)') text
            do j=1,nterms
               text=' -5  SDV         1    1    0    0'
               if(lb+j.le.9) then
                  write(text(9:9),'(i1)') lb+j
               else
                  write(text(9:10),'(i2)') lb+j
               endif
               write(7,'(a132)') text
            enddo
!
            if(l.eq.(nstate_+5)/6) then
               do i=1,nk
                  if(inum(i).eq.0) cycle
                  write(7,101) m1,i,(xstaten(lb+j,i),j=1,nstate_-lb)
               enddo
            else
               do i=1,nk
                  if(inum(i).eq.0) cycle
                  write(7,101) m1,i,(xstaten(lb+j,i),j=1,6)
               enddo
            endif
!
            write(7,'(a3)') m3
         enddo
      endif
!
 100  format(a3,i10,1p,3e12.5)
 101  format(a3,i10,1p,6e12.5)
!
      return
      end
