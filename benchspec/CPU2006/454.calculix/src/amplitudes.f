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
      subroutine amplitudes(text,textpart,amname,amta,namta,nam,
     &  nam_,namtot_,istep,istat,in,n)
!
!     reading the input deck: *AMPLITUDE
!
      implicit none
!
      real*8 amta(2,*),x,y
!
      character*20 amname(*)
      character*40 textpart(16)
      character*132 text
!
      integer namta(3,*),nam,nam_,istep,istat,in,n,key,i,namtot,
     &  namtot_
!
      if(istep.gt.0) then
         write(*,*) '*ERROR in amplitudes: *AMPLITUDE should be'
         write(*,*) '  placed before all step definitions'
         stop
      endif
!
      nam=nam+1
      if(nam.gt.nam_) then
         write(*,*) '*ERROR in amplitudes: increase nam_'
         stop
      endif
      namta(3,nam)=0
!
      do i=2,n
         if(textpart(i)(1:5).eq.'NAME=') then
            amname(nam)=textpart(i)(6:25)
         elseif(textpart(i)(1:14).eq.'TIME=TOTALTIME') then
            namta(3,nam)=1
         endif
      enddo
!
      if(nam.eq.1) then
         namtot=0
      else
         namtot=namta(2,nam-1)
      endif
      namta(1,nam)=namtot+1
!
      do
         call getnewline(text,textpart,istat,in,n,key)
         if((istat.lt.0).or.(key.eq.1)) return
         do i=1,4
            if(textpart(2*i-1).ne.
     &         '                                        ') then
               namtot=namtot+1
               if(namtot.gt.namtot_) then
                  write(*,*) '*ERROR in amplitudes: increase nam_'
                  stop
               endif
               read(textpart(2*i-1),'(f40.0)',iostat=istat) x
               if(istat.gt.0) call inputerror(text)
               read(textpart(2*i),'(f40.0)',iostat=istat) y
               if(istat.gt.0) call inputerror(text)
               amta(1,namtot)=x
               amta(2,namtot)=y
               namta(2,nam)=namtot
            else
               exit
            endif
         enddo
      enddo
!
      return
      end

