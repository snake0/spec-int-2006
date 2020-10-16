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
      subroutine include(text,in)
!
      implicit none
!
      character*132 text,includefn
!
      integer in,nstart,nend,ii,jj,kk,j
!
      if(in.eq.4) then
         write(*,*) '*ERROR in include: include statements can'
         write(*,*) '       not be cascaded over more than 3 levels'
         stop
      endif
!
      nstart=0
      nend=0
!
      loop: do ii=1,132
        if(text(ii:ii).eq.'=') then
           do jj=ii+1,132
              if(text(jj:jj).ne.' ') then
                 if(text(jj:jj).eq.'"') then
                    nstart=jj+1
                    do kk=jj+1,132
                       if(text(kk:kk).eq.'"') then
                          nend=kk-1
                          exit loop
                       endif
                    enddo
                    if(kk.eq.81) then
                       write(*,*)'*ERROR in include: wrong file name'
                       write(*,*) '       card image:'
                       write(*,'(a132)') text
                       stop
                    endif
                 else
                    nstart=jj
                    do kk=jj+1,132
                       if(text(kk:kk).eq.' ') then
                          nend=kk-1
                          exit loop
                       endif
                    enddo
                    if(kk.eq.81) then
                       write(*,*)'*ERROR in include: wrong file name'
                       write(*,*) '       card image:'
                       write(*,'(a132)') text
                       stop
                    endif
                 endif
              endif
           enddo
           if(jj.eq.81) then
              write(*,*)'*ERROR in include: no file name'
              write(*,*) '       card image:'
              write(*,'(a132)') text
              stop
           endif
        endif
      enddo loop
      if(ii.eq.81) then
         write(*,*)'*ERROR in include: syntax error'
         write(*,*) '       card image:'
         write(*,'(a132)') text
         stop
      endif
!
      if(nend.ge.nstart) then
         includefn(1:nend-nstart+1)=text(nstart:nend)
         do j=nend-nstart+2,132
           includefn(j:j)=' '
         enddo
      else
         write(*,*) '*ERROR in include: include file nonexistent'
         write(*,*) '       card image:'
         write(*,'(a132)') text
         stop
      endif
!
      in=in+1
      open(in,file=includefn,status='old',ERR=2)
!
      return
!
 2    write(*,*) '*ERROR in include: include file:'
      write(*,*) includefn,' is empty!'
!
      stop
      end
