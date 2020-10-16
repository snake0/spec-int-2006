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
      subroutine getnewline(text,textpart,istat,in,n,key)
!
      implicit none
!
!     parser for abainput
!
!     n = # comma's +1,
!
      integer istat,in,n,key,i,j,k
!
      character*1 ctext
      character*40 textpart(16)
      character*132 text
!
!     reading a new line
!
      do
         text=''
         read(in,'(a132)',iostat=istat) text
         if(istat.lt.0) then
            if(text.eq.'') then
               if(in.ne.1) then
                  close(in)
                  in=in-1
                  cycle
               else
                  return
               endif
            else
               istat=0
            endif
         endif
         if((text(1:8).eq.'*include').or.
     &          (text(1:8).eq.'*INCLUDE')) then
            call include(text,in)
         elseif(text(1:2).eq.'**') then
         else
            exit
         endif
      enddo
!
      key=0
!
!     only free format is supported
!
      if((text(1:1).eq.'*').and.(text(2:2).ne.'*')) then
         key=1
      endif
!
      n=1
      j=0
      do i=1,132
         ctext=text(i:i)
         if(ctext.ne.',') then
            if(ctext.eq.' ') then
               cycle
            else
               if((ichar(ctext).ge.97).and.(ichar(ctext).le.122))
     &             ctext=char(ichar(ctext)-32)
            endif
            j=j+1
            if(j.le.40) textpart(n)(j:j)=ctext
         else
            do k=j+1,40
               textpart(n)(k:k)=' '
            enddo
            j=0
            n=n+1
            if(n.gt.16) then
               do k=i+1,132
                  if((text(k:k).ne.' ').and.(text(k:k).ne.',')) then
                     write(*,*) '*ERROR in getnewline: there should not'
                     write(*,*) '       be more than 16 entries in a '
                     write(*,*) '       line; card image:'
                     write(*,'(a132)') text
                     stop
                  endif
               enddo
               exit
            endif
         endif
      enddo
      if(j.eq.0) then
         n=n-1
      else
         do k=j+1,40
            textpart(n)(k:k)=' '
         enddo
      endif
!
!     clearing all textpart fields not used
!
      do i=n+1,16
         textpart(i)='                                        '
      enddo
!
      return
      end



