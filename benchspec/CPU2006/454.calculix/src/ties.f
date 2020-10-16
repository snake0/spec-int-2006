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
      subroutine ties(text,textpart,leftset,rightset,tietol,istep,
     &                istat,in,n)
!
!     reading the input deck: *TIE
!
      implicit none
!
      character*21 leftset,rightset
      character*40 textpart(16)
      character*132 text
!
      integer istep,istat,in,n,i,key,ipos
!
      real*8 tietol
!
      if(istep.gt.0) then
         write(*,*) '*ERROR in ties: *TIE should'
         write(*,*) '  be placed before all step definitions'
         stop
      endif
!
      leftset='                     '
      rightset='                     '
      tietol=-1.d0
!
      do i=2,n
         if(textpart(i)(1:18).eq.'POSITIONTOLERANCE=') then
            read(textpart(i)(19:40),'(f22.0)',iostat=istat) tietol
            if(istat.gt.0) call inputerror(text)
            exit
         endif
      enddo
!
      call getnewline(text,textpart,istat,in,n,key)
      if((istat.lt.0).or.(key.eq.1)) then
         write(*,*)'*ERROR in ties: definition of the tie'
         write(*,*) '      is not complete.'
         stop
      endif
!
      leftset=textpart(1)(1:20)
      leftset(21:21)=' '
      ipos=index(leftset,' ')
      leftset(ipos:ipos)='N'
!
      rightset=textpart(2)(1:20)
      rightset(21:21)=' '
      ipos=index(rightset,' ')
      rightset(ipos:ipos)='N'
!
      call getnewline(text,textpart,istat,in,n,key)
!
      return
      end



