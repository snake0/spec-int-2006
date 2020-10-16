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
      subroutine normals(text,textpart,iponor,xnor,ixfree,
     &  ipkon,kon,nk,nk_,ne,lakon,istep,istat,in,n)
!
!     reading the input deck: *NORMAL
!
      implicit none
!
      character*8 lakon(*)
!
      integer iponor(2,*),ixfree,ipkon(*),kon(*),nk,
     &  nk_,ne,istep,istat,in,n,ielement,node,j,indexe,
     &  key
!
      real*8 xnor(*),x,y,z,dd
!
      character*40 textpart(16)
      character*132 text
!
      if(istep.gt.0) then
         write(*,*) '*ERROR in normals: *NORMAL should be placed'
         write(*,*) '  before all step definitions'
         stop
      endif
!
      loop:do
         call getnewline(text,textpart,istat,in,n,key)
         if((istat.lt.0).or.(key.eq.1)) exit
!
         read(textpart(1),'(i40)',iostat=istat) ielement
         if(istat.gt.0) call inputerror(text)
         read(textpart(2),'(i40)',iostat=istat) node
         if(istat.gt.0) call inputerror(text)
         read(textpart(3),'(f40.0)',iostat=istat) x
         if(istat.gt.0) call inputerror(text)
         if(n.le.3) then
            y=0.d0
         else
            read(textpart(4),'(f40.0)',iostat=istat) y
            if(istat.gt.0) call inputerror(text)
         endif
         if(n.le.4) then
            z=0.d0
         else
            read(textpart(5),'(f40.0)',iostat=istat) z
            if(istat.gt.0) call inputerror(text)
         endif
!
!        normalizing the normal
!
         dd=dsqrt(x*x+y*y+z*z)
         x=x/dd
         y=y/dd
         z=z/dd
!
         if(ielement.gt.ne) then
            write(*,*) '*ERROR in normals: element number',ielement
            write(*,*) '       exceeds ne'
            stop
         endif
!
         indexe=ipkon(ielement)
         do j=1,8
            if(kon(indexe+j).eq.node) then
               iponor(1,indexe+j)=ixfree
               if(lakon(ielement)(1:1).eq.'B') then
                  xnor(ixfree+4)=x
                  xnor(ixfree+5)=y
                  xnor(ixfree+6)=z
                  ixfree=ixfree+6
               elseif(lakon(ielement)(1:2).ne.'C3') then
                  xnor(ixfree+1)=x
                  xnor(ixfree+2)=y
                  xnor(ixfree+3)=z
                  ixfree=ixfree+3
               else
                  write(*,*) '*WARNING in normals: specifying a normal'
                  write(*,*) '         3-D element does not make sense'
               endif
               cycle loop
            endif
         enddo
         write(*,*) '*WARNING: node ',node,' does not belong to'
         write(*,*) '          element ',ielement
         write(*,*) '          normal definition discarded'
!
      enddo loop
!
      return
      end










