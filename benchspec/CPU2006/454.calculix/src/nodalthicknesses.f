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
      subroutine nodalthicknesses(text,textpart,set,istartset,iendset,
     &  ialset,nset,thickn,nk,istep,istat,in,n)
!
!     reading the input deck: *NODAL THICKNESS
!
      implicit none
!
      integer istartset(*),iendset(*),ialset(*)
!
      integer nset,nk,istep,istat,in,n,key,i,j,k,l,ipos
!
      real*8 thickn(2,*)
!
      real*8 thickness1,thickness2
!
      character*21 set(*),noset
      character*40 textpart(16)
      character*132 text
!
      if(istep.gt.0) then
         write(*,*) '*ERROR in nodalthicknesses: *NODAL THICKNESS'
         write(*,*) '      should be placed before all step definitions'
         stop
      endif
!
      do
         call getnewline(text,textpart,istat,in,n,key)
         if((istat.lt.0).or.(key.eq.1)) return
         read(textpart(2),'(f40.0)',iostat=istat) thickness1
         if(istat.gt.0) call inputerror(text)
         if(n.eq.2) then
            thickness2=0.d0
         else
            read(textpart(2),'(f40.0)',iostat=istat) thickness2
            if(istat.gt.0) call inputerror(text)
         endif
         read(textpart(1),'(i40)',iostat=istat) l
         if(istat.eq.0) then
            thickn(1,l)=thickness1
            thickn(2,l)=thickness2
         else
            read(textpart(1)(1:20),'(a20)',iostat=istat) noset
            noset(21:21)=' '
            ipos=index(noset,' ')
            noset(ipos:ipos)='N'
            do i=1,nset
               if(set(i).eq.noset) exit
            enddo
            if(i.gt.nset) then
               noset(ipos:ipos)=' '
               write(*,*) '*ERROR in nodalthicknesses: node set ',noset
               write(*,*) '  has not yet been defined. Card image:'
               write(*,'(a132)') text
               stop
            endif
            do j=istartset(i),iendset(i)
               if(ialset(j).gt.0) then
                  thickn(1,ialset(j))=thickness1
                  thickn(2,ialset(j))=thickness2
               else
                  k=ialset(j-2)
                  do
                     k=k-ialset(j)
                     if(k.ge.ialset(j-1)) exit
                     thickn(1,k)=thickness1
                     thickn(2,k)=thickness2
                  enddo
               endif
            enddo
         endif
      enddo
!
      return
      end

