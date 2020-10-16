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
      subroutine transforms(text,textpart,trab,ntrans,ntrans_,
     &     inotr,set,istartset,iendset,ialset,nset,istep,istat,
     &     in,n)
!
!     reading the input deck: *TRANSFORM
!
      implicit none
!
      real*8 trab(7,*)
!
      character*21 set(*),noset
      character*40 textpart(16)
      character*132 text
!
      integer ntrans,ntrans_,istep,istat,in,n,key,i,j,k,inotr(2,*),
     &  istartset(*),iendset(*),ialset(*),nset,ipos
!
      if(istep.gt.0) then
         write(*,*) '*ERROR in transforms: *TRANSFORM should be'
         write(*,*) '  placed before all step definitions'
         stop
      endif
!
      ntrans=ntrans+1
      if(ntrans.gt.ntrans_) then
         write(*,*) '*ERROR in transforms: increase ntrans_'
         stop
      endif
!
!     rectangular coordinate system: trab(7,norien)=1
!     cylindrical coordinate system: trab(7,norien)=-1
!     default is rectangular
!
      trab(7,ntrans)=1.d0
!
      do i=2,n
         if(textpart(i)(1:5).eq.'NSET=') then
            noset=textpart(i)(6:25)
            noset(21:21)=' '
            ipos=index(noset,' ')
            noset(ipos:ipos)='N'
         elseif(textpart(i)(1:5).eq.'TYPE=') then
            if(textpart(i)(6:6).eq.'C') then
               trab(7,ntrans)=-1.d0
            endif
         endif
      enddo
!
      call getnewline(text,textpart,istat,in,n,key)
      if((istat.lt.0).or.(key.eq.1)) then
         write(*,*)'*ERROR in transforms: definition of a'
         write(*,*) '  transformation is not complete'
         call inputerror(text)
         stop
      endif
!
      do i=1,6
         read(textpart(i),'(f40.0)',iostat=istat) trab(i,ntrans)
         if(istat.gt.0) call inputerror(text)
      enddo
!
      do i=1,nset
         if(set(i).eq.noset) exit
      enddo
      if(i.gt.nset) then
         noset(ipos:ipos)=' '
         write(*,*) '*ERROR in transforms: node set ',noset
         write(*,*) '  has not yet been defined.'
         stop
      endif
      do j=istartset(i),iendset(i)
         if(ialset(j).gt.0) then
            inotr(1,ialset(j))=ntrans
         else
            k=ialset(j-2)
            do
               k=k-ialset(j)
               if(k.ge.ialset(j-1)) exit
               inotr(1,k)=ntrans
            enddo
         endif
      enddo
!
      call getnewline(text,textpart,istat,in,n,key)
!
      return
      end

