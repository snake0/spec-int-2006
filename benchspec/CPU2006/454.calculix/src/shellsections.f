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
      subroutine shellsections(text,textpart,set,istartset,iendset,
     &  ialset,nset,ielmat,matname,nmat,ielorien,orname,norien,
     &  thicke,kon,ipkon,offset,istep,istat,in,n)
!
!     reading the input deck: *SHELL SECTION
!
      implicit none
!
      logical nodalthickness
!
      character*20 matname(*),orname(*),material,orientation
      character*21 set(*),elset
      character*40 textpart(16)
      character*132 text
!
      integer istartset(*),iendset(*),ialset(*),ielmat(*),
     &  ielorien(*),kon(*),ipkon(*),indexe
!
      integer nset,nmat,norien,istep,istat,in,n,key,i,j,k,l,imaterial,
     &  iorientation,ipos
!
      real*8 thicke(2,*),thickness,offset(2,*),offset1
!
      if(istep.gt.0) then
         write(*,*) '*ERROR in solidsections: *SOLID SECTION should'
         write(*,*) '  be placed before all step definitions'
         stop
      endif
!
      nodalthickness=.false.
      offset1=0.d0
      orientation='                    '
      do i=2,n
         if(textpart(i)(1:9).eq.'MATERIAL=') then
            material=textpart(i)(10:29)
         elseif(textpart(i)(1:12).eq.'ORIENTATION=') then
            orientation=textpart(i)(13:32)
         elseif(textpart(i)(1:6).eq.'ELSET=') then
            elset=textpart(i)(7:26)
            elset(21:21)=' '
            ipos=index(elset,' ')
            elset(ipos:ipos)='E'
         elseif(textpart(i)(1:14).eq.'NODALTHICKNESS') then
            nodalthickness=.true.
         elseif(textpart(i)(1:7).eq.'OFFSET=') then
            read(textpart(i)(8:40),'(f33.0)',iostat=istat) offset1
            if(istat.gt.0) call inputerror(text)
         endif
      enddo
!
!     check for the existence of the set,the material and orientation
!
      do i=1,nmat
         if(matname(i).eq.material) exit
      enddo
      if(i.gt.nmat) then
         write(*,*) '*ERROR in solidsections: nonexistent material'
         write(*,*) '  Card image:'
         write(*,'(a132)') text
         stop
      endif
      imaterial=i
!
      if(orientation.eq.'                    ') then
         iorientation=0
      else
         do i=1,norien
            if(orname(i).eq.orientation) exit
         enddo
         if(i.gt.norien) then
            write(*,*)'*ERROR in solidsections: nonexistent orientation'
            write(*,*) '  Card image:'
            write(*,'(a132)') text
            stop
         endif
         iorientation=i
      endif
!
      do i=1,nset
         if(set(i).eq.elset) exit
      enddo
      if(i.gt.nset) then
         elset(ipos:ipos)=' '
         write(*,*) '*ERROR in solidsections: element set ',elset
         write(*,*) '  has not yet been defined. Card image:'
         write(*,'(a132)') text
         stop
      endif
!
!     assigning the elements of the set the appropriate material,
!     orientation number and offset
!
      do j=istartset(i),iendset(i)
         if(ialset(j).gt.0) then
            ielmat(ialset(j))=imaterial
            ielorien(ialset(j))=iorientation
            offset(1,ialset(j))=offset1
         else
            k=ialset(j-2)
            do
               k=k-ialset(j)
               if(k.ge.ialset(j-1)) exit
               ielmat(k)=imaterial
               ielorien(k)=iorientation
               offset(1,k)=offset1
            enddo
         endif
      enddo
!
      call getnewline(text,textpart,istat,in,n,key)
!
!     assigning a thickness to the elements
!
      if(.not.nodalthickness) then
         read(textpart(1),'(f40.0)',iostat=istat) thickness
         if(istat.gt.0) call inputerror(text)
         do j=istartset(i),iendset(i)
            if(ialset(j).gt.0) then
               indexe=ipkon(ialset(j))
               do l=1,8
                  thicke(1,indexe+l)=thickness
               enddo
            else
               k=ialset(j-2)
               do
                  k=k-ialset(j)
                  if(k.ge.ialset(j-1)) exit
                  indexe=ipkon(k)
                  do l=1,8
                     thicke(1,indexe+l)=thickness
                  enddo
               enddo
            endif
         enddo
      endif
      call getnewline(text,textpart,istat,in,n,key)
!
      return
      end

