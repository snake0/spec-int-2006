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
      subroutine beamsections(text,textpart,set,istartset,iendset,
     &  ialset,nset,ielmat,matname,nmat,ielorien,orname,norien,
     &  thicke,kon,ipkon,nk,nk_,iponor,xnor,ixfree,
     &  offset,lakon,istep,istat,in,n)
!
!     reading the input deck: *BEAM SECTION
!
      implicit none
!
      character*4 section
      character*8 lakon(*)
      character*20 matname(*),orname(*),material,orientation
      character*21 set(*),elset
      character*40 textpart(16)
      character*132 text
!
      integer istartset(*),iendset(*),ialset(*),ielmat(*),
     &  ielorien(*),kon(*),ipkon(*)
!
      integer nset,nmat,norien,istep,istat,in,n,key,i,j,k,l,imaterial,
     &  iorientation,ipos,m,nk,nk_,iponor(2,*),ixfree,
     &  indexx,indexe
!
      real*8 thicke(2,*),thickness1,thickness2,p(3),xnor(*),offset(2,*),
     &  offset1,offset2,dd
!
      if(istep.gt.0) then
         write(*,*) '*ERROR in solidsections: *SOLID SECTION should'
         write(*,*) '  be placed before all step definitions'
         stop
      endif
!
      offset1=0.d0
      offset2=0.d0
      orientation='                    '
      section='    '
!
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
         elseif(textpart(i)(1:8).eq.'SECTION=') then
            if(textpart(i)(9:12).eq.'CIRC') then
               section='CIRC'
            elseif(textpart(i)(9:12).eq.'RECT') then
               section='RECT'
            else
               write(*,*) '*ERROR in beamsections: unknown section'
               stop
            endif
         elseif(textpart(i)(1:8).eq.'OFFSET1=') then
            read(textpart(i)(9:40),'(f32.0)',iostat=istat) offset1
            if(istat.gt.0) call inputerror(text)
         elseif(textpart(i)(1:8).eq.'OFFSET2=') then
            read(textpart(i)(9:40),'(f32.0)',iostat=istat) offset2
            if(istat.gt.0) call inputerror(text)
         endif
      enddo
!
!     check whether a sections was defined
!
      if(section.eq.'    ') then
         write(*,*) '*ERROR in solidsections: no section defined'
         stop
      endif
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
!     orientation number, section and offset(s)
!
      do j=istartset(i),iendset(i)
         if(ialset(j).gt.0) then
            ielmat(ialset(j))=imaterial
            ielorien(ialset(j))=iorientation
            offset(1,ialset(j))=offset1
            offset(2,ialset(j))=offset2
            if(section.eq.'RECT') then
               lakon(ialset(j))(8:8)='R'
            else
               lakon(ialset(j))(8:8)='C'
            endif
         else
            k=ialset(j-2)
            do
               k=k-ialset(j)
               if(k.ge.ialset(j-1)) exit
               ielmat(k)=imaterial
               ielorien(k)=iorientation
               offset(1,k)=offset1
               offset(2,k)=offset2
               if(section.eq.'RECT') then
                  lakon(k)(8:8)='R'
               else
                  lakon(k)(8:8)='C'
               endif
            enddo
         endif
      enddo
!
      call getnewline(text,textpart,istat,in,n,key)
!
!     assigning a thickness to the elements
!
      read(textpart(1),'(f40.0)',iostat=istat) thickness1
      if(istat.gt.0) call inputerror(text)
      if(n.gt.1) then
         read(textpart(2),'(f40.0)',iostat=istat) thickness2
         if(istat.gt.0) call inputerror(text)
      else
         thickness2=thickness1
      endif
      do j=istartset(i),iendset(i)
         if(ialset(j).gt.0) then
            indexe=ipkon(ialset(j))
            do l=1,8
               thicke(1,indexe+l)=thickness1
               thicke(2,indexe+l)=thickness2
            enddo
         else
            k=ialset(j-2)
            do
               k=k-ialset(j)
               if(k.ge.ialset(j-1)) exit
               indexe=ipkon(k)
               do l=1,8
                  thicke(1,indexe+l)=thickness1
                  thicke(2,indexe+l)=thickness2
               enddo
            enddo
         endif
      enddo
!
      call getnewline(text,textpart,istat,in,n,key)
      if((istat.lt.0).or.(key.eq.1)) return
!
!     assigning normal direction 1 for the beam
!
      indexx=-1
      read(textpart(1),'(f40.0)',iostat=istat) p(1)
      if(istat.gt.0) call inputerror(text)
      read(textpart(2),'(f40.0)',iostat=istat) p(2)
      if(istat.gt.0) call inputerror(text)
      read(textpart(3),'(f40.0)',iostat=istat) p(3)
      if(istat.gt.0) call inputerror(text)
      dd=dsqrt(p(1)*p(1)+p(2)*p(2)+p(3)*p(3))
      if(dd.lt.1.d-10) then
         write(*,*) '*ERROR in beamsections: normal in direction 1'
         write(*,*) '       has zero size'
         stop
      endif
      do j=1,3
         p(j)=p(j)/dd
      enddo
      do j=istartset(i),iendset(i)
         if(ialset(j).gt.0) then
            indexe=ipkon(ialset(j))
            do l=1,8
               if(indexx.eq.-1) then
                  indexx=ixfree
                  do m=1,3
                     xnor(indexx+m)=p(m)
                  enddo
                  ixfree=ixfree+6
               endif
               iponor(1,indexe+l)=indexx
            enddo
         else
            k=ialset(j-2)
            do
               k=k-ialset(j)
               if(k.ge.ialset(j-1)) exit
               indexe=ipkon(k)
               do l=1,8
               if(indexx.eq.-1) then
                  indexx=ixfree
                  do m=1,3
                     xnor(indexx+m)=p(m)
                  enddo
                  ixfree=ixfree+6
               endif
               iponor(1,indexe+l)=indexx
               enddo
            enddo
         endif
      enddo
!
      call getnewline(text,textpart,istat,in,n,key)
!
      return
      end

