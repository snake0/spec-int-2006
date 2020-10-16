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
      subroutine dloads(text,textpart,set,istartset,iendset,
     &  ialset,nset,nelemload,sideload,xload,nload,nload_,
     &  p1,p2,om,bodyf,ielmat,ntmat_,iamload,iamom,iambodyf,
     &  amname,nam,lakon,ne,dload_flag,istep,istat,in,n)
!
!     reading the input deck: *DLOAD
!
!     IMPORTANT: The model can have only one om (=omega square) and
!                only one acceleration vector, the density can vary
!
!
      implicit none
!
      logical dload_flag
!
      character*8 lakon(*)
!
      integer istartset(*),iendset(*),ialset(*),nelemload(2,*),
     &  ielmat(*),nset,nload,nload_,ntmat_,istep,istat,in,n,i,j,l,key,
     &  iamload(2,*),iamom,iambodyf,nam,iamplitude,ipos,ne
!
      real*8 xload(2,*),p1(3),p2(3),bodyf(3),om,
     &  xmagnitude,dd
!
      character*5 sideload(*),label
      character*20 amname(*),amplitude
      character*21 set(*),elset
      character*40 textpart(16)
      character*132 text
!
      iamplitude=0
c      iambodyf=0
c      iamom=0
!
      if(istep.lt.1) then
         write(*,*) '*ERROR in dloads: *DLOAD should only be used'
         write(*,*) '  within a STEP'
         stop
      endif
!
      do i=2,n
         if((textpart(i)(1:6).eq.'OP=NEW').and.(.not.dload_flag)) then
            do j=1,nload
               if(sideload(j)(1:1).eq.'P') then
                  xload(1,j)=0.d0
               endif
            enddo
            om=0.d0
            do j=1,3
               bodyf(j)=0.d0
            enddo
         elseif(textpart(i)(1:10).eq.'AMPLITUDE=') then
            read(textpart(i)(11:30),'(a20)') amplitude
            do j=1,nam
               if(amname(j).eq.amplitude) then
                  iamplitude=j
                  exit
               endif
            enddo
            if(j.gt.nam) then
               write(*,*)'*ERROR in dloads: nonexistent amplitude'
               write(*,*) '  Card image:'
               write(*,'(a132)') text
               stop
            endif
            iamplitude=j
         endif
      enddo
!
      do
         call getnewline(text,textpart,istat,in,n,key)
         if((istat.lt.0).or.(key.eq.1)) return
!
         read(textpart(2)(1:5),'(a5)',iostat=istat) label
         read(textpart(3),'(f40.0)',iostat=istat) xmagnitude
         if(istat.gt.0) call inputerror(text)
         if(label.eq.'CENTR') then
            do i=1,3
               read(textpart(i+3),'(f40.0)',iostat=istat) p1(i)
               if(istat.gt.0) call inputerror(text)
            enddo
            do i=1,3
               read(textpart(i+6),'(f40.0)',iostat=istat) p2(i)
               if(istat.gt.0) call inputerror(text)
            enddo
            dd=dsqrt(p2(1)**2+p2(2)**2+p2(3)**2)
            do i=1,3
               p2(i)=p2(i)/dd
            enddo
         elseif(label.eq.'GRAV ') then
            do i=1,3
               read(textpart(i+3),'(f40.0)',iostat=istat) bodyf(i)
               if(istat.gt.0) call inputerror(text)
            enddo
            do i=1,3
               bodyf(i)=bodyf(i)*xmagnitude
            enddo
            if(nam.gt.0) iambodyf=iamplitude
         elseif(((label(1:2).ne.'P1').and.(label(1:2).ne.'P2').and.
     &           (label(1:2).ne.'P3').and.(label(1:2).ne.'P4').and.
     &           (label(1:2).ne.'P5').and.(label(1:2).ne.'P6').and.
     &           (label(1:2).ne.'P ')).or.
     &          ((label(3:4).ne.'  ').and.(label(3:4).ne.'NU'))) then
            call inputerror(text)
         endif
!
         read(textpart(1),'(i40)',iostat=istat) l
         if(istat.eq.0) then
            if(l.gt.ne) then
               write(*,*) '*ERROR in dloads: element ',l
               write(*,*) '       is not defined'
               stop
            endif
            if(label.eq.'CENTR') then
               om=xmagnitude
               if(nam.gt.0) iamom=iamplitude
            elseif(label.ne.'GRAV ') then
               if((lakon(l)(1:2).eq.'CP').or.
     &              (lakon(l)(2:2).eq.'A').or.
     &              (lakon(l)(7:7).eq.'E').or.
     &              (lakon(l)(7:7).eq.'S').or.
     &              (lakon(l)(7:7).eq.'A')) then
                  if(label(1:2).eq.'P1') then
                     label(1:2)='P3'
                  elseif(label(1:2).eq.'P2') then
                     label(1:2)='P4'
                  elseif(label(1:2).eq.'P3') then
                     label(1:2)='P5'
                  elseif(label(1:2).eq.'P4') then
                     label(1:2)='P6'
                  endif
               elseif((lakon(l)(1:1).eq.'B').or.
     &                (lakon(l)(7:7).eq.'B')) then
                  if(label(1:2).eq.'P2') label(1:2)='P5'
               elseif((lakon(l)(1:1).eq.'S').or.
     &                (lakon(l)(7:7).eq.'L')) then
                  label(1:2)='P1'
               endif
               call loadadd(l,label,xmagnitude,nelemload,sideload,
     &                      xload,nload,nload_,iamload,iamplitude,
     &                      nam)
            endif
         else
            read(textpart(1)(1:20),'(a20)',iostat=istat) elset
            elset(21:21)=' '
            ipos=index(elset,' ')
            elset(ipos:ipos)='E'
            do i=1,nset
               if(set(i).eq.elset) exit
            enddo
            if(i.gt.nset) then
               elset(ipos:ipos)=' '
               write(*,*) '*ERROR in dloads: element set ',elset
               write(*,*) '       has not yet been defined. Card image:'
               write(*,'(a132)') text
               stop
            endif
!
            if(label.eq.'CENTR') then
               om=xmagnitude
               if(nam.gt.0) iamom=iamplitude
            elseif(label.ne.'GRAV ') then
               l=ialset(istartset(i))
               if((lakon(l)(1:2).eq.'CP').or.
     &              (lakon(l)(2:2).eq.'A').or.
     &              (lakon(l)(7:7).eq.'E').or.
     &              (lakon(l)(7:7).eq.'S').or.
     &              (lakon(l)(7:7).eq.'A')) then
                  if(label(1:2).eq.'P1') then
                     label(1:2)='P3'
                  elseif(label(1:2).eq.'P2') then
                     label(1:2)='P4'
                  elseif(label(1:2).eq.'P3') then
                     label(1:2)='P5'
                  elseif(label(1:2).eq.'P4') then
                     label(1:2)='P6'
                  endif
               elseif((lakon(l)(1:1).eq.'B').or.
     &                 (lakon(l)(7:7).eq.'B')) then
                  if(label(1:2).eq.'P2') label(1:2)='P5'
               elseif((lakon(l)(1:1).eq.'S').or.
     &                 (lakon(l)(7:7).eq.'L')) then
                  label(1:2)='P1'
               endif
            endif
!
            do j=istartset(i),iendset(i)
               if(ialset(j).gt.0) then
                  l=ialset(j)
                  if((label.ne.'CENTR').and.(label.ne.'GRAV ')) then
                     call loadadd(l,label,xmagnitude,nelemload,sideload,
     &                            xload,nload,nload_,iamload,iamplitude,
     &                            nam)
                  endif
               else
                  l=ialset(j-2)
                  do
                     l=l-ialset(j)
                     if(l.ge.ialset(j-1)) exit
                     if((label.ne.'CENTR').and.(label.ne.'GRAV ')) then
                        call loadadd(l,label,xmagnitude,nelemload,
     &                               sideload,xload,nload,nload_,
     &                               iamload,iamplitude,nam)
                     endif
                  enddo
               endif
            enddo
         endif
      enddo
!
      return
      end

