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
      subroutine films(text,textpart,set,istartset,iendset,
     &  ialset,nset,nelemload,sideload,xload,nload,nload_,
     &  ielmat,ntmat_,iamload,
     &  amname,nam,lakon,ne,flow_flag,
     &  istep,istat,in,n)
!
!     reading the input deck: *FILM
!
      implicit none
!
      logical flow_flag
!
      character*5 sideload(*),label
      character*8 lakon(*)
      character*20 amname(*),amplitude
      character*21 set(*),elset
      character*40 textpart(16)
      character*132 text
!
      integer istartset(*),iendset(*),ialset(*),nelemload(2,*),
     &  ielmat(*),nset,nload,nload_,ntmat_,istep,istat,in,n,i,j,l,key,
     &  iamload(2,*),nam,iamptemp,ipos,ne,node,iampfilm
!
      real*8 xload(2,*),xmagfilm,xmagtemp
!
      iamptemp=0
      iampfilm=0
!
      if(istep.lt.1) then
         write(*,*) '*ERROR in films: *FILM should only be used'
         write(*,*) '  within a STEP'
         stop
      endif
!
      do i=2,n
         if((textpart(i)(1:6).eq.'OP=NEW').and.(.not.flow_flag)) then
            do j=1,nload
               if(sideload(j)(1:1).eq.'F') then
                  xload(1,j)=0.d0
               endif
            enddo
         elseif(textpart(i)(1:10).eq.'AMPLITUDE=') then
            read(textpart(i)(11:30),'(a20)') amplitude
            do j=1,nam
               if(amname(j).eq.amplitude) then
                  iamptemp=j
                  exit
               endif
            enddo
            if(j.gt.nam) then
               write(*,*)'*ERROR in films: nonexistent amplitude'
               write(*,*) '  Card image:'
               write(*,'(a132)') text
               stop
            endif
            iamptemp=j
         elseif(textpart(i)(1:14).eq.'FILMAMPLITUDE=') then
            read(textpart(i)(15:34),'(a20)') amplitude
            do j=1,nam
               if(amname(j).eq.amplitude) then
                  iampfilm=j
                  exit
               endif
            enddo
            if(j.gt.nam) then
               write(*,*)'*ERROR in films: nonexistent amplitude'
               write(*,*) '  Card image:'
               write(*,'(a132)') text
               stop
            endif
            iampfilm=j
         endif
      enddo
!
      do
         call getnewline(text,textpart,istat,in,n,key)
         if((istat.lt.0).or.(key.eq.1)) return
!
         read(textpart(2)(1:5),'(a5)',iostat=istat) label
!
!        reference temperature and film coefficient
!        (for non uniform loading: use user routine film.f)
!
         if((label(3:4).ne.'NU').and.(label(3:4).ne.'FC')) then
            read(textpart(3),'(f40.0)',iostat=istat) xmagtemp
            if(istat.gt.0) call inputerror(text)
            read(textpart(4),'(f40.0)',iostat=istat) xmagfilm
            if(istat.gt.0) call inputerror(text)
            node=0
!
!        for forced convection: reference node and, optionally,
!        a film coefficient (else use user routine film.f)
!
         elseif(label(3:4).eq.'FC') then
            read(textpart(3),'(i40)',iostat=istat) node
            if(istat.gt.0) call inputerror(text)
            xmagtemp=0.d0
            read(textpart(4),'(f40.0)',iostat=istat) xmagfilm
            if(istat.gt.0) xmagfilm=-1.d0
         endif
         if(((label(1:2).ne.'F1').and.(label(1:2).ne.'F2').and.
     &       (label(1:2).ne.'F3').and.(label(1:2).ne.'F4').and.
     &       (label(1:2).ne.'F5').and.(label(1:2).ne.'F6')).or.
     &      ((label(3:4).ne.'  ').and.(label(3:4).ne.'NU').and.
     &       (label(3:4).ne.'FC'))) then
            call inputerror(text)
         endif
!
         read(textpart(1),'(i40)',iostat=istat) l
         if(istat.eq.0) then
            if(l.gt.ne) then
               write(*,*) '*ERROR in films: element ',l
               write(*,*) '       is not defined'
               stop
            endif
!
            if((lakon(l)(1:2).eq.'CP').or.
     &           (lakon(l)(2:2).eq.'A').or.
     &           (lakon(l)(7:7).eq.'E').or.
     &           (lakon(l)(7:7).eq.'S').or.
     &           (lakon(l)(7:7).eq.'A')) then
               if(label(1:2).eq.'F1') then
                  label(1:2)='F3'
               elseif(label(1:2).eq.'F2') then
                  label(1:2)='F4'
               elseif(label(1:2).eq.'F3') then
                  label(1:2)='F5'
               elseif(label(1:2).eq.'F4') then
                  label(1:2)='F6'
               endif
            elseif((lakon(l)(1:1).eq.'B').or.
     &              (lakon(l)(7:7).eq.'B')) then
               if(label(1:2).eq.'F2') label(1:2)='F5'
            elseif((lakon(l)(1:1).eq.'S').or.
     &              (lakon(l)(7:7).eq.'L')) then
               label(1:2)='F1'
            endif
            call loadaddt(l,label,xmagfilm,xmagtemp,nelemload,sideload,
     &           xload,nload,nload_,iamload,
     &           iamptemp,iampfilm,nam,node)
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
               write(*,*) '*ERROR in films: element set ',elset
               write(*,*) '       has not yet been defined. Card image:'
               write(*,'(a132)') text
               stop
            endif
!
            l=ialset(istartset(i))
            if((lakon(l)(1:2).eq.'CP').or.
     &           (lakon(l)(2:2).eq.'A').or.
     &           (lakon(l)(7:7).eq.'E').or.
     &           (lakon(l)(7:7).eq.'S').or.
     &           (lakon(l)(7:7).eq.'A')) then
               if(label(1:2).eq.'F1') then
                  label(1:2)='F3'
               elseif(label(1:2).eq.'F2') then
                  label(1:2)='F4'
               elseif(label(1:2).eq.'F3') then
                  label(1:2)='F5'
               elseif(label(1:2).eq.'F4') then
                  label(1:2)='F6'
               endif
            elseif((lakon(l)(1:1).eq.'B').or.
     &              (lakon(l)(7:7).eq.'B')) then
               if(label(1:2).eq.'F2') label(1:2)='F5'
            elseif((lakon(l)(1:1).eq.'S').or.
     &              (lakon(l)(7:7).eq.'L')) then
               label(1:2)='F1'
            endif
!
            do j=istartset(i),iendset(i)
               if(ialset(j).gt.0) then
                  l=ialset(j)
                  call loadaddt(l,label,xmagfilm,xmagtemp,nelemload,
     &                 sideload,xload,nload,nload_,iamload,
     &                 iamptemp,iampfilm,nam,node)
               else
                  l=ialset(j-2)
                  do
                     l=l-ialset(j)
                     if(l.ge.ialset(j-1)) exit
                     call loadaddt(l,label,xmagfilm,xmagtemp,nelemload,
     &                    sideload,xload,nload,nload_,iamload,
     &                    iamptemp,iampfilm,nam,node)
                  enddo
               endif
            enddo
         endif
      enddo
!
      return
      end

