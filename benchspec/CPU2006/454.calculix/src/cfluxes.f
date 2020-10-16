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
      subroutine cfluxes(text,textpart,set,istartset,iendset,
     &  ialset,nset,nodeforc,ndirforc,xforc,nforc,nforc_,iamforc,
     &  amname,nam,ntrans,trab,inotr,co,ikforc,ilforc,nk,
     &  cflux_flag,istep,istat,in,n)
!
!     reading the input deck: *CFLUX
!
      implicit none
!
      logical cflux_flag
!
      integer istartset(*),iendset(*),ialset(*),nodeforc(*),ndirforc(*),
     &  nset,nforc,nforc_,istep,istat,in,n,i,j,k,l,iforcdir,key,
     &  iamforc(*),nam,iamplitude,ntrans,inotr(2,*),ipos,ikforc(*),
     &  ilforc(*),nk
!
      real*8 xforc(*),forcval,co(3,*),trab(7,*)
!
      character*20 amplitude,amname(*)
      character*21 set(*),noset
      character*40 textpart(16)
      character*132 text
!
      iamplitude=0
!
      if(istep.lt.1) then
         write(*,*) '*ERROR in cfluxes: *CLOAD should only be used'
         write(*,*) '  within a STEP'
         stop
      endif
!
      do i=2,n
         if((textpart(i)(1:6).eq.'OP=NEW').and.(.not.cflux_flag)) then
            do j=1,nforc
               xforc(j)=0.d0
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
               write(*,*)'*ERROR in cfluxes: nonexistent amplitude'
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
         read(textpart(2),'(i40)',iostat=istat) iforcdir
         if(istat.gt.0) call inputerror(text)
         if(iforcdir.ne.0) then
            write(*,*) '*ERROR in cfluxes: nonexistent degree of '
            write(*,*) '       freedom. card image:'
            write(*,'(a132)') text
            stop
         endif
!
         if(textpart(3).eq.
     &     '                                        ') then
            forcval=0.d0
         else
            read(textpart(3),'(f40.0)',iostat=istat) forcval
            if(istat.gt.0) call inputerror(text)
         endif
!
         read(textpart(1),'(i40)',iostat=istat) l
         if(istat.eq.0) then
            if(l.gt.nk) then
               write(*,*) '*ERROR in cfluxes: node ',l
               write(*,*) '       is not defined'
               stop
            endif
            call forcadd(l,iforcdir,forcval,
     &        nodeforc,ndirforc,xforc,nforc,nforc_,iamforc,
     &        iamplitude,nam,ntrans,trab,inotr,co,ikforc,ilforc)
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
               write(*,*) '*ERROR in cfluxes: node set ',noset
               write(*,*) '  has not yet been defined. Card image:'
               write(*,'(a132)') text
               stop
            endif
            do j=istartset(i),iendset(i)
               if(ialset(j).gt.0) then
               call forcadd(ialset(j),iforcdir,forcval,
     &           nodeforc,ndirforc,xforc,nforc,nforc_,iamforc,
     &           iamplitude,nam,ntrans,trab,inotr,co,ikforc,ilforc)
               else
                  k=ialset(j-2)
                  do
                     k=k-ialset(j)
                     if(k.ge.ialset(j-1)) exit
                     call forcadd(k,iforcdir,forcval,
     &                 nodeforc,ndirforc,xforc,nforc,nforc_,
     &                 iamforc,iamplitude,nam,ntrans,trab,inotr,co,
     &                 ikforc,ilforc)
                  enddo
               endif
            enddo
         endif
      enddo
!
      return
      end

