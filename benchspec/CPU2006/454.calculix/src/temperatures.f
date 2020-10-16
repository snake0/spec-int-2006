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
      subroutine temperatures(text,textpart,set,istartset,iendset,
     &  ialset,nset,t0,t1,nk,ithermal,iamt1,amname,nam,inoelfree,nk_,
     &  nmethod,temp_flag,istep,istat,in,n)
!
!     reading the input deck: *TEMPERATURE
!
      implicit none
!
      logical temp_flag
!
      integer istartset(*),iendset(*),ialset(*),iamt1(*),nmethod
!
      integer nset,nk,ithermal,istep,istat,in,n,key,i,j,k,l,nam,
     &  iamplitude,ipos,inoelfree,nk_
!
      real*8 t0(*),t1(*)
!
      real*8 temperature,tempgrad1,tempgrad2
!
      character*20 amname(*),amplitude
      character*21 set(*),noset
      character*40 textpart(16)
      character*132 text
!
      iamplitude=0
!
      if(nmethod.eq.3) then
         write(*,*) '*ERROR in temperatures: temperature'
         write(*,*) '       loading is not allowed in a linear'
         write(*,*) '       buckling step; perform a static'
         write(*,*) '       nonlinear calculation instead'
         stop
      endif
!
      if(istep.lt.1) then
         write(*,*) '*ERROR in temperatures: *TEMPERATURE'
         write(*,*) '  should only be used within a STEP'
         stop
      endif
!
      if(ithermal.ne.1) then
         write(*,*) '*ERROR in temperatures: a *TEMPERATURE'
         write(*,*) '  card is detected but no thermal'
         write(*,*) '  *INITIAL CONDITIONS are given'
         stop
      endif
!
      do i=2,n
         if((textpart(i).eq.'OP=NEW').and.(.not.temp_flag)) then
            do j=1,nk
               t1(j)=t0(j)
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
               write(*,*)'*ERROR in temperatures: nonexistent amplitude'
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
         read(textpart(2),'(f40.0)',iostat=istat) temperature
         if(istat.gt.0) call inputerror(text)
!
         if(inoelfree.ne.0) then
            tempgrad1=0.d0
            tempgrad2=0.d0
            if(n.gt.2) then
               read(textpart(3),'(f40.0)',iostat=istat) tempgrad1
               if(istat.gt.0) call inputerror(text)
            endif
            if(n.gt.3) then
               read(textpart(4),'(f40.0)',iostat=istat) tempgrad2
               if(istat.gt.0) call inputerror(text)
            endif
         endif
!            
         read(textpart(1),'(i40)',iostat=istat) l
         if(istat.eq.0) then
            t1(l)=temperature
            if(nam.gt.0) iamt1(l)=iamplitude
            if(inoelfree.ne.0) then
               t1(nk_+l)=tempgrad1
               t1(2*nk_+l)=tempgrad2
            endif
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
               write(*,*) '*ERROR in temperatures: node set ',noset
               write(*,*) '  has not yet been defined. Card image:'
               write(*,'(a132)') text
               stop
            endif
            do j=istartset(i),iendset(i)
               if(ialset(j).gt.0) then
                  t1(ialset(j))=temperature
                  if(nam.gt.0) iamt1(ialset(j))=iamplitude
                  if(inoelfree.ne.0) then
                     t1(nk_+ialset(j))=tempgrad1
                     t1(2*nk_+ialset(j))=tempgrad2
                  endif
               else
                  k=ialset(j-2)
                  do
                     k=k-ialset(j)
                     if(k.ge.ialset(j-1)) exit
                     t1(k)=temperature
                     if(nam.gt.0) iamt1(k)=iamplitude
                     if(inoelfree.ne.0) then
                        t1(nk_+k)=tempgrad1
                        t1(2*nk_+k)=tempgrad2
                     endif
                  enddo
               endif
            enddo
         endif
      enddo
!
      return
      end

