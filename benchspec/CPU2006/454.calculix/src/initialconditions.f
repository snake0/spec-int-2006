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
      subroutine initialconditions(text,textpart,set,istartset,iendset,
     &  ialset,nset,t0,t1,prestr,iprestr,ithermal,veold,inoelfree,nk_,
     &  istep,istat,in,n)
!
!     reading the input deck: *INITIAL CONDITIONS
!
      implicit none
!
      integer istartset(*),iendset(*),ialset(*),nset,iprestr,ithermal,
     &  istep,istat,in,n,i,j,k,l,ii,key,idir,ipos,inoelfree,nk_
!
      real*8 t0(*),t1(*),beta(6),prestr(6,*),veold(0:3,*),temperature,
     &  velocity,tempgrad1,tempgrad2
!
      character*21 set(*),noset,elset
      character*40 textpart(16)
      character*132 text
!
      if(istep.gt.0) then
         write(*,*) '*ERROR in initialconditions: *INITIAL CONDITIONS'
         write(*,*) '  should be placed before all step definitions'
         stop
      endif
!
      do i=2,n
         if(textpart(i).eq.'TYPE=TEMPERATURE') then
!
            ithermal=1
            do
               call getnewline(text,textpart,istat,in,n,key)
               if((istat.lt.0).or.(key.eq.1)) return
               read(textpart(2),'(f40.0)',iostat=istat) temperature
               if(istat.gt.0) call inputerror(text)
               temperature=1.d-6*int(1.d6*temperature+0.5d0)
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
                  t0(l)=temperature
                  t1(l)=temperature
                  if(inoelfree.ne.0) then
                     t0(nk_+l)=tempgrad1
                     t0(2*nk_+l)=tempgrad2
                     t1(nk_+l)=tempgrad1
                     t1(2*nk_+l)=tempgrad2
                  endif
               else
                  read(textpart(1)(1:20),'(a20)',iostat=istat) noset
                  noset(21:21)=' '
                  ipos=index(noset,' ')
                  noset(ipos:ipos)='N'
                  do ii=1,nset
                     if(set(ii).eq.noset) exit
                  enddo
                  if(ii.gt.nset) then
                     noset(ipos:ipos)=' '
                     write(*,*) '*ERROR in initialconditions: node set '
     &                       ,noset
                     write(*,*)'  has not yet been defined. Card image:'
                     write(*,'(a132)') text
                     stop
                  endif
                  do j=istartset(ii),iendset(ii)
                     if(ialset(j).gt.0) then
                        t0(ialset(j))=temperature
                        t1(ialset(j))=temperature
                        if(inoelfree.ne.0) then
                           t0(nk_+ialset(j))=tempgrad1
                           t0(2*nk_+ialset(j))=tempgrad2
                           t1(nk_+ialset(j))=tempgrad1
                           t1(2*nk_+ialset(j))=tempgrad2
                        endif
                     else
                        k=ialset(j-2)
                        do
                           k=k-ialset(j)
                           if(k.ge.ialset(j-1)) exit
                           t0(k)=temperature
                           t1(k)=temperature
                           if(inoelfree.ne.0) then
                              t0(nk_+k)=tempgrad1
                              t0(2*nk_+k)=tempgrad2
                              t1(nk_+k)=tempgrad1
                              t1(2*nk_+k)=tempgrad2
                           endif
                        enddo
                     endif
                  enddo
               endif
            enddo
            return
         elseif(textpart(i).eq.'TYPE=STRESS') then
!
            iprestr=1
            do
               call getnewline(text,textpart,istat,in,n,key)
               if((istat.lt.0).or.(key.eq.1)) return
               do j=1,6
                  read(textpart(j+1),'(f40.0)',iostat=istat) beta(j)
                  if(istat.gt.0) call inputerror(text)
               enddo
               read(textpart(1),'(i40)',iostat=istat) l
               if(istat.eq.0) then
                  do j=1,6
                     prestr(j,l)=beta(j)
                  enddo
               else
                  read(textpart(1)(1:20),'(a20)',iostat=istat) elset
                  elset(21:21)=' '
                  ipos=index(elset,' ')
                  elset(ipos:ipos)='E'
                  do ii=1,nset
                     if(set(ii).eq.elset) exit
                  enddo
                  if(ii.gt.nset) then
                     elset(ipos:ipos)=' '
                     write(*,*)'*ERROR in initialconditions: element set
     & ',elset
                     write(*,*)'  has not yet been defined. Card image:'
                     write(*,'(a132)') text
                     stop
                  endif
                  do j=istartset(ii),iendset(ii)
                     if(ialset(j).gt.0) then
                        do l=1,6
                           prestr(l,ialset(j))=beta(l)
                        enddo
                     else
                        k=ialset(j-2)
                        do
                           k=k-ialset(j)
                           if(k.ge.ialset(j-1)) exit
                           do l=1,6
                              prestr(l,k)=beta(l)
                           enddo
                        enddo
                     endif
                  enddo
               endif
            enddo
            return
         elseif(textpart(i).eq.'TYPE=VELOCITY') then
!
            do
               call getnewline(text,textpart,istat,in,n,key)
               if((istat.lt.0).or.(key.eq.1)) return
               read(textpart(2),'(i40)',iostat=istat) idir
               if(istat.gt.0) call inputerror(text)
               read(textpart(3),'(f40.0)',iostat=istat) velocity
               if(istat.gt.0) call inputerror(text)
               read(textpart(1),'(i40)',iostat=istat) l
               if(istat.eq.0) then
                  veold(idir,l)=velocity
               else
                  read(textpart(1)(1:20),'(a20)',iostat=istat) noset
                  noset(21:21)=' '
                  ipos=index(noset,' ')
                  noset(ipos:ipos)='N'
                  do ii=1,nset
                     if(set(ii).eq.noset) exit
                  enddo
                  if(ii.gt.nset) then
                     noset(ipos:ipos)=' '
                     write(*,*) '*ERROR in initialconditions: node set '
     &                 ,noset
                     write(*,*)'  has not yet been defined. Card image:'
                     write(*,'(a132)') text
                     stop
                  endif
                  do j=istartset(ii),iendset(ii)
                     if(ialset(j).gt.0) then
                        veold(idir,ialset(j))=velocity
                     else
                        k=ialset(j-2)
                        do
                           k=k-ialset(j)
                           if(k.ge.ialset(j-1)) exit
                           veold(idir,k)=velocity
                        enddo
                     endif
                  enddo
               endif
            enddo
            return
         endif
      enddo
!
      write(*,*) '*ERROR in initialconditions: unknown type'
      write(*,*) '  card image:'
      write(*,'(a132)') text
!
      return
      end

