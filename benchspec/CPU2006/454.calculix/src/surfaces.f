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
      subroutine surfaces(text,textpart,set,istartset,iendset,ialset,
     &  nset,nset_,nalset,nalset_,nk,ne,istep,istat,in,n)
!
!     reading the input deck: *SURFACE
!     TYPE=NODE is assumed
!
      implicit none
!
      character*21 set(*),noset
      character*40 textpart(16)
      character*132 text
!
      integer nset,nset_,nalset,nalset_,istep,istat,in,n,key,i,nk,ne,
     &  j,istartset(*),iendset(*),ialset(*),ipos
!
      if(istep.gt.0) then
         write(*,*) '*ERROR in surfaces: *SURFACE should be placed'
         write(*,*) '  before all step definitions'
         stop
      endif
!
      nset=nset+1
      if(nset.gt.nset_) then
         write(*,*) '*ERROR in surfaces: increase nset_'
         stop
      endif
!
!     reading the name of the set
!
      do i=2,n
         if(textpart(i)(1:5).eq.'NAME=')
     &        then
            set(nset)=textpart(i)(6:25)
            set(nset)(21:21)=' '
            ipos=index(set(nset),' ')
            set(nset)(ipos:ipos)='N'
            exit
         endif
      enddo
!
      istartset(nset)=nalset+1
      do
         call getnewline(text,textpart,istat,in,n,key)
         if((istat.lt.0).or.(key.eq.1)) return
         if(nalset+1.gt.nalset_) then
            write(*,*) 'ERROR in surfaces: increase nalset_'
            stop
         endif
!
         read(textpart(1),'(i40)',iostat=istat) ialset(nalset+1)
         if(istat.gt.0) then
            noset=textpart(1)(1:20)
            noset(21:21)=' '
            ipos=index(noset,' ')
            noset(ipos:ipos)='N'
            do i=1,nset
               if(set(i).eq.noset) then
                  do j=istartset(i),iendset(i)
                     nalset=nalset+1
                     if(nalset.gt.nalset_) then
                        write(*,*) 'ERROR in surfaces: increase nalset_'
                        stop
                     endif
                     ialset(nalset)=ialset(j)
                  enddo
                  iendset(nset)=nalset
                  exit
               endif
            enddo
            if(i.gt.nset) then
               noset(ipos:ipos)=' '
               write(*,*) '*ERROR in surfaces: node set ',noset
               write(*,*) '       does not exist'
               stop
            endif
         else
            if(ialset(nalset+1).gt.nk) then
               write(*,*) '*WARNING in surfaces: value ',
     &              ialset(nalset+1)
               write(*,*) '         in set ',set(nset),' > nk'
            else
               nalset=nalset+1
               iendset(nset)=nalset
            endif
         endif
      enddo
!
      return
      end

