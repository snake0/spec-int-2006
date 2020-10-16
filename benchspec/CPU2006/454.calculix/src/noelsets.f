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
      subroutine noelsets(text,textpart,set,istartset,iendset,ialset,
     &  nset,nset_,nalset,nalset_,nk,ne,istep,istat,in,n)
!
!     reading the input deck: *NSET and *ELSET
!
      implicit none
!
      integer istartset(*),iendset(*),ialset(*)
!
      logical igen
!
      character*21 set(*),noelset
      character*40 textpart(16)
      character*132 text
!
      integer nset,nset_,nalset,nalset_,istep,istat,in,n,key,i,nk,ne,
     &  kode,ipos,j,k,m,iset,nn
!
      if(istep.gt.0) then
         write(*,*) '*ERROR in noelsets: *NSET/*ELSET should be placed'
         write(*,*) '  before all step definitions'
         stop
      endif
!
!     reading the name of the set
!
      if((textpart(1)(1:5).eq.'*nset').or.(textpart(1)(1:5).eq.'*NSET'))
     &  then
         noelset=textpart(2)(6:25)
         noelset(21:21)=' '
         ipos=index(noelset,' ')
         noelset(ipos:ipos)='N'
         kode=0
      else
         noelset=textpart(2)(7:26)
         noelset(21:21)=' '
         ipos=index(noelset,' ')
         noelset(ipos:ipos)='E'
         kode=1
      endif
!
!     check whether new set or old set 
!
      do iset=1,nset
         if(set(iset).eq.noelset) then
!
!                 existent set
!
            if(iendset(iset).eq.nalset) then
               exit
            else
!
!                    rearranging set information towards the end
!
               nn=iendset(iset)-istartset(iset)+1
               if(nalset+nn.gt.nalset_) then
                  write(*,*)'*ERROR in noelsets: increase nalset_'
                  stop
               endif
               do k=1,nn
                  ialset(nalset+k)=ialset(istartset(iset)+k-1)
               enddo
               do k=istartset(iset),nalset
                  ialset(k)=ialset(k+nn)
               enddo
               do k=1,nset
                  if(istartset(k).gt.iendset(iset)) then
                     istartset(k)=istartset(k)-nn
                     iendset(k)=iendset(k)-nn
                  endif
               enddo
               istartset(iset)=nalset-nn+1
               iendset(iset)=nalset
               exit
            endif
         endif
      enddo
      if(iset.gt.nset) then
         nset=nset+1
         if(nset.gt.nset_) then
            write(*,*) '*ERROR in noelsets: increase nset_'
            stop
         endif
         set(nset)=noelset
         istartset(nset)=nalset+1
         iset=nset
      endif
!
      if((n.gt.2).and.(textpart(3)(1:8).eq.'GENERATE')) then
         igen=.true.
      else
         igen=.false.
      endif
!
      do
         call getnewline(text,textpart,istat,in,n,key)
         if((istat.lt.0).or.(key.eq.1)) return
         if(igen) n=3
         if(nalset+n.gt.nalset_) then
            write(*,*) 'ERROR in noelsets: increase nalset_'
            stop
         endif
!
         if(igen) then
            if(textpart(3).eq.
     &        '                                        ')
     &         textpart(3)='1                                       '
            do i=1,3
               read(textpart(i),'(i40)',iostat=istat) ialset(nalset+i)
               if(istat.gt.0) call inputerror(text)
            enddo
            if(kode.eq.0) then
               if(ialset(nalset+1).gt.nk) then
                  write(*,*) '*ERROR in noelsets: starting value in'
                  write(*,*) '       set ',set(iset),' > nk'
                  stop
               elseif(ialset(nalset+2).gt.nk) then
                  write(*,*) '*WARNING in noelsets: end value in'
                  write(*,*) '         set ',set(iset),' > nk;'
                  write(*,*) '         replaced by nk'
                  ialset(nalset+2)=nk
               elseif(ialset(nalset+3).le.0) then
                  write(*,*) '*ERROR in noelsets: increment in'
                  write(*,*) '       set ',set(iset),' <=0'
                  stop
               endif
            else
               if(ialset(nalset+1).gt.ne) then
                  write(*,*) '*ERROR in noelsets: starting value in'
                  write(*,*) '       set ',set(iset),' > ne'
                  stop
               elseif(ialset(nalset+2).gt.ne) then
                  write(*,*) '*WARNING in noelsets: end value in'
                  write(*,*) '         set ',set(iset),' > ne;'
                  write(*,*) '         replaced by ne'
                  ialset(nalset+2)=nk
               elseif(ialset(nalset+3).le.0) then
                  write(*,*) '*ERROR in noelsets: increment in'
                  write(*,*) '       set ',set(iset),' <=0'
                  stop
               endif
            endif
            ialset(nalset+3)=-ialset(nalset+3)
            nalset=nalset+3
            iendset(iset)=nalset
         else
            do i=1,n
               read(textpart(i),'(i40)',iostat=istat) ialset(nalset+1)
               if(istat.gt.0) then
!
!                 set name
!
                  noelset=textpart(i)(1:20)
                  noelset(21:21)=' '
                  ipos=index(noelset,' ')
                  if(kode.eq.0) then
                     noelset(ipos:ipos)='N'
                  else
                     noelset(ipos:ipos)='E'
                  endif
                  do j=1,nset
                     if(j.eq.iset)cycle
                     if(noelset.eq.set(j)) then
                        m=iendset(j)-istartset(j)+1
                        do k=1,m
                           ialset(nalset+k)=ialset(istartset(j)+k-1)
                        enddo
                        nalset=nalset+m
                        exit
                     endif
                  enddo
                  if(noelset.ne.set(j)) then
                     noelset(ipos:ipos)=' '
                     if(kode.eq.0) then
                        write(*,*) '*ERROR in noelsets: node set ',
     &                    noelset
                     else
                        write(*,*) '*ERROR in noelsets: element set ',
     &                    noelset
                     endif
                     write(*,*) '       has not been defined yet'
                     stop
                  endif
               else
!
!                 node or element number
!                
                  if(kode.eq.0) then
                     if(ialset(nalset+1).gt.nk) then
                        write(*,*) '*WARNING in noelsets: value ',
     &                       ialset(nalset+1)
                        write(*,*) '         in set ',set(iset),' > nk'
                     else
                        nalset=nalset+1
                     endif
                  else
                     if(ialset(nalset+1).gt.ne) then
                        write(*,*) '*WARNING in noelsets: value ',
     &                       ialset(nalset+1)
                        write(*,*) '         in set ',set(iset),' > ne'
                     else
                        nalset=nalset+1
                     endif
                  endif
               endif
            enddo
            iendset(iset)=nalset
         endif
      enddo
!
      return
      end

