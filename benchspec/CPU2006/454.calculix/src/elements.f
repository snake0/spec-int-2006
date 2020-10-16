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
      subroutine elements(text,textpart,kon,ipkon,lakon,nkon,ne,ne_,
     &  set,istartset,iendset,ialset,nset,nset_,nalset,nalset_,mint_,
     &  ixfree,iponor,xnor,istep,istat,in,n)
!
!     reading the input deck: *ELEMENT
!
      implicit none
!
      character*8 lakon(*),label
      character*21 set(*),elset
      character*40 textpart(16)
      character*132 text
!
      integer kon(*),istartset(*),iendset(*),ialset(*),ne,ne_,nset,
     &  nset_,nalset,nalset_,istep,istat,in,n,key,i,ielset,js,k,nn,
     &  nteller,j,ipkon(*),nkon,nope,indexe,mint_,ipos,indexy,ixfree,
     &  iponor(2,*),nopeexp
!
      real*8 xnor(*)
!
      if(istep.gt.0) then
         write(*,*) '*ERROR in elements: *ELEMENT should be placed'
         write(*,*) '  before all step definitions'
         stop
      endif
!
      indexy=-1
      ielset=0
!
      label='        '
!
!     checking for set definition
!      
      loop: do i=2,n
         if(textpart(i)(1:6).eq.'ELSET=') then
            elset=textpart(i)(7:26)
            elset(21:21)=' '
            ipos=index(elset,' ')
            elset(ipos:ipos)='E'
            ielset=1
            do js=1,nset
               if(set(js).eq.elset) then
!
!                 existent set
!
                  if(iendset(js).eq.nalset) then
                     exit loop
                  else
!
!                    rearranging set information towards the end
!
                     nn=iendset(js)-istartset(js)+1
                     if(nalset+nn.gt.nalset_) then
                        write(*,*)'*ERROR in elements: increase nalset_'
                        stop
                     endif
                     do k=1,nn
                        ialset(nalset+k)=ialset(istartset(js)+k-1)
                     enddo
                     do k=istartset(js),nalset
                        ialset(k)=ialset(k+nn)
                     enddo
                     do k=1,nset
                        if(istartset(k).gt.iendset(js)) then
                           istartset(k)=istartset(k)-nn
                           iendset(k)=iendset(k)-nn
                        endif
                     enddo
                     istartset(js)=nalset-nn+1
                     iendset(js)=nalset
                     cycle loop
                  endif
               endif
            enddo
!
!           new set
!
            nset=nset+1
            if(nset.gt.nset_) then
               write(*,*) '*ERROR in elements: increase nset_'
               stop
            endif
            js=nset
            istartset(js)=nalset+1
            iendset(js)=nalset
            set(js)=elset
            cycle
         elseif(textpart(i)(1:5).eq.'TYPE=') then
            read(textpart(i)(6:11),'(a8)') label
            if((label.eq.'C3D20   ').or.
     &        (label.eq.'CPE8    ').or.
     &        (label.eq.'CPS8    ').or.
     &        (label.eq.'CAX8    ').or.
     &        (label.eq.'S8      ').or.
     &        (label.eq.'B32     ')) then
               mint_=max(mint_,27)
            elseif((label.eq.'C3D20R  ').or.
     &             (label.eq.'C3D8    ').or.
     &             (label.eq.'CPE8R   ').or.
     &             (label.eq.'CPS8R   ').or.
     &             (label.eq.'CAX8R   ').or.
     &             (label.eq.'S8R     ').or.
     &             (label.eq.'B32R    ')) then
               mint_=max(mint_,8)
            elseif(label.eq.'C3D8R   ') then
               mint_=max(mint_,1)
            elseif(label.eq.'C3D10   ') then
               mint_=max(mint_,4)
            elseif(label.eq.'C3D4    ') then
               mint_=max(mint_,1)
            elseif(label.eq.'C3D15   ') then
               mint_=max(mint_,9)
            elseif(label.eq.'C3D6    ') then
               mint_=max(mint_,2)
            else
               write(*,*) '*ERROR in elements:'
               write(*,*) label,' is an unknown element type'
               stop
            endif
         endif
      enddo loop
!
      if(label.eq.'        ') then
         write(*,*) '*ERROR in elements: element type is lacking'
         write(*,*) '       card image:'
         write(*,'(a132)') text
         stop
      endif
!
!     nope is the number of nodes per element as defined in the input
!     deck, nopeexp is the number of nodes per element after expansion
!     (only applicable to 1D and 2D elements such as beams, shells..)
!
      if(label(4:5).eq.'20') then
         nope=20
         nopeexp=20
      elseif((label(1:2).eq.'CP').or.
     &        (label(1:2).eq.'CA').or.(label(1:1).eq.'S')) then
         nope=8
         nopeexp=20
      elseif(label(1:1).eq.'B') then
         nope=3
         nopeexp=20
      elseif(label(4:4).eq.'8') then
         nope=8
         nopeexp=8
      elseif(label(4:5).eq.'10') then
         nope=10
         nopeexp=10
      elseif(label(4:4).eq.'4') then
         nope=4
         nopeexp=4
      elseif(label(4:5).eq.'15') then
         nope=15
         nopeexp=15
      elseif(label(4:4).eq.'6') then
         nope=6
         nopeexp=6
      endif
!
      do
         call getnewline(text,textpart,istat,in,n,key)
         if((istat.lt.0).or.(key.eq.1)) return
         read(textpart(1),'(i40)',iostat=istat) i
         if(istat.gt.0) call inputerror(text)
         if(i.gt.ne_) then
            write(*,*) '*ERROR in elements: increase ne_'
            stop
         endif
!
!        check whether element was already defined
!
         if(ipkon(i).ne.-1) then
            write(*,*) '*ERROR in elements: element',i
            write(*,*) '       is already defined'
            write(*,*) '       card image:'
            write(*,'(a132)') text
         endif
!            
!        new element
!
         ipkon(i)=nkon
         lakon(i)=label
         indexe=nkon
!
         nkon=nkon+nopeexp
!
         do j=2,n
            read(textpart(j),'(i40)',iostat=istat) kon(indexe+j-1)
            if(istat.gt.0) call inputerror(text)
         enddo
         nteller=n-1
         if(nteller.lt.nope) then
            do
               call getnewline(text,textpart,istat,in,n,key)
               if((istat.lt.0).or.(key.eq.1)) then
                  write(*,*) '*ERROR in elements: element definition'
                  write(*,*) '       incomplete for element ',i
                  stop
               endif
               if(nteller+n.gt.nope) n=nope-nteller
               do j=1,n
                  read(textpart(j),'(i40)',iostat=istat) 
     &                 kon(indexe+nteller+j)
                  if(istat.gt.0) call inputerror(text)
               enddo
               nteller=nteller+n
               if(nteller.eq.nope) exit
            enddo
         endif
         ne=max(ne,i)
!
!        assigning element to set
!
         if(ielset.eq.1) then
            if(nalset+1.gt.nalset_) then
               write(*,*) '*ERROR in elements: increase nalset_'
               stop
            endif
            nalset=nalset+1
            ialset(nalset)=i
            iendset(js)=nalset
         endif
!
!        for plane stress, plane strain and axisymmetric elements:
!        define the normal
!
         if((label(1:2).eq.'CP').or.(label(1:2).eq.'CA')) then
            if(indexy.eq.-1) then
               indexy=ixfree
               xnor(indexy+1)=0.d0
               xnor(indexy+2)=0.d0
               xnor(indexy+3)=1.d0
               ixfree=ixfree+3
            endif
            do j=1,nope
               iponor(1,indexe+j)=indexy
            enddo
         endif
!
      enddo
!
      return
      end







