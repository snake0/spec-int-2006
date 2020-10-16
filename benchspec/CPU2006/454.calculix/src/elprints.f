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
      subroutine elprints(text,textpart,set,istartset,iendset,ialset,
     &  nset,nset_,nalset,nelemprint,neprint,neprint_,jout,noelplab,
     &  nmethod,elprint_flag,istep,istat,in,n)
!
!     reading the *NODE PRINT cards in the input deck
!
      implicit none
!
      logical elprint_flag
!
      integer istartset(*),iendset(*),ialset(*),nelemprint(*),nset,
     &  nset_,nalset,neprint,neprint_,istep,istat,in,n,i,ii,j,l,key,
     &  jout,joutl,ipos,nmethod
!
      character*4 noelplab(*)
      character*21 set(*),elset
      character*40 textpart(16)
      character*132 text
!
      if(istep.lt.1) then
         write(*,*) '*ERROR in elprints: *EL PRINT should only be'
         write(*,*) '  used within a *STEP definition'
         stop
      endif
!
!     reset the element print requests
!
      if(.not.elprint_flag) then
         noelplab(3)='    '
         noelplab(4)='    '
         noelplab(6)='    '
         noelplab(7)='    '
         noelplab(8)='    '
      endif
!
      jout=max(jout,1)
!
      do ii=2,n
        if(textpart(ii)(1:6).eq.'ELSET=') then
          elset=textpart(ii)(7:26)
          elset(21:21)=' '
          ipos=index(elset,' ')
          elset(ipos:ipos)='E'
          do i=1,nset
            if(set(i).eq.elset) then
              if(ialset(iendset(i)).ne.-1) then
                do j=istartset(i),iendset(i)
                  neprint=neprint+1
                  if(neprint.gt.neprint_) then
                    write(*,*)'*ERROR in elprints: increase neprint_'
                    stop
                  endif
                  nelemprint(neprint)=ialset(j)
                enddo
              else
                do l=istartset(i),iendset(i),3
                  do j=ialset(l),ialset(l+1)
                    neprint=neprint+1
                    if(neprint.gt.neprint_) then
                      write(*,*)
     &                  '*ERROR in elprints: increase neprint_'
                      stop
                    endif
                    nelemprint(neprint)=j
                  enddo
                enddo
              endif
            endif
          enddo
        elseif(textpart(ii)(1:10).eq.'FREQUENCY=') then
           read(textpart(ii)(11:40),'(i30)',iostat=istat) joutl
           if(istat.gt.0) call inputerror(text)
           jout=max(jout,joutl)
        endif
      enddo
!
      do
         call getnewline(text,textpart,istat,in,n,key)
         if(key.eq.1) exit
         do ii=1,n
            if(textpart(ii)(1:2).eq.'S ') then
               noelplab(3)='S   '
            elseif(textpart(ii)(1:4).eq.'E   ') then
               noelplab(4)='E   '
            elseif(textpart(ii)(1:2).eq.'PE') then
               if((nmethod.eq.2).or.(nmethod.eq.3)) then
                  write(*,*) '*WARNING in noelfiles: selection of PE'
                  write(*,*) '         does not make sense for a'
                  write(*,*) '         frequency or bucking calculation'
               else
                  noelplab(6)='PE  '
               endif
            elseif(textpart(ii)(1:4).eq.'ENER') then
               noelplab(7)='ENER'
            elseif(textpart(ii)(1:3).eq.'SDV') then
               if((nmethod.eq.2).or.(nmethod.eq.3)) then
                  write(*,*) '*WARNING in noelfiles: selection of SDV'
                  write(*,*) '         does not make sense for a'
                  write(*,*) '         frequency or bucking calculation'
               else
                  noelplab(8)='SDV '
               endif
            else
               write(*,*) '*WARNING in elprints: label not applicable'
               write(*,*) '         or unknown; card image:'
               write(*,'(a132)') text
            endif
         enddo
      enddo
!
      return
      end

