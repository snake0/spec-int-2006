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
      subroutine nodeprints(text,textpart,set,istartset,iendset,ialset,
     &  nset,nset_,nalset,nodeprint,noprint,noprint_,jout,noelplab,
     &  nodeprint_flag,istep,istat,in,n)
!
!     reading the *NODE PRINT cards in the input deck
!
      implicit none
!
      logical nodeprint_flag
!
      integer istartset(*),iendset(*),ialset(*),nodeprint(*),ii,i,j,l,
     &  jout,joutl
!
      character*4 noelplab(*)
      character*21 set(*),noset
      character*40 textpart(16)
      character*132 text
!
      integer nset,nset_,nalset,noprint,noprint_,istep,istat,in,n,key,
     &  ipos
!
      if(istep.lt.1) then
         write(*,*) '*ERROR in nodeprints: *NODE PRINT should only be'
         write(*,*) '  used within a *STEP definition'
         stop
      endif
!
!     reset the nodal print requests
!
      if(.not.nodeprint_flag) then
         noelplab(1)='    '
         noelplab(2)='    '
         noelplab(5)='    '
      endif
!
      jout=max(jout,1)
!
      do ii=2,n
        if(textpart(ii)(1:5).eq.'NSET=') then
          noset=textpart(ii)(6:25)
          noset(21:21)=' '
          ipos=index(noset,' ')
          noset(ipos:ipos)='N'
          do i=1,nset
            if(set(i).eq.noset) then
              if(ialset(iendset(i)).ne.-1) then
                do j=istartset(i),iendset(i)
                  noprint=noprint+1
                  if(noprint.gt.noprint_) then
                    write(*,*)'*ERROR in nodeprints: increase noprint_'
                    stop
                  endif
                  nodeprint(noprint)=ialset(j)
                enddo
              else
                do l=istartset(i),iendset(i),3
                  do j=ialset(l),ialset(l+1)
                    noprint=noprint+1
                    if(noprint.gt.noprint_) then
                      write(*,*)
     &                  '*ERROR in nodeprints: increase noprint_'
                      stop
                    endif
                    nodeprint(noprint)=j
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
            if(textpart(ii)(1:1).eq.'U') then
               noelplab(1)='U   '
            elseif(textpart(ii)(1:2).eq.'NT') then
               noelplab(2)='NT  '
            elseif(textpart(ii)(1:2).eq.'RF') then
               noelplab(5)='RF  '
            else
               write(*,*) '*WARNING in nodeprints: label not applicable'
               write(*,*) '         or unknown; card image:'
               write(*,'(a132)') text
            endif
         enddo
      enddo
!
      return
      end

