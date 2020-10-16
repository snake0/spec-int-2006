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
      subroutine noelfiles(text,textpart,set,istartset,iendset,ialset,
     &  nset,nset_,nalset,nodeprint,noprint,noprint_,jout,nodeflab,
     &  nmethod,nodefile_flag,elfile_flag,node_flag,istep,istat,in,n)
!
!     reading the *NODE PRINT cards in the input deck
!
      implicit none
!
      logical node_flag,nodefile_flag,elfile_flag
!
      integer istartset(*),iendset(*),ialset(*),nodeprint(*),ii,
     &  jout,joutl,nmethod
!
      character*4 nodeflab(*)
      character*21 set(*)
      character*40 textpart(16)
      character*132 text
!
      integer nset,nset_,nalset,noprint,noprint_,istep,istat,in,n,key
!
      if(istep.lt.1) then
         write(*,*) '*ERROR in noelfiles: *NODE FILE and *EL FILE'
         write(*,*) '       should only be used within a *STEP' 
         write(*,*) '       definition'
         stop
      endif
!
      if(node_flag) then
!
!        reset the nodal print requests
!
         if(.not.nodefile_flag) then
            nodeflab(1)='    '
            nodeflab(2)='    '
            nodeflab(5)='    '
         endif
      else
!
!        reset the element print requests
!
         if(.not.elfile_flag) then
            nodeflab(3)='    '
            nodeflab(4)='    '
            nodeflab(6)='    '
            nodeflab(7)='    '
            nodeflab(8)='    '
         endif
      endif
!
      jout=max(jout,1)
!
      do ii=2,n
        if(textpart(ii)(1:10).eq.'FREQUENCY=') then
           read(textpart(ii)(11:40),'(i30)',iostat=istat) joutl
           if(istat.gt.0) call inputerror(text)
           jout=max(jout,joutl)
        endif
      enddo
!
      do
         call getnewline(text,textpart,istat,in,n,key)
         if((key.eq.1).or.(istat.lt.0)) return
         do ii=1,n
            if(textpart(ii)(1:1).eq.'U') then
               nodeflab(1)='U   '
            elseif(textpart(ii)(1:2).eq.'NT') then
               nodeflab(2)='NT  '
            elseif(textpart(ii)(1:2).eq.'S ') then
               nodeflab(3)='S   '
            elseif(textpart(ii)(1:4).eq.'E   ') then
               nodeflab(4)='E   '
            elseif(textpart(ii)(1:2).eq.'RF') then
               nodeflab(5)='RF  '
            elseif(textpart(ii)(1:2).eq.'PE') then
               if((nmethod.eq.2).or.(nmethod.eq.3)) then
                  write(*,*) '*WARNING in noelfiles: selection of PE'
                  write(*,*) '         does not make sense for a'
                  write(*,*) '         frequency or bucking calculation'
               else
                  nodeflab(6)='PE  '
               endif
            elseif(textpart(ii)(1:4).eq.'ENER') then
               nodeflab(7)='ENER'
            elseif(textpart(ii)(1:3).eq.'SDV') then
               if((nmethod.eq.2).or.(nmethod.eq.3)) then
                  write(*,*) '*WARNING in noelfiles: selection of SDV'
                  write(*,*) '         does not make sense for a'
                  write(*,*) '         frequency or bucking calculation'
               else
                  nodeflab(8)='SDV '
               endif
            else
               write(*,*) '*WARNING in noelfiles: label not applicable'
               write(*,*) '         or unknown; card image:'
               write(*,'(a132)') text
            endif
         enddo
      enddo
!
      return
      end









