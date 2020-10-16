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
      subroutine envtemp(itg,matg,ntg,ntr,sideload,nelemload,
     &  ipkon,kon,lakon,ielmat,ne,nload,iptri,kontri,ntri,nloadtr)
!
!     determines the number of gas temperatures and radiation
!     temperatures
!
      implicit none
!
      character*5 sideload(*)
      character*8 lakon(*)
!
      integer itg(*),matg(*),ntg,ntr,nelemload(2,*),ipkon(*),
     &  kon(*),ielmat(*),ne,i,j,k,l,index,id,node,nload,ifaceq(8,6),
     &  ifacet(6,4),ifacew(8,5),kontri3(3,1),kontri4(3,2),
     &  kontri6(3,4),kontri8(3,6),iptri(*),kontri(3,*),ntri,
     &  konf(8),nloadtr(*),nelem,nope,nopes,ig
!
      data ifaceq /4,3,2,1,11,10,9,12,
     &            5,6,7,8,13,14,15,16,
     &            1,2,6,5,9,18,13,17,
     &            2,3,7,6,10,19,14,18,
     &            3,4,8,7,11,20,15,19,
     &            4,1,5,8,12,17,16,20/
      data ifacet /1,3,2,7,6,5,
     &             1,2,4,5,9,8,
     &             2,3,4,6,10,9,
     &             1,4,3,8,10,7/
      data ifacew /1,3,2,9,8,7,0,0,
     &             4,5,6,10,11,12,0,0,
     &             1,2,5,4,7,14,10,13,
     &             2,3,6,5,8,15,11,14,
     &             4,6,3,1,12,15,9,13/
      data kontri3 /1,2,3/
      data kontri4 /1,2,4,2,3,4/
      data kontri6 /1,4,6,4,5,6,4,2,5,6,5,3/
      data kontri8 /1,5,8,8,5,7,8,7,4,5,2,6,5,6,7,7,6,3/
!
      ntg=0
      ntr=0
      ntri=0
!
!     ordering the gas temperature nodes and counting them
!     counting the radiation temperatures
!
      do i=1,nload
         if(sideload(i)(3:4).eq.'FC') then
            call nident(itg,nelemload(2,i),ntg,id)
            if(id.gt.0) then
               if(itg(id).eq.nelemload(2,i)) cycle
            endif
            ntg=ntg+1
            do j=ntg,id+2,-1
               itg(j)=itg(j-1)
            enddo
            itg(id+1)=nelemload(2,i)
         elseif(sideload(i)(3:4).eq.'CA') then
            ntr=ntr+1
            nelem=nelemload(1,i)
            read(sideload(i)(2:2),'(i1)') ig
!
!           number of nodes in the face
!
            if(lakon(nelem)(4:4).eq.'2') then
               nope=20
               nopes=8
            elseif(lakon(nelem)(4:4).eq.'8') then
               nope=8
               nopes=4
            elseif(lakon(nelem)(4:5).eq.'10') then
               nope=10
               nopes=6
            elseif(lakon(nelem)(4:4).eq.'4') then
               nope=4
               nopes=3
            elseif(lakon(nelem)(4:4).eq.'6') then
               nope=6
               if(ig.le.2) then
                  nopes=3
               else
                  nopes=4
               endif
            elseif(lakon(nelem)(4:5).eq.'15') then
               nope=15
               if(ig.le.2) then
                  nopes=6
               else
                  nopes=8
               endif
            endif
!
!           nodes in the face
!
            if((nope.eq.20).or.(nope.eq.8)) then
               do k=1,nopes
                  konf(k)=kon(ipkon(nelem)+ifaceq(k,ig))
               enddo
            elseif((nope.eq.10).or.(nope.eq.4)) then
               do k=1,nopes
                  konf(k)=kon(ipkon(nelem)+ifacet(k,ig))
               enddo
            else
               do k=1,nopes
                  konf(k)=kon(ipkon(nelem)+ifacew(k,ig))
               enddo
            endif
!
!           triangulation of the face
!
            iptri(ntr)=ntri
            nloadtr(ntr)=i
            if((lakon(nelem)(4:4).eq.'2').or.
     &         ((lakon(nelem)(4:5).eq.'15').and.(ig.gt.2))) then
               do k=1,6
                  ntri=ntri+1
                  do l=1,3
                     kontri(l,ntri)=konf(kontri8(l,k))
                  enddo
               enddo
            elseif((lakon(nelem)(4:4).eq.'8').or.
     &         ((lakon(nelem)(4:4).eq.'6').and.(ig.gt.2))) then
               do k=1,2
                  ntri=ntri+1
                  do l=1,3
                     kontri(l,ntri)=konf(kontri4(l,k))
                  enddo
               enddo
            elseif((lakon(nelem)(4:5).eq.'10').or.
     &         ((lakon(nelem)(4:5).eq.'15').and.(ig.le.2))) then
               do k=1,4
                  ntri=ntri+1
                  do l=1,3
                     kontri(l,ntri)=konf(kontri6(l,k))
                  enddo
               enddo
            elseif((lakon(nelem)(4:4).eq.'4').or.
     &         ((lakon(nelem)(4:4).eq.'6').and.(ig.le.2))) then
               do k=1,1
                  ntri=ntri+1
                  do l=1,3
                     kontri(l,ntri)=konf(kontri3(l,k))
                  enddo
               enddo
            endif   
         endif
      enddo
!
!     check for the property definition of the gas temperatures
!     (specific heat)
!           
      do i=1,ne
         if(lakon(i)(1:1).eq.'D') then
            index=ipkon(i)
            node=kon(index+1)
            call nident(itg,node,ntg,id)
            if(id.gt.0) then
               if(itg(id).eq.node) then
                  matg(id)=ielmat(i)
               endif
            endif
         endif
      enddo
!
!     check whether an element was assigned to each gas
!     temperature (the property definition is applied to
!     elements, not to nodes
!
      do i=1,ntg
         if(matg(i).eq.0) then
            write(*,*) '*ERROR in envtemp: no element assigned to'
            write(*,*) '       gas temperature node',itg(i)
            stop
         endif
      enddo
!
      return
      end


