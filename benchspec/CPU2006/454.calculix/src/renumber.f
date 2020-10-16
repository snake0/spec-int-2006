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
      subroutine renumber(nk,kon,ipkon,lakon,ne,ipompc,nodempc,nmpc,nnn,
     &  npn,adj,xadj,iw,mmm,xnpn)
!
!     renumbers the nodes to reduce the profile length
!
      implicit none
!
      character*8 lakon(*)
!
      integer kon(*),ipompc(*),nodempc(3,*),npn(*),
     &  nnn(*),iw(*),mmm(*),xnpn(*),adj(*),xadj(*),ipkon(*)
!
      integer nne,inpn,iadj,nk,ne,nmpc,i,j,nterm,e2,oldpro,newpro,
     &  index,kflag,nope,indexe,oldpro_exp,newpro_exp
!
      kflag=2
      nne=0
      inpn=0
      iadj=0
!
!     taking the elements into account
!
      do i=1,ne
!
         if(ipkon(i).lt.0) cycle
         indexe=ipkon(i)
         if(lakon(i)(4:4).eq.'2') then
            nope=20
         elseif(lakon(i)(4:4).eq.'8') then
            nope=8
         elseif(lakon(i)(4:5).eq.'10') then
            nope=10
         elseif(lakon(i)(4:4).eq.'4') then
            nope=4
         elseif(lakon(i)(4:5).eq.'15') then
            nope=15
         else
            nope=6
         endif
!
         nne=nne+1
         xnpn(nne)=inpn+1
         do j=1,nope
           npn(inpn+j)=kon(indexe+j)
         enddo
         inpn=inpn+nope
         iadj=iadj+nope*(nope-1)
      enddo
!
!     taking the equations into account
!
      do i=1,nmpc
         nne=nne+1
         xnpn(nne)=inpn+1
         index=ipompc(i)
         nterm=0
         do
            nterm=nterm+1
            npn(inpn+nterm)=nodempc(1,index)
            index=nodempc(3,index)
            if(index.eq.0) exit
         enddo
         inpn=inpn+nterm
         iadj=iadj+nterm*(nterm-1)
      enddo
!
      xnpn(nne+1)=inpn+1
!
      call graph(nk,nne,inpn,npn,xnpn,iadj,adj,xadj)
!
      e2=xadj(nk+1)-1
!
      call label(nk,e2,adj,xadj,mmm,iw,oldpro,newpro,oldpro_exp,
     &  newpro_exp)
!
      write(*,*) 'old profile = ',oldpro_exp,'*2147483647+',oldpro
      write(*,*) 'new profile = ',newpro_exp,'*2147483647+',newpro
      write(*,*)
!
      call isortii(mmm,nnn,nk,kflag)
!
      return
      end
