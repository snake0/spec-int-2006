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
      subroutine forcadd(node,i,val,nodeforc,ndirforc,xforc,
     &  nforc,nforc_,iamforc,iamplitude,nam,ntrans,trab,inotr,co,
     &  ikforc,ilforc)
!
!     adds a cload condition to the data base
!
      implicit none
!
      integer nodeforc(*),ndirforc(*),node,i,nforc,nforc_,j,
     &  iamforc(*),iamplitude,nam,ntrans,inotr(2,*),itr,idf(3),
     &  ikforc(*),ilforc(*),idof,id,k
!
      real*8 xforc(*),val,trab(7,*),a(3,3),co(3,*)
!
      if(ntrans.eq.0) then
         itr=0
      else
         itr=inotr(1,node)
      endif
!
      if(itr.eq.0) then
!
!        no transformation applies to the node
!
         idof=7*(node-1)+i
         call nident(ikforc,idof,nforc,id)
         if(id.gt.0) then
            if(ikforc(id).eq.idof) then
               k=ilforc(id)
               xforc(k)=val
               if(nam.gt.0) iamforc(k)=iamplitude
               return
            endif
         endif
c
         nforc=nforc+1
         if(nforc.gt.nforc_) then
            write(*,*) '*ERROR in forcadd: increase nforc_'
            stop
         endif
         nodeforc(nforc)=node
         ndirforc(nforc)=i
         xforc(nforc)=val
         if(nam.gt.0) iamforc(nforc)=iamplitude
!
!        updating ikforc and ilforc
!            
         do j=nforc,id+2,-1
            ikforc(j)=ikforc(j-1)
            ilforc(j)=ilforc(j-1)
         enddo
         ikforc(id+1)=idof
         ilforc(id+1)=nforc
      else
!
!        a transformation applies
!
         call transformatrix(trab(1,itr),co(1,node),a)
!
         do j=1,3
            idf(j)=0
            idof=7*(node-1)+j
            call nident(ikforc,idof,nforc,id)
            if(id.gt.0) then
               if(ikforc(id).eq.idof) then
                  idf(j)=ilforc(id)
               endif
            endif
         enddo
!
         if(idf(1).ne.0) then
!
!        a force was previously applied to this node. The component
!        in direction i is filtered out and replaced by the new
!        value
!
!        if an amplitude is selected, it applies to all components
!        of the force in the node. No separate amplitudes are allowed.
!
            val=val-xforc(idf(1))*a(1,i)-xforc(idf(2))*a(2,i)
     &             -xforc(idf(3))*a(3,i)
!
            xforc(idf(1))=xforc(idf(1))+val*a(1,i)
            xforc(idf(2))=xforc(idf(2))+val*a(2,i)
            xforc(idf(3))=xforc(idf(3))+val*a(3,i)
         else
            do j=1,3
               nforc=nforc+1
               if(nforc.gt.nforc_) then
                  write(*,*) '*ERROR in forcadd: increase nforc_'
                  stop
               endif
               nodeforc(nforc)=node
               ndirforc(nforc)=j
               xforc(nforc)=val*a(j,i)
               if(nam.gt.0) iamforc(nforc)=iamplitude
!
!              updating ikforc and ilforc
! 
               idof=7*(node-1)+j
               call nident(ikforc,idof,nforc-1,id)
               do k=nforc,id+2,-1
                  ikforc(k)=ikforc(k-1)
                  ilforc(k)=ilforc(k-1)
               enddo
               ikforc(id+1)=idof
               ilforc(id+1)=nforc
            enddo
         endif
      endif
!
      return
      end

