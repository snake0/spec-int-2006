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
      subroutine equations(text,textpart,ipompc,nodempc,coefmpc,
     &  nmpc,nmpc_,mpcfree,nk,co,trab,inotr,ntrans,ikmpc,ilmpc,
     &  labmpc,istep,istat,in,n)
!
!     reading the input deck: *EQUATION
!
      implicit none
!
      integer ipompc(*),nodempc(3,*),nmpc,nmpc_,mpcfree,istep,istat,
     &  in,n,i,j,ii,key,nterm,number,nk,inotr(2,*),ntrans,node,ndir,
     &  mpcfreeold,ikmpc(*),ilmpc(*),id,idof,itr
!
      real*8 coefmpc(*),co(3,*),trab(7,*),a(3,3),x
!
      character*20 labmpc(*)
      character*40 textpart(16)
      character*132 text
!
      if(istep.gt.0) then
         write(*,*) '*ERROR in equations: *EQUATION should be placed'
         write(*,*) '  before all step definitions'
         stop
      endif
!
      do
         call getnewline(text,textpart,istat,in,n,key)
         if((istat.lt.0).or.(key.eq.1)) return
         read(textpart(1),'(i40)',iostat=istat) nterm
!
         nmpc=nmpc+1
         if(nmpc.gt.nmpc_) then
            write(*,*) '*ERROR in equations: increase nmpc_'
            stop
         endif
!
         labmpc(nmpc)='                    '
         ipompc(nmpc)=mpcfree
         ii=0
!
         do
            call getnewline(text,textpart,istat,in,n,key)
            if((istat.lt.0).or.(key.eq.1)) then
               write(*,*) '*ERROR in equations: mpc definition ',nmpc
               write(*,*) '  is not complete. Card image:'
               write(*,'(a132)') text
               stop
            endif
!
            do i=1,n/3
!
               read(textpart((i-1)*3+1),'(i40)',iostat=istat) node
               if(istat.gt.0) call inputerror(text)
               if(node.gt.nk) then
                  write(*,*) '*ERROR in equations:'
                  write(*,*) '       node ',node,' is not defined'
                  stop
               endif
!
               read(textpart((i-1)*3+2),'(i40)',iostat=istat) ndir 
               if(istat.gt.0) call inputerror(text)
               if(ndir.gt.3) then
                  write(*,*) '*ERROR in equations:'
                  write(*,*) '       direction',ndir,' is not defined'
                  stop
               endif
!
               read(textpart((i-1)*3+3),'(f40.0)',iostat=istat) x
               if(istat.gt.0) call inputerror(text)
!
!              check whether the node is transformed
!
               if(ntrans.le.0) then
                  itr=0
               elseif(inotr(1,node).eq.0) then
                  itr=0
               else
                  itr=inotr(1,node)
               endif
!
               if(itr.eq.0) then
                  nodempc(1,mpcfree)=node
                  nodempc(2,mpcfree)=ndir
                  coefmpc(mpcfree)=x
!
!                 updating ikmpc and ilmpc
!
                  if(ii.eq.0) then
                     idof=7*(node-1)+ndir
                     call nident(ikmpc,idof,nmpc-1,id)
                     if(id.gt.0) then
                        if(ikmpc(id).eq.idof) then
                           write(*,100)
     &                   (ikmpc(id)-1)/3+1,ikmpc(id)-3*((ikmpc(id)-1)/3)
                           stop
                        endif
                     endif
                     do j=nmpc,id+2,-1
                        ikmpc(j)=ikmpc(j-1)
                        ilmpc(j)=ilmpc(j-1)
                     enddo
                     ikmpc(id+1)=idof
                     ilmpc(id+1)=nmpc
                  endif
!
                  mpcfreeold=mpcfree
                  mpcfree=nodempc(3,mpcfree)
                  if(mpcfree.eq.0) then
                     write(*,*) '*ERROR in equations: increase nmpc_'
                     stop
                  endif
               else
                  call transformatrix(trab(1,inotr(1,node)),
     &                 co(1,node),a)
!
                  number=ndir-1
                  if(ii.eq.0) then
!
!                    determining which direction to use for the
!                    dependent side: should not occur on the dependent
!                    side in another MPC and should have a nonzero
!                    coefficient
!
                     do j=1,3
                        number=number+1
                        if(number.gt.3) number=1
                        idof=7*(node-1)+number
                        call nident(ikmpc,idof,nmpc-1,id)
                        if(id.gt.0) then
                           if(ikmpc(id).eq.idof) then
                              cycle
                           endif
                        endif
                        if(dabs(a(number,ndir)).lt.1.d-10) cycle
                        exit
                     enddo
                     if(j.gt.3) then
                        write(*,*) '*ERROR in equations: SPC in node'
                        write(*,*) node,' in transformed coordinates'
                        write(*,*) ' cannot be converted in MPC: all'
                        write(*,*) ' DOFs in the node are used as'
                        write(*,*) ' dependent nodes in other MPCs'
                        stop
                     endif
                     number=number-1
!
!                    updating ikmpc and ilmpc
!
                     do j=nmpc,id+2,-1
                        ikmpc(j)=ikmpc(j-1)
                        ilmpc(j)=ilmpc(j-1)
                     enddo
                     ikmpc(id+1)=idof
                     ilmpc(id+1)=nmpc
                  endif
!
                  do j=1,3
                     number=number+1
                     if(number.gt.3) number=1
                     if(dabs(a(number,ndir)).lt.1.d-10) cycle
                     nodempc(1,mpcfree)=node
                     nodempc(2,mpcfree)=number
                     coefmpc(mpcfree)=x*a(number,ndir)
                     mpcfreeold=mpcfree
                     mpcfree=nodempc(3,mpcfree)
                     if(mpcfree.eq.0) then
                        write(*,*) '*ERROR in equations: increase nmpc_'
                        stop
                     endif
                  enddo
               endif
!
               ii=ii+1
            enddo
!
            if(ii.eq.nterm) then
               nodempc(3,mpcfreeold)=0
               exit
            endif
         enddo
      enddo
!
 100  format(/,'*ERROR in equations: the DOF corresponding to',
     &           /,'node',i5,' in direction',i5,' is detected on',
     &           /,'the dependent side of two different MPC''s') 
      return
      end

