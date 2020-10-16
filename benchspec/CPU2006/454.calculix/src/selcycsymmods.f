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
      subroutine selcycsymmods(text,textpart,ns,ics,leftset,istartset,
     &  iendset,ialset,ipompc,nodempc,coefmpc,nmpc,nmpc_,ikmpc,ilmpc,
     &  mpcfree,csab,set,nset,labmpc,istep,istat,in,n)
!
!     reading the input deck: *SELECT CYCLIC SYMMETRY MODES
!
      implicit none
!
      character*20 labmpc(*)
      character*21 set(*),leftset
      character*40 textpart(16)
      character*132 text
!
      integer istep,istat,in,n,key,i,ns(5),ics(*),istartset(*),
     &  iendset(*),ialset(*),id,ipompc(*),nodempc(3,*),nmpc,nmpc_,
     &  ikmpc(*),ilmpc(*),mpcfree,idd,i1(2),i2(2),i3,i4,i5,j,k,
     &  mpcfreeold,idof,nodel,ileft,nset,irepeat,ncsnodes,kflag,
     &  nr(1),mpc,index,indexold
!
      real*8 coefmpc(*),csab(7),x1(2),x2(2),x3,x4,x5,dd,xn,yn,zn
!
!     irepeat indicates whether the step was preceded by another
!     cyclic symmetry step (irepeat=1) or not (irepeat=0)
!
      data irepeat /0/
      save irepeat,ncsnodes
!
      if(istep.eq.0) then
         write(*,*)'*ERROR in selcycsymmods:'
         write(*,*)'       *SELECT CYCLIC SYMMETRY MODES'
         write(*,*)'       should be placed within a step definition'
         stop
      endif
!
      if(irepeat.eq.0) then
         irepeat=1
         ncsnodes=ns(4)
      else
         kflag=1
         call isortii(ics,nr,ncsnodes,kflag)
         ns(4)=ncsnodes
      endif
!
      ns(2)=0
      ns(3)=(ns(1)+2)/2
!
      do i=2,n
         if(textpart(i)(1:5).eq.'NMIN=') then
            read(textpart(i)(6:40),'(i35)',iostat=istat) ns(2)
            if(istat.gt.0) call inputerror(text)
         elseif(textpart(i)(1:5).eq.'NMAX=') then
            read(textpart(i)(6:40),'(i35)',iostat=istat) ns(3)
            if(istat.gt.0) call inputerror(text)
         endif
      enddo
!
!     check the input
!
      if(ns(2).lt.0) then
         ns(2)=0
         write(*,*) '*WARNING in selcycsymmods: minimum nodal'
         write(*,*) '         diameter must be nonnegative'
      endif
      if(ns(3).gt.(ns(1)+1)/2) then
         ns(3)=(ns(1)+1)/2
         write(*,*) '*WARNING in selcycsymmods: maximum nodal'
         write(*,*) '         diameter should not exceed half the'
         write(*,*) '         number of sectors (rounded upwards)'
      endif
      if(ns(3).lt.ns(2)) then
         write(*,*) '*ERROR in selcycsymmods: maximum nodal'
         write(*,*) '       diameter should not exceed minimal one'
         stop
      endif
!
!     check whether cyclic symmetry axis is part of the structure
!
      do i=1,nset
         if(set(i).eq.leftset) exit
      enddo
      ileft=i
!
!     if this step was preceded by a cyclic symmetry step:
!     check for MPC's for nodes on the cyclic symmetry axis
!     and delete them
!
      if(irepeat.eq.1) then
         do i=istartset(ileft),iendset(ileft)
            nodel=ialset(i)
            call nident(ics,nodel,ns(4),idd)
            if(idd.gt.0) then
               if(ics(idd).eq.nodel) then
                  do k=1,3
                     idof=7*(nodel-1)+k
                     call nident(ikmpc,idof,nmpc,id)
                     if(id.gt.0) then
                        if(ikmpc(id).eq.idof) then
                           write(*,*) 'removing MPC',nodel,k
                           mpc=ilmpc(id)
                           index=ipompc(mpc)
                           do
                              indexold=index
                              index=nodempc(3,index)
                              if(index.eq.0) exit
                           enddo
                           nodempc(3,indexold)=mpcfree
                           mpcfree=ipompc(mpc)
                           do j=id,nmpc-1
                              ikmpc(j)=ikmpc(j+1)
                              ilmpc(j)=ilmpc(j+1)
                           enddo
                           ikmpc(nmpc)=0
                           ilmpc(nmpc)=0
                           nmpc=nmpc-1
                        endif
                     endif
                  enddo
               endif
            endif
         enddo
      endif
!
      do i=istartset(ileft),iendset(ileft)
         nodel=ialset(i)
         call nident(ics,nodel,ns(4),idd)
         if(idd.gt.0) then
            if(ics(idd).eq.nodel) then
               if(ns(2).ne.ns(3)) then
                  if((ns(2).eq.0).or.(ns(2).eq.1)) then
                     write(*,*) '*ERROR: axis of cyclic symmetry'
                     write(*,*) '        is part of the structure;'
                     write(*,*) '        nodal diameters 0, 1, and'
                     write(*,*) '        those above must be each in'
                     write(*,*) '        separate steps.'
                     stop
                  endif
               endif
!
!              specifying special MPC's for nodes on the axis
!
!              normal along the axis
!
               xn=csab(4)-csab(1)
               yn=csab(5)-csab(2)
               zn=csab(6)-csab(3)
               dd=dsqrt(xn*xn+yn*yn+zn*zn)
               xn=xn/dd
               yn=yn/dd
               zn=zn/dd
!               
!              nodal diameter 0
!
               if(ns(2).eq.0) then
                  if(dabs(xn).gt.1.d-10) then
                     i1(1)=2
                     i1(2)=3
                     i2(1)=1
                     i2(2)=1
                     x1(1)=xn
                     x1(2)=xn
                     x2(1)=-yn
                     x2(2)=-zn
                  elseif(dabs(yn).gt.1.d-10) then
                     i1(1)=1
                     i1(2)=3
                     i2(1)=2
                     i2(2)=2
                     x1(1)=yn
                     x1(2)=yn
                     x2(1)=-xn
                     x2(2)=-zn
                  elseif(dabs(zn).gt.1.d-10) then
                     i1(1)=1
                     i1(2)=2
                     i2(1)=3
                     i2(2)=3
                     x1(1)=zn
                     x1(2)=zn
                     x2(1)=-xn
                     x2(2)=-yn
                  endif
!
!                 generating two MPC's expressing that the nodes cannot
!                 move in planes perpendicular to the cyclic symmetry
!                 axis
!
                  do k=1,2
                     idof=7*(nodel-1)+i1(k)
                     call nident(ikmpc,idof,nmpc,id)
                     if(id.gt.0) then
                        if(ikmpc(id).eq.idof) then
                           write(*,*) '*ERROR in selcycsymmods:'
                           write(*,*) '       node on cyclic symmetry'
                           write(*,*) '       axis is used in other MPC'
                           stop
                        endif
                     endif
                     nmpc=nmpc+1
                     ipompc(nmpc)=mpcfree
                     labmpc(nmpc)='                    '
!
!                    updating ikmpc and ilmpc
!
                     do j=nmpc,id+2,-1
                        ikmpc(j)=ikmpc(j-1)
                        ilmpc(j)=ilmpc(j-1)
                     enddo
                     ikmpc(id+1)=idof
                     ilmpc(id+1)=nmpc
!
                     nodempc(1,mpcfree)=nodel
                     nodempc(2,mpcfree)=i1(k)
                     coefmpc(mpcfree)=x1(k)
                     mpcfree=nodempc(3,mpcfree)
                     if(mpcfree.eq.0) then
                        write(*,*) '*ERROR in selcycsymmods:'
                        write(*,*) '       increase nmpc_'
                        stop
                     endif
                     nodempc(1,mpcfree)=nodel
                     nodempc(2,mpcfree)=i2(k)
                     coefmpc(mpcfree)=x2(k)
                     mpcfreeold=mpcfree
                     mpcfree=nodempc(3,mpcfree)
                     if(mpcfree.eq.0) then
                        write(*,*) '*ERROR in selcycsymmods:'
                        write(*,*) '       increase nmpc_'
                        stop
                     endif
                     nodempc(3,mpcfreeold)=0
                  enddo
               elseif(ns(2).eq.1) then
!               
!                 nodal diameter 1
!
                  if(dabs(xn).gt.1.d-10) then
                     i3=1
                     i4=2
                     i5=3
                     x3=xn
                     x4=yn
                     x5=zn
                  elseif(dabs(yn).gt.1.d-10) then
                     i3=2
                     i4=2
                     i5=3
                     x3=yn
                     x4=xn
                     x5=zn
                  else
                     i3=3
                     i4=1
                     i5=2
                     x3=zn
                     x4=xn
                     x5=yn
                  endif
!
!                 generating one MPC expressing that the nodes should
!                 not move along the axis
!
                  idof=7*(nodel-1)+i3
                  call nident(ikmpc,idof,nmpc,id)
                  if(id.gt.0) then
                     if(ikmpc(id).eq.idof) then
                        write(*,*) '*ERROR in selcycsymmods:'
                        write(*,*) '       node on cyclic symmetry'
                        write(*,*) '       axis is used in other MPC'
                        stop
                     endif
                  endif
                  nmpc=nmpc+1
                  ipompc(nmpc)=mpcfree
                  labmpc(nmpc)='                    '
!
!                    updating ikmpc and ilmpc
!
                  do j=nmpc,id+2,-1
                     ikmpc(j)=ikmpc(j-1)
                     ilmpc(j)=ilmpc(j-1)
                  enddo
                  ikmpc(id+1)=idof
                  ilmpc(id+1)=nmpc
!
                  nodempc(1,mpcfree)=nodel
                  nodempc(2,mpcfree)=i3
                  coefmpc(mpcfree)=x3
                  mpcfree=nodempc(3,mpcfree)
                  if(mpcfree.eq.0) then
                     write(*,*) '*ERROR in selcycsymmods:'
                     write(*,*) '       increase nmpc_'
                     stop
                  endif
                  nodempc(1,mpcfree)=nodel
                  nodempc(2,mpcfree)=i4
                  coefmpc(mpcfree)=x4
                  mpcfree=nodempc(3,mpcfree)
                  if(mpcfree.eq.0) then
                     write(*,*) '*ERROR in selcycsymmods:'
                     write(*,*) '       increase nmpc_'
                     stop
                  endif
                  nodempc(1,mpcfree)=nodel
                  nodempc(2,mpcfree)=i5
                  coefmpc(mpcfree)=x5
                  mpcfreeold=mpcfree
                  mpcfree=nodempc(3,mpcfree)
                  if(mpcfree.eq.0) then
                     write(*,*) '*ERROR in selcycsymmods:'
                     write(*,*) '       increase nmpc_'
                     stop
                  endif
                  nodempc(3,mpcfreeold)=0
               else
                  do k=1,3
                     idof=7*(nodel-1)+k
                     call nident(ikmpc,idof,nmpc,id)
                     if(id.gt.0) then
                        if(ikmpc(id).eq.idof) then
                           write(*,*) '*ERROR in selcycsymmods:'
                           write(*,*) '       node on cyclic symmetry'
                           write(*,*) '       axis is used in other MPC'
                           stop
                        endif
                     endif
                     nmpc=nmpc+1
                     ipompc(nmpc)=mpcfree
                     labmpc(nmpc)='                    '
!
!                    updating ikmpc and ilmpc
!
                     do j=nmpc,id+2,-1
                        ikmpc(j)=ikmpc(j-1)
                        ilmpc(j)=ilmpc(j-1)
                     enddo
                     ikmpc(id+1)=idof
                     ilmpc(id+1)=nmpc
!
                     nodempc(1,mpcfree)=nodel
                     nodempc(2,mpcfree)=k
                     coefmpc(mpcfree)=1.d0
                     mpcfreeold=mpcfree
                     mpcfree=nodempc(3,mpcfree)
                     if(mpcfree.eq.0) then
                        write(*,*) '*ERROR in selcycsymmods:'
                        write(*,*) '       increase nmpc_'
                        stop
                     endif
                     nodempc(3,mpcfreeold)=0
                  enddo
               endif
!
!              remove nodel from ordered field ics
!              ics contains the independent nodes; nodel is an
!              independent as well as a dependent node
!
               do k=idd,ns(4)-1
                  ics(k)=ics(k+1)
               enddo
               ics(ns(4))=nodel
               ns(4)=ns(4)-1
            endif
         endif
      enddo
!
      call getnewline(text,textpart,istat,in,n,key)
!
      return
      end

