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
      subroutine cycsymmods(text,textpart,set,istartset,iendset,
     &  ialset,nset,leftset,rightset,tietol,co,nk,ipompc,nodempc,
     &  coefmpc,nmpc,nmpc_,ikmpc,ilmpc,mpcfree,rcs,zcs,ics,nr,nz,
     &  rcs0,zcs0,ns,ncs_,csab,labmpc,istep,istat,in,n)
!
!     reading the input deck: *CYCLIC SYMMETRY MODEL
!
!     ns(1): # segments in 360°
!     ns(2): minimum node diameter
!     ns(3): maximum node diameter
!     ns(4): # nodes in set rightset
!     ns(5): # sectors to be plotted
!
      implicit none
!
      character*20 labmpc(*)
      character*21 set(*),leftset,rightset
      character*40 textpart(16)
      character*132 text
!
      integer istartset(*),iendset(*),ialset(*),ipompc(*),nodempc(3,*),
     &  nset,istep,istat,in,n,key,i,j,k,nk,nmpc,nmpc_,mpcfree,ics(*),
     &  nr(*),nz(*),ns(5),ileft,iright,l,nodel,noder,ikmpc(*),ilmpc(*),
     &  number,idof,ndir,kflag,node,ncsnodes,id,mpcfreeold,ncs_,
     &  istart
!
      real*8 tietol,co(3,*),coefmpc(*),rcs(*),zcs(*),rcs0(*),zcs0(*),
     &  csab(7),xn,yn,zn,dd,xap,yap,zap,rp,zp,x1(3),x2(3),x3(3),
     &  al(3,3),ar(3,3)
!
      if(istep.gt.0) then
         write(*,*) '*ERROR in cycsymmods: *CYCLIC SYMMETRY MODEL'
         write(*,*) '  should be placed before all step definitions'
         stop
      endif
!
      do i=2,n
         if(textpart(i)(1:2).eq.'N=') then
            read(textpart(i)(3:40),'(i38)',iostat=istat) ns(1)
            if(istat.gt.0) call inputerror(text)
         elseif(textpart(i)(1:7).eq.'NGRAPH=') then
            read(textpart(i)(8:40),'(i33)',iostat=istat) ns(5)
            if(istat.gt.0) call inputerror(text)
         endif
      enddo
!
      if(ns(1).le.0) then
         write(*,*) '*ERROR in cycsymmods: the required parameter N'
         write(*,*) '       is lacking on the *CYCLIC SYMMETRY MODEL'
         write(*,*) '       keyword card or has a value <=0'
         stop
      endif
      if(ns(5).lt.1) then
         write(*,*) '*ERROR in cycsymmods: cannot plot less than'
         write(*,*) '       one sector: one sector will be plotted'
         ns(5)=1
      endif
      if(ns(5).gt.ns(1)) then
         write(*,*) '*ERROR in cycsymmods: cannot plot more than'
         write(*,*) '       ',ns(1),'sectors;',ns(1),' sectors will'
         write(*,*) '       be plotted'
         ns(5)=ns(1)
      endif
!
      call getnewline(text,textpart,istat,in,n,key)
!
      if((istat.lt.0).or.(key.eq.1)) then
         write(*,*)'*ERROR in cycsymmods: definition of the cyclic'
         write(*,*) '  symmetry model is not complete'
         stop
      endif
!
      do i=1,6
         read(textpart(i),'(f40.0)',iostat=istat) csab(i)
         if(istat.gt.0) call inputerror(text)
      enddo
!
!     cyclic coordinate system
!
      csab(7)=-1.d0
!
!     check whether leftset and rightset exist
!
      do i=1,nset
         if(set(i).eq.leftset) exit
      enddo
      if(i.gt.nset) then
         write(*,*) '*ERROR in cycsymmods: surface ',leftset
         write(*,*) '  has not yet been defined.' 
         stop
      endif
      ileft=i
!
      do i=1,nset
         if(set(i).eq.rightset) exit
      enddo
      if(i.gt.nset) then
         write(*,*) '*ERROR in cycsymmods: surface ',rightset
         write(*,*) '  has not yet been defined.' 
         stop
      endif
      iright=i
!
!     unit vector along the rotation axis (xn,yn,zn)
!
      xn=csab(4)-csab(1)
      yn=csab(5)-csab(2)
      zn=csab(6)-csab(3)
      dd=dsqrt(xn*xn+yn*yn+zn*zn)
      xn=xn/dd
      yn=yn/dd
      zn=zn/dd
!
!     defining the leftset as a 2-D data field (axes: r=radial
!     coordinate, z=axial coordinate): needed to allocated a node
!     of the leftset to a node of the rightset for the cyclic
!     symmetry equations
!
      l=0
      do j=istartset(iright),iendset(iright)
         if(ialset(j).gt.0) then
            l=l+1
            node =ialset(j)
!
            xap=co(1,node)-csab(1)
            yap=co(2,node)-csab(2)
            zap=co(3,node)-csab(3)
!
            ics(l)=node
            zcs(l)=xap*xn+yap*yn+zap*zn
            rcs(l)=dsqrt((xap-zcs(l)*xn)**2+
     &                   (yap-zcs(l)*yn)**2+
     &                   (zap-zcs(l)*zn)**2)
         else
            k=ialset(j-2)
            do
               k=k-ialset(j)
               if(k.ge.ialset(j-1)) exit
               l=l+1
               if(l.gt.ncs_) then
                  write(*,*) '*ERROR in cycsymmods: increase ncs_'
                  stop
               endif
               node=k
!
               xap=co(1,node)-csab(1)
               yap=co(2,node)-csab(2)
               zap=co(3,node)-csab(3)
!
               ics(l)=node
               zcs(l)=xap*xn+yap*yn+zap*zn
               rcs(l)=dsqrt((xap-zcs(l)*xn)**2+
     &                      (yap-zcs(l)*yn)**2+
     &                      (zap-zcs(l)*zn)**2)
            enddo
         endif
      enddo
!
      ncsnodes=l
!
!     initialization of near2d
!
      do i=1,ncsnodes
         nr(i)=i
         nz(i)=i
         rcs0(i)=rcs(i)
         zcs0(i)=zcs(i)
      enddo
      kflag=2
      call dsort(rcs,nr,ncsnodes,kflag)
      call dsort(zcs,nz,ncsnodes,kflag)
!
!     allocating a node of the leftset to each node of the rightset 
!
      istart=0
!
      do i=istartset(ileft),iendset(ileft)
         if(ialset(i).gt.0) then
            nodel=ialset(i)
!
            xap=co(1,nodel)-csab(1)
            yap=co(2,nodel)-csab(2)
            zap=co(3,nodel)-csab(3)
!
            zp=xap*xn+yap*yn+zap*zn
            rp=dsqrt((xap-zp*xn)**2+(yap-zp*yn)**2+(zap-zp*zn)**2)
!
            call near2d(rcs0,zcs0,rcs,zcs,nr,nz,rp,zp,ncsnodes,node)
            if((tietol.gt.0.d0).and.
     &         (tietol.le.dsqrt((rp-rcs0(node))**2+(zp-zcs0(node))**2)))
     &             then
               write(*,*) '*WARNING in cycsymmods: no cyclic'
               write(*,*) '         symmetric partner found for'
               write(*,*) '         node ',nodel
               cycle
            endif
!
            noder=ics(node)
            if(noder.eq.nodel) cycle
!
!           if first node pair: calculate right orientation of xn
!           if the orientation of (xn,yn,zn) is wrong, this is corrected
!           at the end of this routine (via csab)
!
            if(istart.eq.0) then
               do j=1,3
                  x1(j)=(co(j,noder)+co(j,nodel))/2.d0-csab(j)
                  x2(j)=co(j,noder)-co(j,nodel)
               enddo
               x3(1)=x1(2)*x2(3)-x1(3)*x2(2)
               x3(2)=-x1(1)*x2(3)+x1(3)*x2(1)
               x3(3)=x1(1)*x2(2)-x1(2)*x2(1)
               istart=1
            endif
!
            call transformatrix(csab,co(1,nodel),al)
            call transformatrix(csab,co(1,noder),ar)
!
!           generating the MPC's; the generated MPC's are for nodal
!           diameter 0. For other nodal diameters the MPC's are
!           changed implicitly in mastructcs and mafillsmcs
!
            do ndir=1,3
               nmpc=nmpc+1
               labmpc(nmpc)='CYCLIC              '
               ipompc(nmpc)=mpcfree
               number=ndir-1
!
!                    determining which direction to use for the
!                    dependent side: should not occur on the dependent
!                    side in another MPC and should have a nonzero
!                    coefficient
!
               do j=1,3
                  number=number+1
                  if(number.gt.3) number=1
                  idof=7*(nodel-1)+number
                  call nident(ikmpc,idof,nmpc-1,id)
                  if(id.gt.0) then
                     if(ikmpc(id).eq.idof) then
                        cycle
                     endif
                  endif
                  if(dabs(al(number,ndir)).lt.1.d-10) cycle
                  exit
               enddo
               if(j.gt.3) then
                  write(*,*) '*ERROR in cycsymmods: SPC in node'
                  write(*,*) nodel,' in transformed coordinates'
                  write(*,*) ' cannot be converted in MPC: all'
                  write(*,*) ' DOFs in the node are used as'
                  write(*,*) ' dependent nodes in other MPCs'
                  stop
               endif
               number=number-1
!
!              updating ikmpc and ilmpc
!
               do j=nmpc,id+2,-1
                  ikmpc(j)=ikmpc(j-1)
                  ilmpc(j)=ilmpc(j-1)
               enddo
               ikmpc(id+1)=idof
               ilmpc(id+1)=nmpc
!
               do j=1,3
                  number=number+1
                  if(number.gt.3) number=1
                  if(dabs(al(number,ndir)).lt.1.d-10) cycle
                  nodempc(1,mpcfree)=nodel
                  nodempc(2,mpcfree)=number
                  coefmpc(mpcfree)=al(number,ndir)
                  mpcfree=nodempc(3,mpcfree)
                  if(mpcfree.eq.0) then
                     write(*,*) '*ERROR in cycsymmods: increase nmpc_'
                     stop
                  endif
               enddo
               do j=1,3
                  number=number+1
                  if(number.gt.3) number=1
                  if(dabs(ar(number,ndir)).lt.1.d-10) cycle
                  nodempc(1,mpcfree)=noder
                  nodempc(2,mpcfree)=number
                  coefmpc(mpcfree)=-ar(number,ndir)
                  mpcfreeold=mpcfree
                  mpcfree=nodempc(3,mpcfree)
                  if(mpcfree.eq.0) then
                     write(*,*) '*ERROR in cycsymmods: increase nmpc_'
                     stop
                  endif
               enddo
               nodempc(3,mpcfreeold)=0
            enddo
!
         else
            k=ialset(i-2)
            do
               k=k-ialset(i)
               if(k.ge.ialset(i-1)) exit
               nodel=k
!
               xap=co(1,nodel)-csab(1)
               yap=co(2,nodel)-csab(2)
               zap=co(3,nodel)-csab(3)
!
               zp=xap*xn+yap*yn+zap*zn
               rp=dsqrt((xap-zp*xn)**2+(yap-zp*yn)**2+(zap-zp*zn)**2)
!
               call near2d(rcs0,zcs0,rcs,zcs,nr,nz,rp,zp,ncsnodes,node)
               if((tietol.gt.0.d0).and.
     &         (tietol.gt.dsqrt((rp-rcs0(node))**2+(zp-zcs0(node))**2)))
     &              cycle
!
               noder=ics(node)
!
               call transformatrix(csab,co(1,nodel),al)
               call transformatrix(csab,co(1,noder),ar)
!
               do ndir=1,3
                  nmpc=nmpc+1
                  labmpc(nmpc)='CYCLIC              '
                  ipompc(nmpc)=mpcfree
                  number=ndir-1
!
!                    determining which direction to use for the
!                    dependent side: should not occur on the dependent
!                    side in another MPC and should have a nonzero
!                    coefficient
!
                  do j=1,3
                     number=number+1
                     if(number.gt.3) number=1
                     idof=7*(nodel-1)+number
                     call nident(ikmpc,idof,nmpc-1,id)
                     if(id.gt.0) then
                        if(ikmpc(id).eq.idof) then
                           cycle
                        endif
                     endif
                     if(dabs(al(number,ndir)).lt.1.d-10) cycle
                     exit
                  enddo
                  if(j.gt.3) then
                     write(*,*) '*ERROR in cycsymmods: SPC in node'
                     write(*,*) nodel,' in transformed coordinates'
                     write(*,*) ' cannot be converted in MPC: all'
                     write(*,*) ' DOFs in the node are used as'
                     write(*,*) ' dependent nodes in other MPCs'
                     stop
                  endif
                  number=number-1
!
!              updating ikmpc and ilmpc
!
                  do j=nmpc,id+2,-1
                     ikmpc(j)=ikmpc(j-1)
                     ilmpc(j)=ilmpc(j-1)
                  enddo
                  ikmpc(id+1)=idof
                  ilmpc(id+1)=nmpc
!
                  do j=1,3
                     number=number+1
                     if(number.gt.3) number=1
                     if(dabs(al(number,ndir)).lt.1.d-10) cycle
                     nodempc(1,mpcfree)=nodel
                     nodempc(2,mpcfree)=number
                     coefmpc(mpcfree)=al(number,ndir)
                     mpcfree=nodempc(3,mpcfree)
                     if(mpcfree.eq.0) then
                        write(*,*)
     &                    '*ERROR in cycsymmods: increase nmpc_'
                        stop
                     endif
                  enddo
                  do j=1,3
                     number=number+1
                     if(number.gt.3) number=1
                     if(dabs(ar(number,ndir)).lt.1.d-10) cycle
                     nodempc(1,mpcfree)=noder
                     nodempc(2,mpcfree)=number
                     coefmpc(mpcfree)=-ar(number,ndir)
                     mpcfreeold=mpcfree
                     mpcfree=nodempc(3,mpcfree)
                     if(mpcfree.eq.0) then
                        write(*,*) 
     &                    '*ERROR in cycsymmods: increase nmpc_'
                        stop
                     endif
                  enddo
                  nodempc(3,mpcfreeold)=0
               enddo
            enddo
         endif
!
      enddo
!
!     sorting ics
!     ics contains the master (independent) nodes
!
      kflag=1
      call isortii(ics,nr,ncsnodes,kflag)
      ns(4)=ncsnodes
!
!     check orientation of (xn,yn,zn) (important for copying of base
!     sector in arpackcs)
!
      if(x3(1)*xn+x3(2)*yn+x3(3)*zn.lt.0.d0) then
         csab(4)=2.d0*csab(1)-csab(4)
         csab(5)=2.d0*csab(2)-csab(5)
         csab(6)=2.d0*csab(3)-csab(6)
      endif
!
      call getnewline(text,textpart,istat,in,n,key)
!
      return
      end

