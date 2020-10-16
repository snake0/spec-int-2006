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
      subroutine boundaries(text,textpart,set,istartset,iendset,
     &  ialset,nset,nodeboun,ndirboun,xboun,nboun,nboun_,nk,
     &  iamboun,amname,nam,ipompc,nodempc,coefmpc,nmpc,nmpc_,
     &  mpcfree,inotr,trab,ntrans,ikboun,ilboun,ikmpc,ilmpc,nk_,
     &  co,labmpc,boun_flag,istep,istat,in,n)
!
!     reading the input deck: *INITIAL CONDITIONS
!
      implicit none
!
      logical boun_flag
!
      integer istartset(*),iendset(*),ialset(*),nodeboun(*),ndirboun(*),
     &  nset,nboun,nboun_,istep,istat,in,n,i,j,k,l,ibounstart,ibounend,
     &  key,nk,iamboun(*),nam,iamplitude,ipompc(*),nodempc(3,*),
     &  nmpc,nmpc_,mpcfree,inotr(2,*),ikboun(*),ilboun(*),ikmpc(*),
     &  ilmpc(*),nmpcold,id,idof,index1,ntrans,nk_,ipos
!
      real*8 xboun(*),bounval,coefmpc(*),trab(7,*),co(3,*)
!
      character*20 amname(*),amplitude,labmpc(*)
      character*21 set(*),noset
      character*40 textpart(16)
      character*132 text
!
      iamplitude=0
!
      do i=2,n
         if((textpart(i)(1:6).eq.'OP=NEW').and.(.not.boun_flag)) then
!
!           spc's in nonglobal coordinates result in mpc's
!           removing these mpc's
!            
            if(ntrans.gt.0) then
               do j=1,nk
                  if(inotr(2,j).gt.0) then
                     do k=1,3
                        idof=7*(inotr(2,j)-1)+k
                        call nident(ikboun,idof,nboun,id)
                        if(id.gt.0) then
                           if(ikboun(id).eq.idof) then
!
!           if a SPC is defined in direction k for a node j for which a 
!           local coordinate system applies, then the coordinate system
!           number is stored in inotr(1,j) and the additional node
!           for the inhomogeneous term is stored in inotr(2,j). The
!           SPC DOF is (inotr(2,j)-1)*3+k, however, the independent
!           MPC DOF is (j-1)*3+l, where l can be different from k,
!           since (j-1)*3+k might already be taken by another MPC, or
!           the coefficient for this direction might be zero.
!
                              l=k
                              loop: do
                                 idof=7*(j-1)+l
                                 call nident(ikmpc,idof,nmpc,id)
                                 if(id.gt.0) then
                                    if(ikmpc(id).eq.idof) then
                                       index1=ipompc(ilmpc(id))
                                       if(index1.eq.0) then
                                          l=l+1
                                          if(l.gt.3) l=l-3
                                          cycle
                                       endif
                                       do
                                          if(nodempc(3,index1).eq.0)
     &                                       then
                                             if(nodempc(2,index1).eq.k) 
     &                                         then
                                               nodempc(3,index1)=mpcfree
                                               mpcfree=ipompc(ilmpc(id))
                                               ipompc(ilmpc(id))=0
                                               ilmpc(id)=0
                                               exit loop
                                             else
                                               exit
                                             endif
                                          else
                                             index1=nodempc(3,index1)
                                          endif
                                       enddo
                                    endif
                                 endif
                                 l=l+1
                                 if(l.gt.3) l=l-3
                              enddo loop
!
                           endif
                        endif
                     enddo
                  endif
               enddo
               nmpcold=nmpc
!
!              getting rid of the superfluous lines in ikmpc and ilmpc
!
               nmpc=0
               do j=1,nmpcold
                  if(ilmpc(j).ne.0) then
                     nmpc=nmpc+1
                     ikmpc(nmpc)=ikmpc(j)
                     ilmpc(nmpc)=ilmpc(j)
                  endif
               enddo
!
!              getting rid of the superfluous lines in ipompc and labmpc
!
               k=0
               do j=1,nmpcold
                  if(ipompc(j).ne.0) then
                     k=k+1
                     ipompc(k)=ipompc(j)
                     labmpc(k)=labmpc(j)
                     index1=ipompc(j)
                     idof=7*(nodempc(1,index1)-1)+nodempc(2,index1)
                     call nident(ikmpc,idof,nmpc,id)
                     if(id.eq.0) then
                        write(*,*) '*ERROR in boundaries'
                        stop
                     elseif(ikmpc(id).ne.idof) then
                        write(*,*) '*ERROR in boundaries'
                        stop
                     endif
                     ilmpc(id)=k
                  endif
               enddo
            endif
            nboun=0
         elseif(textpart(i)(1:10).eq.'AMPLITUDE=') then
            read(textpart(i)(11:30),'(a20)') amplitude
            do j=1,nam
               if(amname(j).eq.amplitude) then
                  iamplitude=j
                  exit
               endif
            enddo
            if(j.gt.nam) then
               write(*,*)'*ERROR in boundaries: nonexistent amplitude'
               write(*,*) '  Card image:'
               write(*,'(a132)') text
               stop
            endif
            iamplitude=j
         endif
      enddo
!
      do
         call getnewline(text,textpart,istat,in,n,key)
         if((istat.lt.0).or.(key.eq.1)) return
!
         read(textpart(2),'(i40)',iostat=istat) ibounstart
         if(istat.gt.0) call inputerror(text)
!
         if(textpart(3).eq.
     &     '                                        ') then
            ibounend=ibounstart
         else
            read(textpart(3),'(i40)',iostat=istat) ibounend
            if(istat.gt.0) call inputerror(text)
         endif
         if((ibounstart.lt.1).or.(ibounend.gt.6)) then
            write(*,*) '*ERROR in boundaries: nonexistent degree of'
            write(*,*) '       freedom; card image:'
            write(*,'(a132)') text
            stop
         endif
!
         if(textpart(4).eq.
     &     '                                        ') then
            bounval=0.d0
         else
            read(textpart(4),'(f40.0)',iostat=istat) bounval
            if(istat.gt.0) call inputerror(text)
         endif
!
         read(textpart(1),'(i40)',iostat=istat) l
         if(istat.eq.0) then
            if(l.gt.nk) then
               write(*,*) '*ERROR in boundaries:'
               write(*,*) '       node ',l,' is not defined'
               stop
            endif
            call bounadd(l,ibounstart,ibounend,bounval,
     &        nodeboun,ndirboun,xboun,nboun,nboun_,
     &        iamboun,iamplitude,nam,ipompc,nodempc,
     &        coefmpc,nmpc,nmpc_,mpcfree,inotr,trab,
     &        ntrans,ikboun,ilboun,ikmpc,ilmpc,co,nk,nk_,labmpc)
         else
            read(textpart(1)(1:20),'(a20)',iostat=istat) noset
            noset(21:21)=' '
            ipos=index(noset,' ')
            noset(ipos:ipos)='N'
            do i=1,nset
               if(set(i).eq.noset) exit
            enddo
            if(i.gt.nset) then
               noset(ipos:ipos)=' '
               write(*,*) '*ERROR in boundaries: node set ',noset
               write(*,*) '  has not yet been defined. Card image:'
               write(*,'(a132)') text
               stop
            endif
            do j=istartset(i),iendset(i)
               if(ialset(j).gt.0) then
               call bounadd(ialset(j),ibounstart,ibounend,bounval,
     &           nodeboun,ndirboun,xboun,nboun,nboun_,
     &           iamboun,iamplitude,nam,ipompc,nodempc,
     &           coefmpc,nmpc,nmpc_,mpcfree,inotr,trab,
     &           ntrans,ikboun,ilboun,ikmpc,ilmpc,co,nk,nk_,labmpc)
               else
                  k=ialset(j-2)
                  do
                     k=k-ialset(j)
                     if(k.ge.ialset(j-1)) exit
                     call bounadd(k,ibounstart,ibounend,bounval,
     &                 nodeboun,ndirboun,xboun,nboun,nboun_,
     &                 iamboun,iamplitude,nam,ipompc,nodempc,
     &                 coefmpc,nmpc,nmpc_,mpcfree,inotr,trab,
     &                 ntrans,ikboun,ilboun,ikmpc,ilmpc,co,nk,nk_,
     &                 labmpc)
                  enddo
               endif
            enddo
         endif
      enddo
!
      return
      end

