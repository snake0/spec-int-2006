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
      subroutine mpcs(text,textpart,set,istartset,iendset,
     &  ialset,nset,nset_,nalset,nalset_,ipompc,nodempc,coefmpc,
     &  labmpc,nmpc,nmpc_,mpcfree,ikmpc,ilmpc,lakon,ipkon,kon,nk,nk_,
     &  nodeboun,ndirboun,ikboun,ilboun,nboun,nboun_,iperturb,ne_,
     &  co,xboun,ctrl,istep,istat,in,n)
!
!     reading the input deck: *MPC
!
      implicit none
!
      character*8 lakon(*)
      character*20 labmpc(*),label
      character*21 set(*),noset
      character*40 textpart(16)
      character*132 text
!
      integer istartset(*),iendset(*),ialset(*),ipompc(*),nodempc(3,*),
     &  nset,nset_,nalset,nalset_,nmpc,nmpc_,mpcfree,nk,nk_,ikmpc(*),
     &  ilmpc(*),ipkon(*),kon(*),i,node,ipos,istep,istat,in,n,ne_,
     &  j,k,nodeboun(*),ndirboun(*),ikboun(*),ilboun(*),
     &  nboun,nboun_,key,iperturb,istart,inode,m
!
      real*8 coefmpc(3,*),co(3,*),xboun(*),ctrl(26)
!
      if(istep.gt.0) then
         write(*,*) 
     &     '*ERROR in mpcs: *MPC should be placed'
         write(*,*) '  before all step definitions'
         stop
      endif
!
!     the *MPC option implies a nonlinear geometric 
!     calculation
!
      if(iperturb.eq.0) then
         iperturb=2
         ctrl(19)=1.d+30
         ctrl(20)=1.d+30
      elseif(iperturb.eq.1) then
         write(*,*) '*ERROR in rigidbodies: the *MPC option'
         write(*,*) '       cannot be used in a perturbation step'
         stop
      endif
!
      istart=0
      inode=0
      do
         call getnewline(text,textpart,istat,in,n,key)
         if((istat.lt.0).or.(key.eq.1)) exit
!
         if(istart.eq.0) then
            label=textpart(1)
            istart=2
         else
            istart=1
         endif
!
         do i=istart,n
            read(textpart(i),'(i40)',iostat=istat) node
            if(istat.gt.0) then
               noset=textpart(i)(1:20)
               noset(21:21)=' '
               ipos=index(noset,' ')
               noset(ipos:ipos)='N'
               do j=1,nset
                  if(noset.eq.set(j)) then
                     m=iendset(j)-istartset(j)+1
                     do k=1,m
                        node=ialset(istartset(j)+k-1)
                        inode=inode+1
                        if(label(1:8).eq.'STRAIGHT') then
                           call straightmpc(ipompc,nodempc,coefmpc,
     &                          labmpc,nmpc,nmpc_,mpcfree,ikmpc,ilmpc,
     &                          nk,nk_,nodeboun,ndirboun,ikboun,ilboun,
     &                          nboun,nboun_,xboun,inode,node,co)
                        elseif(label(1:5).eq.'PLANE') then
                           call planempc(ipompc,nodempc,coefmpc,
     &                          labmpc,nmpc,nmpc_,mpcfree,ikmpc,ilmpc,
     &                          nk,nk_,nodeboun,ndirboun,ikboun,ilboun,
     &                          nboun,nboun_,xboun,inode,node,co)
                        else
                           call usermpc(ipompc,nodempc,coefmpc,
     &                          labmpc,nmpc,nmpc_,mpcfree,ikmpc,ilmpc,
     &                          nk,nk_,nodeboun,ndirboun,ikboun,ilboun,
     &                          nboun,nboun_,xboun,inode,node,co,label)
                        endif
                     enddo
                     exit
                  endif
               enddo
               if(j.gt.nset) then
                  noset(ipos:ipos)=' '
                  write(*,*) '*ERROR in nosets: node set ',
     &                 noset
                  write(*,*) '       has not been defined yet'
                  stop
               endif
            else
               inode=inode+1
               if(node.eq.0) then
                  inode=inode-1
                  cycle
               endif
               if(label(1:8).eq.'STRAIGHT') then
                  call straightmpc(ipompc,nodempc,coefmpc,
     &                 labmpc,nmpc,nmpc_,mpcfree,ikmpc,ilmpc,
     &                 nk,nk_,nodeboun,ndirboun,ikboun,ilboun,
     &                 nboun,nboun_,xboun,inode,node,co)
               elseif(label(1:5).eq.'PLANE') then
                  call planempc(ipompc,nodempc,coefmpc,
     &                 labmpc,nmpc,nmpc_,mpcfree,ikmpc,ilmpc,
     &                 nk,nk_,nodeboun,ndirboun,ikboun,ilboun,
     &                 nboun,nboun_,xboun,inode,node,co)
               else
                  call usermpc(ipompc,nodempc,coefmpc,
     &                 labmpc,nmpc,nmpc_,mpcfree,ikmpc,ilmpc,
     &                 nk,nk_,nodeboun,ndirboun,ikboun,ilboun,
     &                 nboun,nboun_,xboun,inode,node,co,label)
               endif
            endif
         enddo
!
      enddo
!
!     nonhomogeneous term for user MPC
!
      if((label(1:8).ne.'STRAIGHT').and.(label(1:5).ne.'PLANE'))
     &     then
         node=0
         call usermpc(ipompc,nodempc,coefmpc,
     &        labmpc,nmpc,nmpc_,mpcfree,ikmpc,ilmpc,
     &        nk,nk_,nodeboun,ndirboun,ikboun,ilboun,
     &        nboun,nboun_,xboun,inode,node,co,label)
      endif
!
      return
      end
