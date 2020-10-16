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
      subroutine rigidbodies(text,textpart,set,istartset,iendset,
     &  ialset,nset,nset_,nalset,nalset_,ipompc,nodempc,coefmpc,
     &  labmpc,nmpc,nmpc_,mpcfree,ikmpc,ilmpc,lakon,ipkon,kon,nk,nk_,
     &  nodeboun,ndirboun,ikboun,ilboun,nboun,nboun_,iperturb,ne_,
     &  ctrl,istep,istat,in,n)
!
!     reading the input deck: *RIGID BODY
!
      implicit none
!
      character*8 lakon(*)
      character*20 labmpc(*)
      character*21 set(*),elset,noset
      character*40 textpart(16)
      character*132 text
!
      integer istartset(*),iendset(*),ialset(*),ipompc(*),nodempc(3,*),
     &  nset,nset_,nalset,nalset_,nmpc,nmpc_,mpcfree,nk,nk_,ikmpc(*),
     &  ilmpc(*),ipkon(*),kon(*),inoset,ielset,i,node,ielement,id,
     &  indexe,nope,istep,istat,in,n,irefnode,irotnode,ne_,
     &  j,idof,k,nodeboun(*),ndirboun(*),ikboun(*),ilboun(*),
     &  nboun,nboun_,key,iperturb,ipos
!
      real*8 coefmpc(3,*),ctrl(26)
!
      if(istep.gt.0) then
         write(*,*) 
     &     '*ERROR in rigidbodies: *RIGID BODY should be placed'
         write(*,*) '  before all step definitions'
         stop
      endif
!
!     the *RIGID BODY option implies a nonlinear geometric 
!     calculation
!
      if(iperturb.eq.0) then
         iperturb=2
         ctrl(19)=1.d+30
         ctrl(20)=1.d+30
      elseif(iperturb.eq.1) then
         write(*,*) '*ERROR in rigidbodies: the *RIGID BODY option'
         write(*,*) '       cannot be used in a perturbation step'
         stop
      endif
!
      elset='                     '
      noset='                     '
      irefnode=0
      irotnode=0
!
      do i=2,n
         if(textpart(i)(1:6).eq.'ELSET=') then
            if(noset.eq.'                     ') then
               elset=textpart(i)(7:26)
               elset(21:21)=' '
               ipos=index(elset,' ')
               elset(ipos:ipos)='E'
            else
               write(*,*) '*ERROR in rigidbodies: either NSET or'
               write(*,*) '       ELSET can be specified, not both'
               stop
            endif
         elseif(textpart(i)(1:5).eq.'NSET=') then
            if(elset.eq.'                     ') then
               noset=textpart(i)(6:25)
               noset(21:21)=' '
               ipos=index(noset,' ')
               noset(ipos:ipos)='N'
            else
               write(*,*) '*ERROR in rigidbodies: either NSET or'
               write(*,*) '       ELSET can be specified, not both'
               stop
            endif
         elseif(textpart(i)(1:8).eq.'REFNODE=') then
            read(textpart(i)(9:40),'(i32)',iostat=istat) irefnode
            if(istat.gt.0) call inputerror(text)
            if(irefnode.gt.nk) then
               write(*,*) '*ERROR in rigidbodies: ref node',irefnode
               write(*,*) '       has not been defined'
               stop
            endif
         elseif(textpart(i)(1:8).eq.'ROTNODE=') then
            read(textpart(i)(9:40),'(i32)',iostat=istat) irotnode
            if(istat.gt.0) call inputerror(text)
            if(irefnode.gt.nk) then
               write(*,*) '*ERROR in rigidbodies: rot node',irotnode
               write(*,*) '       has not been defined'
               stop
            endif
         endif
      enddo
!
!     check whether a set was defined
!
      if((elset.eq.'                     ').and.
     &   (noset.eq.'                     ')) then
         write(*,*) '*WARNING in rigidbodies: no set defined'
         return
      endif
!
      inoset=0
      ielset=0
!
!     checking whether the set exists
!
      if(noset.ne.'                     ') then
         do i=1,nset
            if(set(i).eq.noset) then
               inoset=i
               exit
            endif
         enddo
         if(inoset.eq.0) then
            write(*,*) '*WARNING in rigidbodies: node set ',noset
            write(*,*) '         does not exist'
            return
         endif
      endif
!
      if(elset.ne.'                     ') then
         do i=1,nset
            if(set(i).eq.elset) then
               ielset=i
               exit
            endif
         enddo
         if(ielset.eq.0) then
            write(*,*) '*WARNING in rigidbodies: element set ',elset
            write(*,*) '         does not exist'
            return
         endif
      endif
!
!     check for the existence of irefnode and irotnode; if none were
!     defined, new nodes are generated
!
      if(irefnode.eq.0) then
         nk=nk+1
         if(nk.gt.nk_) then
            write(*,*) '*ERROR in rigidbodies: increase nk_'
            stop
         endif
         irefnode=nk
      endif
!
      if(irotnode.eq.0) then
         nk=nk+1
         if(nk.gt.nk_) then
            write(*,*) '*ERROR in rigidbodies: increase nk_'
            stop
         endif
         irotnode=nk
      endif
!
!     check whether other equations apply to the dependent nodes
!
      if(inoset.ne.0) then
         do i=istartset(inoset),iendset(inoset)
            node=ialset(i)
            if(node.gt.nk_) then
               write(*,*) '*ERROR in rigidbodies: node ',node
               write(*,*) '       belonging to set ',noset
               write(*,*) '       has not been defined'
               stop
            endif
            if((node.eq.irefnode).or.(node.eq.irotnode)) cycle
            do j=1,3
               idof=7*(node-1)+j
               call nident(ikmpc,idof,nmpc,id)
               if(id.gt.0) then
                  if(ikmpc(id).eq.idof) then
                     write(*,*) '*WARNING in rigidbodies: dof ',j
                     write(*,*) '         of node ',node,' belonging'
                     write(*,*) '         to a rigid body is detected'
                     write(*,*) '         on the dependent side of '
                     write(*,*) '         another equation; no rigid'
                     write(*,*) '         body constrained applied'
                  endif
               endif
            enddo
         enddo
      endif
!
      if(ielset.ne.0) then
         do i=istartset(ielset),iendset(ielset)
            ielement=ialset(i)
            if(ielement.gt.ne_) then
               write(*,*) '*ERROR in rigidbodies: element ',ielement
               write(*,*) '       belonging to set ',elset
               write(*,*) '       has not been defined'
               stop
            endif
            if(ipkon(ielement).lt.0) cycle
            indexe=ipkon(ielement)
            if(lakon(ielement)(4:4).eq.'2') then
               nope=20
            elseif(lakon(ielement)(4:4).eq.'8') then
               nope=8
            elseif(lakon(ielement)(4:5).eq.'10') then
               nope=10
            elseif(lakon(ielement)(4:4).eq.'4') then
               nope=4
            elseif(lakon(ielement)(4:5).eq.'15') then
               nope=15
            else
               nope=6
            endif
            do k=indexe+1,indexe+nope
               node=kon(k)
               if((node.eq.irefnode).or.(node.eq.irotnode)) cycle
               do j=1,3
                  idof=7*(node-1)+j
                  call nident(ikmpc,idof,nmpc,id)
                  if(id.gt.0) then
                     if(ikmpc(id).eq.idof) then
                        write(*,*)'*WARNING in rigidbodies: dof ',j,'of 
     &node ',node,' belonging to a'
                        write(*,*)'         rigid body is detected on th
     &e dependent side of another'
                        write(*,*)'         equation; no rigid body cons
     &trained applied'
                     endif
                  endif
               enddo
            enddo
         enddo
      endif
!
!     generating the equations in basis form
!
!     node set
!
      if(inoset.ne.0) then
         do i=istartset(inoset),iendset(inoset)
            node=ialset(i)
            if(node.gt.0) then
               if((node.eq.irefnode).or.(node.eq.irotnode)) cycle
               call rigidmpc(ipompc,nodempc,coefmpc,irefnode,irotnode,
     &              labmpc,nmpc,nmpc_,mpcfree,ikmpc,ilmpc,nk,nk_,
     &              nodeboun,ndirboun,ikboun,ilboun,nboun,nboun_,node)
            else
               node=ialset(i-2)
               do
                  node=node-ialset(i)
                  if(node.ge.ialset(i-1)) exit
                  if((node.eq.irefnode).or.(node.eq.irotnode)) cycle
                  call rigidmpc(ipompc,nodempc,coefmpc,irefnode,
     &                 irotnode,labmpc,nmpc,nmpc_,mpcfree,ikmpc,ilmpc,
     &                 nk,nk_,nodeboun,ndirboun,ikboun,ilboun,nboun,
     &                 nboun_,node)
               enddo
            endif
         enddo
      endif
!
!     element set
!
      if(ielset.ne.0) then
         do i=istartset(ielset),iendset(ielset)
            ielement=ialset(i)
            if(ielement.gt.0) then
               if(ipkon(ielement).lt.0) cycle
               indexe=ipkon(ielement)
               if(lakon(ielement)(4:4).eq.'2') then
                  nope=20
               elseif(lakon(ielement)(4:4).eq.'8') then
                  nope=8
               elseif(lakon(ielement)(4:5).eq.'10') then
                  nope=10
               elseif(lakon(ielement)(4:4).eq.'4') then
                  nope=4
               elseif(lakon(ielement)(4:5).eq.'15') then
                  nope=15
               else
                  nope=6
               endif
               do k=indexe+1,indexe+nope
                  node=kon(k)
                  if((node.eq.irefnode).or.(node.eq.irotnode)) cycle
                   call rigidmpc(ipompc,nodempc,coefmpc,irefnode,
     &                 irotnode,labmpc,nmpc,nmpc_,mpcfree,ikmpc,ilmpc,
     &                 nk,nk_,nodeboun,ndirboun,ikboun,ilboun,nboun,
     &                 nboun_,node)
               enddo
            else
               ielement=ialset(i-2)
               do
                  ielement=ielement-ialset(i)
                  if(ielement.ge.ialset(i-1)) exit
                  if(ipkon(ielement).lt.0) cycle
                  indexe=ipkon(ielement)
                  if(lakon(ielement)(4:4).eq.'2') then
                     nope=20
                  elseif(lakon(ielement)(4:4).eq.'8') then
                     nope=8
                  elseif(lakon(ielement)(4:5).eq.'10') then
                     nope=10
                  elseif(lakon(ielement)(4:4).eq.'4') then
                     nope=4
                  elseif(lakon(ielement)(4:5).eq.'15') then
                     nope=15
                  else
                     nope=6
                  endif
                  do k=indexe+1,indexe+nope
                     node=kon(k)
                     if((node.eq.irefnode).or.(node.eq.irotnode)) cycle
                     call rigidmpc(ipompc,nodempc,coefmpc,irefnode,
     &                    irotnode,labmpc,nmpc,nmpc_,mpcfree,ikmpc,
     &                    ilmpc,nk,nk_,nodeboun,ndirboun,ikboun,ilboun,
     &                    nboun,nboun_,node)
                  enddo
               enddo
            endif
         enddo
      endif
!
      call getnewline(text,textpart,istat,in,n,key)
!
      return
      end

