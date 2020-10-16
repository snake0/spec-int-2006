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
      subroutine massflowrates(text,textpart,nodeflow,xflow,
     &  iamflow,nflow,nflow_,amname,nam,flow_flag,istep,istat,in,n)
!
!     reading the input deck: *MASS FLOW RATE
!
      implicit none
!
      logical flow_flag
!
      integer nodeflow(2,*),nodeup,nodedo,nflow,nflow_,istep,istat,in,
     &  n,i,j,key,iamflow(*),nam,iamplitude
!
      real*8 xflow(*),xmagnitude
!
      character*20 amname(*),amplitude
      character*40 textpart(16)
      character*132 text
!
      iamplitude=0
!
      if(istep.lt.1) then
         write(*,*) '*ERROR in dfluxes: *DFLUX should only be used'
         write(*,*) '  within a STEP'
         stop
      endif
!
      do i=2,n
         if((textpart(i)(1:6).eq.'OP=NEW').and.(.not.flow_flag)) then
            do j=1,nflow
               xflow(j)=0.d0
            enddo
         elseif(textpart(i)(1:10).eq.'AMPLITUDE=') then
            read(textpart(i)(11:30),'(a20)') amplitude
            do j=1,nam
               if(amname(j).eq.amplitude) then
                  iamplitude=j
                  exit
               endif
            enddo
            if(j.gt.nam) then
               write(*,*)'*ERROR in dfluxes: nonexistent amplitude'
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
         read(textpart(1),'(i40)',iostat=istat) nodeup
         if(istat.gt.0) call inputerror(text)
         read(textpart(2),'(i40)',iostat=istat) nodedo
         if(istat.gt.0) call inputerror(text)
         read(textpart(3),'(f40.0)',iostat=istat) xmagnitude
         if(istat.gt.0) call inputerror(text)
!
         do j=1,nflow
            if(nodeflow(1,j).ne.nodeup) cycle
            if(nodeflow(2,j).ne.nodedo) cycle
            xflow(j)=xmagnitude
            iamflow(j)=iamplitude
            exit
         enddo
         if(j.gt.nflow) then
            nflow=nflow+1
            if(nflow.gt.nflow_) then
               write(*,*) '*ERROR in massflowrates: increase nflow_'
               stop
            endif
            nodeflow(1,nflow)=nodeup
            nodeflow(2,nflow)=nodedo
            xflow(nflow)=xmagnitude
            iamflow(nflow)=iamplitude
         endif
      enddo
!
      return
      end







