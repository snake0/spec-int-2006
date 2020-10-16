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
      subroutine tempload(xforcold,xforc,xforcact,iamforc,nforc,
     &  xloadold,xload,xloadact,iamload,nload,omold,om,omact,iamom,
     &  bodyfold,bodyf,bodyfact,iambodyf,t1old,t1,t1act,iamt1,nk,
     &  amta,namta,nam,ampli,time,reltime,ttime,dtime,ithermal,nmethod,
     &  xbounold,xboun,xbounact,iamboun,nboun,xflowold,xflow,xflowact,
     &  iamflow,nflow)
!
!     calculates the loading at a given time
!
      implicit none
!
      integer iamforc(*),iamload(2,*),iamom,iambodyf,iamt1(*),
     &  nam,i,istart,iend,j,nforc,nload,nk,namta(3,*),ithermal,
     &  nmethod,iamt1i,iamboun(*),nboun,iamforci,iambouni,
     &  iamflow(*),nflow,iamflowi,iamloadi1,iamloadi2
!
      real*8 xforc(*),xforcact(*),xload(2,*),xloadact(2,*),om,omact,
     &  bodyf(*),bodyfact(*),t1(*),t1act(*),amta(2,*),ampli(*),time,
     &  xforcold(*),xloadold(2,*),omold,bodyfold(*),t1old(*),reltime,
     &  xbounold(*),xboun(*),xbounact(*),ttime,dtime,reftime,
     &  xflowold(*),xflow(*),xflowact(*)

      optional ampli
!
!     if an amplitude is active, the loading is scaled according to
!     the actual time. If no amplitude is active, then the load is
!     - scaled according to the relative time for a static step
!     - applied as a step loading for a dynamic step
!
!     calculating all amplitude values for the current time
!
      do i=1,nam
         if(namta(3,i).eq.1) then
            reftime=ttime+dtime
         else
            reftime=time
         endif
         istart=namta(1,i)
         iend=namta(2,i)
         if(amta(1,istart).ge.reftime) then
            ampli(i)=amta(2,istart)
         elseif(amta(1,iend).le.reftime) then
            ampli(i)=amta(2,iend)
         else
            do j=istart+1,iend
               if(reftime.le.amta(1,j)) then
                  ampli(i)=amta(2,j-1)+(amta(2,j)-amta(2,j-1))
     &                    *(reftime-amta(1,j-1))/(amta(1,j)-amta(1,j-1))
                  exit
               endif
            enddo
         endif
      enddo
!
!     scaling the loading
!
      do i=1,nforc
         if(nam.gt.0) then
            iamforci=iamforc(i)
         else
            iamforci=0
         endif
         if(iamforci.gt.0) then
            xforcact(i)=xforc(i)*ampli(iamforci)
         elseif(nmethod.eq.1) then
            xforcact(i)=xforcold(i)+
     &         (xforc(i)-xforcold(i))*reltime
         else
            xforcact(i)=xforc(i)
         endif
      enddo
!
      do i=1,nload
         if(nam.gt.0) then
            iamloadi1=iamload(1,i)
            iamloadi2=iamload(2,i)
         else
            iamloadi1=0
            iamloadi2=0
         endif
         if(iamloadi1.gt.0) then
            xloadact(1,i)=xload(1,i)*ampli(iamloadi1)
         elseif(nmethod.eq.1) then
            xloadact(1,i)=xloadold(1,i)+
     &         (xload(1,i)-xloadold(1,i))*reltime
         else
            xloadact(1,i)=xload(1,i)
         endif
         if(iamloadi2.gt.0) then
            xloadact(2,i)=xload(2,i)*ampli(iamloadi2)
         elseif(nmethod.eq.1) then
            xloadact(2,i)=xloadold(2,i)+
     &         (xload(2,i)-xloadold(2,i))*reltime
         else
            xloadact(2,i)=xload(2,i)
         endif
      enddo
!
      if(ithermal.ge.2) then
         do i=1,nflow
            if(nam.gt.0) then
               iamflowi=iamflow(i)
            else
               iamflowi=0
            endif
            if(iamflowi.gt.0) then
               xflowact(i)=xflow(i)*ampli(iamflowi)
            elseif(nmethod.eq.1) then
               xflowact(i)=xflowold(i)+
     &              (xflow(i)-xflowold(i))*reltime
            else
               xflowact(i)=xflow(i)
            endif
         enddo
      endif
!
      if(iamom.gt.0) then
         omact=om*ampli(iamom)
      elseif(nmethod.eq.1) then
         omact=omold+(om-omold)*reltime
      else
         omact=om
      endif
!
      if(iambodyf.gt.0) then
         do i=1,3
            bodyfact(i)=bodyf(i)*ampli(iambodyf)
         enddo
      elseif(nmethod.eq.1) then
         do i=1,3
            bodyfact(i)=bodyfold(i)+
     &         (bodyf(i)-bodyfold(i))*reltime
         enddo
      else
         do i=1,3
            bodyfact(i)=bodyf(i)
         enddo
      endif
!
!     scaling the boundary conditions
!
      do i=1,nboun
         if(nam.gt.0) then
            iambouni=iamboun(i)
         else
            iambouni=0
         endif
         if(iambouni.gt.0) then
            xbounact(i)=xboun(i)*ampli(iambouni)
         elseif(nmethod.eq.1) then
            xbounact(i)=xbounold(i)+
     &         (xboun(i)-xbounold(i))*reltime
         else
            xbounact(i)=xboun(i)
         endif
      enddo
!
!     scaling the temperatures
!
      if(ithermal.eq.1) then
         do i=1,nk
            if(nam.gt.0) then
               iamt1i=iamt1(i)
            else
               iamt1i=0
            endif
            if(iamt1i.gt.0) then
               t1act(i)=t1(i)*ampli(iamt1i)
            elseif(nmethod.eq.1) then
               t1act(i)=t1old(i)+(t1(i)-t1old(i))*reltime
            else
               t1act(i)=t1(i)
            endif
         enddo
      endif
!
      return
      end
