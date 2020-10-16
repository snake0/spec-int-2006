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
      subroutine out(co,nk,kon,ipkon,lakon,ne,v,stn,inum,nmethod,
     &  kode,nodeflab,een,t1,fn,time,epn,ielmat,matname,enern,xstaten,
     &  nstate_,istep,iinc,iperturb,ener,mint_,output)
!
!     stores the results in frd format
!
      implicit none
!
      character*3 output
      character*4 nodeflab(*)
      character*8 lakon(*)
      character*20 matname(*)
!
      integer kon(*),inum(*),nk,ne,nmethod,kode,ipkon(*),
     &  ielmat(*),nstate_,istep,iinc,iperturb,mint_
!
      real*8 co(3,*),v(0:3,*),stn(6,*),een(6,*),t1(*),fn(0:3,*),time,
     &  epn(*),enern(*),xstaten(nstate_,*),ener(mint_,*)
!
      if((output.eq.'frd').or.(output.eq.'FRD')) then
         call frd(co,nk,kon,ipkon,lakon,ne,v,stn,inum,nmethod,
     &        kode,nodeflab,een,t1,fn,time,epn,ielmat,matname,enern,
     &        xstaten,nstate_,istep,iinc)
      else
         if(nmethod.ne.0) then
            call onf(co,nk,kon,ipkon,lakon,ne,v,stn,inum,nmethod,
     &        kode,nodeflab,een,t1,fn,time,epn,ielmat,matname,enern,
     &        xstaten,nstate_,istep,iinc,iperturb,ener,mint_)
         endif
      endif
!
      return
      end
