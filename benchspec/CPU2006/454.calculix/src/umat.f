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
      subroutine umat(amat,iel,iint,kode,elconloc,eloc,eth,
     &        beta,xikl,vij,xkl,vj,ithermal,t1l,dtime,icmd,ielas,mint_,
     &        nstate_,xstateini,xstate,stre,stiff,iorien,pgauss,
     &        orab)
!
!     calculates stiffness and stresses for a user defined material
!     law
!
      implicit none
!
      character*20 amat
!
      integer ithermal,icmd,kode,ielas,iel,iint,nstate_,mint_,iorien
!
      real*8 elconloc(21),stiff(21),eloc(6),eth(6),beta(6),stre(6),
     &  vj,t1l,dtime,xkl(3,3),xikl(3,3),vij,pgauss(3),orab(7,*)
!
      real*8 xstate(nstate_,mint_,*),xstateini(nstate_,mint_,*)
!
      if(amat(1:10).eq.'ANISO_PLAS') then
!
         call umat_aniso_plas(amat,iel,iint,kode,elconloc,eloc,eth,
     &        beta,xikl,vij,xkl,vj,ithermal,t1l,dtime,icmd,ielas,mint_,
     &        nstate_,xstateini,xstate,stre,stiff,iorien,pgauss,orab)
!
      elseif(amat(1:11).eq.'ANISO_CREEP') then
!
         call umat_aniso_creep(amat(12:20),
     &        iel,iint,kode,elconloc,eloc,eth,
     &        beta,xikl,vij,xkl,vj,ithermal,t1l,dtime,icmd,ielas,mint_,
     &        nstate_,xstateini,xstate,stre,stiff,iorien,pgauss,orab)
!
      elseif(amat(1:13).eq.'ELASTIC_FIBER') then
!
         call umat_elastic_fiber(amat,iel,iint,kode,elconloc,eloc,eth,
     &        beta,xikl,vij,xkl,vj,ithermal,t1l,dtime,icmd,ielas,mint_,
     &        nstate_,xstateini,xstate,stre,stiff,iorien,pgauss,orab)
!
      elseif(amat(1:9).eq.'IDEAL_GAS') then
!
         call umat_ideal_gas(amat,iel,iint,kode,elconloc,eloc,eth,
     &        beta,xikl,vij,xkl,vj,ithermal,t1l,dtime,icmd,ielas,mint_,
     &        nstate_,xstateini,xstate,stre,stiff,iorien,pgauss,orab)
!
      elseif(amat(1:10).eq.'LIN_ISO_EL') then
!
         call umat_lin_iso_el(amat,iel,iint,kode,elconloc,eloc,eth,
     &        beta,xikl,vij,xkl,vj,ithermal,t1l,dtime,icmd,ielas,mint_,
     &        nstate_,xstateini,xstate,stre,stiff,iorien,pgauss,orab)
!
      elseif(amat(1:14).eq.'SINGLE_CRYSTAL') then
!
         call umat_single_crystal(amat,iel,iint,kode,elconloc,eloc,eth,
     &        beta,xikl,vij,xkl,vj,ithermal,t1l,dtime,icmd,ielas,mint_,
     &        nstate_,xstateini,xstate,stre,stiff,iorien,pgauss,orab)
!
      elseif(amat(1:12).eq.'TENSION_ONLY') then
!
         call umat_tension_only(amat,iel,iint,kode,elconloc,eloc,eth,
     &        beta,xikl,vij,xkl,vj,ithermal,t1l,dtime,icmd,ielas,mint_,
     &        nstate_,xstateini,xstate,stre,stiff,iorien,pgauss,orab)
!
      elseif(amat(1:4).eq.'USER') then
!
         call umat_user(amat,iel,iint,kode,elconloc,eloc,eth,
     &        beta,xikl,vij,xkl,vj,ithermal,t1l,dtime,icmd,ielas,mint_,
     &        nstate_,xstateini,xstate,stre,stiff,iorien,pgauss,orab)
      else
         write(*,*) '*ERROR in umat: no user material subroutine'
         write(*,*) '       defined for material ',amat
         stop
      endif
!
      return
      end
