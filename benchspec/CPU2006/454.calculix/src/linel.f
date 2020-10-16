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
      subroutine linel(ithermal,mattyp,beta,al,um,am1,alph,tt,elas,
     &  icmdl,exx,eyy,ezz,exy,exz,eyz,stre,anisox)
!
!     calculates stresses for linear elastic materials
!
!     icmdl=2: stress corresponding to thermal strain =
!             stress at total strain - stress at mechanical strain
!     icmdl=3: stress at mechanical strain
!
      implicit none
!
      integer ithermal,mattyp,icmdl,m1,m2,m3,m4
!
      real*8 beta(6),al,um,am1,alph(6),tt,elas(21),exx,eyy,ezz,
     &  exy,exz,eyz,stre(6),tkl(3,3),ekl(3,3),anisox(3,3,3,3)
!
      if(ithermal.eq.1) then
         if(mattyp.eq.1) then
            beta(1)=((3.d0*al+2.d0*um)*alph(1))*tt+
     &           beta(1)
            beta(2)=((3.d0*al+2.d0*um)*alph(1))*tt+
     &           beta(2)
            beta(3)=((3.d0*al+2.d0*um)*alph(1))*tt+
     &           beta(3)
         elseif(mattyp.eq.2) then
            beta(1)=(elas(1)*alph(1)+
     &           elas(2)*alph(2)+
     &           elas(4)*alph(3))*tt+beta(1)
            beta(2)=(elas(2)*alph(1)+
     &           elas(3)*alph(2)+
     &           elas(5)*alph(3))*tt+beta(2)
            beta(3)=(elas(4)*alph(1)+
     &           elas(5)*alph(2)+
     &           elas(6)*alph(3))*tt+beta(3)
         elseif(mattyp.eq.3) then
            beta(1)=(elas(1)*alph(1)+
     &           elas(2)*alph(2)+
     &           elas(4)*alph(3)+
     &           elas(7)*alph(4)+
     &           elas(11)*alph(5)+
     &           elas(16)*alph(6))*tt+beta(1)
            beta(2)=(elas(2)*alph(1)+
     &           elas(3)*alph(2)+
     &           elas(5)*alph(3)+
     &           elas(6)*alph(4)+
     &           elas(12)*alph(5)+
     &           elas(17)*alph(6))*tt+beta(2)
            beta(3)=(elas(4)*alph(1)+
     &           elas(5)*alph(2)+
     &           elas(6)*alph(3)+
     &           elas(9)*alph(4)+
     &           elas(13)*alph(5)+
     &           elas(18)*alph(6))*tt+beta(3)
            beta(4)=(elas(7)*alph(1)+
     &           elas(8)*alph(2)+
     &           elas(9)*alph(3)+
     &           elas(10)*alph(4)+
     &           elas(14)*alph(5)+
     &           elas(19)*alph(6))*tt+beta(4)
            beta(5)=(elas(11)*alph(1)+
     &           elas(12)*alph(2)+
     &           elas(13)*alph(3)+
     &           elas(14)*alph(4)+
     &           elas(15)*alph(5)+
     &           elas(20)*alph(6))*tt+beta(5)
            beta(6)=(elas(16)*alph(1)+
     &           elas(17)*alph(2)+
     &           elas(18)*alph(3)+
     &           elas(19)*alph(4)+
     &           elas(20)*alph(5)+
     &           elas(21)*alph(6))*tt+beta(6)
         endif
      endif
!   
!                 skl contains the mechanical stress (=stress at
!                 total strain - thermal strain)
!
      if(icmdl.eq.3) then
         if(mattyp.eq.1) then
            stre(1)=am1*exx+al*(eyy+ezz)-beta(1)
            stre(2)=am1*eyy+al*(exx+ezz)-beta(2)
            stre(3)=am1*ezz+al*(exx+eyy)-beta(3)
            stre(4)=um*exy
            stre(5)=um*exz
            stre(6)=um*eyz
         elseif(mattyp.eq.2) then
            stre(1)=elas(1)*exx+elas(2)*eyy+
     &           elas(4)*ezz-beta(1)
            stre(2)=elas(2)*exx+elas(3)*eyy+
     &           elas(5)*ezz-beta(2)
            stre(3)=elas(4)*exx+elas(5)*eyy+
     &           elas(6)*ezz-beta(3)
            stre(4)=elas(7)*exy
            stre(5)=elas(8)*exz
            stre(6)=elas(9)*eyz
         elseif(mattyp.eq.3) then
            ekl(1,1)=exx
            ekl(2,2)=eyy
            ekl(3,3)=ezz
            ekl(1,2)=exy/2.d0
            ekl(1,3)=exz/2.d0
            ekl(2,3)=eyz/2.d0
            ekl(2,1)=exy/2.d0
            ekl(3,1)=exz/2.d0
            ekl(3,2)=eyz/2.d0
            do m1=1,3
               do m2=1,m1
                  tkl(m1,m2)=0.d0
                  do m3=1,3
                     do m4=1,3
                        tkl(m1,m2)=tkl(m1,m2)+
     &                       anisox(m1,m2,m3,m4)*ekl(m3,m4)
                     enddo
                  enddo
               enddo
            enddo
            stre(1)=tkl(1,1)-beta(1)
            stre(2)=tkl(2,2)-beta(2)
            stre(3)=tkl(3,3)-beta(3)
            stre(4)=tkl(2,1)-beta(4)
            stre(5)=tkl(3,1)-beta(5)
            stre(6)=tkl(3,2)-beta(6)
!
         endif
      endif
!
      return
      end
