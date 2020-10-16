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
!                                                                              
!     identifies the position id of px in an ordered array
!     x of real numbers; 
!  
!     id is such that x(id).le.px and x(id+1).gt.px
!                                                                             
      SUBROUTINE IDENT(X,PX,N,ID)                                               
      IMPLICIT REAL*8 (A-H,O-Z)                                                
      DIMENSION X(N) 
      if(n.eq.0) then
         id=0
         return
      endif
      N1=0                                                                      
      N2=N+1                                                                    
      DO 1 I=1,5000                                                             
      M=(N2+N1)/2                                                               
      IF(PX.GE.X(M)) N1=M                                                       
      IF(PX.LT.X(M)) N2=M                                                       
      IF((N2-N1).EQ.1) GO TO 2                                                  
1     CONTINUE                                                                  
2     ID=N1                                                                     
      RETURN                                                                    
      END                                                                       
