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
      subroutine openfile(jobname,output)
!
      implicit none
!
      logical exi
      character*3 output
      character*132 jobname,fnin,fndat,fnfrd,fnsta,fnonf
      integer i
!
!     opening the input  and output file
!
      do i=1,132
         if(jobname(i:i).eq.' ') exit
      enddo
      i=i-1
!
      fnin=jobname(1:i)//'.inp'
      inquire(file=fnin,exist=exi)
      if(exi) then
         open(1,file=fnin,status='old')
      else
         write(*,*) '*ERROR in openfile: input file ',fnin
         write(*,*) 'does not exist'
         stop
      endif
!
      fndat=jobname(1:i)//'.dat'
      open(5,file=fndat,status='unknown')
      close(5,status='delete')
      open(5,file=fndat,status='new')
!
      if(output.ne.'onf') then
         fnfrd=jobname(1:i)//'.frd'
         open(7,file=fnfrd,status='unknown')
         close(7,status='delete')
         open(7,file=fnfrd,status='new')
      endif
!
      fnsta=jobname(1:i)//'.sta'
      open(8,file=fnsta,status='unknown')
      close(8,status='delete')
      open(8,file=fnsta,status='new')
      write(8,100)
      write(8,101)
 100  format('SUMMARY OF JOB INFORMATION')
 101  format('  STEP   INC   ATT  ITRS   TOT TIME  STEP TIME   INC TIME'
     &)
!
      if(output.eq.'onf') then
         fnonf=jobname(1:i)//'_500.onf'
         open(10,file=fnonf,status='unknown')
         close(10,status='delete')
         open(10,file=fnonf,status='new')
         fnonf=jobname(1:i)//'_510.onf'
         open(11,file=fnonf,status='unknown')
         close(11,status='delete')
         open(11,file=fnonf,status='new')
         fnonf=jobname(1:i)//'_520.onf'
         open(12,file=fnonf,status='unknown')
         close(12,status='delete')
         open(12,file=fnonf,status='new')
         fnonf=jobname(1:i)//'_530.onf'
         open(13,file=fnonf,status='unknown')
         close(13,status='delete')
         open(13,file=fnonf,status='new')
         fnonf=jobname(1:i)//'_540.onf'
         open(14,file=fnonf,status='unknown')
         close(14,status='delete')
         open(14,file=fnonf,status='new')
      endif
!
      return
      end
