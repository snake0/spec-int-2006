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
      subroutine restartshort(nset,nload,nforc, nboun,nk,ne,
     &  nmpc,nalset,nmat,ntmat,npmat,norien,nam,noprint,neprint,mint,
     &  ntrans,ncs,namtot,ncmat,memmpc,ne1d,ne2d,nflow,
     &  set,meminset,rmeminset,jobnamec,irestartstep,icntrl,ithermal,
     &  nener,nstate_)
!
!     istartset := meminset
!     iendset := rmeminset
!
      implicit none
!
      character*21 set(*)
      character*132 fnrstrt,jobnamec
!
      integer istep,nset,nload,nforc,nboun,nk,ne,nmpc,nalset,nmat,
     &  ntmat,npmat,norien,nam,noprint,neprint,mint,ntrans,ncs,
     &  namtot,ncmat,memmpc,ne1d,ne2d,nflow,infree(4),
     &  nmethod,iperturb,meminset(*),rmeminset(*),
     &  i,j,k,ipos,icntrl,nener,irestartstep,im0,im1,im2,mem,iact,
     &  istat,nkon,nlabel,iplas,ithermal,nstate_,iprestr
!
      if(icntrl.eq.0) then
!
!        determining the name of the restart file
!
         ipos=index(jobnamec,char(0))
         fnrstrt(1:ipos-1)=jobnamec(1:ipos-1)
         fnrstrt(ipos:ipos+5)=".rstrt"
         do i=ipos+6,132
            fnrstrt(i:i)=' '
         enddo
!
!        opening the restart file
!
         open(15,file=fnrstrt,ACCESS='SEQUENTIAL',FORM='UNFORMATTED')
!
         do
!
            read(15,iostat=istat)istep
            if(istat.lt.0) then
               write(*,*) '*ERROR in restartshort: requested step'
               write(*,*) '       is not in the restart file'
               stop
            endif
!
!           reading the number of sets
!
            read(15)nset
!
            if(istep.eq.irestartstep) exit
!
            read(15)nalset
!
!           load size
!
            read(15)nload
            read(15)nforc
            read(15)nboun
            read(15)nflow
!
!           mesh size
!
            read(15)nk
            read(15)ne
            read(15)nkon
            read(15)mint
!
!           constraint size
!
            read(15)nmpc
            read(15)memmpc
!
!           material size
!
            read(15)nmat
            read(15)ntmat
            read(15)npmat
            read(15)ncmat
!
!           transformation size
!
            read(15)norien
            read(15)ntrans
!
!           amplitude size
!
            read(15)nam
            read(15)namtot
!
!           print size
!
            read(15)noprint
            read(15)neprint
            read(15)nlabel
!
!           cyclic symmetry size
!
            read(15)ncs
!
!           1d and 2d element size
!
            read(15)ne1d 
            read(15)ne2d 
            read(15)(infree(i),i=1,4)
!
!           procedure info
!
            read(15)nmethod
            read(15)iperturb
            read(15)nener
            read(15)iplas
            read(15)ithermal
            read(15)nstate_
            read(15)iprestr
!
!        skipping the next entries
!     
            call skip(nset,nalset,nload,nforc,nboun,nflow,nk,ne,nkon,
     &         mint,nmpc,memmpc,nmat,ntmat,npmat,ncmat,norien,
     &         ntrans,nam,noprint,neprint,nlabel,ncs,ne1d,ne2d,infree,
     &         nmethod,iperturb,nener,iplas,ithermal,nstate_,iprestr)
!
         enddo
!
         close(15)
!
         return
      endif
!
!     determining the name of the restart file
!
      ipos=index(jobnamec,char(0))
      fnrstrt(1:ipos-1)=jobnamec(1:ipos-1)
      fnrstrt(ipos:ipos+5)=".rstrt"
      do i=ipos+6,132
         fnrstrt(i:i)=' '
      enddo
!
!     opening the restart file
!
      open(15,file=fnrstrt,ACCESS='SEQUENTIAL',FORM='UNFORMATTED')
!
      do
!
         read(15,iostat=istat)istep
         if(istat.lt.0) then
            write(*,*) '*ERROR in restartshort: requested step'
            write(*,*) '       is not in the restart file'
            stop
         endif
!
!        set size
!
         read(15)nset
         read(15)nalset
!
!        load size
!
         read(15)nload
         read(15)nforc
         read(15)nboun
         read(15)nflow
!
!        mesh size
!
         read(15)nk
         read(15)ne
         read(15)nkon
         read(15)mint
!
!        constraint size
!
         read(15)nmpc
         read(15)memmpc
!
!        material size
!
         read(15)nmat
         read(15)ntmat
         read(15)npmat
         read(15)ncmat
!
!        transformation size
!
         read(15)norien
         read(15)ntrans
!
!        amplitude size
!
         read(15)nam
         read(15)namtot
!
!        print size
!
         read(15)noprint
         read(15)neprint
         read(15)nlabel
!
!        cyclic symmetry size
!
         read(15)ncs
!
!        1d and 2d element size
!
         read(15)ne1d 
         read(15)ne2d 
         read(15)(infree(i),i=1,4)
!
!        procedure info
!
         read(15)nmethod
         read(15)iperturb
         read(15)nener
         read(15)iplas
         read(15)ithermal
         read(15)nstate_
         read(15)iprestr
!
         if(istep.eq.irestartstep) exit
!
!        skipping the next entries
!     
         call skip(nset,nalset,nload,nforc,nboun,nflow,nk,ne,nkon,
     &      mint,nmpc,memmpc,nmat,ntmat,npmat,ncmat,norien,ntrans,
     &      nam,noprint,neprint,nlabel,ncs,ne1d,ne2d,infree,nmethod,
     &      iperturb,nener,iplas,ithermal,nstate_,iprestr)
!
      enddo
!
!     sets
!
      read(15)(set(i),i=1,nset)
!
!     the contents of istartset is temporarily stored in meminset
!
      read(15)(meminset(i),i=1,nset)
!
!     the contents of iendset is temporarily stored in rmeminset
!
      read(15)(rmeminset(i),i=1,nset)
!
!     reordering the information of istartset, iendset and ialset
!     into meminset and rmeminset
!
      iact=0
      do j=1,nalset
         if(iact.eq.0) then
            do k=1,nset
               if(meminset(k).eq.j) then
                  meminset(k)=0
                  mem=0
                  iact=1
                  exit
               endif
            enddo
            if(k.gt.nset) cycle
         endif
         mem=mem+1
         im2=im1
         im1=im0
         read(15) im0
         if(im0.gt.0) then
            meminset(k)=meminset(k)+1
         else
!
!           im0<0 and two elements are already stored
!
            meminset(k)=meminset(k)+(im2-im1)/im0-1
         endif
         if(rmeminset(k).eq.j) then
            iact=0
            rmeminset(k)=mem
!
!           make set k ineligible in further iterations
!
            meminset(k)=-meminset(k)
         endif
      enddo
!
!     restore the sign of meminset
!
      do k=1,nset
         meminset(k)=-meminset(k)
      enddo
!
      close(15)
!
      return
      end
