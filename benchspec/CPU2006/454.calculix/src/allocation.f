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
      subroutine allocation(nload,nforc,nboun,nk,ne,nmpc,
     &  nset,nalset,nmat,ntmat,npmat,norien,nam,noprint,
     &  neprint,mint,ntrans,in,itread,set,meminset,rmeminset,ncs,
     &  namtot,ncmat,memmpc,ne1d,ne2d,nflow)
!
!     calculates a conservative estimate of the size of the 
!     fields to be allocated
!
!     the underscores were dropped since they caused problems in the
!     DDD debugger. 
!
!     meminset=total # of terms in mpc's
!     rmeminset=total # of reduced terms (due to use of generate) in
!               mpc's
!
      implicit none
!
      logical igen
      character*1 ctext
      character*5 llab
      character*8 label
      character*20 mpclabel
      character*21 set(*),noset,elset,leftset,rightset,noelset
      character*40 textpart(16)
      character*132 text
!
      integer nload,nforc,nboun,nk,ne,nmpc,nset,nalset,
     &  nmat,ntmat,npmat,norien,nam,noprint,neprint,in,kode,
     &  itread,istat,n,key,meminset(*),i,js,inoset,mint,ii,
     &  ibounstart,ibounend,ibound,ntrans,ntmatl,npmatl,ityp,l,
     &  ielset,nope,nteller,nterm,ialset(16),ncs,rmeminset(*),
     &  ileftset,irightset,namtot,ncmat,nconstants,memmpc,j,ipos,
     &  maxrmeminset,ne1d,ne2d,necpe8r,necps8r,necax8r,nes8r,
     &  neb32,nn,nflow,nradiate
!
      real*8 temperature,tempact
!
      if(itread.eq.0) then
         do
            read(in,'(a132)',iostat=istat) text
            if(istat.lt.0) then
               if(text.eq.'') then
                  if(in.ne.1) then
                     close(in)
                     in=in-1
                     cycle
                  else
                     exit
                  endif
               else
                  istat=0
               endif
            endif
!
!           removing blanks and switching to upper case before the
!           first comma
!
            j=0
            do i=1,132
               ctext=text(i:i)
               if(ctext.eq.',') exit
               if(ctext.eq.' ') cycle
               if((ichar(ctext).ge.97).and.(ichar(ctext).le.122))
     &             ctext=char(ichar(ctext)-32)
               j=j+1
               text(j:j)=ctext
            enddo
!
            if(text(1:8).eq.'*INCLUDE') then
               call include(text,in)
            elseif((text(1:5).eq.'*NODE').and.
     &             (text(1:10).ne.'*NODEPRINT').and.
     &             (text(1:9).ne.'*NODEFILE')) then
               nset=nset+1
            elseif(text(1:8).eq.'*ELEMENT') then
               nset=nset+1
            elseif(text(1:6).eq.'*ELSET') then
               nset=nset+1
            elseif(text(1:5).eq.'*NSET') then
               nset=nset+1
            elseif(text(1:8).eq.'*SURFACE') then
               nset=nset+1
            endif
            do i=1,132
               text(i:i)=' '
            enddo
         enddo
         rewind(in)
!
      else
!
         nset=0
         maxrmeminset=0
         necpe8r=0
         necps8r=0
         necax8r=0
         nes8r=0
         neb32=0
         nradiate=0
!
         call getnewline(text,textpart,istat,in,n,key)
         do
            if(istat.lt.0) exit
!
            if(textpart(1)(1:10).eq.'*AMPLITUDE') then
               nam=nam+1
               do
                  call getnewline(text,textpart,istat,in,n,key)
                  if((istat.lt.0).or.(key.eq.1)) exit
                  namtot=namtot+4
               enddo
            elseif(textpart(1)(1:9).eq.'*BOUNDARY') then
               do
                  call getnewline(text,textpart,istat,in,n,key)
                  if((istat.lt.0).or.(key.eq.1)) exit
!
                  read(textpart(2),'(i40)',iostat=istat) ibounstart
                  if(istat.gt.0) call inputerror(text)
!
                  if(textpart(3).eq.
     &                 '                                        ') then
                     ibounend=ibounstart
                  else
                     read(textpart(3),'(i40)',iostat=istat) ibounend
                     if(istat.gt.0) call inputerror(text)
                  endif
                  ibound=ibounend-ibounstart+1
                  ibound=max(1,ibound)
                  ibound=min(3,ibound)
!
                  read(textpart(1),'(i40)',iostat=istat) l
                  if(istat.eq.0) then
                     nboun=nboun+ibound
                     if(ntrans.gt.0) then
                        nmpc=nmpc+ibound
                        memmpc=memmpc+4
                        nk=nk+1
                     endif
                  else
                     read(textpart(1)(1:20),'(a20)',iostat=istat) noset
                     noset(21:21)=' '
                     ipos=index(noset,' ')
                     noset(ipos:ipos)='N'
                     do i=1,nset
                        if(set(i).eq.noset) then
                           nboun=nboun+ibound*meminset(i)
                           if(ntrans.gt.0)then
                              nmpc=nmpc+ibound*meminset(i)
                              memmpc=memmpc+4*ibound*meminset(i)
                              nk=nk+meminset(i)
                           endif
                           exit
                        endif
                     enddo
                  endif
               enddo
            elseif(textpart(1)(1:6).eq.'*CFLUX') then
               do
                  call getnewline(text,textpart,istat,in,n,key)
                  if((istat.lt.0).or.(key.eq.1)) exit
!
                  read(textpart(1),'(i40)',iostat=istat) l
                  if(istat.eq.0) then
                     nforc=nforc+1
                  else
                     read(textpart(1)(1:20),'(a20)',iostat=istat) noset
                     noset(21:21)=' '
                     ipos=index(noset,' ')
                     noset(ipos:ipos)='N'
                     do i=1,nset
                        if(set(i).eq.noset) then
                           nforc=nforc+meminset(i)
                           exit
                        endif
                     enddo
                  endif
               enddo
            elseif(textpart(1)(1:6).eq.'*CLOAD') then
               do
                  call getnewline(text,textpart,istat,in,n,key)
                  if((istat.lt.0).or.(key.eq.1)) exit
!
                  read(textpart(1),'(i40)',iostat=istat) l
                  if(istat.eq.0) then
                     if(ntrans.eq.0) then
                        nforc=nforc+1
                     else
                        nforc=nforc+3
                     endif
                  else
                     read(textpart(1)(1:20),'(a20)',iostat=istat) noset
                     noset(21:21)=' '
                     ipos=index(noset,' ')
                     noset(ipos:ipos)='N'
                     do i=1,nset
                        if(set(i).eq.noset) then
                           if(ntrans.eq.0) then
                              nforc=nforc+meminset(i)
                           else
                              nforc=nforc+3*meminset(i)
                           endif
                           exit
                        endif
                     enddo
                  endif
               enddo
            elseif(textpart(1)(1:6).eq.'*CREEP') then
               ncmat=max(4,ncmat)
               npmat=max(2,npmat)
               call getnewline(text,textpart,istat,in,n,key)
            elseif(textpart(1)(1:16).eq.'*CYCLICHARDENING') then
               ntmatl=0
               do
                  call getnewline(text,textpart,istat,in,n,key)
                  if((istat.lt.0).or.(key.eq.1)) exit
                  read(textpart(3),'(f40.0)',iostat=istat) temperature
                  if(istat.gt.0) call inputerror(text)
                  if(ntmatl.eq.0) then
                     npmatl=0
                     ntmatl=ntmatl+1
                     ntmat=max(ntmatl,ntmat)
                     tempact=temperature
                  elseif(temperature.ne.tempact) then
                     npmatl=0
                     ntmatl=ntmatl+1
                     ntmat=max(ntmatl,ntmat)
                     tempact=temperature
                  endif
                  npmatl=npmatl+1
                  npmat=max(npmatl,npmat)
               enddo
            elseif(textpart(1)(1:20).eq.'*CYCLICSYMMETRYMODEL') then
               nmpc=nmpc+3*ncs
               memmpc=memmpc+18*ncs
               call getnewline(text,textpart,istat,in,n,key)
            elseif(textpart(1)(1:22).eq.'*DEFORMATIONPLASTICITY') then
               ncmat=max(5,ncmat)
               ntmatl=0
               do
                  call getnewline(text,textpart,istat,in,n,key)
                  if((istat.lt.0).or.(key.eq.1)) exit
                  ntmatl=ntmatl+1
                  ntmat=max(ntmatl,ntmat)
               enddo
            elseif((textpart(1)(1:8).eq.'*DENSITY').or.
     &              (textpart(1)(1:13).eq.'*SPECIFICHEAT')) then
               ntmatl=0
               do
                  call getnewline(text,textpart,istat,in,n,key)
                  if((istat.lt.0).or.(key.eq.1)) exit
                  ntmatl=ntmatl+1
                  ntmat=max(ntmatl,ntmat)
               enddo
            elseif((textpart(1)(1:6).eq.'*DLOAD').or.
     &             (textpart(1)(1:6).eq.'*DFLUX').or.
     &             (textpart(1)(1:5).eq.'*FILM')) then
               do
                  call getnewline(text,textpart,istat,in,n,key)
                  if((istat.lt.0).or.(key.eq.1)) exit
                  read(textpart(2)(1:5),'(a5)',iostat=istat) llab
                  if((llab.eq.'GRAV ').or.(llab.eq.'CENTR')) exit
                  read(textpart(1),'(i40)',iostat=istat) l
                  if(istat.eq.0) then
                     nload=nload+1
                  else
                     read(textpart(1)(1:20),'(a20)',iostat=istat) elset
                     elset(21:21)=' '
                     ipos=index(elset,' ')
                     elset(ipos:ipos)='E'
                     do i=1,nset
                        if(set(i).eq.elset) then
                           nload=nload+meminset(i)
                           exit
                        endif
                     enddo
                  endif
               enddo
            elseif(textpart(1)(1:8).eq.'*DYNAMIC') then
               if((mint.eq.1).or.(mint.eq.8)) then
                  mint=27
               elseif(mint.eq.4) then
                  mint=15
               else
                  mint=18
               endif
               call getnewline(text,textpart,istat,in,n,key)
            elseif(textpart(1)(1:8).eq.'*ELPRINT') then
               do ii=2,n
                  if(textpart(ii)(1:6).eq.'ELSET=') then
                     read(textpart(ii)(7:26),'(a20)',iostat=istat) elset
                     elset(21:21)=' '
                     ipos=index(elset,' ')
                     elset(ipos:ipos)='E'
                     do i=1,nset
                        if(set(i).eq.elset) then
                           neprint=neprint+meminset(i)
                           exit
                        endif
                     enddo
                  endif
               enddo
               call getnewline(text,textpart,istat,in,n,key)
            elseif(textpart(1)(1:8).eq.'*ELASTIC') then
               ntmatl=0
               ityp=2
               ncmat=max(2,ncmat)
               do i=2,n
                  if(textpart(i)(1:5).eq.'TYPE=') then
                     if(textpart(i)(6:8).eq.'ISO') then
                        ityp=2
                        ncmat=max(2,ncmat)
                     elseif(textpart(i)(6:10).eq.'ORTHO') then
                        ityp=9
                        ncmat=max(9,ncmat)
                     elseif(textpart(i)(6:10).eq.'ANISO') then
                        ityp=21
                        ncmat=max(21,ncmat)
                     endif
                     exit
                  endif
               enddo
               if(ityp.eq.2) then
                  do
                     call getnewline(text,textpart,istat,in,n,key)
                     if((istat.lt.0).or.(key.eq.1)) exit
                     ntmatl=ntmatl+1
                     ntmat=max(ntmatl,ntmat)
                  enddo
               elseif(ityp.eq.9) then
                  do
                     call getnewline(text,textpart,istat,in,n,key)
                     if((istat.lt.0).or.(key.eq.1)) exit
                     ntmatl=ntmatl+1
                     ntmat=max(ntmatl,ntmat)
                     read(in,*)
                  enddo
               elseif(ityp.eq.21) then
                  do
                     call getnewline(text,textpart,istat,in,n,key)
                     if((istat.lt.0).or.(key.eq.1)) exit
                     ntmatl=ntmatl+1
                     ntmat=max(ntmatl,ntmat)
                     read(in,*)
                     read(in,*)
                  enddo
               endif
            elseif(textpart(1)(1:8).eq.'*ELEMENT') then
               ielset=0
!
               loop1: do i=2,n
                  if(textpart(i)(1:6).eq.'ELSET=') then
                     elset=textpart(i)(7:26)
                     elset(21:21)=' '
                     ipos=index(elset,' ')
                     elset(ipos:ipos)='E'
                     ielset=1
                     do js=1,nset
                        if(set(js).eq.elset) exit
                     enddo
                     if(js.gt.nset) then
                        nset=nset+1
                        set(nset)=elset
                     endif
                  elseif(textpart(i)(1:5).eq.'TYPE=') then
                     read(textpart(i)(6:11),'(a8)') label
                     if(label.eq.'        ') then
                        write(*,*) 
     &                   '*ERROR in allocation: element type is lacking'
                        write(*,*) '       card image:'
                        write(*,'(a132)') text
                        stop
                     endif
                  endif
                  if(label.eq.'C3D20   ') then
                     mint=max(mint,27)
                     nope=20
                  elseif(label.eq.'C3D20R  ') then
                     mint=max(mint,8)
                     nope=20
                  elseif(label.eq.'C3D8R   ') then
                     mint=max(mint,1)
                     nope=8
                  elseif(label.eq.'C3D10   ') then
                     mint=max(mint,4)
                     nope=10
                  elseif(label.eq.'C3D4    ') then
                     mint=max(mint,1)
                     nope=4
                  elseif(label.eq.'C3D15   ') then
                     mint=max(mint,9)
                     nope=15
                  elseif(label.eq.'C3D6    ') then
                     mint=max(mint,2)
                     nope=6
                  elseif((label.eq.'CPE8R   ').or.
     &                   (label.eq.'CPS8R   ').or.
     &                   (label.eq.'CAX8R   ').or.
     &                   (label.eq.'S8R     ').or.
     &                   (label.eq.'C3D8    ')) then
                     mint=max(mint,8)
                     nope=8
                  elseif((label.eq.'CPE8    ').or.
     &                   (label.eq.'CPS8    ').or.
     &                   (label.eq.'CAX8    ').or.
     &                   (label.eq.'S8      ')) then
                     mint=max(mint,27)
                     nope=8
                  elseif(label.eq.'B32     ') then
                     mint=max(mint,27)
                     nope=3
                  elseif(label.eq.'B32R    ') then
                     mint=max(mint,8)
                     nope=3
                  endif
               enddo loop1
!
               loop2:do
                  call getnewline(text,textpart,istat,in,n,key)
                  if((istat.lt.0).or.(key.eq.1)) exit
                  read(textpart(1),'(i40)',iostat=istat) i
                  if(istat.gt.0) call inputerror(text)
                  if(label(1:2).ne.'C3') then
                     if(label(1:3).eq.'CPE') then
                        necpe8r=necpe8r+1
                     elseif(label(1:2).eq.'CP') then
                        necps8r=necps8r+1
                     elseif(label(1:1).eq.'C') then
                        necax8r=necax8r+1
                     elseif(label(1:1).eq.'S') then
                        nes8r=nes8r+1
                     elseif(label(1:1).eq.'B') then
                        neb32=neb32+1
                     endif
                  endif
                  nteller=n-1
                  if(nteller.lt.nope) then
                     do
                        call getnewline(text,textpart,istat,in,n,key)
                        if((istat.lt.0).or.(key.eq.1)) exit loop2
                        if(nteller+n.gt.nope) n=nope-nteller
                        nteller=nteller+n
                        if(nteller.eq.nope) exit
                     enddo
                  endif
                  ne=max(ne,i)
                  if(ielset.eq.1) then
                     meminset(js)=meminset(js)+1
                     rmeminset(js)=rmeminset(js)+1
                  endif
               enddo loop2
            elseif((textpart(1)(1:5).eq.'*NSET').or.
     &              (textpart(1)(1:6).eq.'*ELSET')) then
               if(textpart(1)(1:5).eq.'*NSET')
     &              then
                  noelset=textpart(2)(6:25)
                  noelset(21:21)=' '
                  ipos=index(noelset,' ')
                  noelset(ipos:ipos)='N'
                  kode=0
               else
                  noelset=textpart(2)(7:26)
                  noelset(21:21)=' '
                  ipos=index(noelset,' ')
                  noelset(ipos:ipos)='E'
                  kode=1
               endif
!
!              check whether new set name or old one
!
               do js=1,nset
                  if(set(js).eq.noelset) exit
               enddo
               if(js.gt.nset) then
                  nset=nset+1
                  set(nset)=noelset
                  nn=nset
               else
                  nn=js
               endif
!
               if((n.gt.2).and.(textpart(3)(1:8).eq.'GENERATE')) then
                  igen=.true.
               else
                  igen=.false.
               endif
               do
                  call getnewline(text,textpart,istat,in,n,key)
                  if((istat.lt.0).or.(key.eq.1)) exit
                  if(igen) then
                     if(textpart(3).eq.
     &                    '                                        ')
     &                    textpart(3)='1'
                     do i=1,3
                        read(textpart(i),'(i40)',iostat=istat) ialset(i)
                        if(istat.gt.0) call inputerror(text)
                     enddo
                     meminset(nn)=meminset(nn)+
     &                    (ialset(2)-ialset(1))/ialset(3)+1
                     rmeminset(nn)=rmeminset(nn)+3
                  else
                     do i=1,n
                        read(textpart(i),'(i40)',iostat=istat) ialset(i)
                        if(istat.gt.0) then
                           noelset=textpart(i)(1:20)
                           noelset(21:21)=' '
                           ipos=index(noelset,' ')
                           if(kode.eq.0) then
                              noelset(ipos:ipos)='N'
                           else
                              noelset(ipos:ipos)='E'
                           endif
                           do j=1,nset
                              if(noelset.eq.set(j)) then
                                 meminset(nn)=meminset(nn)+
     &                                   meminset(j)
                                 rmeminset(nn)=rmeminset(nn)+
     &                                   rmeminset(j)
                                 exit
                              endif
                           enddo
                        else
                           meminset(nn)=meminset(nn)+1
                           rmeminset(nn)=rmeminset(nn)+1
                        endif
                     enddo
                  endif
               enddo
            elseif(textpart(1)(1:9).eq.'*EQUATION') then
               do
                  call getnewline(text,textpart,istat,in,n,key)
                  if((istat.lt.0).or.(key.eq.1)) exit 
                  read(textpart(1),'(i40)',iostat=istat) nterm
                  if(ntrans.eq.0) then
                     nmpc=nmpc+1
                     memmpc=memmpc+nterm
                  else
                     nmpc=nmpc+3
                     memmpc=memmpc+3*nterm
                  endif
                  ii=0
                  do
                     call getnewline(text,textpart,istat,in,n,key)
                     if((istat.lt.0).or.(key.eq.1)) exit
                     ii=ii+n/3
                     if(ii.eq.nterm) exit
                  enddo
               enddo
            elseif((textpart(1)(1:10).eq.'*EXPANSION').or.
     &             (textpart(1)(1:13).eq.'*CONDUCTIVITY')) then
               ntmatl=0
               do
                  call getnewline(text,textpart,istat,in,n,key)
                  if((istat.lt.0).or.(key.eq.1)) exit
                  ntmatl=ntmatl+1
                  ntmat=max(ntmatl,ntmat)
               enddo
            elseif(textpart(1)(1:13).eq.'*HYPERELASTIC') then
               ntmatl=0
               ityp=-7
               do i=2,n
                  if(textpart(i)(1:12).eq.'ARRUDA-BOYCE') then
                     ityp=-1
                     ncmat=max(3,ncmat)
                  elseif(textpart(i)(1:13).eq.'MOONEY-RIVLIN') then
                     ityp=-2
                     ncmat=max(3,ncmat)
                  elseif(textpart(i)(1:8).eq.'NEOHOOKE') then
                     ityp=-3
                     ncmat=max(2,ncmat)
                  elseif(textpart(i)(1:5).eq.'OGDEN') then
                     ityp=-4
                     ncmat=max(3,ncmat)
                  elseif(textpart(i)(1:10).eq.'POLYNOMIAL') then
                     ityp=-7
                     ncmat=max(3,ncmat)
                  elseif(textpart(i)(1:17).eq.'REDUCEDPOLYNOMIAL')
     &               then
                     ityp=-10
                     ncmat=max(2,ncmat)
                  elseif(textpart(i)(1:11).eq.'VANDERWAALS') then
                     ityp=-13
                     ncmat=max(5,ncmat)
                  elseif(textpart(i)(1:4).eq.'YEOH') then
                     ityp=-14
                     ncmat=max(6,ncmat)
                  elseif(textpart(i)(1:2).eq.'N=') then
                     if(textpart(i)(3:3).eq.'1') then
                     elseif(textpart(i)(3:3).eq.'2') then
                        if(ityp.eq.-4) then
                           ityp=-5
                           ncmat=max(6,ncmat)
                        elseif(ityp.eq.-7) then
                           ityp=-8
                           ncmat=max(7,ncmat)
                        elseif(ityp.eq.-10) then
                           ityp=-11
                           ncmat=max(4,ncmat)
                        endif
                     elseif(textpart(i)(3:3).eq.'3') then
                        if(ityp.eq.-4) then
                           ityp=-6
                           ncmat=max(9,ncmat)
                        elseif(ityp.eq.-7) then
                           ityp=-9
                           ncmat=max(12,ncmat)
                        elseif(ityp.eq.-10) then
                           ityp=-12
                           ncmat=max(6,ncmat)
                        endif
                     endif
                  endif
               enddo
               if((ityp.ne.-6).and.(ityp.ne.-9)) then
                  do
                     call getnewline(text,textpart,istat,in,n,key)
                     if((istat.lt.0).or.(key.eq.1)) exit
                     ntmatl=ntmatl+1
                     ntmat=max(ntmat,ntmatl)
                  enddo
               else
                  do
                     call getnewline(text,textpart,istat,in,n,key)
                     if((istat.lt.0).or.(key.eq.1)) exit
                     ntmatl=ntmatl+1
                     ntmat=max(ntmat,ntmatl)
                     read(in,*)
                  enddo
               endif
            elseif(textpart(1)(1:10).eq.'*HYPERFOAM') then
               ntmatl=0
               ityp=-15
               ncmat=max(3,ncmat)
               do i=2,n
                  if(textpart(i)(1:2).eq.'N=') then
                     if(textpart(i)(3:3).eq.'1') then
                     elseif(textpart(i)(3:3).eq.'2') then
                        ityp=-16
                        ncmat=max(6,ncmat)
                     elseif(textpart(i)(3:3).eq.'3') then
                        ityp=-17
                        ncmat=max(9,ncmat)
                     endif
                  endif
               enddo
               if(ityp.ne.-17) then
                  do
                     call getnewline(text,textpart,istat,in,n,key)
                     if((istat.lt.0).or.(key.eq.1)) exit
                     ntmatl=ntmatl+1
                     ntmat=max(ntmat,ntmatl)
                  enddo
               else
                  do
                     call getnewline(text,textpart,istat,in,n,key)
                     if((istat.lt.0).or.(key.eq.1)) exit
                     ntmatl=ntmatl+1
                     ntmat=max(ntmat,ntmatl)
                     read(in,*)
                  enddo
               endif
            elseif(textpart(1)(1:12).eq.'*MASSFLOWRATE') then
               do
                  call getnewline(text,textpart,istat,in,n,key)
                  if((istat.lt.0).or.(key.eq.1)) exit
                  nflow=nflow+1
               enddo
            elseif(textpart(1)(1:9).eq.'*MATERIAL') then
               nmat=nmat+1
               call getnewline(text,textpart,istat,in,n,key)
            elseif((textpart(1)(1:5).eq.'*NODE').and.
     &             (textpart(1)(1:10).ne.'*NODEPRINT').and.
     &             (textpart(1)(1:9).ne.'*NODEFILE')) then
               inoset=0
               loop3: do i=2,n
                  if(textpart(i)(1:5).eq.'NSET=') then
                     noset=textpart(i)(6:25)
                     noset(21:21)=' '
                     ipos=index(noset,' ')
                     noset(ipos:ipos)='N'
                     inoset=1
                     do js=1,nset
                        if(set(js).eq.noset) exit
                     enddo
                     if(js.gt.nset) then
                        nset=nset+1
                        set(nset)=noset
                     endif
                  endif
               enddo loop3
!
               do
                  call getnewline(text,textpart,istat,in,n,key)
                  if((istat.lt.0).or.(key.eq.1)) exit
                  read(textpart(1),'(i40)',iostat=istat) i
                  if(istat.gt.0) call inputerror(text)
                  nk=max(nk,i)
                  if(inoset.eq.1) then
                     meminset(js)=meminset(js)+1
                     rmeminset(js)=rmeminset(js)+1
                  endif
               enddo
            elseif(textpart(1)(1:4).eq.'*MPC') then
               mpclabel='                    '
               do
                  call getnewline(text,textpart,istat,in,n,key)
                  if((istat.lt.0).or.(key.eq.1)) exit
                  do i=1,n
                     read(textpart(i),'(i40)',iostat=istat) ialset(i)
                     if(mpclabel.eq.'                    ')
     &                    mpclabel=textpart(i)(1:20)
                     if((mpclabel(1:8).ne.'STRAIGHT').and.
     &                    (mpclabel(1:4).ne.'PLANE')) then
                        nk=nk+1
                        nmpc=nmpc+1
                        nboun=nboun+1
                     endif
                     if(istat.gt.0) then
                        noelset=textpart(i)(1:20)
                        noelset(21:21)=' '
                        ipos=index(noelset,' ')
                        noelset(ipos:ipos)='N'
                        do j=1,nset
                           if(noelset.eq.set(j)) then
                              if(mpclabel(1:8).eq.'STRAIGHT') then
                                 nk=nk+2*meminset(j)
                                 nmpc=nmpc+2*meminset(j)
                                 nboun=nboun+2*meminset(j)
                                 memmpc=memmpc+14*meminset(j)
                              elseif(mpclabel(1:5).eq.'PLANE') then
                                 nk=nk+meminset(j)
                                 nmpc=nmpc+meminset(j)
                                 nboun=nboun+meminset(j)
                                 memmpc=memmpc+13*meminset(j)
                              else
                                 memmpc=memmpc+meminset(j)
                              endif
                              exit
                           endif
                        enddo
                     else
                        if(mpclabel(1:8).eq.'STRAIGHT') then
                           nk=nk+2
                           nmpc=nmpc+2
                           nboun=nboun+2
                           memmpc=memmpc+14
                        elseif(mpclabel(1:5).eq.'PLANE') then
                           nk=nk+1
                           nmpc=nmpc+1
                           nboun=nboun+1
                           memmpc=memmpc+13
                        else
                           memmpc=memmpc+1
                        endif
                     endif
                  enddo
               enddo
            elseif(textpart(1)(1:10).eq.'*NODEPRINT') then
               do ii=2,n
                  if(textpart(ii)(1:5).eq.'NSET=') then
                     noset=textpart(ii)(6:25)
                     noset(21:21)=' '
                     ipos=index(noset,' ')
                     noset(ipos:ipos)='N'
                     do i=1,nset
                        if(set(i).eq.noset) then
                           noprint=noprint+meminset(i)
                           exit
                        endif
                     enddo
                  endif
               enddo
               call getnewline(text,textpart,istat,in,n,key)
            elseif(textpart(1)(1:12).eq.'*ORIENTATION') then
               norien=norien+1
               call getnewline(text,textpart,istat,in,n,key)
            elseif(textpart(1)(1:8).eq.'*PLASTIC') then
               ntmatl=0
               do
                  call getnewline(text,textpart,istat,in,n,key)
                  if((istat.lt.0).or.(key.eq.1)) exit
                  read(textpart(3),'(f40.0)',iostat=istat) temperature
                  if(istat.gt.0) call inputerror(text)
                  if(ntmatl.eq.0) then
                     npmatl=0
                     ntmatl=ntmatl+1
                     ntmat=max(ntmatl,ntmat)
                     tempact=temperature
                  elseif(temperature.ne.tempact) then
                     npmatl=0
                     ntmatl=ntmatl+1
                     ntmat=max(ntmatl,ntmat)
                     tempact=temperature
                  endif
                  npmatl=npmatl+1
                  npmat=max(npmatl,npmat)
               enddo
            elseif(textpart(1)(1:8).eq.'*RADIATE') then
               do
                  call getnewline(text,textpart,istat,in,n,key)
                  if((istat.lt.0).or.(key.eq.1)) exit
                  read(textpart(2)(1:5),'(a5)',iostat=istat) llab
                  if((llab.eq.'GRAV ').or.(llab.eq.'CENTR')) exit
                  read(textpart(1),'(i40)',iostat=istat) l
                  if(istat.eq.0) then
                     nload=nload+1
                     nradiate=nradiate+1
                  else
                     read(textpart(1)(1:20),'(a20)',iostat=istat) elset
                     elset(21:21)=' '
                     ipos=index(elset,' ')
                     elset(ipos:ipos)='E'
                     do i=1,nset
                        if(set(i).eq.elset) then
                           nload=nload+meminset(i)
                           nradiate=nradiate+meminset(i)
                           exit
                        endif
                     enddo
                  endif
               enddo
            elseif(textpart(1)(1:10).eq.'*RIGIDBODY') then
               noset='                    '
               elset='                    '
               do i=2,n
                  if(textpart(i)(1:5).eq.'NSET=')
     &                 then
                     noset=textpart(i)(6:25)
                     noset(21:21)=' '
                     ipos=index(noset,' ')
                     noset(ipos:ipos)='N'
                     exit
                  elseif(textpart(i)(1:6).eq.'ELSET=')
     &                 then
                     elset=textpart(i)(7:26)
                     elset(21:21)=' '
                     ipos=index(elset,' ')
                     elset(ipos:ipos)='E'
                     exit
                  endif
               enddo
               if(noset.ne.'                     ') then
                  do i=1,nset
                     if(set(i).eq.noset) then
                        nk=nk+2+meminset(i)
                        nmpc=nmpc+3*meminset(i)
                        memmpc=memmpc+18*meminset(i)
                        nboun=nboun+3*meminset(i)
                     endif
                  enddo
               elseif(elset.ne.'                     ') then
                  do i=1,nset
                     if(set(i).eq.elset) then
                        nk=nk+2+20*meminset(i)
                        nmpc=nmpc+60**meminset(i)
                        memmpc=memmpc+360*meminset(i)
                        nboun=nboun+60*meminset(i)
                     endif
                  enddo
               endif
               call getnewline(text,textpart,istat,in,n,key)
            elseif(textpart(1)(1:8).eq.'*SURFACE') then
               nset=nset+1
               do i=2,n
                  if(textpart(i)(1:5).eq.'NAME=')
     &                 then
                     set(nset)=textpart(i)(6:25)
                     set(nset)(21:21)=' '
                     ipos=index(set(nset),' ')
                     set(nset)(ipos:ipos)='N'
                     exit
                  endif
               enddo
               do
                  call getnewline(text,textpart,istat,in,n,key)
                  if((istat.lt.0).or.(key.eq.1)) exit
                  read(textpart(1),'(i40)',iostat=istat) ialset(1)
                  if(istat.gt.0) then
                     noset=textpart(1)(1:20)
                     noset(21:21)=' '
                     ipos=index(noset,' ')
                     noset(ipos:ipos)='N'
                     do i=1,nset-1
                        if(set(i).eq.noset) then
                           meminset(nset)=meminset(nset)+meminset(i)
                           rmeminset(nset)=rmeminset(nset)+rmeminset(i)
                        endif
                     enddo
                  else
                     meminset(nset)=meminset(nset)+1
                     rmeminset(nset)=rmeminset(nset)+1
                  endif
               enddo
            elseif(textpart(1)(1:4).eq.'*TIE') then
               call getnewline(text,textpart,istat,in,n,key)
               if((istat.lt.0).or.(key.eq.1)) cycle
               leftset=textpart(1)(1:20)
               leftset(21:21)=' '
               ipos=index(leftset,' ')
               leftset(ipos:ipos)='N'
               rightset=textpart(2)(1:20)
               rightset(21:21)=' '
               ipos=index(rightset,' ')
               rightset(ipos:ipos)='N'
               ileftset=0
               irightset=0
               do i=1,nset
                  if(set(i).eq.leftset) then
                     ileftset=i
                  elseif(set(i).eq.rightset) then
                     irightset=i
                  endif
               enddo
               if((ileftset.ne.0).and.(irightset.ne.0)) then
                  ncs=max(meminset(ileftset),meminset(irightset))
               else
                  write(*,*) '*ERROR in allocation: either the slave'
                  write(*,*) '       set or the master set in a *TIE'
                  write(*,*) '       option or both do not exist'
                  stop
               endif
               call getnewline(text,textpart,istat,in,n,key)
            elseif(textpart(1)(1:10).eq.'*TRANSFORM') then
               ntrans=ntrans+1
               call getnewline(text,textpart,istat,in,n,key)
            elseif(textpart(1)(1:13).eq.'*USERMATERIAL') then
               ntmatl=0
               do i=2,n
                  if(textpart(i)(1:10).eq.'CONSTANTS=') then
                     read(textpart(i)(11:40),'(i40)',iostat=istat) 
     &                     nconstants
                     if(istat.gt.0) call inputerror(text)
                     ncmat=max(nconstants,ncmat)
                     exit
                  endif
               enddo
               do
                  call getnewline(text,textpart,istat,in,n,key)
                  if((istat.lt.0).or.(key.eq.1)) exit
                  ntmatl=ntmatl+1
                  ntmat=max(ntmatl,ntmat)
                  do i=2,(nconstants-1)/8+1
                     call getnewline(text,textpart,istat,in,n,key)
                  enddo
               enddo
            else
               call getnewline(text,textpart,istat,in,n,key)
            endif
         enddo
!
         do i=1,nset
            nalset=nalset+rmeminset(i)
            maxrmeminset=max(maxrmeminset,rmeminset(i))
         enddo
!
!        extra space needed for rearrangement in elements.f and
!        noelsets.f
!
         nalset=nalset+maxrmeminset
!
         nmpc=nmpc+1
         memmpc=memmpc+1
!
         ne1d=neb32
         ne2d=necpe8r+necps8r+necax8r+nes8r
!
!        providing space for the expansion of shell and beam elements
!        to genuine volume elements
!
         nk=nk+3*8*ne2d+8*3*ne1d
         if(ne1d.gt.0) then
            nboun=nboun*9
            nforc=nforc*9
         elseif(ne2d.gt.0) then
            nboun=nboun*4
            nforc=nforc*4
         endif
!
!        providing for rigid nodes
!
         nk=nk+(1+3)*8*ne2d+(1+8)*3*ne1d
         nmpc=nmpc+3*(3*8*ne2d+8*3*ne1d)
         memmpc=memmpc+18*(3*8*ne2d+8*3*ne1d)
         nboun=nboun+3*(3*8*ne2d+8*3*ne1d)
!
!        extra MPCs to avoid undefinid rotation of rigid body nodes
!        lying on a line
!
         nmpc=nmpc+3*8*ne2d+8*3*ne1d
         memmpc=memmpc+3*(3*8*ne2d+8*3*ne1d)
!
!        expanding the MPCs: 2-node MPC link (2D elements) or
!        5-node MPC link (1D elements) between nodes defined by
!        the user and generated mid-nodes
!
         nmpc=nmpc+3*ne1d+8*ne2d
         memmpc=memmpc+15*ne1d+24*ne2d
!
!        extra nodes for the radiation boundary conditions
!
         nk=nk+nradiate
!
         write(*,*) 'nload = ',nload
         write(*,*) 'nforc = ',nforc
         write(*,*) 'nboun = ',nboun
         write(*,*) 'nk = ',nk
         write(*,*) 'ne = ',ne
         write(*,*) 'ne1d = ',ne1d
         write(*,*) 'ne2d = ',ne2d
         write(*,*) 'nmpc = ',nmpc
         write(*,*) 'nset = ',nset
         write(*,*) 'nalset = ',nalset
         write(*,*) 'nmat = ',nmat
         write(*,*) 'ncmat = ',ncmat
         write(*,*) 'ntmat = ',ntmat
         write(*,*) 'npmat = ',npmat
         write(*,*) 'norien = ',norien
         write(*,*) 'nam = ',nam
         write(*,*) 'namtot = ',namtot
         write(*,*) 'noprint = ',noprint
         write(*,*) 'neprint = ',neprint
         write(*,*) 'mint = ',mint
         write(*,*) 'ntrans = ',ntrans
         write(*,*) 'ncs = ',ncs
         write(*,*) 'memmpc = ',memmpc
!
         rewind(in)
      endif
!
      return
      end
