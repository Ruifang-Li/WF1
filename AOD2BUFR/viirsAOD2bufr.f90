program aod2bufr

 use netcdf
 implicit none

 integer, parameter :: rows = 768, columns = 3200, channels=12, nvar=6, mxmn=35, num_str=4, imax=8
 integer :: ncid, varid, status, i, j,k,n, startRow,startColumn,ivar,nc_num,narg
 integer :: iret_hdr,unit_table,unit_out,qc,iret_ob,iret_chan,iret_aod
 integer :: idx_str(num_str),idx,idate,idx_sat,yymmdd,hhmmss,yy,mon,dd,hh,min,ss
 integer(1),dimension(columns,rows) :: qcall,qcpath
 integer(1) :: qcpathbyte(imax)


 real*8,parameter:: bmiss=10e10                                        ! missing value in GSI 
 real*8,parameter:: chan(channels)=(/&
       &0.412e-6,0.445e-6,0.488e-6,0.555e-6,0.672e-6,0.746e-6,&
       &0.865e-6,1.240e-6,1.378e-6,1.610e-6,2.250e-6,0.550e-6/)        ! units m in bufr table, um in netcdf file,  um -> m
 real*8, dimension(columns,rows) :: lat, lon, aod550 
 real*8, dimension(channels-1,columns,rows) :: aod_chan_tmp 
 real*8 :: dhr, hdr(mxmn),aod_chan(1,channels),chan1(1,channels),satID, aods,percnt,num_all,num_nega


 character*200 :: ncfile,prepfile
 character*200, dimension(:), allocatable :: ncfile_all
 character*15, dimension(nvar) :: vars
 character*4 :: ob_num,yy_str,mon_str,dd_str,hh_str,min_str,ss_str,qcflag,hr
 character*10 :: idate_str,sat,subset,yymmdd_str,hhmmss_str,substr(num_str),str



 ! GSI read_aerosol.f90
 ! character (len= 9) :: vaodchstr  = 'CHWL AOTH'
 ! character (len=69) :: vaodgstr = 'SAID CLATH CLONH YEAR MNTH DAYS HOUR MINU SOZA SOLAZI RSST VAOTQ QPLR'

 ! new viirs bufr table is used here, so change VAOTQ->AOTQ, QPLR->RETRQ, 
 ! will need to update read_aerosol.f90
 character(80):: hdstr='SAID CLATH CLONH YEAR MNTH DAYS HOUR MINU SOZA SOLAZI RSST AOTQ RETRQ'
 character(10):: chanstr='CHWL'
 character(10):: aodstr='AOTH'
 

! read arguments

 narg=iargc()
 if (narg==3) then 
   call getarg(1,hr)

   call getarg(2,ob_num)
   read(ob_num,*) nc_num

   call getarg(3,qcflag)
   read(qcflag,*) qc

   write(*,'(a20,2x,a4,2x,2i4)') 'hour fileNum  QC ',hr, nc_num,qc

 else
   write(*,*)
   write(*,*) 'Usage: aod2bufr.x  hour file_num qcflag'
   write(*,*)
   call exit(2)
 endif


! read AOD netcdf file list

 allocate(ncfile_all(nc_num))
 open(10,file='nc_list_'//hr,form='formatted',err=200)
 do i=1,nc_num+1
   read(10,'(a)',err=300,end=400) ncfile_all(i)
   write(*,'(i4,2x,a150)') i, ncfile_all(i)
 enddo

200 write(6,*) 'Openning Netcdf file failed '
    stop(444)
300 write(6,*) 'Reading Netcdf file  failed '
    stop(666)
400 i=i-1
    close(10)


! open Prepbufr file, write bufr table to it

 unit_table=10
 unit_out=50
 prepfile='VIIRS.AOD.Prepbufr.'//trim(hr)//'H.QC'//qcflag
 open(unit_table,file='viirsAOD.table')
 open(unit_out,file=prepfile,action='write',form='unformatted')
 call openbf(unit_out,'OUT',unit_table)


! AOD netcdf variables

 vars(1)="Latitude"
 vars(2)="Longitude"
 vars(3)="QCAll"
 vars(4)="AOD550"
 vars(5)="QCPath"
 vars(6)="AOD_channel"


 num_all=0.
 num_nega=0.
 chan1(1,:)=chan

! read each of AOD netcdf file and write to prepbufr 

 do k=1,nc_num

   !ncfile='../Data/20190806/JRR-AOD_v2r0_j01_s201908061549121_e201908061550366_c201908061619530.nc'
   !ncfile='../Data/20190806/JRR-AOD_v2r0_npp_s201908061637345_e201908061638587_c201908061741130.nc'
   ncfile=trim(ncfile_all(k))
   status = nf90_open(ncfile, nf90_nowrite, ncid)
   if(status /= nf90_noerr) write(*,*) 'Error to open file', status

   do ivar=1,nvar

     write(*,*) "Reading "//vars(ivar)
     status = nf90_inq_varid(ncid, vars(ivar), varid)
     if(status /= nf90_noerr) write(*,*) 'Error to get '//vars(ivar), status

     if (ivar==1) status = nf90_get_var(ncid, varid, lat, start = (/ 1, 1 /), count =(/columns,rows/))
     if (ivar==2) status = nf90_get_var(ncid, varid, lon, start = (/ 1, 1 /), count =(/columns,rows/))
     if (ivar==3) status = nf90_get_var(ncid, varid, qcall, start = (/ 1, 1 /), count =(/columns,rows/))
     if (ivar==4) status = nf90_get_var(ncid, varid, aod550, start = (/ 1, 1 /), count =(/columns,rows/))
     if (ivar==5) status = nf90_get_var(ncid, varid, qcpath, start = (/ 1, 1 /), count =(/columns,rows/))
     if (ivar==6) status = nf90_get_var(ncid, varid, aod_chan_tmp, start = (/ 1, 1,1 /), count =(/channels-1,columns,rows/))  ! channel M1 to M11
     if(status /= nf90_noerr) write(*,*) 'Error to read '//vars(ivar), status

   enddo

   status = nf90_close(ncid)
   if(status /= nf90_noerr) write(*,*) 'Error to close file '

   substr(1)="JRR"    ! not used
   substr(2)=".nc"    ! not used
   substr(3)="s2019"  ! use to read date
   substr(4)="v2r0"   ! use to read sat ID

   do i=1,num_str
     if(index(ncfile, trim(substr(i))) == 0) then
       write(*,*), substr(i), ' is not found'
       stop(111)
     else
       idx= index(ncfile, trim(substr(i)))
     endif
     idx_str(i)=idx
     !write(*,*) "indx ", substr(i),idx_str(i)
   enddo

   
   ! assign date and time
   idx=idx_str(3)+1
   yy_str=ncfile(idx:idx+4)
   mon_str=ncfile(idx+4:idx+5)
   dd_str=ncfile(idx+6:idx+7)
   hh_str=ncfile(idx+8:idx+9)
   min_str=ncfile(idx+10:idx+11)
   ss_str=ncfile(idx+12:idx+13)

   idate_str=trim(yy_str)//trim(mon_str)//trim(dd_str)//trim(hh_str)
   read(idate_str,*) idate
   read(yy_str,*)    yy
   read(mon_str,*)   mon
   read(dd_str,*)    dd
   read(hh_str,*)    hh
   read(min_str,*)   min
   read(ss_str,*)    ss


   ! assign sat ID 
   idx_sat=idx_str(4)+5
   sat=ncfile(idx_sat:idx_sat+2)
   if (sat=='j01') then
      satID=225.     ! VIIRS noaa-20: 225   
   elseif (sat=='npp') then
      satID=224.     ! VIIRS npp: 224      
   else
      write(*,*) 'unknow satellite'
      stop(222)
   endif        

   !write(*,'(a30,4i15)') 'idate yymmdd hhmmss satID:   ',idate, yymmdd, hhmmss,satID

   subset="NC008043" 

   write(*,*) 
   write(*,'(a25,2x,a4,i4,2x,a150)') 'Writing AOD  file at hour: ',hr,k, ncfile

   do i=1,columns
     do j=1,rows


       ! use qcpath to determine surface type,it's used to tune obs error in setupaod.f90,
       !  QCPath:long_name = "Flags for retrieval path (0-No/1-Yes): 
       !  bit 0: retrieval over water; bit 1: over bright land; bit 2: over glint water;
       !  bit 3: retrieval with SW scheme over land; bit 4: retrieval with SWIR scheme over land;
       !  bit 5: retrieval over bright-land algorithm" ; 

       !do n=0,imax-1
       !   qcpathbyte(n)=IBits(qcpath(i,j),n,1)   ! IBits: right justified
       !enddo
       !write(*,'(a20,2i10,12i4)') 'qcpathbyte ', i,j,qcpath(i,j),(qcpathbyte(n),n=imax-1,0,-1)
       
       
       hdr=bmiss
       aod_chan=bmiss
       hdr(1)=satID
       hdr(2)=lat(i,j)
       hdr(3)=lon(i,j)
       hdr(4)=yy
       hdr(5)=mon
       hdr(6)=dd
       hdr(7)=hh
       hdr(8)=min
       hdr(12)=qcpath(i,j)
       hdr(13)=qcall(i,j)
       
       aod_chan(1,1:11)=aod_chan_tmp(1:11,i,j)
       aod_chan(1,12)=aod550(i,j)
      

       if (qcall(i,j)==qc) then
         num_all=num_all+1.
         
         ! qc=0 is high quality in AOD netcdf obsrvation,
         ! reset to 3, in setupaod.f90, nestat = nint(qcall)+nint(smask)*10, qcall=3 is high quality
         hdr(13)=3

         ! AOD document suggests to use small positive value for negative AOD
         ! aod_chan(1,1:11) are missing if aod_chan(1,12) negative  
         if (aod_chan(1,12)<0 ) then
            num_nega=num_nega+1.
            aod_chan(1,12)=0.001   
         endif

         !write(*,'(a7,2i8,2f10.4,14f15.9)') 'chan=',i,j,lat(i,j),lon(i,j),chan1(1,:)
         !write(*,'(a7,8f15.3,3f25.10,2f5.1,i15)') 'hdr=',(hdr(ivar),ivar=1,13),idate
         !write(*,'(a7,2i8,2f10.4,13f15.4,2i4,i15)') 'obs=',i,j,lat(i,j),lon(i,j),aod_chan(1,:),aod550(i,j),qcpath(i,j),qcall(i,j),idate


         ! hdstr='SAID CLATH CLONH YEAR MNTH DAYS HOUR MINU SOZA SOLAZI RSST AOTQ RETRQ'
         ! chanstr='CHWL'   
         ! aodstr='AOTH'

         call openmb(unit_out,subset,idate)
         call ufbint(unit_out,hdr,mxmn,1,iret_hdr,hdstr)
         call ufbrep(unit_out,chan1,1,channels,iret_chan,chanstr)
         call ufbrep(unit_out,aod_chan,1,channels,iret_aod,aodstr)
         call writsb(unit_out)
       endif
       
     enddo
   enddo

 enddo ! i=k,nc_num

 percnt=num_nega/num_all
 write(*,*) " negative percent ", num_nega, num_all, percnt


 call closbf(unit_table)   
 call closbf(unit_out)   
 deallocate(ncfile_all)
end program
