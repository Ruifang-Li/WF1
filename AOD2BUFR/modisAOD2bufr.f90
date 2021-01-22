program aod2bufr

 use netcdf
 implicit none

 integer, parameter :: rows = 768, columns = 3200, nvar=4, mxmn=35, num_str=4
 integer :: ncid, varid, status, i, j,k, startRow,startColumn,ivar,nc_num,narg
 integer :: iret_hdr,unit_table,unit_out,qc,iret_ob!,num_all,num_nega
 integer :: idx_str(num_str),idx,idate,idx_sat,yymmdd,hhmmss,yy,mon,dd,hh,min,ss
 integer, dimension(columns,rows) :: qcall

 real(8),parameter:: bmiss=10e10                            ! missing value in GSI 
 real*8, dimension(columns,rows) :: lat, lon, aod550, grid 
 real*8 :: dhr, hdr(mxmn),obs(mxmn),satID, aods,pcernt,num_all,num_nega

 character*200 :: ncfile,prepfile
 character*200, dimension(:), allocatable :: ncfile_all
 character*10, dimension(nvar) :: vars
 character*4 :: ob_num,yy_str,mon_str,dd_str,hh_str,min_str,ss_str,qcflag,hr
 character*10 :: idate_str,sat,subset,yymmdd_str,hhmmss_str,substr(num_str),str
 character(80):: hdstr='SAID  AODS  CLONH  CLATH  YEAR  MNTH  DAYS   HOUR  MINU SECO'
 character(80):: obstr='SOZA  SOLAZI SCATTA OPTD  AEROTP'
 
 ! GSI read_aerosol.f90
 !character (len= 4) :: aerostr  = 'OPTD'
 !character (len=53) :: aerogstr = 'SAID CLATH CLONH YEAR MNTH DAYS HOUR MINU SOZA SOLAZI'

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
 prepfile='MODIS.AOD.Prepbufr.'//trim(hr)//'H.QC'//qcflag
 open(unit_table,file='modisAOD.table')
 open(unit_out,file=prepfile,action='write',form='unformatted')
 call openbf(unit_out,'OUT',unit_table)


! AOD netcdf variables

 vars(1)="Latitude"
 vars(2)="Longitude"
 vars(3)="QCAll"
 vars(4)="AOD550"


 num_all=0.
 num_nega=0.

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
     if(status /= nf90_noerr) write(*,*) 'Error to read '//vars(ivar), status

   enddo

   status = nf90_close(ncid)
   if(status /= nf90_noerr) write(*,*) 'Error to close file '

   substr(1)="JRR"
   substr(2)=".nc"
   substr(3)="s2019"
   substr(4)="v2r0"

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


   idx_sat=idx_str(4)+5
   sat=ncfile(idx_sat:idx_sat+2)
   if (sat=='j01') then
      satID=783.     ! VIIRS noaa-20: 225    MODIS terra: 783
    elseif (sat=='npp') then
      satID=784.     ! VIIRS npp: 224        MODIS aqua: 784
    else
      write(*,*) 'unknow satellite'
      stop(222)
   endif        

   !write(*,'(a30,4i15)') 'idate yymmdd hhmmss satID:   ',idate, yymmdd, hhmmss,satID

   aods=1.  
   subset="NC008041" 

   ! hdstr='SAID  AODS  CLONH  CLATH  YEAR  MNTH  DAYS   HOUR  MINUSECO'
   ! obstr='SOZA  SOLAZI SCATTA OPTD  AEROTP'

   write(*,*) 
   write(*,'(a25,2x,a4,i4,2x,a150)') 'Writing AOD  file at hour: ',hr,k, ncfile
   do i=1,columns
     do j=1,rows
       hdr=bmiss
       obs=bmiss 
       hdr(1)=satID
       hdr(2)=aods
       hdr(3)=lon(i,j)
       hdr(4)=lat(i,j)
       hdr(5)=yy
       hdr(6)=mon
       hdr(7)=dd
       hdr(8)=hh
       hdr(9)=min
       hdr(10)=ss
       
       obs(4)=aod550(i,j)

       if (qcall(i,j)==qc) then
         num_all=num_all+1.
         write(*,'(a10,2i8,3f10.4,i4,i15)') 'obs=',i,j,lat(i,j),lon(i,j),aod550(i,j),qcall(i,j),idate
         !write(*,'(a4,2x,i15,10f10.3)') 'hdr=',idate,(hdr(ivar),ivar=1,10)
         if (obs(4)<0 ) then
            obs(4)=0.001        ! AOD document suggests to use small positive value for negative AOD 
            num_nega=num_nega+1.
            !write(*,'(a15,2x,i15,5f25.10)') 'negative AOD=',idate,(obs(ivar),ivar=1,5)
         endif
         call openmb(unit_out,subset,idate)
         call ufbint(unit_out,hdr,mxmn,1,iret_hdr,hdstr)
         call ufbint(unit_out,obs,mxmn,1,iret_ob,obstr)
         call writsb(unit_out)
       endif
       
     enddo
   enddo
 enddo ! i=k,nc_num

 pcernt=num_nega/num_all
 write(*,'(a15,3f15.2)') 'negative AOD ', num_nega,num_all,pcernt

 call closbf(unit_table)   
 call closbf(unit_out)   
 deallocate(ncfile_all)
end program
