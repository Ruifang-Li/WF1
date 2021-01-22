program prepbufr_decode_all
!
! read all observations out from prepbufr. 
! read bufr table from prepbufr file
!
 implicit none

 integer, parameter :: mxmn=35, mxlv=255
 character(80):: hdstr='SAID AODS CLONH CLATH YEAR MNTH DAYS HOUR MINU SECO'
 character(80):: obstr='SOZA SOLAZI SCATTA OPTD AEROTP'
 real(8) :: hdr(mxmn),obs(mxmn)

 INTEGER        :: ireadmg,ireadsb
 
 character(8)   :: subset
 integer        :: unit_in=10,idate,nmsg,ntb,unit_out=50,nsubset

 character(8)   :: c_sid
 real(8)        :: rstation_id,typ,vtcd
 equivalence(rstation_id,c_sid)

 integer        :: i,k,iret_hdr,iret_tpc,iret_ob,iret_oe,iret_qc,iret_aircft
 
! Flag to check if all number of levels are the same
  logical :: allLevelsEqual=.true.

! dump bufr table
 open(24,file='modisAOD.table.dump')
 open(unit_in,file='modisAODbufr',form='unformatted',status='old')
 call openbf(unit_in,'IN',unit_in)
 call dxdump(unit_in,24)

 call datelen(10)
   nmsg=0
   nsubset=0
   msg_report: do while (ireadmg(unit_in,subset,idate) == 0)
     nmsg=nmsg+1
     ntb = 0
     write(*,*)
     write(*,'(3a,i10)') 'msgtype=',subset,' cycle time =',idate

! 'SAID  AODS  CLONH  CLATH  YEAR  MNTH  DAYS   HOUR  MINU  SECO  SOZA  SOLAZI SCATTA OPTD  AEROTP'

     sb_report: do while (ireadsb(unit_in) == 0)
       ntb = ntb+1
       nsubset=nsubset+1
       call ufbint(unit_in,hdr,mxmn,1,iret_hdr,hdstr)
       call ufbint(unit_in,obs,mxmn,1,iret_ob,obstr)
       rstation_id=hdr(1)                      !SID 
       !write(*,'(2a10,15f10.3)')  "subset=",c_sid,hdr(3),hdr(2),hdr(4),hdr(5),hdr(6),hdr(7),hdr(8),hdr(9),hdr(10),hdr(11),hdr(12)
       !write(*,*) 'iret_hdr ',iret_hdr
       !DO k=1,iret_hdr
       write(*,'(a10,10f10.4)') 'hdr=',(hdr(i),i=1,10)
       write(*,'(a10,5f25.12)') 'obs=',(obs(i),i=1,5)
       !ENDDO
     enddo sb_report

   write(*,*)  'message ',nmsg, '  total subset ',ntb
   enddo msg_report

   write(*,*)  'total message ',nmsg, 'total subset ',nsubset
 call closbf(unit_in)

end program
