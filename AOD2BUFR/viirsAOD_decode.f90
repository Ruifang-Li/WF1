program prepbufr_decode_all
!
! read all observations out from prepbufr. 
! read bufr table from prepbufr file
!
 implicit none

 integer, parameter :: mxmn=35, mxlv=255, channel=12,mxib=20
 character(80):: hdstr='SAID CLATH CLONH YEAR MNTH DAYS HOUR MINU SOZA SOLAZI RSST AOTQ RETRQ'
 character(10):: chanstr='CHWL'   
 character(10):: aodstr='AOTH'

 real(8) :: hdr(mxmn),obs(1,channel),chan(1,channel)

 INTEGER        :: ireadmg,ireadsb
 
 character(8)   :: subset
 integer        :: unit_in=10,idate,nmsg,ntb,unit_out=50,nsubset,nib,ibit(mxib)

 character(8)   :: c_sid
 real(8)        :: rstation_id,typ,vtcd
 equivalence(rstation_id,c_sid)

 integer        :: i,k,iret_hdr,iret_chan,iret_ob
 
! Flag to check if all number of levels are the same
  logical :: allLevelsEqual=.true.

! dump bufr table
 open(24,file='viirsAOD.table.dump')
 open(unit_in,file='viirsAODbufr',form='unformatted',status='old')
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


     sb_report: do while (ireadsb(unit_in) == 0)
       ntb = ntb+1
       nsubset=nsubset+1
       call ufbint(unit_in,hdr,mxmn,1,iret_hdr,hdstr)
       call ufbrep(unit_in,chan,1,12,iret_chan,chanstr)
       call ufbrep(unit_in,obs,1,12,iret_ob,aodstr)
       rstation_id=hdr(1)                      !SID 

       ! character(80):: hdstr='SAID CLATH CLONH YEAR MNTH DAYS HOUR MINU SOZA SOLAZI RSST AOTQ RETRQ'
       ! character(10):: chanstr='CHWL'
       ! character(10):: aodstr='AOTH'

   
       !write(*,*) "return level not equal ",iret_hdr, iret_chan, iret_ob
       !write(*,'(2a10,15f10.3)')  "subset=",c_sid,hdr(3),hdr(2),hdr(4),hdr(5),hdr(6),hdr(7),hdr(8),hdr(9),hdr(10),hdr(11),hdr(12)
       !write(*,*) 'iret_hdr ',iret_hdr
       !DO k=1,iret_hdr

       write(*,'(a10,8f10.4,3f25.10,2f5.2)') 'hdr=',(hdr(i),i=1,13)

       ! very confused function, ibit tells bit numbers which set to 1 in hdr(12)
       ! nib: number of bit number returned in ibit, for example
       ! aotq  34.00  2 13 17  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0 0 0 0 
       ! nib=2, hdr(12)=34 has 18 bits, 13 and 17 =1 
       !call upftbv(unit_in,"AOTQ",hdr(12),mxib,ibit,nib)
       !write(*,'(a6,f10.2,21i3)') "aotq ",hdr(12),nib,ibit
       
       write(*,'(a10,12f15.9)') 'chan=',(chan(1,i),i=1,12)
       write(*,'(a10,12f15.4)') 'obs=',(obs(1,i),i=1,12)

       !ENDDO
     enddo sb_report

   write(*,*)  'message ',nmsg, '  total subset ',ntb
   enddo msg_report

   write(*,*)  'total message ',nmsg, 'total subset ',nsubset
 call closbf(unit_in)

end program
