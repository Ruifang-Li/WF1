#!/bin/ksh
set -euax

cwf pmbufr_decode

cyc_day=20190805
cyc_interv=1
here=$(pwd)
pm_exec=${here}/pmbufr_decode.x

cd Data/anowpm_hourly/$cyc_day

for i in 00 01 02 03 04 05 06 07 08 09 10 11 12 \
         13 14 15 16 17 18 19 20 21 22 23 
do

rm -f pm.bufr
rm -f log.pm*

#cat pm.bufr.01 pm.bufr.02 pm.bufr.03 pm.bufr.04 pm.bufr.05 pm.bufr.06 \
#    pm.bufr.07 pm.bufr.08 pm.bufr.09 pm.bufr.10 pm.bufr.11 pm.bufr.12 \
#    pm.bufr.13 pm.bufr.14 pm.bufr.15 pm.bufr.16 pm.bufr.17 pm.bufr.18 \
#    pm.bufr.19 pm.bufr.20 pm.bufr.21 pm.bufr.22 pm.bufr.23 pm.bufr.24 > pm.bufr.chk

ln -fs pm25.airnow.${cyc_day}$i.bufr pm.bufr.chk
${pm_exec} > log.pm.$i

#grep subset= log.pm.$i | awk '{print $8}' > log.tm00.$i
#diff ../../log.tm00.$i .   # sequence number should equal to that from one day data

done
