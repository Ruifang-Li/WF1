#!/bin/ksh

set -euax

cwf viirsAOD_decode

day=20190806

for hh in 19 #01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 #16 17 18 19 20 21 22 23
do
  
    file="VIIRS.AOD.Prepbufr.${hh}H.QC00"
    if [[ -f ../Data/PrepBUFR/${day}/VIIRS/$file  ]]; then
      ln -sf ../Data/PrepBUFR/${day}/VIIRS/${file} viirsAODbufr
      viirsAOD_decode.x > log.${file}.${day} 2>&1
      grep hdr= log.${file}.${day} | awk '{print $4,$3}' > log.${file}.${day}.latlon
    fi
done

