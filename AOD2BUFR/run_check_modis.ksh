#!/bin/ksh

set -euax

day=20190806

for hh in 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 20 21 22 23
do
  
    file="AOD.Prepbufr.${hh}.QC00"
    if [[ -f ../Data/PrepBUFR/${day}/$file  ]]; then
      ln -sf ../Data/PrepBUFR/${day}/${file} AODbufr
      aod_decode.x > log.${file}.${day} 2>&1
      grep hdr= log.${file}.${day} | awk '{print $4,$5}' > log.${file}.${day}.latlon
    fi
done

