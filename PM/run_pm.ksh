#!/bin/ksh -l

#SBATCH --partition=bigmem
#SBATCH --mem=250g
#SBATCH --ntasks=1
#SBATCH --qos=batch  # partition bigmem: batch debug urgent 
#SBATCH --time=0:30:00
#SBATCH --account=zrtrr
#SBATCH --job-name=pm_prepbufr
#SBATCH --output log.pm

set -euax

cwf pmbufr

pmfile=/scratch1/BMC/zrtrr/rli/WF1/PM/Data
cyc_interv=1
here=$(pwd)
pm_exec=${here}/pmbufr.x
fname=pm25.airnow

yy=2019
mm=08

cd ${pmfile}/anowpm_hourly
for dd in 05 06 #07 08 09 10
do

cyc_day=${yy}${mm}${dd}
mkdir -p ${cyc_day}
cd ${cyc_day}
rm -f * 
ln -sf ${pmfile}/anowpm_daily/$cyc_day/aqm.t12z.anowpm.pb.tm00 pm.bufr
ln -sf ${here}/pm.table . 
${pm_exec} ${cyc_interv}

cd ..

done
