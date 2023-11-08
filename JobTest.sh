#!/bin/bash
#PBS -j oe
#PBS -m ae
#PBS -N JobName
#PBS -M slade.allenankins@jcu.edu.au
#PBS -l select=1:ncpus=1:mem=18gb
#PBS -l walltime=00:20:00

#set scratch directory where flac and wav files will temporarily reside
scratchdir=/scratch/jc696551/

# cd to dir where directory was created
cd $PBS_O_WORKDIR
shopt -s expand_aliases
source /etc/profile.d/modules.sh
echo "Job identifier is $PBS_JOBID"
echo "Working directory is $PBS_O_WORKDIR"


#download recording to scratch directory using curl and the recordingID extracted from the csvfile
module load curl
cd $scratchdir
curl -u "zMIyOKUE3jdK0kS:Bioacoustics" -O -J "https://cloud.une.edu.au/public.php/webdav/BC1/20191114_Continuous/20191114_183000_Continuous.wav"
cd $PBS_O_WORKDIR
