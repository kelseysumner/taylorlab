#!/bin/tcsh
#
##SBATCH -p common                # Partition to submit to (comma separated)
#SBATCH -J splitRef         # Job name
#SBATCH -n 1                     # Number of cores
#SBATCH -N 1                     # Ensure that all cores are on one machine
#SBATCH -t 2-00:00                # Runtime in D-HH:MM (or use minutes)
#SBATCH --mem 1000               # Memory in MB
#SBATCH -o _alignSplitRefs_%j.out # File for STDOUT (with jobid = %j) 
#SBATCH -e alignSplitRefs_%j.err       # File for STDERR (with jobid = %j)   


module load cutadapt/1.8.3-gcb01 
module load fastqc/0.11.5-fasrc01
module load java/1.7.0_60-fasrc01
module load jdk/9-gcb01

./splitReadsMultiRef.pl 2 /gpfs/fs1/home/jws48/splitReads/refs/AMA/AMA.fasta,/gpfs/fs1/home/jws48/splitReads/refs/CSP/CSP.fasta /gpfs/fs1/home/jws48/splitReads/out /gpfs/fs1/home/jws48/splitReads/reads/1 /gpfs/fs1/home/jws48/splitReads/reads/2 /gpfs/fs1/home/jws48/splitReads/adapters/forwardPrimers.fasta /gpfs/fs1/home/jws48/splitReads/adapters/reversePrimers.fasta

