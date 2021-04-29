#!/bin/bash
#SBATCH --job-name=kms_dada2_ama
#SBATCH --ntasks=12
#SBATCH --nodes=4
#SBATCH --time=3:00:00
#SBATCH --mem=65536

module load R_core/3.5.1-gcb01

Rscript step2a_dada2_ama.R

