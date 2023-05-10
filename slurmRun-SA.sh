#!/bin/bash -l

#each node has 48 cores*.

#We should also limit the wall time to what we might actually use, as I believe we get
#billed as if we used the entire amount.  2 hours and 30 minutes to be on the safe side.
# pop=100k >>> 2 CPUs; 5 hours
# pop=500k >>> 4 CPUs; 8 hours
# pop=1m >>> 5 CPUs; 10 hours


#SBATCH --partition=defq
#SBATCH --job-name=hivncd-SA
#SBATCH --time=10:00:0
#SBATCH --cpus-per-task=4
#SBATCH --nodes=1
#SBATCH --mem-per-cpu=4GB
#SBATCH --array=1-5200
#SBATCH --output=outputs-sa/outSlurm_%a.out
#SBATCH --error=outputs-sa/outSlurm_%a.err
#SBATCH --mail-type=end
#SBATCH --mail-user=mschnur3@jhu.edu


module load r
# Rscript driver.R $SLURM_ARRAY_TASK_ID
Rscript sa.R $SLURM_ARRAY_TASK_ID