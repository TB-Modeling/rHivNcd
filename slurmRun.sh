#!/bin/bash -l

#each node has 48 cores and can run up to 48 parallel replications
#we run the model in batches of 48 (e.g., 48 reps: 1 node, 480 reps: 10 nodes)....

# Memory/walltime? 
# pop=100k >>> 2 CPUs; 5 hours
# pop=500k >>> 4 CPUs; 8 hours
# pop=1m >>> 5 CPUs; 15 hours



#SBATCH --partition=parallel
#SBATCH --job-name=hivncd
#SBATCH --time=00:01:00 
#SBATCH --nodes=1
#SBATCH --cpus-per-task=1
#SBATCH --ntasks-per-node=48
#SBATCH --output=outputs/outSlurm_%a.out
#SBATCH --error=outputs/outSlurm_%a.err
#SBATCH --mail-type=end
#SBATCH --array=0-100%75

cd "/home/pkasaie/scratch4-ddowdy1/melissa/rHivNcd"

# Calculate the start and end indices for the current array job
first_id=$(( SLURM_ARRAY_TASK_ID * 48 + 1 ))
last_id=$(( first_id + 48 - 1 ))


module load r
module load parallel

# Running jobs in a sequence
seq $first_id $last_id | parallel -j 48 --joblog node-${SLURM_ARRAY_TASK_ID}.log --wd . Rscript driver.R {}

# seq $first_id $last_id: This generates a sequence of numbers from first_id to last_id.

# | parallel -j 48 --joblog node-${SLURM_ARRAY_TASK_ID}.log --wd .:
# This pipes the sequence of numbers into the parallel command.
## -j 48 tells parallel to run up to 48 jobs concurrently.
## --joblog node-${SLURM_ARRAY_TASK_ID}.log specifies the log file for job details.
## --wd . sets the working directory for the jobs

#Rscript driver.R {}: #{} is a placeholder that parallel replaces with each item from the sequence generated by seq.
