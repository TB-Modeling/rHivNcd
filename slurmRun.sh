#!/bin/bash -l

#each node has 48 cores and can run up to 48 parallel replications
#we run the model in batches of 48 (e.g., 48 reps: 1 node, 480 reps: 10 nodes)....

#SBATCH --partition=parallel
#SBATCH --job-name=hivncd
#SBATCH --time=3:00:00 
#SBATCH --nodes=1
#SBATCH --cpus-per-task=2
#SBATCH --ntasks-per-node=24
#SBATCH --output=outputs/slurm_%a.out
#SBATCH --error=outputs/slurm_%a.err
#SBATCH --mail-type=end
#SBATCH --array=42-124

module load r
module load parallel

cd "/home/mschnur3/scratch4/melissa/rHivNcd"
# cd "/home/pkasaie/scr4_ekendal2/pkasaie/hivncd/rHivNcd"

# rm -f outputs/*
#rm -f node*

# Define the number of tasks per node
ntasks_per_node=24
# Calculate the start and end indices for the current array job
first_id=$(( SLURM_ARRAY_TASK_ID * ntasks_per_node + 1 ))
last_id=$(( first_id + ntasks_per_node - 1 ))

echo "Running models from $first_id to $last_id"


# Running jobs in a sequence
seq $first_id $last_id | parallel -j $ntasks_per_node --joblog node-${SLURM_ARRAY_TASK_ID}.log --wd . Rscript driver.R {}

# To merge all the outSlurm_*.out files into a single file after the run is complete:
cat outputs/slurm_*.out > outputs/combined_slurm.out
cat outputs/slurm_*.err > outputs/combined_slurm.err



# seq $first_id $last_id: This generates a sequence of numbers from first_id to last_id.

# | parallel -j 48 --joblog node-${SLURM_ARRAY_TASK_ID}.log --wd .:
# This pipes the sequence of numbers into the parallel command.
## -j 48 tells parallel to run up to 48 jobs concurrently.
## --joblog node-${SLURM_ARRAY_TASK_ID}.log specifies the log file for job details.
## --wd . sets the working directory for the jobs

#Rscript driver.R {}: #{} is a placeholder that parallel replaces with each item from the sequence generated by seq.
