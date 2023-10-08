#!/bin/sh
#SBATCH --cpus-per-task=2
#SBATCH --mem=2gb
#SBATCH --time=01:05:00

#SBATCH --job-name=job_test
#SBATCH --mail-type=ALL
#SBATCH --mail-user=kyra.qj+hypergator@gmail.com
#SBATCH --output=serial_%j.out


pwd; hostname; 
module load R/4.2


# Get the current working directory
current_directory=$(pwd)

# Combine it with the desired subdirectory
library_path="$current_directory/kyra/R/library"

# Export as R_LIBS_USER environment variable
export R_LIBS_USER="$library_path"

# Print the resulting R_LIBS_USER value
echo "R_LIBS_USER is now set to: $R_LIBS_USER"

rm -r JQ_CATEsimulation
git clone https://github.com/kyraquan/JQ_CATEsimulation.git
cd JQ_CATEsimulation
Rscript ALW10.5/preparation_files.R
