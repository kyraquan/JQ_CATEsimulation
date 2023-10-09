#SBATCH --job-name=all_conditions_job
#SBATCH --output=all_conditions_job_%A_%a.out
#SBATCH --error=all_conditions_job_%A_%a.err
#SBATCH --array=2-433%40  # Define the range of array indices
#SBATCH --cpus-per-task=12
#SBATCH --mem=16gb
#SBATCH --time=48:05:00

#SBATCH --mail-type=ALL
#SBATCH --mail-user=kyra.qj+hypergator@gmail.com



pwd; hostname;
module load R/4.3


# Get the current working directory
current_directory=$(pwd)

# Combine it with the desired subdirectory
library_path="$current_directory/${SLURM_ARRAY_TASK_ID}/kyra/R/library"

mkdir -p "$library_path"
# Export as R_LIBS_USER environment variable
export R_LIBS_USER="$library_path"

# Print the resulting R_LIBS_USER value
echo "R_LIBS_USER is now set to: $R_LIBS_USER"

git_path="${SLURM_ARRAY_TASK_ID}/JQ_CATEsimulation"
if [ -d "$git_path" ]; then
    rm -r "$git_path"
fi
echo "Git path is $git_path"
mkdir -p "$git_path"

git clone https://github.com/kyraquan/JQ_CATEsimulation.git "$git_path"
cd "$git_path/ALW10.5"

export access_sas="{Azure_SAS_TOKEN}"

# Define the specific column headers
headers=("level2n" "level1n" "ICC" "PS_model" "treatment_model" "Outcome_model" "tau_var")

# Read the corresponding row from the CSV file based on the array index
IFS=',' read -ra values <<< "$(sed -n ${SLURM_ARRAY_TASK_ID}p ALLCONDITIONS.csv)"

# Expose the values as environment variables with column headers as names
for i in "${!headers[@]}"; do
  export "${headers[$i]}"="${values[$i]}"
done
echo "Task ${SLURM_ARRAY_TASK_ID}:"
for header in "${headers[@]}"; do
  echo "$header=${!header}"
done

Rscript preparation_files.R

# Clean up aftewards
cd "$current_directory"
rm -rf "${SLURM_ARRAY_TASK_ID}"