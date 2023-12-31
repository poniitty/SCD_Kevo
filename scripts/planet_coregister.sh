#!/bin/bash -l
#SBATCH --job-name=corgstr_planet
#SBATCH --account=Project_2007415
#SBATCH --output=output_%j.txt
#SBATCH --error=errors_%j.txt
#SBATCH --time=6:00:00
#SBATCH --ntasks=20
#SBATCH --nodes=2-10
#SBATCH --partition=large
#SBATCH --mem-per-cpu=16G

# Load r-env-singularity
module load r-env-singularity

# Clean up .Renviron file in home directory
if test -f ~/.Renviron; then
    sed -i '/TMPDIR/d' ~/.Renviron
    sed -i '/OMP_NUM_THREADS/d' ~/.Renviron
fi

# Specify a temp folder path
echo "TMPDIR=/scratch/project_2007415/temp" >> ~/.Renviron

# Run the R script
srun singularity_wrapper exec Rscript --no-save 03_planet_coregister.R