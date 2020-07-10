#!/bin/bash

mkdir "$TMPDIR"/DESim/

cd "$HOME"/DESim

sbatch -a 1-900 submit_jobs.sh   