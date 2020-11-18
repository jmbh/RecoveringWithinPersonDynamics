#!/bin/bash

mkdir "$TMPDIR"/DESim/

cd "$HOME"/DESim

sbatch -a 361 submit_jobs.sh
