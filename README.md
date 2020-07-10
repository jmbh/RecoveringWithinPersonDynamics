---
output:
  pdf_document: default
  word_document: default
  html_document: default
---
## Reproducibility Archive

This reproducibility archive allows to reproduce all results and figures of the paper "Recovering Within-Person Dynamics from Psychological Time Series" 


### Section 2: A bistable dynamical system for emotion dynamics

1. `plot_FixedpointsVectorfields.R` Reproduces Figure 1 with fixed points and vector fields
2. `fun_Datagen.R` Functions to generate data from the true system; called by `Generate_Data.R`
3. `Generate_Data.R` Generates the ``ideal" data used throughout the paper from the true system
4. `plot_TimeSeriesData.R` Reproduces the time series plot in Figure 2


### Section 3: The problem of misspecification

1. `fun_StatsModels.R` contains functions to summarize and visualize the results of the time series models in 3.2; called by `analysis_TimeSeriesModels_ideal.R`
2. `analysis_TimeSeriesModels_ideal.R` reproduces all analyses in Section 3.1 - 3.5, except fitting the TVAR model
3. `/TVAR` This folder contains the files necessary to fit the TVAR model with the R-package `tsDyn`
    a) `analysis_TVAR_fit.R` produces the estimated TVAR model in Section 3.5. Note this analysis can take upwards of 9 hours to run.
    b) `tvar_est.RDS` contains the final model produced by `analysis_TVAR_fit.R`.


### Section 4: Recovering Bistable Systems from ESM Data

Generating the 1800 weeks of data with Euler's method in the same way as the original two week data above would have created a ~900GB file. To avoid this, we divide the 1800 weeks into 900 2-week chunks, which we ran on a cluster computer. The files run on the cluster and the output files are in the folder /generateESMdata:

1. `/generateESMdata`
    a) `simulation.R` A version of `Generate_Data.R` which takes a seed (iter) as input and generates a 2-week time series
    b) `submit_all.sh` and `submit_jobs.sh` are bash-scripts which we used to run `simulation.R` 900 times with different seeds on a cluster computer
    c) `/output` Contains 900 files, which are combined into one dataset by `combine_files.R` 
    d) `combine_files` creates `data_ESM.RDS` from the seperate files in `\output`, and stores it in the `/files` folder. This data file will be loaded by `analysis_TimeSeriesModels_ESM.RDS`
2. `plot_TimeSeriesData.R` Reproduces the time series plot in Figure 7
3. `analysis_TimeSeriesModels_ESM.R` reproduces all analyses in Section 4, except fitting the TVAR model
4. `/TVAR` This folder contains the files necessary to fit the TVAR model with the R-package `tsDyn`
    a) `analysis_TVAR_fit_ESM.R` produces the estimated TVAR model in Section 4.3. Note this analysis can take upwards of 9 hours to run.
    b) `tvar_est_ESM.RDS` contains the final model produced by `analysis_TVAR_fit_ESM.R`.



