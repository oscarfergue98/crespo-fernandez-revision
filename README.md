# crespo-fernandez-revision

This repository contains all the code and data files used to produce the results for the first revision of the paper "Explaining Long-Term Bond Yields Synchronization Dynamics in Europe" at the journal **Economic Modelling**. The repository contains the following files and folders: 

1. **data**: this folder contains previously run results of the paper (in R.Data format) and new data files to run additional models to answer some referee comments. The files are: 

    1. synch_levels.xlsx: this dataset contains data on the yield synchronization and all the independent covariates (in levels)
    2. synch_synch.xlsx: this dataset contains data on the yield synchronization and all the independent covariates (sychronization values)
    3. epu_data.xlsx: this dataset contains data on the Economic Policy Uncertainty index (Baker et al.,               [2016](https://doi.org/10.1093/qje/qjw024)).
    4. cds_data.xlsx: this dataset contains data on sovereign Credit Default Swaps (CDS) for all countries in the sample. The data is obtained from Bloomberg
    5. forecast_theil_all.RData: this RData file contains the forecasting results from the original manuscript (i.e., previously run) for all models except for the PIP>50% models. 
    6. forecast_theil_synch_levels.RData: this RData file contains the forecasting results from the original manuscript (i.e., previously run) for the synch-levels PIP>50% models. 
    7. forecast_theil_synch_synch.RData: this RData file contains the forecasting results from the original manuscript (i.e., previously run) for the synch-synch PIP>50% models. 
    8. synch_levels_results.RData: this RData file contains the results for the synch-levels BMA model (Table 1 in the original manuscript)
    
2. **funcs**: this folder contains multiple functions that are imported in the .R code files for the sake of computational speed.

3. **plots**: this folder contains contains all the plots in .eps format produced for the revision.

4. **main_code.R**: this .R file contains all the code generated to reproduce the vast majority of results to answer to the referee comments 

5. **revision_cds.R**: this .R file contains all the code generated to reproduce results to answer to Reviewer's 1 Comment 4 (adding CDS as an additional variable in our BMA models)

6. **revision_epu.R**: this .R file contains all the code generated to reproduce results to answer to Reviewer's 2 Comment 2 (using the EPU index instead of the WUI index as the uncertainty measure)

7. **forecast_revision**: this .R file contains all the code generated to reproduce results to answer to Reviewer's 2 Comment 4 (would the models have predicted the 2010 ESD crisis?)
