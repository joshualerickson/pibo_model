# PIBO Exploration and Statistics
PACFISH/INFISH Biological Opinion (PIBO) "_Comparison of stream morphological metrics in reference and managed
catchments across Western Montana and Northern Idaho_" scripts using linear mixed effect models (`lm_mod.Rmd`) and pre-processing steps (`final_wf.Rmd`). See below for more information.  

---

* First you'll need to load the functions via `source('R/functions.R')`.
* The data is in the `data` folder and has two different data sets: `gr_95.csv` (ANCOVA) and `gr_95_ts.csv` (mixed effects).
* Then you can start with `final_wf.Rmd` (ANCOVA) or `lm_mod.Rmd` (mixed effects) scripts.
* Goal is to somehow have this in a [{targets}](https://github.com/ropensci/targets) framework.
* You can see the {targets} style in the `_targets.R` script.
