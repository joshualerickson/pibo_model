# water_yield
Water yield and channel morphology scripts.

* First you'll need to load the functions via `source('R/functions.R')`.
* The data is in the `data` folder and has two different data sets: `gr_95.csv` (ANCOVA) and `gr_95_ts.csv` (mixed effects).
* Then you can start with `final_wf.Rmd` (ANCOVA) or `lm_mod.Rmd` (mixed effects) scripts.
* Goal is to somehow have this in a [{targets}](https://github.com/ropensci/targets) framework.
* You can see the {targets} style in the `_targets.R` script.
