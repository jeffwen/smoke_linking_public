# Quantifying fire-specific smoke exposure and health impacts
Repository supporting Wen et al. 2023: ["Quantifying fire-specific smoke exposure and health impacts"]().

## Overview
* `renv::restore()` can be used to recreate the project environment. See [renv collaboration](https://rstudio.github.io/renv/articles/renv.html#collaboration) for more information on how to set up the environment.
* `src/` contains the `main.R` file, which has code to replicate figures.
    * Ensure that the working directory is set correctly (line 21) in `main.R`
    * Section headers are used to distinguish different figure plotting code.
* Data can be downloaded from [Dropbox](https://www.dropbox.com/sh/z7tw7q2tnlg6dlk/AADIuRS85ryQlz9xlvEH1kJ5a?dl=0). 
    * The `data/` folder should be in the same root folder as `src/` and `output/`.
* Generated figures will be located in the `output/` folder.
    * The `output/` folder and `fig1/` - `fig5/` and `figsupp/` subfolders will need to be created for the `main.R` script to save figures in the default locations.
