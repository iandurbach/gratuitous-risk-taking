# gratuitous-risk-taking
This repo contains code and data accompanying the paper "Gratuitous risk taking" by Philip Ebert and Ian Durbach. The paper is currently under review and a citation will be added once available.

- Raw survey responses are in the *data* folder.
- Examples of surveys used are in the *vignettes* folder.
- Cleaned survey responses are in the *output* folder, which is also where any results are written.
- To turn the raw survey responses into a single clean dataset, run `data_prep.R`. This saves the file `sportrisks_cleaned.Rds` to the output folder (also included with this repo if you want to skip this step).
- To reproduce the analyses from the paper, run `analyses.R`.