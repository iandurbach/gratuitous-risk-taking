# Perceptions about the riskiness of extreme and non-extreme sports

Code in this repo reproduces results from two surveys examining how people receive the risk of different kinds of sporting activities carried out by a third person. Respondents were presented with various hypothetical scenarios that vary both the nature of the risk (e.g. type of sport involved, fatality rate, whether a guide is used) and the characteristics of the person participating in the activity (e.g. participant experience level, whether they have dependents, etc). 

Analyses of these surveys were written up in two papers,*Gratuitous Risk and Recklessness: An experimental and philosophical study* (Ebert, Durbach and Field, in review), and *Differences in expert and layperson’s danger and recklessness judgments about adventure sports participation* (Ebert and Durbach, in review) and. These papers are currently under review and a citation will be added once available. Raw survey responses are in the *data* folder, and examples of surveys used are in the *vignettes* folder. Intermediate output and results are saved to the *output* 

To reproduce the results in *Gratuitous Risk and Recklessness: An experimental and philosophical study*:

1. Run `dataprep-survey1.R`. This converts the raw survey responses, which are scattered over many Excel files, into a single clean dataset, and saves the file in `output/sportrisks_cleaned.Rds` (also included with this repo if you want to skip this step)
2. Run `analysis-survey1.R`. Results created by this file are reported in both papers.
3. Run `analysis-rtp-survey1.R`. Additional analysis of an adapted "willingness-to-pay"-type question only reported in this paper.

To reproduce the results in *Differences in expert and layperson’s danger and recklessness judgments about adventure sports participation*:

1. Run `dataprep-survey1.R`. (remembering you can skip this step as results are included with this repo)
2. Run `analysis-survey1.R`. Results created by this file are reported in both papers.
3. Run `dataprep-survey2.R`. This preprocesses a set of responses from experienced skiiers, and adds responses about skiing made by laypersons in Survey 1 for comparison. This survey is only reported on in this paper.
4. Run `analysis-survey2.R`. 
