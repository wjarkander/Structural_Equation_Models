# Structural_Equation_Models

## A SEM Analysis of Factors Behind AI Use Among University Students

Computer project for the Structural Equation Modeling course, including a multi-group comparison between men and women.

Folder: `AI_Use_and_Student_Success_SEM/` (report, R code and data)

Authors: William Jarkander, Isac Sandberg, Julius Searancke (May 2026)

### Abstract

This study aims to investigate the effect that perceived benefits and risks have on Artificial Intelligence use (AI-use), and the resulting positive and negative outcomes due to its usage. We are additionally interested in investigating differences in AI-uptake patterns between men and women using multi-group analysis. The study was completed with a perspective on university students, using cross-sectional survey data collected by Correa (2026), focusing on study habits and AI-adoption among students at the Manizales campus of a Colombian public university.

After a literature review, and exploration of the data set at hand, an initial hypothesis was able to be posed. Unfortunately, due to data limitations, a perceived risks factor was unable to be formed in this hypothesis, but perceived benefits were included using "Study Habits" as a proxy, as well as both positive ("Academic Success") and negative ("AI-use Risks") outcomes.

The model was tested with Confirmatory Factor Analysis (CFA) and Structural Equation Modeling (SEM). The final SEM model (using study habits, AI-use, academic success, and AI-use risks as latent variables), showed significant p-values for all indicators, with generally moderate strength standardized loadings.

The results revealed that the most important factor to academic success was study habits. In contrast to one of our initial hypotheses, study habits also showed a significant positive impact on AI-use. We found AI-use significantly increased AI-use risk, which then had a negative impact on academic performance. Admittedly, results from the final model have to be taken with a grain of salt, as the fit measures showed to be barely acceptable.

Finally, for the multi-group analysis, a simpler model was used, with latent variables; study habits, AI-use, and academic success. This comparison failed at configurate invariance, showing good fit measures for men, while showing bad fit measures for women. This indicates that there could be some difference in relationships between study habits, academic successes, and AI-use between genders.

## Data source

The data in `AI_Use_and_Student_Success_SEM/data.csv` was not collected by us. It comes from:

Correa, Cristian (2026), "Study habits and artificial intelligence use among university students: A proportionally stratified survey dataset", Mendeley Data, V1, doi: 10.17632/mcwb2ppdsw.1

Link: https://doi.org/10.17632/mcwb2ppdsw.1

Please cite the original dataset if you reuse it, and check its Mendeley Data page for the license terms.
