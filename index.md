# Supplemental Files

## Effects of 2019's Social Protest on Emergency Health Services Utilization and Case Severity in Santiago, Chile


Below you may find the following stages regarding the database compilation and main analyses:

 - [Compilation of Databases into a Single One](BD_Compilation.html)
 - [Choose the most adequate model for Trauma Consultations](Causal_Impact2_cons_trauma.html)
 - [Choose the most adequate model for Respiratory Consultations](Causal_Impact2_cons_resp.html)
 - [Choose the most adequate model for Trauma Hospitalizations](Causal_Impact2_hosp_trauma.html)
 - [Choose the most adequate model for Respiratory Hospitalizations](Causal_Impact2_hosp_resp.html)
 - [Selected Models](Consolidacion_BDs_FINAL.html)
 - [Selected Models, but defining the time point in October 13 instead of October 21, 2019](Consolidacion_BDs_FINAL_after_revision.html)
 - [Figures](Plots-2021-01-18.html)
 
<br>

`BD_Compilation.Rmd` contains information on how the database was built; `Consolidacion_BDs_FINAL.Rmd` contains the main analytic decisions and analyses performed based on the selection of the most adequate models. In terms of databases, `AtencionesUrg_*` contains the raw databases from MINSAL (Ministry of Health), while `Procesos hasta 4_2` was the result of the compilation of databases into a single one; finally, `Definitive_models_2021.RData` contains the result of the analyses of the selected models. This data could not be uploaded to GitHub but can be accessed [here](https://drive.google.com/file/d/1533ikY5RWOrqrQo3tmInvHCYP2ifubw3/view?usp=sharing). If someone wants to reproduce the figures and plots presented, should access to `Plots-2021-01-18.Rmd`.