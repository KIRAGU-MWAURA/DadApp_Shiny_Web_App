### DadApp Shiny Web Application
DAdApp performs paternity analysis utilizing pairwise likelihood-based method in assigning offspring(s) of interest to their parent pair characterized by the highest
log-likelihood ratio (LOD scores).
The Shiny App is a re-implementation of Cervus (Slate et al., 2000) written in R programming language.
### Running the App
If you want to test the dashboard, you’ll have to install it first. It’s assumed you have R and RStudio configured
Incase R package shiny is not installed please run the following command
```
install.packages("shiny", repos="http://cran.us.r-project.org")
library(shiny)
```
Open your R console or Rstudio and paste the command below which will automatically install all required dependencies (R packages).
```
library(shiny)
runGitHub("DadApp_Shiny_Web_App", "KIRAGU-MWAURA", launch.browser = TRUE)
```
### Workflow of DadApp Shiny Web App.
![Workflow DadApp](https://github.com/KIRAGU-MWAURA/DadApp_Shiny_Web_App/assets/44839744/f0f91d93-c1aa-44b1-b228-f7a94d2c1194)

### Characteristics of Datasets to be uploaded in DadApp
1. Dataset should have 0,1,2 as their genotyped loci with 0 representing homozygous reference, 1 heterozygous and 2 homozygous alternate
2. Dataset can have loci that are not genotyped as NAs
3. Dataset should have individuals as rows and columns as loci
4. All dataset should be in .csv, .txt file
5. Number of loci should be the same in whole population dataset, fathers’ dataset, offspring’s data and mother’s dataset if present
6. Upload each dataset one file at a time

### Authors
David Kiragu Mwaura affliated with Jomo Kenyatta University of Agriculture and Technology

Prof. Jenny Tung affliated with Max Planck Institute of Evolunationary Anthropology, Duke University, Amboseli Baboon Research Project

Dr. Jordan Anderson affliated with Oregon University, Duke University


