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
### Dashboard of DadApp Shiny Web

![User interface](https://github.com/KIRAGU-MWAURA/DadApp_Shiny_Web_App/assets/44839744/528c602b-9a27-4ed5-be28-56644b018f6a)

### Characteristics of Datasets to be uploaded in DadApp
1. Dataset should have 0,1,2 as their genotyped loci with 0 representing homozygous reference, 1 heterozygous and 2 homozygous alternate
2. Dataset can have loci that are not genotyped or that are neither 0,1,2 as NAs
3. Dataset should have individuals as rows and columns as loci
4. All dataset should be in .csv, .txt file
5. Number of loci should be the same in whole population dataset, fathers’ dataset, offspring’s data and mother’s dataset if present
6. Upload each dataset one file at a time

#### NOTE:
- Dataset of the whole population is used in calculating the allele frequency of loci used in calculating the LOD scores of every individual
- While uploading offspring and mothers dataset, make sure the dataset has ONLY one row which represents one individual in every run
- For fathers' dataset, it can have as many rows as per the number of putative fathers present in a population

### Workflow of DadApp Shiny Web App.
![Workflow DadApp](https://github.com/KIRAGU-MWAURA/DadApp_Shiny_Web_App/assets/44839744/52f15c8c-92f3-4b29-937d-cc494ba535ca)

### Results Interpretation
There are four result output in every run namely: LOD Table, Histogram plot, Detailed Table and Detailed Chart.

LOD Table is a tabular representation contains dads' name, Father's postion(Row in uploaded dataset) and the LOD scores. The Table is arranged in accordance to LOD scores with the father with highest LOD score appearing top and father with lowest LOD score appearing bottom.

Histogram Plot is a graphical representation of fathers in the dataset against their LOD scores

Detailed Table is a tabular representation that contains Real names of father (if present), Father's position, LOD scores, Total loci - number of loci used in the algorithm, Compatible genotype- number of loci that are compatible in a given triad or dyad case, Incompatible genotype- number of loci that are incompatible in a given triad or dyad case, Number of NAs- number of loci that are not genotyped or its neither 0,1,2

Detailed Chart is a graphical representation of bar grah of Number of Counts (Number of compartible genotype, incompartible genotype and Number of Nas) against Real names of putative fathers.

NB: The putative father with the highest LOD scores is the most likely father of a given offspring.

### Authors
David Kiragu Mwaura affliated with Jomo Kenyatta University of Agriculture and Technology

Prof. Jenny Tung affliated with Max Planck Institute of Evolunationary Anthropology, Duke University, Amboseli Baboon Research Project

Dr. Jordan Anderson affliated with Oregon University, Duke University


