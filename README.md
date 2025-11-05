# IV Congreso & XV JORNADAS de Usuarios de R 
Taller 3 - Modelos de mortalidad multi-poblacionales (CvmortalityMult). Descomposición de la esperanza de vida (LEdecomp) - David Atance .

# SLIDES 
Las diapositivas del taller se encuentran en los ficheros: 
- Taller3-CvmortalityMult.pdf;
- Taller3-LEdecomp.pdf

# NECESSARY PACKAGES 
# Installation:
To install the CvmortalityMult package from R CRAN:
```
install.packages("CvmortalityMult")
library(CvmortalityMult)
```
Cran repository can be found in:
https://CRAN.R-project.org/package=CvmortalityMult

To install the LEdecomp R package from github:
```
devtools::install_github("davidAtance/LEdecomp")
devtools::install_github("timriffe/LEdecomp")
library(LEdecomp)
```
If you are interested in the packages feel free to email david.atance@uah.es or track development at http://github.com/davidAtance/CvmortalityMult
http://github.com/davidAtance/LEdecomp

# SCRIPTS 
Los ficheros scripts utilizados para el Taller3-CvmortalityMult son: 

- 1.SinglePopulationMortalityModels.R
- 2.MultiPopulationMortalityModels.R
- 3.CVinMultiPopulationMortalityModels.R

Para el Taller3-LEdecomp son: 
-  1.LEdecomp_AllCauses.R
-  2.LEdecomp_Cause-of-Death.R

# DATA 
Se pueden encontrar también en el propio github; para el Taller3-CvmortalityMult son:
- Spain.RData
- CountryCombined.RData
- loocv.RData
- ResultsCV.RData

Mientrás que para el Taller3-LEdecomp son:
- Spain.RData

