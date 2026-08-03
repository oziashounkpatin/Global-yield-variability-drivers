This repository contains the code, data preparation scripts, and analysis workflows for the paper:

> **Global determinants of yield variability under soil conservation practices across climate, soil, and topography: A meta-analysis**  
> *Kpade O. L. Hounkpatin, Johannes Piipponen, Emanuela De Giorgi, Mika Jalava, Jeroen Poelert, Matti Kummu*

**1. Compiling and preprocessing the database** 

The pipeline Step1_database_pre_processing merges three meta-analytic datasets (Su et al.,2021; Jian et al., 2020 and farmgeek), harmonises crop and management classifications, computes yield effect sizes, removes duplicates and inconsistent records, and links each observation to soil, climate, and topographic covariates. It then imputes and normalises soil texture fractions and outputs a cleaned, spatially explicit database for subsequent analyses.

 **2. Study area**
 
 The script Step2_Figure_3_study_area generates the global point maps of field observations for Agroforestry, Cover crop, No-tillage and Organic Farming presented in Figure 3. The final figure was grouped and finalized in Adobe Illustrator.

**3. Overall distribution of the percentage change of the effect size across management, crops and environment variables**  
 
The step3 script Step3_Figure_4_Overall efffect size change generates Figure 4 of the manuscript — Distribution of the percentage change of the effect size between sustainable farming approaches, crop groups, soil properties, topography, and climatic variables — by constructing study-level clusters, deriving environmental and management classes, and using cluster bootstrapping to estimate mean yield effects and confidence intervals for each practice–covariate combination.
   
 **4. Distribution of the percentage change of the effect size across crops and environment variables for each management**
 
The step 4 Step4_Figure_5_6_7_efffect size change per practice script stratifies the database by management (Agroforestry, Cover crop, No-tillage, Organic Farming), constructs consistent climate, soil and topographic classes, and applies study-clustered bootstrapping to estimate practice-specific yield effects. It then assembles panels for crops + climate (Figure 5), soil properties (Figure 6), and topography (Figure 7), each showing how yield responses vary by management and environmental context.

 **5. Publication bias assessment and effect size distribution**
 
The Step5_Figure_8_sensitivity_analysis script generates density plots showing the distribution of effect sizes for each sustainable farming approach (agroforestry, cover cropping, no-tillage, and organic farming). These plots provide a qualitative assessment of publication bias by visualizing the symmetry and spread of effect size distributions. The script overlays kernel density estimates for each farming approach and indicates the 95% confidence interval bounds with dashed vertical lines. The resulting figure corresponds to Figure 8 of the manuscript.

 **6. Leave-one-out sensitivity analysis**

The **Step6_Figure_9_leave_one_out_sensitivity_analysis** script performs a leave-one-out (jackknife) sensitivity analysis to evaluate the robustness of the estimated mean effect sizes for each sustainable farming approach. In each iteration, a single observation is excluded, and the average effect size is recalculated to assess the influence of individual observations on the overall results. The script generates scatter plots for agroforestry (AF), cover cropping (CC), no-tillage (NT), and organic farming (OF), with dashed horizontal lines indicating the 95% confidence interval bounds. The resulting diagnostic plots correspond to **Figure 9** of the manuscript and demonstrate the stability of the estimated effect sizes across observations.

**7. Additional information**

 All figures were further treated and finalized in Adobe Illustrator.

Software requirements

Used R version: 4.2.2

Used R base packages: stats, graphics, grDevices, utils, datasets, methods, base

Used other R packages and their versions:

Core data handling:
tidyverse, dplyr, readxl, writexl, data.table, purrr, mltools, mice


**Other packages:**

- **Core data handling:**
  - `tidyverse` (2.0.0), `dplyr` (1.1.2), `readxl` (1.4.2), `writexl` (1.4.2), `data.table` (1.15.4), `purrr` (1.0.1), `mltools` (0.3.5), `mice` (3.16.0)
- **Spatial data processing:**
  - `terra` (1.7.71), `sf` (1.0.12), `tidyterra` (0.5.1)
- **Visualization:**
  - `ggplot2` (3.5.1), `latticeExtra` (0.6.30), `gridExtra` (2.3), `tmap` (3)
- **Parallel computing:**
  - `doParallel` (1.0.17)

