# Mixed additive modelling of global alien species co-invasions of plants and insects

This repository contains code and data for the analysis presented in our study on global alien species invasions using a Mixed Additive Relational Event Model. 

The directory structure is as follows:

### **Script01 - Envinronment Model Fitting**  

This script (`Script01-Creating-Case-Control-DataSet.R`) is responsible for **creating an environment containing essential objects for model fitting**.  
It focuses specifically on preparing **random-effect matrices** required for the analysis. 

### **Script02 - ModelFitting**  

This script (`Script02-ModelFitting.R`) is responsible for **fitting the relational event model** by selecting the best model based on the **Akaike Information Criterion (AIC)**.  
It incorporates various covariates and random effects to ensure an optimal model formulation.  

The script performs the following key tasks:  
- **Select Endogenous Covariates** – Chooses a set of covariates from multiple model formulations.  
- **Include Monadic Random Effects** – Models species-specific variability:  
  - Insect and plant invasiveness  
  - Country invasibility  
- **Account for Dyadic Random Effects** – Captures species co-invasion relationships.  

### **Script03 - Model Interpretation**  

This script (`Script03-ModelInterpretation`) is responsible for **analyzing and interpreting the fitted model**.  
It provides insights into parameter estimates using both parametric (for fixed, time-varying and random effects) and non-parametric approaches (for the baseline hazard function).   

### **Script04A - Goodness of Fit Functions**  

This script (`Script04A-GoodnessOfFit`) contains **functions for evaluating model goodness of fit (GOF)**.  

## **Script04B - Goodness of Fit Evaluation**  

This script (`Script04B-GoodnessOfFit`) is responsible for **evaluating the GOF of the fitted model**.  
