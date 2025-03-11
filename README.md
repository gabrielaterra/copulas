## Financial Dependency Analysis Using Copulas

This repository contains the code developed for the **Financial Econometrics II** project during the **Spring 2025** semester at the M2 Finance Technology Data **Université Paris 1 Panthéon-Sorbonne**. The objective of this project is to analyze the dependence structure between different asset classes using **copulas** and other statistical measures.

## Repository Structure

1. **data_collection.R**  
   - Collects and preprocesses the data required for the analysis.  
   - Includes data sources, cleaning procedures, and time series organization.  

2. **correlations.R**  
   - Computes correlation matrices for different periods.  
   - Helps identify linear relationships between asset returns.  

3. **copulas_calc.R**  
   - Estimates copula parameters to model the dependence structure.  
   - Implements various copula families. 

4. **copulas_by_period.R**  
   - Applies copula models to different time periods to examine temporal variations.  

5. **tail_dependency.R**  
   - Measures tail dependence between asset classes.  

## Getting Started
To run the analysis, ensure you have the required R packages installed:
\`\`\`r
install.packages(c('copula', 'MASS', 'VineCopula', 'tidyverse'))
\`\`\`
Then, execute the scripts in the following order:
1. \`data_collection.R\`
2. \`correlations.R\`
3. \`copulas_calc.R\`
4. \`copulas_by_period.R\`
5. \`tail_dependency.R\`




