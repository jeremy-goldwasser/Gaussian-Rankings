# Gaussian Rankings

This repository contains ```R``` scripts used to produce figures and results for Gaussian Rank Verification paper. 
- ```unequal_variances.R``` produces Figure 1.
- ```line_plot_12.R``` produces Figure 2a, and ```line_plot_13.R``` produces 2b.
- ```simulation_study.R``` validates on simulated data, producing Figure 3.
- ```simulation_even_i.R``` validates on simulated data, producing Figure 4 in the Appendix.
- ```NHANES_application.R``` generates experimental results, producing Tables 1 and 2.

```testing_functions.R``` contains functions for the valid ranking procedures described in the paper:
- Verify the winner as the best (Theorem 1)
- Identify the highest- or lowest- ranking stable features (Procedure 1)
- Evaluate the stability of the top-*K* set (Procedure 2)
