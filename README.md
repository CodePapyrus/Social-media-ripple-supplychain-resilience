# Social-media-ripple-supplychain-resilience
Code for "Social Media’s Ripple Effect on Supply Chain Competitive Resilience: Imitation Convergence vs Innovation Differentiation"  R code to reproduce all analyses, figures, and tables. Examines new supply chain competitiveness forms for products that gain sudden popularity via social media.

## Code Execution Order

### Step 1: Main Analysis
Run the following two scripts to establish the baseline multi-stage game model:

1. **`01_lamb_function.R`** - Calculates the Lambda function values
   - Output: Framework of baseline utility parameters
   
2. **`02_mentecaro_method.R`** - Implements the Mentecaro method
   - Input: Results from the Lambda function
   - Output: Baseline game equilibrium solutions

### Step 2: Sensitivity Analysis
Run the following script to systematically scan 9 key parameters:

3. **`03_multi_stage_game.R`** - Multi-stage game parameter sensitivity analysis
   - Sequentially scans 9 parameter combinations:
     ```
     1. U_t (Utility parameter)
     2. Beta (β parameter)
     3. Gama (γ parameter)
     4. I_NA_L (Information Asymmetry - Leader)
     5. I_NA_F (Information Asymmetry - Follower)
     6. n_t (Number of participants)
     7. v_t_f (Value function)
     8. Epsilon (ε parameter)
     9. Theta (θ parameter)
     ```
   - Output: 9 CSV files, named in the format `final_result_[parameter_name].csv`

### Step 3: Visualization
Run the following scripts in order to generate the paper's figures:

| Figure Number | Corresponding Parameter | Script to Run |
|---------------|-------------------------|---------------|
| Figure 7      | U_t                     | `04_figure_07.R` |
| Figure 8      | Beta                    | `05_figure_08.R` |
| Figure 9      | Gama                    | `06_figure_09.R` |
| Figure 10     | I_NA_L                  | `07_figure_10.R` |
| Figure 11     | I_NA_F                  | `08_figure_11.R` |
| Figure 12     | n_t                     | `09_figure_12.R` |
| Figure 13     | v_t_f                   | `10_figure_13.R` |
| Figure 14     | Epsilon                 | `11_figure_14.R` |
| Figure 15     | Theta                   | `12_figure_15.R` |
