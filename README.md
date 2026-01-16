# Social-media-ripple-supplychain-resilience
Code for "Social Media’s Ripple Effect on Supply Chain Competitive Resilience: Imitation Convergence vs Innovation Differentiation"  R code to reproduce all analyses, figures, and tables. Examines new supply chain competitiveness forms for products that gain sudden popularity via social media.

## Code Execution Order

### Step 1: Main Analysis
Run the following two scripts to establish the baseline multi-stage game model:

1. **`Code_for_lambert_function.R`** - Calculates the Lambda function values
   - Output: Framework of baseline utility parameters
   
2. **`Code_for_mentecaro_method.R`** - Implements the Mentecaro method
   - Input: Demand Function Parameters: The fundamental parameters defining the demand model and the time decay function (e.g., baseline coefficients, elasticity parameters, decay rates); Simulation Configuration Parameters: Number of Simulations,Random Number Seed (for reproducibility),Confidence Level,Convergence Criteria.
   - Output: Demand Distribution Statistics: Key summary statistics derived from the simulated demand distribution, including: Expected Demand (Mean), Standard Deviation,Coefficient of Variation (CV),Quantiles (5th percentile, 95th percentile)

### Step 2: Sensitivity Analysis
Run the following script to systematically scan 9 key parameters:

3. **`03_multi_stage_game.R`** - Multi-stage game parameter sensitivity analysis
   - Sequentially scans 9 parameter combinations:
     ```
     1. U_t_NA (Potential market demand)
     2. Beta_NA_NA (Price_sensitivity parameter)
     3. Gama_NA_NA (IDL's maeket share)
     4. I_NA_L (L-SC-AC)
     5. I_NA_F (F-SC-AC)
     6. n_t_NA (Number of supply chain network nodes)
     7. v_t_f (ICF's supply chain imitation speed)
     8. Epsilon_NA_NA (Process efficiency factor)
     9. Theta_NA_NA (Competitive erosion rate)
     ```
   - Output: 9 CSV files, named in the format `final_result_[parameter_name].csv`

### Step 3: Visualization
Run the following scripts in order to generate the paper's figures:

| Figure Number | Corresponding Parameter | Script to Run |
|---------------|-------------------------|---------------|
| Figure 7      | U_t_NA                  | `04_figure_07.R` |
| Figure 8      | Beta_NA_NA              | `05_figure_08.R` |
| Figure 9      | Gama_NA_NA              | `06_figure_09.R` |
| Figure 10     | I_NA_L                  | `07_figure_10.R` |
| Figure 11     | I_NA_F                  | `08_figure_11.R` |
| Figure 12     | n_t_NA                  | `09_figure_12.R` |
| Figure 13     | v_t_f                   | `10_figure_13.R` |
| Figure 14     | Epsilon_NA_NA           | `11_figure_14.R` |
| Figure 15     | Theta_NA_NA             | `12_figure_15.R` |
