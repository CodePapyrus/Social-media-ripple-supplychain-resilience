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
Run the following script to systematically scan 8 key parameters:

3. **`03_multi_stage_game.R`** - Multi-stage game parameter sensitivity analysis
   - Sequentially scans 8 parameter combinations:
     ```
     1. U_t_NA (Potential market demand)
     2. Gama_NA_NA (IDL's maeket share)
     3. I_NA_L (L-SC-AC)
     4. I_NA_F (F-SC-AC)
     5. n_t_NA (Number of supply chain network nodes)
     6. v_t_f (ICF's supply chain imitation speed)
     7. Epsilon_NA_NA (Process efficiency factor)
     8. Theta_NA_NA (Competitive erosion rate)
     ```
   - Output: 8 CSV files, named in the format `final_result_[parameter_name].csv`

### Step 3: Visualization
Run the following scripts in order to generate the paper's figures:

| Figure Number | Corresponding Parameter | Script to Run |
|---------------|-------------------------|---------------|
| Figure 3      | U_t_NA                  | `Code for Figure 3.R` |
| Figure 4      | Gama_NA_NA              | `Code for Figure 4.R` |
| Figure 5     | I_NA_L                  | `Code for Figure 5.R` |
| Figure 6     | I_NA_F                  | `Code for Figure 6.R` |
| Figure 7     | n_t_NA                  | `Code for Figure 7.R` |
| Figure 8     | v_t_f                   | `Code for Figure 8.R` |
| Figure 9     | Epsilon_NA_NA           | `Code for Figure 9.R` |
| Figure 10     | Theta_NA_NA             | `Code for Figure 10.R` |
