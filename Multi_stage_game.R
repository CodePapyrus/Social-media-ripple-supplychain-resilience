# This is code for a multi-stage game

# Call the specialized gsl library
library(gsl)

# In parameter settings, A_B_C respectively represent: A is the parameter body, B is the parameter superscript, C is the parameter subscript (NA indicates no corresponding superscript or subscript)

# Define the relevant data frame here
final_result <- data.frame(  # Create an empty data frame
  num_round = numeric(),
  Leader_P = numeric(),  # P
  Follower_P = numeric(),
  Leader_Q = numeric(),  # Q
  Follower_Q = numeric(),
  Leader_R = numeric(),  # R
  Follower_R = numeric()
)
# Define the relevant data frame here

start <- 0.0000001  # Starting value of the variable
end <- 0.85         # Ending value of the variable
step <- 0.05        # Step size of the variable

cal_num <- 0  # Place this outside the K loop

for (k in seq(from=start, to=end, by=step)) {  # This loop is for quickly replacing the corresponding parameters:

cal_num <- cal_num + 1 # Place this inside the K loop
temp_num <- 2 + (cal_num - 1) * 6# Follow the previous line of code
  
final_result[temp_num,1] <- k
final_result[temp_num,2] <- k
final_result[temp_num,3] <- k
final_result[temp_num,4] <- k
final_result[temp_num,5] <- k
final_result[temp_num,6] <- k
final_result[temp_num,7] <- k

# Settings for related basic parameters (if subsequent parameter adjustments are needed, execute them here)
t_NA_NA <- 5  # Number of decision periods
#t_NA_NA <- k

U_t_NA <- numeric(length = t_NA_NA) # The number of rounds of potential market demand is the same as the number of decision periods
P_t_L <- numeric(length = t_NA_NA) # Leader's initial sales price
Beta_NA_NA <- 0.9 # Price sensitivity
#Beta_NA_NA <- k
#Gama_NA_NA <- k # Leader's market sales rate
Gama_NA_NA <- 0.8
I_NA_L <- 20 # Base cost for Leader to reconfigure production line
#I_NA_L <- k
I_NA_F <- 18 # Base cost for Follower to reconfigure production line
#I_NA_F <- k
n_t_NA <- numeric(length = t_NA_NA) # Number of supply chain process steps
#v_t_f <- 3 # Follower's follow speed  
v_t_f <- numeric(length = t_NA_NA)
Lambda_t_NA <- numeric(length = t_NA_NA) # Supply chain improvement ratio [Market share consumption ratio]
a_NA_NA <- 20 # Base for price time decay function  
b_t_NA <- numeric(length = t_NA_NA) # Decision round tiered price 
mu_NA_NA <- 10 # Floor price

# [Supplement] Definition of P_t_F?
P_t_F <- numeric(length = t_NA_NA) # Follower's initial sales price, temporarily set to be consistent with the Leader's initial price.  

# [Supplement] Definition of Leader and Follower revenue functions.
R_t_L <- 0 # Leader's revenue
R_t_F <- 0 # Follower's revenue

# [Supplement] Definition of Q_t_F and Q_T_L?
Q_t_L <- numeric(length = t_NA_NA)
Q_t_F <- numeric(length = t_NA_NA)

# [6.10 Supplement] Calculation of loss function
# Newly inserted parameters related to loss function

Theta_NA_NA <-  0.2# Erosion coefficient
#Theta_NA_NA <- k
Epsilon_NA_NA <-  0.1# Process efficiency coefficient
#Epsilon_NA_NA <- k
Eta_NA_NA <-  0.6# Process delay index
#Eta_NA_NA <- k
K_NA_NA <-  12 # Technology barrier coefficient



# Set the value for each parameter (involving different decision period round numbers) here.
for (i in 1:t_NA_NA) { # Loop through each parameter to import the relevant preset parameters
  U_t_NA[i] <- 1100 - i * 100 # Potential market demand, each execution will generate a random integer between 1000 and 1500, for example, it may output values like 1123 or 1348.
  #U_t_NA[i] <- k - i * 100
  P_t_L[i] <- 500 + (i-1)*50 # Leader's initial selling price, increasing by 50 each year on the existing basis
  #P_t_L[i] <- k + (i-1)*50
  P_t_F[i] <- 500 + (i-1)*50 # Follower's initial selling price, increasing by 50 each year on the existing basis
  n_t_NA[i] <- round(298 * (1 + runif(1,-0.1,0.1)))# Number of supply chain process steps, with a fluctuation of ±10%
  #n_t_NA[i] <- round(k * (1 + runif(1,-0.1,0.1))) 
  v_t_f[i] <- 3 + 0.15 * i # Speed of Follower's replication
  #v_t_f[i] <- k + 0.15 * i
  Lambda_t_NA[i] <- 0.15 # Supply chain improvement ratio is fixed at 0.15
  #Lambda_t_NA[i] <- k
  b_t_NA[i] <- 6 - i # Decision round tiered price  
}

# [6.11 Supplement] Add a line of output to record the output content
print(paste("This modification is: initial selling price P",P_t_L[1]))


# Execute t rounds of loops and output the operation results respectively
for (j in 1:t_NA_NA) {  # Execute loop operation 
  
final_result[temp_num + j,1] <- j  
  


# Calculation and judgment of loss function?  Place at the end of calculation for Follower to use.
Market_loss_rate <- Theta_NA_NA * v_t_f[j] *(1-exp(-1*Eta_NA_NA*n_t_NA[j])) - Epsilon_NA_NA * log(1 + n_t_NA[j])
print(Market_loss_rate) # Output market loss rate to check

# [1] Next, update the selling prices of Leader and Follower
##########################################################################################################

# Lambert function, numerator and denominator separated, parameters for Leader and Follower defined separately
fenzi_Leader <- 0
fenmu_Leader <- 0
fenzi_Follower <- 0
fenmu_Follower <- 0

#  Lambert function numerator and denominator separated 
fenzi_Leader <- exp((2 * Beta_NA_NA * I_NA_L + U_t_NA[j])/2*Beta_NA_NA)  
fenmu_Leader <- (2*Gama_NA_NA*Beta_NA_NA)  

# Temporary data for Leader's Lambert function:
temp_for_L <- fenzi_Leader / fenmu_Leader
# The output value has a problem, ensure it is not negative infinity
#print(temp_for_L)  
temp_L_P <- lambert_W0(temp_for_L,give=FALSE, strict = FALSE)  


fenzi_Follower <- exp((2 * Beta_NA_NA * I_NA_F + U_t_NA[j])/2*Beta_NA_NA)
fenmu_Follower <- (2*(1-Gama_NA_NA)*Beta_NA_NA)

# Temporary data for Follower's Lambert function:
temp_for_F <- fenzi_Follower / fenmu_Follower
# The output value has a problem, ensure it is not negative infinity
#print(temp_for_F)  
temp_F_P <- lambert_W0(temp_for_F,give=FALSE, strict = FALSE) 

P_t_L[j] <-((-2)*Beta_NA_NA*temp_L_P+U_t_NA[j])/(2*Beta_NA_NA)  # Officially update the Leader's price
P_t_F[j] <-((-2)*Beta_NA_NA*temp_F_P+U_t_NA[j])/(2*Beta_NA_NA)  # Officially update the Follower's price
  
# [Output] Prices corresponding to each stage respectively
print(paste("Round",j,"Leader's price is:",P_t_L[j]))
print(paste("Round",j,"Follower's price is:",P_t_F[j]))

final_result[temp_num + j,2] <- P_t_L[j]
final_result[temp_num + j,3] <- P_t_F[j]

# [2] Calculate the sales volumes of Leader and Follower respectively
#########################################################################################################
Q_t_L[j] <- Gama_NA_NA * (U_t_NA[j] - Beta_NA_NA * P_t_L[j]) # Leader's sales volume
Q_t_F[j] <- (1-Gama_NA_NA) * (U_t_NA[j] - Beta_NA_NA * P_t_F[j]) # Follower's sales volume
# Update the impact of product failure rate
Q_t_F[j] <-Q_t_F[j] * (1 - Market_loss_rate)

# [Output] Sales volumes corresponding to each stage respectively
print(paste("Round",j,"Leader's sales volume is:",Q_t_L[j]))
print(paste("Round",j,"Follower's sales volume is:",Q_t_F[j]))

final_result[temp_num + j,4] <- Q_t_L[j]
final_result[temp_num + j,5] <- Q_t_F[j]


##########################################################################

write.csv(final_result,"final_result_Theta_NA_NA.csv",row.names = FALSE) # For each parameter, adjust the output path and file name sequentially
