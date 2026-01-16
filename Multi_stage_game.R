#这里是多阶段博弈的代码

#调用专门的gsl库
library(gsl)

#在参数设置中，A_B_C分别表示：A是参数本体，B是参数上标，C是参数下标（NA表示没有对应的上标或者下标）

#在这里框定相关的数据框
final_result <- data.frame(  #建立一个空的数据框
  num_round = numeric(),
  Leader_P = numeric(),  #P
  Follower_P = numeric(),
  Leader_Q = numeric(),  #Q
  Follower_Q = numeric(),
  Leader_R = numeric(),  #R
  Follower_R = numeric()
)
#在这里框定相关的数据框

start <- 0.0000001  #变量开始的值
end <- 0.85   #变量结束的值
step <- 0.05    #变量的步长

cal_num <- 0  #这个放到K循环的外面

for (k in seq(from=start, to=end, by=step)) {  #这个循环是为了快速替换对应的参数：

cal_num <- cal_num + 1 #这个放到K循环里面
temp_num <- 2 + (cal_num - 1) * 6#紧跟着上面一行代码
  
final_result[temp_num,1] <- k
final_result[temp_num,2] <- k
final_result[temp_num,3] <- k
final_result[temp_num,4] <- k
final_result[temp_num,5] <- k
final_result[temp_num,6] <- k
final_result[temp_num,7] <- k

#相关基础参数的设置（如果后续参数调整，就在这里执行）
t_NA_NA <- 5  #决策期的轮数
#t_NA_NA <- k

U_t_NA <- numeric(length = t_NA_NA) #潜在市场需求的轮数和决策期的轮数是相同的
P_t_L <- numeric(length = t_NA_NA) #Leader的初始销售价格
Beta_NA_NA <- 0.9 #价格敏感度
#Beta_NA_NA <- k
#Gama_NA_NA <- k #领导者市场销售率
Gama_NA_NA <- 0.8
I_NA_L <- 20 #Leader重新配置生产线的成本基础
#I_NA_L <- k
I_NA_F <- 18 #Follower重新配置生产线的成本基础
#I_NA_F <- k
n_t_NA <- numeric(length = t_NA_NA) #供应链流程步骤数量
#v_t_f <- 3 #Follower追随的速度  
v_t_f <- numeric(length = t_NA_NA)
Lambda_t_NA <- numeric(length = t_NA_NA) #供应链改进比例【市场份额消耗比例】
a_NA_NA <- 20 #价格时间衰减函数的基础  
b_t_NA <- numeric(length = t_NA_NA) #决策轮次阶梯价格 
mu_NA_NA <- 10 #地板价格

#【补充】对P_t_F的定义？
P_t_F <- numeric(length = t_NA_NA) #Follower的初始销售价格，暂定和Leader的初始价格一致。  

#【补充】对Leader和Follower收益函数的定义。
R_t_L <- 0 #Leader的收益
R_t_F <- 0 #Follower的收益

#【补充】对Q_t_F和Q_T_L的定义？
Q_t_L <- numeric(length = t_NA_NA)
Q_t_F <- numeric(length = t_NA_NA)

#【6.10补充】对损失函数的计算
#新插入损失函数相关的参数

Theta_NA_NA <-  0.2#侵蚀系数
#Theta_NA_NA <- k
Epsilon_NA_NA <-  0.1#流程效率系数
#Epsilon_NA_NA <- k
Eta_NA_NA <-  0.6#流程延迟指数
#Eta_NA_NA <- k
K_NA_NA <-  12 #技术壁垒系数！！！后续未用到！！！



#在此设定每个（涉及不同决策周期论数的值）参数的值。
for (i in 1:t_NA_NA) { #循环每一个参数，来导入相关的预设参数
  U_t_NA[i] <- 1100 - i * 100 #潜在市场需求,每次执行会产生1000~1500之间的随机整数,例如可能输出1123或1348等值。
  #U_t_NA[i] <- k - i * 100
  P_t_L[i] <- 500 + (i-1)*50 #Leader初始售价，在现有的基础上，逐年增加50
  #P_t_L[i] <- k + (i-1)*50
  P_t_F[i] <- 500 + (i-1)*50 #Follower初始售价，在现有的基础上，逐年增加50
  n_t_NA[i] <- round(298 * (1 + runif(1,-0.1,0.1)))#供应链流程步骤数量，上下10%进行浮动
  #n_t_NA[i] <- round(k * (1 + runif(1,-0.1,0.1))) 
  v_t_f[i] <- 3 + 0.15 * i #Follower复刻的速度
  #v_t_f[i] <- k + 0.15 * i
  Lambda_t_NA[i] <- 0.15 #供应链改进比例恒定为0.15！！！后续未用到，调整为市场丢失率！！！
  #Lambda_t_NA[i] <- k
  b_t_NA[i] <- 6 - i #决策轮次阶梯价格  ！！！后续未用到！！！
}

#【6.11补充】增加一行输出，用于记录输出内容
print(paste("本次修改的是：初始售价P",P_t_L[1]))


#执行t轮循环，并分别输出运算结果
for (j in 1:t_NA_NA) {  #执行循环运算 
  
final_result[temp_num + j,1] <- j  
  
#【6.10补充】具体调用向量中的哪一个数据？【6.10补充】
#【6.10已解决】

#【6.10补充】损失函数的计算和判断？【6.10补充】 放在计算最后，给Follower用的。
Market_loss_rate <- Theta_NA_NA * v_t_f[j] *(1-exp(-1*Eta_NA_NA*n_t_NA[j])) - Epsilon_NA_NA * log(1 + n_t_NA[j])
print(Market_loss_rate) #输出市场失效率检查一下

#【1】接着，更新Leader和Follower的售价
##########################################################################################################

#朗博函数，分子和分母分离，Leader和Follower的参数分别定义
fenzi_Leader <- 0
fenmu_Leader <- 0
fenzi_Follower <- 0
fenmu_Follower <- 0

#【6.10补充】朗博函数分子和分母分离【6.10补充】  
fenzi_Leader <- exp((2 * Beta_NA_NA * I_NA_L + U_t_NA[j])/2*Beta_NA_NA)  
fenmu_Leader <- (2*Gama_NA_NA*Beta_NA_NA)  

#临时数据，为了Leader的朗博函数：
temp_for_L <- fenzi_Leader / fenmu_Leader
#输出值出问题了，保证不能为负无穷
#print(temp_for_L)  
temp_L_P <- lambert_W0(temp_for_L,give=FALSE, strict = FALSE)  


fenzi_Follower <- exp((2 * Beta_NA_NA * I_NA_F + U_t_NA[j])/2*Beta_NA_NA)
fenmu_Follower <- (2*(1-Gama_NA_NA)*Beta_NA_NA)

#临时数据，为了Follower的朗博函数：
temp_for_F <- fenzi_Follower / fenmu_Follower
#输出值出问题了，保证不能为负无穷
#print(temp_for_F)  
temp_F_P <- lambert_W0(temp_for_F,give=FALSE, strict = FALSE) 

P_t_L[j] <-((-2)*Beta_NA_NA*temp_L_P+U_t_NA[j])/(2*Beta_NA_NA)  #正式更新Leader的价格
P_t_F[j] <-((-2)*Beta_NA_NA*temp_F_P+U_t_NA[j])/(2*Beta_NA_NA)  #正式更新Follower的价格
  
#【输出】每一个阶段分别对应的价格
print(paste("第",j,"轮次Leader的价格为：",P_t_L[j]))
print(paste("第",j,"轮次Follower的价格为：",P_t_F[j]))

final_result[temp_num + j,2] <- P_t_L[j]
final_result[temp_num + j,3] <- P_t_F[j]

#【2】分别计算Leader和Follower两者的销量
#########################################################################################################
Q_t_L[j] <- Gama_NA_NA * (U_t_NA[j] - Beta_NA_NA * P_t_L[j]) #Leader的销量
Q_t_F[j] <- (1-Gama_NA_NA) * (U_t_NA[j] - Beta_NA_NA * P_t_F[j]) #Follower的销量
#更新产品失效率的影响
Q_t_F[j] <-Q_t_F[j] * (1 - Market_loss_rate)

#【输出】每一个阶段分别对应的销量
print(paste("第",j,"轮Leader的销量为：",Q_t_L[j]))
print(paste("第",j,"轮Follower的销量为：",Q_t_F[j]))

final_result[temp_num + j,4] <- Q_t_L[j]
final_result[temp_num + j,5] <- Q_t_F[j]


##########################################################################

write.csv(final_result,"final_result_Theta_NA_NA.csv",row.names = FALSE) #针对每个参数，依次调整输出路径和文件名

