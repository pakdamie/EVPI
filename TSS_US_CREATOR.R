### This is a function to run different combinations
### of the 6 uncertainties (not the single EVPI where
### we look at the priors aka figure 1)

### I had to split apart the data.frame because
### R got mad (low memory)

### UNCERTAINTIES
sus_l_h <- c(0.10, 0.5, 1)
tran_l_h <- c(0.10, 0.5, 1)
sev_l_h <- c(0.1, 0.25, 0.5)
WN_l_h <-  c(0, 0.5, 1, 2)
WV1_l_h <- c(0, 0.5, 1, 2)
WV2_l_h <- c(0, 0.5, 1, 2)


### Vaccination Rate (25%,50%,75%)
vacrate <- c(0.287, 0.69, 1.38)
###The percentage of the prioritized age class
vacall <- 0.75
###The action of the prioritized age class
inc <- c("age1", "age2", "age3")
### Different R0
R0 <- c(1.15, 1.5, 2.5)

###The super Grid_USa basically creates all possible
###combinations of the uncertanties and situational factors-
###This is only for united states so I kept country = USA
Super_Grid_USA <- expand.grid(
  sus_l_h,
  tran_l_h,
  sev_l_h,
  WN_l_h,
  WV1_l_h,
  WV2_l_h,
  vacrate,
  vacall,
  inc, R0, "USA"
)

super_grid_list_USA <- split(Super_Grid_USA, 1:nrow(Super_Grid_USA))

###################
### UNITED STATES##
###################
TSS_US_1 <- Simulator_func_C(super_grid_list_USA[1:9331])
save(TSS_US_1, file = "TSS_US_1.RData")
remove(TSS_US_1)

TSS_US_2 <- Simulator_func_C(super_grid_list_USA[9332:18663])
save(TSS_US_2, file = "TSS_US_2.RData")
remove(TSS_US_2)

TSS_US_3 <- Simulator_func_C(super_grid_list_USA[18664:27995])
save(TSS_US_3, file = "TSS_US_3.RData")
remove(TSS_US_3)

TSS_US_4 <- Simulator_func_C(super_grid_list_USA[27996:37426])
save(TSS_US_4, file = "TSS_US_4.RData")
remove(TSS_US_4)

TSS_US_5 <- Simulator_func_C(super_grid_list_USA[37427:46656])
save(TSS_US_5, file = "TSS_US_5.RData")
remove(TSS_US_5)
########################################################################
###Lol don't run this? It takes overnight- just look at the data#######
###files I provided                                             #######
#######################################################################

###Do not criticize my lazy, copy-paste method, but
###i take the raw trajectories above and use the output
###calculator to calculate cases and deaths

TSS_US_1_OUTPUT <- Output_Calculator(
  TSS_US_1, Super_Grid_USA[1:9331, ], 1
)
save(TSS_US_1_OUTPUT, file = "TSS_US_1_OUTPUT.RData")



TSS_US_2_OUTPUT <- Output_Calculator(
  TSS_US_2, Super_Grid_USA[9332:18663, ], 1
)
save(TSS_US_2_OUTPUT, file = "TSS_US_2_OUTPUT.RData")



TSS_US_3_OUTPUT <- Output_Calculator(
  TSS_US_3, Super_Grid_USA[18664:27995, ], 1
)
save(TSS_US_3_OUTPUT, file = "TSS_US_3_OUTPUT.RData")



TSS_US_4_OUTPUT <- Output_Calculator(
  TSS_US_4, Super_Grid_USA[27996:37426, ], 1
)
save(TSS_US_4_OUTPUT, file = "TSS_US_4_OUTPUT.RData")


TSS_US_5_OUTPUT <- Output_Calculator(
  TSS_US_5, Super_Grid_USA[37427:46656, ], 1
)
save(TSS_US_5_OUTPUT, file = "TSS_US_5_OUTPUT.RData")

###########################################

Super_Grid_SAF <- expand.grid(
  sus_l_h, tran_l_h, sev_l_h, WN_l_h, WV1_l_h,
  WV2_l_h, vacrate, vacall, inc, R0, "SAF"
)

super_grid_list_SAF <- split(Super_Grid_SAF, 1:nrow(Super_Grid_SAF))


TSS_SAF_1 <- Simulator_func_C(super_grid_list_SAF[1:9331])
save(TSS_SAF_1, file = "TSS_SAF_1.RData")

TSS_SAF_1_OUTPUT <- Output_Calculator(
  TSS_SAF_1, Super_Grid_SAF[1:9331, ], 1
)

save(TSS_SAF_1_OUTPUT, file = "TSS_SAF_1_OUTPUT.RData")
remove(TSS_SAF_1)
remove(TSS_SAF_1_OUTPUT)

TSS_SAF_2 <- Simulator_func_C(super_grid_list_SAF[9332:18663])
save(TSS_SAF_2, file = "TSS_SAF_2.RData")

TSS_SAF_2_OUTPUT <- Output_Calculator(
  TSS_SAF_2, Super_Grid_SAF[9332:18663, ], 1
)
save(TSS_SAF_2_OUTPUT, file = "TSS_SAF_2_OUTPUT.RData")
remove(TSS_SAF_2)
remove(TSS_SAF_2_OUTPUT)

TSS_SAF_3 <- Simulator_func_C(super_grid_list_SAF[18664:27995])
save(TSS_SAF_3, file = "TSS_SAF_3.RData")
TSS_SAF_3_OUTPUT <- Output_Calculator(
  TSS_SAF_3, Super_Grid_SAF[18664:27995, ], 1
)
save(TSS_SAF_3_OUTPUT, file = "TSS_SAF_3_OUTPUT.RData")
remove(TSS_SAF_3)
remove(TSS_SAF_3_OUTPUT)



TSS_SAF_4 <- Simulator_func_C(super_grid_list_SAF[27996:37426])
save(TSS_SAF_4, file = "TSS_SAF_4.RData")
TSS_SAF_4_OUTPUT <- Output_Calculator(
  TSS_SAF_4, Super_Grid_SAF[27996:37426, ], 1
)
save(TSS_SAF_4_OUTPUT, file = "TSS_SAF_4_OUTPUT.RData")
remove(TSS_SAF_4)
remove(TSS_SAF_4_OUTPUT)


TSS_SAF_5 <- Simulator_func_C(super_grid_list_SAF[37427:46656])

### Something wonky about the parameter values here,
### have to decrease atol and rtol
### as.data.frame(lsoda(y_initial, times=times,
### func =SAIRV_M_Complex_NVac ,
### parms = parms,atol= 1e-8, rtol=1e-8))

save(TSS_SAF_5, file = "TSS_SAF_5.RData")
TSS_SAF_5_OUTPUT <- Output_Calculator(
  TSS_SAF_5, Super_Grid_SAF[37427:46656, ], 1
)
save(TSS_SAF_5_OUTPUT, file = "TSS_SAF_5_OUTPUT.RData")
remove(TSS_SAF_5)
remove(TSS_SAF_5_OUTPUT)



###

###


Super_Grid_JPN <- expand.grid(
  sus_l_h, tran_l_h, sev_l_h, WN_l_h, WV1_l_h,
  WV2_l_h, vacrate, vacall, inc, R0, "JPN"
)

super_grid_list_JPN <- split(Super_Grid_JPN, 1:nrow(Super_Grid_JPN))


TSS_JPN_1 <- Simulator_func_C(super_grid_list_JPN[1:9331])
save(TSS_JPN_1, file = "TSS_JPN_1.RData")
TSS_JPN_1_OUTPUT <- Output_Calculator(
  TSS_JPN_1, Super_Grid_JPN[1:9331, ], 1
)
save(TSS_JPN_1_OUTPUT, file = "TSS_JPN_1_OUTPUT.RData")
remove(TSS_JPN_1)
remove(TSS_JPN_1_OUTPUT)

TSS_JPN_2 <- Simulator_func_C(super_grid_list_JPN[9332:18663])
save(TSS_JPN_2, file = "TSS_JPN_2.RData")

TSS_JPN_2_OUTPUT <- Output_Calculator(
  TSS_JPN_2, Super_Grid_JPN[9332:18663, ], 1
)
save(TSS_JPN_2_OUTPUT, file = "TSS_JPN_2_OUTPUT.RData")
remove(TSS_JPN_2)
remove(TSS_JPN_2_OUTPUT)

TSS_JPN_3 <- Simulator_func_C(super_grid_list_JPN[18664:27995])
save(TSS_JPN_3, file = "TSS_JPN_3.RData")
TSS_JPN_3_OUTPUT <- Output_Calculator(
  TSS_JPN_3, Super_Grid_JPN[18664:27995, ], 1
)
save(TSS_JPN_3_OUTPUT, file = "TSS_JPN_3_OUTPUT.RData")
remove(TSS_JPN_3)
remove(TSS_JPN_3_OUTPUT)



TSS_JPN_4 <- Simulator_func_C(super_grid_list_JPN[27996:37426])
save(TSS_JPN_4, file = "TSS_JPN_4.RData")
TSS_JPN_4_OUTPUT <- Output_Calculator(
  TSS_JPN_4, Super_Grid_JPN[27996:37426, ], 1
)
save(TSS_JPN_4_OUTPUT, file = "TSS_JPN_4_OUTPUT.RData")
remove(TSS_JPN_4)
remove(TSS_JPN_4_OUTPUT)


TSS_JPN_5 <- Simulator_func_C(super_grid_list_JPN[37427:46656])
save(TSS_JPN_5, file = "TSS_JPN_5.RData")
TSS_JPN_5_OUTPUT <- Output_Calculator(
  TSS_JPN_5, Super_Grid_JPN[37427:46656, ], 1
)
save(TSS_JPN_5_OUTPUT, file = "TSS_JPN_5_OUTPUT.RData")
remove(TSS_JPN_5)
remove(TSS_JPN_5_OUTPUT)

###############################################
###############################################

TSS_US_ALL <- rbind(
  TSS_US_1_OUTPUT,
  TSS_US_2_OUTPUT,
  TSS_US_3_OUTPUT,
  TSS_US_4_OUTPUT,
  TSS_US_5_OUTPUT
)
save(TSS_US_ALL, file = "TSS_US_ALL.RData")

TSS_JPN_ALL <- rbind(
  TSS_JAP_1_OUTPUT,
  TSS_JAP_2_OUTPUT,
  TSS_JAP_3_OUTPUT,
  TSS_JAP_4_OUTPUT,
  TSS_JAP_5_OUTPUT
)
save(TSS_JPN_ALL, file = "TSS_JPN_ALL.RData")


TSS_SAF_ALL <- rbind(
  TSS_SAF_1_OUTPUT,
  TSS_SAF_2_OUTPUT,
  TSS_SAF_3_OUTPUT,
  TSS_SAF_4_OUTPUT,
  TSS_SAF_5_OUTPUT
)
save(TSS_SAF_ALL, file = "TSS_SAF_ALL.RData")
