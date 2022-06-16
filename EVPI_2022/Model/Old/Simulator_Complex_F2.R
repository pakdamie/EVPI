###This is a function for simulating the model with
###different parameters values- specifically,
###this is the code that

###Cleaned and Last updated 7/29/2021

Simulator_func_C <- function(list){
###You feed in the list of parameters
###This function simulates the model depending on what you feed into it

Simulator_func_3 <- function(sus1,trans1,sev,WN,WV_1,WV_2,vacrate,
                             vacall, inc,R0,country){

  ##This determines what country one is looking at

  if(country == 'USA'){
    Rec = US_PROP$prop
    Contact = CONTACT_MAT_US

  } else if (country == 'JAP'){
    Rec = D_JAP
    Contact = W_JAP

  } else if (country== 'SAF'){
    Rec = D_SAF
    Contact = W_SAF
  }

  ###This is the initial population

  y_initial <- c(
      S = 0.95 * Rec,
      SV = 0 * Rec,
      SU = 0 * Rec,
      A = 0.01 * Rec,
      I =  0.02 * Rec,
      AV = 0 * Rec,
      IV = 0 * Rec,
      R = 0.02 * Rec,
      RV = 0 * Rec,
      KI_U = 0 * Rec,
      KI_V = 0 * Rec,
      KA = 0 * Rec,
      VacS= 0 * Rec,
      VacR = 0*Rec
    )

### The priority group- Age1 (18-30 year olds), Age2 (31-59
  ###year olds),and Age3 (60-80 year olds)
 if (inc == "age1") {
  age1 <- seq(18, 30)
  inc <- age1
  prop_age <- sum(Rec[18:30])

  } else if (inc == "age2") {
  age2 <- seq(31, 59)
  inc <- age2
  prop_age <- sum(Rec[31:59])

  } else if (inc == "age3") {
  age3 <- seq(60, 80)
  inc <- age3
  prop_age <- sum(Rec[60:80])
}

###The parameters that we feed in

parms= list(n = 80, #The age compartments
             sus= sus1,  #Susceptibility multiplier
             tran =trans1, #Transmissibility multiplier
             gamma =365/14 , #recovery rate (average = 14 Days)
             pv= 0.98,  #vaccination success (98% effectiveness)
             p_i= 0.80, #symptomatic probability when not vaccinated (20% )
             p_iv= sev, #symptomatic probability when vaccinated
             W =Contact, #The contact matrix of interest
             mu= 1/80, #Mortality rate (inverse of the maximum life-span)
             a = rep(1,80), #Ageing Rate
             N= 1, #The total population
             R0=R0, # The Reproductive Number
             omega_N = WN, #Waning of natural immunity #365/180- default is 6 months,
             omega_V1= WV_1,#Waning of vaccinated immunity 1 (365/365) #Vaccinated Susceptible -> Unvaccinated Susceptible
             omega_V2= WV_2, #Waning of vaccinated immunity 2 (365/180) #Recovered Vaccinated Susceptible -> Vaccinated Susceptible
             r = vacrate, ###Vaccination Rates (25%, 50%, 75%)
             r_sub= vacrate*(1/(sum(Rec[18:80]))),  #Vaccination Rate Adjusted for the fact that only 18-80 are vaccinated
             vacprop = vacall , #How much to vaccinate the priority group before opening up (Default is 75%)
             inc_prop=prop_age,
             inc =inc

 )

 times = seq(0, 2, by=1/12 )


###Our desolver
 dat= as.data.frame(lsoda(y_initial, times=times,
                        func =SAIRV_M_Complex_NVac ,
                        parms = parms,atol= 1e-6, rtol=1e-7))

 return(dat)



}


temp_list <- NULL

for (k in seq(1, length(list))) {
  temp_param <- list[[k]]


  tmp <- Simulator_func_3(
    temp_param[[1]],
    temp_param[[2]],
    temp_param[[3]],
    temp_param[[4]],
    temp_param[[5]],
    temp_param[[6]],
    temp_param[[7]],
    temp_param[[8]],
    temp_param[[9]],
    temp_param[[10]],
    temp_param[[11]])


  temp_list[[k]] <- tmp
}

return(temp_list)
}

