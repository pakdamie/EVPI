#The Simplest Age Structure Model
#Let us say that our rate of vaccination is that
#50% of the population will be vaccinated at one time-step.
#But, first we want to vaccinate Age Group 1 to 10
#% before we open it up. 
####################################################
#So we will need two time points to get the vaccination rate to work.


#For quick checking, lets assume that we figured out the rate to vaccinate 
#the total pop based on a percentage.
#I'm vaccinating at a rate where 50% of the total population will be
###vaccinated with one time step (assume a year).

r1 =50 #vaccine rate per total population per year
p1= 0.50 #proportion of vaccine coverage for group 1  

#proportion of vaccine coverage 
parameters_A = c(DT1 = - log(1-p1)/(r1 * 2), #Because we want to vaccinate 
                                         #the first age group first, we 
                                         #have to modify the rate 
                 r = r1 *2)
                                       

y_initial_A <-c(S= c(0.5,0.5),
                SV = c(0,0))

times = seq(0, 5, by=1/10 )


###Very simple 
Simple_AGE_M <- function(t, x, params){
  with(as.list(c(x, params)),{
    
    S = x[1:2] #The Susceptible with two age groups
    SV = x[3:4] #The Vaccinated  wtih two age groups
    
    rate1 = ifelse( t <= DT1 ,r, r/2 ) 
    rate2= ifelse(t >  DT1 ,   r/2, 0 )
    
  
    rf=  c(rate1,rate2)
    
    
    dS = (-rf * S) #The population being vaccinated
    dV = (rf * S) #The population being vaccinated 
    
    
    
    res <- list(c(dS, dV))
    
    return(res)
  })
}

dat=  as.data.frame(ode(y_initial_A, times=times,
                        func =Simple_AGE_M,
                        parms = parameters_A),atol= 1e-16, rtol=1e-16)




plot(dat[,1],rowSums(dat[,4:5]),type='l')
lines(dat[,1],dat[,4],col='red')
lines(dat[,1],dat[,5],col='blue')
abline(h=0.25,col='darkred')
abline(v=0.5,col='darkred')

```

