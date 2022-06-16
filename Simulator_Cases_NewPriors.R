#######################################
###You need to do a data.frame  ###
#######################################
###################################################################                                                                 #
#x should have the susceptibility identifier in the first column #
#, the second column should have the values,                      #
# the third column should have the priors,                       #
# the fourth column should identify if they are the the          #
# equal-case scenario, worst case scenario, or the best case     #
# scenario                                                       #
#################################################################

###We simulate the infection here here
Simulator_Cases_NewPriors <- function(x){
  ######################################
  ###There are six uncertainties- these#
  ###split up to the uncertainties- id#
  ######################################
  tmp <- split(x, x$id)

  ###We can then average them by summing up value * priors
  averages <- lapply(tmp, function(x) sum(x$value * x$ prior))

  #Create it a data.frame
  average_value <- cbind.data.frame(id = names(averages),
                       value= do.call(rbind, averages),
                       priors=NA)

  ###The order that we need
  Uncertainty <- c("sus","trans","sev","wn","wv1","wv2")


  OUTPUT = NULL
  for (u in seq(1, 6)){
    par_interest =  Uncertainty[[u]]
    values_interest <- subset(x,x$id == par_interest)
    ###Average the other uncertainties and do this for different
    ###vaccination rate/R0/countries

    Parameter_Values_df <-
       rbind(values_interest,
             average_value[average_value$id!= par_interest ,])

    vacrate <-c(0.287, 0.69,1.38) #Vaccine rate:
    vacall <- 0.75 #The proportion that the first priority group is vaccinated to

    ###The priority actions- which age groups to deal first
    inc <- c('age1','age2','age3')

    R0 <- c(1.15, 1.5,2.5) #Default is 2.5 1.15, 1.5,
    country <- c("JPN","SAF","USA")

    # make a lot of t

    PAR <-  expand.grid(
                sus=  subset(Parameter_Values_df,
                             Parameter_Values_df$id =='sus')$value,
                trans=  subset(Parameter_Values_df,
                             Parameter_Values_df$id =='trans')$value,
                sev =  subset(Parameter_Values_df,
                               Parameter_Values_df$id =='sev')$value,
                wn =  subset(Parameter_Values_df,
                              Parameter_Values_df$id =='wn')$value,
                wv1 =  subset(Parameter_Values_df,
                             Parameter_Values_df$id =='wv1')$value,
                wv2 =  subset(Parameter_Values_df,
                              Parameter_Values_df$id =='wv2')$value,
                vacrate, vacall,inc, R0, country)


  Splitted <- split(PAR, 1:nrow(PAR))


  OUTPUT[[u]]<-Simulator_func_C(Splitted)

  }
  return(OUTPUT)

}


Simulator_Cases_NewPriors_DF <- function(x){
  ######################################
  ###There are six uncertainties- these#
  ###split up to the uncertainties     #
  ######################################
  tmp <- split(x, x$id)

  averages <- lapply(tmp, function(x) sum(x$value * x$ prior))



  average_value<- cbind.data.frame(id = names(averages),
                                   value= do.call(rbind, averages),
                                   priors=NA)

  Uncertainty <- c("sus","trans","sev","wn","wv1","wv2")


  OUTPUT = NULL
  for (u in seq(1, 6)){
    par_interest =  Uncertainty[[u]]
    values_interest <- subset(x,x$id == par_interest)

    ###Average the other uncertainties and do this for different
    ###vaccination rate/R0/countries

    Parameter_Values_df <-
      rbind(values_interest,
            average_value[average_value$id!=  par_interest ,])

    vacrate <-c(0.287, 0.69,1.38) #Vaccine rate:
    vacall <- 0.75 #The proportion that the first priority group is vaccinated to

    ###The priority actions- which age groups to deal first
    inc <- c('age1','age2','age3')

    R0 <- c(1.15, 1.5,2.5) #Default is 2.5 1.15, 1.5,
    country <- c("JPN","SAF","USA")

    # make a lot of t

    PAR <-  expand.grid(
      sus=  subset(Parameter_Values_df,
                   Parameter_Values_df$id =='sus')$value,
      trans=  subset(Parameter_Values_df,
                     Parameter_Values_df$id =='trans')$value,
      sev =  subset(Parameter_Values_df,
                    Parameter_Values_df$id =='sev')$value,
      wn =  subset(Parameter_Values_df,
                   Parameter_Values_df$id =='wn')$value,
      wv1 =  subset(Parameter_Values_df,
                    Parameter_Values_df$id =='wv1')$value,
      wv2 =  subset(Parameter_Values_df,
                    Parameter_Values_df$id =='wv2')$value,
      vacrate, vacall,inc, R0, country)


    OUTPUT[[u]]<- PAR

  }
  return(OUTPUT)
}

