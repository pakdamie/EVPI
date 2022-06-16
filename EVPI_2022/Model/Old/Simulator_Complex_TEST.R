
#' Simulating the leaky vaccine
#'
#' @param sus1
#' @param trans1
#' @param sev
#' @param wN
#' @param wV
#' @param inc
#'
#' @return
#' @export
#'
#' @examples
#'
#'
#'
#'
Simulator_func_C <- function(list){


Simulator_func_3 <- function(sus1,trans1, sev, WN,WV, vacrate, vacall, inc){



  Rec = c(.5,.5) #US_PROP$prop


 Allocation =rep(0,2)
 Allocation[1]<-1


 parameters= c(
            a =0.5)



state <-(S=20)

 times = seq(0,5,1/12)


 initvalues <- c(x = 1)
 #Define the model parameter: intrinsic growth rate
 parms <- c(a = -.5)
 #Define the times over which you want a solution
 times <- seq(0, 2, 1)


 dat= as.data.frame(lsoda(y_initial, times=times,
                        func =Test_vac,
                        parms = parms, atol = 1e-12,rtol=1e-12))


plot(dat[,1],dat[,2],type='l',col='red')
lines(dat[,1],dat[,3],type='l',col='green')
}

plot(dat[,1],dat[,4],type='l',col='red')
lines(dat[,1],dat[,5],type='l',col='green')


plot

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
    temp_param[[8]]
  )


  temp_list[[k]] <- tmp
}

return(temp_list)
}
