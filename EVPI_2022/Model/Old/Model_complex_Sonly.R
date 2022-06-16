#
#' @param t
#' @param x
#' @param params
#'
#' @return A function to be passed into deSolve
#' @export
#'
#' @examples ode(..., func = SAIRV)
SAIRV_M_Complex_S <-function(t, x, params) {
    n <- params$n # The number of subcompartments (related to maximum age -80)
    #################
    ### Compartments##
    #################
    ## States###
    S <- x[1:n] ### The Susceptible Unvaccinated Class
    SV <- x[(n + 1):(2 * n)] ### The Susceptible Vaccinated Class
    SU <- x[((2 * n) + 1):(3 * n)] # The Susceptible Vaccinated (failed) Class
    A <- x[((3 * n) + 1):(4 * n)] # The Asymptomatic infectious Class
    I <- x[((4 * n) + 1):(5 * n)] ### The Symptomatic infectious class
    AV <- x[((5 * n) + 1):(6 * n)] ### The Asymptomatic infectious Class (Vac)
    IV <- x[((6 * n) + 1):(7 * n)] ### The Symptomatic infectious class (Vac)
    R <- x[((7 * n) + 1):(8 * n)] ### The recovered (Unvaccinated/Failed Take)
    RV <- x[((8 * n) + 1):(9 * n)] ### The recovered (Vaccinated Class)
    #######################
    ### Incidence Classes###
    #######################
    KI <- x[((9 * n) + 1):(10 * n)] ### The symptomatic infection incidence class
    KA <- x[((10 * n) + 1):(11 * n)] ### The asymptomatic infection incidence class
    Vac <- x[((11 * n) + 1):(12 * n)] ### The Vaccine incidence
    ### The vaccine allocation code ###
    ### This is saying looking at the proportion of the total population
    ### that are vaccine eligible- 18 to 80 years olds
    ### (Rec[18:80]) and susceptible (*95%).
    ### The vaccination rate is at the initial vaccination rate
    ### when the total incidence (sum(Vac)) is LESS than the
    ### percentage of the susceptible, vaccine eligible population
    ### that we wanted to vaccinate (Vac_ALL, so if 75% of the
    ### susceptible, vaccine eligible population are needed to be
    ### vaccinated than we would let the model run until the sum(Vac)
    # is greater than the population we need to vaccinate.

      with(as.list(params), {
      v <- ifelse(sum(Vac) < Vac_All * (sum(Rec[18:80]) * 0.95),
      v, rep(0, 80)

        )
      ### The allocation strategy is dependent on the order that
      ### you give the model (params$Order). The for-loop goes in
      ### order. So if you have an age-decreasing order (80->18),
      ### then the first entry of Order is 80. If you have an
      ### age-increasing order (18->80), then the first entry is 18.
      if (sum(Vac[Order]) >= 0.50 * sum(S_initial[Order]))
        {
        Allocation[18:80] <- 1 }
      else{
        Allocation = Allocation
      }

      ### This is the contact-matrix part of the FOI
      ### Here W is the contact matrix, tran is the parameter
      ### that controls the transmissibility of the vaccinated
      ### population. The second part (tran *(AV+ IV)+(A+I)/N) is
      ### the infection prevalence with N representing the total populatoin
      ### This is presumed to be 1 because the total population should
      ### be constant

      NJ =  S  + SV + SU + A + I + AV + IV + R +RV


      WI <- W %*% (tran * ((AV + IV) + (A + I) ) ) # The contact matrix times the prevalence
      WI[!is.finite(WI)] <- 0 # Insures no wonkiness
      # The first part of the FOI is the transmission coefficient that
      ### is represented by R0*(gamma + mu). We have a susceptibility paramater
      ### sus that reduces the susceptibility of the vaccinated population.
      phi_S <- R0 * (gamma + mu) * (WI) # FOI for unvaccinated/failed
      phi_V <- (sus * (R0 * (gamma + mu)) * (WI)) # FOI for vaccinated
      ### For the susceptible unvaccinated population,
      ### we have the birth rate as the first part.
      ### Remember the only death we have is the last age-compartment
      ### Ottar's Recommendation and we have the ageing out.
      ### To balanace out we need to have the last compartment
      ### of each state times those that aged out (a) and died (mu).
      ### All stages then have ageing in and out
      ### Some are going to be infected (phi_S), some are going
      ### to be vaccinated (Depending on age group)
      # There is an inflow of those that lost their immunity
      # aka those from natural population (omega_N) and those from the
      # vaccinated population (omega_V)
      # Susceptible Unvaccinated Population
      dS <- c((mu + 1) * (S[n] + SV[n] + SU[n] + I[n] + A[n] + AV[n] + IV[n]
        + R[n] + RV[n]), rep(0, n - 1)) +
        c(0, a[1:(n - 1)] * S[1:(n - 1)]) -
        (a * S) - phi_S * S - c(rep(0, n - 1), mu) * S -
        Allocation * v * S + omega_N * R + omega_V * SV
      ### The susceptible vaccinated population is dependent on
      ### pv which determines the probability of successful vaccination
      ### We then have susceptibles being lost to infection controlled
      ### by FOI of vaccinated people. Then we have inflow
      ### of the recovered individuals who have lost immunity
      # Susceptible Vaccinated Population
      dSV <- (pv * Allocation * v * S)  +
        c(0, a[1:(n - 1)] * SV[1:(n - 1)]) -
        (a + phi_V) * SV - c(rep(0, n - 1), mu) * SV - omega_V * SV + omega_V * RV
      ### The  susceptible failed vaccinated population is dependent on
      ### 1-pv which determines the probability of unsuccessful vaccination
      ### We then have susceptibles being lost to infection controlled
      ### by FOI of unvaccinated  people.
      # Susceptible Failed Vaccinated Population
      dSU <- (1 - pv) * Allocation * v * S  +
        c(0, a[1:(n - 1)] *
          SU[1:(n - 1)]) - (a * SU) - (phi_S * SU) - c(rep(0, n - 1), mu) * SU
      ### The  Asymptomatic Failed/Unvaccinated Population
      ### is dependent on 1-pi which determines the
      ### probability that an infectious individual have asymptomatic
      ### disease. We then have the recovery rate of gamma
      # Asymptomatic Failed/Unvaccinated Population
      dA <- (1 - p_i) * phi_S * (S) + (1 - p_i) * phi_S * SU +
        c(0, a[1:(n - 1)] * A[1:(n - 1)]) -
        (a * A) - c(rep(0, n - 1), mu) * A - gamma * A
      ### The  Symptomatic Failed/Unvaccinated Population
      ### is dependent on pi which determines the
      ### probability that an infectious individual have symptomatic
      ### disease. We then have the recovery rate of gamma
      # Symptomatic Failed/Unvaccinated Population
      dI <- p_i * phi_S * (S) + p_i * phi_S * SU +
        c(0, a[1:(n - 1)] * I[1:(n - 1)]) -
        (a * I) - c(rep(0, n - 1), mu) * I - gamma * I
      ### The  Asymptomatic Vaccinated Population
      ### is dependent on 1-p_iv which determines the
      ### probability that an infectious individual have asymptomatic
      ### disease. We then have the recovery rate of gamma
      # Asymptomatic Vaccinated Population
      dAV <- (1 - p_iv) * phi_V * (SV) +
        c(0, a[1:(n - 1)] * AV[1:(n - 1)]) -
        (a) * AV - c(rep(0, n - 1), mu) * AV - gamma * AV
      ### The  Symptomatic Vaccinated Population
      ### is dependent on p_iv which determines the
      ### probability that an infectious individual have asymptomatic
      ### disease. We then have the recovery rate of gamma
      # Symptomatic Vaccinated Population
      dIV <- p_iv * phi_V * (SV) +
        c(0, a[1:(n - 1)] * IV[1:(n - 1)]) -
        (a) * IV - c(rep(0, n - 1), mu) * IV - gamma * IV
      ### The Failed/Unvaccinated Recovered Population
      ### We have an inflow depending on gamma (recovery rate) and then
      ### we have the loss due to waning of immunity
      # Failed/Unvaccinated Recovered Population
      dR <- gamma * (I + A) + c(0, a[1:(n - 1)] * R[1:(n - 1)]) -
        a * R - c(rep(0, n - 1), mu) * R - omega_N * R
      ### The Vaccinated Recovered Population
      ### We have an inflow depending on gamma (recovery rate) and then
      ### we have the loss due to waning of immunity
      # Vaccinated Recovered Population
      dRV <- gamma * (IV + AV) + c(0, a[1:(n - 1)] * RV[1:(n - 1)]) -
        a * RV - c(rep(0, n - 1), mu) * RV - omega_N * RV
      ##############
      ### INCIDENCE #
      ##############
      ### The symptomatic infectious (all)
      KI <- (p_i) * phi_S * (S + SU) + (p_iv) * phi_V * (SV)
      ### The asymptomatic infectious (all)
      KA <- (1 - p_i) * phi_S * (S + SU) + (1 - p_iv) * phi_V * (SV) ### The asymptomatic infection incidence class
      ### The total vaccination (doesn't depend if it's failed or not)
      Vac <- (Allocation * v * S)  ### The Vaccine incidence
      res <- c(dS, dSV, dSU, dA, dI, dAV, dIV, dR, dRV, KI, KA, Vac)
      list((res))
    })
  }
