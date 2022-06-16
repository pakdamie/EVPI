### This is the script for the expected value
### of partial perfect information

EVXI <- function(x, VAR, vac) {

  ### This gives the column new names and helps me
  ### subset
  col_names <- c(
    "times", "x", "sus1",
    "trans1", "sev", "WN", "WV1",
    "WV2", "vacrate",
    "vacall", "inc",
    "R0", "country", "mort"
     )

  x <- lapply(x, setNames, nm = col_names)

  PARTIAL_EVPI <- NULL

  ### We're doing 9 because it's different
  ### vacrate (3) and R0 (3)

  for (d in seq(1, 9)) {
    Max_Mean_UC_C <- NULL
    Max_Mean_UC_M <- NULL

    Max_Mean_Net_C <- NULL
    Mean_Net_T <- NULL

    Max_Mean_Net_M <- NULL
    Mean_Net_T_M <- NULL

    v <- ifelse(vac == "vac", 4, 3) ### Remember v has 4 different
    ### values

    for (k in seq(1, v)) {
      ALL_OUTPUT <- split(x[[d]], x[[d]][, VAR])[[k]]
      ### This would give you the outputs
      ### for the selected parameter-
      ### For example, if the chosen parameter
      ### that we are interested in is suceptbility
      ### THEN, k = 1 would give output where
      ### susceptibility is all 0.10, k = 2 would give
      ## me 0.5, and k =3 would give me 1

      ### We split the ALL_OUTPUT by the Actions

      tmp <- split(ALL_OUTPUT, ALL_OUTPUT$inc)

      ### We look at the three different actions (ages)
      ### for the cases
      Ages_C <- cbind.data.frame(
        age1 = tmp[[1]]$x,
        age2 = tmp[[2]]$x,
        age3 = tmp[[3]]$x
      )

      ### We look at the three different actions (ages)
      ### for the mortality
      Ages_M <- cbind.data.frame(
        age1 = tmp[[1]]$mort,
        age2 = tmp[[2]]$mort,
        age3 = tmp[[3]]$mort
      )

      ### We look at the uncertainty which
      Max_Mean_UC_C[[k]] <- colMeans(Ages_C)
      Max_Mean_UC_M[[k]] <- colMeans(Ages_M)

      ### We then choose the minimum
      Max_Mean_Net_C[[k]] <- min(Max_Mean_UC_C[[k]])
      Max_Mean_Net_M[[k]] <- min(Max_Mean_UC_M[[k]])
    }

    ### We calculate the
    EVXI_C_C <- mean(do.call(rbind, Max_Mean_Net_C))
    EVXI_C_M <- mean(do.call(rbind, Max_Mean_Net_M))

    EV_UC_C <- min(colMeans(do.call(rbind, Max_Mean_UC_C)))
    EV_UC_M <- min(colMeans(do.call(rbind, Max_Mean_UC_M)))


    PARTIAL_EVPI[[d]] <- cbind(
      cases = EVXI_C_C - EV_UC_C,
      cases_per = (EVXI_C_C- EV_UC_C)/EVXI_C_C,
      mort = EVXI_C_M - EV_UC_M,
      mort_per = (EVXI_C_M- EV_UC_M)/EVXI_C_M

    )
  }
  EVXI <- cbind.data.frame(
    EVXI = do.call(rbind, PARTIAL_EVPI),
    VAR, Vac_Rate = rep(c(0.287, 0.69, 1.38), 3),
    R0 = rep(c(1.15, 1.5, 2.5), each = 3)
  )
  return(EVXI)
}

#####################################################################
#####################################################################

EVXI_SUS_US <- EVXI(TSS_US_ALL_SPLIT, "sus1", "not")
EVXI_TRAN_US <- EVXI(TSS_US_ALL_SPLIT, "trans1", "not")
EVXI_SEV_US <- EVXI(TSS_US_ALL_SPLIT, "sev", "not")
EVXI_WN_US <- EVXI(TSS_US_ALL_SPLIT, "WN", "vac")
EVXI_WV1_US <- EVXI(TSS_US_ALL_SPLIT, "WV1", "vac")
EVXI_WV2_US <- EVXI(TSS_US_ALL_SPLIT, "WV2", "vac")

EVXI_US_FULL <- rbind(
  EVXI_SUS_US,
  EVXI_TRAN_US,
  EVXI_SEV_US,
  EVXI_WN_US,
  EVXI_WV1_US,
  EVXI_WV2_US
)
EVXI_US_FULL$id <- "US"
EVXI_US_C <- ggplot(EVXI_US_FULL, aes(x = VAR, y = 1)) +
  geom_tile(aes(fill = abs(EVXI.cases)), color = "black") +
  facet_grid(Vac_Rate ~ R0) +
  coord_equal() +
  scale_fill_viridis(option = "rocket")

EVXI_US_M <- ggplot(EVXI_US_FULL, aes(x = VAR, y = 1)) +
  geom_tile(aes(fill = abs(EVXI.mort)), color = "black") +
  facet_grid(Vac_Rate ~ R0) +
  coord_equal() +
  scale_fill_viridis(option = "mako")

EVXI_US_C / EVXI_US_M
##########################################
###########################################

EVXI_SUS_SAF <- EVXI(TSS_SAF_ALL_SPLIT, "sus1", "not")
EVXI_TRAN_SAF <- EVXI(TSS_SAF_ALL_SPLIT, "trans1", "not")
EVXI_SEV_SAF <- EVXI(TSS_SAF_ALL_SPLIT, "sev", "not")
EVXI_WN_SAF <- EVXI(TSS_SAF_ALL_SPLIT, "WN", "vac")
EVXI_WV1_SAF <- EVXI(TSS_SAF_ALL_SPLIT, "WV1", "vac")
EVXI_WV2_SAF <- EVXI(TSS_SAF_ALL_SPLIT, "WV2", "vac")

EVXI_SAF_FULL <- rbind(
  EVXI_SUS_SAF,
  EVXI_TRAN_SAF,
  EVXI_SEV_SAF,
  EVXI_WN_SAF,
  EVXI_WV1_SAF,
  EVXI_WV2_SAF
)
EVXI_SAF_FULL$id <- "SAF"

EVXI_SAF_C <- ggplot(EVXI_SAF_FULL, aes(x = VAR, y = 1)) +
  geom_tile(aes(fill = abs(EVXI.cases)), color = "black") +
  facet_grid(Vac_Rate ~ R0) +
  coord_equal() +
  scale_fill_viridis(option = "rocket")

EVXI_SAF_M <- ggplot(EVXI_SAF_FULL, aes(x = VAR, y = 1)) +
  geom_tile(aes(fill = abs(EVXI.mort)), color = "black") +
  facet_grid(Vac_Rate ~ R0) +
  coord_equal() +
  scale_fill_viridis(option = "mako")

(EVXI_SAF + EVXI_US) / (EVXI_SAF_M + EVXI_US_M)

##########################################
###########################################

EVXI_SUS_JPN <- EVXI(TSS_JPN_ALL_SPLIT, "sus1", "not")
EVXI_TRAN_JPN <- EVXI(TSS_JPN_ALL_SPLIT, "trans1", "not")
EVXI_SEV_JPN <- EVXI(TSS_JPN_ALL_SPLIT, "sev", "not")
EVXI_WN_JPN <- EVXI(TSS_JPN_ALL_SPLIT, "WN", "vac")
EVXI_WV1_JPN <- EVXI(TSS_JPN_ALL_SPLIT, "WV1", "vac")
EVXI_WV2_JPN <- EVXI(TSS_JPN_ALL_SPLIT, "WV2", "vac")

EVXI_JPN_FULL <- rbind(
  EVXI_SUS_JPN,
  EVXI_TRAN_JPN,
  EVXI_SEV_JPN,
  EVXI_WN_JPN,
  EVXI_WV1_JPN,
  EVXI_WV2_JPN
)
EVXI_JPN_FULL$id <- "JPN"

EVXI_JPN_C <- ggplot(EVXI_JPN_FULL, aes(x = VAR, y = 1)) +
  geom_tile(aes(fill = abs(EVXI.cases)), color = "black") +
  facet_grid(Vac_Rate ~ R0) +
  coord_equal() +
  scale_fill_viridis(option = "rocket")

EVXI_JPN_M <- ggplot(EVXI_JPN_FULL, aes(x = VAR, y = 1)) +
  geom_tile(aes(fill = abs(EVXI.mort)), color = "black") +
  facet_grid(Vac_Rate ~ R0) +
  coord_equal() +
  scale_fill_viridis(option = "mako")


###

EVXI_ALL <- rbind(
  EVXI_US_FULL, EVXI_SAF_FULL,
  EVXI_JPN_FULL
)

EVXI_ALL_GG_C <- ggplot(EVXI_ALL, aes(
  y = VAR, fill = abs(EVXI.cases),
  x = id
)) +
  geom_tile(color = "black") +
  coord_equal() +
  facet_grid(Vac_Rate ~ R0) +
  scale_fill_viridis(option = "rocket")

EVXI_ALL_GG_M <- ggplot(EVXI_ALL, aes(
  y = VAR,
  fill = abs(EVXI.mort),
  x = id
)) +
  geom_tile(color = "black") +
  coord_equal() +
  facet_grid(Vac_Rate ~ R0) +
  scale_fill_viridis(option = "mako")

EVXI_ALL_GG_C + EVXI_ALL_GG_M
