# ------------------------
# compute deviance explained
# by random effect
# ------------------------

# load modelling data
# ------------------------
mdat <- readRDS("Data/ModellingData.RDS")
names(mdat) <- c("Closest in Time", "Nearest", "Score")
# ------------------------

# initialize functions for fitting GAMM/GAMM
# weights of GAM need to be similiar to GAMM
# ------------------------
# GAM
fit_gam <- function(data, family = gaussian(), method = "GCV.Cp", sp = NULL) {
  gam(
    ng_g ~ s(TimeDiff, bs = "cr", k = 20) + s(Distance, bs = "cr", k = 10) +
      s(SampleDelay, bs = "cr", k = 20) + s(DefecDay, bs = "cr", k = 20) +
      NumOtherHunts,
    sp = sp,
    data = data,
    family = family
  )
}
# GAMM
fit_gamm <- function(data, family = gaussian(), method = "GCV.Cp") {
  gam(
    ng_g ~ s(TimeDiff, bs = "cr", k = 20) + s(Distance, bs = "cr", k = 10) +
      s(SampleDelay, bs = "cr", k = 20) + s(DefecDay, bs = "cr", k = 20) +
      NumOtherHunts + s(Deer.ID, bs = "re"),
    data = data,
    family = family,
    method = method
  )
}
# ------------------------

# fit every both models with every dataset
# calculate the proportion of deviance explained
# by random effect
# ------------------------
deviance_explained <- lapply(res$data, function(data) {
  gamm <- fit_gamm(data, family = Gamma(link = "log"), method = "GCV.Cp")
  gam <- fit_gam(data, family = Gamma(link = "log"), method = "GCV.Cp", sp = gamm$sp[1:5])
  
  gamm_REML <- fit_gamm(data, family = Gamma(link = "log"), method = "REML")
  gam_REML <- fit_gam(data, family = Gamma(link = "log"), method = "REML", sp = gamm_REML$sp[1:5])
  
  dev_GCV <- (deviance(gam) - deviance(gamm)) / deviance(gamm) * 100
  dev_REML <- (deviance(gam_REML) - deviance(gamm_REML)) / deviance(gamm_REML) * 100
  c(GCV = sprintf("%.2f %%", dev_GCV), REML = sprintf("%.2f %%", dev_REML))
})
# ------------------------

# same as above, but use gam.hp package
# ------------------------
deviance_explained2 <- lapply(res$data, function(data) {
  mod_GCV <- gam(ng_g ~ s(TimeDiff, bs = "cr", k = 20) + s(Distance, bs = "cr", k = 10) +
                   s(SampleDelay, bs = "cr", k = 20) + s(DefecDay, bs = "cr", k = 20) +
                   NumOtherHunts + s(Deer.ID, bs = "re"), 
                 data = data, 
                 family = Gamma(link = "log"), method = "GCV.Cp")
  mod_REML <- gam(ng_g ~ s(TimeDiff, bs = "cr", k = 20) + s(Distance, bs = "cr", k = 10) +
                    s(SampleDelay, bs = "cr", k = 20) + s(DefecDay, bs = "cr", k = 20) +
                    NumOtherHunts + s(Deer.ID, bs = "re"), 
                  data = data, 
                  family = Gamma(link = "log"), method = "REML")
  dev_GCV <- gam.hp::gam.hp(mod_GCV)
  dev_REML <- gam.hp::gam.hp(mod_REML)
  
  c(GCV = sprintf("%2.f %%", dev_GCV$hierarchical.partitioning[6,4]),
    REML = sprintf("%2.f %%", dev_REML$hierarchical.partitioning[6,4]))
})
# ------------------------
