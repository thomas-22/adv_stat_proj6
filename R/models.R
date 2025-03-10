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

fit_gamm_interact <- function(data, family = gaussian(), method = "GCV.Cp") {
  gam(
    ng_g ~ te(TimeDiff, Distance, k = 20) +
      s(SampleDelay, bs = "cr", k = 20) + s(DefecDay, bs = "cr", k = 20) +
      NumOtherHunts + s(Deer.ID, bs = "re"),
    data = data,
    family = family,
    method = method
  )
}

fit_gam <- function(data, family = gaussian(), method = "GCV.Cp", sp = NULL) {
  gam(
    ng_g ~ s(TimeDiff, bs = "cr", k = 20) + 
      s(Distance, bs = "cr", k = 10) +
      s(SampleDelay, bs = "cr", k = 20) +
      s(DefecDay, bs = "cr", k = 20) +
      NumOtherHunts,
    sp = sp,
    data = data,
    family = family,
    method = method
  )
}

fit_gam_interact <- function(data, family = gaussian(), method = "GCV.Cp", sp = NULL) {
  gam(
    ng_g ~ te(TimeDiff, Distance, k = 20) +
      s(SampleDelay, bs = "cr", k = 20) + 
      s(DefecDay, bs = "cr", k = 20) +
      NumOtherHunts,
    sp = sp,
    data = data,
    family = family,
    method = method
  )
}

fit_gam_tp <- function(data, family = gaussian()) {
  gam(
    ng_g ~ s(TimeDiff, bs = "ps") + s(DistanceX, DistanceY, bs = "tp") + s(SampleDelay, bs = "ps") +
      NumOtherHunts + s(DefecDay, bs = "ps"),
    data = data,
    family = family
  )
}
