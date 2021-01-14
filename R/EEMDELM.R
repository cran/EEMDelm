#' @importFrom Rlibeemd emd_num_imfs eemd
#' @importFrom nnfor elm
#' @importFrom forecast forecast
#' @importFrom utils head tail
#' @importFrom graphics plot
#' @importFrom stats as.ts ts
#' @export
#'

EEMDELM <- function(data, stepahead=10, num.IMFs=emd_num_imfs(length(data)),
                    s.num=4L, num.sift=50L, ensem.size=250L, noise.st=0.2){
  n.IMF <- num.IMFs
  AllIMF <- eemd(ts(data), num_imfs = n.IMF, ensemble_size = ensem.size, noise_strength = noise.st,
                 S_number = s.num, num_siftings = num.sift, rng_seed = 0L, threads = 0L)
  data_trn <- ts(head(data, round(length(data) - stepahead)))
  data_test <- ts(tail(data, stepahead))
  IMF_trn <- AllIMF[-c(((length(data)-stepahead)+1):length(data)),]
  Fcast_AllIMF <- NULL
  for (IMF in 1:ncol(IMF_trn)) {
    IndIMF <- NULL
    IndIMF <- IMF_trn[ ,IMF]
    EEMDELMFit <- nnfor::elm(as.ts(IndIMF), keep = NULL, difforder = NULL, outplot = c(
      FALSE), sel.lag = c(FALSE), direct = c(FALSE),
      allow.det.season = c(FALSE))
    EEMDELM_fcast=forecast::forecast(EEMDELMFit, h=stepahead)
    EEMDELM_fcast_Mean=EEMDELM_fcast$mean
    Fcast_AllIMF <- cbind(Fcast_AllIMF, as.matrix(EEMDELM_fcast_Mean))
  }
  FinalEEMDELM_fcast <- ts(rowSums(Fcast_AllIMF, na.rm = T))
  MAE_EEMDELM=mean(abs(data_test - FinalEEMDELM_fcast))
  MAPE_EEMDELM=mean(abs(data_test - FinalEEMDELM_fcast)/data_test)
  rmse_EEMDELM=sqrt(mean((data_test - FinalEEMDELM_fcast)^2))
  Plot_IMFs <- AllIMF
  AllIMF_plots <- plot(Plot_IMFs)
  return(list(TotalIMF = n.IMF, data_test=data_test, AllIMF_forecast=Fcast_AllIMF,
              FinalEEMDELM_forecast=FinalEEMDELM_fcast, MAE_EEMDELM=MAE_EEMDELM,
              MAPE_EEMDELM=MAPE_EEMDELM, rmse_EEMDELM=rmse_EEMDELM,
              AllIMF_plots=AllIMF_plots))
}
