#' @importFrom Rlibeemd emd_num_imfs ceemdan
#' @importFrom nnfor elm
#' @importFrom forecast forecast
#' @importFrom utils head tail
#' @importFrom graphics plot
#' @importFrom stats as.ts ts
#' @export
#'

CEEMDANelm <- function(data, stepahead=10, num.IMFs=emd_num_imfs(length(data)),
                       s.num=4L, num.sift=50L, ensem.size=250L, noise.st=0.2){
  n.IMF <- num.IMFs
  AllIMF <- ceemdan(ts(data), num_imfs = n.IMF, ensemble_size = ensem.size, noise_strength = noise.st,
                    S_number = s.num, num_siftings = num.sift, rng_seed = 0L, threads = 0L)
  data_trn <- ts(head(data, round(length(data) - stepahead)))
  data_test <- ts(tail(data, stepahead))
  IMF_trn <- AllIMF[-c(((length(data)-stepahead)+1):length(data)),]
  Fcast_AllIMF <- NULL
  for (IMF in 1:ncol(IMF_trn)) {
    IndIMF <- NULL
    IndIMF <- IMF_trn[ ,IMF]
    CEEMDANELMFit <- nnfor::elm(as.ts(IndIMF), keep = NULL, difforder = NULL, outplot = c(
      FALSE), sel.lag = c(FALSE), direct = c(FALSE),
      allow.det.season = c(FALSE))
    CEEMDANELM_fcast=forecast::forecast(CEEMDANELMFit, h=stepahead)
    CEEMDANELM_fcast_Mean=CEEMDANELM_fcast$mean
    Fcast_AllIMF <- cbind(Fcast_AllIMF, as.matrix(CEEMDANELM_fcast_Mean))
  }
  FinalCEEMDANELM_fcast <- ts(rowSums(Fcast_AllIMF, na.rm = T))
  MAE_CEEMDANELM=mean(abs(data_test - FinalCEEMDANELM_fcast))
  MAPE_CEEMDANELM=mean(abs(data_test - FinalCEEMDANELM_fcast)/data_test)
  rmse_CEEMDANELM=sqrt(mean((data_test - FinalCEEMDANELM_fcast)^2))
  Plot_IMFs <- AllIMF
  AllIMF_plots <- plot(Plot_IMFs)
  return(list(TotalIMF = n.IMF, data_test=data_test, AllIMF_forecast=Fcast_AllIMF,
              FinalCEEMDANELM_forecast=FinalCEEMDANELM_fcast, MAE_CEEMDANELM=MAE_CEEMDANELM,
              MAPE_CEEMDANELM=MAPE_CEEMDANELM, rmse_CEEMDANELM=rmse_CEEMDANELM,
              AllIMF_plots=AllIMF_plots))
}
