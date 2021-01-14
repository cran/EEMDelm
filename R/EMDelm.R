#' @importFrom Rlibeemd emd_num_imfs emd
#' @importFrom nnfor elm
#' @importFrom forecast forecast
#' @importFrom utils head tail
#' @importFrom graphics plot
#' @importFrom stats as.ts ts
#' @export
#'
EMDelm <- function(xt, stepahead=10,
                   s.num=4L, num.sift=50L){
  data<-ts(xt)
  n.IMF <- emd_num_imfs(length(data))
  AllIMF <- emd(data, num_imfs = n.IMF, S_number = s.num, num_siftings = num.sift)
  data_trn <- ts(head(data, round(length(data) - stepahead)))
  data_test <- ts(tail(data, stepahead))
  IMF_trn <- AllIMF[-c(((length(data)-stepahead)+1):length(data)),]
  Fcast_AllIMF <- NULL
  for (IMF in 1:ncol(IMF_trn)) {
    IndIMF <- NULL
    IndIMF <- IMF_trn[ ,IMF]
    EMDELMFit <- nnfor::elm(as.ts(IndIMF), keep = NULL, difforder = NULL, outplot = c(
      FALSE), sel.lag = c(FALSE), direct = c(FALSE),
      allow.det.season = c(FALSE))
    EMDELM_fcast=forecast::forecast(EMDELMFit, h=stepahead)
    EMDELM_fcast_Mean=EMDELM_fcast$mean
    Fcast_AllIMF <- cbind(Fcast_AllIMF, as.matrix(EMDELM_fcast_Mean))
  }
  FinalEMDELM_fcast <- ts(rowSums(Fcast_AllIMF, na.rm = T))
  MAE_EMDELM=mean(abs(data_test - FinalEMDELM_fcast))
  MAPE_EMDELM=mean(abs(data_test - FinalEMDELM_fcast)/data_test)
  rmse_EMDELM=sqrt(mean((data_test - FinalEMDELM_fcast)^2))
  Plot_IMFs <- AllIMF
  AllIMF_plots <- plot(Plot_IMFs)
  return(list(TotalIMF = n.IMF, AllIMF=AllIMF, data_test=data_test, AllIMF_forecast=Fcast_AllIMF,
              FinalEMDELM_forecast=FinalEMDELM_fcast, MAE_EMDELM=MAE_EMDELM,
              MAPE_EMDELM=MAPE_EMDELM, rmse_EMDELM=rmse_EMDELM,
              AllIMF_plots=AllIMF_plots))
}
