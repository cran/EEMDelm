\name{CEEMDANelm}
\alias{CEEMDANelm}
\title{
Complementary Ensemble Empirical Mode Decomposition with Adaptive Noise Based ELM Model
}
\description{
The CEEMDANelm function computes forecasted value with different forecasting evaluation criteria for Complementary Ensemble Empirical Mode Decomposition with Adaptive Noise based Extreme Learning Machine model.
}
\usage{
CEEMDANelm(data, stepahead=10,
num.IMFs=emd_num_imfs(length(data)),
s.num=4L, num.sift=50L, ensem.size=250L, noise.st=0.2)
}
\arguments{
  \item{data}{
Input univariate time series (ts) data.
}
  \item{stepahead}{
The forecast horizon.
}
  \item{num.IMFs}{
Number of Intrinsic Mode Function (IMF) for input series.
}
  \item{s.num}{
Integer. Use the S number stopping criterion for the EMD procedure with the given values of S. That is, iterate until the number of extrema and zero crossings in the signal differ at most by one, and stay the same for S consecutive iterations.
}
  \item{num.sift}{
Number of siftings to find out IMFs.
}
  \item{ensem.size}{
Number of copies of the input signal to use as the ensemble.
}
  \item{noise.st}{
Standard deviation of the Gaussian random numbers used as additional noise. This value is relative to the standard deviation of the input series.
}
}
\details{
Some useless IMFs are generated in EMD and EEMD, which degrades performance of these algorithms. Therefore, reducing the number of these useless IMFs is advantageous for improving the computation efficiency of these techniques, Torres et al.(2011) proposed CEEMDAN. Fewer IMFs may be generated on the premise of successfully separating different components of a series by using this algorithm, which can reduce the computational cost.
}
\value{
  \item{TotalIMF }{Total number of IMFs.}
  \item{AllIMF }{List of all IMFs with residual for input series.}       \item{data_test }{Testing set is used to measure the out of sample performance.}
  \item{AllIMF_forecast }{Forecasted value of all individual IMF}
  \item{FinalCEEMDANELM_forecast }{Final forecasted value of the CEEMDANELM model.It is obtained by combining the forecasted value of all individual IMF.}
  \item{MAE_CEEMDANELM }{Mean Absolute Error (MAE) for CEEMDANELM model.}
  \item{MAPE_CEEMDANELM }{Mean Absolute Percentage Error (MAPE) for CEEMDANELM  model.}
  \item{rmse_CEEMDANELM }{Root Mean Square Error (RMSE) for CEEMDANELM model.}
}
\references{
Huang, G.B., Zhu, Q.Y. and Siew, C.K. (2006). Extreme learning machine: theory and applications. Neurocomputing, 70, 489--501.

Torres, M.E., Colominas, M.A., Schlotthauer, G. and Flandrin, P. (2011) A complete ensemble empirical mode decomposition with adaptive noise. In 2011 IEEE international conference on acoustics, speech and signal processing (ICASSP) (pp. 4144--4147). IEEE.

Wu, Z. and Huang, N.E. (2009) Ensemble empirical mode decomposition: a noise assisted data analysis method. Advances in adaptive data analysis, 1(1), 1--41.
}
\seealso{
EMDelm, EEMDELM
}
\examples{
\donttest{
data("Data_Soybean")
CEEMDANelm(Data_Soybean)
}
}
\keyword{ceendan}
