\name{EEMDELM}
\alias{EEMDELM}
\title{
Ensemble Empirical Mode Decomposition Based ELM Model
}
\description{
The EEMDelm function computes forecasted value with different forecasting evaluation criteria for Ensemble Empirical Mode Decomposition based Extreme Learning Machine model.
}
\usage{
EEMDELM(data, stepahead=10,
num.IMFs=emd_num_imfs(length(data)), s.num=4L,
num.sift=50L, ensem.size=250L, noise.st=0.2)
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
To overcome the problem of EMD (i.e. mode mixing), Ensemble Empirical Mode Decomposition (EEMD) method was developed by Wu and Huang (2009), which significantly reduces the chance of mode mixing and represents a substantial improvement over the original EMD.
}
\value{
  \item{TotalIMF }{Total number of IMFs.}
  \item{AllIMF }{List of all IMFs with residual for input series.}
  \item{data_test }{Testing set is used to measure the out of sample performance.}
  \item{AllIMF_forecast }{Forecasted value of all individual IMF.}
  \item{FinalEEMDELM_forecast }{Final forecasted value of the EEMDELM model. It is obtained by combining the forecasted value of all individual IMF.}
  \item{MAE_EEMDELM }{Mean Absolute Error (MAE) for EEMDELM model.}
  \item{MAPE_EEMDELM }{Mean Absolute Percentage Error (MAPE) for EEMDELM model.}
  \item{rmse_EEMDELM }{Root Mean Square Error (RMSE) for EEMDELM model.}
}
\references{
Choudhary, K., Jha, G.K., Kumar, R.R. and Mishra, D.C. (2019) Agricultural commodity price analysis using ensemble empirical mode decomposition: A case study of daily potato price series. Indian journal of agricultural sciences, 89(5), 882--886.

Huang, N.E., Shen, Z., Long, S.R., Wu, M.C., Shih, H.H., Zheng, Q. and Liu, H.H. (1998) The empirical mode decomposition and the Hilbert spectrum for nonlinear and non stationary time series analysis. In Proceedings of the Royal Society of London A: mathematical, physical and engineering sciences. 454, 903--995.

Huang, G.B., Zhu, Q.Y. and Siew, C.K. (2006) Extreme learning machine: theory and applications. Neurocomputing, 70, 489--501.

Wu, Z. and Huang, N.E. (2009) Ensemble empirical mode decomposition: a noise assisted data analysis method. Advances in adaptive data analysis, 1(1), 1--41.
}
\seealso{
EMDelm, CEEMDANelm
}
\examples{
\donttest{
data("Data_Soybean")
EEMDELM(Data_Soybean)
}
}
\keyword{eemd}
