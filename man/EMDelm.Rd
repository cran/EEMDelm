\name{EMDelm}
\alias{EMDelm}
\title{
Empirical Mode Decomposition Based ELM Model
}
\description{
The EMDelm function computes forecasted value with different forecasting evaluation criteria for Empirical Mode Decomposition based Extreme Learning Machine model.
}
\usage{
EMDelm(xt, stepahead = 10, s.num = 4L, num.sift = 50L)
}
\arguments{
  \item{xt}{
Input univariate time series (ts) data.
}
  \item{stepahead}{
The forecast horizon.
}
  \item{s.num}{
Integer. Use the S number stopping criterion for the EMD procedure with the given values of S. That is, iterate until the number of extrema and zero crossings in the signal differ at most by one, and stay the same for S consecutive iterations.
}
  \item{num.sift}{
Number of siftings to find out IMFs.
}
}
\details{
This function decomposes the original time series into several independent intrinsic mode functions (IMFs) and one residual component (Huang et al., 1998). Then extreme learning machine, a class of feedforward neural network is used to forecast these IMFs and residual component individually (Huang et al., 2006). Finally, the prediction results of all IMFs including residual are aggregated to formulate an ensemble output for the original time series.
}
\value{
  \item{TotalIMF }{Total number of IMFs.}
  \item{AllIMF }{List of all IMFs with residual for input series.}
  \item{data_test }{Testing set is used to measure the out of sample performance.}
  \item{AllIMF_forecast }{Forecasted value of all individual IMF.}
  \item{FinalEMDELM_forecast }{Final forecasted value of the EMDELM model.It is obtained by combining the forecasted value of all individual IMF.}
  \item{MAE_EMDELM }{Mean Absolute Error (MAE) for EMDELM model.}
  \item{MAPE_EMDELM }{Mean Absolute Percentage Error (MAPE) for EMDELM  model.}
  \item{rmse_EMDELM }{Root Mean Square Error (RMSE) for EMDELM model.}
}
\references{
Choudhary, K., Jha, G.K., Kumar, R.R. and Mishra, D.C. (2019) Agricultural commodity price analysis using ensemble empirical mode decomposition: A case study of daily potato price series. Indian journal of agricultural sciences, 89(5), 882--886.

Dong, J., Dai, W., Tang, L. and Yu, L. (2019) Why do EMD based methods improve prediction. A multiscale complexity perspective. Journal of Forecasting, 38(7), 714--731.

Huang, N.E., Shen, Z., Long, S.R., Wu, M.C., Shih, H.H., Zheng, Q. and Liu, H.H. (1998). The empirical mode decomposition and the Hilbert spectrum for nonlinear and non stationary time series analysis. In Proceedings of the Royal Society of London A: mathematical, physical and engineering sciences. 454, 903--995.

Huang, G.B., Zhu, Q.Y. and Siew, C.K. (2006). Extreme learning machine: theory and applications. Neurocomputing, 70, 489--501.
}
\seealso{
EEMDELM, CEEMDANelm
}
\examples{
\donttest{
data("Data_Soybean")
EMDelm(Data_Soybean)
}
}
\keyword{emd}
