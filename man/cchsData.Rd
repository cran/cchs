\name{cchsData}
\docType{data}
\alias{cchsData}
\title{Data from a case--cohort study with stratified subcohort-selection}

\description{
	A case--cohort dataset where the subcohort was selected by stratified simple random sampling. This is an artificial dataset that was made from \code{\link[survival]{nwtco}}, a real dataset from the National Wilms Tumor Study (NWTS). It is designed for demonstrating the use of \code{\link{cchs}}. 
}

\format{
\tabular{ll}{
	\code{id} \tab An ID number. \cr
    \code{localHistol} \tab Result of the histology from the local institution. \cr
    \code{centralLabHistol} \tab Result of the histology from the central laboratory. \cr
    \code{stage} \tab Stage of the cancer (I, II, III, or IV). \cr
    \code{study} \tab The study (NWTS-3 or NWTS-4). For details see \href{https://web.archive.org/web/20211020034700/http://nwtsg.org/about/clinical_trials.html}{this NWTS webpage (archived copy)}. \cr
    \code{isCase} \tab Indicator for whether this participant had a relapse or not. \cr     
    \code{time} \tab Number of days from diagnosis of Wilms tumor to relapse or censoring. \cr
    \code{ageAtDiagnosis} \tab Age in years at diagnosis of Wilms tumor. \cr
    \code{inSubcohort} \tab Indicator for whether this participant is in the subcohort or not. \cr     
    \code{sampFrac} \tab The sampling fraction for the stratum that contains this participant. 
  }
}
\details{
	The \code{\link[survival]{nwtco}} data is from two clinical trials but can be regarded as cohort data. \code{cchsData} can be created from it by running the code in the Source section below, which is partly based on the Examples section of the \code{\link[survival]{cch}} documentation. 
	
	Two strata are used for the subcohort-selection, corresponding to the two values of \code{localHistol}. The sampling fraction is 5\% for the stratum defined by \code{localHistol="favorable"} and 20\% for the stratum defined by \code{localHistol="unfavorable"}. After the subcohort is selected, the sampling fractions are recalculated using the exact integer numbers of participants in the subcohort and the full cohort, and then stored in the data-frame.
	
	As an alternative to the sampling fractions, the stratum sizes in the full cohort could be used. A suitable value for the \code{cohortStratumSizes} argument to \code{\link{cchs}} would be \code{c(favorable=3622,} \code{unfavorable=406)}. This can be worked out by entering \code{table(nwtco$instit, useNA="always")} and noting that for \code{nwtco$instit} and \code{nwtco$histol}, a value of \code{1} means ``favorable histology result'' and \code{2} means ``unfavorable''---this is not stated in the \code{\link[survival]{nwtco}} documentation but can be deduced from the line in the \code{\link[survival]{cch}} examples that contains \code{labels=c("FH","UH")}, or by comparing the output of the \code{table} command with the numbers in Table 1 of Breslow & Chatterjee (1999).
	
	For information about the two clinical trials, NWTS-3 and NWTS-4, see D'Angio et al. (1989) and Green et al. (1998) respectively, or the \href{https://web.archive.org/web/20211021054158/http://www.nwtsg.org/}{National Wilms Tumor Study website (archived copy)}. 
}

% NB the following code is identified and extracted using the "Start of code ..." and "End of code ..." lines, and then run to create cchsData. 
\section{Source}{\preformatted{
% Start of code to make dataset.
# Starting with nwtco, rename variables, convert some to factors, drop  
# in.subcohort (which is used elsewhere for a different simulated dataset), etc. 
library(survival, quietly=TRUE)
cchsData <- data.frame(
   id = nwtco$seqno, 
   localHistol = factor(nwtco$instit, labels=c("favorable", "unfavorable")), 
   centralLabHistol = factor(nwtco$histol, labels=c("favorable", "unfavorable")), 
   stage = factor(nwtco$stage, labels=c("I", "II", "III", "IV")), 
   study = factor(nwtco$study, labels=c("NWTS-3", "NWTS-4")),
   isCase = as.logical(nwtco$rel), 
   time = nwtco$edrel,
   ageAtDiagnosis = nwtco$age / 12  # nwtco$age is in months
)

# Define the intended sampling fractions for the two strata. 
samplingFractions <- c(favorable=0.05, unfavorable=0.2)

# Select participants/rows to be in the subcohort by stratified simple random 
# sampling. 
cchsData$inSubcohort <- rep(FALSE, nrow(cchsData))
set.seed(1)
for (stratumName in levels(cchsData$localHistol)) {
   inThisStratum <- cchsData$localHistol == stratumName
   stratumSubcohortSize <- 
         round(samplingFractions[stratumName] * sum(inThisStratum))
   rowsToSetTrue <- sample(which(inThisStratum), size=stratumSubcohortSize)
   cchsData$inSubcohort[rowsToSetTrue] <- TRUE
}

# Change the sampling fractions to their exact values. 
stratumSubcohortSizes <- table(cchsData$localHistol[cchsData$inSubcohort])
stratumCohortSizes <- table(cchsData$localHistol)
samplingFractions <- stratumSubcohortSizes / stratumCohortSizes
samplingFractions <- c(samplingFractions)  # make it a vector, not a table

# Keep only the cases and the subcohort. 
cchsData <- cchsData[cchsData$isCase | cchsData$inSubcohort,]

# Put the sampling fraction in each row of the data-frame. 
cchsData$sampFrac <- 
      samplingFractions[match(cchsData$localHistol, names(samplingFractions))]
% End of code to make dataset.
}}

\references{

	Note: doi links are shown where these pass CRAN checks and appear correctly in the PDF reference manual. In other cases, URLs are shown. 

	Breslow, N.E., Chatterjee, N. (1999). Design and analysis of two-phase studies with binary outcome applied to Wilms tumour prognosis. \emph{Journal of the Royal Statistical Society: Series C (Applied Statistics)} \bold{48} (4), 457--468. https://doi.org/10.1111/1467-9876.00165
	
	D'Angio, G.J., Breslow, N., Beckwith, J.B., Evans, A., Baum, E., Delorimier, A., Fernbach, D., Hrabovsky, E., Jones, B., Kelalis, P., Othersen, H.B., Tefft, M., Thomas, P.R.M. (1989). Treatment of Wilms' tumor: Results of the third National Wilms' Tumor Study. \emph{Cancer} \bold{64} (2), 349--360. https://doi.org/bc95fv
	
	Green, D.M., Breslow, N.E., Beckwith, J.B., Finklestein, J.Z., Grundy, P.E., Thomas, P.R., Kim, T., Shochat, S.J., Haase, G.M., Ritchey, M.L., Kelalis, P.P., D'Angio, G.J. (1998). Comparison between single-dose and divided-dose administration of dactinomycin and doxorubicin for patients with Wilms' tumor: a report from the National Wilms' Tumor Study Group. \emph{Journal of Clinical Oncology} \bold{16} (1), 237--245. \doi{10.1200/JCO.1998.16.1.237} 
}

\keyword{datasets}

