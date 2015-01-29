\name{cchsData}
\docType{data}
\alias{cchsData}
\title{Data from a case--cohort study with stratified subcohort-selection}

\description{
	A case--cohort dataset where the subcohort was selected by stratified simple random sampling. This is an artificial dataset that was made from \code{\link{nwtco}}, a real dataset from the National Wilms Tumor Study (NWTS). It is designed for demonstrating the use of \code{\link{cchs}}. 
}

\format{
\tabular{ll}{
	\code{id} \tab An ID number. \cr
    \code{localHistol} \tab Result of the histology from the local institution. \cr
    \code{centralLabHistol} \tab Result of the histology from the central laboratory. \cr
    \code{stage} \tab Stage of the cancer (I, II, III, or IV). \cr
    \code{study} \tab The study (NWTS-3 or NWTS-4). For details see \href{http://www.nwtsg.org/about/clinical_trials.html}{this NWTS webpage}. \cr
    \code{isCase} \tab Indicator for whether this participant had a relapse or not. \cr     
    \code{time} \tab Number of days from diagnosis of Wilms tumor to relapse or censoring. \cr
    \code{ageAtDiagnosis} \tab Age in years at diagnosis of Wilms tumor. \cr
    \code{sampFrac} \tab The sampling fraction for the stratum that contains this participant. 
  }
}

\details{
	The \code{\link{nwtco}} data is from two clinical trials but can be regarded as cohort data. \code{cchsData} can be created from it by running the code in the Source section below, which is partly based on the Examples from the \code{\link{cch}} help page. Two strata are used for the subcohort-selection, corresponding to the two values of \code{localHistol}. The sampling fraction is 5\% for the stratum defined by \code{localHistol="favorable"} and 20\% for the stratum defined by \code{localHistol="unfavorable"}. After the subcohort is selected, the sampling fractions are recalculated using the exact integer numbers of participants in the subcohort and the full cohort. 
}

% NB the following code is identified and extracted using the "Start of code ..." and "End of code ..." lines, and then run to create cchsData. 
\section{Source}{\preformatted{
% Start of code to make cchsData.
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
% End of code to make cchsData.
}
}

\keyword{datasets}

