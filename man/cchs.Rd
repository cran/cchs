\encoding{UTF-8}
\name{cchs}
\alias{cchs}
\title{Cox model for case--cohort data with stratified subcohort-selection}
\description{
	\code{cchs} fits a Cox proportional-hazards regression model to case-cohort data where the subcohort was selected by stratified simple random sampling. It uses Estimator III of Borgan et al (2000). }

\usage{
	cchs(formula, data=parent.frame(), inSubcohort, stratum, 
		samplingFractions, cohortStratumSizes, precision=NULL, 
		returnAdjustedTimes=FALSE, swap=TRUE, dropNeverAtRiskRows=TRUE, 
		dropSubcohEventsDfbeta=FALSE, adjustSampFracIfAnyNAs=FALSE, 
		keepAllCoxphElements=FALSE, confidenceLevel=0.95, verbose=FALSE, 
		annotateErrors=TRUE, coxphControl, ...)
}

\arguments{
	\item{formula}{ An object of class \code{\link{formula}} that specifies the terms in the model. The left-hand side must be a \code{\link[survival]{Surv}} object. The special terms \code{cluster} and \code{strata} are not allowed. }
	
	\item{data}{ A data-frame or environment that contains the variables used in the formula. The variables named in \code{inSubcohort}, \code{stratum}, \code{samplingFractions}, and \code{cohortStratumSizes} will be looked for first in \code{data}, if that is a data-frame, and then in the environment that \code{cchs} was called from. }
	
	\item{inSubcohort}{ A vector of logical variables that shows whether each observation/row is in the subcohort (\code{TRUE}) or not (\code{FALSE}). }
	
	\item{stratum}{ A vector that defines the strata within which the subcohort was selected. Each element of \code{stratum} corresponds to one observation/row in the data. The elements can be character strings, integers, or any other type of variable that can be converted to a \code{\link{factor}}. }
	
	\item{samplingFractions, cohortStratumSizes}{ \code{samplingFractions} is a vector of the sampling fractions in the different strata, and \code{cohortStratumSizes} is a vector of the sizes of the strata in the full cohort. Exactly one of these must be given. There are two possible forms for the vector: if it has names, then these must all be distinct and include the names of the strata (and if one value of \code{stratum} is \code{"France"}, then \code{samplingFraction["France"]} should be the sampling fraction for that stratum); if it does not have names, then it must have one element for each observation/row in the data. } 

	\item{precision}{ For example, if the times were recorded to the nearest day but are stored as numbers of years, then \code{precision} should be \code{1/365.25}. If there are no tied event-times, then it makes no difference what \code{precision} is. If there are tied event-times and \code{precision} is a number, then the tied event-times will be slightly changed before the estimator is calculated. If there are tied event-times and \code{precision} is \code{NULL} (meaning unspecified), then the estimator cannot be calculated and an error will be thrown. }
	
	\item{returnAdjustedTimes}{ If this is \code{TRUE}, the object returned by \code{cchs} will contain the exit-times after they have been adjusted to deal with any tied event-times. If a row is dropped because of missing data (\code{NA}s) then its exit-time is not adjusted. }

	\item{swap}{ If this is \code{FALSE} then the swapping will be omitted (in the formula for Estimator III in Borgan et al 2000, the randomly selected observation/row will not be removed). This is only intended to be used for testing or development. }
	
	\item{dropNeverAtRiskRows}{ If this is \code{TRUE}, observations/rows whose at-risk periods do not include any of the event-times will be dropped just before \code{cchs} internally calls \code{\link[survival]{coxph}}. These observations/rows make no difference to the regression coefficients produced by \code{coxph}, but they do affect the dfbeta residuals (see Langholz & Jiao 2007) and therefore the variance-estimates, because \code{coxph} calculates the dfbeta residuals using an approximation. }  

	\item{dropSubcohEventsDfbeta}{ If this is \code{FALSE}, which is the default, the dfbeta residuals and therefore the variance-estimates will be calculated exactly as described by Borgan et al (2000). If it is \code{TRUE}, they will be calculated as described by Langholz & Jiao (2007) (see ``There is a slight approximation ...'' in section 2.4). }

	\item{adjustSampFracIfAnyNAs}{ If this is \code{TRUE}, and if any observations are dropped because of missing data (\code{NA}s), then the sampling fractions will be recalculated using the numbers of observations after those observations are dropped. }

	\item{keepAllCoxphElements}{ If this is \code{TRUE}, then the object returned by \code{cchs} will contain elements such as \code{loglik} and \code{linear.predictors} from the object that was produced by \code{cchs}'s internal call to \code{\link[survival]{coxph}}. These are not likely to be relevant or correct, since \code{cchs} manipulates and changes the dataset in many ways before passing it to \code{coxph}. (For a list of the elements produced by \code{coxph}, see \code{\link[survival]{coxph.object}}.) }

	\item{confidenceLevel}{ The level for the hazard-ratio confidence intervals (a number in the interval [0,1]). }

	\item{verbose}{ If this is \code{TRUE}, detailed information about the internal manipulations and calculations will be displayed. }
	
	\item{annotateErrors}{ If this is \code{TRUE}, and if certain functions that are called internally by \code{cchs} produce errors or warnings, then extra messages will be added to make those easier to understand. The disadvantage of this is that the call stack produced by \code{\link{traceback}} is more complicated. }

	\item{coxphControl, \dots}{ These are optional arguments to control the working of \code{coxph} when it is called internally by \code{cchs}. If \code{coxphControl} is supplied then it must be a list produced by \code{\link[survival]{coxph.control}}, and if ``\code{\dots}'' arguments are supplied then it must be possible to pass them to \code{coxph.control}. }
}

\details{
	In a case--cohort study, the dataset consists only of the cases (the participants who have an event) and the participants who are in the subcohort, which is a randomly selected subset of the cohort. In a stratified case--cohort study, the subcohort is selected by stratified simple random sampling. This means that the cohort is divided into strata, and from each stratum a proportion of the participants equal to that stratum's sampling fraction is selected to be in the subcohort (and within each stratum, each participant is selected with equal probability). For more on stratified case--cohort studies see any of the references listed below. 
	
	\code{cchs} fits a Cox proportional-hazards regression model to data from a stratified case--cohort study, using the time-fixed version of Estimator III from Borgan et al (2000). Estimators I and II from Borgan et al (2000) are available by using \code{\link[survival]{cch}} with the options \code{method="I.Borgan"} and \code{method="II.Borgan"}, but only Estimator III is score-unbiased, which is the main desirable criterion. The data must be in the usual form where each row corresponds to one observation (that is, one participant). \code{cchs} works by manipulating the data in various ways, then passing it to \code{\link[survival]{coxph}} (which is suitable for fitting a Cox model to data from a cohort study), and finally making corrections to the variance-estimates. It is planned that a vignette will be produced and this will contain more detail. 
	
	For normal use, the logical (boolean) arguments should have their default values. \code{cchs} performs a complete-case analysis, meaning that rows will be dropped if they contain \code{NA}s in any of the variables that appear in the model, including inside the \code{Surv()}, or in \code{inSubcohort} or \code{stratum}. \code{NA}s are not allowed in \code{samplingFractions} or \code{cohortStratumSizes}, unless that vector has names and any of those names are not equal to values of \code{stratum}, in which case the corresponding elements can be \code{NA}. 

	\code{cchs} does not normally give replicable results, because the swapping and the small changes to tied event-times are random (see \code{swap} and \code{precision} in the Arguments section). To get exactly the same results every time, use \code{\link{set.seed}} with a fixed seed just before calling \code{cchs}. 

	For more information about \code{cchs} see the article in \emph{R Journal}, Jones (2018). 

% PREVIOUS VERSION OF ONE PARAGRAPH (NA is prohibited): For normal use, the logical (boolean) arguments should have their default values. \code{NA}s are not allowed in the data, \code{inSubcohort}, \code{stratum}, \code{samplingFractions}, or \code{cohortStratumSizes}. (However, if \code{samplingFractions} or \code{cohortStratumSizes} have names, and any of those names are not equal to values of \code{stratum}, then the corresponding elements can be \code{NA}.)  
% To get bullet-list, use \itemize{ \item abc \item def }, but this does not look good. 
}

\value{
	An S3 object of class \code{cchs}. This is a list that contains the following elements:
	
	\item{coefficients}{ The vector of coefficients. }

	\item{var}{ The variance matrix of the coefficients. }

	\item{loglik}{ A vector of two elements: the first is the log-likelihood with the initial values of the coefficients that were used in the iteration to find the maximum likelihood, and the second is the maximized log-likelihood---that is, the log-likelihood with the final values of the coefficients. (Strictly speaking these should all say ``pseudo-likelihood'' instead of ``likelihood''.) }

	\item{iter}{ The number of iterations used by \code{\link[survival]{coxph}}. }

	\item{n}{ The number of observations (that is, rows), that were used in the call to \code{coxph}. }

	\item{nevent}{ The number of events (also called failures). }

	\item{call}{ The call that was used to create the \code{cchs} object (an object of mode \code{\link{call}}). }
	
	\item{coeffsTable}{ A summary of the main output. This is a matrix that contains the hazard ratios, confidence intervals for them, \emph{p}-values for the Wald tests, log hazard ratios (which are the coefficients in the Cox model), and standard errors of the log hazard ratios. }

	\item{confidenceLevel}{ The level for the confidence intervals in \code{coeffsTable}. (This is a copy of the \code{confidenceLevel} argument.) }

	\item{nEachStatus}{ A vector with three elements: the numbers of subcohort non-cases, subcohort cases, and non-subcohort cases. The sum of these is \code{n}. }
			
	\item{nStrata}{ The number of strata that appear in the data. }

	\item{message}{ A message about observations that have been dropped because of NAs and event-times that have been changed to deal with ties, if either of these happened. }
	
	If \code{keepAllCoxphElements} is \code{TRUE}, then the \code{cchs} object will also contain the other elements listed under \code{\link[survival]{coxph.object}}. If \code{returnAdjustedTimes} is \code{TRUE}, then it will contain an \code{adjustedTimes} element, which is a vector of the adjusted exit-times (with elements in the same order as the observations/rows in the data). 
}

\references{

	Note: doi links are shown where these pass CRAN checks and appear correctly in the PDF reference manual. In other cases, URLs are shown. 

	Borgan, Ø., Langholz, B., Samuelsen S.O., Goldstein, L., Pogoda, J. (2000). Exposure stratified case--cohort designs. \emph{Lifetime Data Analysis} \bold{6} (1), 39--58. \doi{10.1023/A:1009661900674} 
	
	Cologne, J., Preston, D.L., Imai, K., Misumi, M., Yoshida, K., Hayashi, T., Nakachi, K. (2012). Conventional case--cohort design and analysis for studies of interaction. \emph{International Journal of Epidemiology} \bold{41} (4), 1174--1186. \doi{10.1093/ije/dys102}
	
	Jones, E. (2018). cchs: An R package for stratified case--cohort studies. \emph{R Journal} \bold{10} (1), 484--494. https://doi.org/10.32614/RJ-2018-012

	Langholz, B., Jiao, J. (2007). Computational methods for case--cohort studies. \emph{Computational Statistics and Data Analysis} \bold{51} (8), 3737--3748. \doi{10.1016/j.csda.2006.12.028}
}

\seealso{
	\code{\link[survival]{cch}}, which can calculate Estimators I and II from Borgan et al (2000), \code{\link[survival]{coxph}}, which \code{cchs} uses internally, and \code{\link[survival]{coxph.control}}, a container for certain parameters that are passed to \code{coxph}. These are all in the \href{https://cran.r-project.org/package=survival}{\pkg{survival}} package. 
	
	\code{\link{cchsData}}, an example dataset that \code{cchs} can be used on. 
}

\examples{
# Analyze the relation between survival and three covariates in cchsData. 
# The times are stored as numbers of days, so precision has to be 1. The 
# selection of the subcohort was stratified according to two strata, defined 
# by cchsData$localHistol, and the sampling fractions are stored in 
# cchsData$sampFrac. 

cchs(Surv(time, isCase) ~ stage + centralLabHistol + ageAtDiagnosis, 
      data=cchsData, inSubcohort=inSubcohort, stratum=localHistol, 
      samplingFractions=sampFrac, precision=1) 

# Do the same analysis using cohortStratumSizes instead of samplingFractions.
# For the value of cohortStratumSizes see the Details section of ?cchsData. 
# These two calls to cchs will give slightly different results unless set.seed  
# is used with the same seed just before both of them.

cchs(Surv(time, isCase) ~ stage + centralLabHistol + ageAtDiagnosis, 
      data=cchsData, inSubcohort=inSubcohort, stratum=localHistol, 
      cohortStratumSizes=c(favorable=3622, unfavorable=406), precision=1) 
% Omitted: Fit the same model using coxph and the whole cohort:
% coxph(Surv(edrel, rel) ~ stage + histol + age, data=nwtco)
}



% NOTES. As of Nov 2024 it seems to be recommended to put \link[survival]{coxph} etc. OLDER NOTE: In \link[survival]{coxph} etc. it is unnecessary to mention the survival package. See https://cran.r-project.org/doc/manuals/r-devel/R-exts.html#Cross_002dreferences. 


