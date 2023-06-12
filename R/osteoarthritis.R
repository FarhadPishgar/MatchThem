#' @title Data of 2,585 Participants in the Osteoarthritis Initiative (OAI) Project
#'
#' @rdname osteoarthritis
#'
#' @description \code{osteoarthritis} includes demographic data of 2,585 units (individuals) with or at risk of knee osteoarthritis. The recorded data has missing values in body mass index (\code{BMI}, a quantitative variable), race (\code{RAC}, a categorical qualitative variable), smoking status (\code{SMK}, a binary qualitative variable), and knee osteoarthritis status at follow-up (\code{KOA}, a binary qualitative variable).
#'
#' @format This dataset contains 2,585 rows and 7 columns. Each row presents data of an unit (individual) and each column presents data of a characteristic of that unit. The columns are:
#' \describe{
#'   \item{AGE}{Age of each unit (individual);}
#'   \item{SEX}{Gender of each unit (individual), coded as \code{0} (female) and \code{1} (male);}
#'   \item{BMI}{Estimated body mass index of each unit (individual);}
#'   \item{RAC}{Race of each unit (individual), coded as \code{0} (other), \code{1} (Caucasian), \code{2} (African American), and \code{3} (Asian);}
#'   \item{SMK}{The smoking status of each unit (individual), coded as \code{0} (non-smoker) and \code{1} (smoker);}
#'   \item{OSP}{Osteoporosis status of each unit (individual) at baseline, coded as \code{0} (negative) and \code{1} (positive); and}
#'   \item{KOA}{Knee osteoarthritis status of each unit (individual) in the follow-up, coded as \code{0} (at risk) and \code{1} (diagnosed).}
#' }
#' @source The information presented in the \code{osteoarthritis} dataset is based on the publicly available data of the Osteoarthritis Initiative (OAI) project (see \url{https://nda.nih.gov/oai/} for details), with changes.

"osteoarthritis"
