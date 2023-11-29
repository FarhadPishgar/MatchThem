#' @title Data of 2,585 Participants in the Osteoarthritis Initiative (OAI) Project
#'
#' @rdname osteoarthritis
#'
#' @description `osteoarthritis` includes demographic data of 2,585 units (individuals) with or at risk of knee osteoarthritis. The recorded data has missing values in body mass index (`BMI`, a quantitative variable), race (`RAC`, a categorical qualitative variable), smoking status (`SMK`, a binary qualitative variable), and knee osteoarthritis status at follow-up (`KOA`, a binary qualitative variable).
#'
#' @format This dataset contains 2,585 rows and 7 columns. Each row presents data of an unit (individual) and each column presents data of a characteristic of that unit. The columns are:
#' \describe{
#'   \item{AGE}{Age of each unit (individual);}
#'   \item{SEX}{Gender of each unit (individual), coded as `0` (female) and `1` (male);}
#'   \item{BMI}{Estimated body mass index of each unit (individual);}
#'   \item{RAC}{Race of each unit (individual), coded as `0` (other), `1` (Caucasian), `2` (African American), and `3` (Asian);}
#'   \item{SMK}{The smoking status of each unit (individual), coded as `0` (non-smoker) and `1` (smoker);}
#'   \item{OSP}{Osteoporosis status of each unit (individual) at baseline, coded as `0` (negative) and `1` (positive); and}
#'   \item{KOA}{Knee osteoarthritis status of each unit (individual) in the follow-up, coded as `0` (at risk) and `1` (diagnosed).}
#' }
#' @source The information presented in the `osteoarthritis` dataset is based on the publicly available data of the Osteoarthritis Initiative (OAI) project (see <https://nda.nih.gov/oai/> for details), with changes.

"osteoarthritis"
