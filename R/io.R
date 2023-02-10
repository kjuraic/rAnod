

#' extract numeric value from ccurrent row (remove units)
#' @author K. Juraic
#' @param df_col column (array) with data value and units
#' @return extracted numeric value
#' @importFrom forcats %>%
#' @examples \dontrun{current_value(dat$Sample)}
current_value <- function(df_col){
  tmp <- df_col %>%
    stringr::str_extract(pattern = "\\-*\\d+\\,*\\d*") %>%
    stringr::str_replace(pattern = ",", replacement = ".") %>%
    as.numeric()
  return(tmp)
}


#' read Fluke multimeter CSV file
#' @author K. Juraic
#' @param file_name name of CSV file with measured data
#' @return data.frame() with read data
#' @importFrom utils read.table
#' @importFrom forcats %>%
#' @export
#' @examples \dontrun{read_fluke_csv("anodizacija_dat.csv")}
read_fluke_csv <- function(file_name = file.choose()){
  dat.header <- utils::read.table(file_name,
                           sep = ";",
                           skip = 4,
                           header = TRUE,
                           nrows = 1)
  dat <- utils::read.table(file_name,
                    sep = ";",
                    skip = 9,
                    header = TRUE,
                    nrows = dat.header$Total.readings,
                    dec = ',')
  dat$Sample <- current_value(dat$Sample)
  dat$Max <- current_value(dat$Max)
  dat$Average <- current_value(dat$Average)
  dat$Min <- current_value(dat$Min)
  dat$Start.Time <- dat$Start.Time %>%
    stringr::str_replace(pattern = ",", replacement = ".") %>%
    lubridate::dmy_hms()
  dat$Max.Time <- dat$Max.Time %>%
    stringr::str_replace(pattern = ",", replacement = ".") %>%
    lubridate::dmy_hms()
  dat$Min.Time <- dat$Min.Time %>%
    stringr::str_replace(pattern = ",", replacement = ".") %>%
    lubridate::dmy_hms()
  dat$Stop.Time <- dat$Stop.Time %>%
    stringr::str_replace(pattern = ",", replacement = ".") %>%
    lubridate::dmy_hms()
  dat$Start.sec <- dat$Start.Time - dat$Start.Time[1]
  dat$Stop.sec  <- dat$Stop.Time  - dat$Stop.Time[1]
  dat$Min.sec  <- dat$Min.Time  - dat$Min.Time[1]
  dat$Max.sec  <- dat$Max.Time  - dat$Max.Time[1]
  return(dat)
}
