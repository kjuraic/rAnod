
#' read Fluke multimeter CSV file
#' @author K. Juraic
#' @param file_name name of CSV file with measured data
#' @return data.frame() with read data
#' @importFrom utils read.table
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
  return(dat)
}
