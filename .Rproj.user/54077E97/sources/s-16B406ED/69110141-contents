library(readxl)
library(writexl)
library(stringi)


# read exel file for given file name
read_file <- function(file_name) {
  data <- read_exel(file_name)
}

# write data frame to the given file name as xlsx file
write_excel <- function(df, file_name) {
  write_xlsx(df, file_name)
}

# given the input data frame, find all its column names
find_column_names <- function(input_df) {
  colnames(input_df)
}

# give the input data frame and the gene list column,
# extract the unique gene names as a list
get_genelist_from_df <- function(input_df, col){
  if(!(col %in% colnames(input_df))){
    stop("no such column in the file")
  }
  unique(input_df[[col]])
}

