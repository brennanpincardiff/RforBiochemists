# download scripts from the internet and open in R-Studio
base_url <- "https://raw.githubusercontent.com/brennanpincardiff/RforBiochemists/master/R_for_Biochemists_101/"

open_file_from_github <- function(base_url, file_name){
  link <- paste0(base_url, file_name)
  download.file(url=link, destfile=file_name, mode="wb")
  file.edit(file_name)
}

# download script for module 1
file_name <- "module_1.R"
open_file_from_github(base_url, file_name)

# download script for module 2
file_name <- "module_2.R"
open_file_from_github(base_url, file_name)

# download script for module 3
file_name <- "module_3.R"
open_file_from_github(base_url, file_name)
