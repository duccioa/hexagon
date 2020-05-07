library(data.table)
n = fread("./inst/extdata/1000_random_nouns.txt", header=FALSE)
a = fread("./inst/extdata/1000_random_adjectives.txt", header=FALSE)
random_names = n$V1
random_adjectives = a$V1

usethis::use_data(random_names, overwrite = TRUE)
usethis::use_data(random_adjectives, overwrite = TRUE)
