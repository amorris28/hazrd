## code to prepare `phs_input_example` dataset goes here

gmat = read.table("data-raw/gmat.csv", header = FALSE, sep = ",")
gmat = as.matrix(gmat)

age = read.table("data-raw/age.csv", header = FALSE, sep = ",")

snpidlist = read.table("data-raw/snpidlist.csv", header = FALSE, sep = ",")
colnames(gmat) = as.character(snpidlist[[1]])

casevec = read.table("data-raw/casevec.csv", header = FALSE, sep = ",")

my_data = data.frame("age" = age[[1]], "status" = casevec[[1]])

phs_input_example = cbind(my_data, gmat)

usethis::use_data(phs_input_example, compress = "bzip2", overwrite = TRUE)
