require(pdftools)
require(tidyverse)
require(readr)

files.v <- dir(pattern = ".pdf")

txt_pdf <- pdf_text(files.v[2])
dat_pdf <- pdf_data(files.v[2])

txt[[1]] %>%
  View()

pdf_txt %>%
  View()

dat_pdf[[1]] %>%
  View()

x <- txt_pdf %>%
  read_lines()
x[4]

resp <- which(str_detect(x, "Response"))
x[-resp]

x[-resp] %>%
  str_squish()
i <- 1

x[seq(from = (resp[i] + 1), to = (resp[i] + 6) )]  %>%
  str_squish()
z <- NULL

seq(from = (resp[i] + 1), to = (resp[i] + 6) )

for (i in seq_along(resp)) {
  z <- seq(from = (resp[i] + 1), to = (resp[i] + 6) ) %>%
    append(z, .)
}

x[z] 


x %>%
  str_detect("[^a-zA-Z]")
x[which(str_detect(x, "^[a-zA-Z]") == TRUE)]

z <- which(str_detect(x, "^[a-zA-Z]") == TRUE)

x[-z]

x[str_detect(x, "^[0-9]\\.")]
x[str_detect(x, "^[0-9].\\.")]

x[str_detect(xx, "^[0-9]/")]

grep("^[\\d\\+\\.]", x)

xx <- x[-which(str_detect(x, "^[0-9]\\.") == TRUE)]
xx <- xx[-which(str_detect(xx, "^[0-9].\\.") == TRUE)]
xx <- xx[-which(str_detect(xx, "^[a-zA-Z]") == TRUE)]
xx <- xx[-which(str_detect(xx, "[a-z]|[A-Z]") == TRUE)]
xx <- xx[-which(str_detect(xx, "^[0-9]/") == TRUE)]
xx <- str_remove_all(xx, "Spring 2019")



xx <- str_squish(xx)

xx <- xx[-which(xx == "")]
x[2]

xx[1] %>%
  str_split(" ")

zz <- sapply(xx, str_split, " ")


x[str_detect(x, "^[0-9]\\.")]
x[which(str_detect(x, "^[0-9].\\.") == TRUE)]
x[which(str_detect(x, "^[a-z]") == TRUE)]
      

a <- "For Language Courses: Helpfulness of course in increasing "
b <- c("your vocabulary skills", "your grasp of the grammar and structure of the language", "your reading and comprehension skills")



c <- "For non-Language Courses: Helpfulness of course in teaching you "
d <- c("to read carefully", "to write better", "to think critically and independently")



col_names_1.v <- c(x[str_detect(x, "^[0-9]\\.")], x[which(str_detect(x, "^[0-9].\\.") == TRUE)][1:5] )  
col_names_1.v <- append(col_names_1.v, paste(a, b))
col_names_1.v <- append(col_names_1.v, paste(c, d))
col_names_1.v <- append(col_names_1.v, x[which(str_detect(x, "^[0-9].\\.") == TRUE)][8:9])

col_names_1.v <- gsub(".*[0-9]\\.", "", col_names_1.v) %>%
  str_squish()

x[str_detect(x, "Forms Read")] %>% # total number of evaluaitons scanned
  gsub("Form MB01", "", .) %>%
  gsub("Forms Read", "", .) %>%
  str_squish() %>%
  as.integer()

x[1] %>%
  str_squish() %>%
  word(1)

meta.v <- x[1] %>%
  str_squish() %>%
  str_split(" ") %>%
  unlist() %>%
  gsub(",", "", .)

meta.v <- meta.v[-5]
meta.v <- c(meta.v, x[str_detect(x, "Forms Read")] %>% # total number of evaluaitons scanned
              gsub("Form MB01", "", .) %>%
              gsub("Forms Read", "", .) %>%
              str_squish() %>%
              as.integer())

###

output.df <- matrix(nrow = 1, ncol = length(meta.v)) %>%
  as.data.frame()

colnames(output.df) <- c("Last_Name", "First_Name", "Prefix", "Course", "Term", "Year", "Number_of_Evals")
output.df[1,] <- meta.v



expanded_col_names.v <- NULL

for (n in seq_along(col_names_1.v)) {
  expanded_col_names.v <- append(expanded_col_names.v, rep(col_names_1.v[n], 5)) 
}

for (n in seq_along(xx)) {
  nomen <- paste0(expanded_col_names.v[n], " Rating of ", xx[[n]][1])
  output.df[1, nomen] <- xx[[n]][2] %>%
    as.integer()
}

paste0(expanded_col_names.v[1], " Rating of ", xx[[1]][1])

xx <- xx %>%
  str_split(" ") 

xx[[1]][2] %>%
  as.integer()
