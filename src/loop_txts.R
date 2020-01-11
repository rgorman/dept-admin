rm(list = ls())
require(pdftools)
require(tidyverse)
require(readr)
require(readtext)

files.v <- dir(pattern = ".txt")

x <- readtext(files.v[1])
x$text

working <- x$text %>%
  read_lines()


working[p] %>%
  str_squish()


p <- seq(13, 57, 4) 
q <- seq(64, 100, 4)

working[c(p, q)]

working[1:20]

meta.v <- working[str_detect(working, "^Course")] %>%
  str_squish() %>%
  str_split(" ") %>%
  unlist()

meta.v <- meta.v[c(5, 2, 3)]

meta.v <- (working[str_detect(working, "^Semester")] %>%
  str_squish() %>%
  str_split(" ") %>%
  unlist() )[c(2, 3, 6)] %>%
  append(meta.v, .)

meta.v <- c(meta.v[1], NA, meta.v[2:length(meta.v)])


output.df <- matrix(nrow = 1, ncol = length(meta.v)) %>% # make dataframe 
  as.data.frame()

colnames(output.df) <- c("Last_Name", "First_Name", "Prefix", "Course", "Term", "Year", "Number_of_Evals") # names for meta cols
output.df[1, ] <- meta.v

###########

holder.l <- vector(mode = "list", length(files.v))

for (i in seq_along(files.v)) {
  
  x <- readtext(files.v[i])
  
  
  working <- x$text %>%
    read_lines()
  
  meta.v <- working[str_detect(working, "^Course")] %>%
    str_squish() %>%
    str_split(" ") %>%
    unlist()
  
  
  instr.v <- which(meta.v == "Instructor:") + 2
  
  meta.v <- c(meta.v[instr.v], meta.v[instr.v-1], meta.v[c(2, 3)])
  
  
  meta.v <- (working[str_detect(working, "^Semester")] %>%
               str_squish() %>%
               str_split(" ") %>%
               unlist() )[c(2, 3, 6)] %>%
    append(meta.v, .)
  
  # meta.v <- c(meta.v[1], NA, meta.v[2:length(meta.v)])
  
  
  output.df <- matrix(nrow = 1, ncol = length(meta.v)) %>% # make dataframe 
    as.data.frame()
  
  colnames(output.df) <- c("Last_Name", "First_Name", "Prefix", "Course", "Term", "Year", "Number_of_Evals") # names for meta cols
  output.df[1, ] <- meta.v
  
  
  p <- seq(13, 57, 4) 
  q <- seq(64, 100, 4)
  
  vals.l <- working[c(p, q)] %>%
    str_squish() %>%
    str_split(" ")
  
  col_names.v  <- working[ c(seq(12, 56, 4), seq(63, 99, 4)) ]
  
  col_names.v  <- col_names.v %>%
    gsub("[0-9]", "", .)
  
  col_names.v <- col_names.v %>%
    str_squish()
  
  rating.v <- seq(1, 5, 1) %>%
    rep(length(vals.l))
  
  for (n in seq_along(col_names.v)) {
    for (m in 1:5) {
      nomen <- paste0(col_names.v[n], ".", " Rating of ", m)
      output.df[ , nomen] <- vals.l[[n]][m] %>%
        as.integer()
    }
    
  }
  
  
  
  holder.l[[i]] <- output.df
  
}


big.df <- do.call(bind_rows, holder.l)

archive_2.df <- big.df

archive_2.df <- bind_rows(archive_2.df, big.df)

saveRDS(archive_2.df, file = "evaluations_from_txt.RDS")

################

i <- 15

big.df$Last_Name[24:25] <- "Crawford"
