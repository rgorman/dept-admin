require(pdftools)
require(tidyverse)
require(readr)

files.v <- dir(pattern = ".pdf")
files.v <- files.v[-c(26,27)] # remove Effie's course

holder.l <- vector(mode = "list", length(files.v))

for (i in seq_along(files.v)) {
  
  txt_pdf <- pdf_text(files.v[i]) # read in pdf as text file
  
  x <- txt_pdf %>% # each line of pdf becomes element of a character vector
    read_lines()
  
  # remove extraneous lines
  xx <- x[-which(str_detect(x, "^[0-9]\\.") == TRUE)]
  xx <- xx[-which(str_detect(xx, "^[0-9].\\.") == TRUE)]
  xx <- xx[-which(str_detect(xx, "^[a-zA-Z]") == TRUE)]
  xx <- xx[-which(str_detect(xx, "[a-z]|[A-Z]") == TRUE)]
  
  xx <- gsub("2018", "", xx)
  
  
  xx <- str_squish(xx) # remove extra white space
  
  xx <- xx[xx != ""]
  
  
  ########## make column names for output dataframe
  
  a <- "For Language Courses: Helpfulness of course in increasing "
  b <- c("your vocabulary skills", "your grasp of the grammar and structure of the language", "your reading and comprehension skills")
  c <- "For non-Language Courses: Helpfulness of course in teaching you "
  d <- c("to read carefully", "to write better", "to think critically and independently")
  
  
  
  col_names_1.v <- c(x[str_detect(x, "^[0-9]\\.")], x[which(str_detect(x, "^[0-9].\\.") == TRUE)][1:5] )  
  col_names_1.v <- append(col_names_1.v, paste(a, b))
  col_names_1.v <- append(col_names_1.v, paste(c, d))
  col_names_1.v <- append(col_names_1.v, x[which(str_detect(x, "^[0-9].\\.") == TRUE)][8:9])
  
  col_names_1.v <- gsub(".*[0-9]\\.", "", col_names_1.v) %>% # remove numerals from names
    str_squish()
  
  meta.v <- x[1] %>% # collect metadata for observation
    str_squish() %>%
    str_split(" ") %>%
    unlist() %>%
    gsub(",", "", .)
  
  
  meta.v <- meta.v %>%
    gsub("Eval", "", .)
  
  meta.v <- meta.v %>%
    gsub("MB01", "", .)
  
  meta.v <- meta.v[meta.v != ""] # drop empty element
  
  # if (meta.v[1] == "Turner") {
  #   r <- paste(meta.v[2], meta.v[6], sep = "/" )
  #   q <- paste( meta.v[3], meta.v[5], sep = "/" )
  #   
  #   
  #   meta.v <- c(meta.v[1], r, q, meta.v[8])
  #   
  # }
  
  if (length(meta.v) > 4 ) {
    if (meta.v[1] == "Loar" & str_detect(meta.v[5], "840") == TRUE ) {
      
      q <- paste( meta.v[3], meta.v[5], sep = "/" )
      
      
      meta.v <- c(meta.v[1:2], q, meta.v[6])
      
    }
  }
  
  if (meta.v[1] == "White") {
   meta.v[1] <- paste(meta.v[1], meta.v[2], sep = "-")
   meta.v <- meta.v[-2]
  }
  
 
  
  meta.v <- c(meta.v, x[str_detect(x, "Form MB01")] %>% #  add total number of evaluaitons scanned
                gsub("Form MB01", "", .) %>%
                gsub("Forms Read", "", .) %>%
                str_squish() %>%
                as.integer())
  
  
  if (str_detect(meta.v[4], "&") == TRUE) {
    r <- paste0(meta.v[3], meta.v[4], meta.v[6] )
    meta.v <- c(meta.v[1:2], r, meta.v[7:8])
    
  }
  
  
  meta.v <- c(meta.v[1], NA, meta.v[2:length(meta.v)])
  
 if ("Spring" %in% meta.v == FALSE ) {
   meta.v <- c(meta.v[1:4], "Fall",  meta.v[5:6])
 } 
  
 # meta.v <- c(meta.v[1:4], "Spring",  meta.v[5:6])
  
  #meta.v <- meta.v[-7]
  
  
  output.df <- matrix(nrow = 1, ncol = length(meta.v)) %>% # make dataframe 
    as.data.frame()
  
  colnames(output.df) <- c("Last_Name", "First_Name", "Prefix", "Course", "Term", "Year", "Number_of_Evals") # names for meta cols
  output.df[1,] <- meta.v # add metadata values to columns
  
  
  expanded_col_names.v <- NULL # multiple column names for each question (one for each possible rating)
  
  xx <- xx %>% # xx becomes list object; each list item is vector of 3 elements
    str_split(" ") 
  
  for (n in seq_along(col_names_1.v)) {
    expanded_col_names.v <- append(expanded_col_names.v, rep(col_names_1.v[n], 5)) 
  }
  
  for (n in seq_along(xx)) { # add values to dataframe
    nomen <- paste0(expanded_col_names.v[n], " Rating of ", xx[[n]][1])
    output.df[1, nomen] <- xx[[n]][2] %>%
           as.integer()
  }
  
  
  holder.l[[i]] <- output.df
  
  
  
} # end of loop i



big.df <- do.call(bind_rows, holder.l)
archive.df <- bind_rows(archive_1.df, big.df)

holder.l[[12]] %>%
  View()

big.df[, c(1:4, 118:127)] %>%
  View()



saveRDS(archive.df, file = "evaluations_from_pdf.RDS")

archive_1.df <- readRDS(file = "evaluations.RDS")

archive_1.df %>%
  colnames()

archive_2.df[, 5:6]
