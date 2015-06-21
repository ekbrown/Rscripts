# R script to take the words in a .txt or .csv file on your hard drive and retrieve their corresponding frequency numbers from the 20th century section of the Corpus del Español (corpusdelespanol.org), or a specific genre of that section, and save those frequencies to a .csv file

# (c) 2014 Earl K. Brown www-personal.ksu.edu/~ekbrown
# This script can be freely used and modified for non-profit purposes

#####
# NOTE:
# This script requires three packages not included in base R. If you don't already have the following packages on your computer, uncomment the next line (remove the hash tag) and run that one line, and then comment it again (put the hash tag back)
# install.packages("RCurl"); install.packages("stringr"); install.packages("XML")

#####
# FOLLOW THESE STEPS TO GET FREQUENCY NUMBERS

# STEP 1:
# Specify your credentials. This is the email you use to login to the corpus. If you're not sure which email you have registered, simply go to the corpus in a web browser and try logging in to figure it out.
login.email <- "your.name@your.institution.edu"
login.password <- "your.password"

# STEP 2:
# Specify the pathway to the file with the words and strings that you want frequency numbers for. The file should be a .txt or .csv file and have only one word or multi-word string per line. For example, the word "antes" should be on the first line, "antes de" on the second line, and "antes de que" on the third line, etc.
# NOTE: The number of words or strings in the file should be equal to or less than the daily search limit of your level of access. If you're not sure what that limit is, simply go to the Corpus in a web browser and view your profile and find your level of access and the corresponding daily search limit. If your file has more words than you can get in one day, you should split up the file into smaller files that can be processed on subsequent days.
input.file <- "/pathway/to/directory/input_file.txt"

# STEP 3:
# Specify the section you want to pull the frequency numbers from. To use the entire 20th century section use the option "all". If you'd like to use a specific genre of that section, specify one of the following: "oral" for the oral section, "fic" for fiction, "per" for periodicals, "acad" for academic. Additionally, if you'd like to pull frequency numbers from the three written sections (fiction, periodicals, and academic), specify "written".
freq.section <- "oral"

# STEP 4:
# Specify the pathway to the file the frequency numbers should be saved to. This will overwrite any previous files with the same name.
output.file <- "/pathway/to/directory/output_file.csv"

# STEP 5:
# Now, save this script and then run the whole thing, and sit back and let R do the work!

##################
##################
# DON'T CHANGE ANYTHING BELOW HERE, UNLESS YOU KNOW WHAT YOU'RE DOING

# begin script

# loads necessary packages and defines functions
library("RCurl")
library("stringr")
library("XML")

# defines function to encode urls for the Corpus
urlEncodeCdE <- function(url) {
  url <- gsub(" {1,}", "+", url)
  url <- gsub("á", "%E1", url)
  url <- gsub("é", "%E9", url)
  url <- gsub("í", "%ED", url)
  url <- gsub("ó", "%F3", url)
  url <- gsub("ú", "%FA", url)
  url <- gsub("ü", "%FC", url)
  url <- gsub("ñ", "%F1", url)
  url <- gsub("Á", "%C1", url)
  url <- gsub("É", "%C9", url)
  url <- gsub("Í", "%CD", url)
  url <- gsub("Ó", "%D3", url)
  url <- gsub("Ú", "%DA", url)
  url <- gsub("Ü", "%DC", url)
  url <- gsub("Ñ", "%D1", url)
  url <- gsub("'", "%27", url)
  url <- gsub(",", "%2C", url)
  return(url)
} # end defining function

# defines function to hex-encode urls
shortURLencode = function(url, reserved = F) {
  OK <- paste0("[^-ABCDEFGHIJKLMNOPQRSTUVWXYZ", "abcdefghijklmnopqrstuvwxyz0123456789$_.+!*'(),", if (!reserved) ";/?:@=&", "]")
  x <- strsplit(url, "")[[1L]]
  z <- grep(OK, x)
  if (length(z)) {
    y <- sapply(x[z], function(x) paste0("%", as.hexmode(utf8ToInt(x)), collapse = ""))
    x[z] <- y
  }
  paste0(x, collapse = "")
}

###
# gets words from input file
words <- str_trim(scan(input.file, what = "char", sep = "\n"))

# defines login url with login info
url <- sprintf("http://www.corpusdelespanol.org/login.asp?email=%s&password=%s&e=", shortURLencode(login.email), shortURLencode(login.password))

# deletes any cookies from previous sessions in Corpus
unlink("cookie.txt")

# creates initial curl handle
ch <- getCurlHandle(cookiefile = "cookie.txt", cookiejar = "cookie.txt", verbose = F, followlocation = T, header = T, useragent = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.7; rv:25.0) Gecko/20100101 Firefox/25.0")

# logins to get cookie in order to continue working in COCA
login.page <- getURL(url = url, curl = ch, referer = "http://www.corpusdelespanol.org/x.asp")

# gets code for genre
freq.section <- tolower(freq.section)
if (!freq.section %in% c("all", "oral", "fic", "per", "acad", "written")) {
  stop("You must specify freq.section as one of: 'all', 'oral', 'fic', 'per', 'acad', 'written'")
}
codes.df <- data.frame(
  genre = c("all", "oral", "fic", "per", "acad", "written"), 
  code = c("sec1=19", "sec1=23", "sec1=22", "sec1=21", "sec1=20", "sec1=22&sec1=21&sec1=20")
)
cur.code <- codes.df[grep(freq.section, codes.df$genre), 'code']

# creates output file on the hard drive
freq.label <- paste0("freq", toupper(freq.section))
column.headers <- paste("STRING", freq.label, sep = "\t")
cat(column.headers, file = output.file, sep = "\n")

# loops over words, pulling their freqs and saving them to the output file
for (i in 1:length(words)) {
  #i=1
  cur.word <- words[i]
  cat("Working on:", cur.word, "\n")
  
  # gets the frequency page
  cur.url <- paste0("http://www.corpusdelespanol.org/x2.asp?chooser=seq&p=", urlEncodeCdE(cur.word), "&w2=&wl=4&wr=4&r1=&r2=&ipos1=-select-&B7=SEARCH&", cur.code, "&sec2=0&sortBy=freq&sortByDo2=freq&minfreq1=freq&freq1=4&freq2=4&numhits=100&kh=100&groupBy=words&whatshow=raw&saveList=no&changed=&corpus=cde&word=&sbs=&sbs1=&sbsreg1=&sbsr=&sbsgroup=&redidID=&ownsearch=y&compared=&holder=&whatdo=seq&waited=y&rand1=y&whatdo1=1&didRandom=n&minFreq=freq&s1=0&s2=0&s3=0&perc=mi")
  freq.page <- getURL(url = cur.url, curl = ch, referer = "http://www.corpusdelespanol.org/x1.asp?h=&q1=&q=&c=cde", header = F)
  
  no.results <- grep("(Sorry, there are no matching records|NO MATCHES FOR THE FOLLOWING \'SLOTS\')", freq.page)
  no.results <- length(no.results) > 0
  if (no.results) {
    cat("\t\t*** Looks like '", cur.word, "' isn't in the Corpus, or at least in the '", freq.section, "' section/genre ***\n", sep = "")
    cur.freq <- 0
  } else {
    # word is found
    # retrieves the freq number
    freq.table <- readHTMLTable(freq.page, which = 2)
    cur.freq <- as.character(freq.table[1,'FREQ'])    
    if (length(cur.freq) == 0) {
      limit <- grep("You have exceeded the allowable number of queries", freq.page)
      if (any(limit)) stop("*** It looks like you have reached your maximum number of searches queries in the Corpus for today ***")
      cur.freq <- 0
    } # end if
    
  } # end if there are results
  
  
  # reports the freq to the user
  cat("\t", cur.freq, "\n")
  
  # saves the word and its freq to the output file
  cur.pair <- paste(cur.word, cur.freq, sep = "\t")
  cat(cur.pair, file = output.file, sep = "\n", append = T)
  
} # next word

cat("\aDone!\n")
cat("The frequencies are in the file '", output.file, "'\n", sep="")

# end script
