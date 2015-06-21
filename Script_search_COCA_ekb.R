# R script to search for a string or several strings in the Corpus of Contemporary American English (corpus.byu.edu/coca) and save the results to a .csv file

# (c) 2014 Earl K. Brown www-personal.ksu.edu/~ekbrown
# Thanks to Jennifer Hinnell for feedback and suggestions on this script
# This script can be freely used and modified for non-profit purposes

#####
# NOTE:
# This script requires three packages not included in base R. If you don't already have the following packages on your computer, uncomment the next line (remove the hash tag) and run that one line, and then comment it again (put the hash tag back)
# install.packages("stringr"); install.packages("RCurl"); install.packages("XML")

#####
# FOLLOW THESE STEPS TO SEARCH THE CORPUS

# STEP 1:
# Specify your credentials. This is the email you use to login to the corpus. If you're not sure which email you have registered, simply go to the Corpus in a web browser and try logging in to figure it out.
login.email <- "your.name@your.institution.edu"
login.password <- "your.password"

# STEP 2:
# Specify the search string or strings. See the Corpus in a web browser for help with search syntax, including part-of-speech tags and wildcards, etc. You can specify multiple search strings by putting them into a vector, like this: c("searchstring1", "searchstring2", "searchstring3")
# NOTE: The Corpus doesn't handle apostrophes in a way one might expect. For example, you can't directly search for "I don't know"; you have to transform it to "I do n't know". If you have apostrophes in a search string, take a minute to manually search for it to see how you will need to transform it before running this script.
search.strings <- c(". You know ", ". I mean ")

# STEP 3:
# Specify the section you want to search in. To search in the entire Corpus use the option "all". If you'd like to use a specific genre of the Corpus, specify one of the following: "spok" for the spoken section, "fic" for fiction, "mag" for magazine, "news" for newspaper, "acad" for academic.
search.section <- "spok"

# STEP 4:
# Specify the maximum number of unique types to return for each search string (results shown in the upper right portion of the Corpus). For example, searching for nouns with the search string "[n*]" could potentially return tens of thousands of unique types, but you may only be interested in the 100 most frequent ones. For single word searches that don't have any part-of-speech tags or wildcards, for example "book", this number simply needs to be at least 1.
max.types <- 100

# STEP 5:
# Specify the maximum number of results to return for each search string before moving onto the next one, if you have specified more than one search string above in "search.strings". If you have specified only one search string, this number has the same effect as the number in the next step ("max.results.total").
max.results.per.search.string <- 1000

# STEP 6:
# Specify the maximum number of total results to return. This number should be equal to or smaller than your keyword-in-context view limit.
max.results.total <- 10000

# STEP 7:
# Specify whether you want to retrieve the links to the expanded view for each hit, with one of two options: "yes" or "no". Once the script has finished running, in order to view the expanded context for a given hit you will need to go to the Corpus in a web browser and login in, and then copy the corresponding link from the .csv file and paste it into the address bar and press <Enter> on your keyboard. 
expanded.view <- "yes"

# STEP 8:
# Specify the name of a .csv file that the results will be saved to, for example, "search_results.csv". This will overwrite any previous file with this name in your working directory.
output.file <- "search_results.csv"

# STEP 9:
# Now, save this script and then run the whole thing, and watch the magic happen!

##################
##################
# DON'T CHANGE ANYTHING BELOW HERE, UNLESS YOU KNOW WHAT YOU'RE DOING

# begin script
start.time <- strptime(date(), format = "%a %b %d %H:%M:%S %Y")

# loads necessary packages and defines functions
library("stringr")
library("RCurl")
library("XML")

# defines some helper functions
get.kwic.results <- function(webpage, expanded = FALSE) {
# 	webpage <- kwic.page
  
  webpage <- substr(webpage, regexpr("<html>", webpage), nchar(webpage))
  webpage <- sub("(charset=)windows-1252", "\\1UTF-8", webpage, ignore.case = T)  
  webpage <- gsub("<b><u>", "\t", webpage)
  webpage <- gsub("</u></b> ", "\t", webpage)
  webpage <- gsub("</u></b>", "\t", webpage)
  webpage <- gsub("\t\t", " ", webpage)
  
  res.table <- readHTMLTable(webpage, which = 2)
    
  year <- res.table[, 2]
  genre <- res.table[, 3]
  text.source <- res.table[, 4]
  text.source <- gsub("[\r\n\t]", "", text.source)
  hits.in.context <- res.table[, 8]
    
  if (length(year) != length(genre)) stop("For some reason, the number of centuries and the number of text names differ, but they shouldn't.")  
  if (length(genre) != length(text.source)) stop("From some reason, the number of years and the number of genre listings differ, but they shouldn't")
  if (length(text.source) != length(hits.in.context)) stop("For some reason, the number of text names and the number of keyword-in-context results differ, but they shouldn't.")
  
  
  if (expanded) {
    exp.links <- getHTMLLinks(webpage)
    exp.links <- grep("^x4\\.asp", exp.links, value = T)
    exp.links <- unique(exp.links)
    if (length(hits.in.context) != length(exp.links)) stop("For some reason, the number of links for the expanded view and the keyword-in-context results differ, but they shouldn't.")
    exp.links <- paste0("http://corpus.byu.edu/coca/", exp.links)
    output <- paste(year, genre, text.source, hits.in.context, exp.links, sep = "\t")
  } else {
    output <- paste(year, genre, text.source, hits.in.context, sep="\t")    
  }
  return(output)
} # end function

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
  url <- gsub("\\[", "%5B", url)
  url <- gsub("\\]", "%5D", url)
  url <- gsub("'", "%27", url)
  url <- gsub(",", "%2C", url)
  return(url)
} # end  function

shortURLencode = function(url, reserved = F) {
  OK <- paste0("[^-ABCDEFGHIJKLMNOPQRSTUVWXYZ", "abcdefghijklmnopqrstuvwxyz0123456789$_.+!*'(),", if (!reserved) ";/?:@=&", "]")
  x <- strsplit(url, "")[[1L]]
  z <- grep(OK, x)
  if (length(z)) {
    y <- sapply(x[z], function(x) paste0("%", as.hexmode(utf8ToInt(x)), collapse = ""))
    x[z] <- y
  }
  paste0(x, collapse = "")
} # end function

expanded.view <- tolower(expanded.view)
if (expanded.view == "yes") {
  expanded.view <- TRUE
} else if (expanded.view == "no") {
  expanded.view <- FALSE
} else {
  stop("You must specify either 'yes' or 'no' for the 'expanded.view' variable\n")
}

# creates output file for results to be saved to
if (expanded.view) {
  cat("searchTERM\tYEAR\tSECTION\tSOURCE\tPRE\tMATCH\tFOL\tEXPANDED\n", file = output.file)    
} else {
  cat("searchTERM\tYEAR\tSECTION\tSOURCE\tPRE\tMATCH\tFOL\n", file = output.file)  
}

# deletes any cookies from previous sessions in the Corpus
unlink("cookies.txt")

# creates initial curl handle
ch <- getCurlHandle(header = F, cookiefile = "cookies.txt", cookiejar = "cookies.txt", followlocation = T, autoreferer = T, useragent = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.9; rv:32.0) Gecko/20100101 Firefox/32.0")

# gets a cookie at the splash page
splash <- getURL("http://corpus.byu.edu/coca", curl = ch)

# logins into Corpus
url <- sprintf("http://corpus.byu.edu/coca/login.asp?email=%s&password=%s&e=", shortURLencode(login.email), shortURLencode(login.password))
login.page <- getURL(url = url, curl = ch)

# gets code for genre
search.section <- tolower(search.section)
if (!search.section %in% c("all", "spok", "fic", "mag", "news", "acad")) {
  stop("You must specify freq.section as one of: 'all', 'spok', 'fic', 'mag', 'news', 'acad'")
}
codes.df <- data.frame(genre = c("all", "spok", "fic", "mag", "news", "acad"), code = c(0, 1, 2, 3, 4, 5))
cur.code <- codes.df[grep(search.section, codes.df$genre), 'code']

# counter for running number of results
all.counter <- 0

for (h in 1:length(search.strings)) {
	#h=1
  search.string <- search.strings[h]
	cat("Working on search string ", h, " of ", length(search.strings), ": ", search.string, "\n", sep = "")  
  
	# gets the third page
  url <- paste0("http://corpus.byu.edu/coca/x2.asp?chooser=seq&p=", urlEncodeCdE(search.string), "&w2=&wl=4&wr=4&r1=&r2=&ipos1=-select-&B7=SEARCH&sec1=", cur.code, "&sec2=0&sortBy=freq&sortByDo2=freq&minfreq1=freq&freq1=10&freq2=10&numhits=", max.types, "&kh=100&groupBy=words&whatshow=raw&saveList=no&changed=&corpus=coca&word=&sbs=&sbs1=&sbsreg1=&sbsr=&sbsgroup=&redidID=&ownsearch=y&compared=&holder=&whatdo=seq&waited=y&rand1=y&whatdo1=1&didRandom=n&minFreq=freq&s1=0&s2=0&s3=0&perc=mi")
  freq.page <- getURL(url = url, curl = ch)
    
	no.results <- length(grep("(Sorry, there are no matching records.|NO MATCHES FOR THE FOLLOWING \\'SLOTS\\')", freq.page, ignore.case = T)) > 0
	if (no.results) {
	  stop(paste0("There appears to be no results for '", search.string, "'"))
	}
  
  links <- str_match_all(freq.page, "href=\"(x3.asp\\?[^\"]+)\">([ \\.,'A-ZÁÉÍÓÚÜÑ]+)</a>")
  links.themselves <- links[[1]][,2]
  words <- str_trim(links[[1]][,3])
	if (length(links.themselves) != length(words)) {
	    stop("The number of links and the words that those links are connected are different, and they shouldn't be")
	}

  # counter for number of results for current search string
  cur.counter <- 0
  
	for (i in 1:length(words)) {
	#i=1
	  cat("\tHit ", i, " of ", length(words), " for search term \"", search.string, "\": ", tolower(words[i]), "\n", sep = "")
	  kwic.page <- getURL(paste0("http://corpus.byu.edu/coca/", urlEncodeCdE(links.themselves[i])), curl = ch)
  
	  # figures out if there is more than one page of results
	  more.than.one.page <- length(grep("<span ID=\"w_page\">PAGE</span>:</font></td>", kwic.page)) > 0
	  if (more.than.one.page) {
		# true, there's more than one page of results
	
		# gets the number of pages of results and runs a loop that many times, loading each successive page and pulling the results
	
    num.pages <- as.integer(str_match(kwic.page, "&nbsp;&nbsp;1 / (\\d+)&nbsp;\r\n\r")[,2])
		
		for (j in 1:num.pages) {
		  # 	  j=1
		  # progress report
		  cat("\t\tPage ", j, " of ", num.pages, " of \"", tolower(words[i]), "\"\n", sep="")
		  if (j == 1) {
  			current.results <- get.kwic.results(kwic.page, expanded = expanded.view)
  			current.results <- paste(search.string, current.results, sep = "\t")
  			all.counter <- all.counter + length(current.results)
        cur.counter <- cur.counter + length(current.results)
  			if (all.counter > max.results.total) {
  			  current.results <- current.results[1:(length(current.results) - (all.counter - max.results.total))]
  			  cat(current.results, file = output.file, sep="\n", append = T)
  			  break
  		  }
  			if (cur.counter > max.results.per.search.string) {
  			  current.results <- current.results[1:(length(current.results) - (cur.counter - max.results.per.search.string))]
  			  cat(current.results, file = output.file, sep="\n", append = T)
  			  break
  			}
  			cat(current.results, file = output.file, sep="\n", append = T)
		  } else {
  			cur.link <- links.themselves[i]
  			cur.link <- sub("xx=\\d+", paste0("p=", j), cur.link)
  			fifth.url <- paste0("http://corpus.byu.edu/coca/", cur.link)
  			next.kwic.page <- getURL(url = fifth.url, curl = ch)
  			current.results <- get.kwic.results(next.kwic.page, expanded = expanded.view)
  			current.results <- paste(search.string, current.results, sep = "\t")
  			all.counter <- all.counter + length(current.results)
        cur.counter <- cur.counter + length(current.results)
  			if (all.counter > max.results.total) {
  			  current.results <- current.results[1:(length(current.results) - (all.counter - max.results.total))]
  			  cat(current.results, file = output.file, sep="\n", append = T)
  			  break
  			}
  			if (cur.counter > max.results.per.search.string) {
  			  current.results <- current.results[1:(length(current.results) - (cur.counter - max.results.per.search.string))]
  			  cat(current.results, file = output.file, sep="\n", append = T)
  			  break
  			}			
        cat(current.results, file = output.file, sep="\n", append = T)
		  }
		} # next page, end of j loop
	  } else {
  		# only one page of results, pulls the results from that one page
  		current.results <- get.kwic.results(kwic.page, expanded = expanded.view)
  		current.results <- paste(search.string, current.results, sep = "\t")
  		all.counter <- all.counter + length(current.results)
      cur.counter <- cur.counter + length(current.results)
  		if (all.counter > max.results.total) {
  		  current.results <- current.results[1:(length(current.results) - (all.counter - max.results.total))]
  		  cat(current.results, file = output.file, sep="\n", append = T)
  		  break
  		}
  		if (cur.counter > max.results.per.search.string) {
  		  current.results <- current.results[1:(length(current.results) - (cur.counter - max.results.per.search.string))]
  		  cat(current.results, file = output.file, sep="\n", append = T)
  		  break
  		}
      cat(current.results, file = output.file, sep="\n", append = T)
	  }
	  if (all.counter >= max.results.total | cur.counter >= max.results.per.search.string) {
		  break
	  }
	} # next link
	if (all.counter >= max.results.total) {
	  break
  }
  if (cur.counter >= max.results.per.search.string) {
    next
  }
} # next search string

end.time <- strptime(date(), format = "%a %b %d %H:%M:%S %Y")
dd <- difftime(end.time, start.time)

cat("\aDone!\n")
cat("The script took", round(dd, 1), attr(dd, "units"), "\n")
cat("The results file is named \"", output.file, "\" and is in the folder \"", getwd(), "\"\n", sep="")
cat("\n\n***If this script has been helpful to you while preparing a presentation and/or a publication, please acknowledge my efforts to facilitate the extraction of results from the COCA and my willingness to share this script publicly. Thanks!\n\nEarl K. Brown, Kansas State University\n\n")

# end script
