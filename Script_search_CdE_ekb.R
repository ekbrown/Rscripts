# R script to search for a string in the Corpus del Español (corpusdelespanol.org) and save the results to a .csv file

# (c) 2014 Earl K. Brown www-personal.ksu.edu/~ekbrown
# This script can be freely used and modified for non-profit purposes

#####
# NOTE:
# This script requires four packages not included in base R. If you don't already have the following packages on your computer, uncomment the next line (remove the hash tag) and run that one line, and then comment it again (put the hash tag back)
# install.packages("gsubfn"); install.packages("RCurl"); install.packages("stringr"); install.packages("XML")

#####
# FOLLOW THESE STEPS TO SEARCH THE CORPUS

# STEP 1:
# Specify your credentials. This is the email you use to login to the corpus. If you're not sure which email you have registered, simply go to the Corpus in a web browser and try logging in to figure it out.
login.email <- "your.name@your.institution.edu"
login.password <- "your.password"

# STEP 2:
# Specify the search string or strings. See the corpus in a web browser for help with search syntax, including part-of-speech tags and wildcards, etc. You can specify multiple search strings by putting them into a vector, like this: c("searchstring1", "searchstring2", "searchstring3")
search.strings <- c("me [gustar]", "me [encantar]")

# STEP 3:
# Specify which section of the corpus you want to search in, from two options: "oral" and "all". The option "oral" will search only in the oral section of the 20th century while "all" will search in all centuries, including the four sections of the 20th century. When searching, particularly when using the "all" option, care should be taken so as to not run up against your daily keyword-in-context view limit, which is based on the level of access your profile allows for.
search.section <- "oral"

# STEP 4:
# Specify the maximum number of unique types to return for each search string (results shown in the upper right portion of the Corpus). For example, searching for nouns with the search string "[n*]" could potentially return tens of thousands of unique types, but you may only be interested in the 100 most frequent ones. For single word searches that don't have any part-of-speech tags or wildcards, for example "libro", this number simply needs to be at least 1.
max.types <- 100

# STEP 5:
# Specify the maximum number of results to return. This number should be equal to or smaller than your keyword-in-context view limit.
max.results <- 1000

# STEP 6:
# Specify whether you want to retrieve the links to the expanded view for each hit, with one of two options: "yes" or "no". Once the script has finished running, in order to view the expanded context for a given hit you will need to go to the Corpus in a web browser and login in, and then copy the corresponding link from the .csv file and paste it into the address bar and press <Enter> on your keyboard. 
expanded.view <- "yes"

# STEP 7:
# Specify the name of a .csv file that the results will be saved to, for example, "search_results.csv". This will overwrite any previous file with this name in your working directory.
output.file <- "search_results.csv"

# STEP 8:
# Now, save this script and then run the whole thing, and watch the magic happen!

##################
##################
# DON'T CHANGE ANYTHING BELOW HERE, UNLESS YOU KNOW WHAT YOU'RE DOING

# begin script
start.time <- strptime(date(), format = "%a %b %d %H:%M:%S %Y")

# loads necessary packages and defines functions
library("RCurl")
library("gsubfn")
library("stringr")
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
    
  century <- res.table[, 2]
  text.source <- res.table[, 3]
  text.source <- gsub("[\r\n\t]", "", text.source)
  hits.in.context <- res.table[, 7]
    
  if (length(century) != length(text.source)) stop("For some reason, the number of centuries and the number of text names differ, but they shouldn't.")  
  if (length(text.source) != length(hits.in.context)) stop("For some reason, the number of text names and the number of keyword-in-context results differ, but they shouldn't.")
  
  if (expanded) {
    exp.links <- getHTMLLinks(webpage)
    exp.links <- grep("^x4\\.asp", exp.links, value = T)
    exp.links <- unique(exp.links)
    if (length(hits.in.context) != length(exp.links)) stop("For some reason, the number of links for the expanded view and the keyword-in-context results differ, but they shouldn't.")
    exp.links <- paste0("http://www.corpusdelespanol.org/", exp.links)
    output <- paste(century, text.source, hits.in.context, exp.links, sep = "\t")
  } else {
    output <- paste(century, text.source, hits.in.context, sep="\t")    
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
  cat("searchTERM\tYEARS\tSOURCE\tPRE\tMATCH\tFOL\tEXPANDED\n", file = output.file)    
} else {
  cat("searchTERM\tYEARS\tSOURCE\tPRE\tMATCH\tFOL\n", file = output.file)  
}

# deletes any cookies from previous sessions in the Corpus
unlink("cookies.txt")

# creates initial curl handle
ch <- getCurlHandle(header = T, cookiefile = "cookies.txt", cookiejar = "cookies.txt", followlocation = T, autoreferer = T, useragent = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.9; rv:32.0) Gecko/20100101 Firefox/32.0")

# gets first cookie on splash screen
splash.screen <- getURL("http://www.corpusdelespanol.org/", curl = ch)

# defines login url with login credentials
url <- sprintf("http://www.corpusdelespanol.org/login.asp?email=%s&password=%s&e=", shortURLencode(login.email), shortURLencode(login.password))

# logins to get cookie in order to continue working in Corpus
login.page <- getURL(url = url, curl = ch)

# sets the switch for the sections to search in
search.section <- tolower(search.section)
if (!search.section %in% c("all", "oral")) stop("You must specify the search.section variable as either 'all' or 'oral'\n")
if (search.section == "oral") {
  search.section.code <- 23
} else {
  search.section.code <- 0
}

# counter for running number of results
counter <- 0

for (h in 1:length(search.strings)) {
	#h=1
  search.string <- search.strings[h]
	cat("Working on search string ", h, " of ", length(search.strings), ": ", search.string, "\n", sep = "")

	# gets the third page
	url <- paste0("http://www.corpusdelespanol.org/x2.asp?chooser=seq&p=", urlEncodeCdE(search.string), "&w2=&wl=4&wr=4&r1=&r2=&ipos1=-select-&B7=SEARCH&sec1=", search.section.code, "&sec2=0&sortBy=freq&sortByDo2=freq&minfreq1=freq&freq1=4&freq2=4&numhits=", max.types, "&kh=100&groupBy=words&whatshow=raw&saveList=no&changed=&corpus=cde&word=&sbs=&sbs1=&sbsreg1=&sbsr=&sbsgroup=&redidID=&ownsearch=y&compared=&holder=&whatdo=seq&waited=y&rand1=y&whatdo1=1&didRandom=n&minFreq=freq&s1=0&s2=0&s3=0&perc=mi")
  
  freq.page <- getURL(url = url, curl = ch)
  
  
	no.results <- length(grep("(Sorry, there are no matching records.|NO MATCHES FOR THE FOLLOWING \\'SLOTS\\')", freq.page, ignore.case = T)) > 0
	if (no.results) {
	  stop(paste0("There appears to be no results for '", search.string, "'"))
	}

	links <- str_trim(unlist(strapplyc(freq.page, "href=\"(x3.asp\\?[^\"]+)\">([ A-ZÁÉÍÓÚÜÑ]+)</a>", backref = 2)))
	links.themselves <- grep("x3\\.asp\\?", links, value = T)
	words <- grep("^[ A-ZÁÉÍÓÚÜÑ]+$", links, value = T)
	if (length(links.themselves) != length(words)) {
	  stop("The number of links and the words that those links are connected are different, and they shouldn't be")
	}

	for (i in 1:length(words)) {
	#i=1
	  cat("\tHit ", i, " of ", length(words), " for search term \"", search.string, "\": ", tolower(words[i]), "\n", sep = "")
	  kwic.page <- getURL(paste0("http://www.corpusdelespanol.org/", urlEncodeCdE(links.themselves[i])), curl = ch)
  
	  # figures out if there is more than one page of results
	  more.than.one.page <- length(grep("<span ID=\"w_page\">PAGE</span>:</font></td>", kwic.page)) > 0
	  if (more.than.one.page) {
		# true, there's more than one page of results
	
		# gets the number of pages of results and runs a loop that many times, loading each successive page and pulling the results
	
		num.pages <- as.numeric(strapplyc(kwic.page, "&nbsp;&nbsp;1 / (\\d+)&nbsp;\r\n\r", backref=1)[[1]])
	
		for (j in 1:num.pages) {
		  # 	  j=1
		  # progress report
		  cat("\t\tPage ", j, " of ", num.pages, " of \"", tolower(words[i]), "\"\n", sep="")
		  if (j == 1) {
  			current.results <- get.kwic.results(kwic.page, expanded = expanded.view)
  			current.results <- paste(search.string, current.results, sep = "\t")
  			counter <- counter + length(current.results)
  			if (counter > max.results) {
  			  current.results <- current.results[1:(length(current.results) - (counter - max.results))]
  			  cat(current.results, file = output.file, sep="\n", append = T)
  			  break
  		  }
  			cat(current.results, file = output.file, sep="\n", append = T)
		  } else {
  			cur.link <- links.themselves[i]
  			cur.link <- sub("xx=\\d+", paste0("p=", j), cur.link)
  			fifth.url <- paste0("http://www.corpusdelespanol.org/", cur.link)
  			next.kwic.page <- getURL(url = fifth.url, curl = ch)
  			current.results <- get.kwic.results(next.kwic.page, expanded = expanded.view)
  			current.results <- paste(search.string, current.results, sep = "\t")
  			counter <- counter + length(current.results)
  			if (counter > max.results) {
  			  current.results <- current.results[1:(length(current.results) - (counter - max.results))]
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
  		counter <- counter + length(current.results)
  		if (counter > max.results) {
  		  current.results <- current.results[1:(length(current.results) - (counter - max.results))]
  		  cat(current.results, file = output.file, sep="\n", append = T)
  		  break
  		}
  		cat(current.results, file = output.file, sep="\n", append = T)
	  }
	  if (counter >= max.results) {
		  break
	  }
	} # next link
	if (counter >= max.results) {
	  break
  }
} # next search string

end.time <- strptime(date(), format = "%a %b %d %H:%M:%S %Y")
dd <- difftime(end.time, start.time)

cat("\aDone!\n")
cat("The script took", round(dd, 1), attr(dd, "units"), "\n")
cat("The results file is named \"", output.file, "\" and is in the folder \"", getwd(), "\"\n", sep="")

# end script
