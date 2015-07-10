# Script to get frequency lists of words (single words, bigrams, and trigrams) in a directory of text files on a computer

# (c) 2015 Earl K. Brown ekbrown@ksu.edu
# This script can be redistributed and modified for non-profit purposes

# Stefan Th. Gries' function dispersions1 is used here in a modified form. The original is available from: http://www.linguistics.ucsb.edu/faculty/stgries/research/dispersion/_dispersion1.r

# clears the working memory
rm(list = ls(all = T))

#####
# NOTE:
# This script requires four packages not included in base R. If you don't already have the following packages on your computer, uncomment the next line (remove the hash tag) and run that one line, and then comment it again (put the hash tag back)
# install.packages("Rcpp"); install.packages("stringr"); install.packages("dplyr"); install.packages("readr")

##################
# SPECIFY THE DIRECTORY WITH THE TEXT FILES
# input.folder <- "/pathway/to/directory/with/texts"
input.folder <- "/Users/earlbrown/Corpora/United_States/California/Salinas/Textos/Finished"

# SPECIFY THE DIRECTORY WHERE THE FREQUENCY LISTS SHOULD BE SAVED
# output.folder <- "/pathway/to/directory/where/files/should/be/saved"
output.folder <- "/Users/earlbrown/Documents"

# SPECIFY HOW MANY GRAMS TO INCLUDE IN EACH LIST; USE "-1" TO GET ALL POSSIBLE GRAMS
num.grams = 1000
##################

# DON'T CHANGE ANYTHING BELOW HERE, UNLESS YOU KNOW WHAT YOU'RE DOING

# gets start time of script
start.time <- strptime(date(), format = "%a %b %d %H:%M:%S %Y")

# loads packages
library("Rcpp")
library("readr")
library("stringr")
library("dplyr")

##
# defines ngram function
create.ngrams <- function(corpus.lines, num.words = 2, combiner = " ") {
			
  corpus.lines <- cur.file
  
	# put spaces around punctuation that likely represents a pause
	corpus.lines <- str_replace_all(corpus.lines, "([\\.\\,\\?\\!\\:\\;])", " \\1 ")
	
	#split up into individual words and punctuation
	corpus.words <- unlist(str_split(str_to_lower(corpus.lines), "\\s+"), use.names = F)
	
	#exclude empty elements
	corpus.words <- corpus.words[str_length(corpus.words) > 0]
	
	# gets ngrams
	all.ngrams <- apply(
	  mapply(
	    seq, 1:(length(corpus.words) - num.words + 1), num.words:length(corpus.words)), 2, function(q) paste(corpus.words[q], collapse=combiner
    ) # end mapply
	) # end apply

	# exclude grams with punctuation
	all.ngrams <- all.ngrams[!str_detect(all.ngrams, "[\\;|\\:|\\.|\"|\\?|\\[|\\]|\\,|\\(|\\)|\\!|&|/|\\\\|\\’]")]
	
	return(all.ngrams)
	
} # end defining word gram function

##
# defines two C++ functions
cppFunction('
  int num_matches(CharacterVector corpus, String word) {
    int n = corpus.size();
    int cur_num = 0;
    String cur_wd;
    for (int i = 0; i < n; i++) {
      cur_wd = corpus[i];
        if (cur_wd == word) {
          cur_num++;
        }
      }
      return cur_num;
    }
')

cppFunction('
  IntegerVector match_or_not(CharacterVector corpus, String word) {
    int n = corpus.size();
    String cur_wd;
    IntegerVector output(n);
    for (int i = 0; i < n; i++) {
      cur_wd = corpus[i];
      if (cur_wd == word) {
        output[i] = 1;
      } else {
        output[i] = 0;
      }
    }
    return output;
  }
')

##
# defines a simplified version of Stefan Th. Gries' function "dispersions1"
# please cite: Gries, Stefan Th. 2008. Dispersions and adjusted frequencies in corpora. /International Journal of Corpus Linguistics/ 13(4). 403-437.

dispersions1 <- function(corpus, corpus.part.sizes, element) {

	if(sum(corpus.part.sizes) != length(corpus)) stop("The sum of the sizes of the corpus parts is not identical to the corpus size!")
  corpus.parts <- rep(1:length(corpus.part.sizes), corpus.part.sizes)

	n <- length(corpus.part.sizes)
	l <- length(corpus)
	f <- num_matches(corpus, element)
	if(f==0) { return("NA"); break() }
	s <- corpus.part.sizes/l # s
	v <- rowsum(match_or_not(corpus, element), corpus.parts)
	
	values <- list()	
	values[["element"]] <- element
	values[["range"]] <- sum(v>0)
	values[["Deviation of proportions DP"]] <- sum(abs((v/f)-s))/2 # Gries
	values[["Deviation of proportions DP (normalized)"]] <- (sum(abs((v/f)-s))/2)/(1-min(s))

	return(values)
} # end defining function "dispersions1"

##
# defines function to figure out how many grams to pull out
how.many.grams <- function(number.grams, len.grams) {
	if (number.grams == -1) {
		how.many.grams <- len.grams
	} else {
		how.many.grams <- number.grams
	}
	return(how.many.grams)

} # end defining function

##################
# this section saves temporary files to store words and creates collector vectors for the numbers, both to be used later in the script

setwd(output.folder)

# single words
cat(file = "single_words.csv")
single.numbers <- c()

# bigrams
cat(file = "bigram_words.csv")
bigram.numbers <- c()

# trigrams
cat(file = "trigram_words.csv")
trigram.numbers <- c()

##################
# this section loops through the files, pulling out single words, bigrams, and trigrams from each file

corpus.files <- dir(input.folder, pattern = "\\.txt", full.names = T, ignore.case = T)

cat("\nWorking on gathering words, bigrams, and trigrams from the files...\n")

# loop to load each corpus file
for (i in 1:length(corpus.files)) {
	# i <- 1
	cur.name <- corpus.files[i]
	
	# prints a progress report
	if (i %% 10 == 0) {
	  cat("\tAdding file ", i, " of ",  length(corpus.files), ": ", basename(cur.name), "\n", sep="")
	} else if (i == length(corpus.files)) {
	  cat("\tAdding last file, file ", i, " of ",  length(corpus.files), ": ", basename(cur.name), "\n", sep="")
	}
	
	# loads corpus file and makes it lower-case
	cur.file <- str_to_lower(read_lines(cur.name))

	# delete the annotations that aren't part of the speech of the speakers
	cur.file <- str_replace(cur.file, "^[^:]*:", "")
	cur.file <- str_replace_all(cur.file, "\t", " ")
	
	# puts a period at the end of each line
	cur.file <- str_replace(cur.file, "$", "\\.")
	
	# splits up at non-word characters
	cur.words <- unlist(str_split(cur.file, "[^-a-záéíóúñü\\']+"))
	cur.words <- cur.words[str_length(cur.words) > 0]

	# gets bigrams and trigrams
	cur.bigrams <- create.ngrams(cur.file, num.words = 2)
	cur.trigrams <- create.ngrams(cur.file, num.words = 3)
	
	##
	# saves words, bigrams, and trigrams to temp files, and save the number of each to collector vectors
		
	# saves words
	write_csv(data_frame(cur.words), "single_words.csv", append = T)
	single.numbers <- append(single.numbers, length(cur.words))

	# saves bigrams
	write_csv(data_frame(cur.bigrams), "bigram_words.csv", append = T)
  bigram.numbers <- append(bigram.numbers, length(cur.bigrams))
		
	# saves trigrams
	write_csv(data_frame(cur.trigrams), "trigram_words.csv", append = T)
	trigram.numbers <- append(trigram.numbers, length(cur.trigrams))
	
} # next corpus file

#############
# this section creates the single word frequency list 

cat("\nWorking on creating the frequency list of single words...\n")

push.to.hard.drive <- 1000

# retrieves single words from temp file
single.data <- read_csv("single_words.csv", col_names = F)

# converts data from data frames to vectors
single.data <- as.data.frame(single.data)[, 1]

# gets frequencies
raw.freq <- sort(table(single.data), decreasing = T)
log.freq <- round(log(raw.freq), 3)
norm.freq <- round((raw.freq / sum(raw.freq)) * 1000000, 3)

num.grams.to.extract <- how.many.grams(num.grams, length(raw.freq))

# saves output file
output.file <- "freq_list_single_words.csv"
cat("RANK,WORD,RAW.FREQ,LOG.FREQ,NORM.FREQ,RANGE,RANGE%,DP,DP.NORM", file = output.file, sep = "\n")

all.range.dp <- data_frame()

for (i in 1:num.grams.to.extract) {
	# i <- 1
	if (i %% push.to.hard.drive == 0) {
	  cat("\tGetting dispersion of word ", i, " of ", num.grams.to.extract, ": ",  names(raw.freq)[i], "\n", sep="")	

	  # saves set of 100 ranges and dps to hard drive
		write_csv(all.range.dp, path = output.file, append = T)
		
		# empties out the range and dp collector vectors
		all.range.dp <- data_frame()

	} # end of if this is a 100th iteration
	
	# gets range and Gries' DP dispersion measurement
	word.dispersion <- dispersions1(single.data, single.numbers, names(raw.freq)[i])

	cur.range <- word.dispersion$range
	cur.range.perc <- round((cur.range/length(corpus.files)) * 100, digits = 2)
	cur.dp <- round(word.dispersion$'Deviation of proportions DP', 5)
	cur.dp.norm <- round(word.dispersion$'Deviation of proportions DP (normalized)', 5)

  output.data <- data_frame(i, names(raw.freq)[i], raw.freq[i], log.freq[i], norm.freq[i], cur.range, cur.range.perc, cur.dp, cur.dp.norm)

	all.range.dp <- bind_rows(all.range.dp, output.data)

	# if last iteration, saves the collector vector to hard drive
	if (i == num.grams.to.extract) {
	  write_csv(all.range.dp, path = output.file, append = T)
	} # end of if last iteration

} # next word

unlink("single_words.txt")
rm(single.numbers)

cat("\nFinished creating the frequency list of single words:\n", paste(output.folder, output.file, sep="/"), "\n", sep="")

#############
# this section creates the bigram word frequency list 

cat("\nWorking on creating the frequency list of bigrams...\n")

# retrieves bigram words from temp file
bigram.data <- read_csv("bigram_words.csv", col_names = F)

# converts data from data frames to vectors
bigram.data <- as.data.frame(bigram.data)[, 1]

# gets frequencies
raw.freq <- sort(table(bigram.data), decreasing = T)
log.freq <- round(log(raw.freq), 3)
norm.freq <- round((raw.freq/sum(raw.freq)) * 1000000, 3)

# determines how many grams to extract
num.grams.to.extract <- how.many.grams(num.grams, length(raw.freq))

# saves output file
output.file <- "freq_list_bigrams.csv"
cat("RANK,BIGRAM,RAW.FREQ,LOG.FREQ,NORM.FREQ,RANGE,RANGE%,DP,DP.NORM", file = output.file, sep = "\n")

all.range.dp <- data_frame()

for (i in 1:num.grams.to.extract) {

	if (i %% push.to.hard.drive == 0) {
	    cat("\tGetting dispersion of bigram ", i, " of ", num.grams.to.extract, ": ",  names(raw.freq)[i], "\n", sep="")	
		
		# saves set of 100 ranges and dps to hard drive
		write_csv(all.range.dp, path = output.file, append = T)
		
		# empties out the range and dp collector vectors
		all.range.dp <- data_frame()
		
	} # end of if this is a 100th iteration
	
	# gets range and Gries' DP dispersion measurement
	word.dispersion <- dispersions1(bigram.data, bigram.numbers, names(raw.freq)[i])
	cur.range <- word.dispersion$range
	cur.range.perc <- round((cur.range/length(corpus.files)) * 100, 2)
	cur.dp <- round(word.dispersion$'Deviation of proportions DP', 5)
	cur.dp.norm <- round(word.dispersion$'Deviation of proportions DP (normalized)', 5)

	output.data <- data_frame(i, names(raw.freq)[i], raw.freq[i], log.freq[i], norm.freq[i], cur.range, cur.range.perc, cur.dp, cur.dp.norm)
	
	all.range.dp <- bind_rows(all.range.dp, output.data)

	# if last iteration, saves the collector vector to hard drive
	if (i == num.grams.to.extract) {
	  write_csv(all.range.dp, path = output.file, append = T)
	} # end of if last iteration
	
} # next bigram

unlink("bigram_words.txt")
rm("bigram.numbers")

cat("\nFinished creating the frequency list of bigram words:\n", paste(output.folder, output.file, sep="/"), "\n", sep="")

#############
# this section creates the trigram word frequency list 

cat("\nWorking on creating the frequency list of trigrams...\n")

# retrieves bigram words from temp file
trigram.data <- read_csv("trigram_words.csv", col_names = F)

# converts data from data frames to vectors
trigram.data <- as.data.frame(trigram.data)[, 1]

# gets frequencies
raw.freq <- sort(table(trigram.data), decreasing=T)
log.freq <- round(log(raw.freq), 3)
norm.freq <- round((raw.freq/sum(raw.freq)) * 1000000, 3)

# determines how many grams to extract
num.grams.to.extract <- how.many.grams(num.grams, length(raw.freq))

# saves output file
output.file<-"freq_list_trigrams.csv"
cat("RANK\tTRIGRAM\tRAW.FREQ\tLOG.FREQ\tNORM.FREQ\tRANGE\tRANGE%\tDP\tDP.NORM", file = output.file, sep = "\n")

all.range.dp <- data_frame()

for (i in 1:num.grams.to.extract) {
	
	if (i %% push.to.hard.drive == 0) {
		cat("\tGetting dispersion of trigram ", i, " of ", num.grams.to.extract, ": ",  names(raw.freq)[i], "\n", sep="")	
		
	  # saves set of 100 ranges and dps to hard drive
	  write_csv(all.range.dp, path = output.file, append = T)
	  
	  # empties out the range and dp collector vectors
	  all.range.dp <- data_frame()

	} # end of if this is a 100th iteration
	
	# gets range and Gries' DP dispersion measurement
	word.dispersion <- dispersions1(trigram.data, trigram.numbers, names(raw.freq)[i])
	cur.range <- word.dispersion$range
	cur.range.perc <- round((cur.range/length(corpus.files)) * 100, 2)
	cur.dp <- round(word.dispersion$'Deviation of proportions DP', 5)
	cur.dp.norm <- round(word.dispersion$'Deviation of proportions DP (normalized)', 5)

	output.data <- data_frame(i, names(raw.freq)[i], raw.freq[i], log.freq[i], norm.freq[i], cur.range, cur.range.perc, cur.dp, cur.dp.norm)
	
	all.range.dp <- bind_rows(all.range.dp, output.data)
	
	# if last iteration, saves the collector vector to hard drive
	if (i == num.grams.to.extract) {
	  write_csv(all.range.dp, path = output.file, append = T)
	} # end of if last iteration

} # next trigram

unlink("trigram_words.txt")
rm(trigram.numbers)

cat("\nFinished creating the frequency list of trigram words:\n", paste(output.folder, output.file, sep="/"), "\n", sep="")

cat("\a\nAll done!\nThree frequency lists have been created and are located in the folder:\n", output.folder, "\n\nIf you use the DP or DP normalized measurements, please cite:\nGries, Stefan Th. 2008. Dispersions and adjusted frequencies in corpora. /International Journal of Corpus Linguistics/ 13(4):403-437.", sep="")

end.time <- strptime(date(), format = "%a %b %d %H:%M:%S %Y")
dd <- difftime(end.time, start.time)
cat("The script took", round(dd, 1), attr(dd, "units"), "\n")

