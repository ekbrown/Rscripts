# Script to get freq lists of words in a folder of text files

# (c) 2012 Earl K. Brown ekbrown@ksu.edu
# Stefan Th. Gries' function dispersions1 is used here in a modified form. The original is available from: http://www.linguistics.ucsb.edu/faculty/stgries/research/dispersion/_dispersion1.r
# This script can be redistributed and modified for non-profit purposes

# clears the working memory
rm(list=ls(all=T))

###########################################
# SPECIFY THE FOLDER WITH THE TEXT FILES
input.folder<-"/Users/earlbrown/Corpora/United_States/California/Salinas/Textos/Finished_Word"

# SPECIFY THE FOLDER WHERE THE FREQUENCY LISTS SHOULD BE SAVED
output.folder<-"/Users/earlbrown/Corpora/United_States/California/Salinas"

# SPECIFY HOW MANY GRAMS TO INCLUDE IN EACH LIST; USE "-1" TO GET ALL POSSIBLE GRAMS
num.grams = 212
###########################################

# defines ngram function
create.ngrams<-function(corpus.lines, num.words=2, combiner=" ") {
			
	# put spaces around punctuation that likely represents a pause
	corpus.lines<-gsub("([\\.\\,\\?\\!\\:\\;])", " \\1 ", corpus.lines, perl=T)

	#split up into individual words and punctuation
	corpus.words<-unlist(strsplit(tolower(corpus.lines), "\\s+", perl=T))

	#exclude empty elements
	corpus.words<-corpus.words[nchar(corpus.words)>0]

	# gets ngrams
	all.ngrams<-apply(mapply(seq, 1:(length(corpus.words)-num.words+1), num.words:length(corpus.words)), 2, function(q) paste(corpus.words[q], collapse=combiner))

	# exclude grams with punctuation
	exclude<-grep("[\\;|\\:|\\.|\"|\\?|\\[|\\]|\\,|\\(|\\)|\\!|&|/|\\\\|\\’]", all.ngrams, perl=T)
	all.ngrams<-all.ngrams[-exclude]

	return(all.ngrams)
}

# end defining word gram function

#######
# defines Stefan Th. Gries' function "dispersions1"
# please cite: Gries, Stefan Th. 2008. Dispersions and adjusted frequencies in corpora. /International Journal of Corpus Linguistics/ 13(4). 403-437.

dispersions1 <- function(corpus, corpus.part.sizes, element) {

	if(sum(corpus.part.sizes)!=length(corpus)) stop("The sum of the sizes of the corpus parts is not identical to the corpus size!")
	corpus.parts <- rep(1:length(corpus.part.sizes), corpus.part.sizes)

	n <- length(corpus.part.sizes) # n
	l <- length(corpus) # l
	f <- sum(corpus==element) # f
	if(f==0) { return("NA"); break() }
	s <- corpus.part.sizes/l # s
	v <- rowsum(as.numeric(corpus==element), corpus.parts)

	values <- list()	
	values[["element"]] <- element
	values[["range"]] <- sum(v>0)
	values[["Deviation of proportions DP"]] <- sum(abs((v/f)-s))/2 # Gries
	values[["Deviation of proportions DP (normalized)"]] <- (sum(abs((v/f)-s))/2)/(1-min(s))

	return(values)
} # end defining function "dispersions1"

#######
# defines a function to test whether a number is a whole number (that is, w/o decimals)

is.wholenumber <-
    function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol

# end defining functions used later in the script

#######
# defines function to figure out how many grams to pull out
how.many.grams<-function(number.grams, len.grams) {
	if (number.grams == -1) {
		how.many.grams<-len.grams
	} else {
		how.many.grams<-number.grams
	}
	return(how.many.grams)

} # end defining function

##################
# this section saves temporary files to store words and numbers to be used later in the script

setwd(output.folder)

# single words
cat(file="single_words.txt", append=F)
cat(file="single_numbers.txt", append=F)

# bigrams
cat(file="bigram_words.txt", append=F)
cat(file="bigram_numbers.txt", append=F)

# trigrams
cat(file="trigram_words.txt", append=F)
cat(file="trigram_numbers.txt", append=F)

##################
# this section loops through the files, pulling out single words, bigrams, and trigrams from each file

corpus.files<-dir(input.folder, pattern="\\.(txt|TXT|Txt)", full.names=T)

cat("\nWorking on gathering words, bigrams, and trigrams from the files...\n")

# loop to load each corpus file
for (i in 1:length(corpus.files)) {
	#i<-1
	cur.name<-corpus.files[i]
	
	# prints a progress report
	cat("\tAdding file ", i, " of ",  length(corpus.files), ": ", basename(cur.name), "\n", sep="")
	
	# loads corpus file and makes it lower-case
	cur.data<-tolower(scan(cur.name, what="char", sep="\n", quiet=T, comment.char=""))
		
	# delete the annotations that aren't part of the speech of the speakers
	cur.data<-sub("^[^:]*:", "", cur.data, perl=T)
	cur.data<-gsub("\t", " ", cur.data, perl=T)
	
	# puts a period at the end of each line
	cur.data<-sub("$", "\\.", cur.data, perl=T)
	
	# splits up at non-word characters
	cur.words<-unlist(strsplit(cur.data, "[^-a-záéíóúñü\\']+", perl=T))	
	cur.words<-cur.words[nchar(cur.words)>0]

	# gets bigrams and trigrams
	cur.bigrams<-create.ngrams(cur.data, num.words=2)
	cur.trigrams<-create.ngrams(cur.data, num.words=3)
	
	######
	# saves words, bigrams, and trigrams and the number of each to the temp files
		
	# saves words
	cat(cur.words, file="single_words.txt", sep="\n", append=T)
	cat(length(cur.words), file="single_numbers.txt", sep="\n", append=T)
		
	# saves bigrams
	cat(cur.bigrams, file="bigram_words.txt", sep="\n", append=T)
	cat(length(cur.bigrams), file="bigram_numbers.txt", sep="\n", append=T)
	
	# saves trigrams
	cat(cur.trigrams, file="trigram_words.txt", sep="\n", append=T)
	cat(length(cur.trigrams), file="trigram_numbers.txt", sep="\n", append=T)
	
} # next corpus file

#############
# this section creates the single word frequency list 

cat("\nWorking on creating the frequency list of single words...\n")

# retrieves single words from temp file
single.data<-scan(file="single_words.txt", what="char", sep="\n", quiet=T)
single.numbers<-as.numeric(scan(file="single_numbers.txt", what="num", sep="\n", quiet=T))

# gets frequencies
raw.freq<-sort(table(single.data), decreasing=T)
log.freq<-round(log(raw.freq), 3)
norm.freq<-round((raw.freq/sum(raw.freq))*1000000, 3)

num.grams.to.extract<-how.many.grams(num.grams, length(raw.freq))

# saves output file
output.file<-"freq_list_single_words.txt"
cat("RANK\tWORD\tRAW.FREQ\tLOG.FREQ\tNORM.FREQ\tRANGE\tRANGE%\tDP\tDP.NORM\n", file=output.file)

all.range.dp<-numeric(0)

for (i in 1:num.grams.to.extract) {
	
	if (is.wholenumber(i/100)) {
		cat("\tGetting dispersion of word ", i, " of ", num.grams.to.extract, ": ",  names(raw.freq)[i], "\n", sep="")	
		# saves set of 100 ranges and dps to hard drive
		cat(all.range.dp, file=output.file, sep="\n", append=T)
		
		# empties out the range and dp collector vectors
		all.range.dp<-numeric(0)

	} # end of if this is a 100th iteration
	
	# gets range and Gries' DP dispersion measurement
	word.dispersion<-dispersions1(single.data, single.numbers, names(raw.freq)[i])
	cur.range<-word.dispersion$range
	cur.range.perc<-round((cur.range/length(corpus.files))*100, digits=2)
	cur.dp<-round(word.dispersion$'Deviation of proportions DP', 5)
	cur.dp.norm<-round(word.dispersion$'Deviation of proportions DP (normalized)', 5)

	output.data<-paste(i, names(raw.freq)[i], raw.freq[i], log.freq[i], norm.freq[i], cur.range, cur.range.perc, cur.dp, cur.dp.norm, sep="\t")

	all.range.dp<-append(all.range.dp, output.data)

	# if last iteration, saves the collector vector to hard drive
	if (i==num.grams.to.extract) {
		cat(all.range.dp, file=output.file, sep="\n", append=T)

	} # end of if last iteration

} # next word

unlink("single_words.txt")
unlink("single_numbers.txt")

cat("\nFinished creating the frequency list of single words:\n", paste(output.folder, output.file, sep="/"), "\n", sep="")

#############
# this section creates the bigram word frequency list 

cat("\nWorking on creating the frequency list of bigrams...\n")

# retrieves bigram words from temp file
bigram.data<-scan(file="bigram_words.txt", what="char", sep="\n", quiet=T)
bigram.numbers<-as.numeric(scan(file="bigram_numbers.txt", what="num", sep="\n", quiet=T))

# gets frequencies
raw.freq<-sort(table(bigram.data), decreasing=T)
log.freq<-round(log(raw.freq), 3)
norm.freq<-round((raw.freq/sum(raw.freq))*1000000, 3)

# determines how many grams to extract
num.grams.to.extract<-how.many.grams(num.grams, length(raw.freq))

# saves output file
output.file<-"freq_list_bigrams.txt"
cat("RANK\tBIGRAM\tRAW.FREQ\tLOG.FREQ\tNORM.FREQ\tRANGE\tRANGE%\tDP\tDP.NORM\n", file=output.file)

all.range.dp<-numeric(0)

for (i in 1:num.grams.to.extract) {
	
	if (is.wholenumber(i/100)) {
		cat("\tGetting dispersion of bigram ", i, " of ", num.grams.to.extract, ": ",  names(raw.freq)[i], "\n", sep="")	
		
		# saves set of 100 ranges and dps to hard drive
		cat(all.range.dp, file=output.file, sep="\n", append=T)
		
		# empties out the range and dp collector vectors
		all.range.dp<-numeric(0)

	} # end of if this is a 100th iteration
	
	# gets range and Gries' DP dispersion measurement
	word.dispersion<-dispersions1(bigram.data, bigram.numbers, names(raw.freq)[i])
	cur.range<-word.dispersion$range
	cur.range.perc<-round((cur.range/length(corpus.files))*100, 2)
	cur.dp<-round(word.dispersion$'Deviation of proportions DP', 5)
	cur.dp.norm<-round(word.dispersion$'Deviation of proportions DP (normalized)', 5)

	output.data<-paste(i, names(raw.freq)[i], raw.freq[i], log.freq[i], norm.freq[i], cur.range, cur.range.perc, cur.dp, cur.dp.norm, sep="\t")

	all.range.dp<-append(all.range.dp, output.data)

	# if last iteration, saves the collector vector to hard drive
	if (i==num.grams.to.extract) {
		cat(all.range.dp, file=output.file, sep="\n", append=T)

	} # end of if last iteration

} # next word

unlink("bigram_words.txt")
unlink("bigram_numbers.txt")

cat("\nFinished creating the frequency list of bigram words:\n", paste(output.folder, output.file, sep="/"), "\n", sep="")

#############
# this section creates the trigram word frequency list 

cat("\nWorking on creating the frequency list of trigrams...\n")

# retrieves trigram words from temp file
trigram.data<-scan(file="trigram_words.txt", what="char", sep="\n", quiet=T)
trigram.numbers<-as.numeric(scan(file="trigram_numbers.txt", what="num", sep="\n", quiet=T))

# gets frequencies
raw.freq<-sort(table(trigram.data), decreasing=T)
log.freq<-round(log(raw.freq), 3)
norm.freq<-round((raw.freq/sum(raw.freq))*1000000, 3)

# determines how many grams to extract
num.grams.to.extract<-how.many.grams(num.grams, length(raw.freq))

# saves output file
output.file<-"freq_list_trigrams.txt"
cat("RANK\tTRIGRAM\tRAW.FREQ\tLOG.FREQ\tNORM.FREQ\tRANGE\tRANGE%\tDP\tDP.NORM\n", file=output.file)

all.range.dp<-numeric(0)

for (i in 1:num.grams.to.extract) {
	
	if (is.wholenumber(i/100)) {
		cat("\tGetting dispersion of trigram ", i, " of ", num.grams.to.extract, ": ",  names(raw.freq)[i], "\n", sep="")	
		
		# saves set of 100 ranges and dps to hard drive
		cat(all.range.dp, file=output.file, sep="\n", append=T)
		
		# empties out the range and dp collector vectors
		all.range.dp<-numeric(0)

	} # end of if this is a 100th iteration
	
	# gets range and Gries' DP dispersion measurement
	word.dispersion<-dispersions1(trigram.data, trigram.numbers, names(raw.freq)[i])
	cur.range<-word.dispersion$range
	cur.range.perc<-round((cur.range/length(corpus.files))*100, 2)
	cur.dp<-round(word.dispersion$'Deviation of proportions DP', 5)
	cur.dp.norm<-round(word.dispersion$'Deviation of proportions DP (normalized)', 5)

	output.data<-paste(i, names(raw.freq)[i], raw.freq[i], log.freq[i], norm.freq[i], cur.range, cur.range.perc, cur.dp, cur.dp.norm, sep="\t")

	all.range.dp<-append(all.range.dp, output.data)

	# if last iteration, saves the collector vector to hard drive
	if (i==num.grams.to.extract) {
		cat(all.range.dp, file=output.file, sep="\n", append=T)

	} # end of if last iteration

} # next word

unlink("trigram_words.txt")
unlink("trigram_numbers.txt")

cat("\nFinished creating the frequency list of trigram words:\n", paste(output.folder, output.file, sep="/"), "\n", sep="")

cat("\a\nAll done!\nThree frequency lists have been created and are located in the folder:\n", output.folder, "\n\nIf you use the DP or DP normalized measurements, please cite:\nGries, Stefan Th. 2008. Dispersions and adjusted frequencies in corpora. /International Journal of Corpus Linguistics/ 13(4):403-437.", sep="")
