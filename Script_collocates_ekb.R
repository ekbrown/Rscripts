# Script to retrieve the collocates of a list of words in a folder of text files

# (c) 2015 Earl K. Brown ekbrown@ksu.edu
# A lot of the code was written by Stefan Th. Gries
# This script can be redistributed and modified for non-profit purposes

# clears working memory
rm(list=ls(all=T))

#############################################
# SPECIFY THE DIRECTORY WITH THE TEXTS
input.folder <- "/pathway/to/directory/with/texts"

# SPECIFY THE DIRECTORY IN WHICH THE COLLOCATE FILES SHOULD BE SAVED
output.folder <- "/pathway/to/directory/where/files/should/be/saved"

# SPECIFY THE WORDS WHOSE COLLOCATES SHOULD BE SEARCH FOR
search.terms<-c("need to", "have to", "must", "high school", "libro", "capstone", "antes de que")

# SPECIFY THE NUMBER OF SURROUNDING COLLOCATES
sur.words<-3
#############################################

# defines prepare.term function
prepare.term<-function(raw.term) {
	apos<-grep("^\\\\'", raw.term, perl=T)
	if (length(apos)>0) {
		prepared.term<-paste(raw.term, "\\b", sep="")
	} else {
		prepared.term<-paste("\\b", raw.term, "\\b", sep="")
	} # end of if there is apostrophe

	return(prepared.term)
	
} # end defining prepare.term function

#########

setwd(output.folder)
span<-(-sur.words:sur.words)

# saves temp file
cat(file="single_words.txt", append=F)

#########
# this section loops through the files, pulling out single words from each file

corpus.files<-dir(input.folder, pattern="\\.(txt|TXT|Txt)", full.names=T)

cat("\nWorking on gathering words from the files...\n")

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
	
	######
	# saves words to the temp file
	cat(cur.words, file="single_words.txt", sep="\n", append=T)
	
} # next corpus file

#####################

# retrieves single words from temp file
corpus.file<-scan(file="single_words.txt", what="char", sep="\n", quiet=T)

cat("\nWorking on getting collocates...\n")

for (i in search.terms) {
	#i<-search.terms[4]
	cat("\tWorking on: ", i, "\n", sep="")
	cur.file.name<-gsub("(\\'|\\\\)", "", i, perl=T)
	
	#i<-"have to"
	
	# breaks up the search term into individual words and gets the number of words
	ind.search.terms<-unlist(strsplit(i, "\\s+"))
	num.search.terms<-length(ind.search.terms)
	
	#########
	# if there's only one search term	
	if (num.search.terms == 1) {
		
		output.file<-paste(output.folder, "/collocates_", cur.file.name, ".txt", sep="")
		
		# puts word boundary marker around current search term
		cur.term<-prepare.term(i)
	
		#find the position of the elements with the search word
		positions.of.matches<-grep(cur.term, corpus.file, perl=T)
		
		if (length(positions.of.matches)<1) {
			cat("\t\tThe search term \"", i, "\" was not found\n", sep="")
			next
		}
				
		results<-list()
		for (j in 1:length(span)) {
			collocate.positions<-positions.of.matches+span[j]
			collocates<-corpus.file[collocate.positions]
			sorted.collocates<-sort(table(collocates), decreasing=T)
			results[[j]]<-sorted.collocates
		} # next collocate position
		
		lengths<-sapply(results, length)
	
		cat(paste(rep(c("W_", "F_"), length(span)), rep(span, each=2), sep=""), "\n", sep="\t", file=output.file)
		
		for (j in 1:max(lengths)) {
			output.string<-paste(names(sapply(results, "[", j)), sapply(results, "[", j), sep="\t")
			output.string<-gsub("NA\tNA", "\t", output.string, perl=T)
			cat(output.string, "\n", sep="\t", file=output.file, append=T)
		} # next collocate
	
	} # end of if there is only one search term
	
	#########
	# if there are two search terms
	if (num.search.terms == 2) {
		
		output.file<-paste(output.folder, "/collocates_", sub("\\s+", "_", cur.file.name, perl=T), ".txt", sep="")
		
		first.term<-prepare.term(ind.search.terms[1])
		second.term<-prepare.term(ind.search.terms[2])
		
		# finds positions of search terms
		positions.first.term<-grep(first.term, corpus.file, perl=T)
		positions.second.term<-grep(second.term, corpus.file, perl=T)

		# determines where both words occur next to each other
		positions.both.terms<-positions.first.term[(positions.first.term+1) %in% positions.second.term]

		if (length(positions.both.terms)<1) {
			cat("\t\tThe search term \"", i, "\" was not found\n", sep="")
			next
		}

		results<-list()
		for (j in 1:length(span)) {
			#j<-4
			offset<-span[j]
			collocate.positions<-positions.both.terms+offset
			
			if (offset < 0) {
				collocate.collector<-corpus.file[collocate.positions]
			}
			
			if (j == median(1:length(span)) && length(collocate.positions) > 1)  {
				collocate.collector<-character()
				for (k in collocate.positions) {
					first.collocate<-corpus.file[k]
					second.collocate<-corpus.file[k+1]
					collocate.collector<-append(collocate.collector, paste(first.collocate, second.collocate, sep=" "))
				} # next collocate position
			} # end of if more than one collocate position

			if (offset > 0) {
				collocate.collector<-corpus.file[collocate.positions+1]				
			}

			#collocates<-corpus.file[collocate.positions]
			sorted.collocates<-sort(table(collocate.collector), decreasing=T)
			results[[j]]<-sorted.collocates
		} # next collocate position

		lengths<-sapply(results, length)
	
		cat(paste(rep(c("W_", "F_"), length(span)), rep(span, each=2), sep=""), "\n", sep="\t", file=output.file)
		
		for (j in 1:max(lengths)) {
			output.string<-paste(names(sapply(results, "[", j)), sapply(results, "[", j), sep="\t")
			output.string<-gsub("NA\tNA", "\t", output.string, perl=T)
			cat(output.string, "\n", sep="\t", file=output.file, append=T)
		} # next collocate

	} # end of if there are two search terms
		
	#########
	# if there are three search terms
	if (num.search.terms == 3) {
		
		output.file<-paste(output.folder, "/collocates_", gsub("\\s+", "_", cur.file.name, perl=T), ".txt", sep="")
		
		first.term<-prepare.term(ind.search.terms[1])
		second.term<-prepare.term(ind.search.terms[2])
		third.term<-prepare.term(ind.search.terms[3])
		
		# finds positions of search terms
		positions.first.term<-grep(first.term, corpus.file, perl=T)
		positions.second.term<-grep(second.term, corpus.file, perl=T)
		positions.third.term<-grep(third.term, corpus.file, perl=T)

		# determines where all three terms occur next to each other
		positions.three.terms<-positions.first.term[((positions.first.term+1) %in% positions.second.term) & ((positions.first.term+2) %in% positions.third.term)]

		if (length(positions.three.terms)<1) {
			cat("\t\tThe search term \"", i, "\" was not found\n", sep="")
			next
		}

		results<-list()
		for (j in 1:length(span)) {
			#j<-4
			offset<-span[j]
			collocate.positions<-positions.three.terms+offset
			
			if (offset < 0) {
				collocate.collector<-corpus.file[collocate.positions]
			}
			
			if (j == median(1:length(span)) && length(collocate.positions) > 1)  {
				collocate.collector<-character()
				for (k in collocate.positions) {
					first.collocate<-corpus.file[k]
					second.collocate<-corpus.file[k+1]
					third.collocate<-corpus.file[k+2]
					collocate.collector<-append(collocate.collector, paste(first.collocate, second.collocate, third.collocate, sep=" "))
				} # next collocate position
			} # end of if more than one collocate position

			if (offset > 0) {
				collocate.collector<-corpus.file[collocate.positions+2]
			}

			#collocates<-corpus.file[collocate.positions]
			sorted.collocates<-sort(table(collocate.collector), decreasing=T)
			results[[j]]<-sorted.collocates
		} # next collocate position

		lengths<-sapply(results, length)
	
		cat(paste(rep(c("W_", "F_"), length(span)), rep(span, each=2), sep=""), "\n", sep="\t", file=output.file)
		
		for (j in 1:max(lengths)) {
			output.string<-paste(names(sapply(results, "[", j)), sapply(results, "[", j), sep="\t")
			output.string<-gsub("NA\tNA", "\t", output.string, perl=T)
			cat(output.string, "\n", sep="\t", file=output.file, append=T)
		} # next collocate
		
	} # end of if there are three search terms
	
	#########
	# if there are more than three search terms
	if (num.search.terms > 3) stop("Each search term must contain three words or fewer. The search term \"", i, "\" has more than three.\n")
		
} # next search term

# cleans up
unlink("single_words.txt")

cat("\a\nDone!\nThe collocate files are in:\n", output.folder, sep="")