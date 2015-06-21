# Script to search for a list of words in a folder of text files

# (c) 2015 Earl K. Brown ekbrown@ksu.edu
# This script uses Stefan Th. Gries' function exact.matches, available at: http://www.linguistics.ucsb.edu/faculty/stgries/exact_matches.r
# This script can be redistributed and modified for non-profit purposes

# clears the working memory
rm(list=ls(all=T))

#############################################
# SPECIFY THE DIRECTORY WITH THE TEXTS
input.folder<-"/pathway/to/directory/with/texts"

# SPECIFY THE FILE (NOT THE DIRECTORY) INTO WHICH THE SEARCH RESULTS SHOULD BE SAVED
output.file<-"/pathway/to/file/search_results.txt"

# SPECIFY THE WORDS TO BE SEARCHED FOR
search.words<-c("have to", "got to", "need to", "should", "must", "\\'ve")
#############################################

# gets function to search for words and tabulate results
# Stefan Th. Gries' function "exact.matches"
exact.matches <- function (search.expression, corpus.vector, pcre=TRUE, case.sens=TRUE, characters.around=0, lines.around=0, clean.up.spaces=TRUE) {
   # Thanks to Earl Brown for feedback on an earlier version
   if (characters.around!=0 & lines.around!=0) {
      stop("At least one of 'characters.around' and 'lines.around' has to be zero ...")
   }
   line.numbers.with.matches <- grep(search.expression, corpus.vector, perl=pcre, value=FALSE, ignore.case=!case.sens) # the numbers of lines that contain matches
   if (any(line.numbers.with.matches)) { # if there are matches ...
      if (characters.around!=0) {
         lines.with.matches <- gsub("( ?_qW1aS3zX5eR7dF9cV_|_qW1aS3zX5eR7dF9cV_ ?)",
                                    " ",
                                    paste(corpus.vector, collapse = "_qW1aS3zX5eR7dF9cV_"),
                                    perl=TRUE)
      } else {
         lines.with.matches <- corpus.vector[line.numbers.with.matches] # the lines that contain matches
      }
      matches <- gregexpr(search.expression, lines.with.matches, perl = pcre, ignore.case = !case.sens) # the start positions and lengths of matches
      number.of.matches <- sapply(matches, length) # the number of matches per line (of the lines that have matches)
      lines <- rep(lines.with.matches, number.of.matches) # the lines with matches, each as many times as it has matches
      line.numbers.with.matches <- rep(line.numbers.with.matches, number.of.matches)
      starts <- unlist(matches) # starting positions of matches
      stops <- starts + unlist(sapply(matches, attr, "match.length")) - 1 # end positions of matches
      exact.string.matches <- substr(lines, starts, stops) # the exact matches
      lines.with.delimited.matches <- paste( # the lines with the tab-delimited matches
         substr(lines, if (characters.around!=0) starts-characters.around else 1, starts-1), "\t", # preceding contexts
         exact.string.matches, "\t", # matches
         substr(lines, stops+1, if (characters.around!=0) stops+characters.around else nchar(lines)), # subsequent contexts
         sep="")
      
      if (lines.around!=0) {
         corpus.vector <- append(corpus.vector, rep("", lines.around))
         starts.of.previous.lines <- pmax(0, line.numbers.with.matches - lines.around)
         ends.of.subsequent.lines <- pmin(line.numbers.with.matches + lines.around, length(corpus.vector))
         for (current.line.with.delimited.match in seq(lines.with.delimited.matches)) {
            lines.with.delimited.matches[current.line.with.delimited.match] <- paste(
               paste(corpus.vector[starts.of.previous.lines[current.line.with.delimited.match]:(line.numbers.with.matches[current.line.with.delimited.match]-1)], collapse=" "),
               lines.with.delimited.matches[current.line.with.delimited.match],
               paste(corpus.vector[(line.numbers.with.matches[current.line.with.delimited.match]+1):ends.of.subsequent.lines[current.line.with.delimited.match]], collapse=" "),
               sep=" ")
         }
      }
      
      # cleaning output as necessary/requested by user
      if (clean.up.spaces) { # clean up spaces around tabs
         lines.with.delimited.matches <- gsub(" *\t *", "\t", lines.with.delimited.matches, perl=TRUE)
      }
      lines.with.delimited.matches <- gsub("(^ {1,}| {1,}$)", "", lines.with.delimited.matches, perl=TRUE) # clean up leading and trailing spaces
      output.list <- list(exact.string.matches,
                          if (characters.around!=0) starts else line.numbers.with.matches, # starting character positions or the numbers of lines with matches, each as many times as it has matches
                          length(unique(line.numbers.with.matches))/sum(nzchar(corpus.vector)),
                          lines.with.delimited.matches,
                          c(Pattern = search.expression, "Corpus (1st 100 char.)"=substr(paste(corpus.vector, collapse=" "), 1, 100), PCRE=pcre, "Case-sensitive"=case.sens),
                          "1.2 (17 August 2012)")
      names(output.list) <- c("Exact matches",
                              paste("Locations of matches (", ifelse (characters.around!=0, "characters", "lines"), ")", sep=""),
                              "Proportion of non-empty corpus parts with matches",
                              "Lines with delimited matches",
                              "Search parameters",
                              "Version (date)")
      return(output.list)
   }
}

# end of defining function exact.matches
###########

# gets the names of the corpus files
file.names<-dir(input.folder, pattern="\\.(TXT|txt|Txt)", full.names=T)

# prepares the search words
search.words2<-character(0)
for (i in search.words) {
	apos<-grep("^\\\\'", i, perl=T)
	if (length(apos)>0) {
		cur_search_word<-paste(i, "\\b", sep="")
	} else {
		cur_search_word<-paste("\\b", i, "\\b", sep="")
	} # end of if there is apostrophe
	search.words2<-append(search.words2, cur_search_word)
} # next search word

search.words2<-paste(search.words2, collapse="|")
search.words2<-sub("^", "(", search.words2)
search.words2<-sub("$", ")", search.words2)

# creates empty variable to collect all hits
all.hits<-character(0)

# loops through corpus files
for (i in file.names) {
	
	# prints progress report to the screen
	cat("Working on: ", basename(i), "\n", sep="")
	
	# loads current corpus file
	cur.file<-scan(file=i, what="char", sep="\n", quiet=T)

	# replaces tabs with a space
	cur.file<-gsub("\t", "  ", cur.file, perl=T)

	# searches current file for search words
	cur.hits<-exact.matches(search.words2, cur.file, case.sens=F, lines.around=2)
	
	if (is.null(cur.hits)) next	
	
	cur.hits<-paste(basename(i), cur.hits[[2]], cur.hits[[4]], sep="\t")
	
	# add hits from current file to all hits
	all.hits<-append(all.hits, cur.hits)
	
} # next corpus file

# saves all hits to output file
cat("FILE\tPRGH\tPRE\tHIT\tFOL", all.hits, file=output.file, sep="\n")

# announces finish
cat("\a\nAll done! The search has finished and the output file is:\n", output.file, "\n", sep="")
