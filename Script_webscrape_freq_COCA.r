# R script to webscrape the frequency of words in the Corpus of Contemporary American English
# Earl Kjar Brown, ekbrown byu edu (add appropriate characters to create email)

library("rvest")
library("httr")
library("tidyverse")

get_freq_coca <- function(email, password, section, wds, outfile) {
    
    # param email: string, with email with which the user accesses the Davies corpus;
    # param password: string, with associated password (double check with web browser);
    # param section: string, the section in the COCA to get frequencies from, from among 'all', 'spok', 'fict', 'mag', 'news', 'acad';
    # param wds: vector of strings, with words to retrieve the frequencies of;
    # param outfile: string, the pathway to the CSV file to write frequencies to;
    # return value: string, simple message to user "All done!".

    # validate data from user about the section to search for frequencies in
    if (!section %in% c('all', 'spok', 'fict', 'mag', 'news', 'acad')) {
        stop(str_interp("You must specify 'section' as one of 'all', 'spok', 'fict', 'mag', 'news', 'acad', but you specified '${section}'!"))
    } else {
        cat(str_interp("wd,freq_${section}"), file = outfile, sep = "\n")
        
        section <- switch(section,
                          all = 0,
                          spok = 1,
                          fict = 2,
                          mag = 3,
                          news = 4,
                          acad = 5)
    }
    
    # log in to the corpus    
    r <- GET(str_c("https://www.english-corpora.org/coca/login.asp?email=", URLencode(email), "&password=", URLencode(password), "&B1=Log+in&e="), add_headers('user-agent' = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_14_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/79.0.3945.130 Safari/537.36'))
    
    # loop over words
    for (wd in wds) {
        
        cat(str_interp("Working on: ${wd}\n"))
        
        # create list to hold the parameters and their corresponding values that will be passed in with the POST request
        to_body = list(
            'chooser' = 'seq',
            'allcoll' = 'n',
            'ignoreHighFreq' = 'n',
            'isVC' = 'n',
            'whatdo' = '',
            'whatdo1' = '',
            'showHelp' = '',
            'wl' = '4',
            'wr' = '4',
            'w1a' = '',
            'p' = wd,
            'posDropdown' = 'Insert+PoS',
            'w1b' = '',
            'posWord2Dropdown' = 'Insert+PoS',
            'w2' = '',
            'posColDropdown' = 'Insert+PoS',
            'submit1' = 'Find+matching+strings',
            'sec1' = as.character(section),
            'sec2' = '0',
            'sortBy' = 'freq',
            'sortByDo2' = 'freq',
            'minfreq1' = 'freq',
            'freq1' = '10',
            'freq2' = '0',
            'numhits' = '100',
            'kh' = '200',
            'groupBy' = 'words',
            'whatshow' = 'raw',
            'saveList' = 'no',
            'ownsearch' = 'y',
            'changed' = '',
            'word' = '',
            'sbs' = '',
            'sbs1' = '',
            'sbsreg1' = '',
            'sbsr' = '',
            'sbsgroup' = '',
            'redidID' = '',
            'compared' = '',
            'holder' = '',
            'waited' = 'y',
            'user' = '',
            's1' = '0',
            's2' = '0',
            's3' = '0',
            'perc' = 'mi',
            'r1' = '',
            'r2' = '',
            'didRandom' = 'y'
        )
        
        # headers for request
        to_headers <- add_headers(
            'origin' = 'https://www.english-corpora.org',
            'referer' =  'https://www.english-corpora.org/coca/x1.asp?w=1920&h=1080&c=coca',
            'user-agent' = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_14_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/79.0.3945.130 Safari/537.36'
        )
        
        # make request for current word
        freq <- POST(url = "https://www.english-corpora.org/coca/x2.asp", query = to_body, config = to_headers)
        
        # find the frequency in the response's HTML
        freq <- read_html(freq) %>% 
            html_nodes(xpath = '//font[@color="#003399"]/text()') %>% 
            html_text() %>% 
            str_trim()
        if (length(freq) != 1) {
            freq <- 0
        }
        
        # write out current word and frequency to the outfile
        cat(str_interp("${wd},${freq}"), file = outfile, sep = "\n", append = T)
        
    }  # next word
    return("All done!")
}  # end function definition


### TEST THE FUNCTION ###
email = "YOUR_EMAIL"  # your email to access Davies corpora
password = "YOUR_PASSWORD"  # and associated password
section <-  "all"  # from 'all', 'spok', 'fict', 'mag', 'news', 'acad'
wds <- c("apple", "banana", "mango", "orange", "fdsa")
outfile <- "/pathway/to/dir/freq_coca.csv"

# save the script and run it
get_freq_coca(email, password, section, wds, outfile)
