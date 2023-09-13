# ============================= #
# ======   TEXT MINING   ====== #
# ============================= #

get_raw_text = function(df) {
  # df is a data frame like the one obtained from get_music_df
  # Returns character containing the lyrics.
  raw_text = ""
  for (i in 1:nrow(df)) {
    if ((df$text[i] == "") | (df$extend_type[i] == "stop"))
      next
    if ((df$syllabic[i] == "single") |
        (df$syllabic[i] == "begin")) {
      raw_text = paste(raw_text, df$text[i])
      next
    }
    raw_text = paste0(raw_text, df$text[i])
  }
  substr(raw_text, 2, str_length(raw_text))
}

export_lyrics = function(xmls) {
  # Exports the lyrics from every xml file placed in xmls, a character vector
  # containing the paths to every MusicXML we're interested in.
  # The default path is ./Lyrics/, where . is the working directory.
  # Keeps track of which xml is currently being parsed by printing its position
  # Be aware of the memory leak bug, do not parse more than 60-75 XML files
  # per GB of available RAM!!
  setwd(here::here("Lyrics"))
  i = 1
  
  while (i <= length(xmls)) {
    aria = xmlParse(xmls[i])
    root_node = xmlRoot(aria)
    name = aria_name(root_node)
    
    year = get_year(root_node)
    
    identification_node = xmlElementsByTagName(root_node, "identification")[[1]]
    composer = str_remove_all(getChildrenStrings(identification_node)["creator"],
                              "\n")
    
    parts = Filter(is_part, xmlChildren(root_node))
    
    part_list_subnode = xmlElementsByTagName(root_node, "part-list")[[1]]
    score_part_subnodes = xmlElementsByTagName(part_list_subnode, "score-part")
    
    instruments = lapply(score_part_subnodes, get_instrument)
    
    l = length(which_voice(instruments))
    
    free(aria)
    rm(aria,
       root_node,
       part_list_subnode,
       score_part_subnodes,
       identification_node)
    gc()
    
    for (l in which_voice(instruments)) {
      voice_part = parts[[l]]
      
      notes = xmlElementsByTagName(voice_part, "note", recursive = TRUE)
      
      attributes = xmlElementsByTagName(voice_part, "attributes", recursive = T)[[1]]
      
      time1 <<- get_time(attributes)
      
      rm(voice_part, attributes)
      
      aria_df = get_music_df(notes)
      rm(notes)
      gc()
      
      identifier = aria_identifier(root_node, voice_name(instruments, l))
      
      write_csv(
        aria_df,
        file = paste0(identifier, "-df.csv")
      )
    }
    rm(aria_df, name, instruments)
    gc()
    print(i)
    i = i + 1
  }
}

remove_apostrophe = function(x) {
  # removes curly apostrophe, which is U+2019
  str_replace_all(x, "\u2019", " ")
}

remove_apostrophe_no_space = function(x) {
  # removes curly apostrophe, doesn't leave a blank space
  str_remove_all(x, "\u2019")
}

remove_ellipsis = function(x) {
  # removes single character ellipsis ..., which is U+2026
  str_replace_all(x, "\u2026", " ")
}

remove_quotations = function(x) {
  # removes double quotations " (both left and right), U+201C and U+201D
  str_replace_all(str_replace_all(x, "\u201C", " "), "\u201D", " ")
}

lemmatizer = function(chr_string, dict) {
  # This function is slower than ideal
  # Probably the definition will change in the future, although nothing should
  # break since output will be the same, just a faster implementation.
  
  # chr_string is the string where we want to lemmatize
  # dict is a dictionary, where dict["word"] gives us the corresponding stem
  unique_words = unique(str_split(chr_string, " ")[[1]]) # list of unique words
  chr_string = paste("", chr_string, "")
  for (indiv_word in unique_words) {
    repl = dict[indiv_word]
    if (is.na(repl))
      next
    n_appearances = sum(str_count(chr_string, indiv_word))
    while (n_appearances > 0) {
      chr_string = str_replace(chr_string, paste("", indiv_word, ""), 
                  paste("", repl, ""))
      n_appearances = n_appearances - 1
    }
  }
  str_trim(chr_string)
}

lemmatizer_old = function(chr_string, dict) {
  # DEPRECATED
  
  # This function is deprecated because it doesn't quite work correctly
  # It doesn't account for blank spaces, just replaces substrings.
  # However it is very fast compared to the current one. It is still supported
  # in case some old implementation used it, but its output is sometimes wrong.
  
  # chr_string is the string which we want to lemmatize
  # dict is a dictionary, where dict["word"] gives us the corresponding stem
  unique_words = unique(str_split(chr_string, " ")[[1]]) # list of unique words
  for (word in unique_words) {
    repl = dict[word]
    if (is.na(repl))
      next
    chr_string = gsub(pattern = word,
                      replacement = dict[word],
                      x = chr_string)
  }
  chr_string
}