# ============================ #
# ======   DATA FRAME   ====== #
# ============================ #

# In this source file several functions will be defined, whose ultimate goal is
# to extract information from a MusicXML file.

aria_name = function(root_node) {
  # gets the aria name
  str_trim(str_remove_all(getChildrenStrings(root_node)["movement-title"], "\\?"))
}

aria_composer = function(identification_node) {
  # gets the composer
  str_trim(getChildrenStrings(identification_node)["creator"])
}

aria_lyricist = function(identification_node) {
  # gets the lyricist
  getChildrenStrings(identification_node)[-which(aria_composer(identification_node) ==
                                                   getChildrenStrings(identification_node))]["creator"]
}

get_didone_code = function(xml_source) {
  # gets the DIDONE code for xml_source file.
  w = str_replace_all(word(str_replace_all(xml_source, "/", " "),-1), "-", " ")
  if (str_count(w, " ") == 3) {
    return(word(w, 1))
  }
  else {
    return(paste(word(w, c(1, 2)), collapse = "-"))
  }
}

is_part = function(node) {
  # node is a subnode of a root node of aria
  # Bool that stores whether node is a <part> node or not
  xmlName(node) == "part"
}

get_year = function(root_node) {
  # gets the year
  credit_list =
    lapply(
      xmlElementsByTagName(xmlElementsByTagName(root_node,
                                                "credit")[[2]],
                           "credit-words"),
      getChildrenStrings
    )
  
  year = credit_list[unlist(lapply(credit_list, (\(x) str_length(x) == 6)))]
  
  year = Filter(\(x) x != "Indie ", year)
  
  year = Filter(\(x) str_detect(x, "1"), year)
  
  if (length(year) == 0)
    return("Unknown")
  
  year = year[[1]]
  
  year = as.numeric(str_sub(year, start = 2, end = 5))
  
  if (is.na(year)) {
    return("Unkwnown")
  }
  year
}

get_instrument = function(score_part_subnode) {
  # score_part_subnode is a subnode of the form <score-part>
  # Gets the instrument sound according to what the MusicXML says
  score_instrument = xmlElementsByTagName(score_part_subnode,
                                          "score-instrument")[[1]]
  getChildrenStrings(score_instrument)["instrument-sound"]
}

is_anacrusis = function(voice_part) {
  # bool that stores whether there is anacrusis in voice_part or not
  xmlGetAttr(xmlElementsByTagName(voice_part, "measure")[[1]],
             "number") == "0"
}

which_voice = function(instruments) {
  # vector of positions of voices
  # instruments is a list of all the instruments
  which(
    instruments == "voice.soprano" |
      instruments == "voice.alto" |
      instruments == "voice.tenor" |
      instruments == "voice.bass" |
      instruments == "voice.baritone"
  )
}

get_key = function(key_node) {
  # from a node <key> gets a pair of vectors of the form (key, mode)
  c(getChildrenStrings(key_node)["fifths"],
    getChildrenStrings(key_node)["mode"])
}

get_time = function(attributes) {
  # from a node <attributes> gets a vector of the form (beats, beat-unit, divisions)
  # where for example a 3/4 measure would have beats = 3, beat-unit = 4. Division
  # is dependent on each aria, and is a little arbitrary
  time1 = as.numeric(getChildrenStrings(xmlElementsByTagName(attributes, "time")[[1]]))
  time1[3] = as.numeric(getChildrenStrings(attributes)["divisions"])
  time1
}

is_rest_note = function(note) {
  # note is an XMLNode.
  # Returns if note is a rest note (TRUE if it is).
  length(xmlElementsByTagName(note, "rest")) > 0
}

get_pitch = function(note) {
  # Returns a string with the pitch of XMLNode note.
  # note must be an XMLnode with tag <pitch>.
  if (is_rest_note(note)) {
    return(c("pitch" = NA))
  }
  getChildrenStrings(note)["pitch"]
}

sep_pitch = function(pitch) {
  # From string pitch, obtains a vector of the form c(note, octave).
  # pitch must be a string like 'C3', 'B-13' (Bb 3), 'F+14' (F# 4)...
  # We use # for sharps and - for flats
  
  if (is.na(pitch)) {
    return(c("note" = NA, "octave" = NA))
  }
  names(pitch) = NULL
  if (str_detect(pitch, "-")) {
    return(c(
      "note" = paste0(substr(pitch, 1, 1), "-"),
      "octave" = substr(pitch, 4, 4)
    ))
  }
  else if (str_length(pitch) == 3) {
    return(c(
      "note" = paste0(substr(pitch, 1, 1), "#"),
      "octave" = substr(pitch, 3, 3)
    ))
  }
  else
    return(c(
      "note" = substr(pitch, 1, 1),
      "octave" = substr(pitch, 2, 2)
    ))
}

numeric_duration_to_words = # transforms numeric duration to words
  c(
    "16th",
    "eighth",
    "eighth.",
    "quarter",
    "",
    "quarter.",
    "quarter..",
    "half",
    "",
    "",
    "",
    "half.",
    "",
    "half..",
    "half...",
    "whole",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "whole.",
    "",
    "",
    "",
    "whole...",
    "",
    "whole....",
    "",
    "breve"
  )

get_type = function(note) {
  # Returns a string with the type of XMLNode note.
  # note is an XMLNode with (maybe) tag <type>,
  # time1 is a vector of the form obtained above, we need time1 for this function
  # time1 must be stored in the global environment
  if (is_rest_note(note) &
      is.na(getChildrenStrings(note)["type"])) {
    real_dur = as.numeric(getChildrenStrings(note)["duration"]) * time1[2] /
      time1[3]
    return(c("type" = numeric_duration_to_words[real_dur]))
  }
  else
    getChildrenStrings(note)["type"]
}

number_dots = function(note) {
  # Returns the number of dots XMLNode note (a <note> node) has
  sum(names(getChildrenStrings(note)) == "dot")
}

add_dots = function(notes, dots) {
  # Adds as many dots to type of each note in list of notes "notes", as the
  # corresponding entry in list dots. For example, if there is a dot,
  # on a quarter note, the type returned would be "quarter.". Max 2 dots supported
  types = lapply(notes, get_type)
  for (i in 1:length(notes)) {
    if (dots[[i]] == 1) {
      types[[i]] = c("type" = paste0(types[[i]], "."))
    }
    else if (dots[[i]] == 2) {
      types[[i]] = c("type" = paste0(types[[i]], ".."))
    }
  }
  types
}

get_lyric = function(note) {
  # From <lyric> inside XMLNode note, gets a list of vectors of the form
  # c(syllabic, text, extend = 0), or c(syllabic, text, extend = 1, type = 'start'),
  # or c(extend = 1, type = 'stop').
  # If there is no tie, extend = 0. If there is, extend = 1, type = 'start'
  # or 'stop'. List of as many vectors as syllabi in the same note (elisions+1).
  
  if (is_rest_note(note)) {
    return(list(c(
      "syllabic" = "none",
      "text" = "",
      "extend" = 0
    )))
  }
  
  if (length(xmlElementsByTagName(note, "lyric", recursive = T)) == 0) {
    return(list(""))
  } # If there's no lyric
  else{
    lyric_node = xmlElementsByTagName(note, "lyric")[[1]]
  }
  lyric_vector = getChildrenStrings(lyric_node)
  num_syl = sum(names(lyric_vector) == "elision") + 1
  tie_bool = sum(names(lyric_vector) == "extend") > 0 # Is there tie or not
  if (tie_bool) {
    # there is tie
    if (num_syl > 1) {
      # >1 syllab
      extend = xmlElementsByTagName(lyric_node, "extend", recursive = T)[[1]]
      tie_type = xmlGetAttr(extend, "type") # Start or stop
      list_lyric = list()
      if (tie_type == "start") {
        for (syllabus in 1:(num_syl - 1)) {
          list_lyric[[syllabus]] = c(
            lyric_vector["syllabic"],
            "text" = str_remove_all(lyric_vector["text"], "\u2028"),
            "extend" = "0"
          )
          lyric_vector = lyric_vector[-(1:3)] # remove first syllabus
        }
        list_lyric[[num_syl]] = c(
          lyric_vector["syllabic"],
          "text" = str_remove_all(lyric_vector["text"], "\u2028"),
          "extend" = "1",
          "type" = "start"
        )
        return(list_lyric)
      }
      else {
        list_lyric[[1]] = c("extend" = "1", "type" = "stop")
        lyric_vector = lyric_vector[-1]
        for (syllabus in 2:num_syl) {
          list_lyric[[syllabus]] = c(
            lyric_vector["syllabic"],
            "text" = str_remove_all(lyric_vector["text"], "\u2028"),
            "extend" = "0"
          )
          lyric_vector = lyric_vector[-(1:3)]
        }
        return(list_lyric)
      }
    }
    else {
      extend = xmlElementsByTagName(lyric_node, "extend", recursive = T)[[1]]
      tie_type = xmlGetAttr(extend, "type") # Start or stop
      if (tie_type == "start") {
        return (list(
          c(
            lyric_vector["syllabic"],
            "text" = str_remove_all(lyric_vector["text"], "\u2028"),
            "extend" = "1",
            "type" = "start"
          )
        ))
      }
      else
        return(list(c(
          "extend" = "1", "type" = "stop"
        )))
    }
  }
  list_lyric = list()
  for (syllabus in 1:num_syl) {
    list_lyric[[syllabus]] = c(
      lyric_vector["syllabic"],
      "text" = str_remove_all(lyric_vector["text"], "\u2028"),
      "extend" = "0"
    )
    lyric_vector = lyric_vector[-(1:3)] # Remove first syllabus
  }
  return(list_lyric)
}

is_grace = function(note) {
  # Returns a bool: is nota a grace note or not.
  # note is an XMLNode of tag <note>, which includes (or not) a tag <grace>.
  if (is_rest_note(note)) {
    return(FALSE)
  }
  any(names(getChildrenStrings(note)) == "grace")
}

note_to_number = c(
  "C" = 0,
  "C#" = 1,
  "D-" = 1,
  "D" = 2,
  "D#" = 3,
  "E-" = 3,
  "E" = 4,
  "F-" = 4,
  "E#" = 5,
  "F" = 5,
  "F#" = 6,
  "G-" = 6,
  "G" = 7,
  "G#" = 8,
  "A-" = 8,
  "A" = 9,
  "B-" = 10,
  "A#" = 10,
  "B" = 11,
  "C-" = 11,
  "B#" = 0
)
# Maps a note to a number from 0 to 11, 0 at C and 11 at B.

add_stemming_df = function(aria_df) {
  # This function takes a certain type of data frame (like the one created by
  # the main part of function get_music_df, defined below), and adds stems to it.
  n = nrow(aria_df)
  p = ncol(aria_df)
  column_names = colnames(aria_df)
  i = 1
  while (i <= n) {
    if ((aria_df$text[i] == "") | (aria_df$syllabic[i] == "none")) {
      aria_df[i, p + 1] = ""
      i = i + 1
      next
    }
    else if (aria_df$syllabic[i] == "begin") {
      starting = i
      ending = i + 1
      while ((aria_df$syllabic[ending] != "end") &
             not(is.na(aria_df$syllabic[ending]))) {
        ending = ending + 1
      } # word starts in starting and ends in ending
      
      if (ending > n) {
        aria_df[seq(starting, ending - 1), p + 1] = ""
        i = ending
        next
      }
      
      ac_word = ""
      for (j in seq(starting, ending)) {
        if ((aria_df$syllabic[j] == "none") |
            (aria_df$extend_type[j] == "stop")) {
          next
        }
        ac_word = str_c(ac_word, aria_df$text[j])
      }
      aria_df[seq(starting, ending), p + 1] = ac_word
      i = ending + 1
    }
    else {
      aria_df[i, p + 1] = aria_df$text[i]
      i = i + 1
    }
  }
  colnames(aria_df) = c(column_names, "stem")
  aria_df$stem = tolower(aria_df$stem)
  aria_df$stem = tm::removePunctuation(aria_df$stem)
  aria_df$stem = remove_apostrophe(aria_df$stem)
  aria_df$stem = remove_ellipsis(aria_df$stem)
  aria_df$stem = remove_quotations(aria_df$stem)
  aria_df$stem = stripWhitespace(aria_df$stem)
  aria_df$stem = str_trim(aria_df$stem)
  aria_df$stem = mapply(lemmatizer,
                        aria_df$stem,
                        MoreArgs = list(dict = lemmatization_it_stems))
  aria_df$stem = stemDocument(aria_df$stem, language = "italian")
  aria_df$stem = mapply(lemmatizer,
                        aria_df$stem,
                        MoreArgs = list(dict = custom_after))
  
  # Now we will remove stopwords from stems with >= 2 words
  for (j in 1:n) {
    if (str_count(aria_df$stem[j], "\\S+") >= 2) {
      if (stripWhitespace(tm::removeWords(aria_df$stem[j], stopwords)) == "") {
        aria_df$stem[j] = word(aria_df$stem[j], 1)
        next
      }
      aria_df$stem[j] = tm::removeWords(aria_df$stem[j], stopwords)
      aria_df$stem[j] = stripWhitespace(aria_df$stem[j])
      if (str_ends(aria_df$stem[j], " ")) {
        aria_df$stem[j] = substr(aria_df$stem[j],
                                 1, str_length(aria_df$stem[j]) - 1)
      }
      if (str_starts(aria_df$stem[j], " ")) {
        aria_df$stem[j] = substr(aria_df$stem[j],
                                 2, str_length(aria_df$stem[j]))
      }
    }
  }
  aria_df
}

get_music_df = function(notes) {
  # notes is a list of notes (XMLNodes), like the one we get from xmlElementsByTagName
  # Returns a data frame
  
  pitches = lapply(notes, get_pitch)
  
  names(pitches) = NULL
  
  pitches = lapply(pitches, sep_pitch)
  
  types = add_dots(notes, lapply(notes, number_dots))
  
  names(types) = NULL
  
  lyrics = lapply(notes, get_lyric)
  names(lyrics) = NULL
  
  graces = lapply(notes, is_grace)
  names(graces) = NULL
  
  aria_df = data.frame()
  
  # Index counting the real note we are on
  i_real = 1
  
  # Index that counts the row of the data frame we are on
  i_df = 1
  
  # We do the following: if there's notes with multiple syllabi (elisions), we
  # repeat the same note as many times as necessary, each time with a different
  # syllabus, as if they were different notes
  
  while (i_real <= length(lyrics)) {
    if (all(lyrics[[i_real]][[1]] == "")) {
      # No lyric in that note
      aria_df[i_df, 1] = pitches[[i_real]]["note"] # note
      aria_df[i_df, 2] = pitches[[i_real]]["octave"] # octave
      aria_df[i_df, 3] = types[[i_real]][1] # type
      aria_df[i_df, 4] = graces[[i_real]] # grace
      aria_df[i_df, 5] = "none" # syllabic
      aria_df[i_df, 6] = "" # text
      aria_df[i_df, 7] = 0 # extend
      aria_df[i_df, 8] = "none" # extend_type
      i_real = i_real + 1
      i_df = i_df + 1
    }
    else if (length(lyrics[[i_real]]) == 1) {
      # Only one syllab
      aria_df[i_df, 1] = pitches[[i_real]]["note"]
      aria_df[i_df, 2] = pitches[[i_real]]["octave"]
      aria_df[i_df, 3] = types[[i_real]][1]
      aria_df[i_df, 4] = graces[[i_real]]
      aria_df[i_df, 5] = ifelse(is.na(lyrics[[i_real]][[1]]["syllabic"]),
                                "none",
                                lyrics[[i_real]][[1]]["syllabic"])
      aria_df[i_df, 6] = ifelse(is.na(lyrics[[i_real]][[1]]["text"]),
                                "",
                                lyrics[[i_real]][[1]]["text"])
      aria_df[i_df, 7] = as.numeric(lyrics[[i_real]][[1]]["extend"])
      aria_df[i_df, 8] = ifelse(is.na(lyrics[[i_real]][[1]]["type"]),
                                "none",
                                lyrics[[i_real]][[1]]["type"])
      i_real = i_real + 1
      i_df = i_df + 1
    }
    else {
      # > 1 syllab
      for (k in 1:length(lyrics[[i_real]])) {
        aria_df[i_df, 1] = pitches[[i_real]]["note"]
        aria_df[i_df, 2] = pitches[[i_real]]["octave"]
        aria_df[i_df, 3] = types[[i_real]][1]
        aria_df[i_df, 4] = graces[[i_real]]
        aria_df[i_df, 5] = lyrics[[i_real]][[k]]["syllabic"]
        aria_df[i_df, 6] = lyrics[[i_real]][[k]]["text"]
        aria_df[i_df, 7] = as.numeric(lyrics[[i_real]][[k]]["extend"])
        aria_df[i_df, 8] = ifelse(is.na(lyrics[[i_real]][[k]]["type"]),
                                  "none",
                                  lyrics[[i_real]][[k]]["type"])
        i_df = i_df + 1
      }
      i_real = i_real + 1
    }
  }
  
  colnames(aria_df) = c("step",
                        "octave",
                        "type",
                        "grace",
                        "syllabic",
                        "text",
                        "extend",
                        "extend_type")
  
  aria_df$octave = as.numeric(aria_df$octave)
  
  # Lastly for ties, if there are two tied notes, the second one doesn't hold a
  # lyric, so we use the lyric from the first one.
  aria_df[which(aria_df$extend_type == "stop"), 6] =
    aria_df[which(aria_df$extend_type == "stop") - 1, 6]
  
  # Adding note number, useful for shape identification
  numbers = note_to_number[aria_df$step] + 12 * (aria_df$octave + 1)
  aria_df$numbers = numbers
  aria_df[, 3:9] = aria_df[, c(9, 3:8)]
  names(aria_df)[3:9] = names(aria_df)[c(9, 3:8)]
  
  # Adding stemming
  add_stemming_df(aria_df)
}

voice_name = function(instruments, l) {
  # This function returns the l-th voice from the aria. Will be used in
  # the aria identification (with function aria_identifier)
  
  voice_xml_name = instruments[[l]]
  case_when(
    voice_xml_name == "voice.soprano" ~ "Soprano",
    voice_xml_name == "voice.alto" ~ "Alto",
    voice_xml_name == "voice.tenor" ~ "Tenor",
    voice_xml_name == "voice.baritone" ~ "Baritone",
    voice_xml_name == "voice.bass" ~ "Bass"
  )
}

aria_identifier = function(root_node, part_voice, l) {
  # This function returns a unique identifier for each aria. This is done like:
  # 4 first letters from each name of the composer
  # 4 first letters from the first two words of the aria title
  # year
  # 4 first letter of the voice part (e.g. Sopr for soprano, Bass for bass...)
  # l is a number to differentiate between different parts of the same voice type
  identification_node = xmlElementsByTagName(root_node, "identification")[[1]]
  name_words = stringr::word(aria_name(root_node), 1:2)
  name_words = paste0(substr(str_to_title(name_words), 1, 4), collapse = "")
  
  composer_words = aria_composer(identification_node)
  composer_words = stringr::word(composer_words,
                                 1:(str_count(composer_words, " ") + 1))
  composer_words = paste0(substr(str_to_title(composer_words), 1, 4), collapse = "")
  
  year = get_year(root_node)
  part_voice = substr(str_to_title(part_voice), 1, 4)
  
  paste(composer_words,
        name_words,
        as.character(year),
        part_voice,
        as.character(l),
        sep = "-")
}

export_music_df = function(xmls) {
  # This function exports the music df's (with get_music_df) as .csv's, to the
  # folder here::here("aria_dfs"). Its original objective was to save computing
  # time and avoid memory leak associated with XML::xmlParse, but a better
  # way to do so has been implemented in export_rdata_files()
  setwd(here::here("aria_dfs"))
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
      
      write_csv(aria_df,
                file = paste0(aria_identifier(
                  root_node, voice_name(instruments, l), l
                ),
                ".csv"))
    }
    rm(aria_df, name, instruments)
    gc()
    print(paste(
      as.character(i),
      aria_name(root_node),
      "-",
      aria_composer(xmlElementsByTagName(root_node, "identification")[[1]]),
      "-",
      get_year(root_node)
    ))
    i = i + 1
    rm(root_node)
  }
}

export_rdata_files = function(xmls) {
  # This function exports a .RData for each voice (could be multiple RDatas for
  # each xml). This RData contains:
  # aria name
  # aria composer
  # year
  # didone code
  # voice type
  # instruments
  # time1
  # aria_df
  # identifier
  # Due to a known memory leak bug in XML::xmlParse, no more than ~ 75 arias per
  # GB of available RAM should be parsed at once.
  # Files are saved to ~/RDatas, where ~ is here::here()
  
  for (i in 1:length(xmls)) {
    aria = xmlParse(xmls[i])
    root_node = xmlRoot(aria)
    curr_aria_name = aria_name(root_node)
    
    parts = Filter(is_part, xmlChildren(root_node))
    
    part_list_subnode = xmlElementsByTagName(root_node, "part-list")[[1]]
    score_part_subnodes = xmlElementsByTagName(part_list_subnode, "score-part")
    
    identification_node = xmlElementsByTagName(root_node, "identification")[[1]]
    composer = aria_composer(identification_node)
    
    curr_aria_year = get_year(root_node)
    
    didone_id = get_didone_code(xmls[i])
    
    instruments = lapply(score_part_subnodes, get_instrument)
    
    free(aria)
    rm(aria,
       part_list_subnode,
       score_part_subnodes)
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
      
      voice_type = voice_name(instruments, l)
      
      identifier = aria_identifier(root_node, voice_type, l)
      
      save(
        curr_aria_name,
        composer,
        curr_aria_year,
        didone_id,
        voice_type,
        instruments,
        time1,
        aria_df,
        identifier,
        file = here::here("RDatas", paste0(identifier, ".RData"))
      )
      
      print(identifier)
    }
    print(as.character(i))
  }
}

extract_bars_xml = function(xml_file,
                            first_bar,
                            last_bar,
                            voice_number,
                            color_palette = NULL, 
                            unix = F, 
                            svg = T) {
  # This function exports both a MusicXML file and a PDF compiled from it
  # via MuseScore. musescore_exporter.bat or musescore_exporter.sh should be in
  # here::here() (depending on system type, Windows or Unix, e.g. Mac and Linux).
  # xml_file is an xml file (e.g. "C:\Example.xml"), can also be an external
  # pointer object, like the output of read_xml; first_bar and last_bar are
  # numbers that delimit the section we want to extract, both included. Lastly,
  # voice_number is the number of the voice from which we want to extract the
  # section. Note that anacrusis bar is by default numbered as 0.
  # The color_palette argument accepts a vector of colours, which are
  # applied to the noteheads. This argument is optional.
  # Lastly, if svg is set to TRUE, an svg file is also exported.
  # Example:
  # These numbers are the identifiers for the part in the xml, so if
  # we want to get bars 1-3 from the voice corresponding to node
  # <score-part id="P2"> we would do
  # extract_bars_xml(xml_file, 1, 3, 2, F)
  
  library(xml2)
  
  if (first_bar > last_bar)
    stop("first_bar can't be greater than last_bar")
  
  aria = read_xml(xml_file)
  
  # Remove credits
  repeat {
    credit_node = xml_child(aria, "credit")
    if (is.na(xml_name(credit_node)))
      break
    xml_remove(credit_node)
  }
  
  # Remove other parts from part-list
  partlist_node = xml_child(aria, "part-list")
  node_to_rm = 1
  repeat {
    if (length(xml_children(partlist_node)) == 1)
      break
    subnode = xml_child(partlist_node, node_to_rm)
    if (xml_has_attr(subnode, "id")) {
      if (xml_attr(subnode, "id") == paste0("P", as.character(voice_number))) {
        node_to_rm = 2
        next
      }
    }
    xml_remove(subnode)
  }
  
  # Remove other parts
  node_to_rm = 1
  repeat {
    if (length(xml_children(aria)) < node_to_rm) {
      break
    }
    subnode = xml_child(aria, search = node_to_rm)
    if (xml_name(subnode) != "part") {
      node_to_rm = node_to_rm + 1
      next
    }
    if (xml_attr(subnode, "id") == paste0("P", as.character(voice_number))) {
      node_to_rm = node_to_rm + 1
      next
    }
    xml_remove(subnode)
  }
  
  part_node = xml_child(aria, "part")
  
  anacrusis_flag = xml_attr(xml_child(part_node, 1), "number") == "0"
  
  # Remove every bar but the first one and the ones between first_bar and last_bar
  node_to_rm = 2
  suppressWarnings(repeat {
    if (length(xml_children(part_node)) < node_to_rm)
      break
    measure = xml_child(part_node, search = node_to_rm)
    if (as.numeric(xml_attr(measure, "number")) %in% seq(first_bar, last_bar, 1)) {
      node_to_rm = node_to_rm + 1
      next
    }
    xml_remove(measure)
  })
  
  # Move every note from the second measure to the first one
  first_node = xml_child(part_node, 1)
  second_node = xml_child(part_node, 2)
  # Remove notes from first_node
  node_to_rm = 1
  repeat {
    if (length(xml_children(first_node)) < node_to_rm)
      break
    subnode = xml_child(first_node, node_to_rm)
    if (xml_name(subnode) != "note") {
      node_to_rm = node_to_rm + 1
      next
    }
    xml_remove(subnode)
  }
  # Add notes from second_node
  node_to_add = 1
  repeat {
    if (length(xml_children(second_node)) < node_to_add)
      break
    subnode = xml_child(second_node, node_to_add)
    if (xml_name(subnode) != "note") {
      node_to_add = node_to_add + 1
      next
    }
    xml_add_child(first_node, subnode)
    node_to_add = node_to_add + 1
  }
  # Remove second_node
  xml_remove(second_node)
  
  # If anacrusis, change first bar number to 1
  if (anacrusis_flag) {
    xml_attr(first_node, "number") = "1"
  }
  
  # Remove bar numbering
  measure_numbering_node = xml_child(xml_child(first_node, "print"), "measure-numbering")
  xml_text(measure_numbering_node) = "none"
  xml_attrs(measure_numbering_node) = NULL
  
  # Change numbers of the remaining bars
  if (length(xml_children(part_node)) == 1) {
    ariaxml = xmlParse(xml_file)
    root_node = xmlRoot(ariaxml)
    part_list_subnode = xmlElementsByTagName(root_node, "part-list")[[1]]
    score_part_subnodes = xmlElementsByTagName(part_list_subnode, "score-part")
    instruments = lapply(score_part_subnodes, get_instrument)
    id = aria_identifier(root_node,
                         voice_name(instruments, voice_number),
                         voice_number)
    if (is.null(color_palette)) {
      write_xml(aria,
                paste0(
                  id,
                  "-bars-",
                  as.character(first_bar),
                  "-to-",
                  as.character(last_bar),
                  ".xml"
                ))
    }
    else {
      write_xml(aria,
                paste0(
                  id,
                  "-bars-",
                  as.character(first_bar),
                  "-to-",
                  as.character(last_bar),
                  ".xml"
                ))
    }
  }
  else {
    for (bar in 2:length(xml_children(part_node))) {
      current_bar = xml_child(part_node, bar)
      xml_attr(current_bar, "number") = as.character(bar)
    }
    ariaxml = xmlParse(xml_file)
    root_node = xmlRoot(ariaxml)
    part_list_subnode = xmlElementsByTagName(root_node, "part-list")[[1]]
    score_part_subnodes = xmlElementsByTagName(part_list_subnode, "score-part")
    instruments = lapply(score_part_subnodes, get_instrument)
    id = aria_identifier(root_node,
                         voice_name(instruments, voice_number),
                         voice_number)
    
    if (is.null(color_palette)) {
      write_xml(aria,
                paste0(
                  id,
                  "-bars-",
                  as.character(first_bar),
                  "-to-",
                  as.character(last_bar),
                  ".xml"
                ))
    }
    else {
      write_xml(aria,
                paste0(
                  id,
                  "-bars-",
                  as.character(first_bar),
                  "-to-",
                  as.character(last_bar),
                  ".xml"
                ))
    }
  }
  
  if (!unix) {
    cmd_to_run = paste(
      here::here("musescore_exporter.bat"),
      here::here(
        paste0(
          id,
          "-bars-",
          as.character(first_bar),
          "-to-",
          as.character(last_bar),
          ".xml"
        )
      ),
      here::here(
        paste0(
          id,
          "-bars-",
          as.character(first_bar),
          "-to-",
          as.character(last_bar),
          ".pdf"
        )
      )
    )
    cmd_to_run = str_replace_all(cmd_to_run, "/", "\\\\")
    system(cmd_to_run)
    if (svg) {
      cmd_to_run = paste(
        here::here("musescore_exporter.bat"),
        here::here(
          paste0(
            id,
            "-bars-",
            as.character(first_bar),
            "-to-",
            as.character(last_bar),
            ".xml"
          )
        ),
        here::here(
          paste0(
            id,
            "-bars-",
            as.character(first_bar),
            "-to-",
            as.character(last_bar),
            ".svg"
          )
        ), 
        "svg"
      )
      cmd_to_run = str_replace_all(cmd_to_run, "/", "\\\\")
      system(cmd_to_run)
    }
    
    print(paste0("Export: ", id, "-bars-", as.character(first_bar), "-to-", as.character(last_bar)))
  }
  else {
    sh_to_run = paste(
      here::here("musescore_exporter.sh"),
      here::here(
        paste0(
          id,
          "-bars-",
          as.character(first_bar),
          "-to-",
          as.character(last_bar),
          ".xml"
        )
      ),
      here::here(
        paste0(
          id,
          "-bars-",
          as.character(first_bar),
          "-to-",
          as.character(last_bar),
          ".pdf"
        )
      ),
      "> /dev/null"
    )
    system(sh_to_run)
    
    print(paste0("Export: ", id, "-bars-", as.character(first_bar), "-to-", as.character(last_bar)))
  }
}

extract_part_from_notes = function(xml_file,
                                   first_note,
                                   last_note,
                                   voice_number,
                                   color_palette = NULL, 
                                   unix = F, 
                                   svg = T) {
  # Exports both a MusicXML file and a pdf file of the aria voice voice_number,
  # using extract_bars_xml and first_note, last_note as delimiters. Note that
  # only exporting whole measures is supported, so the export won't necessarily
  # start in first_note, but instead it will start in the first note of its bar,
  # similarly with last_note.
  
  library(xml2)
  
  aria = read_xml(xml_file)
  
  repeat {
    part_node = xml_child(aria, search = "part")
    attrib = xml_attr(part_node, "id")
    if (attrib == paste0("P", as.character(voice_number)))
      break
    xml_remove(part_node)
  }
  note_count = 0
  first_bar = -1
  last_bar = -1
  for (measure in 1:length(xml_children(part_node))) {
    bar = xml_child(part_node, search = measure)
    repeat {
      note = xml_child(bar, search = "note")
      if (is.na(note)) {
        break
      }
      xml_remove(note)
      note_count = note_count + 1
      
      if (note_count == first_note) {
        first_bar = measure
      }
      
      if (note_count == last_note) {
        last_bar = measure
      }
    }
    
    if (last_bar >= 0) {
      break
    }
  }
  
  extract_bars_xml(xml_file, first_bar, last_bar, voice_number, color_palette, unix, svg)
}

source(here::here("metric_fun.R"))

extract_xml_from_df_line = function(xml_file,
                                    voice_number,
                                    unix = F, 
                                    svg = T, 
                                    aria_df,
                                    df_line,
                                    centered = T,
                                    n_neighbors,
                                    metric,
                                    coloring = T) {
  
  
  # This function extracts the neighborhood (word-based) according to df_line
  # (line of aria_df in which the first note of the stem appears), centered
  # (whether it is a symmetric neighborhood or only before), n_neighbors (has to
  # be 1, 2 or 3), and the corresponding voice number. It exports the MusicXML
  # as well as the pdf. If coloring is set to TRUE, the noteheads are colored
  # according to the neighborhood as well as the metric.
  
  aria = read_xml(xml_file)
  
  if (centered) {
    neighborhood = word_neighbors(aria_df, n_neighbors, df_line, T)
  }
  else {
    neighborhood = word_asymmetric_neighbors(aria_df, n_neighbors, 0, df_line, T)
  }
  
  # Extract notes
  
  first_line = as.numeric(row.names(neighborhood)[1])
  last_line = as.numeric(row.names(neighborhood)[nrow(neighborhood)])
  
  # Colour neighborhood
  
  if (!coloring) {
    extract_part_from_notes(xml_file, first_line, last_line, voice_number, NULL, 
                            unix)
  }
  else {
    library(RColorBrewer)
    
    if (metric == "local_slope") {
      metric_value = ifelse(
        centered,
        local_slope(aria_df, n_neighbors, df_line),
        local_slope_before(aria_df, n_neighbors, df_line)
      )
      
      if (metric_value > 0) {
        color_palette =
          c(
            colorRampPalette(brewer.pal(9, name = "YlGn")[3:8])(df_line + 1 - first_line),
            rev(
              colorRampPalette(brewer.pal(9, name = "YlGn")[3:8])(last_line + 1 - df_line)[-last_line - 1 + df_line]
            )
          )
      } 
      else {
        color_palette =
          c(
            colorRampPalette(brewer.pal(9, name = "YlOrRd")[2:7])(df_line + 1 - first_line),
            rev(
              colorRampPalette(brewer.pal(9, name = "YlOrRd")[2:7])(last_line + 1 - df_line)[-last_line - 1 + df_line]
            )
          )
      }
    }
    
    if (metric == "monotonic_rho") {
      metric_value = ifelse(
        centered,
        monotonic_rho(aria_df, n_neighbors, df_line),
        monotonic_rho_before(aria_df, n_neighbors, df_line)
      )
      
      if (metric_value > 0) {
        color_palette =
          c(
            colorRampPalette(brewer.pal(9, name = "YlGn")[3:8])(df_line + 1 - first_line),
            rev(
              colorRampPalette(brewer.pal(9, name = "YlGn")[3:8])(last_line + 1 - df_line)[-last_line - 1 + df_line]
            )
          )
      }
      else {
        color_palette =
          c(
            colorRampPalette(brewer.pal(9, name = "YlOrRd")[2:7])(df_line + 1 - first_line),
            rev(
              colorRampPalette(brewer.pal(9, name = "YlOrRd")[2:7])(last_line + 1 - df_line)[-last_line - 1 + df_line]
            )
          )
      }
    }
    
    if (metric == "local_quadratic") {
      metric_value = ifelse(
        centered,
        local_quadratic(aria_df, n_neighbors, df_line),
        local_quadratic_before(aria_df, n_neighbors, df_line)
      )
      
      color_palette =
        c(
          colorRampPalette(brewer.pal(9, name = "PuBu")[4:9])(df_line + 1 - first_line),
          rev(colorRampPalette(brewer.pal(9, name = "PuBu")[4:9])(last_line + 1 - df_line)[-last_line - 1 + df_line])
        )
    }
    
    if (metric == "interv_mean") {
      metric_value = ifelse(
        centered, 
        interv_mean(aria_df, n_neighbors, df_line), 
        interv_mean_before(aria_df, n_neighbors, df_line)
      )
      
      color_palette = rep("#253494", nrow(neighborhood))
    }
    
    file_path = here::here("temp_file.xml")
    write_xml(aria, file = file_path)
    extract_part_from_notes(here::here("temp_file.xml"), first_line, last_line, voice_number, color_palette, unix, svg)
    Sys.sleep(5)
    file.remove(here::here("temp_file.xml"))
  }
}
