# ============================== #
# =======   GM PACKAGE   ======= #
# ============================== #

# Getting the pitches right for gm
convert_to_gm_pitch = function(pitch) {
  # pitches is a vector of a pitch like the one we get from sep_pitch
  if (is.na(pitch[1])) {
    # rest
    return(NA)
  }
  else
    paste0(pitch["note"], pitch["octave"])
}


get_score = function(notas, anacrusis_duration, type_voice) {
  # notas is a list of notes, best if there are no grace notes since gm doesn't
  # support them
  pitches_score = lapply(notas, get_pitch)
  pitches_score = lapply(pitches_score, sep_pitch)
  names(pitches_score) = NULL
  
  gm_pitches = lapply(pitches_score, convert_to_gm_pitch)
  
  types_score = add_dots(notas, lapply(notas, number_dots))
  names(types_score) = NULL
  
  clef = case_when(
    type_voice == "Soprano" ~ gm::Clef(sign = "C", line = 1, to = 1),
    type_voice == "Alto" ~ gm::Clef(sign = "C", line = 3, to = 1),
    type_voice == "Tenor" ~ gm::Clef(sign = "C", line = 4, to = 1),
    type_voice == "Baritone" ~ gm::Clef(sign = "F", line = 3, to = 1),
    TRUE ~ gm::Clef(sign = "F", line = 4, to = 1)
  )
  
  score = gm::Music()
  if (anacrusis_duration == 0) {
    score = score +
      gm::Meter(time1[1], time1[2])
    gm::Line(pitches = gm_pitches,
             durations = types_score) +
      clef +
      gm::Key(as.numeric(key["fifths"]))
    
    gm::export(
      score,
      dir_path = here::here(),
      file_name = paste0(
        str_replace_all(aria_name, " ", "-"),
        "--",
        str_replace_all(composer, " ", "-")
      ),
      "pdf"
    )
  }
  else {
    mcd = gcd(anacrusis_duration, time1[2])
    score = score +
      gm::Meter(time1[1],
                time1[2],
                actual_number = anacrusis_duration / mcd,
                actual_unit = 16 / mcd) +
      gm::Meter(time1[1], time1[2], bar = 2, invisible = TRUE) +
      gm::Line(pitches = gm_pitches,
               durations = types_score) +
      gm::Clef(sign = "C",
               line = clef,
               to = 1) +
      gm::Key(as.numeric(key["fifths"]))
    
    gm::export(
      score,
      dir_path = here::here(),
      file_name = paste0(
        str_replace_all(aria_name, " ", "-"),
        "--",
        str_replace_all(composer, " ", "-")
      ),
      "pdf"
    )
  }
}