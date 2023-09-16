# TFM Word painting

This is a repository containing files from Master's Thesis "Word painting in the eighteenth century: truth or lie?"

**Author**: Francisco Jesús Morilla Ortega

**Supervisors**: Eduardo García Portugués, Ana Llorens Martín

**Year**: 2023

Universidad Carlos III de Madrid


# Description of files and important functions

## `df_fun.R`

This file contains functions whose purpose is to extract information from a MusicXML file like the ones we work with. The first functions are mostly used to 
define the main function, `get_music_df()`. This function takes as input a list of nodes, that can be obtained via `XML::xmlElementsByTagName()`, as well as 
some global variables. It outputs a data frame which is the basis of the analysis we perform (we compute the metrics on these data frames): this type of
data frame will henceforth be called "music data frame".

Another important function defined in this file is `export_rdata_files()`, which takes as an argument a list (atomic vector) of paths to the MusicXML files. For
each MusicXML file, it outputs one `.RData` file for each part in the MusicXML file which is a voice. This `.RData` contains metadata like aria name, aria composer, 
year, Didone ID, voice type (Soprano, Tenor, etc.)... It also contains `time1`, a vector related to the time signature, and `aria_df` which is the data frame obtained
via `get_music_df()`. The goal of this function is double: first, it saves time by only computing the data frames once, and then each time they have to be used they 
are loaded instead of recalculated (it takes between 2 and 8 seconds to process a data frame); second, calculating many data frames at a time is not possible 
due to a known memory leak bug in package `XML` (see [here](https://github.com/thomasp85/mzID/issues/10), [here](https://www.appsloveworld.com/r/100/68/workaround-to-r-memory-leak-with-xml-package), 
and [here](https://stackoverflow.com/questions/23696391/memory-leak-when-using-package-xml-on-windows)). Apparent solutions did not work for us, and this memory leak bug
made it impossible for us to parse more than 400 XML files at one go without our computers running out of memory (maximum seems to be around 60-75 arias per GB of available memory). So we parsed them into data frames in 
series of shorter runs, before resetting the R session and continuing the parsing process. We were able to avoid going through this process many times by going
through it only once and saving the environment containing the needed variables as an `.RData` file (as the memory leaking happens with `XML::xmlParse()`).
The `.RData` files are supplied in the `./RDatas` folder.

Lastly, another important function is `extract_bars_xml()`, which allows us to extract the voice part between two given measures, and export it to PDF or
SVG using MuseScore and its shell integration. Shell integration is done via files `musescore_exporter.bat` (for Windows) and `musescore_exporter.sh` (for
Unix). In this function, instead of package `XML`, we used `xml2`, which still has some memory leaking problems but to a lesser extent. The objective of this 
function is to extract rendered music on which word painting may be present, according to results given from our analysis.

## `gm_fun.R`

This short file contains functions relating integration with package `gm` (a package to write music graphically using R objects). The objective was to verify
that our codification as music data frames was correct, so that no information was lost and we could "revert" this format back to music notation.
Its importance in the overall work is small, as it just served as verification that our data model was loyal to the written music.

## `metric_fun.R`

In this file, functions related to metrics are defined. There are functions that calculate neighborhoods given a music data frame and a particular word (like
`word_neighbors()`, `word_asymmetric_neighbors()`, `note_neighbors()`, etc.); there are functions that calculate metrics in centered and uncentered neighborhoods (like
`interv_mean()` or `interv_mean_before()`); there are auxiliary functions for metric computation (for instance `kernel_regression_coefficients()` calculates the 
coefficients in local polynomial regression); and a function called `metrics_tidy_csv()`. This function exports a data frame containing every metric value
from every stem passed as its argument, from every aria passed as `.RData` as another argument. It supports appending, meaning that exporting as CSV can be
done in shorter steps, by calling the function with reduced amounts of RDatas and using the argument `append = TRUE`. To save time, the output of this 
function with the stems we've used is the file `metric_table.csv`.

## `tm_fun.R`

This short file contains functions related to text mining. There is a function `export_lyrics()`, which exports the lyrics of an atomic vector of XML files
passed to it (watch out as it suffers from the memory leaking bug). Nevertheless, the output of this function on all the corpus is in the `./Lyrics` folder.
The rest of the functions are used in the text mining/stemming part: removing apostrophes, ellipses, quotations, etc.

## `prep.R`

This file contains some work whose results are used in the rest of the thesis. In particular, in this file we perform tf-idf, lemmatization and stemming, 
associated text mining techniques, pruning of less relevant stems, use `metrics_tidy_csv()` to export a CSV with metrics, use `export_rdata_files`
to export the necessary `.RData`s, try the functions in `gm_fun.R` to get back rendered musical notation, etc.

## `model_fitting.R`

This file contains the code that fits the models, plots some diagnostic graphs, and it also includes the code for bootstrapping. As model fitting
and particularly bootstrapping are computationally expensive, the outputs of bootstrapping (with coefficients, confidence intervals, etc.) are placed as
CSV files in folder `./bootstrap_tables`. Diagnostic plots are made here instead of elsewhere since here is where models are fitted.

## `graphs.R`

This file contains the code that produces some graphs that have been used elsewhere in our work.

## `musescore_exporter.bat`, `musescore_exporter.sh`

These shell files are to be used with `extract_bars_xml()` from file `df_fun.R`. They require MuseScore to be installed, and in the Windows case each 
user will have to modify the `.bat` to change the path of installation. Perhaps these files may be changed so that they don't require user intervention to work, 
although that will need the creation of an environment variable with MuseScore's path. The bash script was tested in an Ubuntu distribution with a standard
MuseScore 4 installation, it was not tested in Mac OS but it should work just fine.

## `custom_stemming.txt`

A file with custom stemming that Snowball stemmer wasn't doing right. We looked at the stemming by hand and corrected the words that were wrongly stemmed.

## `michmech-italian-stemming/lemmatization-it.txt`

This is the file containing the dictionary for lemmatization, taken from <https://github.com/michmech/lemmatization-lists>.

## `stopwords.txt`

This file contains a (slightly modified, with added entries) file with what we have used as stopwords. The original file is taken from <https://github.com/stopwords-iso/stopwords-it>, 
and some entries have been added after we noticed some words that we thought were stopwords but weren't being considered.

## `x.RData` and `y.RData` (placeholder)

These two `.RData` files, which are outside the folder `./RDatas`, are meant to be used with the functions in `gm_fun.R`, like the section at the end of
`prep.R`. They serve as examples that our process of transforming MusicXML to data frames is lossless, in terms of information, and thus it can be reverted.

## `fitted_models.RData`

This is a file which allows for instant loading of the pre-fitted models (although they do not take that much time to fit).

## `./RDatas` folder

This folder contains all of the `.RData` files, that have been obtained by means of `export_rdata_files()`. There is one such file for every part, and they 
contain (most importantly) the music data frame obtained via `get_music_df()`.

## `./bootstrap_tables` folder

This folder contains tables with information regarding mixed model bootstrapping, as outputs of the code in `model_fitting.R`. They are meant to be used 
without having to redo bootstrap, as it is intensive in computing time.

## `./coefficient_tables` folder

This folder contains tables with the fixed effect coefficients (one for each model that was fitted). 

## `./Lyrics` folder

This folder contains the lyrics of every aria in the corpus. These are used in the text mining part in `prep.R`, where they are lemmatized, stemmed and treated (remove
apostrophes, commas, etc.), and are used to build a document term matrix. After that, tf-idf and pruning are performed.
