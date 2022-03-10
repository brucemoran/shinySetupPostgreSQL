# utilities.R

#' Launch the app
#'
#' @return launched app
#' @rdname launchSSP
#' @export

launchSSP <- function(){
  shiny::runApp(system.file(package = "shinySetupPostgreSQL", "app"))
}

#' Columns for new table
#' @return vector of below values
#' @rdname new_table_cols
#' @export

new_table_cols <- function(){

  ntc_names <- c(
    "Year",
    "Forename",
    "Surname",
    "DOB",
    "Sex",
    "Lab_ID",
    "Specimen",
    "Block",
    "MRN",
    "Hospital",
    "Cancer",
    "Test",
    "Mutation",
    "Pri_Met",
    "Site",
    "Source",
    "Macrod",
    "Comment",
    "Pathologist",
    "Clinician",
    "Tumour_pc_SVUH",
    "Tumour_pc_Ext",
    "Date_Requested",
    "Date_to_Pathologist",
    "Date_Ext_Rec",
    "Date_Ext_Rep",
    "Date_Authorised",
    "User")

    ntc_field_types <- c(
      "numeric",
      "character",
      "character",
      "date",
      "character",
      "character",
      "character",
      "character",
      "character",
      "character",
      "character",
      "character",
      "character",
      "character",
      "character",
      "character",
      "character",
      "character",
      "character",
      "numeric",
      "numeric",
      "date",
      "date",
      "date",
      "date",
      "date",
      "character")

    ntc_tb <- tibble::as_tibble(t(ntc_field_types))
    colnames(ntc_tb) <- ntc_names

    ##NB that numeric columns are character, so need to be changed in obsev_extantabled
    return(ntc_tb)
}

#' Required columns for new table
#' @return vector of required values for table, if all NA or "-" row is removed
#' @rdname new_table_reqs
#' @export

new_table_reqs <- function(){

  c("Forename",
    "Surname",
    "Specimen",
    "MRN",
    "Date_Requested")
}

#' Columns that are numeric
#' @return vector of below values
#' @rdname numeric_table_cols
#' @export

numeric_table_cols <- function(){

  c("Year",
    "Tumour_pc_SVUH",
    "Tumour_pc_Ext")

}

#' Columns that are date
#' @return vector of below values
#' @rdname date_table_cols
#' @export

date_table_cols <- function(){

  c("DOB",
    "Date_Requested",
    "Date_to_Pathologist",
    "Date_Ext_Rec",
    "Date_Ext_Rep",
    "Date_Authorised")

}

#' Take a tibble with colnames to match pattern and force to Date if not already
#' @param tb tibble
#' @param pattern string to match colnames to be forced to Date
#' @return list containing matches and Date values (NB NA coercion likely!)
#' @rdname date_as_class
#' @export

date_as_class <- function(tb, pattern){

  ##find date cols
  date_cols <- tb[, grep(pattern, colnames(tb))]

  ##iterate over classes, if not Date then force
  dc_list <- lapply(colnames(date_cols), function(f){
    first_notna <- unlist(date_cols[[f]][!is.na(date_cols[[f]])])[1]
    if(is.character(first_notna)){
      return(lubridate::as_date(as.numeric(date_cols[[f]])))
    } else {
      return(lubridate::as_date(date_cols[[f]]))
    }
  })
  return(dc_list)
}

#' Parse CMD PDF reports, very hacky
#' @param pdf_path path to PDF
#' @return named character vector
#' @rdname import_cmd_pdf
#' @export

import_cmd_pdfs <- function (pdf_path) {

  print(paste0("Working on: ", pdf_path))
  pdf_f <- pdftools::pdf_text(pdf_path)
  str_f <- unlist(strsplit(pdf_f, "\n"))

  ##test length, scanned image files are usually == "" but can have some text
  if(length(str_f)>5){

    ##use 20 spaces in a row to denote a new line and split to get single entries
    space20 <- paste(rep(" ", times = 20), collapse = "")
    str_f_sp <- unlist(lapply(str_f, function(fsp) {
      sso <- stringr::str_trim(gsub("\\s *", " ", strsplit(fsp,
        space20)[[1]]))
    }))

  ##remove uneccesary empty lines
  str_f_sp <- str_f_sp[str_f_sp != ""]

  ##function for splitting on a term and returning values
  grep_split_ret <- function(trm, s) {
    gtrm <- grep(trm, s, value = TRUE)
    ##ensure the term exists in a line
    if(length(gtrm) > 0){
      ##iff term is different to found term, i.e. value exists
      ##split by term and trim whitespace of result, else NA
      if(gtrm != trm){
        ssp <- strsplit(grep(trm, s, value = TRUE), trm)[[1]]
        strsplit(trimws(ssp[ssp != ""]), " ")[[1]]
      }} else {
        NA
    }
  }

  ##find specimen to ensure we have unique ID to work from
  speci <- grep_split_ret ("Your Ref:", str_f_sp)
  if (length(speci) > 0){
    if(!is.na(speci[1])) {
      pname <- rev(grep_split_ret("Patient's Name:", str_f_sp))
      fname <- pname[1]

      if (length(pname) > 2) {
        sname <- paste0(pname[3], "'", pname[2])
      } else {
        sname <- pname[2]
      }

      if (length(speci) > 1) {
        block <- speci[2]
        speci <- speci[1]
      } else {
        block <- "-"
        speci <- speci[1]
      }

      hospno <- grep_split_ret("Hospital No:", str_f_sp)[1]
      dob <- grep_split_ret("Date of Birth:", str_f_sp)
      date_rec <- grep_split_ret("Date of Receipt:", str_f_sp)
      date_rep <- grep_split_ret("Date of Report:", str_f_sp)
      ref_r <- grep_split_ret("Referral Reason:", str_f_sp)

      if (length(ref_r) > 0) {
        cancer <- gsub("\\.", "", paste(ref_r, collapse = "_"))
      } else {
        cancer <- "-"
      }

      tumour_pc <- grep_split_ret("Tumour Percentage", str_f_sp)

      if (length(tumour_pc) > 0){
        if(!is.na(tumour_pc[1])) {
        tumour_pc <- as.numeric(tumour_pc[1])
      } else {
        tumour_pc <- NA
      }}

      results <- str_f_sp[c(grep("RESULT:", str_f_sp) + 1):c(grep("INTERPRETATION:", str_f_sp) - 1)]

      gtr <- grep("Tier", results)
      if (length(gtr) > 0) {
        muts <- paste(unlist(lapply(gtr, function(f) {
          tire <- gsub(" ", "_", rev(strsplit(results[f], "[()]")[[1]])[1])
          paste0(tire, ":", gsub(" ", "_", results[f + 1]))
          })), collapse = ";")
        } else {
          muts <- "NO MUTS"
        }

        return(list(Year = 1,
                    Forename = fname,
                    Surname = sname,
                    DOB = dob,
                    Specimen = speci,
                    Block = block,
                    Cancer = cancer,
                    MRN = hospno,
                    Tumour_pc_Ext = tumour_pc,
                    Mutation = muts,
                    Test = "External",
                    Pri_Met = "-",
                    Macrod = "-",
                    Site = "-",
                    Source = "-",
                    Date_Ext_Rec = date_rec,
                    Date_Ext_Rep = date_rep))
      }
    }
  } else {
    return(NULL)
  }
}

#' string to grep XLSX sheets
#' @return string
#' @rdname sheet_grep_string
#' @export

sheet_grep_string <- function(){
  c("KRAS |BRAF |NRAS |EGFR |EXTERNAL")
}

#' Set of predetermined named vector for grep_choice_selectin()
#' @return named vector
#' @rdname grep_choice_selectin_vec
#' @export

grep_choice_selectin_vec <- function(){

  c("Forename" = "First",
    "Surname"  = "Surname",
    "Lab_ID"   = "Lab",
    "Specimen" = "SVUH",
    "Block"    = "SVUH",
    "MRN"      = "Hosp\\.",
    "Hospital" = "Hospital",
    "Cancer"   = "Cancer",
    "Test"     = "Test",
    "Mutation" = "Mutation",
    "Pri_Met"  = "Primary",
    "Site"     = "Tissue",
    "Source"   = "iopsy",
    "Macrod"   = "Macrod",
    "Comment"  = "Comment",
    "Pathologist" = "empty",
    "Tumour_pc_SVUH" = "%",
    "Tumour_pc_Ext" = "%",
    "Date_Requested" = "Requested",
    "Date_to_Pathologist" = "passed",
    "Date_Ext_Rec" = "REC",
    "Date_Ext_Rep" = "EXT",
    "Date_Authorised" = "Authorise")
}

#' For selectizeInput, grep on set of choices as to what is most likely match,
#  or return fail
#' @param choices vector of choices in selectInput
#' @param label string label of selectInput
#' @param fail string to return if no matches
#' @return string
#' @rdname grep_choice_selectin
#' @export

grep_choice_selectin <- function(choices, label, fail, label_vec = NULL){

  if(is.null(label_vec)){
    label_vec <- shinySetupPostgreSQL::grep_choice_selectin_vec()
  }

  label_clue <- label_vec[grep(label, names(label_vec))]
  if(length(match(label_clue,'named character(0)')) > 0){
    label <- label_clue
  }
  cut_label <- strsplit(paste(strsplit(label, "")[[1]][-1], collapse  = ""),
                        " ")[[1]][1]
  choice <- grep(cut_label, choices, value = TRUE)
  if(length(match(choice,character(0)))==0){
    return(fail)
  } else {
    return(choice)
  }
}

#' Take a tibble column and split on delimiter returning index x of vector from split
#' @param tb tibble with column 'col'
#' @param col colname to split on delim and take xth entry
#' @param delim delimiter to split entries in tb[,col]
#' @param x index of element to return
#' @return string
#' @rdname tb_col_delim_split
#' @export

tb_col_delim_split <- function(tb, col, delim, x){
  unlist(lapply(tb[[col]], function(f){
      strsplit(" ", f)[[1]][x]
    }))
}
