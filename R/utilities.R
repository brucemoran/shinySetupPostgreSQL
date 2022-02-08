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
    "Sex",
    "Lab ID",
    "Specimen",
    "Block",
    "Hosp. No.",
    "Hospital",
    "Cancer",
    "Test",
    "Mutation",
    "Pri/Met",
    "Tissue",
    "Source",
    "Comment",
    "Pathologist",
    "Clinician",
    "Tumour % SVUH",
    "Tumour % Ext.",
    "Date_Requested",
    "Date_Ext_Rec",
    "Date_Ext_Rep",
    "Date_Authorised")

    ntc_field_types <- c(
      "numeric",
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
      "character",
      "character",
      "numeric",
      "numeric",
      "date",
      "date",
      "date",
      "date")

    ntc_tb <- tibble::as_tibble(t(ntc_field_types))
    colnames(ntc_tb) <- ntc_names

    ##NB that numeric columns are character, so need to be changed in obsev_extantabled
    return(ntc_tb)
}

#' Columns that are numeric
#' @return vector of below values
#' @rdname numeric_table_cols
#' @export

numeric_table_cols <- function(){

  c("Year", "Tumour % SVUH", "Tumour % Ext.")

}

#' Columns that are date
#' @return vector of below values
#' @rdname date_table_cols
#' @export

date_table_cols <- function(){

  c("Date_Requested",
    "Date_Ext_Rec",
    "Date_Ext_Rep",
    "Date_Authorised",
    "DOB")

}

#' Parse CMD PDF reports
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
                    `Hosp. No.` = hospno,
                    `Tumour % Ext.` = tumour_pc,
                    Mutation = muts,
                    Test = "External",
                    `Pri/Met` = "-",
                    Macrod. = "-",
                    Tissue = "-",
                    Source = "-",
                    Date_Ext_Rec = date_rec,
                    Date_Ext_Rep = date_rep))
      }
    }
  } else {
    return(NULL)
  }
}
