# utilities.R

#' Parse CMD PDF reports
#' @param pdf_path path to PDF
#' @param batch logical, if TRUE test all PDFs in pdf_path (default: TRUE)
#' @return named character vector
#' @rdname import_cmd_pdf
#' @export

import_cmd_pdf <- function(pdf_path, batch = TRUE){

  if(batch){
    pdf_s <- dir(path = pdf_path, pattern = "pdf$", full.names = TRUE)
  } else {
    pdf_s <- pdf_path
  }

  ##iterate over PDF input(s)
  unlist(lapply(pdf_s, function(f){
  	print(paste0("Working on: ", f))

    ##read in PDF text
    pdf_f <- pdftools::pdf_text(f)

    ##split and split again
    str_f <- unlist(strsplit(pdf_f, "\n"))

    ##use this to denote a new entry and split
    space20 <- paste(rep(" ", times=20), collapse = "")

    str_f_sp <- unlist(lapply(str_f, function(fsp){
      sso <- stringr::str_trim(gsub("\\s *", " ", strsplit(fsp, space20)[[1]]))
    }))
    str_f_sp <- str_f_sp[str_f_sp != ""]

    ##function to grep on term, split on term and return all after term
    grep_split_ret <- function(trm, s){
      ssp <- strsplit(grep(trm, s, value = TRUE), trm)[[1]]
      strsplit(trimws(ssp[ssp != ""]), " ")[[1]]
    }

    ##test we have a Ref(erence) otherwse do not use as we cannot place the data
  	if(length(grep("Your Ref:", str_f_sp))>0){

  	  ##name
  	  pname <- rev(grep_split_ret("Patient's Name:", str_f_sp))
  	  fname <- pname[1]
  	  if(length(pname)>2){
  	    sname <- paste0(pname[3], "'", pname[2])
  	  } else {
  	    sname <- pname[2]
  	  }

  	  ##Specimen + Block
      speci <- grep_split_ret("Your Ref:", str_f_sp)
      if(length(speci) > 1){
        block <- speci[2]
        speci <- speci[1]
      } else {
        block <- "-"
        speci <- speci[1]
      }

      ##Hospital Number, Dates
      hospno <- grep_split_ret("Hospital No:", str_f_sp)[1]
      dob <- grep_split_ret("Date of Birth:", str_f_sp)
      date_rec <- grep_split_ret("Date of Receipt:", str_f_sp)
      date_rep <- grep_split_ret("Date of Report:", str_f_sp)

      ##Referral Reason aka Cancer
      ref_r <- grep_split_ret("Referral Reason:", str_f_sp)
  	  if(length(ref_r)>0){
        cancer <- gsub("\\.", "", paste(ref_r, collapse = "_"))
      } else {
        cancer <- "-"
      }

      ##Tumour % Ext.
      tumour_pc <- grep_split_ret("Tumour Percentage", str_f_sp)
      if(length(tumour_pc)>0){
    	  tumour_pc <- as.numeric(tumour_pc)
    	} else {
    		tumour_pc <- NA
    	}

      ##Mutation
      results <- str_f_sp[c(grep("RESULT:", str_f_sp)+1):c(grep("INTERPRETATION:", str_f_sp)-1)]
      gtr <- grep("Tier", results)
      if(length(gtr) > 0){
        muts <- paste(unlist(lapply(gtr, function(f){
            tire <- gsub(" ", "_", rev(strsplit(results[f], "[()]")[[1]])[1])
            if(length(grep("Tier_III$", tire)) > 0){
              tire <- paste0(tire, "_VUS:")
            }
            if(length(grep("Tier_II$", tire)) > 0){
              tire <- paste0(tire, "_potential:")
            }
            if(length(grep("Tier_I$", tire)) > 0){
              tire <- paste0(tire, "_strong:")
            }
            paste0(tire, gsub(" ", "_", results[f+1]))
        })), collapse = ";")
        } else {
          muts <- "NO MUTS"
        }

        return(list(Forename = fname,
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
                    `Macrod.` = "-",
                    Tissue = "-",
                    Source = "-",
                    Date_Ext_Rec = date_rec,
                    Date_Ext_Rep = date_rep))
    }
  }))
}
