# parsing.R

#' Column grep from vector of terms
#' grep spectaculare
#' @param INVEC a vector to search for match
#' @param TERMS a vector of terms to match on
#' @param INVERTS a boolean vec of inverted or not greps
#' @return a single character object
#' @rdname grepSpec
#' @export

grepSpec <- function(INVEC, TERMS, INVERTS){

  t1 <- grep(TERMS[1], INVEC, invert = INVERTS[1], value = TRUE)
  for (x in 2:length(TERMS)){
    t1 <- grep(TERMS[x], t1, invert = INVERTS[x], value = TRUE)
  }
  if(length(t1)==1){
    return(t1)
  } else {
    print("Need more terms to find singular match")
    print(INVEC)
    print(TERMS)
    return(NA)
  }
}

#' Test if a column exists
#'
#' @param COLNM a column name
#' @return a vector from a column
#' @rdname colExtant
#' @export

colExtant <- function(COLNM, SHEET){
  if(length(unique(COLNM))!=0){
    if(!is.na(COLNM)){
      toupper(SHEET[[COLNM]])
    }
    else{NA}
  }
  else{NA}
}

#' Make TAT
#'
#' @param A an element
#' @param B also an element
#' @return numeric or NA
#' @rdname tatNA
#' @export

tatNA <- function(A, B){
  unlist(lapply(seq_along(A), function(x){
    if(is.na(unlist(A)[x]) | is.na(unlist(B)[x])){
      NA
    } else {
      as.numeric(A -B)
    }
  }))
}

#' Make Test_Code from sheet NAME and other Test_Codes
#'
#' @param CODE column of codes extant in sheet ('test_code')
#' @param NAME of sheet (takes first element split on whitespace)
#' @return vector of values
#' @rdname test_coding
#' @export

test_coding <- function(CODE, NAME){
  def_code <- strsplit(NAME, " ")[[1]][1]
  unlist(lapply(seq_along(CODE), function(x){
    if(is.na(CODE[x])){
      return(def_code)
    } else {
      return(CODE[x])
    }
  }))
}

#' Make Year based on one of the 4 dates included
#'
#' @param Y1 date the first
#' @param Y2 date the second
#' @param Y3 date the 3rd
#' @param Y4 date the dth
#' @return single most represented year
#' @rdname year_shootout
#' @export

year_shootout <- function(Y1, Y2, Y3, Y4, NAME){
  unlist(lapply(seq_along(Y1), function(x){
    ys <- unique(na.omit(Y1[x], Y2[x], Y3[x], Y4[x]))
    if(length(ys)==0){
      return(NA)
    } else {
      uys <- unique(unlist(lapply(ys, lubridate::year)))
      return(rev(uys)[1])
    }
  }))
}

#' Parse mutation status into correct format
#' ensures that data is formatted correctly
#' @return a Tibble object
#' @rdname mutationStatus
#' @importFrom magrittr '%>%'
#' @export

mutationStatus <- function(INPUT){
  braf_v600e_match <- c("BRAF V600E MUT", "BRAF V600E", "BRAF V600/E", "BRAF V600E/E2/D", "BRAF V600E/E2/E2D", "BRAF V600/E2/D", "BRAFV600E", "BRAFV600E/E2/D", "MUT BRAFV600E/E2/D", "MUT BRAF V600E/E2/D")
  braf_v600k_match <- c("MUT BRAF V600K", "BRAF VK00K", "BRAFV600K")
  braf_v600_match <- c("BRAF V600 MUT", "BRAF MUT V600", "BRAFV600", "BRAFV600R + BRAF K601E", "BRAF V600", "BRAF V600R")
  braf_mut_match <- c("BRAF MUT", "OTHER BRAF MUT", "BRAFMUT")
  nras_q61x_match <- c("NRASQ61X", "Q61X", "MUT NRAS Q61X", "NRASQ16X", "NRAS Q61X")
  del_19_match <- c("EGFR EX19DEL", "EXON 19", "EXON 19 DELETION", "EX19DEL", "EXON19 DEL", "EXON 19 DEL", "EXON19DEL", "EX19 DEL", "EX 19 DEL", "EX 19DEL")
  del_19_t790m_match <- c("EX19DEL + T790M", "EX19DEL,T790M")
  ins_20_match <- c("EX20INS")
  codon_1213_match <- c("KRAS CODON 12/13", "KRAS CODON 12/13 MUT", "KRAS MUT 12/13", "KRAS 12 MUT", "KRAS CODON12 MUT", "KRAS MUT CODON 13", "KRAS12MUT", "KRAS13MUT", "CODON 12/13 MUT", "CODON12/13", "CODON 12.13 MUT", "CODON 12/13", "MUT12/13", "MUT CODON 12/13", "NRAS 12/13 MUT", "MUT 12/13")
  codon_61_match <- c("CODON 61","CODON 61 MUT", "NRAS61", "61AAA", "AAA MUT", "MUT CODON 61", "NRAS 61 MUT", "NRAS 61 MUT CTA", "KRAS MUT 61", "MUT 61", "MUT 61 CGA", "NRAS CODON 61")
  codon_121361_match <- c("CODON 12/13, CODON 61", "CODON12/13 +61")
  codon_117_match <- c("MUT 117", "KRAS117 MUT", "61AAA", "AAA MUT")
  repeat_match <- c("RPT", "REPEAT", "FOR REPEAT", "MACRODISSECT AND REPEAT", "NEXT WEEK RUN", "RPT NEXT WEEK, NO DNA AT EXTRACTION", "**BACKGROUND FPR RPT", "?MUT RPT", "? LOW LEVEL MUT FOR RPT", "MACRODISSECT AND RPT", "RPT NO STOCK REXET", "INVALID- FOR REPEAT", "INVALID FOR RPT#", "INVALID FOR RPT", "INVALID FOR REEXTRACTION", "INVALID - FO RPT", "FOR REPEAT EXTRACTION NEW BLOCK", "FOR REPEAT EXTRACTION")
  invalid_match <- c("IN VALID", "INVALID X2", "INVALID X3", "WHOLE SAMPLE SIGNED OUT AS INVALID BY KS 6.9.16", "(PRE CUT SECTIONS RECEIVED) NO BLOCK")

  replace(toupper(INPUT$Mutation), toupper(INPUT$Mutation) %in% c("0"), NA) %>%
  replace(., . %in% "N/A", NA) %>%
  replace(., . %in% "no mut", "NO MUT") %>%
  replace(., substr(.,1,2)=="NO", "NO MUT") %>%
  replace(., . %in% repeat_match, "REPEAT") %>%
  replace(., . %in% invalid_match, "INVALID") %>%
  replace(., substr(.,1,5)=="INSUF", "INSUFFICIENT") %>%
  replace(., . %in% braf_v600e_match, "BRAF V600E") %>%
  replace(., . %in% braf_v600k_match, "BRAF V600K") %>%
  replace(., . %in% braf_v600_match, "BRAF V600 OTHER") %>%
  replace(., . %in% braf_mut_match, "BRAF OTHER") %>%
  replace(., . %in% nras_q61x_match, "Q61X") %>%
  replace(., grep("G12X", .), "G12X") %>%
  replace(., grep("G13X", .), "G13X") %>%
  replace(., grep("L858R", .), "L858R") %>%
  replace(., grep("G719X", .), "EXON 18 G719X") %>%
  replace(., . %in% del_19_match, "EXON 19 DEL") %>%
  replace(., . %in% del_19_t790m_match, "EXON 19 DEL + T790M") %>%
  replace(., . %in% ins_20_match, "EXON 20 INS") %>%
  replace(., . %in% codon_1213_match, "CODON 12/13") %>%
  replace(., . %in% codon_121361_match, "CODON 12/13 + 61") %>%
  replace(., . %in% codon_61_match, "CODON 61") %>%
  replace(., . %in% codon_117_match, "CODON 117") %>%
  replace(., is.na(.), "-")
}

#' Parse referring hospital into correct format
#' ensures that data is formatted correctly
#' @return a Tibble object
#' @rdname refHospital
#' @importFrom magrittr '%>%'
#' @export

refHospital <- function(INPUT){
  beaumont_match <- c("BH", "BEAUMOUNT")
  blackrock_match <- c("BC", "BRC", "BLACKROCK")
  galway_match <- c("GALWAY", "GC 2586/18 A1", "GC")
  mater_match <- c("MMUH", "M")

  replace(toupper(INPUT$`Hospital`), is.na(toupper(INPUT$`Hospital`)), "SVUH") %>%
  replace(., . %in% beaumont_match, "BEAUMONT") %>%
  replace(., . %in% blackrock_match, "BLACKROCK CLINIC") %>%
  replace(., . %in% mater_match, "MMUH") %>%
  replace(., . %in% c("SVUHP"), "SVPH") %>%
  replace(., . %in% c("SLIGO"), "SGH") %>%
  replace(., . %in% c("LIMERICK"), "LRH") %>%
  replace(., . %in% c("RVEE"), "RVEEH") %>%
  replace(., . %in% c("KERRY GEN"), "KGH") %>%
  replace(., . %in% galway_match, "GALWAY CLINIC") %>%
  replace(., is.na(.), "-")
}

#' Parse site source into correct format
#' ensures that data is formatted correctly
#' @return a Tibble object
#' @rdname siteSource
#' @export

siteSource <- function(INPUT){
  replace(toupper(INPUT$`Source`), substr(toupper(INPUT$`Source`), 1, 1)=="R", "Resection") %>%
  replace(., substr(., 1, 1)=="S", "Surgical") %>%
  replace(., substr(., 1, 1)=="B", "Biopsy") %>%
  replace(., substr(., 1, 1)=="C", "Cytology") %>%
  replace(., . %in% NA, "-")
}

#' Parse macrodissection into correct format
#' ensures that data is formatted correctly
#' @return a Tibble object
#' @rdname macroDissect
#' @importFrom magrittr '%>%'
#' @export

macroDissect <- function(INPUT){
    replace(toupper(INPUT$`Macrod.`), toupper(INPUT$`Macrod.`) %in% 0, NA) %>%
    replace(., substr(.,1,1)=="Y", "YES") %>%
    replace(., substr(.,1,1)=="N", "NO") %>%
    replace(., . %in% "MO", "NO") %>%
    replace(., . %in% NA, "-")
}

#' Parse site code
#' ensures that data is formatted correctly
#' @return a Tibble object
#' @rdname siteCode
#' @export

siteCode <- function(INPUT){
  abd_match <- c("ABD", "ABDOMINAL MASS")
  adr_match <- c("ADR", "ADRENAL")
  anal_match <- c("ANAL BX", "ANALX", "ANUS")
  axil_match <- c("AX", "AXIL", "AXILLARY", "AXILLARY DISSECTION", "AXLN", "AXLNX")
  bld_match <- c("BDX", "BL", "BLADDER", "BLDX", "BLX")
  bone_match <- c("BONE", "BONE BX", "BONEX", "BONX")
  breast_match <- c("BREAST", "BREAST BX", "BREASTBX", "BREASTX", "BREX", "BREXL", "BREXR")
  bron_match <- c("BRONX", "BROX", "BROXWASH")
  chest_match <- c("CHEST", "CHEST WALL BX")
  col_match <- c("COL", "COLC", "COLO", "COLOM", "COLON", "COLR", "COLRX", "COLX", "CORLX", "CRC")
  colp_match <- c("COLP", "COLP3")
  conj_match <- c("CONJ", "CONJUNCTIVA", "CONJUNCTIVAL BIOPSY", "CONJUNCTIVAL LESION", "CONJUNTIVAL LESION")
  cyto_match <- c("CYTO", "CYTO FNA")
  duo_match <- c("DUO", "DUOX")
  ebus_match <- c("EBUS", "EBUS-FNA", "EBUS FNA", "EBUS LN")
  ec_match <- c("EC", "EMCX", "EMX")
  endobro_match <- c("ENDOBRO FNA", "ENDOBRON")
  eye_match <- c("EYE", "EYE BX", "EYEX")
  femur_match <- c("FEM", "FEMORAL", "FEMUR")
  groin_match <- c("GROIN", "GROIN BX")
  liver_match <- c("LIV", "LIVE", "LIVER", "LIVER BX", "LIVERX", "LIVX")
  lung_match <- c("LNX", "LUN", "LUNG", "LUNG BX", "LUNGX", "LUNX")
  muscle_match <- c("MUSCLE", "MUSX")
  omen_match <- c("OMEN", "OMENTAL", "OMENENTENAL", "OMENTAL BX", "OMENTUM", "OMENX")
  ovary_match <- c("OVA", "OVAR", "OVARIAN", "OVARY")
  panc_match <- c("PAN", "PANC", "PANCA", "PANCREAS", "PANX")
  parotid_match <- c("PAR", "PARATOID", "PAROTID", "PART", "PARTOID", "PARTOID GLAND")
  pelvic_match <- c("PEL", "PELVIC", "PELVIC LESION", "PELVIC BIOPSY", "PELVIC BX", "PELVIC MASS BX", "PELVIS", "PELVIX BX", "PELVX")
  peritoneum_match <- c("PERITENIUMX", "PERITINEAL", "PERITINEAL BX", "PERITONEAL", "PERIOTNEAL BX", "PERITONEUM", "PERITX", "PERT", "PERTX")
  pleura_match <- c("PLEAURA", "PLEURA", "PLEURAL", "PLEURAL BX", "PLEURAL FL", "PLEURAL FLUID", "PLEUX", "PLUF", "PLURAL", "PLURAL MASS BX", "PLUX")
  rectum_match <- c("REC", "RECTAL", "RECTAL BX", "RECTUM", "RECX")
  skin_match <- c("SK", "SK PUNCH", "SKEX", "SKEX1", "SKIN", "SKPIN", "SKPX", "SKTX")
  soft_match <- c("SOFT", "SOFTC", "SOFTX", "SOFT TISSUE")
  vagina_match <- c("VAG", "VAGINAL", "VAGX")
  vulva_match <- c("VULVA", "VULVX")

  replace(toupper(INPUT$`Site`), toupper(INPUT$`Site`) %in% abd_match, "ABDOMEN") %>%
  replace(., . %in% adr_match, "ADRENAL") %>%
  replace(., . %in% anal_match, "ANUS") %>%
  replace(., . %in% axil_match, "AXILLARY") %>%
  replace(., . %in% bld_match, "BLADDER") %>%
  replace(., . %in% bone_match, "BONE") %>%
  replace(., . %in% breast_match, "BREAST") %>%
  replace(., . %in% bron_match, "BRONCHUS") %>%
  replace(., . %in% chest_match, "CHEST") %>%
  replace(., . %in% col_match, "COLON") %>%
  replace(., . %in% colp_match, "COLP") %>%
  replace(., . %in% conj_match, "CONJUNCTIVA") %>%
  replace(., . %in% cyto_match, "CYTO") %>%
  replace(., . %in% duo_match, "DUODENUM") %>%
  replace(., . %in% ebus_match, "EBUS") %>%
  replace(., . %in% ec_match, "ENDOMETRIUM") %>%
  replace(., . %in% endobro_match, "ENDOBRO") %>%
  replace(., . %in% eye_match, "EYE") %>%
  replace(., . %in% femur_match, "FEMUR") %>%
  replace(., . %in% groin_match, "GROIN") %>%
  replace(., . %in% liver_match, "LIVER") %>%
  replace(., . %in% lung_match, "LUNG") %>%
  replace(., . %in% muscle_match, "MUSCLE") %>%
  replace(., . %in% omen_match, "OMENTUM") %>%
  replace(., . %in% ovary_match, "OVARY") %>%
  replace(., . %in% panc_match, "PANCREAS") %>%
  replace(., . %in% parotid_match, "PAROTID") %>%
  replace(., . %in% pelvic_match, "PELVIS") %>%
  replace(., . %in% peritoneum_match, "PERITONEUM") %>%
  replace(., . %in% pleura_match, "PLEURA") %>%
  replace(., . %in% rectum_match, "RECTUM") %>%
  replace(., . %in% skin_match, "SKIN") %>%
  replace(., . %in% soft_match, "SOFT") %>%
  replace(., . %in% vagina_match, "VAGINA") %>%
  replace(., . %in% vulva_match, "VULVA") %>%
  replace(., . %in% NA, "-")
}

#' Allow choice to return 'other' as a value from summary tables
#' @return a Tibble object
#' @rdname otherSummary
#' @export

otherSummary <- function(COLNM, CHOICE){
  unlist(lapply(COLNM, function(f){
    if(f != CHOICE){ "OTHER" }
    else{ f }
  }))
}

#' Tests for previous input data, and/or takes input from user
#' ensures that data is formatted correctly, saved correctly
#' @return a Tibble object
#' @rdname inputData
#' @importFrom magrittr '%>%'
#' @export

parse_input <- function(INPUT, VALS_DATA){

  ##XLSX
  if(length(grep(".xlsx$", INPUT$FILENAMES$datapath[1]) > 0)){

    shiny::showModal(modalDialog("Reading XLSX input, please wait.\n", footer = NULL))

    shinySetupPostgreSQL::input_from_xlsx(INPUT, VALS_DATA)
    shinySetupPostgreSQL::obsev_valsdata_new(VALS_DATA)

    shiny::removeModal()

  }

  # ##RDS
  # if(length(grep(".rds$", INPUT$FILENAMES$datapath[1]) > 0)){
  #
  #   shiny::showModal(modalDialog("Reading RDS input, please wait.\n", footer = NULL))
  #
  #   tibList <- lapply(INPUT$FILENAMES$datapath, function(f){
  #     readRDS(f)
  #   })
  #
  #   tibList_nn <- Filter(Negate(is.null), tibList)
  #   vals_tib <- do.call(dplyr::bind_rows, tibList_nn)
  #
  #   #instead of rigid parsing, modal asks user to define what cols are what
  #   #this creates input$ntc_f, for f in 1:length(colnames(new_table_cols()))
  #   mod_map_columns(VALS_TIB = vals_tib)
  #   data_out <- obsev_go_map_table(INPUT = INPUT, VALS_TIB = vals_tib)
  #
  #   #still require parsing to be done
  #   shiny::removeModal()
  #
  #   return(data_out)
  # }

  if(length(grep(".pdf$", INPUT$FILENAMES$datapath[1]) > 0)){

    shiny::showModal(modalDialog("Reading CMD format PDF input, please wait.\n", footer = NULL))

    tibList <- lapply(INPUT$FILENAMES$datapath, function(f){
      import_cmd_pdfs(pdf_path = f)
    })

    tibList_nn <- Filter(Negate(is.null), lapply(tibList, unlist))
    vals_tib <- dplyr::bind_rows(tibList_nn)
    VALS_DATA$New <- dplyr::distinct(vals_tib)

    shiny::removeModal()

  }
}

#' Read sheets from XLSX
#' @param FILENAME is the XLSX file to read sheets from
#' @return a list object containing Tibble elements
#' @rdname inputDat
#' @importFrom magrittr '%>%'
#' @export

read_sheets_to_list <- function(FILENAME) {

    ##define all sheets, could be modified to match/grep on an input variable
    sheets <- suppressMessages(readxl::excel_sheets(FILENAME))

    ##return the list of non-empty sheets, named as per original
    sheetsList <- lapply(sheets, function(f){

      ##need to specify cols which are Dates
      ##as they really fuck everything up so badly
       test_r <- suppressMessages(readxl::read_excel(FILENAME, sheet = f))
       if(dim(test_r)[1] > 0){
         coltypes <- rep("text", dim(test_r)[2])
         coltypes[grep("DATE", toupper(colnames(test_r)))] <- "date"
         suppressMessages(readxl::read_excel(FILENAME, sheet = f, col_types = coltypes))
       }
    })
    names(sheetsList) <- sheets
    return(sheetsList)
}

#' Input XLSX file parsing
#' @param INPUT object with FILENAMES$datapath
#' @return list of input sheets matching sheet_grep_string()
#' @rdname input_xlsx
#' @importFrom magrittr '%>%'
#' @export

input_xlsx <- function(INPUT){

  tibList <- lapply(INPUT$FILENAMES$datapath, function(f){
    sheetList <- shinySetupPostgreSQL::read_sheets_to_list(f)
    nsheetList <- names(sheetList)
    nsl <- lapply(nsheetList, function(ff){
      if(length(grep(sheet_grep_string(), toupper(ff))) > 0){
        #instead of rigid parsing, modal asks user to define what cols are what
        #this creates input$ntc_f, for f in 1:length(colnames(new_table_cols()))
        list(sheetList[[ff]], ff)
      }
    })
    Filter(Negate(is.null), nsl)
  })

  ##set into single list
  nms <- c()
  tiblist <- list()
  xx <- 0
  for(x in 1:length(tibList)){
    for(y in 1:length(tibList[[x]])){
      xx <- xx + 1
      nms <- c(nms, tibList[[x]][[y]][[2]])
      tiblist[[xx]] <- tibList[[x]][[y]][[1]]
    }
  }
  names(tiblist) <- nms
  return(tiblist)
}

#' Input XLSX file parsing
#' @param INPUT object
#' @param VALS_DATA data values reactive
#' @return list of input sheets matching sheet_grep_string()
#' @rdname input_from_xlsx
#' @importFrom magrittr '%>%'
#' @export

input_from_xlsx <- function(INPUT, VALS_DATA){

  print("gets in")
  ##read in sheets from all fiiles first, holding in list structure
  shets <- shiny::reactiveValues(sh = 0, ix = NULL, mx = NULL, tb = NULL)

  an_shet <- shiny::eventReactive(shets$sh, {

    ##read files and list initially

    if(shets$sh == 0){
      shets$ix <- input_xlsx(INPUT)
      shets$mx <- length(shets$ix)
      print(paste0("Read in total usable sheets: ", shets$mx))

      ##increment to begin parsing these
      shets$sh <- shets$sh + 1
    } else{
      names(shets$ix)[shets$sh]
    }
  })

  shiny::observeEvent(an_shet(), ignoreInit = TRUE, {

    ##grep out colnames that contain ..., these are empty
    cnam <- colnames(shets$ix[[an_shet()]])[grep("\\.\\.\\.", colnames(shets$ix[[an_shet()]]), invert = TRUE)]

    ##define a vals_tib element of VD, adding in Test and empty cols
    VALS_DATA[[paste0(an_shet(),"_vals_tib")]] <- dplyr::select(.data = shets$ix[[an_shet()]], !!cnam) %>%
    dplyr::mutate(empty = rep(NA, dim(shets$ix[[an_shet()]])[1]),
                  Test = rep(an_shet(), dim(shets$ix[[an_shet()]])[1]))

    ##map the columns extant into our colnames for table
    shinySetupPostgreSQL::mod_map_columns(INPUT = INPUT,
                                          VALS_TIB = VALS_DATA[[paste0(an_shet(), "_vals_tib")]],
                                          NAME = an_shet())
  })

  shiny::observeEvent(INPUT$go_map_table, {
    print("go_map_table")
    shiny::removeModal()

    ##mapping select
    tibList <-
    lapply(colnames(new_table_cols())[-1], function(f){

      ntcf <- paste0("ntc_", f)

      ##select col and renamme
      dplyr::select(.data = VALS_DATA[[paste0(an_shet(), "_vals_tib")]], !!f := !!INPUT[[ntcf]])

    })

    ##combine into tibble
    shets$tb <- dplyr::bind_rows(shets$tb, do.call(dplyr::bind_cols, tibList))

    ##continue to allow other sheets to be read until there are no more
    ##then define VALS_DATA$Xlsx
    if(shets$sh == shets$mx){
      print(paste0("Finished working on: ", shets$sh, " / ", shets$mx))
      VALS_DATA$New <- dplyr::filter(.data = tibble::as_tibble(shets$tb),
        rlang::eval_tidy(rlang::parse_expr(paste(
          paste("!", new_table_reqs(), " %in% c(NA, \"-\")"),
          collapse = " & "))))
      shinySetupPostgreSQL::obsev_valsdata_new(VALS_DATA)

    } else {
      print(paste0("Working on: ", shets$sh, " / ", shets$mx))
      shets$sh <- shets$sh + 1
    }
  })
}
