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

#' General parser for data structures incoming
#' ensures that data is formatted correctly
#' @return a Tibble object
#' @rdname parseGeneral
#' @importFrom magrittr '%>%'
#' @export

parseGeneral <- function(SHEET, NAME){

  print(paste0("Working on: ", NAME))

  SHEET1 <- SHEET
  colnames(SHEET1) <- toupper(colnames(SHEET1))

  ##parsing out unique colnames for each desired column
  forenms <- grepSpec(colnames(SHEET1), c("PATIENT", "NAME", "SURNAME"), c(FALSE, FALSE, TRUE))
  surnms <- grepSpec(colnames(SHEET1), c("PATIENT", "NAME", "SURNAME"), c(FALSE, FALSE, FALSE))
  lab_ids <- grepSpec(colnames(SHEET1), c("LAB", "SVUH"), c(FALSE, TRUE))
  svuh_ids <- grep("SVUH LAB", colnames(SHEET1), value=TRUE)
  hosp_nos <- grepSpec(colnames(SHEET1), c("HOSP", "NO"), c(FALSE, FALSE))
  hosp_ids <- grepSpec(colnames(SHEET1), c("HOSP", "NO"), c(FALSE, TRUE))
  tissue_codes <- grep("TISSUE CODE", colnames(SHEET1), value=TRUE)
  mutation_statuss <- grep("MUTATION", colnames(SHEET1), value=TRUE)
  date_reqs <- grep("DATE REQ", colnames(SHEET1), value=TRUE)
  date_auths <- grep("AUTHORISED", colnames(SHEET1), value=TRUE)

  canc_types <- grep(pattern="CANCER|CRC", colnames(SHEET1), value=TRUE)[1]
  prim_mets <- grep("PRIMARY", colnames(SHEET1), value=TRUE)
  tiss_sources <- grep("SURGICAL", colnames(SHEET1), value=TRUE)
  macrodissects <- grep("ACRODIS", colnames(SHEET1), value=TRUE)
  test_codes <- grep("TEST", colnames(SHEET1), value=TRUE)
  date_recs <- grep("DATE RCD", colnames(SHEET1), value=TRUE)
  date_reps <- grep("DATE REP", colnames(SHEET1), value=TRUE)

  wanted_vars <- c(forenms, surnms, lab_ids[1], svuh_ids, hosp_nos, hosp_ids[1],
                   tissue_codes, mutation_statuss, date_reqs, date_auths)

  ##no lab_ids in EXTERNALS...
  tatNA_v <- Vectorize(tatNA)
  SHEET11 <- SHEET1 %>%
              dplyr::mutate(forenm = colExtant(forenms, SHEET1)) %>%
              dplyr::mutate(surnm = colExtant(surnms, SHEET1)) %>%
              dplyr::mutate(lab_id = colExtant(lab_ids, SHEET1)) %>%
              dplyr::mutate(svuh_id = colExtant(svuh_ids, SHEET1)) %>%
              dplyr::mutate(hosp_no = colExtant(hosp_nos, SHEET1)) %>%
              dplyr::mutate(hosp_id = colExtant(hosp_ids, SHEET1)) %>%
              dplyr::mutate(tissue_code = colExtant(tissue_codes, SHEET1)) %>%
              dplyr::mutate(mut_status = colExtant(mutation_statuss, SHEET1)) %>%
              dplyr::mutate(date_req = colExtant(date_reqs, SHEET1)) %>%
              dplyr::mutate(date_auth = colExtant(date_auths, SHEET1)) %>%
              dplyr::mutate(canc_type = colExtant(canc_types, SHEET1)) %>%
              dplyr::mutate(pri_met = colExtant(prim_mets, SHEET1)) %>%
              dplyr::mutate(tiss_source = colExtant(tiss_sources, SHEET1)) %>%
              dplyr::mutate(macrodissect = colExtant(macrodissects, SHEET1)) %>%
              dplyr::mutate(test_code = colExtant(test_codes, SHEET1)) %>%
              dplyr::mutate(date_rec = colExtant(date_recs, SHEET1)) %>%
              dplyr::mutate(date_rep = colExtant(date_reps, SHEET1)) %>%
              dplyr::mutate(surnm = gsub(" ", "", surnm)) %>%
              tidyr::separate(., svuh_id, into = c("svuh_lab_id", "svuh_block_id"), sep="[[:space:]]", extra = "merge", fill = "right") %>%
              dplyr::mutate(lab_id_upper = toupper(lab_id)) %>%
              dplyr::mutate(svuh_block_id = gsub("[[:space:]]", "", svuh_block_id)) %>%
              dplyr::mutate(lab_id_upper = replace(lab_id_upper, lab_id_upper %in% NA, "-")) %>%
              dplyr::mutate(svuh_lab_id = replace(svuh_lab_id, svuh_lab_id %in% NA, "-")) %>%
              dplyr::mutate(svuh_block_id = replace(svuh_block_id, svuh_block_id %in% NA, "-")) %>%
              dplyr::mutate(hosp_id = replace(hosp_id, hosp_id %in% NA, "-")) %>%
              dplyr::mutate(hosp_no = replace(hosp_no, hosp_no %in% NA, "-")) %>%
              dplyr::mutate(tissue_code = replace(tissue_code, tissue_code %in% NA, "-")) %>%
              dplyr::mutate(mut_status = replace(mut_status, mut_status %in% NA, "-")) %>%
              dplyr::mutate(canc_type = replace(canc_type, canc_type %in% NA, "-")) %>%
              dplyr::mutate(pri_met = replace(pri_met, pri_met %in% NA, "-")) %>%
              dplyr::mutate(tiss_source = replace(tiss_source, tiss_source %in% NA, "-")) %>%
              dplyr::mutate(macrodissect = replace(macrodissect, macrodissect %in% NA, "-")) %>%
              dplyr::mutate(test_code = replace(test_code, test_code %in% NA, "-")) %>%
              dplyr::mutate(date_auth = lubridate::ymd(date_auth)) %>%
              dplyr::mutate(date_req = lubridate::ymd(date_req)) %>%
              dplyr::mutate(date_rec = lubridate::ymd(date_rec)) %>%
              dplyr::mutate(date_rep = lubridate::ymd(date_rep)) %>%
              dplyr::mutate(tat = tatNA_v(date_auth, date_req)) %>%
              dplyr::mutate(tat_ext = tatNA_v(date_rep, date_rec)) %>%
              dplyr::mutate(Year = year_shootout(date_req, date_rec, date_rep, date_auth)) %>%
              dplyr::mutate(test_coded = test_coding(test_code, NAME)) %>%
              dplyr::select(Year,
                            "Forename" = forenm,
                            "Surname" = surnm,
                            "Lab ID" = lab_id_upper,
                            "SVUH Lab No." = svuh_lab_id,
                            "Block" = svuh_block_id,
                            "Hosp. No." = hosp_no,
                            "Hospital" = hosp_id,
                            "Cancer" = canc_type,
                            "Mutation" = mut_status,
                            "Test" = test_coded,
                            "Pri/Met" = pri_met,
                            "Tissue" = tissue_code,
                            "Source" = tiss_source,
                            "Macrod." = macrodissect,
                            Date_Requested = date_req,
                            Date_Ext_Rec = date_rec,
                            Date_Ext_Rep = date_rep,
                            Date_Authorised = date_auth,
                            TAT = tat,
                            TAT_ext = tat_ext) %>%
    dplyr::filter(!.[[2]] %in% NA & !.[[3]] %in% NA & !.[[5]] %in% "-" & !.[[7]] %in% "-" & !.[[10]] %in% "-")
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

#' Parse tissue source into correct format
#' ensures that data is formatted correctly
#' @return a Tibble object
#' @rdname tissueSource
#' @export

tissueSource <- function(INPUT){
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

#' Parse tissue code
#' ensures that data is formatted correctly
#' @return a Tibble object
#' @rdname tissueCode
#' @export

tissueCode <- function(INPUT){
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

  replace(toupper(INPUT$`Tissue`), toupper(INPUT$`Tissue`) %in% abd_match, "ABDOMEN") %>%
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

#' Allow choice to return 'other' as a value fro summary tables
#' @return a Tibble object
#' @rdname otherSummary
#' @export

otherSummary <- function(COLNM, CHOICE){
  unlist(lapply(COLNM, function(f){
    if(f != CHOICE){ "OTHER" }
    else{ f }
  }))
}

#' Rename parsed data into output format
#' ensures that data is formatted correctly
#' @return a Tibble object
#' @rdname renameParse
#' @importFrom magrittr '%>%'
#' @export

renameParse <- function(INPUT){

  print("renameParse")

  INPUT %>% dplyr::mutate(mut_status = mutationStatus(.)) %>%
  dplyr::mutate(hosp_id = refHospital(.)) %>%
  dplyr::mutate(tiss_source = tissueSource(.)) %>%
  dplyr::mutate(tiss_code = tissueCode(.)) %>%
  dplyr::mutate(macrods = macroDissect(.)) %>%
  dplyr::mutate(pri_mets = replace(`Pri/Met`, substr(`Pri/Met`, 1, 1)=="P", "Primary")) %>%
  dplyr::mutate(pri_mets = replace(pri_mets, substr(pri_mets, 1, 1)=="M", "Metastasis")) %>%
  dplyr::mutate(canc_type = replace(`Cancer`, substr(toupper(`Cancer`), 1, 3)=="LUN", "LUNG")) %>%
  dplyr::mutate(canc_type = replace(canc_type, substr(toupper(canc_type), 1, 2)=="LN", "LUNG")) %>%
  dplyr::mutate(canc_type = replace(canc_type, substr(toupper(canc_type), 1, 3)=="THY", "THYROID")) %>%
  dplyr::mutate(canc_type = replace(canc_type, substr(toupper(canc_type), 1, 3)=="PAP", "PAPILLARY")) %>%
  dplyr::mutate(canc_type = replace(canc_type, substr(toupper(canc_type), 1, 3)=="MEL", "MELANOMA")) %>%
  dplyr::mutate(canc_type = replace(canc_type, substr(toupper(canc_type), 1, 3)=="OTH", "OTHER")) %>%
  dplyr::mutate(canc_type = replace(canc_type, substr(toupper(canc_type), 1, 3)=="DUO", "DUODENAL")) %>%
  dplyr::mutate(canc_type = replace(canc_type, substr(toupper(canc_type), 1, 3)=="CYT", "CYTOLOGY")) %>%
  dplyr::select("Year",
                "Forename",
                "Surname",
                "Lab ID",
                "SVUH Lab No.",
                "Block",
                "Hosp. No.",
                "Hospital" = hosp_id,
                "Cancer" = canc_type,
                "Mutation" = mut_status,
                "Test",
                "Pri/Met" = pri_mets,
                "Tissue" = tiss_code,
                "Source" = tiss_source,
                "Macrod." = macrods,
                Date_Requested,
                Date_Ext_Rec,
                Date_Ext_Rep,
                Date_Authorised,
                TAT, TAT_ext) %>%
  dplyr::distinct()
}

#' Tests for previous input data, and/or takes input from user
#' ensures that data is formatted correctly, saved correctly
#' @return a Tibble object
#' @rdname inputData
#' @importFrom magrittr '%>%'
#' @export

parse_input <- function(INPUT){

  ##XLSX
  if(length(grep(".xlsx$", INPUT$FILENAMES$datapath[1]) > 0)){

    shiny::showModal(modalDialog("Reading XLSX input, please wait.\n", footer = NULL))

    data_out <- input_from_xlsx(INPUT)
    shiny::removeModal()

    return(data_out)
  }

  ##RDS
  if(length(grep(".rds$", INPUT$FILENAMES$datapath[1]) > 0)){

    shiny::showModal(modalDialog("Reading RDS input, please wait.\n", footer = NULL))

    tibList <- lapply(INPUT$FILENAMES$datapath, function(f){
      readRDS(f)
    })
    vals_tib <- do.call(dplyr::bind_rows, tibList)
    data_out <- renameParse(vals_tib) %>%
                dplyr::distinct()
    shiny::removeModal()

    return(data_out)
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

#' Input from XLSX
#' @param INPUT object with FILENAMES$datapath
#' @return data.frame of input
#' @rdname input_from_xlsx
#' @importFrom magrittr '%>%'
#' @export

input_from_xlsx <- function(INPUT){

  tibList <- lapply(INPUT$FILENAMES$datapath, function(f){
    sheetList <- read_sheets_to_list(f)
    nsheetList <- names(sheetList)
    lapply(nsheetList, function(ff){
      toMatch <- c()
      if(length(grep("KRAS |BRAF |NRAS |EGFR |EXTERNAL", toupper(ff))) > 0){
        return(parseGeneral(SHEET = sheetList[[ff]], NAME = ff))
      }
    })
  })
  vals_tib <- do.call(dplyr::bind_rows, tibList)
  data_out <- renameParse(vals_tib) %>%
              dplyr::distinct()
  return(data_out)
}
