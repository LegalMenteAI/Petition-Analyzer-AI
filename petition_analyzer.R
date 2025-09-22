#!/usr/bin/env Rscript

# [CLI & Libraries] ----
suppressPackageStartupMessages({
  library(optparse)
  library(data.table)
  library(readr)
  library(pdftools)
  library(stringi)
  library(stringr)
  library(stringdist)
  library(httr)
  library(jsonlite)
  suppressWarnings({
    if (!requireNamespace("readxl", quietly = TRUE)) {
      message("[warn] 'readxl' not installed; .xlsx input will not be supported.")
    }
  })
})

option_list <- list(
  make_option("--pdf",    type = "character", help = "Path to scanned petition PDF", metavar="FILE"),
  make_option("--voters", type = "character", help = "Path to voter CSV/XLSX",        metavar="FILE"),
  make_option("--out",    type = "character", help = "Path to output CSV",            metavar="FILE"),
  make_option("--model",  type = "character", default = "gpt-4o",
              help = "OpenAI model for PDF extraction [default: %default]")
  #make_option("--api_key", type = "character", default = Sys.getenv("OPENAI_API_KEY",""),
  #            help = "OpenAI API key (or set OPENAI_API_KEY env var)")
)
args <- parse_args(OptionParser(option_list = option_list))
req  <- c("pdf","voters","out")
miss <- req[!nzchar(unlist(args[req]))]
if (length(miss)) stop("Missing required options: ", paste(miss, collapse=", "))

#if (nzchar(args$api_key)) Sys.setenv(OPENAI_API_KEY = args$api_key)
OPENAI_API_KEY = "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"          # or put "sk-..." directly
Sys.setenv(OPENAI_API_KEY = "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")          # or put "sk-..." directly


PDF_PATH    <- args$pdf
VOTER_PATH  <- args$voters
OUTPUT_PATH <- args$out
MODEL_NAME  <- args$model


cat(sprintf("[start] Script start at %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))




# [Load voter file] ----
t0 = Sys.time(); cat(sprintf("[task] Load voter file — start %s\n", format(t0, "%H:%M:%S")))
voters_ok <- TRUE
ext <- tolower(tools::file_ext(VOTER_PATH))
if (ext %in% c("csv","txt")) {
  voters_dt <- tryCatch(
    data.table::fread(VOTER_PATH, encoding = "UTF-8", showProgress = TRUE),
    error = function(e){ cat("[error] Failed to read CSV/TXT: ", e$message, "\n"); NULL }
  )
} else if (ext %in% c("xlsx","xls")) {
  if (!requireNamespace("readxl", quietly = TRUE)) {
    cat("[error] readxl not installed; cannot read Excel voter files.\n"); voters_dt <- NULL
  } else {
    tmp_tbl  <- tryCatch(readxl::read_excel(VOTER_PATH),
                         error = function(e){ cat("[error] Failed to read Excel: ", e$message, "\n"); NULL })
    voters_dt <- if (is.null(tmp_tbl)) NULL else data.table::as.data.table(tmp_tbl)
    rm(tmp_tbl)
  }
} else {
  cat("[error] Unsupported voter file type; use CSV or XLSX.\n"); voters_dt <- NULL
}
if (is.null(voters_dt)) voters_ok <- FALSE
t1 = Sys.time(); cat(sprintf("[task] Load voter file — end %s (%.1f s)\n",
                             format(t1, "%H:%M:%S"),
                             as.numeric(difftime(t1, t0, units="secs"))))





# [Validate columns - TUHSD Tolleson Union High School District] ----
t0 = Sys.time(); cat(sprintf("[task] Validate columns — start %s\n", format(t0, "%H:%M:%S")))
if (voters_ok) {
  required_cols <- c("Last Name","First Name","Middle Name","Suffix Name",
                     "Primary Address","City","State","Zip","PrimaryZip4","Precinct Name")
  cn <- names(voters_dt)
  map_idx <- match(tolower(required_cols), tolower(cn))
  if (any(is.na(map_idx))) {
    missing <- required_cols[is.na(map_idx)]
    cat("[error] Voter file missing required columns: ", paste(missing, collapse=", "), "\n")
    voters_ok <- FALSE
  } else {
    cols <- cn[map_idx]
    to_chr_empty <- function(x){ y <- as.character(x); y[is.na(y)] <- ""; y }
    last_name    <- to_chr_empty(voters_dt[[cols[which(required_cols=="Last Name")]]])
    first_name   <- to_chr_empty(voters_dt[[cols[which(required_cols=="First Name")]]])
    middle_name  <- to_chr_empty(voters_dt[[cols[which(required_cols=="Middle Name")]]])
    suffix_name  <- to_chr_empty(voters_dt[[cols[which(required_cols=="Suffix Name")]]])
    primary_addr <- to_chr_empty(voters_dt[[cols[which(required_cols=="Primary Address")]]])
    city_raw     <- to_chr_empty(voters_dt[[cols[which(required_cols=="City")]]])
    state_raw    <- to_chr_empty(voters_dt[[cols[which(required_cols=="State")]]])
    zip_raw      <- to_chr_empty(voters_dt[[cols[which(required_cols=="Zip")]]])
    zip4_raw     <- to_chr_empty(voters_dt[[cols[which(required_cols=="PrimaryZip4")]]])
    precinct     <- to_chr_empty(voters_dt[[cols[which(required_cols=="Precinct Name")]]])
  }
}
t1 = Sys.time(); cat(sprintf("[task] Validate columns — end %s (%.1f s)\n",
                             format(t1, "%H:%M:%S"),
                             as.numeric(difftime(t1, t0, units="secs"))))





# [Compose canonical voter fields] ----
t0 <- Sys.time(); cat(sprintf("[task] Compose canonical voter fields — start %s\n", format(t0, "%H:%M:%S")))
if (voters_ok) {
  to_chr_empty <- function(x){ y <- as.character(x); y[is.na(y)] <- ""; y }
  first_name  <- to_chr_empty(first_name)
  middle_name <- to_chr_empty(middle_name)
  last_name   <- to_chr_empty(last_name)
  suffix_name <- to_chr_empty(suffix_name)
  primary_addr<- to_chr_empty(primary_addr)
  city_vec    <- to_chr_empty(city_raw)
  state_vec   <- to_chr_empty(state_raw)
  zip_vec     <- to_chr_empty(zip_raw)
  zip4_vec    <- to_chr_empty(zip4_raw)
  precinct_vec<- to_chr_empty(precinct)
  
  voters_dt[, voter_id := sprintf("row_%d", .I)]
  name_raw_vec <- stringr::str_squish(paste(first_name, middle_name, last_name))  # (suffix kept separately)
  zip5_vec     <- stringr::str_extract(zip_vec,  "\\d{5}")
  zip4_vec_ex  <- stringr::str_extract(zip4_vec, "\\d{4}")
  
  data.table::set(voters_dt, j="suffix_raw",    value = suffix_name)
  data.table::set(voters_dt, j="name_raw",      value = name_raw_vec)
  data.table::set(voters_dt, j="street_raw",    value = primary_addr)
  data.table::set(voters_dt, j="city_raw",      value = city_vec)
  data.table::set(voters_dt, j="state_raw",     value = state_vec)
  data.table::set(voters_dt, j="zip_raw",       value = ifelse(!is.na(zip5_vec), zip5_vec, NA_character_))
  data.table::set(voters_dt, j="zip4_raw",      value = zip4_vec_ex)
  data.table::set(voters_dt, j="precinct_name", value = precinct_vec)
  rm(name_raw_vec, zip5_vec, zip4_vec_ex)
}
t1 <- Sys.time(); cat(sprintf("[task] Compose canonical voter fields — end %s (%.1f s)\n",
                              format(t1, "%H:%M:%S"),
                              as.numeric(difftime(t1, t0, units="secs"))))





# [Normalize names] ----
t0 <- Sys.time(); cat(sprintf("[task] Normalize names — start %s\n", format(t0, "%H:%M:%S")))
if (voters_ok) {
  fn_raw  <- voters_dt[["First Name"]]
  mn_raw  <- voters_dt[["Middle Name"]]
  ln_raw  <- voters_dt[["Last Name"]]
  sfx_raw <- voters_dt[["Suffix Name"]]
  
  to_chr_empty <- function(x){ y <- as.character(x); y[is.na(y)] <- ""; y }
  norm_part <- function(x){
    x <- to_chr_empty(x)
    x <- stringr::str_replace_all(x, "[\\.,\\-]", " ")
    x <- stringr::str_replace_all(x, "'", "")
    x <- stringi::stri_trans_general(x, "Any-Latin; Latin-ASCII")
    x <- stringr::str_to_lower(x)
    x <- stringr::str_squish(x)
    x
  }
  
  fn <- norm_part(fn_raw)
  mn <- norm_part(mn_raw)
  ln <- norm_part(ln_raw)     # lastn_normalized from voter file Last Name only
  sfx<- norm_part(sfx_raw)
  
  name_norm_vec <- stringr::str_squish(paste(fn, mn, ln, sfx))
  name_norm_vec[!nzchar(name_norm_vec)] <- NA_character_
  
  voters_dt[, name_normalized   := name_norm_vec]
  voters_dt[, firstn_normalized := ifelse(nzchar(fn), fn, NA_character_)]
  voters_dt[, lastn_normalized  := ifelse(nzchar(ln), ln, NA_character_)]
  
  rm(fn_raw, mn_raw, ln_raw, sfx_raw, fn, mn, ln, sfx, name_norm_vec, to_chr_empty, norm_part)
}
t1 <- Sys.time(); cat(sprintf("[task] Normalize names — end %s (%.1f s)\n",
                              format(t1, "%H:%M:%S"),
                              as.numeric(difftime(t1, t0, units="secs"))))





# [Normalize addresses] ----
t0 = Sys.time(); cat(sprintf("[task] Normalize addresses — start %s\n", format(t0, "%H:%M:%S")))
if (voters_ok) {
  street_norm <- tolower(as.character(voters_dt$street_raw))
  street_norm <- stringi::stri_trans_general(street_norm, "Any-Latin; Latin-ASCII")
  street_norm <- stringr::str_replace_all(street_norm, "[\\.,#]", " ")
  for (k in c("apartment","apt","unit","ste","suite")) {
    street_norm <- stringr::str_replace_all(street_norm, paste0("\\b", k, "\\b"), "apt")
  }
  sfx_keys <- c("st","str","street","ave","av","avenue","rd","road","dr","drive",
                "blvd","boul","boulevard","ln","lane","ct","court","pl","place","cir","cr","circle")
  sfx_vals <- c("street","street","street","avenue","avenue","avenue","road","road","drive","drive",
                "boulevard","boulevard","boulevard","lane","lane","court","court","place","place","circle","circle","circle")
  for (i in seq_along(sfx_keys)) {
    street_norm <- stringr::str_replace_all(street_norm, paste0("\\b", sfx_keys[i], "\\b"), sfx_vals[i])
  }
  dir_keys <- c("n","s","e","w","ne","nw","se","sw")
  dir_vals <- c("north","south","east","west","northeast","northwest","southeast","southwest")
  for (i in seq_along(dir_keys)) {
    street_norm <- stringr::str_replace_all(street_norm, paste0("\\b", dir_keys[i], "\\b"), dir_vals[i])
  }
  street_norm <- stringr::str_squish(street_norm)
  voters_dt[, street_normalized := street_norm]; rm(street_norm, sfx_keys, sfx_vals, dir_keys, dir_vals)
  
  city_norm <- ifelse(!is.na(voters_dt$city_raw),
                      stringr::str_to_lower(stringi::stri_trans_general(voters_dt$city_raw,"Any-Latin; Latin-ASCII")),
                      NA_character_)
  voters_dt[, city_normalized := city_norm]; rm(city_norm)
  voters_dt[, zip_normalized := stringr::str_extract(voters_dt$zip_raw, "\\d{5}")]
  voters_dt[, last_initial := substr(voters_dt$lastn_normalized, 1, 1)]
  data.table::setkey(voters_dt, last_initial)
}
t1 = Sys.time(); cat(sprintf("[task] Normalize addresses — end %s (%.1f s)\n",
                             format(t1, "%H:%M:%S"),
                             as.numeric(difftime(t1, t0, units="secs"))))





# [Rasterize PDF] ----
png_files <- character(0); pdf_ok <- TRUE
t0 <- Sys.time(); cat(sprintf("[task] Rasterize PDF — start %s\n", format(t0, "%H:%M:%S")))
if (!file.exists(PDF_PATH)) {
  cat("[error] PDF file not found: ", PDF_PATH, "\n"); pdf_ok <- FALSE
} else {
  tmpdir <- tempfile("pa_png_"); dir.create(tmpdir, showWarnings = FALSE, recursive = TRUE)
  png_files <- pdftools::pdf_convert(
    PDF_PATH,
    format    = "png",
    dpi       = 220,
    filenames = file.path(tmpdir, "page-%03d.png")
  )
  if (!length(png_files) || !all(file.exists(png_files))) {
    cat("[error] Failed to create PNG pages.\n"); pdf_ok <- FALSE
  }
}
t1 <- Sys.time(); cat(sprintf("[task] Rasterize PDF — end %s (%.1f s)\n",
                              format(t1, "%H:%M:%S"),
                              as.numeric(difftime(t1, t0, units="secs"))))




# [OpenAI extraction] ----
sig_dt <- data.table()
t0 <- Sys.time(); cat(sprintf("[task] Extract signatures (OpenAI) — start %s\n", format(t0, "%H:%M:%S")))
if (!nzchar(Sys.getenv("OPENAI_API_KEY",""))) {
  cat("[error] OPENAI_API_KEY not set; skipping extraction.\n")
} else if (!pdf_ok) {
  cat("[warn] PDF rasterization failed; skipping extraction.\n")
} else {
  all_sigs <- list()
  total_pages <- length(png_files)
  
  system_prompt <- "You are a data extraction model that reads scanned recall petition sheets. 
  Extract signatures as a JSON array. For each row with handwriting, return: 
  {extracted_name (string), 
  extracted_address (street only string), 
  city (string or empty), 
  zip (string or empty), 
  row (int)}. Ignore empty rows. Do not invent data."
  user_prompt   <- "Return STRICT JSON only as {\"signatures\":[ ... ]} without commentary or any other information. Also, do not return any triple backquotes."
  
  for (i in seq_along(png_files)) {
    page_t0 <- Sys.time(); cat(sprintf("  [PDF Page %d of %d] call — start %s\n", i, total_pages, format(page_t0, "%H:%M:%S")))
    tryCatch({
      fsz     <- file.info(png_files[i])$size
      img_raw <- readBin(png_files[i], what = "raw", n = fsz)
      img_b64 <- paste0("data:image/png;base64,", jsonlite::base64_enc(img_raw))
      
      body <- list(
        model  = MODEL_NAME,
        input  = list(
          list(role="system", content=list(list(type="input_text",  text=system_prompt))),
          list(role="user",   content=list(
            list(type="input_text",  text=user_prompt),
            list(type="input_image", image_url=img_b64)
          ))
        ),
        temperature = 0
      )
      
      resp <- httr::POST(
        url = "https://api.openai.com/v1/responses",
        httr::add_headers(
          Authorization = paste("Bearer", Sys.getenv("OPENAI_API_KEY")),
          "Content-Type" = "application/json"
        ),
        body   = body,
        encode = "json",
        httr::timeout(300),              # total seconds
        httr::config(connecttimeout = 30)
      )
      
      txt <- httr::content(resp, as = "text", encoding = "UTF-8")
      js  <- jsonlite::fromJSON(txt)
      text_out <- unlist(js$output$content[[1]]$text)
      parsed   <- jsonlite::fromJSON(text_out)
      
      sigs <- data.table::as.data.table(parsed$signatures)
      sigs[, page := i]
      if (!"row" %in% names(sigs)) sigs[, row := NA_integer_]
      all_sigs[[length(all_sigs)+1]] <- sigs
    }, error = function(e){
      cat(sprintf("  [PDF Page %d of %d] skipped due to error: %s\n", i, total_pages, conditionMessage(e)))
    })
    page_t1 <- Sys.time(); cat(sprintf("  [PDF Page %d of %d] call — end %s (%.1f s)\n",
                                       i, total_pages, format(page_t1, "%H:%M:%S"),
                                       as.numeric(difftime(page_t1, page_t0, units="secs"))))
  }
  if (length(all_sigs)) sig_dt <- data.table::rbindlist(all_sigs, fill = TRUE)
}
t1 <- Sys.time(); cat(sprintf("[task] Extract signatures (OpenAI) — end %s (%.1f s)\n",
                              format(t1, "%H:%M:%S"),
                              as.numeric(difftime(t1, t0, units="secs"))))





# [Ensure signature columns] ----
t0 = Sys.time(); cat(sprintf("[task] Prepare signatures — start %s\n", format(t0, "%H:%M:%S")))
need_cols <- c("extracted_name","extracted_address","city","zip","page","row")
for (cname in need_cols) if (!cname %in% names(sig_dt)) sig_dt[[cname]] <- NA
t1 = Sys.time(); cat(sprintf("[task] Prepare signatures — end %s (%.1f s)\n",
                             format(t1, "%H:%M:%S"),
                             as.numeric(difftime(t1, t0, units="secs"))))







# [Process signatures / match & score (concat LV)] ----
t0 = Sys.time(); cat(sprintf("[task] Match signatures — start %s\n", format(t0, "%H:%M:%S")))
results_list <- list()
if (!voters_ok) {
  cat("[warn] Voters not available; skipping matching.\n")
} else if (!nrow(sig_dt)) {
  cat("[warn] No signatures extracted; skipping matching.\n")
} else {
  if (!"full_name_address_normalized" %in% names(voters_dt)) {
    voters_dt[, full_name_address_normalized := stringr::str_squish(
      paste(name_normalized, street_normalized, city_normalized, zip_normalized)
    )]
  }
  voter_orig_cols <- c("Last Name","First Name","Middle Name","Suffix Name",
                       "Primary Address","City","State","Zip","PrimaryZip4","Precinct Name")
  
  for (i in seq_len(nrow(sig_dt))) {
    if (i %% 5 == 0 || i == 1) cat(sprintf("  [match] signature %d/%d\n", i, nrow(sig_dt)))
    sig_row <- sig_dt[i]
    
    # normalize extracted name
    nm <- as.character(sig_row$extracted_name)
    if (is.na(nm) || !nzchar(nm)) {
      sig_name_norm <- NA_character_
    } else {
      if (stringr::str_detect(nm, ",")) {
        pr <- stringr::str_split_fixed(nm, ",", 2)
        nm <- stringr::str_trim(paste(pr[,2], pr[,1]))
      }
      nm <- stringr::str_to_lower(nm)
      nm <- stringr::str_replace_all(nm, "[\\.,\\-]", " ")
      nm <- stringr::str_replace_all(nm, "'", "")
      nm <- stringi::stri_trans_general(nm, "Any-Latin; Latin-ASCII")
      nm <- stringr::str_remove_all(nm, stringr::regex("(?:,?\\s*)(jr|sr|ii|iii|iv|md|phd|esq|esquire|jd|m\\.?d\\.?|d\\.?o\\.?|dmd|dds)\\b", ignore_case=TRUE))
      sig_name_norm <- stringr::str_squish(nm)
    }
    toks     <- if (!is.na(sig_name_norm)) unlist(stringr::str_split(sig_name_norm, "\\s+")) else character(0)
    sig_last <- if (length(toks)>0) toks[length(toks)] else NA_character_
    
    # normalize extracted address
    st <- as.character(sig_row$extracted_address)
    if (is.na(st) || !nzchar(st)) {
      sig_street_norm <- NA_character_
    } else {
      st <- tolower(st)
      st <- stringi::stri_trans_general(st, "Any-Latin; Latin-ASCII")
      st <- stringr::str_replace_all(st, "[\\.,#]", " ")
      for (k in c("apartment","apt","unit","ste","suite")) st <- stringr::str_replace_all(st, paste0("\\b",k,"\\b"), "apt")
      sfx_keys <- c("st","str","street","ave","av","avenue","rd","road","dr","drive","blvd","boul","boulevard","ln","lane","ct","court","pl","place","cir","cr","circle")
      sfx_vals <- c("street","street","street","avenue","avenue","avenue","road","road","drive","drive","boulevard","boulevard","boulevard","lane","lane","court","court","place","place","circle","circle","circle")
      for (j in seq_along(sfx_keys)) st <- stringr::str_replace_all(st, paste0("\\b", sfx_keys[j], "\\b"), sfx_vals[j])
      dir_keys <- c("n","s","e","w","ne","nw","se","sw")
      dir_vals <- c("north","south","east","west","northeast","northwest","southeast","southwest")
      for (j in seq_along(dir_keys)) st <- stringr::str_replace_all(st, paste0("\\b", dir_keys[j], "\\b"), dir_vals[j])
      sig_street_norm <- stringr::str_squish(st)
    }
    sig_city_norm <- if (!is.null(sig_row$city) && nzchar(sig_row$city)) {
      stringr::str_to_lower(stringi::stri_trans_general(as.character(sig_row$city), "Any-Latin; Latin-ASCII"))
    } else NA_character_
    sig_zip_norm <- if (!is.null(sig_row$zip)) stringr::str_extract(as.character(sig_row$zip), "\\d{5}") else NA_character_
    
    # concatenations
    extracted_concat <- stringr::str_squish(paste(sig_name_norm, sig_street_norm))
    
    # blocking
    if (is.na(sig_last) || !nzchar(sig_last)) {
      cand <- voters_dt
    } else {
      last_init <- substr(sig_last, 1, 1)
      cand <- voters_dt[last_initial == last_init]
      if (nrow(cand) == 0) cand <- voters_dt
    }
    if (!is.na(sig_zip_norm) && nzchar(sig_zip_norm)) {
      cand2 <- cand[zip_normalized == sig_zip_norm]
      if (nrow(cand2) > 0) cand <- cand2
    }
    
    # Levenshtein similarity on concatenations
    sim_score <- if (!is.na(extracted_concat) && nzchar(extracted_concat)) {
      stringdist::stringsim(extracted_concat, cand$full_name_address_normalized, method = "lv")
    } else rep(0, nrow(cand))
    
    if (!length(sim_score)) {
      status <- "Not Found"; confidence <- 0; best_row <- NULL
    } else {
      ord <- order(-sim_score)
      cand_best  <- cand[ord][1]
      best_score <- sim_score[ord][1]
      #status <- if (!is.na(best_score) && best_score >= 0.85) "Matched"
      #else if (!is.na(best_score) && best_score >= 0.65) "Possible Match"
      #else "Not Found"
      confidence <- ifelse(is.na(best_score), 0, round(best_score, 3))
      best_row <- cand_best
    }
    
    out_row <- data.table::data.table(
      extracted_name    = as.character(sig_row$extracted_name),
      extracted_address = as.character(sig_row$extracted_address),
      extracted_city    = as.character(sig_row$city),
      extracted_zip     = as.character(sig_row$zip),
      page              = suppressWarnings(as.integer(sig_row$page)),
      row               = suppressWarnings(as.integer(sig_row$row)),
      #match_status      = status,
      match_confidence  = confidence
    )
    
    if (!is.null(best_row)) {
      out_row <- cbind(out_row, best_row[, .(
        voter_id,
        matched_name = name_raw,
        matched_street = street_raw,
        matched_city = city_raw,
        matched_zip = zip_raw,
        matched_name_normalized = name_normalized,
        matched_street_normalized = street_normalized,
        matched_city_normalized = city_normalized,
        matched_zip_normalized = zip_normalized
      )])
      keep_cols <- c("Last Name","First Name","Middle Name","Suffix Name",
                     "Primary Address","City","State","Zip","PrimaryZip4","Precinct Name")
      keep_cols <- keep_cols[keep_cols %in% names(best_row)]
      if (length(keep_cols)) out_row <- cbind(out_row, best_row[, keep_cols, with = FALSE])
    }
    
    results_list[[length(results_list)+1]] <- out_row
  }
}
final_dt <- if (length(results_list)) data.table::rbindlist(results_list, fill=TRUE) else data.table()

# rename columns
rename_map <- c(
  extracted_name    = "pdf_extracted_name",
  extracted_address = "pdf_extracted_address",
  extracted_city    = "pdf_extracted_city",
  extracted_zip     = "pdf_extracted_zip",
  page              = "pdf_page",
  match_confidence  = "match_confidence_score"
)
to_rename <- intersect(names(rename_map), names(final_dt))
if (length(to_rename)) data.table::setnames(final_dt, to_rename, unname(rename_map[to_rename]))

t1 = Sys.time(); cat(sprintf("[task] Match signatures — end %s (%.1f s)\n",
                             format(t1, "%H:%M:%S"),
                             as.numeric(difftime(t1, t0, units="secs"))))





# [Write output & summary] ----
t0 = Sys.time(); cat(sprintf("[task] Write output — start %s\n", format(t0, "%H:%M:%S")))
data.table::fwrite(final_dt, OUTPUT_PATH)
#matched_ct  <- if (nrow(final_dt)) sum(final_dt$match_status=="Matched", na.rm=TRUE) else 0
#possible_ct <- if (nrow(final_dt)) sum(final_dt$match_status=="Possible Match", na.rm=TRUE) else 0
#notfound_ct <- if (nrow(final_dt)) sum(final_dt$match_status=="Not Found", na.rm=TRUE) else 0
t1 = Sys.time(); cat(sprintf("[task] Write output — end %s (%.1f s)\n", format(t1, "%H:%M:%S"),
                             as.numeric(difftime(t1, t0, units="secs"))))
cat(sprintf("[summary] number of signatures matched=%d\n", #matched=%d possible=%d not_found=%d\n
            nrow(final_dt)
            #matched_ct, possible_ct, notfound_ct
            ))
cat(sprintf("[end] Script end at %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
