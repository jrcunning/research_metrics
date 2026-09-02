library(scholar)
library(readxl)
library(tidyverse)

extract_dois <- function(reference) {
  reference %>%
    str_replace_all(regex("%2f", ignore_case = TRUE), "/") %>%
    str_extract_all(regex("10\\.\\d{4,9}/[^\\s\"<>]+", ignore_case = TRUE)) %>%
    unlist(use.names = FALSE) %>%
    str_replace_all(regex("^https?://(dx\\.)?doi\\.org/", ignore_case = TRUE), "") %>%
    str_remove("[?#].*$") %>%
    str_remove(regex("(\\.(full|abstract|pdf|epdf|xml|html))+$", ignore_case = TRUE)) %>%
    str_remove("[\\]\\)\\.,;:]+$") %>%
    str_to_lower()
}

normalize_publication_text <- function(x) {
  x %>%
    str_to_lower() %>%
    str_replace_all("[^a-z0-9]+", " ") %>%
    str_squish()
}

publication_reference_columns <- c("Citation", "Link", "Publication Link")

publication_reference_text <- function(data) {
  available_cols <- intersect(publication_reference_columns, names(data))

  if (length(available_cols) == 0) {
    return(rep(NA_character_, nrow(data)))
  }

  pmap_chr(data[available_cols], function(...) {
    values <- as.character(c(...))
    values <- str_squish(values)
    values <- values[!is.na(values) & values != "" & str_to_upper(values) != "NA"]

    if (length(values) == 0) {
      return(NA_character_)
    }

    str_squish(paste(values, collapse = " "))
  })
}

publication_references <- function(data, category_col) {
  publications <- data %>%
    filter(str_detect(coalesce(as.character(.data[[category_col]]), ""), regex("Publication", ignore_case = TRUE)))

  publications %>%
    transmute(
      reference = as.character(Citation),
      DOIReference = publication_reference_text(publications)
    )
}

first_crossref_year <- function(item) {
  if (is.null(item$issued) || is.null(item$issued[["date-parts"]])) {
    return(NA_integer_)
  }

  date_parts <- item$issued[["date-parts"]][[1]]

  if (length(date_parts) == 0) {
    return(NA_integer_)
  }

  as.integer(date_parts[[1]])
}

or_missing <- function(x, missing_value) {
  if (is.null(x) || length(x) == 0) {
    return(missing_value)
  }

  x
}

resolve_doi_from_crossref <- function(title, year = NA_integer_) {
  if (
    !requireNamespace("httr", quietly = TRUE) ||
      !requireNamespace("jsonlite", quietly = TRUE)
  ) {
    return(tibble(DOI = NA_character_, MatchTitle = NA_character_, MatchScore = NA_real_, MatchYear = NA_integer_))
  }

  response <- tryCatch(
    httr::GET(
      "https://api.crossref.org/works",
      query = list(
        "query.title" = title,
        rows = 5,
        select = "DOI,title,issued,score"
      ),
      httr::user_agent("Shedd research metrics DOI lookup"),
      httr::timeout(15)
    ),
    error = function(e) NULL
  )

  if (is.null(response) || httr::status_code(response) != 200) {
    return(tibble(DOI = NA_character_, MatchTitle = NA_character_, MatchScore = NA_real_, MatchYear = NA_integer_))
  }

  content <- httr::content(response, as = "text", encoding = "UTF-8")
  items <- jsonlite::fromJSON(content, simplifyVector = FALSE)$message$items

  if (length(items) == 0) {
    return(tibble(DOI = NA_character_, MatchTitle = NA_character_, MatchScore = NA_real_, MatchYear = NA_integer_))
  }

  title_norm <- normalize_publication_text(title)

  candidates <- map_dfr(items, function(item) {
    match_title <- if (length(item$title) > 0) item$title[[1]] else NA_character_
    match_year <- first_crossref_year(item)

    tibble(
      DOI = str_to_lower(or_missing(item$DOI, NA_character_)),
      MatchTitle = match_title,
      MatchScore = stringdist::stringsim(title_norm, normalize_publication_text(match_title), method = "jw"),
      MatchYear = match_year,
      CrossrefScore = or_missing(item$score, NA_real_)
    )
  }) %>%
    mutate(YearMatches = !is.na(year) & !is.na(MatchYear) & year == MatchYear) %>%
    filter(!is.na(DOI), MatchScore >= 0.88 | (YearMatches & MatchScore >= 0.82)) %>%
    arrange(desc(YearMatches), desc(MatchScore), desc(CrossrefScore))

  if (nrow(candidates) == 0) {
    return(tibble(DOI = NA_character_, MatchTitle = NA_character_, MatchScore = NA_real_, MatchYear = NA_integer_))
  }

  candidates %>%
    slice(1) %>%
    select(DOI, MatchTitle, MatchScore, MatchYear)
}

doi_from_references <- function(title, reference_lookup) {
  title_norm <- normalize_publication_text(title)
  matches <- reference_lookup %>%
    filter(str_detect(ReferenceNorm, fixed(title_norm))) %>%
    pull(DOI) %>%
    unlist(use.names = FALSE)

  matches <- matches[!is.na(matches) & matches != ""]

  if (length(matches) == 0) {
    return(NA_character_)
  }

  matches[1]
}

write_publication_dois <- function(
  publications,
  references,
  output_file = "output/publication_dois.txt",
  detail_file = "output/publication_doi_details.csv"
) {
  reference_lookup <- references

  if (!"DOIReference" %in% names(reference_lookup)) {
    reference_lookup <- reference_lookup %>%
      mutate(DOIReference = reference)
  }

  reference_lookup <- reference_lookup %>%
    mutate(
      ReferenceNorm = normalize_publication_text(reference),
      DOIReference = str_squish(as.character(DOIReference)),
      DOIReference = if_else(str_to_upper(DOIReference) == "NA", NA_character_, DOIReference),
      DOIReference = coalesce(na_if(DOIReference, ""), as.character(reference)),
      DOI = map(DOIReference, extract_dois)
    )

  publication_dois <- publications %>%
    filter(!is.na(title), title != "") %>%
    mutate(
      ScholarAuthorID = if ("scholar_id" %in% names(.)) as.character(scholar_id) else NA_character_,
      ScholarCID = if ("cid" %in% names(.)) as.character(cid) else NA_character_,
      ScholarPubID = if ("pubid" %in% names(.)) as.character(pubid) else NA_character_
    ) %>%
    transmute(
      Title = title,
      Year = as.integer(year),
      ScholarAuthorID,
      ScholarCID,
      ScholarPubID,
      DOI = map_chr(title, doi_from_references, reference_lookup),
      DOISource = if_else(is.na(DOI), NA_character_, "local reference")
    )

  if (file.exists(detail_file)) {
    cached_dois <- read_csv(detail_file, show_col_types = FALSE) %>%
      filter(!is.na(DOI), DOI != "") %>%
      transmute(
        TitleNorm = normalize_publication_text(Title),
        Year = as.integer(Year),
        CachedDOI = DOI,
        CachedDOISource = DOISource
      ) %>%
      distinct(TitleNorm, Year, .keep_all = TRUE)

    publication_dois <- publication_dois %>%
      mutate(TitleNorm = normalize_publication_text(Title)) %>%
      left_join(cached_dois, by = c("TitleNorm", "Year")) %>%
      mutate(
        DOI = coalesce(DOI, CachedDOI),
        DOISource = coalesce(DOISource, CachedDOISource)
      ) %>%
      select(-TitleNorm, -CachedDOI, -CachedDOISource)
  }

  missing_doi_rows <- which(is.na(publication_dois$DOI))

  if (length(missing_doi_rows) > 0) {
    crossref_matches <- map_dfr(missing_doi_rows, function(row_index) {
      Sys.sleep(0.1)

      resolve_doi_from_crossref(
        publication_dois$Title[[row_index]],
        publication_dois$Year[[row_index]]
      ) %>%
        mutate(RowIndex = row_index)
    })

    if (nrow(crossref_matches) > 0) {
      for (match_index in seq_len(nrow(crossref_matches))) {
        row_index <- crossref_matches$RowIndex[[match_index]]
        doi <- crossref_matches$DOI[[match_index]]

        if (!is.na(doi) && doi != "") {
          publication_dois$DOI[[row_index]] <- doi
          publication_dois$DOISource[[row_index]] <- "Crossref title lookup"
        }
      }
    }
  }

  doi_list <- publication_dois %>%
    filter(!is.na(DOI), DOI != "") %>%
    distinct(DOI) %>%
    arrange(DOI) %>%
    pull(DOI)

  dir.create(dirname(output_file), showWarnings = FALSE, recursive = TRUE)
  write_lines(doi_list, output_file)
  write_csv(publication_dois, detail_file)

  message("Wrote ", length(doi_list), " unique DOI(s) to ", output_file)
}

## Get provided list of Haerther Center authors
hc_authors <- read_tsv("data/authors.txt") %>% drop_na()

## Get provided reference list of pre-2019 Haerther Center publications
hc_oldrefs <- read_tsv("data/pre2019_publications.txt", col_names = "reference") %>%
  mutate(DOIReference = reference)

## Get newer pubs that have been input into research metrics form
hc_refs_2019_2022 <- read_xlsx("data/research_metrics_2019-2022.xlsx") %>%
  publication_references("Category")
hc_refs_2022_2024 <- read_xlsx("data/research_metrics_2022-2024.xlsx") %>%
  publication_references("What was the engagement type?")

new_metrics_candidates <- list.files(path = "data", pattern = "^Research metrics new.*\\.xlsx$", full.names = TRUE)
new_metrics_candidates <- new_metrics_candidates[file.info(new_metrics_candidates)$size > 0]

if (length(new_metrics_candidates) == 0) {
  stop("No non-empty 'Research metrics new*.xlsx' file found in data/.")
}

new_metrics_file <- new_metrics_candidates[which.max(file.info(new_metrics_candidates)$mtime)]

hc_refs_2024_on <- read_xlsx(new_metrics_file) %>%
  publication_references("Category")
hc_newrefs <- bind_rows(hc_refs_2019_2022, hc_refs_2022_2024, hc_refs_2024_on)

## Create combined hc_references 
hc_references <- bind_rows(hc_oldrefs, hc_newrefs)

# Get publication list for individual author
#get_publications(hc_authors$scholar_id[14])

# Get full publication list for each author from Google Scholar
all_pubs <- hc_authors %>%
  mutate(all_pubs = map(scholar_id, ~ get_publications(.))) %>%
  unnest(all_pubs)

# Subset only Haerther Center publications (since 2012) using fuzzy text matching of titles to the provided reference list
hc_pubs <- all_pubs %>%
  mutate(hc_affil = map_lgl(title, ~ any(agrepl(., hc_references$reference)))) %>%
  filter(hc_affil)  # select only entries with titles that matched

# Remove duplicated publications based on title (e.g., multiple Haerther Center authors on same publication)
hc_pubs <- hc_pubs %>% 
  distinct(title, .keep_all = TRUE)

dir.create("output", showWarnings = FALSE, recursive = TRUE)
write_csv(hc_pubs, "output/google_scholar_publications.csv")
write_publication_dois(hc_pubs, hc_references)

# Calculate departmental h-index
cites <- hc_pubs %>% 
  arrange(-cites) %>%
  pull(cites)

h.index <- tail(which(cites >= seq_along(cites)), 1)
i10.index <- sum(cites >= 10)

# Summarize citation counts with today's date
todays_totals <- hc_pubs %>%
  summarise(date = Sys.Date(),
            total_pubs = nrow(hc_pubs),
            total_cites = sum(cites),
            h.index = h.index,
            i10.index = i10.index)

# Append today's totals to output file
write.table(todays_totals, file = "data/citation_counts.txt", sep = "\t", quote = F, 
            row.names = F, col.names = F, append = T)
