# Pulls Brian's own publications (ORCID + OpenAlex) into
# data/publications_auto.csv, plus headline citation stats into
# data/scholar_stats.csv, for publications.qmd to render.
#
# Matching logic mirrors ../cv/scripts/fetch_publications.R (ORCID + known-
# institution matching against OpenAlex, plus Jaccard-based dedup) - see that
# file's comments for the reasoning behind simplifyDataFrame=FALSE, the fuzzy
# title match, and why ORCID alone isn't used as a strict whitelist.
library(tidyverse)
library(httr)
library(jsonlite)
library(glue)

orcid_id <- "0000-0002-3039-2004"
contact_email <- "stacybw@gmail.com"

normalize_doi <- function(doi) {
  if (is.null(doi) || length(doi) == 0) return(NA_character_)
  tolower(str_remove(doi, "^https?://doi\\.org/"))
}

# --- 1. ORCID: authoritative list of Brian's own works ---------------------
orcid_resp <- GET(
  glue("https://pub.orcid.org/v3.0/{orcid_id}/works"),
  add_headers(Accept = "application/json")
)
stop_for_status(orcid_resp)
orcid_json <- content(orcid_resp, as = "text", encoding = "UTF-8") %>%
  fromJSON(simplifyDataFrame = FALSE)

orcid_works <- map_dfr(orcid_json$group, function(g) {
  ws <- g[["work-summary"]][[1]]
  eids <- ws[["external-ids"]][["external-id"]]
  doi <- NA_character_
  for (eid in eids) {
    if (identical(tolower(eid[["external-id-type"]] %||% ""), "doi")) {
      doi <- normalize_doi(eid[["external-id-value"]])
    }
  }
  tibble(
    orcid_title = ws[["title"]][["title"]][["value"]] %||% NA_character_,
    doi = doi
  )
})

# --- 2. OpenAlex: enrichment (venue, co-authors, citation counts, links) ---
author_resp <- GET(
  glue("https://api.openalex.org/authors/orcid:{orcid_id}"),
  query = list(mailto = contact_email)
)
stop_for_status(author_resp)
author_json <- content(author_resp, as = "text", encoding = "UTF-8") %>%
  fromJSON(simplifyDataFrame = FALSE)
works_api_url <- author_json$works_api_url

fetch_all_openalex_works <- function(url) {
  page <- 1
  all_results <- list()
  repeat {
    resp <- GET(url, query = list(mailto = contact_email, `per-page` = 200, page = page))
    stop_for_status(resp)
    page_json <- content(resp, as = "text", encoding = "UTF-8") %>%
      fromJSON(simplifyDataFrame = FALSE)
    results <- page_json$results
    if (length(results) == 0) break
    all_results <- c(all_results, results)
    if (length(results) < 200) break
    page <- page + 1
  }
  all_results
}

openalex_works <- fetch_all_openalex_works(works_api_url)

known_institutions <- c("world bank", "usda", "economic research service", "michigan state")

openalex_df <- map_dfr(openalex_works, function(w) {
  doi <- normalize_doi(w$doi)
  authorships <- w$authorships
  author_names <- if (!is.null(authorships)) {
    map_chr(authorships, ~ .x$author$display_name %||% NA_character_)
  } else {
    character(0)
  }
  is_stacy <- str_detect(author_names, fixed("Stacy"))
  coauthors <- author_names[!is_stacy]
  name_key <- map_chr(coauthors, ~ paste(sort(str_split(str_to_lower(.x), "[^a-z]+")[[1]]), collapse = " "))
  coauthors <- coauthors[!duplicated(name_key)]
  venue <- w$primary_location$source$display_name %||% NA_character_
  link <- w$doi %||% w$primary_location$landing_page_url %||% NA_character_
  # Institutions listed against Brian's own authorship entry (fallback signal
  # for works not yet registered on ORCID).
  stacy_institutions <- if (any(is_stacy)) {
    authorships[is_stacy] %>%
      map(~ .x$institutions) %>%
      unlist(recursive = FALSE) %>%
      map_chr(~ .x$display_name %||% NA_character_)
  } else {
    character(0)
  }
  tibble(
    openalex_id = w$id %||% NA_character_,
    title = w$title %||% NA_character_,
    doi = doi,
    year = w$publication_year %||% NA_integer_,
    venue = venue,
    coauthors = paste(coauthors, collapse = ", "),
    cited_by_count = w$cited_by_count %||% 0,
    link = link,
    type = w$type %||% NA_character_,
    known_institution_match = length(stacy_institutions) > 0 &&
      any(map_lgl(known_institutions, ~ any(str_detect(str_to_lower(stacy_institutions), fixed(.x)))))
  )
})

openalex_df <- openalex_df %>% filter(!type %in% c("dataset", "supplementary-materials"))

# --- 3. Restrict to works that are plausibly Brian's own: matched against
# his self-registered ORCID list (by DOI, falling back to normalized
# substring title match), OR his own authorship entry lists one of his known
# institutions - catches recent papers not yet added to ORCID (see cv repo
# script for the fuller writeup of why ORCID-only matching drops legit work) ---
norm_title <- function(x) {
  x %>% str_to_lower() %>% str_replace_all("[^a-z0-9 ]", " ") %>% str_squish()
}

orcid_dois <- orcid_works$doi[!is.na(orcid_works$doi)]
orcid_titles_norm <- unique(na.omit(norm_title(orcid_works$orcid_title)))
orcid_titles_norm <- orcid_titles_norm[nchar(orcid_titles_norm) > 8]

openalex_df <- openalex_df %>%
  mutate(
    norm_title = norm_title(title),
    title_matches = map_lgl(norm_title, function(t) {
      if (is.na(t) || t == "") return(FALSE)
      any(str_detect(orcid_titles_norm, fixed(t)) | str_detect(t, fixed(orcid_titles_norm)))
    })
  )

matched <- openalex_df %>%
  filter((!is.na(doi) & doi %in% orcid_dois) | title_matches | known_institution_match) %>%
  distinct(title, .keep_all = TRUE)

# Collapse near-duplicate records of the same paper (working paper suffix,
# or a looser retitle between preprint and final version) via word-set
# (Jaccard) similarity, keeping the most recent year (the final version).
strip_wp_suffix <- function(t) str_remove(t, "\\s*working paper\\s*#?\\s*\\d*\\.?\\s*$")
word_set <- function(t) unique(str_split(t, "\\s+")[[1]])
jaccard_sim <- function(a, b) {
  sa <- word_set(a); sb <- word_set(b)
  length(intersect(sa, sb)) / length(union(sa, sb))
}

matched <- matched %>% mutate(dedup_key = strip_wp_suffix(norm_title))

keys <- unique(matched$dedup_key)
cluster_id <- setNames(seq_along(keys), keys)
if (length(keys) > 1) {
  for (i in seq_len(length(keys) - 1)) {
    for (j in seq((i + 1), length(keys))) {
      if (jaccard_sim(keys[i], keys[j]) > 0.75) {
        old_id <- cluster_id[[keys[j]]]
        new_id <- cluster_id[[keys[i]]]
        cluster_id[cluster_id == old_id] <- new_id
      }
    }
  }
}

matched <- matched %>%
  mutate(cluster_id = cluster_id[dedup_key]) %>%
  group_by(cluster_id) %>%
  slice_max(year, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(-cluster_id, -dedup_key)

# --- 4. Write publications_auto.csv -----------------------------------------
# venue is left NA rather than "N/A" - the render side omits the field when
# it's missing instead of printing a placeholder.
publications_auto <- matched %>%
  transmute(
    title,
    year,
    venue,
    coauthors,
    cited_by_count,
    link
  ) %>%
  arrange(desc(year))

write_csv(publications_auto, "data/publications_auto.csv", na = "")
cat(glue("Wrote {nrow(publications_auto)} publications to data/publications_auto.csv\n"))

# --- 5. Headline stats for the Publications page ----------------------------
# These come from the author endpoint already fetched in step 2, so this costs
# no additional API calls. Author-level figures cover all works OpenAlex knows
# about (more than the curated list rendered on the site), hence the explicit
# attribution in the rendered stat line.
stats <- author_json$summary_stats
scholar_stats <- tibble(
  works_count = author_json$works_count %||% NA_integer_,
  cited_by_count = author_json$cited_by_count %||% NA_integer_,
  h_index = stats$h_index %||% NA_integer_,
  i10_index = stats$i10_index %||% NA_integer_,
  retrieved = format(Sys.Date())
)

write_csv(scholar_stats, "data/scholar_stats.csv", na = "")
cat(glue("Wrote data/scholar_stats.csv ({scholar_stats$cited_by_count} citations, h-index {scholar_stats$h_index})\n"))
