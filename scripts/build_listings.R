# Turns the fetched CSVs into YAML metadata files that Quarto's native
# `listing` feature consumes (`listing: contents: data/*.yml`), which is what
# gives the Publications page its search box, sort dropdown and type filter
# without any hand-written JS.
#
# This runs as a project `pre-render` step (see _quarto.yml), so it fires on
# every `quarto render` - locally and in CI. Deliberately NOT folded into
# fetch_publications.R: that only runs on the weekly CI path, so the YAML
# would silently go stale during local edits.
#
# Outputs are build artifacts and are gitignored.
suppressPackageStartupMessages({
  library(tidyverse)
  library(yaml)
})

norm_title <- function(x) {
  x %>% str_to_lower() %>% str_replace_all("[^a-z0-9 ]", " ") %>% str_squish()
}

# Drop NULL/NA/empty entries so Quarto simply omits absent fields rather than
# rendering "NA" placeholders.
drop_empty <- function(lst) {
  lst[!map_lgl(lst, ~ is.null(.x) || (length(.x) == 1 && (is.na(.x) || !nzchar(as.character(.x)))))]
}

# --- Publications: journal articles + WB reports, one merged list -----------
pubs <- read_csv("data/publications_auto.csv", show_col_types = FALSE) %>%
  transmute(
    title,
    sort_date = as.Date(paste0(year, "-01-01")),
    category = "Journal Article",
    venue,
    coauthors,
    citations = cited_by_count,
    link
  )

reports <- read_csv("data/wb_reports_auto.csv", show_col_types = FALSE) %>%
  transmute(
    title,
    sort_date = date,
    category = "WB Report",
    # `type` here is the document class ("Report", "Policy Research Working
    # Paper"); it duplicates the category pill, so it isn't used as a venue.
    venue = NA_character_,
    coauthors = NA_character_,
    citations = NA_integer_,
    link = url
  )

# Hand-maintained GitHub repo links, joined on normalized title so slight
# punctuation/casing differences between sources still match.
repo_links <- read_csv("data/publication_repos.csv", show_col_types = FALSE) %>%
  transmute(norm_title = norm_title(title), github_url)

all_pubs <- bind_rows(pubs, reports) %>%
  mutate(norm_title = norm_title(title)) %>%
  left_join(repo_links, by = "norm_title") %>%
  arrange(desc(sort_date))

# Six works appear in both feeds - the World Bank working paper and the
# published journal version of the same paper (e.g. "Enrollment without
# learning : teacher effort..." vs the JEP article). Collapse those, keeping
# the journal version and inheriting any code link from either row.
#
# Matching is on an EXACT normalized word set, deliberately not fuzzy: the
# annual SPI reports ("2024 Update of the SPI..." vs "2025 Update of the
# SPI...") score 0.82-0.9 on word-overlap similarity but are genuinely
# different publications, and a loose threshold would silently merge them.
word_key <- function(x) map_chr(str_split(x, " "), ~ paste(sort(unique(.x)), collapse = " "))

all_pubs <- all_pubs %>%
  mutate(dedup_key = word_key(norm_title)) %>%
  group_by(dedup_key) %>%
  mutate(github_url = first(na.omit(github_url)) %||% NA_character_) %>%
  arrange(factor(category, levels = c("Journal Article", "WB Report")), desc(sort_date),
          .by_group = TRUE) %>%
  slice(1) %>%
  ungroup() %>%
  select(-dedup_key, -norm_title) %>%
  arrange(desc(sort_date))

# Venue / co-authors / citations / code link are folded into `description`
# rather than exposed as separate listing fields. Two reasons: Quarto's
# built-in filter box searches the description, so venue and co-author names
# become searchable for free; and a custom EJS template (the only other way to
# show a real "Code" hyperlink) would replace the markup that Quarto's
# sort/filter JS binds to.
pub_description <- function(venue, coauthors, citations, github_url) {
  bits <- c(
    if (!is.na(venue)) sprintf("<em>%s</em>", venue),
    if (!is.na(coauthors) && nzchar(coauthors)) sprintf("Authored with %s.", coauthors),
    if (!is.na(citations) && citations > 0) sprintf("Cited by %d.", citations),
    if (!is.na(github_url)) sprintf("<a href='%s'>Code</a>", github_url)
  )
  if (length(bits) == 0) NA_character_ else paste(bits, collapse = " &middot; ")
}

pub_items <- all_pubs %>%
  pmap(function(title, sort_date, category, venue, coauthors, citations, link, github_url) {
    drop_empty(list(
      title = title,
      path = link,
      date = format(sort_date, "%Y-%m-%d"),
      categories = list(category),
      description = pub_description(venue, coauthors, citations, github_url)
    ))
  })

write_yaml(pub_items, "data/publications_listing.yml")
cat(sprintf("Wrote %d items to data/publications_listing.yml\n", length(pub_items)))

# --- Highlights: hand-curated, drives the landing page ----------------------
highlights <- read_csv("data/highlights.csv", show_col_types = FALSE)

hl_items <- highlights %>%
  pmap(function(title, blurb, url, kind, image = NA_character_) {
    drop_empty(list(
      title = title,
      path = url,
      description = blurb,
      categories = list(kind),
      image = image
    ))
  })

write_yaml(hl_items, "data/highlights_listing.yml")
cat(sprintf("Wrote %d items to data/highlights_listing.yml\n", length(hl_items)))
