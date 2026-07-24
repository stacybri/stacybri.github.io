# Pulls Brian's official World Bank reports and blog posts, keyed by his WB
# staff UPI (000469649). Found by inspecting the network calls the WB staff
# profile page (worldbank.org/en/about/people/b/brian-william-stacy) makes to
# populate its "Publications" and "Blog Posts" tabs - both are backed by
# public, documented, unauthenticated APIs, so we call them directly instead
# of scraping the rendered page:
#   - Research/reports: GET https://search.worldbank.org/api/v2/research
#   - Blog posts:       POST https://webapi.worldbank.org/aemsite/blogs/global/search
library(tidyverse)
library(httr)
library(jsonlite)
library(glue)

wb_upi <- "000469649"

# JSON nulls come back as NULL, but some fields arrive as "" instead - treat
# both as missing so the url fallback below actually fires.
blank_to_na <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA_character_)
  x <- as.character(x)[1]
  if (is.na(x) || !nzchar(trimws(x))) NA_character_ else x
}

# --- Reports (Documents & Reports) ------------------------------------------
research_resp <- GET(
  "https://search.worldbank.org/api/v2/research",
  query = list(
    format = "json",
    fl = "docty,docdt,display_title,url",
    apilang = "en",
    lang_exact = "English",
    rows = 200,
    authr_upi = wb_upi
  )
)
stop_for_status(research_resp)
research_json <- content(research_resp, as = "text", encoding = "UTF-8") %>%
  fromJSON(simplifyDataFrame = FALSE)

# `documents` carries a trailing "facets" key alongside the real D<id> entries;
# iterating it blindly yields an all-empty row that renders as a blank
# publication. Keep only entries that actually look like documents.
documents <- research_json$documents
documents <- documents[names(documents) != "facets"]

wb_reports <- map_dfr(documents, function(d) {
  tibble(
    title = blank_to_na(d$display_title),
    date = blank_to_na(d$docdt),
    type = blank_to_na(d$docty),
    url = blank_to_na(d$url)
  )
}) %>%
  filter(!is.na(title)) %>%
  mutate(date = as.Date(date)) %>%
  arrange(desc(date))

write_csv(wb_reports, "data/wb_reports_auto.csv", na = "")
cat(glue("Wrote {nrow(wb_reports)} WB reports to data/wb_reports_auto.csv\n"))

# --- Blog posts --------------------------------------------------------------
blogs_resp <- POST(
  "https://webapi.worldbank.org/aemsite/blogs/global/search",
  body = list(
    filter = glue("(languageCode eq 'en') and (bloggers/any(blogger: blogger/upi eq '{wb_upi}'))"),
    search = "*",
    top = 200,
    count = TRUE,
    orderby = "blogDate desc",
    skip = 0
  ),
  encode = "json"
)
stop_for_status(blogs_resp)
blogs_json <- content(blogs_resp, as = "text", encoding = "UTF-8") %>%
  fromJSON(simplifyDataFrame = FALSE)

wb_blogs <- map_dfr(blogs_json$value, function(b) {
  tibble(
    title = blank_to_na(b$title),
    date = blank_to_na(b$blogDate),
    description = blank_to_na(b$description),
    # shortLink is null for ~8 of these posts; pagePublishPath carries a valid
    # absolute blogs.worldbank.org URL in every one of those cases (verified
    # 200), so fall back to it rather than rendering a dead entry.
    url = coalesce(blank_to_na(b$shortLink), blank_to_na(b$pagePublishPath))
  )
}) %>%
  mutate(date = as.Date(date)) %>%
  arrange(desc(date))

write_csv(wb_blogs, "data/wb_blogs_auto.csv", na = "")
cat(glue("Wrote {nrow(wb_blogs)} WB blog posts to data/wb_blogs_auto.csv\n"))
