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

wb_reports <- map_dfr(research_json$documents, function(d) {
  tibble(
    title = d$display_title %||% NA_character_,
    date = d$docdt %||% NA_character_,
    type = d$docty %||% NA_character_,
    url = d$url %||% NA_character_
  )
}) %>%
  mutate(date = as.Date(date)) %>%
  arrange(desc(date))

write_csv(wb_reports, "data/wb_reports_auto.csv")
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
    title = b$title %||% NA_character_,
    date = b$blogDate %||% NA_character_,
    description = b$description %||% NA_character_,
    url = b$shortLink %||% NA_character_
  )
}) %>%
  mutate(date = as.Date(date)) %>%
  arrange(desc(date))

write_csv(wb_blogs, "data/wb_blogs_auto.csv")
cat(glue("Wrote {nrow(wb_blogs)} WB blog posts to data/wb_blogs_auto.csv\n"))
