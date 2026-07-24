# Personal site

Quarto site for Brian Stacy, deployed to GitHub Pages. Publications, citations, and World
Bank blog/report mentions refresh weekly via `.github/workflows/update-and-deploy.yml`,
which runs `scripts/fetch_publications.R` (ORCID + OpenAlex) and
`scripts/fetch_wb_profile.R` (World Bank research + blogs search APIs), commits any
changed `data/*.csv`, renders the site with Quarto, and deploys `_site/` to `gh-pages`.

CV lives in a separate repo ([stacybri/cv](https://github.com/stacybri/cv)) and is linked
from `cv.qmd`, not duplicated here.

## Local development

```
quarto preview
```

To refresh the data locally before previewing:

```
Rscript scripts/fetch_publications.R
Rscript scripts/fetch_wb_profile.R
```
