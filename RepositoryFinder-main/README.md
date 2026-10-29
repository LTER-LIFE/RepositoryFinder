# Repository Finder


The **Repository Finder** is designed to help researchers, students, developers, and practitioners discover repositories related to ecology, biodiversity, conservation, climate science, environmental monitoring, and other ecological disciplines.
Instead of searching through countless sources, this project provides a centralized collection of trusted repositories.

## Running the app

Open `app.R` in RStudio and click **Run App**, or from R in this folder:

```r
shiny::runApp()
```

Requires the R packages `shiny` and `bslib`.

The repository data lives in `data/repositories.csv`. Edit `build_repositories_csv.py` and re-run it (`python3 build_repositories_csv.py`) rather than editing the CSV by hand.
