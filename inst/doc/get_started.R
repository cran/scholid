## ----install, eval = FALSE----------------------------------------------------
# install.packages("scholid")

## ----scholid types, eval = TRUE-----------------------------------------------
scholid::scholid_types()

## ----check if DOI-------------------------------------------------------------
x <- c(
    "10.1000/182",
    "not a doi",
    NA
)
scholid::is_scholid(
    x    = x,
    type = "doi"
)

## ----normalize DOI------------------------------------------------------------
x <- c(
  "https://doi.org/10.1000/182.",
  "doi:10.1000/182",
  " 10.1000/182 "
)
scholid::normalize_scholid(
    x    = x, 
    type = "doi"
)

## ----normalize ORCID----------------------------------------------------------
x <- c(
  "https://orcid.org/0000-0002-1825-0097",
  "0000000218250097"
)
scholid::normalize_scholid(
    x    = x,
    type = "orcid"
)

## ----extract DOI--------------------------------------------------------------
txt <- c(
  "See https://doi.org/10.1000/182 and doi:10.5555/12345678.",
  "No identifier here.",
  NA
)
scholid::extract_scholid(
    text = txt,
    type = "doi"
)

## ----classify id--------------------------------------------------------------
x <- c(
  "10.1000/182",
  "0000-0002-1825-0097",
  "PMC12345",
  "2101.00001v2",
  "not an id",
  NA
)
scholid::classify_scholid(x = x)

## ----normalization + classification-------------------------------------------
txt <- "Read https://doi.org/10.1000/182 (and ORCID 0000-0002-1825-0097)."
dois <- scholid::extract_scholid(txt, "doi")[[1]]
orcids <- scholid::extract_scholid(txt, "orcid")[[1]]

dois_n <- scholid::normalize_scholid(dois, "doi")
orcids_n <- scholid::normalize_scholid(orcids, "orcid")

scholid::classify_scholid(c(dois_n, orcids_n))
scholid::is_scholid(dois_n, "doi")
scholid::is_scholid(orcids_n, "orcid")

## ----classification does not work---------------------------------------------
x <- c(
  "https://doi.org/10.1000/182",
  "ORCID: 0000-0002-1825-0097",
  "arXiv:2101.00001",
  "PMID: 12345",
  "not an id"
)
scholid::classify_scholid(x)

## ----detect scholid type------------------------------------------------------
scholid::detect_scholid_type(x)

## ----detect scholid type with whitespaces-------------------------------------
scholid::detect_scholid_type(
  c(
    " 0000-0002-1825-0097 ",
    " 10.1000/182 ",
    "ISSN 0317-8471"
  )
)

