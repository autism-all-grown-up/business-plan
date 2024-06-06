library(flextable)
library(ftExtra)
library(tidyverse)
library(knitr)

printGlossary <- function(glossary, output_format) {
  glossary <- glossary %>%
    mutate(Definition = map(Definition, function(def) {
      # Find URLs in the definition
      urls <- str_extract_all(def, "https?://[\\w\\.-/]+")[[1]]
      if (length(urls) > 0) {
        # Replace URLs with markdown links
        for (url in urls) {
          def <- str_replace(def, url, sprintf("[%s](%s)", url, url))
        }
      }
      def
    }))

  # Create flextable object
  ft <- flextable(glossary) %>%
    theme_booktabs() %>%
    bold(part = "header") %>%
    fontsize(size = 12, part = "header") %>%
    font(part = "header", fontname = "Times") %>%
    bg(part = "header", bg = "grey") %>%
    fontsize(size = 10, part = "body") %>%
    font(part = "body", fontname = "Arial") %>%
    width(j = 1, width = 2.5, unit = "in") %>%
    width(j = 2, width = 5.5, unit = "in") %>%
    valign(valign = "top", part = "all")

  # Apply markdown link formatting
  ft <- ft %>%
    compose(
      j = "Definition",
      value = as_paragraph_md(Definition),
      part = "body"
    )

  # Detect output format
  # output_format <- knitr::opts_knit$get("rmarkdown.pandoc.to")

  # Adjustments for PDF
  if (!is.null(output_format) && output_format == "latex") {
    ft <- ft %>%
      set_table_properties(layout = "autofit")
  }

  # Adjustments for DOCX
  if (!is.null(output_format) && output_format == "docx") {
    ft <- ft %>%
      set_table_properties(layout = "fixed")
  }

  # Default for interactive sessions
  if (is.null(output_format)) {
    ft <- ft
  }

  return(ft)
}
