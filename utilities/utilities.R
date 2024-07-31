library(tidyverse)
library(rvest)

# Function to get the title of a web page
get_page_title <- function(url) {
  tryCatch({
    webpage <- read_html(url)
    page_title <- webpage %>%
      html_node("title") %>%
      html_text()
    return(page_title)
  }, error = function(e) {
    return(url)  # Return the URL itself if there's an error
  })
}

# Function to replace URLs with markdown links
replace_urls_with_md <- function(text) {
  # Improved regex pattern to match URLs, including those with hyphens
  url_pattern <- "https?://[^\\s]+"

  # Find all URLs in the text
  urls <- str_extract_all(text, url_pattern)[[1]]

  # Replace each URL with markdown link [title](url)
  for (url in urls) {
    title <- get_page_title(url)
    title = ifelse(is.na(title), url, title)
    md_link <- sprintf("[%s](%s)", title, url)
    text <- str_replace_all(text, fixed(url), md_link)
  }

  return(text)
}

# Example usage
#text <- "Check out this website: https://www.dol.gov/sites/dolgov/files/OWCP/energy/regs/compliance/Outreach/Outreach_Presentation/lmn_mba06222022.pdf."
#get_page_title("https://www.dol.gov/sites/dolgov/files/OWCP/energy/regs/compliance/Outreach/Outreach_Presentation/lmn_mba06222022.pdf")
#new_text <- replace_urls_with_md(text)
#print(new_text)


calculateBudgetSummary = function(data, phase, overhead, n_weeks)
{

  budget =
    data %>%
    filter(Phase == !!phase) %>%
    arrange(FTE) %>%
    select(Responsibility, Description, FTE, Rate) %>%

    ### Add weekly cost per employee and overhead
    mutate(
      `Weekly Rate` = FTE * Rate * 40,
      Overhead = `Weekly Rate` * overhead
    ) %>%

    ### Create rows for totals
    rbind(

      ### Simple sum
      summarize(.,
                Responsibility = "Subtotal per Week",
                Description = "",
                FTE = sum(FTE),
                Rate = NA,
                `Weekly Rate` = sum(`Weekly Rate`),
                Overhead = sum(Overhead)
      ),

      ### Add overhead and weekly rate
      summarize(.,
                Responsibility = "Total per Week",
                Description = "",
                FTE = sum(FTE),
                Rate = NA,
                `Weekly Rate` = sum(`Weekly Rate`)*(1 + overhead),
                Overhead = NA
      ),

      ### Multiply by number of weeks
      summarize(.,
                Responsibility = str_glue("Total for {n_weeks} Weeks"),
                Description = "",
                FTE = NA,
                Rate = NA,
                `Weekly Rate` = sum(`Weekly Rate`)*(1 + overhead)*n_weeks,
                Overhead = NA
      )
    )

  return(budget)

}
