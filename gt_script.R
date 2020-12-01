# GT Tutorial - Tidy Tuesday
# December 1, 2020
# Heili Lowman

# install.packages()

# Load packages
library(tidyverse)
library(lubridate)
library(gt)
library(paletteer)

# Get more info
?pizzaplace

# Load our data
pizza <- gt::pizzaplace
View(pizza)

# Today, we are interested in making a table of pizza sales by month and by type of pizza.

# (1) Make a new column for month.

pizza <- pizza %>% # Take the original dataset
  mutate(date_format = as_date(date)) %>% # Create newly formatted date column
  mutate(month_num = month(date_format)) # Create new month column

# (2) Create a summary stats table.

pizza_money <- pizza %>% # Take the original dataset
  group_by(month_num, type) %>% # Group by month and pizza type
  mutate(type = factor(type,
    levels = c("classic", "veggie", "chicken", "supreme"))) %>% # Releveling pizza types
  summarize(sold = n(), profit = sum(price)) %>% # Summarize profits by defined groups
  ungroup() # Always make sure to ungroup after you group!!

# Build our table below

pizza_table <- pizza_money %>%
  
  # Make some edits to underlying dataset.
  filter(month_num > 9) %>% # Filter for Q4
  
  mutate(month_name = case_when(month_num == 10 ~ "October",
    month_num == 11 ~ "November",
    month_num == 12 ~ "December")) %>% # Create month names
  
  gt(groupname_col = "month_name", rowname_col = "type") %>% # Base table creation/call
  
  # Format our table
  cols_hide(columns = vars(month_num)) %>% # Hide column
  fmt_currency(columns = vars(profit), currency = "USD") %>% # Format profit into dollars
  
  # Add helpful titles
  tab_header(title = "Q4 Monthly Pizza Sales",
    subtitle = "by Heili Lowman") %>% # Adds title & subtitle
  tab_source_note(md("More information can be found at `?pizzaplace`.")) %>% # Adds a nice footer.

  # Add helpful column names.
  cols_label(sold = "Pizzas Sold",
    profit = "Profits") %>%
  tab_stubhead(label = "Month") %>% # Renames stubhead
  tab_spanner(label = "Pizzas Sold + Revenue",
    columns = vars(sold, profit)) %>% # Adds spanner
  
  # Add more summary statistics.
  summary_rows(groups = TRUE,
    columns = vars(profit),
    fns = list("TOTAL" = "sum"),
    formatter = fmt_currency, currency = "USD",
    use_seps = TRUE) %>% # Monthly summary rows
  grand_summary_rows(
    columns = vars(profit),
    fns = list("GRAND TOTAL" = "sum"),
    formatter = fmt_currency, currency = "USD",
    use_seps = TRUE) %>% # Grand summary row
  
  # Add some color.
  data_color(
    columns = vars(profit),
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::teal_material") %>%
        as.character(),
      domain = NULL),
      alpha = 0.75)

pizza_table

# Save our table.
gtsave(pizza_table,
  "pizza_table.png",
  path = "/Users/heilil/Desktop/R_figures")

# End of script.


