# Convert obsidian quarto export links to exobrain format
# Test
library(tidyverse)
library(yaml)
source('functions/obsidian_functions.R')

obstext = read_file('drafts/drafts_from_obsidian/Game-money-magic.qmd')

# While we're here, let's tweak the YAML too.
# read_yaml('drafts/drafts_from_obsidian/Game-money-magic.qmd') 
yaml = extract_frontmatter(obstext)
# yaml[[1]]

# Check for duplicate tags, keep uniques
uniquetags = unique(yaml[[1]]$tags)

# Keep only title and date
outputyaml = yaml[[1]][1:2]

# Convert tags into quarto blog categories
# Test string
uniquetags = c('tagone','tagtwo','tagthree')
# paste(testtags,collapse = ',')
# Does it work if only one tag? Yep.
# paste('singletag',collapse = ',')


outputyaml$categories = paste0("[",paste(uniquetags,collapse = ','),"]")

# https://cran.r-project.org/web/packages/yaml/vignettes/yaml.html
attr(outputyaml$title, "quoted") <- TRUE
# class(outputyaml$categories) <- "verbatim"#Won't work

# Turn the block-style categories list into inline flow style
finishedyaml = as.yaml(outputyaml)

# Fix the category issue manually
# i.e. it ends up in single quote marks
finishedyaml <- gsub(
  # "categories:\n- ([^\n]+)\n- ([^\n]+)\n",
  # "categories: [\\1, \\2]\n",
  "\\'\\[","[",
  finishedyaml
)

finishedyaml <- gsub(
  # "categories:\n- ([^\n]+)\n- ([^\n]+)\n",
  # "categories: [\\1, \\2]\n",
  "\\]\\'","]",
  finishedyaml
)


# as.yaml(outputyaml)

# result = convert_obsidian_links(obstext, 'https://exobrain.coveredinbees.org')
result = convert_obsidian_links(yaml$body, 'https://exobrain.coveredinbees.org')

write_file(paste0("---\n",finishedyaml,"---\n",result),'drafts/drafts_from_obsidian/testlinkchange.qmd')
