library(tidyverse)
library(yaml)

# Check the drafts_from_obsidian folder for any new entries
# And make a quarto folder and formatted post from them if so
make_obsidiandrafts = function(){
  
  # Made the rds in the console
  # Check current filenames against it
  lastsavedfilenames = readRDS('drafts/drafts_from_obsidian/listofalreadyprocessed.rds')
  
  currentfilenamesinfolder = list.files('drafts/drafts_from_obsidian/', pattern = "*.qmd")
  
  newfiles = currentfilenamesinfolder[!currentfilenamesinfolder %in% lastsavedfilenames]
  
  for(i in newfiles) format_obsidianexport_forblog(str_split_i(i,"\\.",1))
  
}


# Short file name only as it'll always be in the same folder
# Then create its own draft folder and save as the correct filename for use with Quarto
# Minus the QMD so we can use in naming folder as well
format_obsidianexport_forblog = function(shortfilename){

  obstext = read_file(paste0('drafts/drafts_from_obsidian/',shortfilename,'.qmd'))
  
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
  # uniquetags = c('tagone','tagtwo','tagthree')
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
  
  # If folder doesn't exist, make it
  dir.create(file.path(paste0('drafts/',shortfilename)), showWarnings = FALSE)
  
  write_file(
    paste0("---\n",finishedyaml,"---\n",result),
    paste0('drafts/',shortfilename,'/index.qmd')
  )

}




# Both chatGPT generated functions below

convert_obsidian_links <- function(text, base_url, note_slug = NULL) {
  # Ensure base_url doesn't end with a trailing slash
  base_url <- sub("/+$", "", base_url)
  
  # Helper: slugify text -> URL-friendly
  slugify <- function(x) {
    x <- tolower(trimws(x))
    x <- gsub("[\\s_]+", "-", x, perl = TRUE)          # spaces/underscores -> -
    x <- gsub("[^a-z0-9-]", "", x, perl = TRUE)        # keep only a-z, 0-9, -
    x <- gsub("-+", "-", x, perl = TRUE)               # collapse multiple -
    x <- gsub("^-+|-+$", "", x, perl = TRUE)           # trim leading/trailing -
    x
  }
  
  # Pattern to match [[...]]
  pattern <- "\\[\\[([^\\]]+)\\]\\]"
  
  # Vectorised over text
  vapply(text, function(single) {
    # Find all matches
    m <- gregexpr(pattern, single, perl = TRUE)
    if (m[[1]][1] == -1) {
      # No wikilinks in this string
      return(single)
    }
    
    matches <- regmatches(single, m)[[1]]             # full [[...]] strings
    inners  <- gsub("^\\[\\[|\\]\\]$", "", matches)   # inner content without [[ ]]
    
    replacements <- character(length(matches))
    
    for (i in seq_along(inners)) {
      inner <- inners[i]
      
      # Split alias: [[target|display]]
      parts <- strsplit(inner, "\\|", perl = TRUE)[[1]]
      target <- parts[1]
      alias  <- if (length(parts) > 1) parts[2] else NULL
      
      # Parse the target: note, heading, block-ref
      note    <- target
      heading <- NULL
      
      # Strip block ref: [[Note^block-id]] or [[^block-id]]
      block_pos <- regexpr("\\^", note)
      if (block_pos[1] != -1) {
        note <- substr(note, 1, block_pos[1] - 1)
      }
      
      # Extract heading: [[Note#Heading]] or [[#Heading]]
      hash_pos <- regexpr("#", note)
      if (hash_pos[1] != -1) {
        heading <- substr(note, hash_pos[1] + 1, nchar(note))
        note    <- substr(note, 1, hash_pos[1] - 1)
      }
      
      note <- trimws(note)
      
      # Decide on link text (display)
      display <- if (!is.null(alias)) {
        alias
      } else if (!is.null(heading) && nzchar(heading)) {
        heading
      } else if (nzchar(note)) {
        note
      } else {
        inner
      }
      
      # Determine the note slug/path for the URL
      if (nzchar(note)) {
        note_slug_path <- slugify(note)
      } else if (!is.null(note_slug)) {
        # For [[#Heading]] or [[^block]] referencing the current note
        note_slug_path <- note_slug
      } else {
        # No way to infer which note this refers to: leave as original wikilink
        replacements[i] <- matches[i]
        next
      }
      
      # Build URL
      url <- paste0(base_url, "/", note_slug_path)
      if (!is.null(heading) && nzchar(heading)) {
        anchor <- slugify(heading)
        if (nzchar(anchor)) {
          url <- paste0(url, "#", anchor)
        }
      }
      
      replacements[i] <- sprintf("[%s](%s)", display, url)
    }
    
    # Plug replacements back into the string
    regmatches(single, m)[[1]] <- replacements
    single
  }, FUN.VALUE = character(1))
}





extract_frontmatter <- function(text) {
  # Split into lines
  lines <- unlist(strsplit(text, "\n", fixed = TRUE))
  
  # No lines / too short
  if (length(lines) < 3) {
    return(list(yaml = NULL, yaml_text = NULL, body = text))
  }
  
  # Require first line to be --- (start of YAML)
  if (trimws(lines[1]) != "---") {
    return(list(yaml = NULL, yaml_text = NULL, body = text))
  }
  
  # Find the closing --- (after the first line)
  end_idx_rel <- which(trimws(lines[-1]) == "---")[1]
  if (is.na(end_idx_rel)) {
    # No closing ---
    return(list(yaml = NULL, yaml_text = NULL, body = text))
  }
  end_idx <- end_idx_rel + 1  # adjust for dropping first line
  
  # YAML lines are between the two ---
  yaml_lines <- lines[2:(end_idx - 1)]
  yaml_text  <- paste(yaml_lines, collapse = "\n")
  
  # Body is everything after the closing ---
  if (end_idx + 1 <= length(lines)) {
    body <- paste(lines[(end_idx + 1):length(lines)], collapse = "\n")
  } else {
    body <- ""
  }
  
  # Parse YAML safely
  yaml_list <- tryCatch(
    yaml::yaml.load(yaml_text),
    error = function(e) {
      warning("Failed to parse YAML frontmatter: ", conditionMessage(e))
      NULL
    }
  )
  
  list(
    yaml      = yaml_list,
    yaml_text = yaml_text,
    body      = body
  )
}







