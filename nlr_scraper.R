library(rvest)
library(xml2)
library(openxlsx)
library(jsonlite)
library(dplyr)
library(purrr)
library(tibble)

.collapse_spaces <- function(x) {
  x <- gsub("\\s*[\r\n]+\\s*", " ", x, perl = TRUE)
  x <- gsub("\t+", " ", x, perl = TRUE)
  x <- gsub(" {2,}", " ", x, perl = TRUE)
  trimws(x)
}

.keep_newlines <- function(x) {
  lines <- unlist(strsplit(x, "\n", fixed = TRUE))
  lines <- trimws(lines)
  x <- paste(lines, collapse = "\n")
  x <- gsub("\r", "", x, perl = TRUE)
  x <- gsub("\t+", " ", x, perl = TRUE)
  x <- gsub(" {2,}", " ", x, perl = TRUE)
  x
}

# Join split initials at line starts safely:
# - De-dupe doubled initial (e.g., "A\nA hundred" -> "A hundred")
# - Join "O nce" style for non A/I initials
# - Join "I t" (single-letter remainder) -> "It", but DO NOT join "I am" or "I think"
.fix_opening <- function(txt) {
  if (!nzchar(txt)) return(txt)

  # 1) De-dupe doubled initial across whitespace/newlines
  txt <- sub('(^|\\n)([""'\']?)([A-Za-z])[ \\t\\r\\n]*\\3\\b', '\\1\\2\\3', txt, perl = TRUE)

  # 2) Join split first two letters for non A/I (e.g., "O nce" -> "Once")
  txt <- sub('(^|\\n)([""'\']?)([B-HJ-Zb-hj-z])\\s+([A-Za-z])', '\\1\\2\\3\\4', txt, perl = TRUE)

  # 3) Join "I t" -> "It" (single-letter remainder only)
  txt <- sub('(^|\\n)([""'\']?)I\\s+([A-Za-z])\\b', '\\1\\2I\\3', txt, perl = TRUE)

  txt
}

# Headings-only fix: join "I nterview" -> "Interview" safely (won't touch body text)
.fix_heading_split_initial <- function(x) {
  if (!nzchar(x)) return(x)
  sub('^([""'\']?)I\\s+([A-Za-z]{2,})', '\\1I\\2', x, perl = TRUE)
}

# Drop-in replacement: force the injected dropcap to be UPPERCASE
.normalize_dropcaps <- function(scope) {
  if (!length(scope)) return(invisible(NULL))

  # Remove spacer spans like style="display:inline-block; ..."
  spacers <- html_elements(
    scope,
    xpath = ".//span[contains(translate(@style,'ABCDEFGHIJKLMNOPQRSTUVWXYZ','abcdefghijklmnopqrstuvwxyz'),'display: inline-block')]"
  )
  if (length(spacers)) xml_remove(spacers)

  # Find the first .first-words in scope
  fw_node <- html_element(scope, ".first-words")
  if (!length(fw_node)) return(invisible(NULL))

  # Paragraph (or nearest ancestor) containing .first-words
  para <- xml_find_first(fw_node, "ancestor::p[1]")
  if (!length(para)) para <- xml_parent(fw_node)

  # Capture first dropcap letter (if any) from paragraph or scope
  dc_nodes <- html_elements(para, ".dropcap")
  if (!length(dc_nodes)) dc_nodes <- html_elements(scope, ".dropcap")
  dc_char <- NA_character_
  if (length(dc_nodes)) {
    dc_txt <- rvest::html_text2(dc_nodes[1])
    dc_char <- substr(trimws(dc_txt), 1, 1)
  }

  # Remove all dropcaps
  if (length(dc_nodes)) xml_remove(dc_nodes)

  # Remove stray single-letter text nodes before .first-words in this paragraph
  if (length(para)) {
    stray <- xml_find_all(
      para,
      "./text()[normalize-space(.) and string-length(normalize-space(.))=1 and following-sibling::*[contains(@class,'first-words')]]"
    )
    if (length(stray)) xml_remove(stray)
  }

  # Inject the dropcap (forced UPPERCASE) with smart spacing for A/I
  if (!is.na(dc_char) && grepl("^[A-Za-z]$", dc_char)) {
    fw_text <- rvest::html_text2(fw_node)

    # Force uppercase initial regardless of small-caps/lowercase in source
    dc_out <- toupper(dc_char)

    # Space rule: if dropcap is A or I and the first token of fw_text has length >= 2,
    # insert a space ("A hundred"); if token length == 1 (e.g., "n early"), no space ("In early").
    first_token <- sub("^([A-Za-z]+).*", "\\1", fw_text)
    need_space <- toupper(dc_char) %in% c("A", "I") && nchar(first_token) >= 2

    xml_set_text(fw_node, paste0(dc_out, if (need_space) " " else "", fw_text))
  }
}

# Format list items as "1. ..." (for <ol>) or "• ..." (for <ul>)
.emit_list <- function(list_node, ordered = TRUE) {
  start_val <- suppressWarnings(as.integer(html_attr(list_node, "start")))
  if (is.na(start_val)) start_val <- 1
  li_nodes <- html_elements(list_node, "li")
  out <- character(0)
  if (!length(li_nodes)) return(out)
  for (i in seq_along(li_nodes)) {
    tx <- rvest::html_text2(li_nodes[[i]])
    tx <- gsub("footnote", "", tx, ignore.case = TRUE, perl = TRUE)
    tx <- .collapse_spaces(tx)
    if (!nzchar(tx)) next
    prefix <- if (ordered) sprintf("%d. ", start_val + i - 1) else "• "
    out <- c(out, paste0(prefix, tx))
  }
  out
}

get_content <- function(
  url,
  preview_nchars = 1000
) {
  page  <- read_html(url)
  body  <- html_element(page, "div.article-body-wrapper .article-body")
  intro <- html_element(page, "div.article-intro")

  # Normalize dropcaps/spacers before extraction
  if (length(intro)) .normalize_dropcaps(intro)
  if (length(body))  .normalize_dropcaps(body)

  # Remove in-text footnote refs and superscripts (defensive)
  xml_find_all(page, "//a[contains(@class,'article-body__note-ref') or contains(@class,'note-ref')]" ) %>% xml_remove()
  xml_find_all(page, "//sup") %>% xml_remove()

  # ---- Metadata
  art <- html_element(page, "article.article--full")

  # initial type guess
  type <- {
    t <- html_attr(art, "data-articles-article-type-value")
    if (is.na(t) || !nzchar(t)) {
      cls <- html_attr(art, "class")
      vals <- character(0)
      if (!is.na(cls)) {
        parts <- unlist(strsplit(cls, "\\s+"))
        vals <- sub("^article--", "", grep("^article--", parts, value = TRUE))
        vals <- vals[vals != "full"]
      }
      if (length(vals)) tolower(vals[1]) else "unknown"
    } else tolower(t)
  }

  # robust review detection (override type if needed)
  class_str <- html_attr(art, "class")
  has_review_class <- if (!is.na(class_str)) grepl("\\barticle--review\\b", class_str) else FALSE
  has_review_nodes <- length(html_elements(page, ".reviewed-item")) > 0
  data_type <- tolower(tryCatch(html_attr(art, "data-articles-article-type-value"), error = function(e) NA_character_))
  has_review_data <- identical(data_type, "review")
  is_review <- isTRUE(has_review_class) || isTRUE(has_review_nodes) || isTRUE(has_review_data)
  if (is_review) type <- "review"

  issue <- {
    top <- html_element(page, ".show-for-print.text-center")
    if (length(top) && nzchar(html_text2(top))) {
      trimws(html_text2(top))
    } else {
      num <- html_element(page, ".article-publication-details__issue_number") %>%
        { if (length(.)==0) NA else html_text(., trim = TRUE) }
      dat <- html_element(page, ".article-publication-details time") %>%
        { if (length(.)==0) NA else html_text(., trim = TRUE) }
      if (!is.na(num) && !is.na(dat)) .collapse_spaces(paste0("NLR ", num, ", ", gsub("^,\\s*", "", dat))) else NA_character_
    }
  }

  author <- {
    metas <- html_elements(page, "meta[itemprop='author']")
    a <- if (length(metas)) unique(na.omit(html_attr(metas, "content"))) else character(0)
    a <- a[nzchar(a)]
    if (length(a)) paste(a, collapse = "; ") else {
      by <- html_element(page, "p.article-authors")
      if (length(by)) .collapse_spaces(html_text(by, trim = TRUE)) else NA_character_
    }
  }

  title <- {
    tnode <- html_element(page, "h1.article-title[itemprop='headline']")
    if (length(tnode)) html_text(tnode, trim = TRUE) else NA_character_
  }

  # ---- Footnotes
  fn_nodes <- html_nodes(page, "footer.article-footnotes .article-footnote")
  footnotes <- purrr::map_chr(fn_nodes, ~{
    num <- html_node(.x, "a.number") %>% html_text(trim = TRUE)
    content <- html_text(.x, trim = TRUE)
    content <- sub("^\\d+\\s*", "", content, perl = TRUE)
    content <- gsub("\\s*[\r\n]+\\s*", " ", content, perl = TRUE)
    content <- gsub("\t+", " ", content, perl = TRUE)
    content <- gsub(" {2,}", " ", content, perl = TRUE)
    sprintf("%s. %s", num, content)
  })

  # ---- Italics (intro + body, exclude footnotes)
  intro_italics <- if (length(intro)) {
    html_nodes(intro, "em") %>% html_text(trim = TRUE) %>%
      gsub("\\s*[\r\n]+\\s*", " ", ., perl = TRUE) %>% gsub("\t+", " ", ., perl = TRUE) %>%
      gsub(" {2,}", " ", ., perl = TRUE) %>% trimws()
  } else character(0)

  body_italics <- if (length(body)) {
    html_nodes(body, "em") %>% html_text(trim = TRUE) %>%
      gsub("\\s*[\r\n]+\\s*", " ", ., perl = TRUE) %>% gsub("\t+", " ", ., perl = TRUE) %>%
      gsub(" {2,}", " ", ., perl = TRUE) %>% trimws()
  } else character(0)

  footnote_italics <- html_nodes(page, "footer.article-footnotes em") %>%
    html_text(trim = TRUE) %>% gsub("\\s*[\r\n]+\\s*", " ", ., perl = TRUE) %>%
    gsub("\t+", " ", ., perl = TRUE) %>% gsub(" {2,}", " ", ., perl = TRUE) %>% trimws()

  italics <- setdiff(
    unique(c(intro_italics, body_italics)[nzchar(c(intro_italics, body_italics))]),
    unique(footnote_italics[nzchar(footnote_italics)])
  )

  # ---- Reviewed items (search anywhere on the page)
  reviewed_items <- character(0)
  if (is_review || has_review_nodes) {
    reviewed_items <- html_elements(page, ".reviewed-item") %>% purrr::map_chr(function(it) {
      parts <- c(
        html_element(it, ".reviewed-item__authors") %>% { if (length(.)==0) "" else html_text(., trim = TRUE) },
        html_element(it, ".reviewed-item__title") %>% { if (length(.)==0) "" else html_text(., trim = TRUE) },
        html_element(it, ".reviewed-item__publication-details") %>% { if (length(.)==0) "" else html_text(., trim = TRUE) },
        html_element(it, ".reviewed-item__technical-details") %>% { if (length(.)==0) "" else html_text(., trim = TRUE) }
      )
      parts <- parts[nzchar(parts)]
      if (length(parts) > 1) {
        keep <- c(TRUE, parts[-1] != parts[-length(parts)])
        parts <- parts[keep]
      }
      paste(parts, collapse = ", ")
    })
    reviewed_items <- unique(reviewed_items[nzchar(reviewed_items)])
  }

  # ---- Intro nodes (DOM order)
  intro_parts <- character(0)
  if (length(intro)) {
    intro_nodes <- html_elements(intro, "h1, h2, h3, p, blockquote, ol, ul")
    for (nd in intro_nodes) {
      tag <- xml_name(nd)
      if (tag %in% c("h1","h2","h3")) {
        txt <- rvest::html_text2(nd) %>% .collapse_spaces() %>% .fix_heading_split_initial()
        if (nzchar(txt)) intro_parts <- c(intro_parts, txt, "")
      } else if (tag == "blockquote") {
        txt <- rvest::html_text2(nd) %>% .keep_newlines()
        if (nzchar(txt)) intro_parts <- c(intro_parts, "", txt, "")
      } else if (tag %in% c("ol","ul")) {
        lines <- .emit_list(nd, ordered = (tag == "ol"))
        if (length(lines)) intro_parts <- c(intro_parts, lines, "")
      } else { # p and others
        txt <- rvest::html_text2(nd) %>% .collapse_spaces()
        if (nzchar(txt)) intro_parts <- c(intro_parts, txt)
      }
    }
  }

  # ---- Body content nodes (DOM order)
  content_nodes <- if (length(body)) {
    html_elements(
      body,
      paste(
        "h2.subtitle",
        "h3",
        ".subsection h4",
        ".question",
        "blockquote",
        ".body_text",
        "ol",
        "ul",
        "p",
        sep = ", "
      )
    )
  } else xml2::xml_missing()

  body_parts <- character(0)
  if (length(content_nodes)) {
    for (nd in content_nodes) {
      tag <- xml_name(nd)
      if (tag %in% c("h2","h3","h4")) {
        txt <- rvest::html_text2(nd) %>% .collapse_spaces() %>% .fix_heading_split_initial()
        if (nzchar(txt)) body_parts <- c(body_parts, txt, "")
      } else if (tag == "blockquote") {
        txt <- rvest::html_text2(nd) %>% .keep_newlines()
        if (nzchar(txt)) body_parts <- c(body_parts, "", txt, "")
      } else if (tag %in% c("ol","ul")) {
        # Avoid double-counting lists that are inside .body_text/.question (handled below)
        inside_container <- length(xml_find_first(nd, "ancestor::*[contains(@class,'body_text') or contains(@class,'question')]")) > 0
        if (inside_container) next
        lines <- .emit_list(nd, ordered = (tag == "ol"))
        if (length(lines)) body_parts <- c(body_parts, lines, "")
      } else {
        # e.g., .body_text or .question; if it contains lists, walk its children in order
        has_lists <- length(html_elements(nd, "ol, ul")) > 0
        if (has_lists) {
          kids <- html_elements(nd, "p, blockquote, ol, ul")
          for (kd in kids) {
            ktag <- xml_name(kd)
            if (ktag == "blockquote") {
              ktxt <- rvest::html_text2(kd) %>% .keep_newlines()
              if (nzchar(ktxt)) body_parts <- c(body_parts, "", ktxt, "")
            } else if (ktag %in% c("ol","ul")) {
              lines <- .emit_list(kd, ordered = (ktag == "ol"))
              if (length(lines)) body_parts <- c(body_parts, lines, "")
            } else { # paragraphs
              ktxt <- rvest::html_text2(kd)
              ktxt <- gsub("footnote", "", ktxt, ignore.case = TRUE, perl = TRUE)
              ktxt <- .collapse_spaces(ktxt)
              if (nzchar(ktxt)) body_parts <- c(body_parts, ktxt)
            }
          }
        } else {
          # no lists inside: original behavior
          txt <- rvest::html_text2(nd)
          txt <- gsub("footnote", "", txt, ignore.case = TRUE, perl = TRUE)
          txt <- .collapse_spaces(txt)
          if (nzchar(txt)) body_parts <- c(body_parts, txt)
        }
      }
    }
  }

  # ---- Combine intro + body and tidy blank lines
  parts <- c(intro_parts, if (length(intro_parts) && length(body_parts)) "" else NULL, body_parts)

  if (length(parts)) {
    while (length(parts) && parts[1] == "") parts <- parts[-1]
    while (length(parts) && tail(parts, 1) == "") parts <- parts[-length(parts)]
    if (length(parts) > 1) {
      idx <- c(TRUE, !(parts[-1] == "" & parts[-length(parts)] == ""))
      parts <- parts[idx]
    }
  }

  article_text <- paste(parts, collapse = "\n")
  article_text <- gsub("\n{3,}", "\n\n", article_text, perl = TRUE)
  article_text <- .fix_opening(article_text)

  # ---- Filename base: substring after ".../issues/", sanitize to avoid dirs
  slug <- sub("^https?://[^/]+/issues/([^?#]+).*", "\\1", url)
  file_base <- gsub("[/\\\\]", "-", slug)
  json_file <- paste0(file_base, ".json")
  xlsx_file <- paste0(file_base, ".xlsx")

  # ---- Outputs
  json_data <- list(
    url = url,
    type = type,
    is_review = is_review,
    reviewed_items_count = length(reviewed_items),
    issue = issue,
    author = author,
    title = title,
    article = article_text,
    footnotes = footnotes,
    italics = italics
  )
  if (length(reviewed_items)) json_data$item <- reviewed_items

  jsonlite::write_json(json_data, json_file, pretty = TRUE, auto_unbox = TRUE)

  article_preview <- substr(article_text, 1, preview_nchars)
  footnotes_text <- if (length(footnotes)) paste(footnotes, collapse = "\n") else ""
  italics_text   <- if (length(italics))   paste(italics,   collapse = "\n") else ""
  items_text     <- if (length(reviewed_items)) paste(reviewed_items, collapse = "\n") else NA_character_

  excel_data <- tibble::tibble(
    url = url,
    type = type,
    is_review = is_review,
    reviewed_items_count = length(reviewed_items),
    issue = issue,
    author = author,
    title = title,
    item = items_text,
    article_preview = article_preview,
    footnotes = footnotes_text,
    italics = italics_text
  )

  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "preview")
  openxlsx::writeData(wb, "preview", excel_data)
  openxlsx::setColWidths(wb, "preview", cols = 1:ncol(excel_data), widths = "auto")
  openxlsx::saveWorkbook(wb, xlsx_file, overwrite = TRUE)

  invisible(list(
    files = c(json = json_file, xlsx = xlsx_file),
    url = url,
    type = type,
    is_review = is_review,
    reviewed_items_count = length(reviewed_items),
    issue = issue,
    author = author,
    title = title,
    item = reviewed_items,
    article = article_text,
    footnotes = footnotes,
    italics = italics
  ))
}