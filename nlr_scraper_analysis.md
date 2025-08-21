# New Left Review Article Scraper - Code Analysis

## Overview
This R script scrapes articles from the New Left Review website and exports the data in both JSON and Excel formats.

## Key Components

### 1. Helper Functions

#### Text Processing Functions:
- **`.collapse_spaces()`**: Removes extra whitespace, newlines, and tabs, collapsing them into single spaces
- **`.keep_newlines()`**: Preserves newlines while cleaning up other whitespace
- **`.fix_opening()`**: Fixes split initials at line starts (e.g., "O nce" → "Once", "I t" → "It")
- **`.fix_heading_split_initial()`**: Specifically fixes split initials in headings (e.g., "I nterview" → "Interview")

#### Content Normalization:
- **`.normalize_dropcaps()`**: Handles dropcap letters (large initial letters) in articles:
  - Removes spacer spans
  - Forces dropcaps to uppercase
  - Handles special spacing for "A" and "I" dropcaps
  
#### List Processing:
- **`.emit_list()`**: Formats HTML lists into text:
  - Ordered lists: "1. item", "2. item"
  - Unordered lists: "• item"

### 2. Main Function: `get_content(url, preview_nchars = 1000)`

#### What it extracts:
1. **Metadata:**
   - Article type (review, article, etc.)
   - Issue number and date
   - Author(s)
   - Title
   
2. **Content:**
   - Introduction section
   - Main body text
   - Preserves structure (headings, paragraphs, blockquotes, lists)
   
3. **Additional Elements:**
   - Footnotes (cleaned and numbered)
   - Italicized text (excluding footnotes)
   - Reviewed items (for book reviews)

#### Processing Steps:
1. Reads HTML from URL using `rvest`
2. Normalizes dropcaps and removes footnote references
3. Extracts metadata from article tags
4. Processes intro and body sections maintaining DOM order
5. Combines all parts with proper spacing
6. Applies text fixes for split initials

#### Output Formats:
1. **JSON file**: Complete article data with all fields
2. **Excel file**: Preview format with first 1000 characters of article

## Special Features

### Review Detection
The code has robust review detection using multiple methods:
- CSS class checking (`article--review`)
- Presence of `.reviewed-item` nodes
- Data attribute checking

### Text Preservation
- Maintains paragraph structure
- Preserves blockquotes with proper spacing
- Handles ordered and unordered lists
- Keeps headings separate with blank lines

### Smart Text Fixes
- De-duplicates doubled initials
- Joins split first letters of words
- Special handling for "I" and "A" as standalone words
- Uppercase normalization for dropcaps

## File Naming
Files are named based on the URL path after "/issues/", with slashes replaced by hyphens to avoid directory creation.

## Dependencies
- `rvest`: Web scraping
- `xml2`: XML/HTML manipulation
- `openxlsx`: Excel file creation
- `jsonlite`: JSON export
- `dplyr`: Data manipulation
- `purrr`: Functional programming
- `tibble`: Data frames

## Potential Areas for Modification
1. Content extraction scope
2. Text processing rules
3. Output formats
4. Metadata fields
5. Preview length
6. File naming convention
7. Additional content elements (images, tables, etc.)