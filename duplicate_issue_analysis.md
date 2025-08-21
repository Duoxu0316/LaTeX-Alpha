# Duplicate Paragraph Issue Analysis

## Problem
The scraper is extracting some paragraphs twice. Example:
- Text: "literal: the (old) dynastic text (Hamlet); music as the tension between temporality and an eternal present (Mahler's Sixth); the search for bourgeois Myth, the transition from absolutism to the nineteenth century (Faust)"
- HTML: `<p rend="block"><span class="small-caps">literal:</span> the (old) dynastic text (<em>Hamlet</em>)...`

## Likely Cause
Looking at the body content extraction code:

```r
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
}
```

The selector includes both:
1. `.body_text` - which might be a container div/section
2. `p` - which selects all paragraphs

If a `<p>` element is inside a `.body_text` container, it could be processed twice:
- Once when processing the `.body_text` container (which extracts all text within it)
- Once when processing the `p` element directly

## Solution Options

### Option 1: Exclude paragraphs already inside .body_text
Use XPath or CSS to select only paragraphs NOT inside .body_text containers

### Option 2: Track processed nodes
Keep track of which DOM nodes have been processed to avoid duplication

### Option 3: Use more specific selectors
Instead of selecting both containers and their children, select only the appropriate level

### Option 4: Process in hierarchical order
Process containers first, marking their children as processed