#' "Kuopio University Hospital" (KUH) Themed Beamer Presentation Template for RMarkdown
#'
#' Generates from an RMarkdown file a Beamer presentation with "Kuopio University Hospital" (KUH)
#' colors and identity standards.
#'
#' @inheritParams rmarkdown::beamer_presentation
#'
#' @return
#' A modified `beamer_presentation`  based on the "Kuopio University Hospital" (KUH)
#' Beamer themed template.
#'
#' @export
#'
#' @author
#' Juuso Tamminen (Theme Hooks and Beamer Template Modifications)
#' See AUTHORS for more specific details behind each of the contributions.
#'
#' @examples
#' \dontrun{
#' # Generate slide deck from beamer template
#' rmarkdown::draft("slide_deck.Rmd", template = "kuh_article", package = "kuhdoctemplate")
#'
#' # Compile the document
#' rmarkdown::render("slide_deck/slide_deck.Rmd")
#' }
kuh_article <- function(toc = FALSE,
                       slide_level = 2,
                       incremental = FALSE,
                       fig_width = 10,
                       fig_height = 7,
                       fig_crop = TRUE,
                       fig_caption = TRUE,
                       dev = 'pdf',
                       df_print = "default",
                       fonttheme = "default",
                       highlight = "default",
                       keep_tex = FALSE,
                       latex_engine = "xelatex",
                       citation_package = c("default", "natbib", "biblatex"),
                       includes = NULL,
                       md_extensions = NULL,
                       pandoc_args = NULL){

  template <- find_resource("kuh_article", "template.tex")

  load_resources_if_missing("kuh_article", c("kuh.cls", "KYS_sin_fi_print_pdf.pdf", "KYS_val_fi_print_pdf.pdf"))

  rmarkdown::pdf_document(template = template,
                                 toc = toc,
                                 slide_level = slide_level,
                                 incremental = incremental,
                                 fig_width = fig_width,
                                 fig_height = fig_height,
                                 fig_crop = fig_crop,
                                 fig_caption = fig_caption,
                                 dev = dev,
                                 df_print = df_print,
                                 fonttheme = fonttheme,
                                 highlight = highlight,
                                 keep_tex = keep_tex,
                                 latex_engine = latex_engine,
                                 citation_package = citation_package,
                                 includes = includes,
                                 md_extensions = md_extensions,
                                 pandoc_args = pandoc_args)

}
