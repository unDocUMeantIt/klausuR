#' Create reports from evaluated tests
#' 
#' This is a rewrite of \code{klausur.report()} using RMarkdown instead of \LaTeX{}.
#'
#' If you want to use a certain value from the results slot in \code{klsr} in the code of \code{body}, you can use \code{\\newcommand} in your \code{template} and use the command in the markdown.
#'
#' @param klsr An object of class \code{klausuR}.
#' @param matn TODO
#' @param path TODO
#' @param file_name Character vector, either defining the output file name directly (if \code{matn="anon"} or \code{matn="glob"}), or defining the column names of the results slot in \code{klsr} whose
#'    values should be used to create individual file names.
#' @param also_valid Character vector, values for \code{file_name} that should also be considered valid even if not column names of the results slot in \code{klsr}. They will be used as-is.
#' @param save TODO
#' @param pdf TODO
#' @param body TODO
#' @param template TODO
#' @param header A list of variables to be defined in the YAML header of each RMarkdown file to be available for use in \code{template}.
#'    Lists of lists are supported, see \code{\link[yaml:as_yaml]{yaml::as_yaml}} for details.
#'    For individual reports, \code{header} will get a nested variable named \code{results} including all variables/columns of the results slot in \code{klsr} with their respective values
#'    of the row matching \code{matn}.
#' @param ... TODO
#' @docType methods
#' @rdname report-methods
#' @export
setGeneric(
  "report",
  function(
      klsr
    , matn
    , path
    , file_name = "MatrNo"
    , also_valid = c("_", "-")
    , save = FALSE
    , pdf = FALSE
    , body
    , template
    , header
    , ...
  ) standardGeneric("report")
)

#' @docType methods
#' @aliases report,klausuR-method
#' @rdname report-methods
setMethod(
  "report",
  signature(klsr="klausuR"),
  function(
      klsr
    , matn
    , path
    , file_name = "MatrNo"
    , also_valid = c("_", "-")
    , save = FALSE
    , pdf = FALSE
    , body
    , template
    , header
    , ...
  ){

    if(!dir.exists(path)){
      stop(simpleError(paste0("Output directory not found:\n  ", path)))
    } else {}

    klsr_results  <- slot(klsr, "results")
    klsr_points   <- slot(klsr, "points")
    klsr_answ     <- slot(klsr, "answ")
    klsr_anon     <- slot(klsr, "anon")
    klsr_corr     <- slot(klsr, "corr")

    if(identical(matn, "glob")){
      this_file <- paste(file_name, sep="", collapse="")
      if(missing(template)){
        template <- system.file(
          file.path("rmarkdown", "templates", "reports", "resources", "template_global.latex")
          , package="klausuR"
        )
      } else {}
      if(missing(body)){
        body <- system.file(
          file.path("rmarkdown", "templates", "reports", "resources", "template_global_body.Rmd")
          , package="klausuR"
        )
      } else {}
      pre_body <- paste(
          "<!--"
        , "```{r setup, include=FALSE, echo=FALSE}"
        , "```"
        , "-->\n\n"
        , sep = "\n"
      )
    } else if(identical(matn, "anon")) {
      this_file <- paste(file_name, sep="", collapse="")
      if(missing(template)){
        template <- system.file(
          file.path("rmarkdown", "templates", "reports", "resources", "template_anonymous.latex")
          , package="klausuR"
        )
      } else {}
      if(missing(body)){
        body <- system.file(
          file.path("rmarkdown", "templates", "reports", "resources", "template_anonymous_body.Rmd")
          , package="klausuR"
        )
      } else {}
      pre_body <- paste(
          "<!--"
        , "```{r setup, include=FALSE, echo=FALSE}"
        , "```"
        , "-->\n\n"
        , sep = "\n"
      )
    } else {
      if(missing(template)){
        template <- system.file(
          file.path("rmarkdown", "templates", "reports", "resources", "template_individual.latex")
          , package="klausuR"
        )
      } else {}
      if(missing(body)){
        body <- system.file(
          file.path("rmarkdown", "templates", "reports", "resources", "template_individual_body.Rmd")
          , package="klausuR"
        )
      } else {}

#       if(identical(matn, "all")) {
#         for(i in results[["MatrNo"]]){
#           ## loop
#         }
#         if(isTRUE(merge) & isTRUE(pdf)){
#           merge.reports(results=results, text=text, descr=descr, file.name=file.name, pdf=pdf, path=path, path.orig=path.orig, quiet=quiet, fancyhdr=fancyhdr)
#         } else {}
#       } else {
        this_file <- filename_from_df(
            matn = matn
          , file_name = file_name
          , also_valid = also_valid
          , df=klsr_results
        )

        header[["results"]] <- as.list(as.character(klsr_results[klsr_results[["MatrNo"]] == matn,]))
        names(header[["results"]]) <- colnames(klsr_results)
        pre_body <- paste(
            "```{r setup, include=FALSE, echo=FALSE}"
          , paste0("results <- ", paste0(deparse(header[["results"]]), collapse="\n"), collapse="\n")
          , paste0("correct <- ", paste0(deparse(klsr_corr), collapse="\n"), collapse="\n")
          , paste0("answers <- ", paste0(deparse(unlist(klsr_answ[klsr_answ[["MatrNo"]] == matn, names(klsr_corr)])), collapse="\n"), collapse="\n")
          , paste0("points <- ", paste0(deparse(unlist(klsr_points[klsr_points[["MatrNo"]] == matn, names(klsr_corr)])), collapse="\n"), collapse="\n")
          , "all_items <- sort(names(correct))"
          , paste0(
                "results_df <- data.frame(\""
              , ifelse(is.null(header[["labels"]][["answer"]]), "Answer", header[["labels"]][["answer"]]), "\"=answers[all_items], \""
              , ifelse(is.null(header[["labels"]][["correct"]]), "Correct", header[["labels"]][["correct"]]), "\"=correct[all_items], \""
              , ifelse(is.null(header[["labels"]][["points"]]), "Points", header[["labels"]][["points"]]), "\"=points[all_items], row.names=all_items)"
              , collapse=""
            )
          , "```\n\n"
          , sep = "\n"
        )
#       }
    }

    write_markdown(
        file = paste0(this_file, ".Rmd")
      , path = path
      , header = header
      , pre_body = pre_body
      , body = body
      , template = template
      , save = save
      , pdf = pdf
    )

    return(invisible(NULL))
})
