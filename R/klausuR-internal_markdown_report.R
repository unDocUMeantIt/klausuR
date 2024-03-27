## function write_markdown()
# takes data from an evaluated test and formats it into an rmarkdown file
#' @importFrom yaml as.yaml
write_markdown <- function(
    file
  , path
  , header
  , pre_body = ""
  , body = system.file(
      file.path("rmarkdown", "templates", "reports", "resources", "template_individual_body.Rmd"),
      package="klausuR"
    )
  , template = system.file(
      file.path("rmarkdown", "templates", "reports", "resources", "template_individual.latex"),
      package="klausuR"
    )
  , save = FALSE
  , pdf = FALSE
){

  document_body <- paste0(
      pre_body
    , paste0(
          readLines(body)
        , collapse="\n"
      )
    , collapse="\n"
  )

  content <- paste(
    "---"
    , yaml::as.yaml(header)
    , "---\n"
    , document_body
    , sep="\n"
  )

  if(any(isTRUE(save), isTRUE(pdf))){
    tmp_path <- tempfile("klausuR")
    if(!dir.create(tmp_path, recursive=TRUE)){
      stop(simpleError("Couldn't create temporary directory!"))
    } else {}

    # if the function is done, remove the tempdir
    on.exit(
      if(!identical(path, tmp_path)){
        unlink(tmp_path, recursive=TRUE)
      } else {}
    )

    tmp_file <- file.path(tmp_path, file)

    cat(content, file=tmp_file)

    if(isTRUE(pdf)){
      tmp_pdf <- rmarkdown::render(
        tmp_file,
        rmarkdown::pdf_document(
          template=template
        ),
        quiet=TRUE
      )
      file.copy(
        from=tmp_pdf,
        to=path,
        overwrite=TRUE
      )
    } else {}

    if(isTRUE(save)){
      file.copy(
        from=tmp_file,
        to=path,
        overwrite=TRUE
      )
    } else {}

    return(invisible(NULL))
  } else {
    return(content)
  }
} ## end function write_markdown()


## function filename_from_df()
# takes a data frame, fetches the row with a matching matriculation number
# and tries to generate a file name from the column names given as file_name
# - file_name: a character vector, allowed are all column names of df plus the values of "also_valid"
filename_from_df <- function(
    matn
  , file_name
  , also_valid = c("_", "-")
  , df
){
  if(!matn %in% df[["MatrNo"]]){
    stop(simpleError(paste0("MatrNo not found in data:\n  ", matn)))
  } else {
    this_result <- df[df[["MatrNo"]] %in% matn,, drop = FALSE]
    if(nrow(this_result) > 1){
      stop(simpleError(paste0("Multiple matches found for MatrNo:\n  ", matn)))
    } else {}
  }

  this_file_name <- paste0(sapply(
    file_name,
    function(col){
      if(col %in% also_valid){
        return(col)
      } else {
        # this could have been a job for match_arg(), but unfortunately
        # it will not fail if "several.ok" is TRUE and at least one match is valid
        # since we loop through individually anyway, check this here
        if(col %in% colnames(this_result)){
          return(file.umlaute(this_result[[col]]))
        } else {
          stop(simpleError(paste0("Invalid column name:\n  ", col)))
        }
      }
    }
  ), collapse="")
  
  return(this_file_name)
} ## end function filename_from_df()
