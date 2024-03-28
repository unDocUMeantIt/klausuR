## function write_markdown()
# takes data from an evaluated test and formats it into an rmarkdown file
#' @importFrom yaml as.yaml
#' @importFrom rmarkdown render pdf_document
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


## function default_labels()
# sets the lables to use in other functions, by either using a custom set label
# or the language default
default_labels <- function(
    labels = list()
  , lang = "en"
){
  labels_orig <- labels
  labels <- switch(
      lang
    , "de" = list(
          docent = ifelse(is.null(labels[["docent"]]), "Dozent", labels[["docent"]])
        , dateoftest = ifelse(is.null(labels[["dateoftest"]]), "Datum der Klausur", labels[["dateoftest"]])
        , familyname = ifelse(is.null(labels[["familyname"]]), "Name", labels[["familyname"]])
        , firstname = ifelse(is.null(labels[["firstname"]]), "Vorname", labels[["firstname"]])
        , pseudonym = ifelse(is.null(labels[["pseudonym"]]), "Pseudonym", labels[["pseudonym"]])
        , testno = ifelse(is.null(labels[["testno"]]), "Nr.", labels[["testno"]])
        , matno = ifelse(is.null(labels[["matno"]]), "Matrikel-Nr.", labels[["matno"]])
        , matno_abbr = ifelse(is.null(labels[["matno_abbr"]]), "MatrNr.", labels[["matno_abbr"]])
        , pointsachieved = ifelse(is.null(labels[["pointsachieved"]]), "Erreichte Punktzahl", labels[["pointsachieved"]])
        , ofpoints = ifelse(is.null(labels[["ofpoints"]]), "der erreichbaren Punkte", labels[["ofpoints"]])
        , thisresultsin = ifelse(is.null(labels[["thisresultsin"]]), "Daraus ergibt sich die", labels[["thisresultsin"]])
        , mark = ifelse(is.null(labels[["mark"]]), "Note", labels[["mark"]])
        , answer = ifelse(is.null(labels[["answer"]]), "Antwort", labels[["answer"]])
        , correct = ifelse(is.null(labels[["correct"]]), "L\u00f6sung", labels[["correct"]])
        , results = ifelse(is.null(labels[["results"]]), "Ergebnisse", labels[["results"]])
        , points = ifelse(is.null(labels[["points"]]), "Punkte", labels[["points"]])
        , percent = ifelse(is.null(labels[["percent"]]), "Prozent", labels[["percent"]])
        , dist = ifelse(is.null(labels[["dist"]]), "Verteilung der Klausurergebnisse", labels[["dist"]])
        , p_main = ifelse(is.null(labels[["p_main"]]), "Verteilung nach Punkten", labels[["p_main"]])
        , p_xlab = ifelse(is.null(labels[["p_xlab"]]), "Punkte", labels[["p_xlab"]])
        , p_ylab = ifelse(is.null(labels[["p_ylab"]]), "H\u00e4ufigkeit", labels[["p_ylab"]])
        , m_main = ifelse(is.null(labels[["m_main"]]), "Verteilung nach Noten", labels[["m_main"]])
        , m_xlab = ifelse(is.null(labels[["m_xlab"]]), "Note", labels[["m_xlab"]])
        , m_ylab = ifelse(is.null(labels[["m_ylab"]]), "H\u00e4ufigkeit", labels[["m_ylab"]])
        , m_scale = ifelse(is.null(labels[["m_scale"]]), "Notenschl\u00fcssel", labels[["m_scale"]])
        , NRET_expl = ifelse(is.null(labels[["NRET_expl"]]), ". \\emph{Erl\u00e4uterung:} >>+<< -- richtig; >>-<< -- falsch; >>0<< -- keine Angabe; >>*<< -- fehlerhafte Angabe.", labels[["NRET_expl"]])

      )
    # fallback is the english default
    , list(
          docent = ifelse(is.null(labels[["docent"]]), "Docent", labels[["docent"]])
        , dateoftest = ifelse(is.null(labels[["dateoftest"]]), "Date of test", labels[["dateoftest"]])
        , familyname = ifelse(is.null(labels[["familyname"]]), "Name", labels[["familyname"]])
        , firstname = ifelse(is.null(labels[["firstname"]]), "First name", labels[["firstname"]])
        , pseudonym = ifelse(is.null(labels[["pseudonym"]]), "Pseudonym", labels[["pseudonym"]])
        , testno = ifelse(is.null(labels[["testno"]]), "No.", labels[["testno"]])
        , matno = ifelse(is.null(labels[["matno"]]), "Matriculation No.", labels[["matno"]])
        , matno_abbr = ifelse(is.null(labels[["matno_abbr"]]), "MatrNo.", labels[["matno_abbr"]])
        , pointsachieved = ifelse(is.null(labels[["pointsachieved"]]), "Achieved score", labels[["pointsachieved"]])
        , ofpoints = ifelse(is.null(labels[["ofpoints"]]), "of achievable points", labels[["ofpoints"]])
        , thisresultsin = ifelse(is.null(labels[["thisresultsin"]]), "This score gives", labels[["thisresultsin"]])
        , mark = ifelse(is.null(labels[["mark"]]), "mark", labels[["mark"]])
        , answer = ifelse(is.null(labels[["answer"]]), "Answer", labels[["answer"]])
        , correct = ifelse(is.null(labels[["correct"]]), "Solution", labels[["correct"]])
        , results = ifelse(is.null(labels[["results"]]), "Results", labels[["results"]])
        , points = ifelse(is.null(labels[["points"]]), "Points", labels[["points"]])
        , percent = ifelse(is.null(labels[["percent"]]), "Percent", labels[["percent"]])
        , dist = ifelse(is.null(labels[["dist"]]), "Distribution of test results", labels[["dist"]])
        , p_main = ifelse(is.null(labels[["p_main"]]), "Distribution by points", labels[["p_main"]])
        , p_xlab = ifelse(is.null(labels[["p_xlab"]]), "Points", labels[["p_xlab"]])
        , p_ylab = ifelse(is.null(labels[["p_ylab"]]), "Frequency", labels[["p_ylab"]])
        , m_main = ifelse(is.null(labels[["m_main"]]), "Distribution by marks", labels[["m_main"]])
        , m_xlab = ifelse(is.null(labels[["m_xlab"]]), "Mark", labels[["m_xlab"]])
        , m_ylab = ifelse(is.null(labels[["m_ylab"]]), "Frequency", labels[["m_ylab"]])
        , m_scale = ifelse(is.null(labels[["m_scale"]]), "Marks defined", labels[["m_scale"]])
        , NRET_expl = ifelse(is.null(labels[["NRET_expl"]]), ". \\emph{Explaination:} >>+<< -- right; >>-<< -- wrong; >>0<< -- not answered; >>*<< -- errenous answer.", labels[["NRET_expl"]])
      )
  )
  # re-add entries not covered here
  missing_labels <- names(labels_orig)[!names(labels_orig) %in% names(labels)]
  if(length(missing_labels) > 0) {
    labels[missing_labels] <- labels_orig[missing_labels]
  } else {}

  return(labels)
} ## end function default_labels()
