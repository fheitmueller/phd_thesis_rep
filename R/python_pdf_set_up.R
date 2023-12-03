#' check python set-up for pdf extraction
#'
#' @import reticulate
#' @import tidyverse
#' @export
#' @return
#'

python_pdf_set_up <- function(){

  conda_installations <- conda_list(conda = "auto")

  #check if one with the name "pdfext" already exists, if not create one
  is_pdfext <- which(conda_installations$name == "pdfext")

  if(is_empty(is_pdfext)) {
    conda_create(envname = "pdfext")
    conda_installations <- conda_list(conda = "auto")
    is_pdfext <- which(conda_installations$name == "pdfext")
  }

  conda_path <- conda_installations$python[is_pdfext]
  use_condaenv(condaenv = conda_path)

  packages <- py_list_packages()

  if(!("camelot-py" %in% packages$package)){
    py_install("camelot-py[all]", pip=TRUE, conda = "auto")
    }


}
