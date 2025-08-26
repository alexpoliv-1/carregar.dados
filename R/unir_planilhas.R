#' Unir múltiplas planilhas Excel de um diretório
#'
#' Lê todos os arquivos Excel (.xlsx, .xls) de um diretório e retorna um único
#' tibble, com opção de remover duplicados.
#'
#' @importFrom readxl read_excel
#' @importFrom dplyr bind_rows distinct mutate
#' @importFrom magrittr %>%
#' @param path Caminho para o diretório contendo as planilhas.
#' @param remove.duplicates Lógico (TRUE por padrão). Se TRUE, remove linhas duplicadas após unir.
#' @return Um tibble com os dados combinados e uma coluna `arquivo` indicando a origem.
#' @examples
#' \dontrun{
#' base <- unir.planilhas("meu_diretorio_com_planilhas", remove.duplicates = TRUE)
#' }
#' @export
unir.planilhas <- function(path, remove.duplicates = TRUE) {
  if (!requireNamespace("readxl", quietly = TRUE)) {
    stop("O pacote 'readxl' é necessário. Instale com install.packages('readxl')")
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("O pacote 'dplyr' é necessário. Instale com install.packages('dplyr')")
  }

  arquivos <- list.files(path = path, pattern = "\\.(xlsx|xls)$", full.names = TRUE, ignore.case = TRUE)

  if (length(arquivos) == 0) {
    stop("Nao foram encontrados arquivos Excel no caminho informado.")
  }

  message("Foram encontrados ", length(arquivos), " arquivos Excel:")
  message(paste0(" - ", basename(arquivos), collapse = "\n"))

  dfs <- lapply(arquivos, readxl::read_excel)
  dados <- dplyr::bind_rows(dfs, .id = "arquivo") %>%
    dplyr::mutate(arquivo = basename(arquivos[as.integer(arquivo)]))

  if (remove.duplicates) {
    dados <- dplyr::distinct(dados)
    message("Linhas duplicadas foram removidas.")
  } else {
    message("Linhas duplicadas foram mantidas.")
  }

  return(dados)
}
