remap_unidades <- function(data, uo = NULL, acao = NULL) {
  dt <- data.table::copy(data)

  if(!is.null(uo)) {
    data.table::setnames(dt, uo, "UO_COD")
  }

  if(!is.null(acao)) {
    data.table::setnames(dt, acao, "ACAO_COD")
  }

  dt[
    UO_COD == 1501 & ACAO_COD %in% c(4480, 4481, 4482), UO_COD := 1502
  ]


  # em 2023 houve transposição das ações da PCMG para a SEPLAG
  dt[
    (UO_COD == 1511 | 1501) & ACAO_COD %in% c(4100, 4105, 4124, 4134, 4135), UO_COD := 1551
  ]



  if(!is.null(uo)) {
    data.table::setnames(dt, "UO_COD", uo)
  }

  if(!is.null(acao)) {
    data.table::setnames(dt, "ACAO_COD", acao)
  }

  return(dt[])
}
