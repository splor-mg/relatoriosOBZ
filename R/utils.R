remap_unidades <- function(data, uo = NULL, acao = NULL, ano = NULL) {
  dt <- data.table::copy(data)

  if(!is.null(uo)) {
    data.table::setnames(dt, uo, "UO_COD")
  }

  if(!is.null(acao)) {
    data.table::setnames(dt, acao, "ACAO_COD")
  }

  if(!is.null(ano)) {
    data.table::setnames(dt, ano, "ANO")
  }

  dt[
    UO_COD == 1511 & ACAO_COD %in% c(4019, 4021, 4020, 4017, 4018), UO_COD := 1551
  ]

  dt[
    ANO == 2024 & UO_COD == 1501 & ACAO_COD %in% c(4491, 4492, 4494, 4495, 4496), UO_COD := 1551
  ]

  dt[
    UO_COD == 1501 & ACAO_COD %in% c(4480, 4481, 4482), UO_COD := 1502
  ]

  dt[
    ANO == 2024 & UO_COD == 1501 & ACAO_COD %in% c(1085, 4465, 4466, 4467), UO_COD := 1502
  ]

  # em 2023 houve transposição das ações da PCMG para a SEPLAG
  dt[
    (UO_COD == 1511 | 1501) & ANO == 2023 & ACAO_COD %in% c(4100, 4105, 4124, 4134, 4135), UO_COD := 1551
  ]

  dt[
    UO_COD == 1221 & ACAO_COD == 4408, UO_COD := 1721
  ]

  dt[
    UO_COD == 1491 & ACAO_COD == 2015, UO_COD := 1721
  ]

  dt[
    UO_COD == 1501 & ACAO_COD == 4140, UO_COD := 1721
  ]

  dt[
    UO_COD == 1631 & ACAO_COD %in% c(2058, 2059), UO_COD := 1711
  ]

  dt[
    UO_COD == 1631 & ACAO_COD == 2060, UO_COD := 1491
  ]

  if(!is.null(uo)) {
    data.table::setnames(dt, "UO_COD", uo)
  }

  if(!is.null(acao)) {
    data.table::setnames(dt, "ACAO_COD", acao)
  }

  if(!is.null(ano)) {
    data.table::setnames(dt, "ANO", ano)
  }

  return(dt[])
}
