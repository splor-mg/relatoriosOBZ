remap_unidades <- function(data, uo = NULL, acao = NULL) {
  dt <- data.table::copy(data)

  # 
  if(!is.null(uo)) {
    data.table::setnames(dt, uo, "uo_cod")
  }

  if(!is.null(acao)) {
    data.table::setnames(dt, acao, "acao_cod")
  }


  # 2023 ----------------------------------------------------------------------- 

  dt[
    uo_cod == 1511 & ano == 2023 & acao_cod %in% c(4019, 4021, 4020, 4017, 4018), uo_cod := 1551
  ]
  
  dt[
    uo_cod == 1501 & ano == 2023 & acao_cod %in% c(4480, 4481, 4482), uo_cod := 1502
  ]


  # em 2023 houve transposição das ações da pcmg para a seplag
  dt[
    (uo_cod == 1511 | 1501) & ano == 2023 & acao_cod %in% c(4100, 4105, 4124, 4134, 4135), uo_cod := 1551
  ]

  dt[
    uo_cod == 1221 & ano == 2023 & acao_cod == 4408, uo_cod := 1721
  ]

  dt[
    uo_cod == 1491 & ano == 2023 & acao_cod == 2015, uo_cod := 1721
  ]

  dt[
    uo_cod == 1501 & ano == 2023 & acao_cod == 4140, uo_cod := 1721
  ]

  dt[
    uo_cod == 1631 & ano == 2023 & acao_cod %in% c(2058, 2059), uo_cod := 1711
  ]

  dt[
    uo_cod == 1631 & ano == 2023 & acao_cod == 2060, uo_cod := 1491
  ]


  
  # 2024 -----------------------------------------------------------------------
  
  # transposição das acoes da seplag para uo's ficticias camg (1502) e cet (1503)

  # seplag - camg
  dt[
    uo_cod == 1501 & ano == 2024 & acao_cod %in% c(1085, 4465, 4466, 4467), uo_cod := 1502
  ]

  # seplag - cet
  dt[
    uo_cod == 1501 & ano == 2024 & acao_cod %in% c(4491, 4492, 4494, 4495, 4496), uo_cod := 1503
  ]


  if(!is.null(uo)) {
    data.table::setnames(dt, "uo_cod", uo)
  }

  if(!is.null(acao)) {
    data.table::setnames(dt, "acao_cod", acao)
  }

  return(dt[])
}
