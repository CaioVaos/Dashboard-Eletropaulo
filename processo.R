library(tidyverse)

df_1 <- readr::read_csv2('Grupo1_CPFL-PAULISTA.csv') |> 
  select(DatCompetencia, DscClasseConsumoMercado,
         DscOpcaoEnergia, VlrMercado, DscDetalheMercado) |>
  filter(grepl("^Receita", DscDetalheMercado) |
           DscDetalheMercado %in% c("Número de consumidores","Número de Consumidores",
                                    "Energia TUSD (kWh)", 
                                    "Energia TE (kWh)", "Demanda Faturada (kW)"))|> 
  mutate(VlrMercado = as.numeric(VlrMercado)) |>
  pivot_wider(names_from = DscDetalheMercado,
              values_from = VlrMercado,
              values_fn = sum) |> 
  mutate(`Receita Total`  = rowSums(across(starts_with("Receita"), as.numeric), na.rm = TRUE),
         `Número de Consumidores` = rowSums(cbind(`Número de consumidores`, `Número de Consumidores`), na.rm = T)) |> 
  select(-c("Receita Energia (R$)","Receita Demanda (R$)",
            "Receita Ultrapassagem Demanda (R$)","Receita Bandeiras (R$)",
            "Receita energia compensada (R$)","Número de consumidores")) |> 
  mutate(Empresa = "CPFL-PAULISTA", .before = DatCompetencia)

df_2 <- readr::read_csv2('Grupo2_ELETROPAULO.csv') |> 
  select(DatCompetencia, DscClasseConsumoMercado,
         DscOpcaoEnergia, VlrMercado, DscDetalheMercado) |>
  filter(grepl("^Receita", DscDetalheMercado) |
           DscDetalheMercado %in% c("Número de consumidores","Número de Consumidores",
                                    "Energia TUSD (kWh)", 
                                    "Energia TE (kWh)", "Demanda Faturada (kW)"))|> 
  mutate(VlrMercado = as.numeric(VlrMercado)) |>
  pivot_wider(names_from = DscDetalheMercado,
              values_from = VlrMercado,
              values_fn = sum) |> 
  mutate(`Receita Total`  = rowSums(across(starts_with("Receita"), as.numeric), na.rm = TRUE),
         `Número de Consumidores` = rowSums(cbind(`Número de consumidores`, `Número de Consumidores`), na.rm = T)) |> 
  select(-c("Receita Energia (R$)","Receita Demanda (R$)",
            "Receita Ultrapassagem Demanda (R$)","Receita Bandeiras (R$)",
            "Receita energia compensada (R$)","Número de consumidores")) |>
  mutate(Empresa = "ELETROPAULO", .before = DatCompetencia)

df_3 <- readr::read_csv2('Grupo3_ENEL-RJ.csv') |> 
  select(DatCompetencia, DscClasseConsumoMercado,
         DscOpcaoEnergia, VlrMercado, DscDetalheMercado) |>
  filter(grepl("^Receita", DscDetalheMercado) |
           DscDetalheMercado %in% c("Número de consumidores","Número de Consumidores",
                                    "Energia TUSD (kWh)", 
                                    "Energia TE (kWh)", "Demanda Faturada (kW)"))|> 
  mutate(VlrMercado = as.numeric(VlrMercado)) |>
  pivot_wider(names_from = DscDetalheMercado,
              values_from = VlrMercado,
              values_fn = sum) |> 
  mutate(`Receita Total`  = rowSums(across(starts_with("Receita"), as.numeric), na.rm = TRUE),
         `Número de Consumidores` = rowSums(cbind(`Número de consumidores`, `Número de Consumidores`), na.rm = T)) |> 
  select(-c("Receita Energia (R$)","Receita Demanda (R$)",
            "Receita Ultrapassagem Demanda (R$)","Receita Bandeiras (R$)",
            "Receita energia compensada (R$)","Número de consumidores")) |>
  mutate(Empresa = "ENEL-RJ", .before = DatCompetencia)

df_4 <- readr::read_csv2('Grupo4_LIGHT.csv') |> 
  select(DatCompetencia, DscClasseConsumoMercado,
         DscOpcaoEnergia, VlrMercado, DscDetalheMercado) |>
  filter(grepl("^Receita", DscDetalheMercado) |
           DscDetalheMercado %in% c("Número de consumidores","Número de Consumidores",
                                    "Energia TUSD (kWh)", 
                                    "Energia TE (kWh)", "Demanda Faturada (kW)"))|> 
  mutate(VlrMercado = as.numeric(VlrMercado)) |>
  pivot_wider(names_from = DscDetalheMercado,
              values_from = VlrMercado,
              values_fn = sum) |> 
  mutate(`Receita Total`  = rowSums(across(starts_with("Receita"), as.numeric), na.rm = TRUE),
         `Número de Consumidores` = rowSums(cbind(`Número de consumidores`, `Número de Consumidores`), na.rm = T)) |> 
  select(-c("Receita Energia (R$)","Receita Demanda (R$)",
            "Receita Ultrapassagem Demanda (R$)","Receita Bandeiras (R$)",
            "Receita energia compensada (R$)","Número de consumidores")) |> 
  mutate(Empresa = "LIGHT", .before = DatCompetencia)

df_5 <- readr::read_csv2('Grupo5_ENERGISA-MS.csv') |> 
  select(DatCompetencia, DscClasseConsumoMercado,
         DscOpcaoEnergia, VlrMercado, DscDetalheMercado) |>
  filter(grepl("^Receita", DscDetalheMercado) |
           DscDetalheMercado %in% c("Número de consumidores","Número de Consumidores",
                                    "Energia TUSD (kWh)", 
                                    "Energia TE (kWh)", "Demanda Faturada (kW)"))|> 
  mutate(VlrMercado = as.numeric(VlrMercado)) |>
  pivot_wider(names_from = DscDetalheMercado,
              values_from = VlrMercado,
              values_fn = sum) |> 
  mutate(`Receita Total`  = rowSums(across(starts_with("Receita"), as.numeric), na.rm = TRUE),
         `Número de Consumidores` = rowSums(cbind(`Número de consumidores`, `Número de Consumidores`), na.rm = T)) |> 
  select(-c("Receita Energia (R$)","Receita Demanda (R$)",
            "Receita Ultrapassagem Demanda (R$)","Receita Bandeiras (R$)",
            "Receita energia compensada (R$)","Número de consumidores")) |>
  mutate(Empresa = "ENERGISA-MS", .before = DatCompetencia)

df_6 <- readr::read_csv2('Grupo6_CEMIG.csv') |> 
  select(DatCompetencia, DscClasseConsumoMercado,
         DscOpcaoEnergia, VlrMercado, DscDetalheMercado) |>
  filter(grepl("^Receita", DscDetalheMercado) |
           DscDetalheMercado %in% c("Número de consumidores","Número de Consumidores",
                                    "Energia TUSD (kWh)", 
                                    "Energia TE (kWh)", "Demanda Faturada (kW)"))|> 
  mutate(VlrMercado = as.numeric(VlrMercado)) |>
  pivot_wider(names_from = DscDetalheMercado,
              values_from = VlrMercado,
              values_fn = sum) |> 
  mutate(`Receita Total`  = rowSums(across(starts_with("Receita"), as.numeric), na.rm = TRUE),
         `Número de Consumidores` = rowSums(cbind(`Número de consumidores`, `Número de Consumidores`), na.rm = T)) |> 
  select(-c("Receita Energia (R$)","Receita Demanda (R$)",
            "Receita Ultrapassagem Demanda (R$)","Receita Bandeiras (R$)",
            "Receita energia compensada (R$)","Número de consumidores")) |>
  mutate(Empresa = "CEMIG", .before = DatCompetencia)

df_7 <- readr::read_csv2('Grupo7_EDP-SP.csv') |> 
  select(DatCompetencia, DscClasseConsumoMercado,
         DscOpcaoEnergia, VlrMercado, DscDetalheMercado) |>
  filter(grepl("^Receita", DscDetalheMercado) |
           DscDetalheMercado %in% c("Número de consumidores","Número de Consumidores",
                                    "Energia TUSD (kWh)", 
                                    "Energia TE (kWh)", "Demanda Faturada (kW)"))|> 
  mutate(VlrMercado = as.numeric(VlrMercado)) |>
  pivot_wider(names_from = DscDetalheMercado,
              values_from = VlrMercado,
              values_fn = sum) |> 
  mutate(`Receita Total`  = rowSums(across(starts_with("Receita"), as.numeric), na.rm = TRUE),
         `Número de Consumidores` = rowSums(cbind(`Número de consumidores`, `Número de Consumidores`), na.rm = T)) |> 
  select(-c("Receita Energia (R$)","Receita Demanda (R$)",
            "Receita Ultrapassagem Demanda (R$)","Receita Bandeiras (R$)",
            "Receita energia compensada (R$)","Número de consumidores")) |>
  mutate(Empresa = "EDP-SP", .before = DatCompetencia)

df_8 <- readr::read_csv2('Grupo8_EQUATORIAL-GO.csv') |> 
  select(DatCompetencia, DscClasseConsumoMercado,
         DscOpcaoEnergia, VlrMercado, DscDetalheMercado) |>
  filter(grepl("^Receita", DscDetalheMercado) |
           DscDetalheMercado %in% c("Número de consumidores","Número de Consumidores",
                                    "Energia TUSD (kWh)", 
                                    "Energia TE (kWh)", "Demanda Faturada (kW)"))|> 
  mutate(VlrMercado = as.numeric(VlrMercado)) |>
  pivot_wider(names_from = DscDetalheMercado,
              values_from = VlrMercado,
              values_fn = sum) |> 
  mutate(`Receita Total`  = rowSums(across(starts_with("Receita"), as.numeric), na.rm = TRUE),
         `Número de Consumidores` = rowSums(cbind(`Número de consumidores`, `Número de Consumidores`), na.rm = T)) |> 
  select(-c("Receita Energia (R$)","Receita Demanda (R$)",
            "Receita Ultrapassagem Demanda (R$)","Receita Bandeiras (R$)",
            "Receita energia compensada (R$)","Número de consumidores")) |>
  mutate(Empresa = "EQUATORIAL-GO", .before = DatCompetencia)

df_unica <- rbind(df_1, df_2, df_3, df_4, df_5, df_6, df_7, df_8)

plotly::ggplotly(
df_unica |> 
  group_by(Empresa, DatCompetencia) |> 
  summarise(variavel = sum(`Receita Total`, na.rm = T)/sum(`Número de Consumidores`, na.rm = T)) |> 
  ggplot(aes(x= DatCompetencia, y = variavel, colour = Empresa))+
  geom_line()+
  theme_minimal()
)
  

--------------------------------------------------------------------------------

arrow::write_feather(df_unica, 'Trabalho/Bases/comparativo_empresas.feather')

Sdf <- arrow::read_feather('Trabalho/Bases/comparativo_empresas.feather')
