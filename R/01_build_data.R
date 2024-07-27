#### Fiscal policy and growth ##################################################
#' 
#' @name 01_build_data.R
#' 
#' @description Download and assemble data on fiscal policy and growth.
#' 
#' @author Esteban Degetau
#' 
#' @created 2024-07-16
#' 
#### Build data ################################################################

rm(list = ls())
gc()

#---- Libraries ----------------------------------------------------------------

pacman::p_load(tidyverse, wbstats, here, readxl, labelled, conflicted)

conflicts_prefer(dplyr::lag)
conflicts_prefer(dplyr::filter)

#---- Load data ----------------------------------------------------------------

all_indicators <- wb_indicators()

countries <- wb_countries()


all_indicators |>
  filter(
    str_detect(indicator, "lending|Lending"),
    str_detect(indicator, "GDP|LCU")
    ) |>
  view()



indicator_ids <- c(
  "NY.GDP.PCAP.KD", "GC.NLD.TOTL.GD.ZS",
  "SL.TLF.TOTL.IN", "NY.GDP.PCAP.KD.ZG", "GB.BAL.XINT.CN", "GB.TAX.GSRV.CN",
  "GC.TAX.YPKG.CN", "GC.REV.SOCL.CN", "GC.TAX.OTHR.CN", "GB.REV.IGRT.CN", "GB.XPC.TOTL.CN",
  "NE.CON.GOVT.CN", "GB.XPD.DEFN.CN", "SE.XPD.TOTL.GD.ZS", "SH.XPD.GHED.GD.ZS", "IE.ICT.TOTL.GD.ZS",
  "GB.XPD.RSDV.GD.ZS", "CC.SP.EXP.ZS", "GC.TAX.INTT.RV.ZS", "GC.TAX.GSRV.CN",
  "GC.REV.XGRT.GD.ZS", "GB.REV.IGRT.CN", "GC.XPN.TOTL.GD.ZS", "GC.REV.TOTL.CN",
  "GB.BAL.OVRX.CN", "GC.XPN.COMP.CN", "GC.XPN.COMP.CN", "GB.BAL.CIGR.CN", "GC.XPN.TRFT.CN",
  "GB.BAL.XINT.CN", "GC.REV.XGRT.GD.ZS", "GC.NFN.TOTL.GD.ZS",
  "GC.XPN.TOTL.GD.ZS", "NY.GDP.MKTP.CN", "GB.RVC.TOTL.CN", "NE.CON.GOVT.ZS", "GB.REV.XAGT.CN.ZS"
)

my_indicators <- all_indicators |>
    filter(indicator_id %in% indicator_ids)
    
my_indicators |>
  view()

data <- wb_data(my_indicators$indicator_id)


data |> generate_dictionary() |> view()

all_indicators |>
  filter(str_detect(indicator_id, "GC.|GB.")) |>
  filter(str_detect(indicator, "LCU|GDP")) |>
  view()

#---- Compute theoretical variables --------------------------------------------

theoretical <- data |>
  # mutate(
  #   across(
  #     where(is.numeric) & !matches("year"),
  #     ~ case_when(
  #       is.na(.x) ~ 0,
  #       T ~ .x)
  #   )
  # ) |>
  mutate(

    gdp_pc_usd = NY.GDP.PCAP.KD,

    net_lending = GC.NLD.TOTL.GD.ZS,
    
    total_revenue = GC.REV.XGRT.GD.ZS,
    
    non_distortionary_taxation = 100 * GC.TAX.GSRV.CN / NY.GDP.MKTP.CN,
    
    # Distortionary taxation
    tax_income_profit = 100 * GC.TAX.YPKG.CN / NY.GDP.MKTP.CN,
    social_security_contributions = 100 * GC.REV.SOCL.CN / NY.GDP.MKTP.CN,
    
    
    distortionary_taxation = tax_income_profit + social_security_contributions,
    
    
    # Other revenues
    other_revenues = total_revenue - distortionary_taxation - non_distortionary_taxation,
    
    # Expenditure
    total_expenditure = GC.XPN.TOTL.GD.ZS,
    
    # general_expenditure = 100 * GC.XPN.COMP.CN / NY.GDP.MKTP.CN,
    
    # defense_expenditure = GB.XPD.DEFN.CN / NY.GDP.MKTP.CN,
    
    education_expenditure = SE.XPD.TOTL.GD.ZS,
    
    health_expenditure = SH.XPD.GHED.GD.ZS,
    
    # transport_expenditure = IE.ICT.TOTL.GD.ZS,
    
    # rnd_expenditure = GB.XPD.RSDV.GD.ZS,
    
    productive_expenditure =  education_expenditure + health_expenditure, 
    #+ transport_expenditure + rnd_expenditure + defense_expenditure,

    subsidies_transfers = 100 * GC.XPN.TRFT.CN / NY.GDP.MKTP.CN,
    
    unproductive_expenditure = subsidies_transfers,

    other_expenditure = total_expenditure - productive_expenditure - unproductive_expenditure,
    
    
    gdp_pc_growth = NY.GDP.PCAP.KD.ZG,
    
    budget_surplus = total_revenue - total_expenditure,

    investment = GC.NFN.TOTL.GD.ZS
    
    
  ) |>
  mutate(
    across(c(other_expenditure, other_revenues), ~ ifelse(.x < 0, 0, .x))
  ) |>
  arrange(country, date) |>
  mutate(
    labor_growth = 100 * ((SL.TLF.TOTL.IN / lag(SL.TLF.TOTL.IN)) - 1),
    .by = country
  )

#---- Tidy ---------------------------------------------------------------------

labs <- generate_dictionary(theoretical) |>
  drop_na(label) |>
  mutate(
    label = str_remove(label, "\\(current LCU\\)|\\(% of GDP\\)|\\(%of GDP\\)") |>
      str_squish()
  )

var_labels <- setNames(as.list(labs$label), labs$variable)

GKS <- theoretical |>
  select(
    iso2c,
    iso3c,
    country,
    date,
    gdp_pc_growth,
    gdp_pc_usd,
    labor_growth,
    budget_surplus,
    non_distortionary_taxation,
    tax_income_profit,
    social_security_contributions,
    distortionary_taxation,
    total_revenue,
    other_revenues,
    total_expenditure,
    education_expenditure,
    health_expenditure,
    productive_expenditure,
    other_expenditure,
    unproductive_expenditure,
    investment,
    net_lending
  ) |>
  left_join(countries) |>
  set_variable_labels(
    .labels = var_labels, .strict = FALSE,
    budget_surplus = "Budget surplus",
    distortionary_taxation = "Distortionary taxation",
    non_distortionary_taxation = "Non-distortionary taxation",
    other_revenues = "Other revenues",
    productive_expenditure = "Productive expenditure",
    unproductive_expenditure = "Unproductive expenditure",
    other_expenditure = "Other expenditure",
    labor_growth = "Labor growth (annual %)",
    gdp_pc_growth = "GDP per capita growth (annual %)",
    initial_gdp = "Initial GDP per capita (2015 USD)",
    date = "Year",
    country = "Country"
  )
#---- Save ---------------------------------------------------------------------

save(GKS, file = here("data/GKS.RData"))

