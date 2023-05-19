 # Let's import global data

 library(WDI) 
  WLD_GDPpc <- WDI(
    country = "WLD",
    indicator = "NY.GDP.PCAP.CD",
    start = 2002,
    end = 2020,
    extra = FALSE,
    cache = NULL,
    latest = NULL,
    language = "en"
  )
  
  WLD_GDP <- WDI(
    country = "WLD",
    indicator = "NY.GDP.MKTP.CD",
    start = 2002,
    end = 2020,
    extra = FALSE,
    cache = NULL,
    latest = NULL,
    language = "en"
  )

   WLD_population <- WDI(
    country = "WLD",
    indicator = "SP.POP.TOTL",
    start = 2002,
    end = 2020,
    extra = FALSE,
    cache = NULL,
    latest = NULL,
    language = "en"
  )


 WLD_exp <- WDI(
    country = "WLD",
    indicator = "NE.EXP.GNFS.CD",
    start = 2002,
    end = 2020,
    extra = FALSE,
    cache = NULL,
    latest = NULL,
    language = "en"
  )
  WLD_imp <- WDI(
    country = "WLD",
    indicator = "NE.IMP.GNFS.CD",
    start = 2002,
    end = 2020,
    extra = FALSE,
    cache = NULL,
    latest = NULL,
    language = "en"
  )

 WLD_inflation <- WDI(
    country = "WLD",
    indicator = "NY.GDP.DEFL.KD.ZG",
    start = 2002,
    end = 2020,
    extra = FALSE,
    cache = NULL,
    latest = NULL,
    language = "en"
  )

WLD <- data.frame("country"=WLD_GDP$country,
                 "year"=WLD_GDP$year,
                 "iso3"=WLD_GDP$iso3c,
                 "gdp_w"=WLD_GDP$NY.GDP.MKTP.CD,
                 "gdp_pc_w"=WLD_GDPpc$NY.GDP.PCAP.CD,
                 "population_w"=WLD_population$SP.POP.TOTL,
                 "import_w"=WLD_imp$NE.IMP.GNFS.CD,
                 "export_w"=WLD_exp$NE.EXP.GNFS.CD,
                 "inflation_w"=WLD_inflation$NY.GDP.DEFL.KD.ZG)

#===============================================================================
#=============================== WLD ===================================
#===============================================================================

WLD_adj_inf <- function(x){
  adjust_inf <- ((x/WLD$inflation_w) * WLD$inflation_w[6])
  return(adjust_inf)
}
WLD <- mutate(WLD, 
                 gdp_w = WLD_adj_inf(WLD$gdp_w),
                 gdp_pc_w = WLD_adj_inf(WLD$gdp_pc_w),
                 import_w = WLD_adj_inf(WLD$import_w),
                 import_w = WLD_adj_inf(WLD$import_w)
                 )

#Create world aggregates to include in data for project
#Use the repeat loop to do so. These variables start with trying

trying_gdp_w <- rep(WLD_inf$gdp_w, times=36)
trying_gdp_pc_w <- rep(WLD_inf$gdp_pc_w, times=36)
trying_import_w <- rep(WLD_inf$import_w, times=36)
trying_export_w <- rep(WLD_inf$export_w, times=36)


#The new data formed by including the world components shall be called
# thesis_WLD

thesis_WLD <- mutate(thesisData,
              gdp_w = trying_gdp_w, 
              gdp_pc_w = trying_gdp_pc_w,
              import_w = trying_import_w,
              export_w = trying_export_w)


#Create funtions to handle the computations of trade shares
#You may want to reuse some of this functions in future

imp_exp_sum <- function(exp_X, imp_M) {
  M_X <- (exp_X + imp_M)
  return(M_X)
}

country_trade_share <- function(exp_c, imp_c, country_gdp) {
  wts_gen <- (exp_c + imp_c) / (country_gdp)
  return(wts_gen)
}

world_trade_share <- function(exp_c, imp_c, exp_w, imp_w) {
  wts_gen <- (exp_c + imp_c) / ((1/217) * (exp_w + imp_w))
  return(wts_gen)
}

composite_trade_share <- function(exp_c, imp_c, country_gdp, exp_w, imp_w) {
  cts_gen <- (217 * ((exp_c + imp_c)^2)) / (country_gdp * (exp_w + imp_w))
  return(cts_gen)
}

# Here let's add the trade shares with the mutate function from tidyverse

thesis_WLD <- mutate(thesis_WLD,
                     TS = country_trade_share(export, import, gdp_1), 
                     WTS = world_trade_share(export, import, export_w, import_w)
                     )

#compute the composite trade shares by multiply TS and WTS from the above mutation

thesis_WLD <- mutate(thesis_WLD, 
                     WTS = TS * WTS
                     )

#===========Editing Labels of the Variables==========
#============= don't run these codes=================
#====================================================
attr(thesis_WLD[["gdp_1"]], "label") <- "GDP: Inf_Adjusted"
attr(thesis_WLD[["fdi_2_gdp"]], "label") <- "FDI_GDP ratio: Not Credible"
attr(thesis_WLD[["income"]], "label") <- "WDI Income Group"
attr(thesis_WLD[["gdp_pc_1"]], "label") <- "Composite Trade Share: (TS * WTS)"
attr(thesis_WLD[["fdi_2_gdp_inf"]], "label") <- "FDI to GDP ratio: Inf_Adjusted"

attr(thesis_WLD[["LN_fdi"]], "label") <- "Log of FDI inflows: Inf_Adjusted"
attr(thesis_WLD[["gdp_pc_inf"]], "label") <- "GDP per capita: Inf_Adjusted"
attr(thesis_WLD[["export"]], "label") <- "Exports: Inf_Adjusted"
attr(thesis_WLD[["import"]], "label") <- "Imports: Inf_Adjusted"

attr(thesis_WLD[["gdp_w"]], "label") <- "World GDP: Inf_Adjusted"
attr(thesis_WLD[["gdp_pc_w"]], "label") <- "World GDP per capita: Inf_Adjusted"
attr(thesis_WLD[["export_w"]], "label") <- "World Exports: Inf_Adjusted"
attr(thesis_WLD[["import_w"]], "label") <- "World Imports: Inf_Adjusted"
attr(thesis_WLD[["gdp_pc_w"]], "label") <- "World GDP per capita: Inf_Adjusted"

attr(thesis_WLD[["LN_politica"]], "label") <- "Log of Political Stability Index"
attr(thesis_WLD[["TS"]], "label") <- "Trade Share: (X+M)/GDP"
attr(thesis_WLD[["WTS"]], "label") <- "World Trade Share: 217 Countries Used"
attr(thesis_WLD[["CTS"]], "label") <- "Composite Trade Share: (TS * WTS)"
