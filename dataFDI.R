#
# VIEW MISSING VALUES IN THE DATAFRAME
#
# exr_oer_na <- DF[rowSums(is.na(DF)) > 0,]

#Create a function called country to extract each country from the panel
# country <- function(x) {
#   cou <- filter(myData, iso3 == x)
#   return(cou)
# }

# myData <- mutate(myData, 
#                  inflation = myData$inflation + 100,
#                  political = nega_tive(myData$political))

#===============================================================================
#=============================== AGO ===================================
#===============================================================================
AGO <- country("AGO")

AGO_adj_inf <- function(x){
  adjust_inf <- ((x/AGO$inflation) * AGO$inflation[6])
  return(adjust_inf)
}
AGO <- mutate(AGO, 
                 fdi_1 = AGO_adj_inf(AGO$fdi_1),
                 fdi_2_gdp = AGO_adj_inf(AGO$fdi_2_gdp),
                 gdp_1 = AGO_adj_inf(AGO$gdp_1),
                 gdp_pc_1 = AGO_adj_inf(AGO$gdp_pc_1),
                 export = AGO_adj_inf(AGO$export),
                 import = AGO_adj_inf(AGO$import),
                 )


#===============================================================================
#=============================== BDI ===================================
#===============================================================================
BDI <- country("BDI")

BDI_adj_inf <- function(x){
  adjust_inf <- ((x/BDI$inflation) * BDI$inflation[6])
  return(adjust_inf)
}
BDI <- mutate(BDI, 
                 fdi_1 = BDI_adj_inf(BDI$fdi_1),
                 fdi_2_gdp = BDI_adj_inf(BDI$fdi_2_gdp),
                 gdp_1 = BDI_adj_inf(BDI$gdp_1),
                 gdp_pc_1 = BDI_adj_inf(BDI$gdp_pc_1),
                 export = BDI_adj_inf(BDI$export),
                 import = BDI_adj_inf(BDI$import),
                 )

#===============================================================================
#=============================== BEN ===================================
#===============================================================================
BEN <- country("BEN")

BEN_adj_inf <- function(x){
  adjust_inf <- ((x/BEN$inflation) * BEN$inflation[6])
  return(adjust_inf)
}
BEN <- mutate(BEN, 
                 fdi_1 = BEN_adj_inf(BEN$fdi_1),
                 fdi_2_gdp = BEN_adj_inf(BEN$fdi_2_gdp),
                 gdp_1 = BEN_adj_inf(BEN$gdp_1),
                 gdp_pc_1 = BEN_adj_inf(BEN$gdp_pc_1),
                 export = BEN_adj_inf(BEN$export),
                 import = BEN_adj_inf(BEN$import),
                 )

#===============================================================================
#=============================== BWA ===================================
#===============================================================================
BWA <- country("BWA")

BWA_adj_inf <- function(x){
  adjust_inf <- ((x/BWA$inflation) * BWA$inflation[6])
  return(adjust_inf)
}
BWA <- mutate(BWA, 
                 fdi_1 = BWA_adj_inf(BWA$fdi_1),
                 fdi_2_gdp = BWA_adj_inf(BWA$fdi_2_gdp),
                 gdp_1 = BWA_adj_inf(BWA$gdp_1),
                 gdp_pc_1 = BWA_adj_inf(BWA$gdp_pc_1),
                 export = BWA_adj_inf(BWA$export),
                 import = BWA_adj_inf(BWA$import),
                 )

#===============================================================================
#=============================== CAF ===================================
#===============================================================================
CAF <- country("CAF")

CAF_adj_inf <- function(x){
  adjust_inf <- ((x/CAF$inflation) * CAF$inflation[6])
  return(adjust_inf)
}
CAF <- mutate(CAF, 
                 fdi_1 = CAF_adj_inf(CAF$fdi_1),
                 fdi_2_gdp = CAF_adj_inf(CAF$fdi_2_gdp),
                 gdp_1 = CAF_adj_inf(CAF$gdp_1),
                 gdp_pc_1 = CAF_adj_inf(CAF$gdp_pc_1),
                 export = CAF_adj_inf(CAF$export),
                 import = CAF_adj_inf(CAF$import),
                 )

#===============================================================================
#=============================== CIV ===================================
#===============================================================================
CIV <- country("CIV")

CIV_adj_inf <- function(x){
  adjust_inf <- ((x/CIV$inflation) * CIV$inflation[6])
  return(adjust_inf)
}
CIV <- mutate(CIV, 
                 fdi_1 = CIV_adj_inf(CIV$fdi_1),
                 fdi_2_gdp = CIV_adj_inf(CIV$fdi_2_gdp),
                 gdp_1 = CIV_adj_inf(CIV$gdp_1),
                 gdp_pc_1 = CIV_adj_inf(CIV$gdp_pc_1),
                 export = CIV_adj_inf(CIV$export),
                 import = CIV_adj_inf(CIV$import),
                 )                

#===============================================================================
#=============================== CMR ===================================
#===============================================================================
CMR <- country("CMR")

CMR_adj_inf <- function(x){
  adjust_inf <- ((x/CMR$inflation) * CMR$inflation[6])
  return(adjust_inf)
}
CMR <- mutate(CMR, 
                 fdi_1 = CMR_adj_inf(CMR$fdi_1),
                 fdi_2_gdp = CMR_adj_inf(CMR$fdi_2_gdp),
                 gdp_1 = CMR_adj_inf(CMR$gdp_1),
                 gdp_pc_1 = CMR_adj_inf(CMR$gdp_pc_1),
                 export = CMR_adj_inf(CMR$export),
                 import = CMR_adj_inf(CMR$import),
                 )

#===============================================================================
#=============================== COG ===================================
#===============================================================================
COG <- country("COG")

COG_adj_inf <- function(x){
  adjust_inf <- ((x/COG$inflation) * COG$inflation[6])
  return(adjust_inf)
}
COG <- mutate(COG, 
                 fdi_1 = COG_adj_inf(COG$fdi_1),
                 fdi_2_gdp = COG_adj_inf(COG$fdi_2_gdp),
                 gdp_1 = COG_adj_inf(COG$gdp_1),
                 gdp_pc_1 = COG_adj_inf(COG$gdp_pc_1),
                 export = COG_adj_inf(COG$export),
                 import = COG_adj_inf(COG$import),
                 )

#===============================================================================
#=============================== CPV ===================================
#===============================================================================
CPV <- country("CPV")

CPV_adj_inf <- function(x){
  adjust_inf <- ((x/CPV$inflation) * CPV$inflation[6])
  return(adjust_inf)
}
CPV <- mutate(CPV, 
                 fdi_1 = CPV_adj_inf(CPV$fdi_1),
                 fdi_2_gdp = CPV_adj_inf(CPV$fdi_2_gdp),
                 gdp_1 = CPV_adj_inf(CPV$gdp_1),
                 gdp_pc_1 = CPV_adj_inf(CPV$gdp_pc_1),
                 export = CPV_adj_inf(CPV$export),
                 import = CPV_adj_inf(CPV$import),
                 )

#===============================================================================
#=============================== GAB ===================================
#===============================================================================
GAB <- country("GAB")

GAB_adj_inf <- function(x){
  adjust_inf <- ((x/GAB$inflation) * GAB$inflation[6])
  return(adjust_inf)
}
GAB <- mutate(GAB, 
                 fdi_1 = GAB_adj_inf(GAB$fdi_1),
                 fdi_2_gdp = GAB_adj_inf(GAB$fdi_2_gdp),
                 gdp_1 = GAB_adj_inf(GAB$gdp_1),
                 gdp_pc_1 = GAB_adj_inf(GAB$gdp_pc_1),
                 export = GAB_adj_inf(GAB$export),
                 import = GAB_adj_inf(GAB$import),
                 )

#===============================================================================
#=============================== GHA ===================================
#===============================================================================
GHA <- country("GHA")

GHA_adj_inf <- function(x){
  adjust_inf <- ((x/GHA$inflation) * GHA$inflation[6])
  return(adjust_inf)
}
GHA <- mutate(GHA, 
                 fdi_1 = GHA_adj_inf(GHA$fdi_1),
                 fdi_2_gdp = GHA_adj_inf(GHA$fdi_2_gdp),
                 gdp_1 = GHA_adj_inf(GHA$gdp_1),
                 gdp_pc_1 = GHA_adj_inf(GHA$gdp_pc_1),
                 export = GHA_adj_inf(GHA$export),
                 import = GHA_adj_inf(GHA$import),
                 )

#===============================================================================
#=============================== GIN ===================================
#===============================================================================
GIN <- country("GIN")

GIN_adj_inf <- function(x){
  adjust_inf <- ((x/GIN$inflation) * GIN$inflation[6])
  return(adjust_inf)
}
GIN <- mutate(GIN, 
                 fdi_1 = GIN_adj_inf(GIN$fdi_1),
                 fdi_2_gdp = GIN_adj_inf(GIN$fdi_2_gdp),
                 gdp_1 = GIN_adj_inf(GIN$gdp_1),
                 gdp_pc_1 = GIN_adj_inf(GIN$gdp_pc_1),
                 export = GIN_adj_inf(GIN$export),
                 import = GIN_adj_inf(GIN$import),
                 )

#===============================================================================
#=============================== GMB ===================================
#===============================================================================
GMB <- country("GMB")

GMB_adj_inf <- function(x){
  adjust_inf <- ((x/GMB$inflation) * GMB$inflation[6])
  return(adjust_inf)
}
GMB <- mutate(GMB, 
                 fdi_1 = GMB_adj_inf(GMB$fdi_1),
                 fdi_2_gdp = GMB_adj_inf(GMB$fdi_2_gdp),
                 gdp_1 = GMB_adj_inf(GMB$gdp_1),
                 gdp_pc_1 = GMB_adj_inf(GMB$gdp_pc_1),
                 export = GMB_adj_inf(GMB$export),
                 import = GMB_adj_inf(GMB$import),
                 )


#===============================================================================
#=============================== GNB ===================================
#===============================================================================
GNB <- country("GNB")

GNB_adj_inf <- function(x){
  adjust_inf <- ((x/GNB$inflation) * GNB$inflation[6])
  return(adjust_inf)
}
GNB <- mutate(GNB, 
                 fdi_1 = GNB_adj_inf(GNB$fdi_1),
                 fdi_2_gdp = GNB_adj_inf(GNB$fdi_2_gdp),
                 gdp_1 = GNB_adj_inf(GNB$gdp_1),
                 gdp_pc_1 = GNB_adj_inf(GNB$gdp_pc_1),
                 export = GNB_adj_inf(GNB$export),
                 import = GNB_adj_inf(GNB$import),
                 )

#===============================================================================
#=============================== KEN ===================================
#===============================================================================
KEN <- country("KEN")

KEN_adj_inf <- function(x){
  adjust_inf <- ((x/KEN$inflation) * KEN$inflation[6])
  return(adjust_inf)
}
KEN <- mutate(KEN, 
                 fdi_1 = KEN_adj_inf(KEN$fdi_1),
                 fdi_2_gdp = KEN_adj_inf(KEN$fdi_2_gdp),
                 gdp_1 = KEN_adj_inf(KEN$gdp_1),
                 gdp_pc_1 = KEN_adj_inf(KEN$gdp_pc_1),
                 export = KEN_adj_inf(KEN$export),
                 import = KEN_adj_inf(KEN$import),
                 )

#===============================================================================
#=============================== MDG ===================================
#===============================================================================
MDG <- country("MDG")

MDG_adj_inf <- function(x){
  adjust_inf <- ((x/MDG$inflation) * MDG$inflation[6])
  return(adjust_inf)
}
MDG <- mutate(MDG, 
                 fdi_1 = MDG_adj_inf(MDG$fdi_1),
                 fdi_2_gdp = MDG_adj_inf(MDG$fdi_2_gdp),
                 gdp_1 = MDG_adj_inf(MDG$gdp_1),
                 gdp_pc_1 = MDG_adj_inf(MDG$gdp_pc_1),
                 export = MDG_adj_inf(MDG$export),
                 import = MDG_adj_inf(MDG$import),
                 )

#===============================================================================
#=============================== MLI ===================================
#===============================================================================
MLI <- country("MLI")

MLI_adj_inf <- function(x){
  adjust_inf <- ((x/MLI$inflation) * MLI$inflation[6])
  return(adjust_inf)
}
MLI <- mutate(MLI, 
                 fdi_1 = MLI_adj_inf(MLI$fdi_1),
                 fdi_2_gdp = MLI_adj_inf(MLI$fdi_2_gdp),
                 gdp_1 = MLI_adj_inf(MLI$gdp_1),
                 gdp_pc_1 = MLI_adj_inf(MLI$gdp_pc_1),
                 export = MLI_adj_inf(MLI$export),
                 import = MLI_adj_inf(MLI$import),
                 )

#===============================================================================
#=============================== MOZ ===================================
#===============================================================================
MOZ <- country("MOZ")

MOZ_adj_inf <- function(x){
  adjust_inf <- ((x/MOZ$inflation) * MOZ$inflation[6])
  return(adjust_inf)
}
MOZ <- mutate(MOZ, 
                 fdi_1 = MOZ_adj_inf(MOZ$fdi_1),
                 fdi_2_gdp = MOZ_adj_inf(MOZ$fdi_2_gdp),
                 gdp_1 = MOZ_adj_inf(MOZ$gdp_1),
                 gdp_pc_1 = MOZ_adj_inf(MOZ$gdp_pc_1),
                 export = MOZ_adj_inf(MOZ$export),
                 import = MOZ_adj_inf(MOZ$import),
                 )

#===============================================================================
#=============================== MRT ===================================
#===============================================================================
MRT <- country("MRT")

MRT_adj_inf <- function(x){
  adjust_inf <- ((x/MRT$inflation) * MRT$inflation[6])
  return(adjust_inf)
}
MRT <- mutate(MRT, 
                 fdi_1 = MRT_adj_inf(MRT$fdi_1),
                 fdi_2_gdp = MRT_adj_inf(MRT$fdi_2_gdp),
                 gdp_1 = MRT_adj_inf(MRT$gdp_1),
                 gdp_pc_1 = MRT_adj_inf(MRT$gdp_pc_1),
                 export = MRT_adj_inf(MRT$export),
                 import = MRT_adj_inf(MRT$import),
                 )

#===============================================================================
#=============================== MUS ===================================
#===============================================================================
MUS <- country("MUS")

MUS_adj_inf <- function(x){
  adjust_inf <- ((x/MUS$inflation) * MUS$inflation[6])
  return(adjust_inf)
}
MUS <- mutate(MUS, 
                 fdi_1 = MUS_adj_inf(MUS$fdi_1),
                 fdi_2_gdp = MUS_adj_inf(MUS$fdi_2_gdp),
                 gdp_1 = MUS_adj_inf(MUS$gdp_1),
                 gdp_pc_1 = MUS_adj_inf(MUS$gdp_pc_1),
                 export = MUS_adj_inf(MUS$export),
                 import = MUS_adj_inf(MUS$import),
                 )

#===============================================================================
#=============================== NAM ===================================
#===============================================================================
NAM <- country("NAM")

NAM_adj_inf <- function(x){
  adjust_inf <- ((x/NAM$inflation) * NAM$inflation[6])
  return(adjust_inf)
}
NAM <- mutate(NAM, 
                 fdi_1 = NAM_adj_inf(NAM$fdi_1),
                 fdi_2_gdp = NAM_adj_inf(NAM$fdi_2_gdp),
                 gdp_1 = NAM_adj_inf(NAM$gdp_1),
                 gdp_pc_1 = NAM_adj_inf(NAM$gdp_pc_1),
                 export = NAM_adj_inf(NAM$export),
                 import = NAM_adj_inf(NAM$import),
                 )

#===============================================================================
#=============================== NER ===================================
#===============================================================================
NER <- country("NER")

NER_adj_inf <- function(x){
  adjust_inf <- ((x/NER$inflation) * NER$inflation[6])
  return(adjust_inf)
}
NER <- mutate(NER, 
                 fdi_1 = NER_adj_inf(NER$fdi_1),
                 fdi_2_gdp = NER_adj_inf(NER$fdi_2_gdp),
                 gdp_1 = NER_adj_inf(NER$gdp_1),
                 gdp_pc_1 = NER_adj_inf(NER$gdp_pc_1),
                 export = NER_adj_inf(NER$export),
                 import = NER_adj_inf(NER$import),
                 )

#===============================================================================
#=============================== NGA ===================================
#===============================================================================
NGA <- country("NGA")

NGA_adj_inf <- function(x){
  adjust_inf <- ((x/NGA$inflation) * NGA$inflation[6])
  return(adjust_inf)
}
NGA <- mutate(NGA, 
                 fdi_1 = NGA_adj_inf(NGA$fdi_1),
                 fdi_2_gdp = NGA_adj_inf(NGA$fdi_2_gdp),
                 gdp_1 = NGA_adj_inf(NGA$gdp_1),
                 gdp_pc_1 = NGA_adj_inf(NGA$gdp_pc_1),
                 export = NGA_adj_inf(NGA$export),
                 import = NGA_adj_inf(NGA$import),
                 )

#===============================================================================
#=============================== RWA ===================================
#===============================================================================
RWA <- country("RWA")

RWA_adj_inf <- function(x){
  adjust_inf <- ((x/RWA$inflation) * RWA$inflation[6])
  return(adjust_inf)
}
RWA <- mutate(RWA, 
                 fdi_1 = RWA_adj_inf(RWA$fdi_1),
                 fdi_2_gdp = RWA_adj_inf(RWA$fdi_2_gdp),
                 gdp_1 = RWA_adj_inf(RWA$gdp_1),
                 gdp_pc_1 = RWA_adj_inf(RWA$gdp_pc_1),
                 export = RWA_adj_inf(RWA$export),
                 import = RWA_adj_inf(RWA$import),
                 )

#===============================================================================
#=============================== SDN ===================================
#===============================================================================
SDN <- country("SDN")

SDN_adj_inf <- function(x){
  adjust_inf <- ((x/SDN$inflation) * SDN$inflation[6])
  return(adjust_inf)
}
SDN <- mutate(SDN, 
                 fdi_1 = SDN_adj_inf(SDN$fdi_1),
                 fdi_2_gdp = SDN_adj_inf(SDN$fdi_2_gdp),
                 gdp_1 = SDN_adj_inf(SDN$gdp_1),
                 gdp_pc_1 = SDN_adj_inf(SDN$gdp_pc_1),
                 export = SDN_adj_inf(SDN$export),
                 import = SDN_adj_inf(SDN$import),
                 )

#===============================================================================
#=============================== SEN ===================================
#===============================================================================
SEN <- country("SEN")

SEN_adj_inf <- function(x){
  adjust_inf <- ((x/SEN$inflation) * SEN$inflation[6])
  return(adjust_inf)
}
SEN <- mutate(SEN, 
                 fdi_1 = SEN_adj_inf(SEN$fdi_1),
                 fdi_2_gdp = SEN_adj_inf(SEN$fdi_2_gdp),
                 gdp_1 = SEN_adj_inf(SEN$gdp_1),
                 gdp_pc_1 = SEN_adj_inf(SEN$gdp_pc_1),
                 export = SEN_adj_inf(SEN$export),
                 import = SEN_adj_inf(SEN$import),
                 )

#===============================================================================
#=============================== SLE ===================================
#===============================================================================
SLE <- country("SLE")

SLE_adj_inf <- function(x){
  adjust_inf <- ((x/SLE$inflation) * SLE$inflation[6])
  return(adjust_inf)
}
SLE <- mutate(SLE, 
                 fdi_1 = SLE_adj_inf(SLE$fdi_1),
                 fdi_2_gdp = SLE_adj_inf(SLE$fdi_2_gdp),
                 gdp_1 = SLE_adj_inf(SLE$gdp_1),
                 gdp_pc_1 = SLE_adj_inf(SLE$gdp_pc_1),
                 export = SLE_adj_inf(SLE$export),
                 import = SLE_adj_inf(SLE$import),
                 )

#===============================================================================
#=============================== SWZ ===================================
#===============================================================================
SWZ <- country("SWZ")

SWZ_adj_inf <- function(x){
  adjust_inf <- ((x/SWZ$inflation) * SWZ$inflation[6])
  return(adjust_inf)
}
SWZ <- mutate(SWZ, 
                 fdi_1 = SWZ_adj_inf(SWZ$fdi_1),
                 fdi_2_gdp = SWZ_adj_inf(SWZ$fdi_2_gdp),
                 gdp_1 = SWZ_adj_inf(SWZ$gdp_1),
                 gdp_pc_1 = SWZ_adj_inf(SWZ$gdp_pc_1),
                 export = SWZ_adj_inf(SWZ$export),
                 import = SWZ_adj_inf(SWZ$import),
                 )

#===============================================================================
#=============================== SYC ===================================
#===============================================================================
SYC <- country("SYC")

SYC_adj_inf <- function(x){
  adjust_inf <- ((x/SYC$inflation) * SYC$inflation[6])
  return(adjust_inf)
}
SYC <- mutate(SYC, 
                 fdi_1 = SYC_adj_inf(SYC$fdi_1),
                 fdi_2_gdp = SYC_adj_inf(SYC$fdi_2_gdp),
                 gdp_1 = SYC_adj_inf(SYC$gdp_1),
                 gdp_pc_1 = SYC_adj_inf(SYC$gdp_pc_1),
                 export = SYC_adj_inf(SYC$export),
                 import = SYC_adj_inf(SYC$import),
                 )

#===============================================================================
#=============================== TCD ===================================
#===============================================================================
TCD <- country("TCD")

TCD_adj_inf <- function(x){
  adjust_inf <- ((x/TCD$inflation) * TCD$inflation[6])
  return(adjust_inf)
}
TCD <- mutate(TCD, 
                 fdi_1 = TCD_adj_inf(TCD$fdi_1),
                 fdi_2_gdp = TCD_adj_inf(TCD$fdi_2_gdp),
                 gdp_1 = TCD_adj_inf(TCD$gdp_1),
                 gdp_pc_1 = TCD_adj_inf(TCD$gdp_pc_1),
                 export = TCD_adj_inf(TCD$export),
                 import = TCD_adj_inf(TCD$import),
                 )

#===============================================================================
#=============================== TGO ===================================
#===============================================================================
TGO <- country("TGO")

TGO_adj_inf <- function(x){
  adjust_inf <- ((x/TGO$inflation) * TGO$inflation[6])
  return(adjust_inf)
}
TGO <- mutate(TGO, 
                 fdi_1 = TGO_adj_inf(TGO$fdi_1),
                 fdi_2_gdp = TGO_adj_inf(TGO$fdi_2_gdp),
                 gdp_1 = TGO_adj_inf(TGO$gdp_1),
                 gdp_pc_1 = TGO_adj_inf(TGO$gdp_pc_1),
                 export = TGO_adj_inf(TGO$export),
                 import = TGO_adj_inf(TGO$import),
                 )

#===============================================================================
#=============================== TZA ===================================
#===============================================================================
TZA <- country("TZA")

TZA_adj_inf <- function(x){
  adjust_inf <- ((x/TZA$inflation) * TZA$inflation[6])
  return(adjust_inf)
}
TZA <- mutate(TZA, 
                 fdi_1 = TZA_adj_inf(TZA$fdi_1),
                 fdi_2_gdp = TZA_adj_inf(TZA$fdi_2_gdp),
                 gdp_1 = TZA_adj_inf(TZA$gdp_1),
                 gdp_pc_1 = TZA_adj_inf(TZA$gdp_pc_1),
                 export = TZA_adj_inf(TZA$export),
                 import = TZA_adj_inf(TZA$import),
                 )


#===============================================================================
#=============================== UGA ===================================
#===============================================================================
UGA <- country("UGA")

UGA_adj_inf <- function(x){
  adjust_inf <- ((x/UGA$inflation) * UGA$inflation[6])
  return(adjust_inf)
}
UGA <- mutate(UGA, 
                 fdi_1 = UGA_adj_inf(UGA$fdi_1),
                 fdi_2_gdp = UGA_adj_inf(UGA$fdi_2_gdp),
                 gdp_1 = UGA_adj_inf(UGA$gdp_1),
                 gdp_pc_1 = UGA_adj_inf(UGA$gdp_pc_1),
                 export = UGA_adj_inf(UGA$export),
                 import = UGA_adj_inf(UGA$import),
                 )

#===============================================================================
#=============================== ZAF ===================================
#===============================================================================
ZAF <- country("ZAF")

ZAF_adj_inf <- function(x){
  adjust_inf <- ((x/ZAF$inflation) * ZAF$inflation[6])
  return(adjust_inf)
}
ZAF <- mutate(ZAF, 
                 fdi_1 = ZAF_adj_inf(ZAF$fdi_1),
                 fdi_2_gdp = ZAF_adj_inf(ZAF$fdi_2_gdp),
                 gdp_1 = ZAF_adj_inf(ZAF$gdp_1),
                 gdp_pc_1 = ZAF_adj_inf(ZAF$gdp_pc_1),
                 export = ZAF_adj_inf(ZAF$export),
                 import = ZAF_adj_inf(ZAF$import),
                 )
#===============================================================================
#=============================== ZMB ===================================
#===============================================================================
ZMB <- country("ZMB")

ZMB_adj_inf <- function(x){
  adjust_inf <- ((x/ZMB$inflation) * ZMB$inflation[6])
  return(adjust_inf)
}
ZMB <- mutate(ZMB, 
                 fdi_1 = ZMB_adj_inf(ZMB$fdi_1),
                 fdi_2_gdp = ZMB_adj_inf(ZMB$fdi_2_gdp),
                 gdp_1 = ZMB_adj_inf(ZMB$gdp_1),
                 gdp_pc_1 = ZMB_adj_inf(ZMB$gdp_pc_1),
                 export = ZMB_adj_inf(ZMB$export),
                 import = ZMB_adj_inf(ZMB$import),
                 )
#===============================================================================
#=============================== ZWE ===================================
#===============================================================================
ZWE <- country("ZWE")

ZWE_adj_inf <- function(x){
  adjust_inf <- ((x/ZWE$inflation) * ZWE$inflation[6])
  return(adjust_inf)
}
ZWE <- mutate(ZWE, 
                 fdi_1 = ZWE_adj_inf(ZWE$fdi_1),
                 fdi_2_gdp = ZWE_adj_inf(ZWE$fdi_2_gdp),
                 gdp_1 = ZWE_adj_inf(ZWE$gdp_1),
                 gdp_pc_1 = ZWE_adj_inf(ZWE$gdp_pc_1),
                 export = ZWE_adj_inf(ZWE$export),
                 import = ZWE_adj_inf(ZWE$import),
                 )

#===============================================================================
#============================= MY INF ADJ DATA==============================
#===============================================================================

myData_inf <- rbind(AGO, BDI, BEN, BWA, CAF,
                    CIV, CMR, COG, CPV,GAB, GHA,
                    GIN, GMB, GNB, KEN, MDG, MLI,
                    
                    MOZ, MRT, MUS, NAM,
                    NER, NGA, RWA, SDN, SEN, SLE,
                    SWZ, SYC, TCD,
                    TGO, TZA, UGA, ZAF, ZMB, ZWE)

myData_inf <- mutate(myData_inf, 
                     gdp_pc_inf = myData_inf$gdp_1/myData_inf$population,
                     fdi_2_gdp_inf = myData_inf$fdi_1/myData_inf$gdp_1,
)

#Transform the FDI and FDI to GDP ration= variables
#nega_tive is natural log transformation of a translated series.
    #The translate and transform produce 1 and 0 respectly as benchmarks
#nega_percent is plane transform to get 0 as benchmark

myData_inf <- mutate(myData_inf, 
                     fdi_1 = nega_tive(myData_inf$fdi_1),
                     fdi_2_gdp = nega_percent(myData_inf$fdi_2_gdp),
                     fdi_2_gdp_inf = nega_percent(myData_inf$fdi_2_gdp_inf)
)
#Let's Create a new data within which we're going to renmae variables

thesisData <- rename(myData_inf, LN_fdi = fdi_1,
                     LN_politica = political)




"AGO" "BDI" "BEN"
"BWA" "CAF" "CIV" 
"CMR" "COG" "CPV" 
"GAB" "GHA" "GIN"
"GMB" "GNB" "KEN" 
"MDG" "MLI" "MOZ"
"MRT" "MUS" "NAM" 
"NER" "NGA" "RWA" 
"SDN" "SEN" "SLE" 
"SWZ" "SYC" "TCD"
 "TGO" "TZA" "UGA" 
 "ZAF""ZMB" "ZWE"


