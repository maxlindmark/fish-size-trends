#### With this script you can download exchange data from DATRAS. It takes a lot of time to run this script on a normal laptop so it is advised to run it on a server or a cluster.

library(icesDatras)
# install.packages('DATRAS',repos='http://www.rforge.net/',type='source')
library(DATRAS)

## check surveys available for download
getSurveyList()

# Select one survey
SURV <- "NS-IBTS"

## Extract available years for the survey of interest.
# Available years may get extracted with icesDatras::getSurveyYearList
YYaval <- getSurveyYearList(SURV)

## Extract all available records for the survey.
# Extraction is performed using icesDatras::getDATRAS, on user selection for survey and year and all quarters available
# ca <- getDATRAS("CA", SURV, YYaval, c(1,3))
# hh <- getDATRAS("HH", SURV, YYaval, c(1,3))
# hl <- getDATRAS("HL", SURV, YYaval, c(1,3))

### strict	if TRUE, missing haul ids in age data should be uniquely matched when filled in, if FALSE a random match will be assigned.

NS_IBTS <- getDatrasExchange(SURV, YYaval, c(1,3), strict = T)

NS_IBTS_nostrict <- getDatrasExchange(SURV, YYaval, c(1,3), strict = F)


# Select one survey
SURV <- "EVHOE"

## Extract available years for the survey of interest.
# Available years may get extracted with icesDatras::getSurveyYearList
YYaval <- getSurveyYearList(SURV)


EVHOE_strict <- getDatrasExchange(SURV, YYaval, 1:4, strict = T)
EVHOE_nostrict <- getDatrasExchange(SURV, YYaval, 1:4, strict = F)


# Select one survey
SURV <- "SCOWCGFS"

## Extract available years for the survey of interest.
# Available years may get extracted with icesDatras::getSurveyYearList
YYaval <- getSurveyYearList(SURV)


SCOWCGFS_strict <- getDatrasExchange(SURV, YYaval, 1:4, strict = T)
SCOWCGFS_nostrict <- getDatrasExchange(SURV, YYaval, 1:4, strict = F)


# Select one survey
SURV <- "SWC-IBTS"

## Extract available years for the survey of interest.
# Available years may get extracted with icesDatras::getSurveyYearList
YYaval <- getSurveyYearList(SURV)


SWC_IBTS_strict <- getDatrasExchange(SURV, YYaval, 1:4, strict = T)
SWC_IBTS_nostrict <- getDatrasExchange(SURV, YYaval, 1:4, strict = F)

### Save all surveys as Rdata
save(NS_IBTS, EVHOE_strict, SWC_IBTS_strict, SCOWCGFS_strict, file = "Data/Surveys_strict.Rdata")
save(NS_IBTS_nostrict, EVHOE_nostrict, SWC_IBTS_nostrict, SCOWCGFS_nostrict, file = "Data/Surveys_nostrict.Rdata")
