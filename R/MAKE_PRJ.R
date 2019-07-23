MAKE_PRJ <- function(rep, couche){
  options(useFancyQuotes = FALSE)
  PRJ <- file(paste0(paste(rep,couche,sep="/"), ".prj"))
    writeLines(
    paste0('PROJCS[', dQuote('RGF93_Lambert_93'), ',GEOGCS[', dQuote('GCS_RGF93'),
           ',DATUM[', dQuote('D_RGF_1993'), ',SPHEROID[', dQuote('GRS_1980'),
           ',6378137,298.257222101]],PRIMEM[', dQuote('Greenwich'), ',0],UNIT[',
           dQuote('Degree'), ',0.017453292519943295]],PROJECTION[',
           dQuote('Lambert_Conformal_Conic'), '],PARAMETER[', dQuote('standard_parallel_1'),
           ',49],PARAMETER[', dQuote('standard_parallel_2'), ',44],PARAMETER[',
           dQuote('latitude_of_origin'), ',46.5],PARAMETER[', dQuote('central_meridian'), ',3],PARAMETER[',
           dQuote('false_easting'), ',700000],PARAMETER[', dQuote('false_northing'), ',6600000],UNIT[',
           dQuote('Meter'), ',1]]')
    , PRJ)
  close(PRJ)
}
