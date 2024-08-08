# To Do Functions. These are funcitons specifically geared towards making
# working with my elaborate to do data set easier.

#' @importFrom dplyr filter group_by left_join mutate rename select ungroup
#' @importFrom lubridate days
#' @importFrom magrittr %>%
#' @importFrom readxl read_excel
#' @importFrom stringr str_count str_pad str_to_lower
#' @importFrom tidyr pivot_longer separate
NULL

#' Reads in the ritual to do table, which is formatted kind of weirdly, in a
#' format that is significantly easier to work with
#'
#' This function reads the ritual table out of the project list.xlsx file.
#'
#' @param inFile The directory to project list.xlsx
#' @param ritualSheet The name of the sheet where the ritual table lives
#' @param parentSheet The name of the sheet that contains ritual parent data,
#'   which is split out from teh ritual sheet
#' @param deetSheet The name of the sheet that contains the ritual details
#'   table, separate from the main sheet
#' @return A cleaned, long-form data set containing the completion times of all
#'   of my ritual tasks
#' @export

readToDoRituals <- function(inFile,ritualSheet='rituals',parentSheet='Ritual parentage',deetSheet='Ritual active range') {

  minutesPerDay <- 24*60

  nameList <- c('entryDate','id','tz','parent','project','dueDate','toDo','finDate','type','notes')

  # to do details are stored in a separate sheet
  toDoDeets <- readxl::read_excel(inFile,sheet=deetSheet)

  # parentage is stored in a separate sheet
  toDoParents <- readxl::read_excel(inFile,sheet=parentSheet) %>%
    dplyr::rename(notes=desc) %>%
    dplyr::mutate(
      temp=round(as.numeric(parent),2),
      parent = ifelse(as.numeric(parent)%%1>0 & !is.na(temp),as.character(temp),parent)
    )

  toDoData <- readxl::read_excel(inFile,sheet=ritualSheet,skip = 5,col_types='text') %>%
    dplyr::filter(id != 0) %>%
    tidyr::pivot_longer(
      -c('date','id','tz'),
      names_to = 'toDo',
      values_to = 'completionTime'
    ) %>%
    tidyr::separate(toDo,into=c('idNum','toDo'),sep=' - ') %>%
    dplyr::mutate(
      date = as.POSIXct(as.Date(as.numeric(date),origin = '1899-12-30'),tz='UTC'),
      idNum = as.numeric(idNum),
      completionTime = stringr::str_to_lower(completionTime),
      totalComps = stringr::str_count(completionTime,';')+1,
      incomplete = ifelse(is.na(completionTime),1,0)
    ) %>%
    #we merge details in early because we need to know when a ritual started &
    #ended, so that we can drop lines in that range
    dplyr::left_join(toDoDeets,by=c('idNum'='num')) %>%
    dplyr::filter(!(date < start | date > end)) %>%
    #filter out x's since thoswe are days where completion is not expected
    dplyr::filter(completionTime!='x'|incomplete==1) %>%
    #split out the total completion times
    tidyr::separate(completionTime,into=sprintf('%s',seq(1:max(.$totalComps,na.rm=T))),sep=';') %>%
    tidyr::pivot_longer(
      sprintf('%s',seq(1:max(.$totalComps,na.rm=T))),
      names_to = 'compNum',
      values_to = 'completionTime'
    ) %>%
    dplyr::filter(!(compNum > 1 & is.na(completionTime))) %>%
    dplyr::group_by(id,idNum) %>%
    # The times are stored as both text and numeric values because, on certain
    # days, I complete multiple rituals necessitating listing multiple times. This
    # convention fucking sucks because it necessitates a complicated set of code
    # to extract those days and appropriately format them.
    dplyr::mutate(
      idMaxComp = ifelse(max(compNum,na.rm = T) > 1,1,0),
      excelTime = as.numeric(completionTime) * minutesPerDay,
      compDays = floor(excelTime/24/60),
      compHr = floor(excelTime/60)-(compDays*24),
      compMin = excelTime%%60,

      textTime = ifelse(
        idMaxComp == 1,
        completionTime,
        ifelse(
          incomplete==0,
          sprintf('%s:%s',compHr,stringr::str_pad(round(compMin,0),2,pad='0')),
          NA
        )
      ),
      # most ids for rituals are simply R16.1, where the number after the R is the
      # indexed day of tracking and the .1 is the ritual number. Days with
      # multiple completions need to distinguish between each to do, so we include
      # an additional .1 or .2 or .3, indexed to the sequence in which these tasks
      # were completed
      id = sprintf(
        'R%s.%s%s',
        id,idNum,
        ifelse(
          idMaxComp == 1,
          sprintf('.%s',compNum),
          ''
        )
      ),
      completion = ifelse(
        is.na(textTime),NA,sprintf('%s %s',date+lubridate::days(compDays),textTime)
      ),
      complete = as.POSIXct(completion, format = "%Y-%m-%d %H:%M",tz='UTC'),
      type = 'ritual',
      dueDate = date
    ) %>%
    #with id's calculated we can merge in parentage
    dplyr::left_join(toDoParents,by=c('id'='ID')) %>%
    dplyr::ungroup() %>%
    dplyr::select(
      date,id,tz,parent,project,dueDate,desc,complete,type,notes
    )

  #rename all columns
  colnames(toDoData) <- nameList

  return(toDoData)
}

