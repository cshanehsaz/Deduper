#merges records based on duplicate emails
#ToDo: add a time estimator
loadAndSortDataframe <- function(file = 'DuplicatesForRTest.csv') {
  cat('Processing: 0%\n')
  df <- data.frame(read.csv(file), stringsAsFactors = FALSE)
  df <- df[order(df$Email),]
  numCols <- length(df)
  rowsToSave <- c()
  df.dupes <- setNames(data.frame(matrix(rep(NA,numCols), ncol=numCols)), names(df))
  for(keyRow in 1:nrow(df)) {
    count <- 0
    keyEmail <- df$Email[keyRow]
    if((keyRow/100) == as.integer(keyRow/100)) {
      cat('\014')
      cat("Processing: ", as.integer(keyRow/nrow(df)*100), "%\r", sep='')
      #progress <<- as.integer(keyRow/nrow(df)*100)
    }
    
    lowerIndex <- keyRow - 20
    if(lowerIndex < 1) {lowerIndex <- 1}
    upperIndex <- keyRow + 20
    if(upperIndex > nrow(df)) {upperIndex <- nrow(df)}
    count <- length(which(df$Email[lowerIndex:upperIndex] == keyEmail))
    
    if(count >= 2) {
      #df.dupes <- rbind(df.dupes, df[keyRow,])
      rowsToSave <- c(rowsToSave, keyRow)
    }
    
  }
  cat("Processing Complete: Finishing Execution...\n")
  df.dupes <- rbind(df.dupes, df[rowsToSave,])
  return(df.dupes)
}

mergeAllDuplicates <- function(df) {
  index <- 1
  while(index < nrow(df) + 1) {
    dupe <- data.frame()
    dupe <- rbind(dupe, df[index,])
    
    if(index/100==as.integer(index/100)){
      cat("Records Processed:", index, '\n')
    }
    while(identical(df$Email[index+1],df$Email[index])) {
      index <- index + 1
      dupe <- rbind(dupe, df[index,])
    }
    
    df.clean <<- mergeFields(dupe)
    index <- index + 1
  }
}

#merges a set of duplicate records provided
#only works on one set of records e.g. 2 duplicates
mergeFields <- function(dataframe) {
  numCols <- length(dataframe)
  creation_matrix <- matrix(rep(NA,numCols),ncol=numCols)
  merged_record <- data.frame(creation_matrix)
  oldestContact <- which(dataframe$Created.Date == min(dataframe$Created.Date))
  IdsToDelete <<- c(IdsToDelete, dataframe$X18.Digit.Contact.ID[-oldestContact[1]])
  
  for(column in 1:length(dataframe)) {
    temp <- NA
    tempDate <- NA
    count <- 0
    for(row in 1:nrow(dataframe)) {
      #no values for this field yet
      if(count==0) {
        if(!is.na(dataframe[row, column])) {
          temp <- dataframe[row, column]
          tempDate <- dataframe$Created.Date[row]
          count <- count + 1
        }
      #conflicting values for this field
      } else {
        if(!is.na(dataframe[row, column])) {
          #old is master
          if(names(dataframe)[column] %in% fieldsWithOldMaster) {
            if(dataframe$Created.Date[row] < tempDate) {
              temp <- dataframe[row, column]
              tempData <- dataframe$Created.Date[row]
              count <- count + 1
            }
          }
          #recent is master
          else {
            if(dataframe$Created.Date[row] > tempDate) {
              temp <- dataframe[row, column]
              tempData <- dataframe$Created.Date[row]
              count <- count + 1
            }
          }
        }
      }
    }
    
    # if((names(dataframe)[column]%in% fieldsWithOldMaster) && (count >= 2)) {
    #   print(paste('CONFLICTING FIELDS:', dataframe[1,2], 'Column:', column))
    # }
    merged_record[,column] <- temp
    
  }
  df.clean <- rbind(df.clean, merged_record)
  return(df.clean)
}

replaceBlankWithNA <- function(df) {
  for(row in 1:nrow(df)){
    for(col in 1:ncol(df)){
      if(!is.na(df[row,col]) && df[row, col] == '') {
        df[row,col] <- NA
      }
    }
  }
  return(df)
}

DeleteFirstRow <- function(df.clean) {
  deletefirstrow <- TRUE
  for(i in df.clean[1,]) {
    if(!is.na(i)) {
      deletefirstrow <- FALSE
      break
    }
  }
  if(deletefirstrow == TRUE) {
    df.clean <- df.clean[-1,]
  }
  return(df.clean)
}

RunDedupe <- function(file='all_contacts_for_dedupe.csv', oldMaster=''){
  df <- head(read.csv(file))
  if(!('X18.Digit.Contact.ID'%in%names(df) && 'Email'%in%names(df) && 'Created.Date'%in%names(df))) {
    warning('You must include the 18 digit contact ID, Created Date, and Email Fields.')
    cat('Execution Interrupted. Please submit new CSV with required fields.')
    return()
  }
  cat('File is Valid\n')
  cat('Pre-Processing Records...\n')
  df <- loadAndSortDataframe(file)
  df <- replaceBlankWithNA(df)
  df.clean <<- data.frame()
  fieldsWithOldMaster <<- c("Original.Guru.Trial.Created", "Guru.Edition", "Original.Guru.Org.ID", "X18.Digit.Contact.ID",
                           "Original.Guru.Demo.Requested", "Demo.Request.Synced", "Original.Guru.Role",
                           "Original.Guru.Date.Created", "First.Conversion", "Original.Source",
                           "First.Conversion.Date", "Old...Cadence.Name", "Old...Last.Complete.Step",
                           "Old...Next.Step.Due.Date")
  if(oldMaster[1] != '') {
    fieldsWithOldMaster <<- oldMaster
  }
  IdsToDelete <<- c()
  mergeAllDuplicates(df)
  df.clean <<- setNames(df.clean, names(df))
  df.clean <- DeleteFirstRow(df.clean)
  IdsToDelete <<- data.frame(IdsToDelete)
  
  updateDir <- paste0(getwd(), '/RecordsToUpdate.csv')
  deleteDir <- paste0(getwd(), '/RecordsToDelete.csv')
  
  try(file.remove(updateDir))
  try(file.remove(deleteDir))
  write.csv(df.clean, updateDir)
  write.csv(IdsToDelete, deleteDir)
  cat('Execution Complete\n')
}

# RunDedupe('records_to_dedupe.csv')
# RunDedupe('DuplicatesForRTest5.csv')
# RunDedupe('dedupetest6.csv')
