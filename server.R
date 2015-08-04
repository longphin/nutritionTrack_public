#setwd("~/Documents/codes/R/shinyapps")
library(shiny)
library(rPython)
library(DT)
#python.load("nutriTrack/main.py")
python.load("main.py")

DEBUG=FALSE
# --------------------------------------------------------------------
# global variables
# --------------------------------------------------------------------
knownVolumes=c("tsp","tbs","cup","pt","qt","gal","oz","L","mL")
knownVolumes_aliases=c(teaspoon="tsp", teaspoons="tsp",
                       tbsp="tbs", tablespoon="tbs", tablespoons="tbs",
                       cups="cup",
                       pint="pt", pints="pt",
                       quart="qt", quarts="qt", qrt="qt",
                       gallon="gal", gallons="gal",
                       ounce="oz", ounces="oz",
                       "fluid oz"="oz", 'fl oz'='oz',
                       liter="L", liters="L",
                       mililiter="mL", mililiters="mL")

knownVolumesEtc=c("serving","small","medium","large")
knownVolumesEtc_aliases=c(servings="serving", jumbo="large")

knownWeights=c("g","kg","oz","lb")
knownWeights_aliases=c(gram="g", grams="g",
                       kilogram="kg", kilograms="kg",
                       ounce="oz", ounces="oz",
                       lbs="lb", pound="lb", pounds="lb")

# volume regex: whole words with "(L)|(liters)|(gal)..."
knownVolumes_regexpr=paste("(",paste(c(knownVolumes, names(knownVolumes_aliases)), collapse=")|("), ")", sep="")
volume_regexpr=paste("\\<(", knownVolumes_regexpr, ")\\>", sep="")
# volume (etc) regex: whole words with "(small)|(medium)|(large)"
knownVolumesEtc_regexpr=paste("(",paste(c(knownVolumesEtc, names(knownVolumesEtc_aliases)), collapse=")|("), ")", sep="")
volumeEtc_regexpr=paste("\\<(", knownVolumesEtc_regexpr, ")\\>", sep="")

# weight regex: whole words with "(g)|(grams)|(kg)|..."
knownWeights_regexpr=paste("(",paste(c(knownWeights, names(knownWeights_aliases)), collapse=")|("), ")", sep="")
weight_regexpr=paste("\\<(", knownWeights_regexpr, ")\\>", sep="")
# recipe input regex: match form "1 cup milk"
knownMeasurements=c(knownVolumes,knownWeights, knownVolumesEtc,
                    names(knownVolumes_aliases), names(knownVolumesEtc_aliases), names(knownWeights_aliases))
knownMeasurements_regexpr=paste("(",paste(knownMeasurements, collapse=")|("), ")", sep="")
measurement_regexpr=paste("[[:digit:][:punct:] ]+ \\<(", knownMeasurements_regexpr, ")\\>", sep="")

createConversionMatrix<-function(mat){
  # make the recipricol
  w=which(!is.na(mat) & mat!=1,arr.ind=TRUE)
  for(i in 1:nrow(w)){
    mat[w[i,2],w[i,1]]=1/mat[w[i,1],w[i,2]]
  }
  # NOTE: Filling in the conversion table only works because the off-diagonal and 1-off diagonal are known
  # fill the rest, upper triangular
  for(j in 2:ncol(mat)){
    for(i in 1:(j-1)){
      if(is.na(mat[i,j])){
        mat[i,j]=mat[i,(j-1)]*mat[(j-1),j]
      }
    }
  }
  w=which(!is.na(mat) & mat!=1,arr.ind=TRUE)
  for(i in 1:nrow(w)){
    mat[w[i,2],w[i,1]]=1/mat[w[i,1],w[i,2]]
  }
  return(mat)
}

# knownVolumes=c("tsp","tbs","cup","pt","qt","gal","oz","L","mL")
mat=matrix(ncol=length(knownVolumes),nrow=length(knownVolumes), dimnames=list(knownVolumes,knownVolumes))
diag(mat)=rep(1,length(knownVolumes))
mat['tsp','tbs']=3
mat['tbs','cup']=16
mat['cup','oz']=1/8
mat['pt','cup']=1/2
mat['qt','pt']=1/2
mat['gal','qt']=1/4
mat['gal','oz']=1/128
mat['oz','L']=33.814
mat['L','mL']=1/1000
volumeConversion=createConversionMatrix(mat)

# knownWeights=c("g","kg","oz","lb")
mat=matrix(ncol=length(knownWeights),nrow=length(knownWeights), dimnames=list(knownWeights,knownWeights))
diag(mat)=rep(1,length(knownWeights))
mat['g','kg']=1000
mat['kg','oz']=1/35.274
mat['oz','lb']=16
weightConversion=createConversionMatrix(mat)

# helper functions
mixedToFloat <- function(x){
  # remove leading and trailing whitespace
  x = sub("^[^\\([:digit:]\\.]+", "", x)
  # turn non-prefaced decimals to have a 0, ex: .5->0.5
  x = sub("^ *[[:digit:]]*\\.+([[:digit:]])", "0.\\1", x)
  # replace consequetive division signs with only 1 division.
  x = gsub("\\*+", "\\*", gsub("\\-+", "\\-", gsub("\\++", "\\+", gsub("/+", "/", x))))
  # turn whitespaces into additions
  while(grepl("[[:digit:]]+ +[[:digit:]]+", x))
  {
    x = gsub("([[:digit:]]+) +([[:digit:]]+)", '\\1+\\2', x)
  }
  
  res=try(unlist(lapply(x, function(x) eval(parse(text=x)))))
  if(inherits(res, 'try-error')) res=0
  return(res)
}

# --------------------------------------------------------------------
# server functions
# --------------------------------------------------------------------

# userMeasurement (string), item_measurements (string vector)
# Return a vector of indices for which userMeasurement is the same category (weight|volume) as item_measurements's elements.
# e.g. userMeasurement="tsp", item_measurements=["cup", "grams", "tbs"] -> [1,3]
whichSameMeasurementType<-function(userMeasurement, item_measurements)
{
  if(userMeasurement%in%knownVolumes){
    return(which(grepl(volume_regexpr, item_measurements)))
  }
  
  if(userMeasurement%in%knownWeights)
    return(which(grepl(weight_regexpr, item_measurements)))
  
  if(userMeasurement%in%knownVolumesEtc)
    return(which(grepl(volumeEtc_regexpr, item_measurements)))
  
  return(which(!grepl(volume_regexpr, item_measurements) & !grepl(weight_regexpr, item_measurements)))
  #return(integer(0))
}

# measurement (string)
# Return the standardized measurement name from input.
# e.g. measurement="tablespoons" -> "tbs"
standardizeMeasurement<-function(measurement)
{
  measurement=tolower(measurement)
  if(measurement %in% names(knownWeights_aliases))
    return(unname(knownWeights_aliases[measurement]))
  if(measurement %in% knownWeights)
    return(measurement)
  
  if(measurement %in% names(knownVolumes_aliases))
    return(unname(knownVolumes_aliases[measurement]))
  if(measurement %in% knownVolumes)
    return(measurement)
  
  return('')
}

# item (string)
# Returns vector (qty, unit, item) info from input.
getItemInfo <- function(item){
  qty_regexpr="^([[:digit:]/\\+\\-\\*\\(\\) ¼½¾⅓⅔⅕⅖⅗⅘⅙⅚⅛⅜⅝⅞]+) ?.*"
  unit_regexpr="^([[:digit:]/\\+\\-\\*\\(\\) ]+) ?([^ ]+) ?.*"
  item_regexpr="^([[:digit:]/\\+\\-\\*\\(\\) ]+) ?([^ ]+) ?(.*)"
  item_regexpr_with_nonstandard_unit="^([[:digit:]/\\+\\-\\*\\(\\) ]+) ?(.*)"
  item=gsub(" +$", "", gsub("^ +", "", item)) # remove trailing and leading whitespace
  item=sub("^pinch ", "1/8 tsp ", item, ignore.case=TRUE)
  item=sub("^([[:digit:] ]*)\\(([[:digit:]]+)([^\\)]*)\\)", "(\\1)*\\2 \\3", item) # split up form 1 (8 oz) item -> 1*8 oz item
  item=sub("^([[:digit:] ]*)dozen", "\\1*12", item, ignore.case=TRUE)
  itemInfo = character(3)
  if(grepl(qty_regexpr, item)==FALSE)
    return(c(item, paste(item, ' - Missing quantity.', collapse=''),"(Error)")) # improper input
  tmp=gsub(qty_regexpr, "\\1", item)
  if(grepl("[¼½¾⅓⅔⅕⅖⅗⅘⅙⅚⅛⅜⅝⅞]", tmp)) # only do these many gsubs if necessary
  {
    tmp=gsub("¼", " 1/4 ", tmp)
    tmp=gsub("½", " 1/2 ", tmp)
    tmp=gsub("¾", " 3/4 ", tmp)
    tmp=gsub("⅓", " 1/3 ", tmp)
    tmp=gsub("⅔", " 2/3 ", tmp)
    tmp=gsub("⅕", " 1/5 ", tmp)
    tmp=gsub("⅖", " 2/5 ", tmp)
    tmp=gsub("⅗", " 3/5 ", tmp)
    tmp=gsub("⅘", " 4/5 ", tmp)
    tmp=gsub("⅙", " 1/6 ", tmp)
    tmp=gsub("⅚", " 5/6 ", tmp)
    tmp=gsub("⅛", " 1/8 ", tmp)
    tmp=gsub("⅜", " 3/8 ", tmp)
    tmp=gsub("⅝", " 5/8 ", tmp)
    tmp=gsub("⅞", " 7/8 ", tmp)
  }
  itemInfo[1]=try(as.character(mixedToFloat(tmp)))
  if(inherits(itemInfo[1], "try-error")) # the conversion from character to float did not work
    return(c(item, paste(item, ' - Could not evaluate quantity.', collapse=''),"(Error)")) # unknown quantity
  
#  if(grepl(unit_regexpr, item, ignore.case=TRUE)==FALSE)
#    return(c('2a','Measurement is not known or standard.',"(Error)")) # improper measurement
#  if(!grepl(unit_regexpr, item, ignore.case=TRUE)==FALSE){
    tmp=gsub(unit_regexpr, "\\2", item, ignore.case=TRUE)
    itemInfo[2]=standardizeMeasurement(tmp)
    
    if(itemInfo[2]!=''){ # item uses standard unit
      tmp=gsub(item_regexpr, "\\3", item)
    }else{ # item uses nonstandard unit, which is set to ''
      tmp=gsub(item_regexpr_with_nonstandard_unit, "\\2", item)
    }
    
    #if(grepl(item_regexpr, item)==FALSE)
    #  return(c('3a','Missing item.',"(Error)")) # no item input
    #tmp=gsub(item_regexpr, "\\3", item)
#   }else{ # item has a nonstandard unit
#     if(grepl(item_regexpr_with_nonstandard_unit, item)==FALSE)
#       return(c('3b','Missing item.',"(Error)")) # no item input
#     tmp=gsub(item_regexpr_with_nonstandard_unit, "\\3", item)
#   }
  
    # remove excess punctuation and contents between parentheses. [BREAKS] when parentheses are nested
  itemInfo[3]=gsub("[[:punct:]]", "", gsub("\\s*\\(+[^\\)]+\\)+","", tmp))
  
#  if(!(itemInfo[2] %in% knownMeasurements))
#    return(c('2b','Measurement is not known or standard.',"(Error)")) # improper measurement
  
  #if(length(itemInfo)!=3)
  #  return(c('1b','Missing quantity, unit size, or item.',"(Error)")) # improper input
  if(itemInfo[3]=="")
    return(c(item, paste(item, ' - Missing item.', collapse=''),"(Error)")) # no item input
  
  return(itemInfo)
}

# recipe (string with newlines)
# Return a list:
# [[1]] a list of valid (qty, unit, item, itemNumber) for each line from recipe
# [[2]] a list of error messages for items that did not fit the format of [[1]]
filterInput <- function(recipe){
  items=unlist(strsplit(recipe, split="\n"))
  items=items[items!='']
  if(length(items)==0) return(list(NULL,NULL)) # error check: empty input
  
  info=t(unname(sapply(items, getItemInfo)))
  info=cbind(info, 1:nrow(info)) # keeps track of which item is looked at
  w=info[,3]!="(Error)"
  validinfo=matrix(info[w,], ncol=4)
  if(sum(!w)==0)
  {
    errorMessages=NULL
  }else{
    errorMessages=matrix(info[!w,c(4,2)],ncol=2)
  }
  return(list(validinfo, errorMessages))
}

# userMeasurement (string), dbMeasurement (string)
# Return the multiplier for converting from dbMeasurement to userMeasurement
# e.g. userMeasurement="tbs", dbMeasurement="tsp" -> return 3 (since 3 tsp in one tbs)
convertServing<-function(userMeasurement, dbMeasurement){
  if(userMeasurement%in%knownVolumes & dbMeasurement%in%knownVolumes)
    return(volumeConversion[dbMeasurement,userMeasurement])
  
  if(userMeasurement%in%knownWeights & dbMeasurement%in%knownWeights)
    return(weightConversion[dbMeasurement,userMeasurement])
  
  return(1)
}

# facts (list(list(list))), recipeMatrix (matrix nx4), selectionIndex (integer vector), factIndex (integer vector)
# 
cbindFacts<-function(facts, recipeMatrix, selectionIndex, factIndex){
  # Go through each item's related set, and choose the one that the user selected.
  info=lapply(1:length(factIndex), function(i){
    #t=unlist(facts[[factIndex[i]]][[selectionIndex[i]]])
    #t['Calories']=4*as.numeric(t['nf_protein'])+4*as.numeric(t['nf_total_carbohydrate'])+9*as.numeric(t['nf_total_fat'])
    return(unlist(facts[[factIndex[i]]][[selectionIndex[i]]]))
  })
  itemNames=unname(sapply(info, function(i) i['item_name']))
  factNames=unique(names(unlist(info)))
  factNames=factNames[!factNames%in%c("item_id", "item_name", "brand_name", "nf_serving_size_unit", "nf_serving_size_qty")]
  
  # create facts table and fill
  factsTable=matrix(ncol=length(factIndex), nrow=length(factNames))
  for(i in 1:ncol(factsTable)){
    v=info[[i]][factNames]
    # conversion from database serving size to user's serving size
    userServing=recipeMatrix[i,'unit']
    dbServing=info[[i]]['nf_serving_size_unit']
    dbToUser=convertServing(userServing, dbServing)
    
    w=which(factNames%in%names(v))
    factsTable[w,i]=as.numeric(v[factNames[w]])*dbToUser*as.numeric(recipeMatrix[i,'qty'])
  }
  factsTable=cbind(Total=rowSums(factsTable, na.rm=TRUE), factsTable)
  # correct the rownames to make it human-readable
  factNames=gsub("_", " ", gsub("nf_","", factNames))
  factNames=gsub("^monounsaturated", "Monounsat.", factNames, ignore.case=TRUE)
  factNames=gsub("^polyunsaturated", "Polyunsat.", factNames, ignore.case=TRUE)
  factNames=gsub("^saturated", "Sat.", factNames, ignore.case=TRUE)
  factNames=gsub("carbohydrate", "Carbs", factNames, ignore.case=TRUE)
  factNames=gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", factNames, perl=TRUE)
  factNames=gsub(" Dv$", " (DV %)", factNames)
  # specific corrections
  factNames[factNames=="Dietary Fiber"]="Fiber"
  
  # sort the rows based on common nutrition ordering
  tier1=c("Calories", "Calories From Fat")
  tier2=c("Total Fat","Sat. Fat","Monounsat. Fat", "Polyunsat. Fat", "Trans Fat", "Cholesterol","Sodium","Total Carbs", "Fiber", "Sugars","Protein")
  w1=unlist(sapply(tier1, function(x) which(factNames%in%x)))
  w2=unlist(sapply(tier2, function(x) which(factNames%in%x)))
  w0=which(!factNames %in% c(tier1,tier2))
  mat=cbind(factNames[c(w1,w2,w0)], matrix(factsTable[c(w1,w2,w0),], nrow=length(factNames)))
  colnames(mat)=c("Facts", "Total", itemNames)
  
  return(mat)
}

# Make the selectionIndex boxes for alternative items.
#
# > i = which selection box is this for, 1,2,3,...,10
# > li = nutrition facts list
createChoiceButtons<-function(i, li, selectionIndex, choices){
  if(is.null(li) | length(choices)==0){
    return(NULL)
  }else{
    l=as.list(choices)
    names(l)=sapply(choices, function(j) sprintf("%s (%s) -- serving: %s, calories: %.2f, fat: %.2f, protein: %.2f, carbs: %.2f",
                                                 li[[j]]$item_name,
                                                 li[[j]]$brand_name,
                                                 li[[j]]$nf_serving_size_unit,
                                                 as.numeric(li[[j]]$calories),
                                                 as.numeric(li[[j]]$nf_total_fat),
                                                 as.numeric(li[[j]]$nf_protein),
                                                 as.numeric(li[[j]]$nf_total_carbohydrate))
                    )
    return(selectInput(paste("item",i,sep=''), label=NULL, choices=l, selected=selectionIndex, width='100%'))
  }
}

# string (string vector), length (integer)
# Return a shortened version of strings, up to <length> characters. Adds ellipses if shortened.
shortenString<-function(string, length=10)
{
  n=nchar(string)
  w=which(n>length)
  string[w]=sub("^(\\w[^ ]+ +\\w[^ ]+ +\\w[^ ]+) +.*", "\\1...", string[w])
  return(string)
}

# string (string), strings (string vector)
# Return a measure for how similar strings are.
stringSimilarity<-function(string, strings)
{
  # chars=list() with chars[[1]]=vector of characters for string
  #                   chars[[-1]]=vector of characters for strings
  #chars=strsplit(c(string, strings),'')
  
  string=gsub("[[:punct:]]","", string)
  strings=gsub("[[:punct:]]","",strings)
  chars=strsplit(c(string, strings), ' ')
  val=sapply(as.list(chars[-1]), function(x) sum(x%in%chars[[1]])/length(x))
  return(val)
}

# --------------------------------------------------------------------
# Main function
# --------------------------------------------------------------------
shinyServer(function(input, output, session){
  reactivevals<-reactiveValues()
  # page number that user is currently on
  #reactivevals$serverBusy=FALSE
  reactivevals$errorMessages=NULL
  reactivevals$pageNumber=1
  reactivevals$servings=1
  reactivevals$itemSelection=0 # dummy default
  reactivevals$factIndex=NULL # dummy default
  reactivevals$choiceList=NULL # dummy default
  
  # character limit
  output$characterlimit<-renderText(
    {
      x=unlist(strsplit(input$recipeInput, ''))#nchar(input$recipeInput)
      newline_count=sum(x=='\n')
      char_count=2*newline_count+(length(x)-newline_count)
      if(char_count<250) # only show character limit if input has more than 250 characters
        return(NULL)
      
      return(paste("character limit:", 750-char_count))
    })
  
  # Submit button is pressed -> filter input and form matrix
  recipeMatrix<-reactive({
    if(DEBUG) print("recipeMatrix")
    if(is.null(input$submit))#serverBusy==TRUE)# input$submit == 0)
      return(NULL)
    
    isolate({
      # reset page number reactive values
      #reactivevals$serverBusy=TRUE
      reactivevals$errorMessages=NULL
      reactivevals$pageNumber=1
      reactivevals$factIndex=NULL
      
      # ---------------------------------------------
      # Obtain recipe, filter, and nutritional facts.
      # ---------------------------------------------
      inputMatrix=filterInput(input$recipeInput)
      reactivevals$errorMessages=inputMatrix[[2]]
      itemsList=inputMatrix[[1]]
      if(is.null(itemsList)){ # error: textbox was empty
        output$selectUI_1<-renderUI(return())
        output$selectUI_2<-renderUI(return())
        output$selectUI_3<-renderUI(return())
        output$selectUI_4<-renderUI(return())
        output$selectUI_5<-renderUI(return())
        output$selectUI_6<-renderUI(return())
        output$selectUI_7<-renderUI(return())
        output$selectUI_8<-renderUI(return())
        output$selectUI_9<-renderUI(return())
        output$selectUI_10<-renderUI(return())
        
        reactivevals$errorMessages=NULL
        output$errorMessages<-renderPrint(invisible())
        return(NULL)
      }
      
      res=matrix(itemsList, ncol=4,dimnames=list(NULL,c("qty","unit","item","itemNumber")))
      if(nrow(res)==0) return(NULL) # error: all items had errors
      
      # recipeMatrix has columns qty, unit, item, itemNumber
      return(res)
    })})
  
  nutriTrackFacts<-reactive({
    if(DEBUG) print("nutriTrackFacts")
    if(is.null(recipeMatrix()))
    {
      #reactivevals$serverBusy=FALSE
      return(NULL)
    }
    isolate({
      # take care of duplicates to reduce database searching
      items=unname(recipeMatrix()[,'item'])
      uniqueItems=NULL
      reactivevals$factIndex=numeric(length(items))
      names(reactivevals$factIndex)=1:length(items)
      for(i in 1:length(items))
      {
        if(reactivevals$factIndex[i]!=0) next # item is already determined to be a duplicate
        
        uniqueItems=c(uniqueItems, i)
        w=which(items[-i]==items[i])
        if(length(w)>0) reactivevals$factIndex[w+1]=length(uniqueItems)#i # not unique item
        
        reactivevals$factIndex[i]=length(uniqueItems)#i
      }
      
      # get nutritional facts
      nutritionFacts=python.call("main", as.list(unname(recipeMatrix()[uniqueItems,3])))
      if(length(nutritionFacts)==0) return(NULL)
      if(!is.list(nutritionFacts[[1]][[1]]))
        nutritionFacts=list(nutritionFacts)
      
      # item name corrections - to avoid errors
      for(i in 1:length(nutritionFacts)){
        for(j in 1:length(nutritionFacts[[i]])){
          itemname=nutritionFacts[[i]][[j]]$item_name
          itemname=gsub('([[:digit:]])"', "\\1 inch",itemname)
          itemname=gsub("([[:digit:]])'", "\\1 ft",itemname)
          itemname=gsub(measurement_regexpr, "",itemname, ignore.case=TRUE)
          nutritionFacts[[i]][[j]]$item_name=itemname
          
          # convert NULL values to 0
          w=sapply(nutritionFacts[[i]][[j]], function(x) is.null(x))
          nutritionFacts[[i]][[j]][w]=0
          
          # add a calories section
          t=unlist(nutritionFacts[[i]][[j]][c('nf_protein', 'nf_total_carbohydrate', 'nf_total_fat')])
          nutritionFacts[[i]][[j]]$calories=unname(4*ifelse(!is.null(t['nf_protein']), as.numeric(t['nf_protein']), 0)+
            4*ifelse(!is.na(t['nf_total_carbohydrate']), as.numeric(t['nf_total_carbohydrate']))+
            9*ifelse(!is.na(t['nf_total_fat']), as.numeric(t['nf_total_fat']), 0))
          
          # standardize measurements
          standardmeasurement=standardizeMeasurement(nutritionFacts[[i]][[j]]$nf_serving_size_unit)
          if(standardmeasurement!='')
            nutritionFacts[[i]][[j]]$nf_serving_size_unit=standardmeasurement
        }
      }
      
      # find the indexes for matches that have same measurement units
      reactivevals$choiceList=lapply(1:length(reactivevals$factIndex), function(index){
        i=as.numeric(names(reactivevals$factIndex)[index])
        userMeasurement=recipeMatrix()[i,2]
        related_items=nutritionFacts[[reactivevals$factIndex[i]]]
        item_measurements=sapply(related_items, function(x) x['nf_serving_size_unit'])
        choiceList=whichSameMeasurementType(userMeasurement, item_measurements)
        
        if(length(choiceList)==0)
        {
          reactivevals$errorMessages=rbind(reactivevals$errorMessages,
                                           c(i,sprintf("%s - Database does not have matches using those units. Potential matches use: %s", 
                                                       paste(recipeMatrix()[i,1:3], collapse=''),
                                                       paste(unique(item_measurements), collapse=', '))))
        }
        
        user_item=recipeMatrix()[i,'item']
        related_items_names=sapply(related_items[choiceList], function(x) x$item_name)
        sorted=sort(stringSimilarity(tolower(user_item), tolower(related_items_names)), decreasing=TRUE, index.return=TRUE)$ix
        return(choiceList[sorted])
      })
      
      # remove items that had no matches
      w=which(sapply(reactivevals$choiceList, length)==0)
      if(length(w)>0){
        reactivevals$factIndex=reactivevals$factIndex[-w]
        reactivevals$choiceList=reactivevals$choiceList[-w]
      }
      
      # return errors if no matches
      if(is.null(reactivevals$errorMessages)){
        updateTabsetPanel(session, "recipeTab", "Database Matches")
        output$errorMessages<-renderPrint(invisible())
      }else{
        sorting=sort(as.integer(reactivevals$errorMessages[,1]), index.return=TRUE)$ix
        reactivevals$errorMessages=matrix(reactivevals$errorMessages[sorting,],ncol=2)
        output$errorMessages<-renderPrint(
          return(cat(paste("line ", reactivevals$errorMessages[,1], ": ", reactivevals$errorMessages[,2], collapse="\n", sep="")))
        )
      }
      
      reactivevals$itemSelection=sapply(reactivevals$choiceList, function(x) x[1])#rep(1, length(reactivevals$factIndex))
      #reactivevals$serverBusy=FALSE
      return(nutritionFacts)
    })})
  
  observeEvent(input$seeNutrition, updateTabsetPanel(session, "recipeTab", "The Facts"))
  
  choiceButtons<-observe({
    if(DEBUG) print("choiceButtons")
    if(is.null(nutriTrackFacts()))
      return()
    
    # Item correction tab
    facts=nutriTrackFacts()
    page=reactivevals$pageNumber-1
    minIndex=page*10
    factnum=reactivevals$factIndex
    selectnum=reactivevals$itemSelection
    n=min(length(factnum), page*10+10)-minIndex
    
    if(n>=1){
      output$selectUI_1 <- renderUI(createChoiceButtons(1,facts[[factnum[minIndex+1]]], selectnum[minIndex+1], reactivevals$choiceList[[minIndex+1]]))
      output$select1_info<-renderPrint(cat(paste(recipeMatrix()[as.numeric(names(factnum[minIndex+1])),1:3], collapse=" ")))
    }else{
      output$selectUI_1<-renderUI(return())
      output$select1_info<-renderPrint(invisible())
    }
    if(n>=2){
      output$selectUI_2 <- renderUI(createChoiceButtons(2,facts[[factnum[minIndex+2]]], selectnum[minIndex+2], reactivevals$choiceList[[minIndex+2]]))
      output$select2_info<-renderPrint(cat(paste(recipeMatrix()[as.numeric(names(factnum[minIndex+2])),1:3], collapse=" ")))
    }else{
      output$selectUI_2<-renderUI(return())
      output$select2_info<-renderPrint(invisible())
    }
    if(n>=3){
      output$selectUI_3 <- renderUI(createChoiceButtons(3,facts[[factnum[minIndex+3]]], selectnum[minIndex+3], reactivevals$choiceList[[minIndex+3]]))
      output$select3_info<-renderPrint(cat(paste(recipeMatrix()[as.numeric(names(factnum[minIndex+3])),1:3], collapse=" ")))
    }else{
      output$selectUI_3<-renderUI(return())
      output$select3_info<-renderPrint(invisible())
    }
    if(n>=4){
      output$selectUI_4 <- renderUI(createChoiceButtons(4,facts[[factnum[minIndex+4]]], selectnum[minIndex+4], reactivevals$choiceList[[minIndex+4]]))
      output$select4_info<-renderPrint(cat(paste(recipeMatrix()[as.numeric(names(factnum[minIndex+4])),1:3], collapse=" ")))
    }else{
      output$selectUI_4<-renderUI(return())
      output$select4_info<-renderPrint(invisible())
    }
    if(n>=5){
      output$selectUI_5 <- renderUI(createChoiceButtons(5,facts[[factnum[minIndex+5]]], selectnum[minIndex+5], reactivevals$choiceList[[minIndex+5]]))
      output$select5_info<-renderPrint(cat(paste(recipeMatrix()[as.numeric(names(factnum[minIndex+5])),1:3], collapse=" ")))
    }else{
      output$selectUI_5<-renderUI(return())
      output$select5_info<-renderPrint(invisible())
    }
    if(n>=6){
      output$selectUI_6 <- renderUI(createChoiceButtons(6,facts[[factnum[minIndex+6]]], selectnum[minIndex+6], reactivevals$choiceList[[minIndex+6]]))
      output$select6_info<-renderPrint(cat(paste(recipeMatrix()[as.numeric(names(factnum[minIndex+6])),1:3], collapse=" ")))
    }else{
      output$selectUI_6<-renderUI(return())
      output$select6_info<-renderPrint(invisible())
    }
    if(n>=7){
      output$selectUI_7 <- renderUI(createChoiceButtons(7,facts[[factnum[minIndex+7]]], selectnum[minIndex+7], reactivevals$choiceList[[minIndex+7]]))
      output$select7_info<-renderPrint(cat(paste(recipeMatrix()[as.numeric(names(factnum[minIndex+7])),1:3], collapse=" ")))
    }else{
      output$selectUI_7<-renderUI(return())
      output$select7_info<-renderPrint(invisible())
    }
    if(n>=8){
      output$selectUI_8 <- renderUI(createChoiceButtons(8,facts[[factnum[minIndex+8]]], selectnum[minIndex+8], reactivevals$choiceList[[minIndex+8]]))
      output$select8_info<-renderPrint(cat(paste(recipeMatrix()[as.numeric(names(factnum[minIndex+8])),1:3], collapse=" ")))
    }else{
      output$selectUI_8<-renderUI(return())
      output$select8_info<-renderPrint(invisible())
    }
    if(n>=9){
      output$selectUI_9 <- renderUI(createChoiceButtons(9,facts[[factnum[minIndex+9]]], selectnum[minIndex+9], reactivevals$choiceList[[minIndex+9]]))
      output$select9_info<-renderPrint(cat(paste(recipeMatrix()[as.numeric(names(factnum[minIndex+9])),1:3], collapse=" ")))
    }else{
      output$selectUI_9<-renderUI(return())
      output$select9_info<-renderPrint(invisible())
    }
    if(n>=10){
      output$selectUI_10 <- renderUI(createChoiceButtons(10,facts[[factnum[minIndex+10]]], selectnum[minIndex+10], reactivevals$choiceList[[minIndex+10]]))
      output$select10_info<-renderPrint(cat(paste(recipeMatrix()[as.numeric(names(factnum[minIndex+10])),1:3], collapse=" ")))
    }else{
      output$selectUI_10<-renderUI(return())
      output$select10_info<-renderPrint(invisible())
    }
  })
  
  # item selection
  select1<-observe({
    if(is.null(input$item1)) return()
    isolate({
      page=reactivevals$pageNumber-1
      itemIndex=10*page+1
      reactivevals$itemSelection[itemIndex]=as.numeric(input$item1)
      info=nutriTrackFacts()[[reactivevals$factIndex[itemIndex]]][[reactivevals$itemSelection[itemIndex]]]
    })})
  select2<-observe({
    if(is.null(input$item2)) return()
    isolate({
      page=reactivevals$pageNumber-1
      itemIndex=10*page+2
      reactivevals$itemSelection[itemIndex]=as.numeric(input$item2)
      info=nutriTrackFacts()[[reactivevals$factIndex[itemIndex]]][[reactivevals$itemSelection[itemIndex]]]
    })})
  select3<-observe({
    if(is.null(input$item3)) return()
    isolate({
      page=reactivevals$pageNumber-1
      itemIndex=10*page+3
      reactivevals$itemSelection[itemIndex]=as.numeric(input$item3)
      info=nutriTrackFacts()[[reactivevals$factIndex[itemIndex]]][[reactivevals$itemSelection[itemIndex]]]
    })})
  select4<-observe({
    if(is.null(input$item4)) return()
    isolate({
      page=reactivevals$pageNumber-1
      itemIndex=10*page+4
      reactivevals$itemSelection[itemIndex]=as.numeric(input$item4)
      info=nutriTrackFacts()[[reactivevals$factIndex[itemIndex]]][[reactivevals$itemSelection[itemIndex]]]
    })})
  select5<-observe({
    if(is.null(input$item5)) return()
    isolate({
      page=reactivevals$pageNumber-1
      itemIndex=10*page+5
      reactivevals$itemSelection[itemIndex]=as.numeric(input$item5)
      info=nutriTrackFacts()[[reactivevals$factIndex[itemIndex]]][[reactivevals$itemSelection[itemIndex]]]
    })})
  select6<-observe({
    if(is.null(input$item6)) return()
    isolate({
      page=reactivevals$pageNumber-1
      itemIndex=10*page+6
      reactivevals$itemSelection[itemIndex]=as.numeric(input$item6)
      info=nutriTrackFacts()[[reactivevals$factIndex[itemIndex]]][[reactivevals$itemSelection[itemIndex]]]
    })})
  select7<-observe({
    if(is.null(input$item7)) return()
    isolate({
      page=reactivevals$pageNumber-1
      itemIndex=10*page+7
      reactivevals$itemSelection[itemIndex]=as.numeric(input$item7)
      info=nutriTrackFacts()[[reactivevals$factIndex[itemIndex]]][[reactivevals$itemSelection[itemIndex]]]
    })})
  select8<-observe({
    if(is.null(input$item8)) return()
    isolate({
      page=reactivevals$pageNumber-1
      itemIndex=10*page+8
      reactivevals$itemSelection[itemIndex]=as.numeric(input$item8)
      info=nutriTrackFacts()[[reactivevals$factIndex[itemIndex]]][[reactivevals$itemSelection[itemIndex]]]
    })})
  select9<-observe({
    if(is.null(input$item9)) return()
    isolate({
      page=reactivevals$pageNumber-1
      itemIndex=10*page+9
      reactivevals$itemSelection[itemIndex]=as.numeric(input$item9)
      info=nutriTrackFacts()[[reactivevals$factIndex[itemIndex]]][[reactivevals$itemSelection[itemIndex]]]
    })})
  select10<-observe({
    if(is.null(input$item10)) return()
    isolate({
      page=reactivevals$pageNumber-1
      itemIndex=10*page+10
      reactivevals$itemSelection[itemIndex]=as.numeric(input$item10)
      info=nutriTrackFacts()[[reactivevals$factIndex[itemIndex]]][[reactivevals$itemSelection[itemIndex]]]
    })})
  selectedItems<-reactive({
    if(is.null(nutriTrackFacts()))
      return(0)
    return(reactivevals$itemSelection)
  })
  
  # nutritions table
  nutritionsTable<-reactive({
    if(DEBUG) print("nutritionsTable")
    if (is.null(nutriTrackFacts()))
    {
      output$sortColumns<-renderUI(NULL)
      return(NULL)
    }
    
    res=cbindFacts(nutriTrackFacts(), recipeMatrix(), selectedItems(), reactivevals$factIndex)
    
    # create sort button
    if(ncol(res)>3)
    {
      l=as.list(1:(nrow(res)+1))
      names(l)=c("Entry order", res[,1])
      output$sortColumns<-renderUI(selectInput("sortColumns", label="Sort items by:", choices=l, selected=1, width='30%'))
    }else{
      output$sortColumns<-renderUI(NULL)
    }
    
    return(res)
  })
  
  prevPage<-observe({
    if(DEBUG) print("prevPage")
    input$previousPage
    
    isolate({
      pagenumber=reactivevals$pageNumber
      if(pagenumber>1)
        reactivevals$pageNumber=pagenumber-1
    })
  })
  nextPage<-observe({
    if(DEBUG) print("nextPage")
    input$nextPage
    
    isolate({
      if(length(reactivevals$factIndex)>0){
        pagenumber=reactivevals$pageNumber
        if(pagenumber<ceiling(length(reactivevals$factIndex)/10))
          reactivevals$pageNumber=pagenumber+1
      }})
  })
  
  output$pageNumber<-renderText({
    if(DEBUG) print("output$pageNumber")
    if(ceiling(length(reactivevals$factIndex)/10)<=1)
      return('')
    
    return(sprintf("page: %i/%i, items %i-%i",
                   reactivevals$pageNumber,
                   ceiling(length(reactivevals$factIndex)/10),
                   (reactivevals$pageNumber-1)*10+1,
                   min(length(reactivevals$factIndex),  reactivevals$pageNumber*10)
    )
    )
  })
  
  output$submitButton<-renderUI({
    if(DEBUG) print('output$submitButton')
    
    #if(reactivevals$serverBusy) return()
    return(actionButton("submit", label = "Submit and Check database"))
  })
  output$previousButton<-renderUI({
    if(ceiling(length(reactivevals$factIndex)/10)<=1) return()
    return(actionButton("previousPage", label = "Previous 10 Items"))
  })
  output$nextButton<-renderUI({
    if(ceiling(length(reactivevals$factIndex)/10)<=1) return()
    return(actionButton("nextPage", label = "Next 10 Items"))
  })
  
  output$nutritionTable = DT::renderDataTable({
    if(DEBUG) print("output$nutritionTable")
    if(is.matrix(nutritionsTable()))
    {
      tab=nutritionsTable()
      
      if(!is.null(input$sortColumns))
      {
        columnOrder=as.numeric(input$sortColumns)
        if(columnOrder==1) 
          sortedColumns=3:ncol(tab)
        else
          sortedColumns=sort(as.numeric(tab[columnOrder-1, 3:ncol(tab)]), decreasing=TRUE, index.return=TRUE)$ix+2
      }else{
        sortedColumns=3:ncol(tab)
      }
      
      tmp=data.frame(tab, check.names=FALSE, stringsAsFactors=FALSE)
      # add a row for the unit sizes
      if(grepl("[[:digit:]]", input$servingInput)){
        servings=try(mixedToFloat(input$servingInput))
        if(inherits(servings, "try-error")) servings=reactivevals$servings
        else reactivevals$servings=servings
      }else{
        servings=reactivevals$servings
      }
      
      factIndices=as.numeric(names(reactivevals$factIndex))[sortedColumns-2]
      units=unname(recipeMatrix()[factIndices, 'unit']) #[NEED] this is supposed to put the selected item's units into the quantity row, if there wasnt a standard unit used
      w=which(units=='')
      for(i in w){
        units[i]=nutriTrackFacts()[[factIndices[i]]][[reactivevals$itemSelection[i]]]$nf_serving_size_unit
      }
      
      if(length(servings)>0 & servings>0 & servings!=1){
        factMeasurements=paste(round(as.numeric(recipeMatrix()[factIndices, 'qty'])/servings,2), units)
        
        tmp[,-1]=round(apply(tmp[,-1], 2, as.numeric)/servings,2)
        tmp=rbind(c("Quantity", ifelse(servings>1, paste(servings, 'servings'), paste(servings, 'serving')), factMeasurements), tmp)
      }else{
        factMeasurements=paste(recipeMatrix()[factIndices, 'qty'], units)
        tmp[,-1]=round(apply(tmp[,-1], 2, as.numeric),2)
        tmp=rbind(c("Quantity", '1 serving', factMeasurements), tmp)
      }
      
      colnames(tmp)[-c(1,2)]=shortenString(colnames(tmp)[-c(1,2)], length=10)
      
      return(datatable(tmp[,c(1,2,sortedColumns)],
                       extensions="Scroller",
                       options=list(dom='t', deferRender=TRUE, scrollX=100,
                                    autoWidth=TRUE),
                       rownames=FALSE,
                       selection='single'))
    }else{
      return(NULL)
    }
  })
})
