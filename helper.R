# --------------------------------------------------------------------
# global variables
# --------------------------------------------------------------------
knownVolumes=c("tsp","tbs","cup","pt","qt","gal","floz","liter","ml")
knownVolumes_aliases=tolower(c(teaspoon="tsp", teaspoons="tsp",
                       tbsp="tbs", tablespoon="tbs", tablespoons="tbs",
                       cups="cup",
                       pint="pt", pints="pt",
                       quart="qt", quarts="qt", qrt="qt",
                       gallon="gal", gallons="gal",
                       oz="floz", ounce="floz", ounces="floz",
                       "fluid oz"="floz", 'fl oz'='floz',
                       l="liter", liters="liter",
                       mililiter="ml", mililiters="ml"))

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

# known volumes
mat=matrix(ncol=length(knownVolumes),nrow=length(knownVolumes), dimnames=list(knownVolumes,knownVolumes))
diag(mat)=rep(1,length(knownVolumes))
mat['tsp','tbs']=3
mat['tbs','cup']=16
mat['cup','floz']=1/8
mat['pt','cup']=1/2
mat['qt','pt']=1/2
mat['gal','qt']=1/4
mat['gal','floz']=1/128
mat['floz','liter']=33.814
mat['liter','ml']=1/1000
volumeConversion=createConversionMatrix(mat)

# known weights
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
  x = sub("^ *[^[:digit:]]*\\.+([[:digit:]])", "0.\\1", x)
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

qty_regexpr="^([[:digit:]/\\+\\-\\*\\(\\) ¼½¾⅓⅔⅕⅖⅗⅘⅙⅚⅛⅜⅝⅞]+) ?.*"
unit_regexpr="^([[:digit:]/\\+\\-\\*\\(\\) ]+) ?([^ ]+) ?.*"
item_regexpr="^([[:digit:]/\\+\\-\\*\\(\\) ]+) ?([^ ]+) ?(.*)"
item_regexpr_with_nonstandard_unit="^([[:digit:]/\\+\\-\\*\\(\\) ]+) ?(.*)"
cleanItemInput<-function(item)
{
  item=gsub(" +$", "", gsub("^ +", "", item)) # remove trailing and leading whitespace
  item=sub("^pinch ", "1/8 tsp ", item, ignore.case=TRUE)
  item=gsub("(fl oz)|(fl. oz)|(fluid ounce) ", "floz ", item)
  item=sub("^([[:digit:] ]*)\\(([[:digit:]]+)([^\\)]*)\\)", "(\\1)*\\2 \\3", item) # split up form 1 (8 oz) item -> 1*8 oz item
  item=sub("^([[:digit:] ]*)dozen", "\\1*12", item, ignore.case=TRUE)
  return(item)
}
cleanItemUnit<-function(item)
{
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
  return(tmp)
}
# item (string)
# Returns vector (qty, unit, item) info from input.
getItemInfo <- function(item){
  item=cleanItemInput(item)
  itemInfo=character(3)
  if(grepl(qty_regexpr, item)==FALSE)
    return(c(item, paste(item, ' - Missing quantity.', collapse=''),"(Error)")) # improper input
  itemInfo[1]=try(as.character(mixedToFloat(cleanItemUnit(item))))
  if(inherits(itemInfo[1], "try-error")) # the conversion from character to float did not work
    return(c(item, paste(item, ' - Could not evaluate quantity.', collapse=''),"(Error)")) # unknown quantity
  
  tmp=gsub(unit_regexpr, "\\2", item, ignore.case=TRUE)
  itemInfo[2]=standardizeMeasurement(tmp)
  
  if(itemInfo[2]!=''){ # item uses standard unit
    tmp=gsub(item_regexpr, "\\3", item)
  }else{ # item uses nonstandard unit, which is set to ''
    tmp=gsub(item_regexpr_with_nonstandard_unit, "\\2", item)
  }
  
  # remove excess punctuation and contents between parentheses. [BREAKS] when parentheses are nested
  itemInfo[3]=gsub("[[:punct:]]", "", gsub("\\s*\\(+[^\\)]+\\)+","", tmp))
  
  if(itemInfo[3]=="")
    return(c(item, paste(item, ' - Missing item.', collapse=''),"(Error)")) # no item input
  
  return(itemInfo)
}
# similar to getItemInfo() but does not return error if no item name is present
getMeasurement <- function(item){
  item=cleanItemInput(item)
  itemInfo=character(3)
  if(grepl(qty_regexpr, item)==FALSE)
    return(c(item, paste(item, ' - Missing quantity.', collapse=''),"(Error)")) # improper input
  itemInfo[1]=try(as.character(mixedToFloat(cleanItemUnit(item))))
  if(inherits(itemInfo[1], "try-error")) # the conversion from character to float did not work
    return(c(item, paste(item, ' - Could not evaluate quantity.', collapse=''),"(Error)")) # unknown quantity
  
  tmp=gsub(unit_regexpr, "\\2", item, ignore.case=TRUE)
  itemInfo[2]=standardizeMeasurement(tmp)
  
  if(itemInfo[2]!=''){ # item uses standard unit
    tmp=gsub(item_regexpr, "\\3", item)
  }else{ # item uses nonstandard unit, which is set to ''
    tmp=gsub(item_regexpr_with_nonstandard_unit, "\\2", item)
  }
  
  # remove excess punctuation and contents between parentheses. [BREAKS] when parentheses are nested
  itemInfo[3]=gsub("[[:punct:]]", "", gsub("\\s*\\(+[^\\)]+\\)+","", tmp))
  
  if(itemInfo[3]=="") return(itemInfo) # no item input
  
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

# convert measurements of conversionType to the optimal measurement of same type.
# e.g. for conversionType="volume", "8 tbs" gets converted to "0.5 cup"
optimizeUnits<-function(conversionType, info)
{
  if(conversionType=="weight")
  {
    w=which(info[,2]%in%knownWeights)
    tmp=matrix(as.numeric(info[w,1])/weightConversion[info[w,2],], ncol=ncol(weightConversion))
    colnames(tmp)=colnames(weightConversion)
  }else{
    w=which(info[,2]%in%knownVolumes)
    tmp=matrix(as.numeric(info[w,1])/volumeConversion[info[w,2],], ncol=ncol(volumeConversion))
    colnames(tmp)=colnames(volumeConversion)
  }
  if(length(w)==0) return(info) # error, no units are of the conversion type
  
  opt=apply(tmp,1,function(i) which.min(abs(i-1)))
  optimizedquants=sapply(1:nrow(tmp), function(i) tmp[i,opt[i]])
  info[w,1]=as.character(optimizedquants)
  info[w,2]=colnames(tmp)[opt]
  return(info)
}
# convert info matrix measurements to the targetUnits
# e.g. if info[,2]=c("tsp","tbs"), targetUnits=c("tsp", "cup"), then
# result is info[,1:2] converted to c("tsp", "cup")
convertToTargetUnits<-function(info, targetUnits)
{
  range=1:min(nrow(info),length(targetUnits))
  w=which(info[range, 2]%in%knownWeights & targetUnits[range]%in%knownWeights)
  for(i in w){
    info[i,1]=as.numeric(info[i,1])/weightConversion[info[i,2], targetUnits[i]]
  }
  info[w,2]=targetUnits[w]
  
  w=which(info[range, 2]%in%knownVolumes & targetUnits[range]%in%knownVolumes)
  for(i in w){
    info[i,1]=as.numeric(info[i,1])/volumeConversion[info[i,2], targetUnits[i]]
  }
  info[w,2]=targetUnits[w]
  return(info)
}
# parse recipe and multiply measurements by scale.
# recipe (string with newlines), scale (numeric), optimizeUnits (boolean)
convertItemUnits <- function(recipe, targetUnits, scale=1, optimizeUnits=FALSE){
  if(scale<=0) return(NULL) # error, scale was less than 0
  
  items=unlist(strsplit(recipe, split="\n"))
  items=items[items!='']
  if(length(items)==0) return(NULL) # error check: empty input
  
  info=t(unname(sapply(items, getMeasurement)))
  
  w=info[,3]!="(Error)"
  info[!w,]=cbind(paste("[invalid quantity]", items[!w]), rep("",sum(!w)), rep("", sum(!w)))
  quant=as.numeric(info[w, 1])*scale
  # change the quantities
  info[w,1]=as.character(quant)
  # standardize measurement names for simplicity
  info[,2]=unname(sapply(info[,2], standardizeMeasurement))
  
  # if targetUnits are used
  if(!is.null(targetUnits))
  {
    target=tolower(unlist(strsplit(targetUnits, split="\n")))
    #target=target[target!='']
    info=convertToTargetUnits(info, target)
    info[w,1]=as.character(round(as.numeric(info[w,1]), 3))
    return(info)
  }
  
  # if optimizing units is not used, stop here
  if(optimizeUnits==FALSE){
    info[w,1]=as.character(round(as.numeric(info[w,1]), 3))
    return(info)
  }
  
  # optimize weights
  info=optimizeUnits("weight", info)
  
  info=optimizeUnits("volume", info)
  info[w,1]=as.character(round(as.numeric(info[w,1]),3))
  return(info)
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
# stringSimilarity<-function(string, strings)
# {
#   string=gsub("[[:punct:]]","", string)
#   strings=gsub("[[:punct:]]","",strings)
#   chars=strsplit(c(string, strings), ' ')
#   val=sapply(as.list(chars[-1]), function(x) sum(x%in%chars[[1]])/length(x))
#   return(val)
# }
stringSimilarity<-function(string, strings)
{
  string=gsub("[[:punct:]]","", string)
  strings=gsub("[[:punct:]]","",strings)
  chars=strsplit(c(string, strings), ' ')
  val=sapply(as.list(chars[-1]), function(x) sum(x%in%chars[[1]])/length(x) + sum(chars[[1]]%in%x)/length(chars[[1]]))
  return(val)
}

# rescale an input
rescaleItems<-function(input, targetUnits=NULL, scale, servings_old, newUnits, optimizeUnits=FALSE)
{
  if(grepl("[[:digit:]]", scale)){
    servings=try(mixedToFloat(scale))
    if(inherits(servings, "try-error")) servings=servings_old
  }else{
    return(NULL)
  }
  
  optimized=convertItemUnits(input, targetUnits, optimizeUnits=optimizeUnits, scale=servings)
  if(is.null(optimized)){
    return(c(rescaleText=NULL, res=NULL, servings=servings))
  }
  optimized=matrix(optimized, ncol=3)
  
  w=which(optimized[,3]=="(Error)")
  optimized[w,]=c("\n","","")
  rescaleText=paste(optimized[,1], " ", optimized[,2], " ", optimized[,3], collapse="\n", sep="")
  
  return(c(rescaleText=rescaleText, res=optimized, servings=servings))
}
