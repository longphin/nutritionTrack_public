library(shiny)
library(rPython)
library(DT)
python.load("main.py")
source("helper.R")

DEBUG=TRUE

# --------------------------------------------------------------------
# Main function
# --------------------------------------------------------------------
shinyServer(function(input, output, session){
  # --== NUTRITIONS TAB ==-- #
  reactivevals<-reactiveValues()
  # page number that user is currently on
  reactivevals$serverBusy=FALSE
  reactivevals$errorMessages=NULL
  reactivevals$pageNumber=1
  reactivevals$servings=1
  reactivevals$mc_scale=1
  reactivevals$itemSelection=0 # dummy default
  reactivevals$factIndex=NULL # dummy default
  reactivevals$choiceList=NULL # dummy default
  reactivevals$customInputBoxes=logical(10)
  reactivevals$customInputBoxesText=paste(rep("Enter your own nutrition for this item.", 10), 1:10, sep='')
  
  # prerendered UI
  output$check1<-renderUI(checkboxInput("check1", label = "", value = FALSE))
  output$check2<-renderUI(checkboxInput("check2", label = "", value = FALSE))
  output$check3<-renderUI(checkboxInput("check3", label = "", value = FALSE))
  output$check4<-renderUI(checkboxInput("check4", label = "", value = FALSE))
  output$check5<-renderUI(checkboxInput("check5", label = "", value = FALSE))
  output$check6<-renderUI(checkboxInput("check6", label = "", value = FALSE))
  output$check7<-renderUI(checkboxInput("check7", label = "", value = FALSE))
  output$check8<-renderUI(checkboxInput("check8", label = "", value = FALSE))
  output$check9<-renderUI(checkboxInput("check9", label = "", value = FALSE))
  output$check10<-renderUI(checkboxInput("check10", label = "", value = FALSE))
  
  # internal helper functions
  
  output$recipeBox<-renderUI({
    tags$textarea(id="recipeInput", rows=17, cols=110, "", maxlength=750)
  })
  
  # character limit
  output$characterlimit<-renderText(
    {
      if(length(input$recipeInput)==0) return(NULL)
      
      x=unlist(strsplit(input$recipeInput, ''))
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
      reactivevals$serverBusy=TRUE
      reactivevals$errorMessages=NULL
      reactivevals$pageNumber=1
      reactivevals$factIndex=NULL
      
      # ---------------------------------------------
      # Obtain recipe, filter, and nutritional facts.
      # ---------------------------------------------
      # recreate textbox, with fluff in input removed
#       if(DEBUG) print(input$recipeInput)
#       newInput=removeEmptyLines(input$recipeInput)
#       if(newInput!=input$recipeInput){
#         isolate(
#           output$recipeBox<-renderUI({
#             tags$textarea(id="recipeInput", rows=17, cols=110, newInput, maxlength=750)
#           })
#         )
#         return(NULL)
#       }
      
      #inputMatrix=filterInput(newInput)
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
        output$customItemBox_1<-renderUI(return())
        output$customItemBox_2<-renderUI(return())
        output$customItemBox_3<-renderUI(return())
        output$customItemBox_4<-renderUI(return())
        output$customItemBox_5<-renderUI(return())
        output$customItemBox_6<-renderUI(return())
        output$customItemBox_7<-renderUI(return())
        output$customItemBox_8<-renderUI(return())
        output$customItemBox_9<-renderUI(return())
        output$customItemBox_10<-renderUI(return())
        
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
    if(is.null(recipeMatrix())){
      reactivevals$serverBusy=FALSE
      return(NULL)
    }
    isolate({
      # take care of duplicates to reduce database searching
      items=unname(recipeMatrix()[,'item'])
      uniqueItems=NULL
      reactivevals$factIndex=numeric(length(items))
      names(reactivevals$factIndex)=1:length(items)
      for(i in 1:length(items)){
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
        
        if(length(choiceList)==0){
          reactivevals$errorMessages=rbind(reactivevals$errorMessages,
                                           c(i,sprintf("%s - Database does not have matches using those units. Potential matches use: %s", 
                                                       paste(recipeMatrix()[i,1:3], collapse=''),
                                                       paste(unique(item_measurements), collapse=', '))))
          return()
        }
        
        user_item=recipeMatrix()[i,'item']
        related_items_names=sapply(related_items[choiceList], function(x) x$item_name)
        related_brand_names=sapply(related_items[choiceList], function(x) x$brand_name)=="USDA"
        sorted=sort(stringSimilarity(tolower(user_item), tolower(related_items_names))+1*related_brand_names, decreasing=TRUE, index.return=TRUE)$ix
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
        updateTabsetPanel(session, "recipeTab", "Step 2: Database")
        output$errorMessages<-renderPrint(invisible())
      }else{
        sorting=sort(as.integer(reactivevals$errorMessages[,1]), index.return=TRUE)$ix
        reactivevals$errorMessages=matrix(reactivevals$errorMessages[sorting,],ncol=2)
        output$errorMessages<-renderPrint(
          return(cat(paste("line ", reactivevals$errorMessages[,1], ": ", reactivevals$errorMessages[,2], collapse="\n", sep="")))
        )
      }
      
      # reset checkboxes
      reactivevals$itemSelection=sapply(reactivevals$choiceList, function(x) x[1])
      reactivevals$customInputBoxes=logical(max(10,length(reactivevals$itemSelection)))
      #for(tmp in paste0("testText", 1:10)) updateCheckboxInput(session, tmp, label=NULL, value=FALSE) # NEED checkboxes do not display its actual value
      reactivevals$customInputBoxesText=rep("Enter your own nutrition for this item.", length(reactivevals$customInputBoxes))
      reactivevals$serverBusy=FALSE
      return(nutritionFacts)
    })})
  
  # button press: go to Facts table
  observeEvent(input$seeNutrition, updateTabsetPanel(session, "recipeTab", "Step 3: Nutrition"))
  # button press: clear recipe input
  observeEvent(input$clearButton, output$recipeBox<-renderUI({
    tags$textarea(id="recipeInput", rows=17, cols=110, "", maxlength=750)
  }))
  
  { # user selects "custom nutrients"
  observeEvent({
    input$check1
    input$check2
    input$check3
    input$check4
    input$check5
    input$check6
    input$check7
    input$check8
    input$check9
    input$check10
  },ignoreNULL=TRUE, {
    print(sprintf("----- %s -----",Sys.time()))
    print("input$check1")
    print(input$check1)
    facts=nutriTrackFacts()
    page=reactivevals$pageNumber-1
    minIndex=page*10
    factnum=reactivevals$factIndex
    selectnum=reactivevals$itemSelection
    n=min(length(factnum), page*10+10)-minIndex
    
    # only check boxes that have changed
    oldCheckBoxes=reactivevals$customInputBoxes
    reactivevals$customInputBoxes=c(input$check1,input$check2,input$check3,input$check4,input$check5,input$check6,input$check7,input$check8,input$check9,input$check10)
    print(reactivevals$customInputBoxesText)
    print(sprintf("page %i, minIndex %i, n %i, len(choiceList) %i, class(choiceList) %s, len(oldCheckBoxes) %i", page, minIndex, n, length(reactivevals$choiceList), class(reactivevals$choiceList), length(oldCheckBoxes)))
    
    ind=1
#     res=getCustomInput(ind, input$check1, oldCheckBoxes[minIndex+ind], reactivevals$customInputBoxesText[minIndex+ind], facts[[factnum[minIndex+ind]]], selectnum[minIndex+ind], reactivevals$choiceList[[minIndex+ind]], input$testText1, recipeMatrix(), minIndex)
#     getCustomInput<-function(ind, input, oldCheckBox, customBox='', facts, selection, choiceList, customText, recipeMatrix, minIndex)
#     {
#       print(ind)
#       returnval=list()
#       index=minIndex+ind
#     if(oldCheckBox!=input){  
#       if(input){
#         print(paste(ind,"a"))
#         returnval$UI<-renderUI(textInput(paste0("testText",ind), label = NULL, value = customBox))
#         #output$selectUI_1<-renderUI(textInput("testText1", label = NULL, value = customBox))
#       }else{
#         if(n>=ind){
#           print(paste(ind,"b"))
#           print(choiceList)
#           returnval$text<-input[paste0('testText',ind)]
#           #reactivevals$customInputBoxesText[index]=input$testText1
#           returnval$UI<-renderUI(createChoiceButtons(ind, facts[[factnum[index]]], selectnum[index], reactivevals$choiceList[[index]]))
#           #output$selectUI_1 <- renderUI(createChoiceButtons(ind, facts[[factnum[index]]], selectnum[index], reactivevals$choiceList[[index]]))
#           output$select1_info<-renderPrint(cat(paste(recipeMatrix[as.numeric(names(factnum[index])),1:3], collapse=" ")))
#         }else{
#           print("1c")
#           returnval$text<-input[paste0('testText',ind)]
#           #reactivevals$customInputBoxesText[index]=input$testText1
#           returnval$UI<-renderUI(return())
#           output$select1_info<-renderPrint(invisible())
#         } # returnval$UI
#       }}
#    return(returnval)
#     }
#    output$selectUI_1<-res$UI
    print(ind)
    if(oldCheckBoxes[minIndex+ind]!=input$check1){  
      if(input$check1){
        print(paste(ind,"a"))
        val=reactivevals$customInputBoxesText[minIndex+ind]
        output$selectUI_1<-renderUI(textInput("testText1", label = NULL, value = val))
      }else{
        if(n>=ind){
          print(paste(ind,"b"))
          reactivevals$customInputBoxesText[minIndex+ind]=input$testText1
          tmp1=facts[[factnum[minIndex+ind]]]
          tmp2=selectnum[minIndex+ind]
          tmp3=reactivevals$choiceList[[minIndex+ind]]
          output$selectUI_1 <- renderUI(createChoiceButtons(ind, tmp1, tmp2, tmp3))
#          output$select1_info<-renderPrint(cat(paste(recipeMatrix()[as.numeric(names(factnum[minIndex+ind])),1:3], collapse=" ")))
        }else{
          print("1c")
          reactivevals$customInputBoxesText[minIndex+ind]=input$testText1
          output$selectUI_1<-renderUI(return())
          output$select1_info<-renderPrint(invisible())
        }
      }}
    ind=2
    print(ind)
    if(oldCheckBoxes[minIndex+ind]!=input$check2){  
      if(input$check2){
        print(paste(ind,"a"))
        val=reactivevals$customInputBoxesText[minIndex+ind]
        output$selectUI_2<-renderUI(textInput("testText2", label = NULL, value = val))
      }else{
        if(n>=ind){
          print(paste(ind,"b"))
          reactivevals$customInputBoxesText[minIndex+ind]=input$testText2
          tmp=reactivevals$choiceList[[minIndex+ind]]
          output$selectUI_2 <- renderUI(createChoiceButtons(ind, facts[[factnum[minIndex+ind]]], selectnum[minIndex+ind], tmp))
          output$select2_info<-renderPrint(cat(paste(recipeMatrix()[as.numeric(names(factnum[minIndex+ind])),1:3], collapse=" ")))
        }else{
          print("2c")
          reactivevals$customInputBoxesText[minIndex+ind]=input$testText2
          output$selectUI_2<-renderUI(return())
          output$select2_info<-renderPrint(invisible())
        }
      }}
#     ind=3
#     print(ind)
#     if(oldCheckBoxes[minIndex+ind]!=input$check3){  
#       if(input$check3){
#         print(paste(ind,"a"))
#         val=reactivevals$customInputBoxesText[minIndex+ind]
#         output$selectUI_3<-renderUI(textInput("testText3", label = NULL, value = val))
#       }else{
#         if(n>=ind){
#           print(paste(ind,"b"))
#           reactivevals$customInputBoxesText[minIndex+ind]=input$testText3
#           output$selectUI_3 <- renderUI(createChoiceButtons(ind, facts[[factnum[minIndex+ind]]], selectnum[minIndex+ind], reactivevals$choiceList[[minIndex+ind]]))
#           output$select3_info<-renderPrint(cat(paste(recipeMatrix()[as.numeric(names(factnum[minIndex+ind])),1:3], collapse=" ")))
#         }else{
#           reactivevals$customInputBoxesText[minIndex+ind]=input$testText3
#           output$selectUI_3<-renderUI(return())
#           output$select3_info<-renderPrint(invisible())
#         }
#       }}
#     ind=4
#     print(ind)
#     if(oldCheckBoxes[minIndex+ind]!=input$check4){  
#       if(input$check4){
#         print(paste(ind,"a"))
#         val=reactivevals$customInputBoxesText[minIndex+ind]
#         output$selectUI_4<-renderUI(textInput("testText4", label = NULL, value = val))
#       }else{
#         if(n>=ind){
#           print(paste(ind,"b"))
#           reactivevals$customInputBoxesText[minIndex+ind]=input$testText4
#           output$selectUI_4 <- renderUI(createChoiceButtons(ind, facts[[factnum[minIndex+ind]]], selectnum[minIndex+ind], reactivevals$choiceList[[minIndex+ind]]))
#           output$select4_info<-renderPrint(cat(paste(recipeMatrix()[as.numeric(names(factnum[minIndex+ind])),1:3], collapse=" ")))
#         }else{
#           reactivevals$customInputBoxesText[minIndex+ind]=input$testText4
#           output$selectUI_4<-renderUI(return())
#           output$select4_info<-renderPrint(invisible())
#         }
#       }}
#     ind=5
#     print(ind)
#     if(oldCheckBoxes[minIndex+ind]!=input$check5){  
#       if(input$check5){
#         print(paste(ind,"a"))
#         val=reactivevals$customInputBoxesText[minIndex+ind]
#         output$selectUI_5<-renderUI(textInput("testText5", label = NULL, value = val))
#       }else{
#         if(n>=ind){
#           print(paste(ind,"b"))
#           reactivevals$customInputBoxesText[minIndex+ind]=input$testText5
#           output$selectUI_5 <- renderUI(createChoiceButtons(ind, facts[[factnum[minIndex+ind]]], selectnum[minIndex+ind], reactivevals$choiceList[[minIndex+ind]]))
#           output$select5_info<-renderPrint(cat(paste(recipeMatrix()[as.numeric(names(factnum[minIndex+ind])),1:3], collapse=" ")))
#         }else{
#           reactivevals$customInputBoxesText[minIndex+ind]=input$testText5
#           output$selectUI_5<-renderUI(return())
#           output$select5_info<-renderPrint(invisible())
#         }
#       }}
#     ind=6
#     print(ind)
#     if(oldCheckBoxes[minIndex+ind]!=input$check6){  
#       if(input$check6){
#         print(paste(ind,"a"))
#         val=reactivevals$customInputBoxesText[minIndex+ind]
#         output$selectUI_6<-renderUI(textInput("testText6", label = NULL, value = val))
#       }else{
#         if(n>=ind){
#           print(paste(ind,"b"))
#           reactivevals$customInputBoxesText[minIndex+ind]=input$testText6
#           output$selectUI_6 <- renderUI(createChoiceButtons(ind, facts[[factnum[minIndex+ind]]], selectnum[minIndex+ind], reactivevals$choiceList[[minIndex+ind]]))
#           output$select6_info<-renderPrint(cat(paste(recipeMatrix()[as.numeric(names(factnum[minIndex+ind])),1:3], collapse=" ")))
#         }else{
#           reactivevals$customInputBoxesText[minIndex+ind]=input$testText6
#           output$selectUI_6<-renderUI(return())
#           output$select6_info<-renderPrint(invisible())
#         }
#       }}
#     ind=7
#     print(ind)
#     if(oldCheckBoxes[minIndex+ind]!=input$check7){  
#       if(input$check7){
#         print(paste(ind,"a"))
#         val=reactivevals$customInputBoxesText[minIndex+ind]
#         output$selectUI_7<-renderUI(textInput("testText7", label = NULL, value = val))
#       }else{
#         if(n>=ind){
#           print(paste(ind,"b"))
#           reactivevals$customInputBoxesText[minIndex+ind]=input$testText7
#           output$selectUI_7 <- renderUI(createChoiceButtons(ind, facts[[factnum[minIndex+ind]]], selectnum[minIndex+ind], reactivevals$choiceList[[minIndex+ind]]))
#           output$select7_info<-renderPrint(cat(paste(recipeMatrix()[as.numeric(names(factnum[minIndex+ind])),1:3], collapse=" ")))
#         }else{
#           reactivevals$customInputBoxesText[minIndex+ind]=input$testText7
#           output$selectUI_7<-renderUI(return())
#           output$select7_info<-renderPrint(invisible())
#         }
#       }}
#     ind=8
#     print(ind)
#     if(oldCheckBoxes[minIndex+ind]!=input$check8){  
#       if(input$check8){
#         print(paste(ind,"a"))
#         val=reactivevals$customInputBoxesText[minIndex+ind]
#         output$selectUI_8<-renderUI(textInput("testText8", label = NULL, value = val))
#       }else{
#         if(n>=ind){
#           print(paste(ind,"b"))
#           reactivevals$customInputBoxesText[minIndex+ind]=input$testText8
#           output$selectUI_8 <- renderUI(createChoiceButtons(ind, facts[[factnum[minIndex+ind]]], selectnum[minIndex+ind], reactivevals$choiceList[[minIndex+ind]]))
#           output$select8_info<-renderPrint(cat(paste(recipeMatrix()[as.numeric(names(factnum[minIndex+ind])),1:3], collapse=" ")))
#         }else{
#           reactivevals$customInputBoxesText[minIndex+ind]=input$testText8
#           output$selectUI_8<-renderUI(return())
#           output$select8_info<-renderPrint(invisible())
#         }
#       }}
#     ind=9
#     print(ind)
#     if(oldCheckBoxes[minIndex+ind]!=input$check9){  
#       if(input$check9){
#         print(paste(ind,"a"))
#         val=reactivevals$customInputBoxesText[minIndex+ind]
#         output$selectUI_9<-renderUI(textInput("testText9", label = NULL, value = val))
#       }else{
#         if(n>=ind){
#           print(paste(ind,"b"))
#           reactivevals$customInputBoxesText[minIndex+ind]=input$testText9
#           output$selectUI_9 <- renderUI(createChoiceButtons(ind, facts[[factnum[minIndex+ind]]], selectnum[minIndex+ind], reactivevals$choiceList[[minIndex+ind]]))
#           output$select9_info<-renderPrint(cat(paste(recipeMatrix()[as.numeric(names(factnum[minIndex+ind])),1:3], collapse=" ")))
#         }else{
#           reactivevals$customInputBoxesText[minIndex+ind]=input$testText9
#           output$selectUI_9<-renderUI(return())
#           output$select9_info<-renderPrint(invisible())
#         }
#       }}
#     ind=10
#     print(ind)
#     if(oldCheckBoxes[minIndex+ind]!=input$check10){  
#       if(input$check10){
#         print(paste(ind,"a"))
#         val=reactivevals$customInputBoxesText[minIndex+ind]
#         output$selectUI_10<-renderUI(textInput("testText10", label = NULL, value = val))
#       }else{
#         if(n>=ind){
#           print(paste(ind,"b"))
#           reactivevals$customInputBoxesText[minIndex+ind]=input$testText10
#           output$selectUI_10 <- renderUI(createChoiceButtons(ind, facts[[factnum[minIndex+ind]]], selectnum[minIndex+ind], reactivevals$choiceList[[minIndex+ind]]))
#           output$select10_info<-renderPrint(cat(paste(recipeMatrix()[as.numeric(names(factnum[minIndex+ind])),1:3], collapse=" ")))
#         }else{
#           reactivevals$customInputBoxesText[minIndex+ind]=input$testText10
#           output$selectUI_10<-renderUI(return())
#           output$select10_info<-renderPrint(invisible())
#         }
#       }}
    print("end")
  })
  }
  
  # generate buttons for users to choos from database
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
    
    output$check1<-renderUI(checkboxInput("check1", label = "", value = FALSE))
    output$check2<-renderUI(checkboxInput("check2", label = "", value = FALSE))
    output$check3<-renderUI(checkboxInput("check3", label = "", value = FALSE))
    output$check4<-renderUI(checkboxInput("check4", label = "", value = FALSE))
    output$check5<-renderUI(checkboxInput("check5", label = "", value = FALSE))
    output$check6<-renderUI(checkboxInput("check6", label = "", value = FALSE))
    output$check7<-renderUI(checkboxInput("check7", label = "", value = FALSE))
    output$check8<-renderUI(checkboxInput("check8", label = "", value = FALSE))
    output$check9<-renderUI(checkboxInput("check9", label = "", value = FALSE))
    output$check10<-renderUI(checkboxInput("check10", label = "", value = FALSE))
    
    if(n>=1){
      #output$check1<-renderUI(checkboxInput("check1", label = "", value = FALSE))
      output$selectUI_1 <- renderUI(createChoiceButtons(1,facts[[factnum[minIndex+1]]], selectnum[minIndex+1], reactivevals$choiceList[[minIndex+1]]))
      output$select1_info<-renderPrint(cat(paste(recipeMatrix()[as.numeric(names(factnum[minIndex+1])),1:3], collapse=" ")))
    }else{
      output$selectUI_1<-renderUI(return())
      output$select1_info<-renderPrint(invisible())
    }
    if(n>=2){
      #output$check2<-renderUI(checkboxInput("check2", label = "", value = FALSE))
      output$selectUI_2 <- renderUI(createChoiceButtons(2,facts[[factnum[minIndex+2]]], selectnum[minIndex+2], reactivevals$choiceList[[minIndex+2]]))
      output$select2_info<-renderPrint(cat(paste(recipeMatrix()[as.numeric(names(factnum[minIndex+2])),1:3], collapse=" ")))
    }else{
      output$selectUI_2<-renderUI(return())
      output$select2_info<-renderPrint(invisible())
    }
    if(n>=3){
      #output$check3<-renderUI(checkboxInput("check3", label = "", value = FALSE))
      output$selectUI_3 <- renderUI(createChoiceButtons(3,facts[[factnum[minIndex+3]]], selectnum[minIndex+3], reactivevals$choiceList[[minIndex+3]]))
      output$select3_info<-renderPrint(cat(paste(recipeMatrix()[as.numeric(names(factnum[minIndex+3])),1:3], collapse=" ")))
    }else{
      output$selectUI_3<-renderUI(return())
      output$select3_info<-renderPrint(invisible())
    }
    if(n>=4){
      #output$check4<-renderUI(checkboxInput("check4", label = "", value = FALSE))
      output$selectUI_4 <- renderUI(createChoiceButtons(4,facts[[factnum[minIndex+4]]], selectnum[minIndex+4], reactivevals$choiceList[[minIndex+4]]))
      output$select4_info<-renderPrint(cat(paste(recipeMatrix()[as.numeric(names(factnum[minIndex+4])),1:3], collapse=" ")))
    }else{
      output$selectUI_4<-renderUI(return())
      output$select4_info<-renderPrint(invisible())
    }
    if(n>=5){
      #output$check5<-renderUI(checkboxInput("check5", label = "", value = FALSE))
      output$selectUI_5 <- renderUI(createChoiceButtons(5,facts[[factnum[minIndex+5]]], selectnum[minIndex+5], reactivevals$choiceList[[minIndex+5]]))
      output$select5_info<-renderPrint(cat(paste(recipeMatrix()[as.numeric(names(factnum[minIndex+5])),1:3], collapse=" ")))
    }else{
      output$selectUI_5<-renderUI(return())
      output$select5_info<-renderPrint(invisible())
    }
    if(n>=6){
      #output$check6<-renderUI(checkboxInput("check6", label = "", value = FALSE))
      output$selectUI_6 <- renderUI(createChoiceButtons(6,facts[[factnum[minIndex+6]]], selectnum[minIndex+6], reactivevals$choiceList[[minIndex+6]]))
      output$select6_info<-renderPrint(cat(paste(recipeMatrix()[as.numeric(names(factnum[minIndex+6])),1:3], collapse=" ")))
    }else{
      output$selectUI_6<-renderUI(return())
      output$select6_info<-renderPrint(invisible())
    }
    if(n>=7){
      #output$check7<-renderUI(checkboxInput("check7", label = "", value = FALSE))
      output$selectUI_7 <- renderUI(createChoiceButtons(7,facts[[factnum[minIndex+7]]], selectnum[minIndex+7], reactivevals$choiceList[[minIndex+7]]))
      output$select7_info<-renderPrint(cat(paste(recipeMatrix()[as.numeric(names(factnum[minIndex+7])),1:3], collapse=" ")))
    }else{
      output$selectUI_7<-renderUI(return())
      output$select7_info<-renderPrint(invisible())
    }
    if(n>=8){
      #output$check8<-renderUI(checkboxInput("check8", label = "", value = FALSE))
      output$selectUI_8 <- renderUI(createChoiceButtons(8,facts[[factnum[minIndex+8]]], selectnum[minIndex+8], reactivevals$choiceList[[minIndex+8]]))
      output$select8_info<-renderPrint(cat(paste(recipeMatrix()[as.numeric(names(factnum[minIndex+8])),1:3], collapse=" ")))
    }else{
      output$selectUI_8<-renderUI(return())
      output$select8_info<-renderPrint(invisible())
    }
    if(n>=9){
      #output$check9<-renderUI(checkboxInput("check9", label = "", value = FALSE))
      output$selectUI_9 <- renderUI(createChoiceButtons(9,facts[[factnum[minIndex+9]]], selectnum[minIndex+9], reactivevals$choiceList[[minIndex+9]]))
      output$select9_info<-renderPrint(cat(paste(recipeMatrix()[as.numeric(names(factnum[minIndex+9])),1:3], collapse=" ")))
    }else{
      output$selectUI_9<-renderUI(return())
      output$select9_info<-renderPrint(invisible())
    }
    if(n>=10){
      #output$check10<-renderUI(checkboxInput("check10", label = "", value = FALSE))
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
    
    if(reactivevals$serverBusy) return()
    return(actionButton("submit", label = "Go To Step 2"))
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
        tmp=rbind(c("Quantity", ifelse(servings>1, paste('1/', servings, ' servings', sep=''), paste('1/', servings, ' serving', sep='')), factMeasurements), tmp)
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
  
  # --== SCALING TAB ==-- #
  mc_scaled<-observe({
    if(is.null(input$mc_submit)){
      output$mc_rescaledText<-renderPrint(invisible())
      return()
    }
    
    isolate({
      res=rescaleItems(input$mc_input, NULL, input$mc_scale, reactivevals$mc_servings, input$mc_units)
      tmp=unname(res['rescaleText'])
      if(is.null(tmp)){
        output$mc_rescaledText<-renderPrint(invisible())
      }else{
        output$mc_rescaledText<-renderPrint(cat(tmp))
      }
      reactivevals$mc_servings=res['servings']
    })
  })
  mc_scaled<-observe({
    if(is.na(input$mc_submit2)){
      output$mc_rescaledText<-renderPrint(invisible())
      return()
    }
    
    isolate({
      res=rescaleItems(input$mc_input, NULL, input$mc_scale, reactivevals$mc_servings, input$mc_units, TRUE)
      if(is.null(res)){ # error: scale input is invalid
        output$mc_rescaledText<-renderPrint(invisible())
        return()
      }
      
      tmp=unname(res['rescaleText'])
      if(is.na(tmp)){
        output$mc_rescaledText<-renderPrint(invisible())
      }else{
        output$mc_rescaledText<-renderPrint(cat(tmp))
      }
      reactivevals$mc_servings=res['servings']
    })
  })
  # rescale with custom measurements
  mc_scaled<-observe({
    if(is.na(input$mc_submit3)){
      output$mc_rescaledText<-renderPrint(invisible())
      return()
    }
    isolate({
      res=rescaleItems(input$mc_input, input$mc_units, input$mc_scale, reactivevals$mc_servings, input$mc_units)
      if(is.null(res)){ # error: scale input is invalid
        output$mc_rescaledText<-renderPrint(invisible())
        return()
      }
      
      tmp=unname(res['rescaleText'])
      if(is.na(tmp)){
        output$mc_rescaledText<-renderPrint(invisible())
      }else{
        output$mc_rescaledText<-renderPrint(cat(tmp))
      }
      reactivevals$mc_servings=res['servings']
    })
  })
})
