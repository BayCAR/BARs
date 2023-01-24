#' The postP.Bfun function
#'
#' @param input  The list of input variables.
#'
#' @param output  The list of output variables.
#'
#' @examples
#'
#' myserver(input, output)
#'
#' @export


myserver = function(input, output) {
  # Define a reactive value to hold the data
  data <- reactiveVal(NULL)

  observeEvent(input$show, {
    # Read data from the project
    dataR <- redcap_read(batch_size=300, redcap_uri=api_url, token=api_token, fields=fields)
    names(dataR$data)=tolower(names(dataR$data))

    data0=dataR$data[substr(dataR$data[,eventcol],1,3)=="ppt",]
    data00=data0=data0[!duplicated(data0[, IDID]),]

    data1=dataR$data[substr(dataR$data[,eventcol],1,1)=="b",]
    data0=data0[,c(IDID, eventcol, covariates)]
    data1=data1[,c(IDID,groupID)]
    data1=data1[data1[, IDID]%in%data0[, IDID],]


    if (all(data0[, IDID]%in%data1[, IDID])) {} else
    {ndata1=data.frame(record_id=data0[, IDID][!data0[, IDID]%in%data1[, IDID]], randomization_group=NA)
    data1=rbind(data1, ndata1)}

    datam0=datam=merge(data0, data1, by=IDID)
    duplicates=duplicated(datam0[,IDID])

    file2 <- input$file2
    ext2 <- tools::file_ext(file2$datapath)
    req(file2)
    validate(need(ext2 == "csv", "Please upload a csv file"))

    new=  read.csv(file2$datapath, header = input$header2)
    names(new)=tolower(names(new))
    if (any(names(new)==groupID)) {} else
    {new$randomization_group=NA}
    if (any(names(datam)==groupID)) {} else
    {if (nrow(datam)>0) {datam$randomization_group=NA}}
    new=new[,c(IDID, covariates, groupID)]


    eventcol.char=nchar(events)
    siteid=new$site
    siteid[siteid=="PBRC"]=1
    siteid[siteid=="Kaiser"]=2
    siteid[siteid=="Dana Farber"]=3

    new[,eventcol]=paste0(substr(events,1, (eventcol.char-1)), siteid)
    datam=datam[,c(IDID,eventcol, covariates, groupID)]
    datam[,groupID][is.na(datam[,groupID])]=""
    datam=datam[datam[,groupID]!="",]

    if (any(new[, IDID]=="")) {
      output$noID= shiny::renderText("Warning! Please input ID for this(these) subjects! ")
    } else
      if (any(!new[, IDID]%in%datam0[, IDID])) { newIDID=new[,IDID]
      output$noID=shiny::renderUI( HTML(as.character(div(style="color: red;", paste("Warning! Some of new subject IDs (", newIDID[!newIDID%in%datam[,IDID]], ") does not exist in the pre-screening ID list. Please double check the IDs!", sep=" "))) ) )
      } else
        if (any(duplicates)) {
          output$noID=shiny::renderUI( HTML(as.character(div(style="color: red;", "Warning! There are duplicated IDs in the existing REDCap database. Clean it up first!")) ) )
        } else
        {     output$noID=shiny::renderText("                                           ")
        output$tablenew <- DT::renderDataTable(new,
                                               options = list(dom = 't',bFilter=0,
                                                              autoWidth = TRUE,  columnDefs = list(list(orderable=FALSE,targets='_all', className = 'dt-center', visible=TRUE, width='100') ),
                                                              buttons = list(
                                                                list(extend = 'copy', title = "New data")),pageLength = 15, lengthChange = FALSE)
        )
        }

    if (any(duplicates)&any(!new[, IDID]%in%datam0[, IDID])) {
      output$noID=shiny::renderUI( HTML(as.character(div(style="color: red;", "Warning! There are duplicated IDs in the existing REDCap database. Also the new subject(s) has(ve) not been screened. Clean up the data first!")) ) )}

    should.be.ran=randomized=rep(NA, nrow(datam))
    output$missing.ran=shiny::renderUI("")
    for (i in 1:nrow(datam))
    {should.be.ran[i]=all(!is.na(datam[i,covariates]))&(is.na(datam[i,groupID]))}
    if (any(should.be.ran)) {
      output$missing.ran=shiny::renderUI( HTML(as.character(div(style="color: red;", "Warning! Some subjects have covariate data, but not been randomized. Clean up the data first!")) ) )}

    output$wrong.ran=shiny::renderUI("")
    for (i in 1:nrow(datam))
    {randomized[i]=any(is.na(datam[i,covariates]))&(!is.na(datam[i,groupID]))}
    if (any(should.be.ran)) {
      output$wrong.ran=shiny::renderUI( HTML(as.character(div(style="color: red;", "Warning! Some subjects have missing covariate data, but been randomized. Clean up the data first!")) ) )}

    output$spce=shiny::renderText("")
  }
  )

  ##############################################################################
  observeEvent(input$action, {
    # Read data from the project
    dataR <- redcap_read(batch_size=300, redcap_uri=api_url, token=api_token, fields=fields)
    data0=dataR$data[substr(dataR$data[,eventcol],1,3)=="ppt",]
    data00=data0=data0[!duplicated(data0[, IDID]),]

    data1=dataR$data[substr(dataR$data[,eventcol],1,1)=="b",]
    data0=data0[,c(IDID, eventcol, covariates)]
    data1=data1[,c(IDID,groupID)]
    data1=data1[data1[, IDID]%in%data0[, IDID],]



    if (all(data0[, IDID]%in%data1[, IDID])) {} else
    {ndata1=data.frame(record_id=data0[, IDID][!data0[, IDID]%in%data1[, IDID]], randomization_group=NA)
    data1=rbind(data1, ndata1)}

    datam=merge(data0, data1, by=IDID)

    if (any(is.na(datam[,c(covariates, groupID)]))) {} else
    {}

    file2 <- input$file2
    ext2 <- tools::file_ext(file2$datapath)
    req(file2)
    validate(need(ext2 == "csv", "Please upload a csv file"))

    new=  read.csv(file2$datapath, header = input$header2)
    names(new)=tolower(names(new))
    if (any(names(new)==groupID)) {} else
    {new$randomization_group=NA}
    if (any(names(datam)==groupID)) {} else
    {if (nrow(datam)>0) {datam$randomization_group=NA}}
    new=new[,c(IDID, covariates, groupID)]


    eventcol.char=nchar(events)
    siteid=new$site
    siteid[siteid=="PBRC"]=1
    siteid[siteid=="Kaiser"]=2
    siteid[siteid=="Dana Farber"]=3

    new[,eventcol]=paste0(substr(events,1, (eventcol.char-1)), siteid)
    datam=datam[,c(IDID,eventcol, covariates, groupID)]
    datam[,groupID][is.na(datam[,groupID])]=""
    datam=datam[datam[,groupID]!="",]


    data=rbind(datam, new)


    dataevent=data[, c(IDID, eventcol)]

    data=data[!duplicated(data[, IDID]),]

    covnames=names(data)
    categorical=covnames[!covnames%in%c(IDID, eventcol, groupID)]
    categorical=categorical[categorical%in%covariates]
    Continuous= input$Continuous
    Continuous=Continuous[Continuous%in%names(data)]
    categorical=categorical[!categorical%in%Continuous]

    if (input$trial=="single-site")
    {categorical=categorical[!categorical%in%c("site")]}

    data$randomization_group[is.na(data$randomization_group)]=""
    planned.sample.size=input$planned.sample.size

    if (input$trial=="multi-site")
    {  categoricalx=c(categorical[!categorical%in%c("site")])
    for (i in 1:length(categoricalx))
    {interi=as.factor(as.character(paste(data$site, data[,categoricalx[i]], sep="-")))

    if (i==1) {inx=data.frame(interi)} else {inx=data.frame(inx, interi)}
    }
    inx=as.data.frame(inx)
    names(inx)=paste0("s",categoricalx)
    data=cbind(data,inx)
    if (length(Continuous)>0)
    {
      nsite=rep(NA, nrow(data))
      nsite[data$site=="PBRC"]=1
      nsite[data$site=="Kaiser"]=2
      nsite[data$site=="Dana Farber"]=3

      for (j in 1:length(Continuous))
      {sContinuous=nsite*data[,Continuous[j]]
      if (j==1) {dContinous=sContinuous} else
      {dContinous=cbind(dContinous, sContinuous)}
      }
      dContinous=as.data.frame(dContinous)
      names(dContinous)=paste0("s", Continuous)
      data=cbind(data, dContinous)
      Continuous=c(Continuous, names(dContinous))
    }
    categorical=names(data)[-which(names(data)%in%c(IDID, eventcol, groupID, Continuous))]
    }

    group.level=c(groupletterID)[1:(as.numeric(substr(input$arms,1,1)))]

    #eids=c(strsplit(input$excludedid, ",")[[1]])
    if (length(input$excludedid)==0) {eids="NO exclusion!"} else
    {excludedid <- input$excludedid
    excld2 <- tools::file_ext(excludedid$datapath)
    validate(need(excld2 == "csv", "Please upload a csv file"))

    ex= as.data.frame(read.csv(excludedid$datapath, header = input$header3))
    names(ex)=IDID

    eids=ex[,IDID]}

    if (any(eids%in%data[, IDID])) {excluded.data=data[data[, IDID]%in%eids,]
    data=data[!data[, IDID]%in%eids,]}
    datab=data

    if (length(Continuous)<1)
    {
      data$randomization_group=FUN.BCAR(data,group.var=groupID, categorical.covariates =  categorical, group.level=group.level,
                                        planned.sample.size = planned.sample.size)

    } else
    {data$randomization_group=FUN.BCAR(data,group.var=groupID, categorical.covariates =  categorical, continuous.covariates = c(Continuous), group.level=group.level,
                                       planned.sample.size = planned.sample.size)
    }

    outdata0=outdata1=outdata=data
    outdata0=outdata[,c(IDID, eventcol, covariates0)]
    outdata1=outdata[,c(IDID, eventcol, groupID)]
    outdata1[, eventcol]=paste0("b", substr(outdata1[, eventcol], 4, 50))

    for (i in 1:length(covariates))
    {wh.name=which(tolower(names(outdata0))==tolower(covariates[i]))
    names(outdata0)[wh.name]=covariates[i]
    }

    redcap_write(outdata0,redcap_uri=api_url, token=api_token, overwrite=TRUE)
    redcap_write(outdata1,redcap_uri=api_url, token=api_token, overwrite=TRUE)


    newlyRandomized=NULL
    newlyRandomized=data[data[, IDID]%in%datab[, IDID][(is.na(datab$randomization_group))|(datab$randomization_group=="")],]

    out=vector("list", 1+length(categorical))
    data$summary=rep(1, nrow(data))
    for (i in 1:(1+length(categorical)))
    {
      dd=table(data[,c("summary", categorical)[i]], data$randomization_group)
      ddd= as.data.frame.matrix(dd)

      ddd$var=row.names(ddd)
      names(ddd)[names(ddd)=="var"]=c("summary", categorical)[i]
      out[[i]]=ddd
    }

    if (input$display=="No")
    {out=vector("list", 1+length(categorical))}

    df_names=paste0("table", 1:(1+length(categorical)))

    for (i in 1:(1+length(categorical)))
    {
      local({i<-i;
      output[[df_names[i]]] = DT::renderDataTable(out[[i]], rownames=FALSE,
                                                  options = list(dom = 't',bFilter=0, autoWidth = TRUE,
                                                                 columnDefs = list( list(targets = as.numeric(substr(input$arms,1,1)), className = "dt-center", createdCell = JS("function(td, cellData, rowData, row, col) {td.style.color = 'blue';}")),
                                                                                    #list(targets = 0                                   , className = "dt-center", createdCell = JS("function(td, cellData, rowData, row, col) {td.style.color = 'white';}")),
                                                                                    list(orderable=FALSE, targets='_all', className = 'dt-center', visible=TRUE, width='30') ),
                                                                 buttons = list(list(extend = 'copy', title = "My custom title 1")), pageLength = 15, lengthChange = FALSE,
                                                                 initComplete = JS(
                                                                   "function(settings, json) {",
                                                                   "$(this.api().table().header()).css({'background-color': 'blue', 'color': 'white'});",
                                                                   "}"))
      )})}


    if (nrow(newlyRandomized)<1)
    {newlyRandomized=NULL
    NOnewlyRandomized="No new group assignment was made. Please make sure this(these) NEW subject has(ve) not been assigned to a group before runing this software."
    output$WNOnewlyRandomized=shiny::renderUI( HTML(as.character(div(style="color: red;", NOnewlyRandomized))))} else
    {
      output$WNOnewlyRandomized=shiny::renderUI( HTML(as.character(div(style="color: red;", ""))))
      output$table99=NULL
      output$table99 <- DT::renderDataTable(newlyRandomized[,c(IDID, groupID)],
                                            options = list(dom = 't',bFilter=0,addClass = 'green-table',  autoWidth = TRUE,
                                                           columnDefs = list(list(orderable=FALSE,targets='_all', className = 'dt-center', visible=TRUE, width='30')),
                                                           initComplete = JS(
                                                             "function(settings, json) {",
                                                             "$(this.api().table().header()).css({'background-color': 'red', 'color': 'white'});",
                                                             "}")
                                            ))

      output$downloadData <- downloadHandler(
        filename = function() { "data.csv" },
        content = function(file) {
          write.csv(newlyRandomized[,c(IDID, groupID)], file, row.names = FALSE)
        }
      )
      }


    if (eids[1]!="NO exclusion!")
    {eex=ex; names(eex)="Excluded IDs"
    output$table98 <- DT::renderDataTable(eex,
                                          options = list(dom = 't',bFilter=0,
                                                         autoWidth = TRUE,  columnDefs = list(list(orderable=FALSE,targets='_all', className = 'dt-center', visible=TRUE, width='30') ),
                                                         buttons = list(
                                                           list(extend = 'copy', title = "My custom title 1")),pageLength = 15, lengthChange = FALSE)
    )}

  }
  )


}
