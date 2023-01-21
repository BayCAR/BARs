#' The postP.Bfun function
#'
#' @param input  The list of input variables.
#'
#' @param output  The list of output variables.
#'
#' @param api_url The REDCap URL
#'
#' @param api_token The token generated for the REDCap URL
#'
#' @param fields  The fields in REDCap to be downloaded
#'
#' @param eventcol  The name of the column in REDCap for trial phase input
#'
#' @param IDID  The name of the column for subject ID input
#'
#' @param covariates  The covariates to be balanced in BayCAR
#'
#' @param groupID The name of the column for group assignment input
#'
#' @param events The name of trial phase
#'
#' @param groupletterID The list of letters for group assignments
#'
#' @examples
#'
#' myserver(input, output)
#'
#' @export


myserver = function(input, output, api_url, api_token, fields, eventcol, IDID, covariates, groupID, events, groupletterID) {
  # Define a reactive value to hold the data
  data <- reactiveVal(NULL)

  observeEvent(input$show, {
    # Read data from the project
    dataR <- redcap_read(batch_size=300, redcap_uri=api_url, token=api_token, fields=fields)
    data0=dataR$data[substr(dataR$data[,eventcol],1,3)=="ppt",]
    duplicates=duplicated(paste0(data0[,IDID], data0[,eventcol]))+0
    dataevent=data0[, c(IDID, eventcol)]

    data1=dataR$data[substr(dataR$data[,eventcol],1,1)=="b",]
    data0=data0[,c(IDID, eventcol, covariates)]
    data1=data1[,c(IDID,groupID)]
    data1=data1[data1[, IDID]%in%data0[, IDID],]

    if (all(data0[, IDID]%in%data1[, IDID])) {} else
    {ndata1=data.frame(record_id=data0[, IDID][!data0[, IDID]%in%data1[, IDID]], randomization_group=NA)
    data1=rbind(data1, ndata1)}
    datam=merge(data0, data1, by=IDID)

    file2 <- input$file2
    ext2 <- tools::file_ext(file2$datapath)
    req(file2)
    validate(need(ext2 == "csv", "Please upload a csv file"))

    new=  read.csv(file2$datapath, header = input$header2)
    new[,IDID]=as.character(new[,IDID])
    new=new[,c(IDID, covariates, groupID)]

    datam=datam[,c(IDID, covariates, groupID)]

    if (any(new[, IDID]=="")) {
      output$noID= shiny::renderText("Warning! Please input ID for this(these) subjects! ")
    } else

      if (any(new[, IDID]%in%datam[, IDID])) {
        output$noID=shiny::renderUI( HTML(as.character(div(style="color: red;", "Warning! The new subject ID(s) has(ve) duplicates in those already randomized. Please double check the IDs!")) ) )
      }

    else
      if (any(duplicates==1)) {
        output$noID=shiny::renderUI( HTML(as.character(div(style="color: red;", "Warning! There are duplicated IDs in the existing REDCap database. Clean up first!")) ) )
      } else
      {     output$noID=shiny::renderText("                                           ")
      output$tablenew <- DT::renderDataTable(new,
                                         options = list(dom = 't',bFilter=0,
                                                        autoWidth = TRUE,  columnDefs = list(list(orderable=FALSE,targets='_all', className = 'dt-center', visible=TRUE, width='100') ),
                                                        buttons = list(
                                                          list(extend = 'copy', title = "New data")),pageLength = 15, lengthChange = FALSE)
      )
      }

    if (any(duplicates==1)&any(new[, IDID]%in%datam[, IDID])) {
      output$noID=shiny::renderUI( HTML(as.character(div(style="color: red;", "Warning! There are duplicated IDs in the existing REDCap database. Also the new subject(s) has(ve) been randomized before. Clean up first!")) ) )}
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

    file2 <- input$file2
    ext2 <- tools::file_ext(file2$datapath)
    req(file2)
    validate(need(ext2 == "csv", "Please upload a csv file"))

    new=  read.csv(file2$datapath, header = input$header2)
    if (any(names(new)==groupID)) {} else
    {new$randomization_group=NA}
    if (any(names(datam)==groupID)) {} else
    {datam$randomization_group=NA}
    new=new[,c(IDID, covariates, groupID)]

    eventcol.char=nchar(events)

    new[,eventcol]=paste0(substr(events,1, (eventcol.char-1)), new$site)
    datam=datam[,c(IDID,eventcol, covariates, groupID)]
    data=rbind(datam, new)
    covariates0=covariates

    dataevent=data[, c(IDID, eventcol)]

    data=data[!duplicated(data[, IDID]),]

    covnames=names(data)
    categorical=covnames[!covnames%in%c(IDID, eventcol, groupID)]
    Continuous= input$Continuous
    Continuous=Continuous[Continuous%in%names(data)]
    categorical=categorical[!categorical%in%Continuous]

    if (input$trial=="single-site")
    {categorical=categorical[!categorical%in%c("site")]}

    data$randomization_group[is.na(data$randomization_group)]=""
    planned.sample.size=input$planned.sample.size

    if (input$trial=="multi-site")
    {  categoricalx=c(categorical[categorical!="site"])
    for (i in 1:length(categoricalx))
    {interi=as.factor(as.character(paste0(data$site, data[,categoricalx[i]])))

    if (i==1) {inx=data.frame(interi)} else {inx=data.frame(inx, interi)}
    }
    inx=as.data.frame(inx)
    names(inx)=paste0("s",categoricalx)
    data=cbind(data,inx)
    if (length(Continuous)>0)
    {covariates0=covariates0[!covariates0%in%Continuous]
      for (j in 1:length(Continuous))
      {sContinuous=as.numeric(data$site)*data[,Continuous[j]]
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
    redcap_write(outdata0,redcap_uri=api_url, token=api_token, overwrite_with_blanks=FALSE, verbose=FALSE)
    redcap_write(outdata1,redcap_uri=api_url, token=api_token, overwrite_with_blanks=FALSE, verbose=FALSE)


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
      # buttons = list( list(extend = 'copy', title = "My custom title 1")),pageLength = 15, lengthChange = FALSE)
    }

    redcap <- redcapConnection(api_url, api_token)

    # Define the recipient, subject, and body of the email
    to <- "shengping.yang@pbrc.edu"
    subject <- paste0("Randomization on ", Sys.Date())
    body <-  paste0("<p>This is a test email sent by REDCap API.</p>", newlyRandomized[,c(IDID, groupID)])
    # Send the email
    send_email(redcap, to = to, subject = subject, body =  body)
    output$email_status <- DT:shiny::renderText("Email sent successfully!")

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
