source('R/api.R')

# Your api key is found in your project settings on the Okala dashboard.
# You can find the Okala dashboard here: https://dashboard.okala.io/

api_key <- 'you_api' # Replace with your actual API key
# Set auth headers appropriately
headers <- auth_headers(api_key,okala_url="https://dev.api.dashboard.okala.io/api/")

# Way to see the project you are pulling and confirm api key is correct
get_project(hdr=headers)

# Get station information for videos in the project and there corresponding IDs
stations <- get_station_info(hdr=headers,datatype="video")

plot_stations(stations)

# Get media lables for a list of sensors
media_labels <- get_media_assets(hdr=headers,
                                 datatype="video",
                                 psrID=stations$project_system_record_id)

# Get project labels for the project
project_camera_labels <- get_project_labels(hdr=headers,labeltype='Camera')

# Get labels from the wider data base using a search term if need, remmove the query parameter if you want all results to come back
labelled_data = getIUCNLabels(hdr=headers,
                              limit=2000,
                              offset=0,
                              search_term = 'Domestic horse')

# Add labels to IUCN database
example_data <- 'data/domestic_animals.json'
example_data <- readLines(example_data)
example_data <- jsonlite::fromJSON(example_data)
example_data <-  jsonlite::fromJSON(example_data)
example_data <- example_data[example_data$species!='NA',]
example_data$class <- example_data$class_
example_data$extant_country_list = NA
example_data$iucn_redlist_status[example_data$iucn_redlist_status=='Extinct In The Wild'] = 'Extinct in the Wild'
example_data$iucn_redlist_status <- gsub('Lower Risk/', '',example_data$iucn_redlist_status)
example_data$iucn_redlist_status <- gsub('Conservation Dependent', 'Not Evaluated',example_data$iucn_redlist_status)


add_IUCN_labels(hdr=headers,labels=example_data,chunksize = 500)

# Update existing labels on the platform

data.frame()
media_labels[1,]
new_label_id <- project_camera_labels[which(project_camera_labels$common_name=='Uneven-toothed Rat'),'label_id']
segment_record_to_change = data.frame(segment_record_id_fk = media_labels[1,'segment_record_id'],label_id_fk = 1)

submission_frame <- data.frame(segment_record_id_fk = segment_record_to_change,label_id_fk = new_label_id, number_of_individuals = 1)


push_new_labels(hdr=headers,submission_records = submission_frame,chunksize=30)












testt <- new_labels(Hdr=headers,
                    Data=DATA,
                    ProjectLabels=PCL,
                    PSRIs=uPSRI,
                    Datatype="video")

testt$label_f = ifelse(is.na(testt$label_f),-1,testt$label_f)

testt$label_fspp = ifelse(testt$label_fspp==-1,'Blank',testt$label_fspp)

test_labels <- testt %>% select(segment_record_id,label_record_id,label_f) %>% rename(segment_record_id_fk = segment_record_id, label_id_fk=label_f)

push_new_labels(headers,submission_records = test_labels,chunksize=30)



new_labels <- function(Hdr,
                       Data,
                       ProjectLabels,
                       PSRIs,
                       Datatype=c("video","audio","image","eDNA")){

  RES <- list()

  for (psri in 1:length(PSRIs)){

    ml <- get_media_assets(hdr=Hdr,
                           datatype=Datatype,
                           psrID=PSRIs[psri])

    ml$label_f <- NA
    ml$label_fspp <- NA

    for (i in 1:nrow(ml)){
      lab <- ml$species[i]
      mfrl <- ml$media_file_reference_location[i]
      ss <- Data[Data$new_vid_id==mfrl,]
      if (nrow(ss)==0){  # If there are no records in the correctly labelled dataset for that specific camera location
        ml$label_f[i] <- NA
        ml$label_fspp[i] <- NA
      }
      else{
        nr <- nrow(ss)
        for (j in 1:nr){
          sss <- ss[j,]
          blank <- ifelse(sss$species_label=="Blank",1,0)
          if (blank==1){   # If correct label is blank
            ml$label_f[i] <- NA
            ml$label_fspp[i] <- -1
          }
          else{
            lab_correct <- sss$latin_name[1]
            lab_correct2 <- strsplit(lab_correct, split="_")
            lab_correct3 <- paste(c(lab_correct2[[1]][1],lab_correct2[[1]][2]),collapse=" ")
            temp <- ProjectLabels$label_id[which(ProjectLabels$label==lab_correct3)]
            if (length(temp)>0){
              ml$label_f[i] <- temp
              ml$label_fspp[i] <- lab_correct3
            }
            else{
              ml$label_f[i] <- NA
            }
          }
        }
      }
    }

    RES[[psri]] <- ml

    print(psri)
  }

  return(do.call(rbind,RES))

}







for (psri in 1:length(PSRIs)){

  ml <- get_media_assets(hdr=headers,
                         datatype="video",
                         psrID=PSRIs[psri])

  ml$label_f <- NA

  for (i in 1:nrow(ml)){
    lab <- ml$species[i]
    mfrl <- ml$media_file_reference_location[i]
    lab_correct <- DATA[DATA$new_vid_id==mfrl,"latin_name"][1]
    if (is.na(lab_correct)==T){
      lab_correct <- "Blank"
      BLANK <- c(BLANK,lab_correct)
      ml$label_f[i] <- NA
    }
    else{
      lab_correct2 <- strsplit(lab_correct, split="_")
      lab_correct3 <- paste(c(lab_correct2[[1]][1],lab_correct2[[1]][2]),collapse=" ")
      temp <- ifelse(is.na(PCLs$label_id[which(PCLs$label==lab_correct3)])==F,
                     PCLs$label_id[which(PCLs$label==lab_correct3)],
                     PCLs$label_id[which(PCLs$label==lab_correct3)])
      if (length(temp)>0){
        ml$label_f[i] <- temp
      }
      else{
        ADD_LABEL <- lab_correct3
        ml$label_f[i] <- NA
      }
    }
  }

  RES[[psri]] <- ml

  print(psri)
}

UPDATED_LABELS <- data.table::rbindlist(RES)



