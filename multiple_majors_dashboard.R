
# Preamble ----------------------------------------------------------------
library(tidyverse) # using tidyverse 2.0.0 -- will not work for tidyr 1.2 and earlier
library(shiny)
library(shinyWidgets)
library(shinyjs)
library(forcats)
library(DT)
library(data.table)

# Either load the data script image (if it has been created) or source the data script
load("multiple_majors_process_data_IMAGE.RData")
# OR
# source("multiple_majors_process_data.R")








primarycol1 <- "#2774AE"
primarycol2 <- '#FFD100'

major_choices <- sort(unique(d2$major_name))
year_choices <- sort(unique(d2$year_leading_summer_7))
status_choices <- c("All Admitted Students", "Freshmen Only", "Transfers Only")



#### Define UI ---------------------------------------------------------------


ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$style(HTML("
      body {
        font-size: 15px;
      }
    .custom-fluid-row {
      padding-bottom: 5vh; /* Adjust the padding as needed */
      padding-right: 25vw;
      padding-left: 3vw;
    }
    "))
  ),
  
  titlePanel("Multiple Majors Awarded"),
  tags$h3("Client: UCLA Chancellor's Office of Data Analytics (CODA)"),
  tags$br(),
  
  
  ### Select Input and Time to Degree Row -------------------------------------

  
  fluidRow(
    column(5, #style = "height: 50vh;", 
           # Select Filters
           wellPanel(
             # Major
             selectizeInput("SELECTED_MAJOR",
                            "Select a Major",
                            choices = c("All Majors", major_choices),
                            selected = "All Majors" 
             ),
             # Year
             virtualSelectInput("SELECTED_YEAR", 
                                "Select Years",
                                choices = rev(year_choices),
                                multiple = TRUE,
                                selected = year_choices
             ),
             # Admit Status
             radioButtons("SELECTED_STATUS",
                          "Select Admission Status",
                          choices = status_choices,
                          selected = "All Admitted Students",
                          inline = TRUE),
             actionButton("GOBUTTON", "Go")
           )),
    
    column(7, #style = "height: 50vh;",
           plotOutput("Time_to_Degree")),
    
    class = "custom-fluid-row"
  ),
  

  ### Major Distribution Row --------------------------------------------------

  
  fluidRow(
    column(width = 9, 
      plotOutput("Major_Distribution_Bar_Graph")
    ),
    column(width = 3, 
      tableOutput("Major_Distribution_Table")
    ),
    class = "custom-fluid-row"
  ),
  

  ### Major Distribution Over Time Row ----------------------------------------

  
  fluidRow(
    column(width = 9,
           plotOutput("Multiple_Majors_Over_Time_Plot")),
    column(width = 3,
           tableOutput("Multiple_Majors_Over_Time_Table")),
    class = "custom-fluid-row"
  ),
  

  ### Major Combos Plot Row ---------------------------------------------------

  
  fluidRow(
    column(width=12,
           plotOutput("MAJOR_COMBOS_PLOT")),
    class = "custom-fluid-row"
  ),
  

  ### Major Combos Table Row --------------------------------------------------

  
  fluidRow(
    column(width=12,
           DT::dataTableOutput("MAJOR_COMBOS_TABLE")),
    class = "custom-fluid-row"
  )
)


#### Define server logic -----------------------------------------------------


server <- function(input, output) {
  my_theme <- theme(
    text = element_text(size = 15),
    #panel.background = element_blank()
  )
  theme_set(my_theme)
  
  click("GOBUTTON") # for initial renderings
  

  ### Creating data_reactive() ------------------------------------------------


  data_reactive <- eventReactive(input$GOBUTTON, { # This function only runs when the go button is clicked 
    # The goal of this function is to return the desired subset of data (based on user input)
    

    ## major_index -------------------------------------------------------------

    
    if (input$SELECTED_MAJOR == "All Majors") {
      major_index <- TRUE
    } else {
     major_index <- major_index_df[, input$SELECTED_MAJOR]
    }
    

    ## double_major_enrolled_index ---------------------------------------------

    
    if (input$SELECTED_MAJOR == "All Majors") {
      double_major_enrolled_index <- vapply(dSMALL$majors_enrolled,
                                            function(x) {
                                              !is.null(x)
                                            }, logical(1))
    } else {
      double_major_enrolled_index <- vapply(dSMALL$majors_enrolled,
                                            function(x) {
                                              if (is.null(x)) {
                                                FALSE
                                              } else {
                                                any(x$major_name == input$SELECTED_MAJOR)
                                              }
                                            }, logical(1))
    }
    

    ## year_index --------------------------------------------------------------

    
    year_index <- dSMALL$year_graduated %in% input$SELECTED_YEAR
    

    ## status_index ------------------------------------------------------------

    
    if (input$SELECTED_STATUS == "All Admitted Students") {
      status_index <- TRUE
    } 
    else if (input$SELECTED_STATUS == "Freshmen Only") {
      status_index <- dSMALL$admit_status_cleaned == "F"
    }
    else if (input$SELECTED_STATUS == "Transfers Only") {
      status_index <- dSMALL$admit_status_cleaned == "A"
    }
    
    

    ## Output - data_reactive() ------------------------------------------------


    acceptable_students_index <- major_index & 
      year_index & 
      status_index 
    
    acceptable_students_index_2 <- double_major_enrolled_index &
      year_index &
      status_index &
      full_enrollment_index
    
    acceptable_students <- dSMALL$stu_id_hash[acceptable_students_index] # these are the hashes of the acceptable students (version 1)
    
    return(list(
      SELECTED_MAJOR = input$SELECTED_MAJOR,
      SELECTED_YEAR = input$SELECTED_YEAR,
      SELECTED_STATUS = input$SELECTED_STATUS,
      d2 = filter(d2, stu_id_hash %in% acceptable_students),
      d2n = filter(d2n, stu_id_hash %in% acceptable_students),
      dretention = filter(dSMALL, acceptable_students_index_2)
    )) 
    # Now `data_reactive()$DATASETNAME` will return your subsetted dataset
    
  })
  
  

  ### Rendering Graphics - Number of Majors Distribution -------------------------------

  
  num_table <- eventReactive(input$GOBUTTON,{
    n <- nrow(data_reactive()$d2n)
    
    num_majors <- sapply((data_reactive()$d2n)$Awarded, function(x) {nrow(x)})
    
    if(length(num_majors) == 0) return (data.frame())
    num_table <- as.data.frame(table(num_majors)) %>% 
      mutate(Prop = Freq / n) 
    num_table
  })
  
  output$Major_Distribution_Bar_Graph <- renderPlot({
    validate(need(nrow(num_table()) > 0, "No data found!"))
    ggplot(data = tail(num_table(), 4),
           mapping = aes(
             x = `Freq`,
             y = `num_majors`
           )) +
      geom_col(color = primarycol1, fill = primarycol1) +
      ggtitle(paste(
        "Number of Majors Awarded"
      )) +
      xlab("Students")+
      ylab("") 
  })
  
  output$Major_Distribution_Table <- renderTable({
    validate(need(nrow(num_table()) > 0, "No data found!"))
    num_table <- num_table()
    num_table$Prop <- paste0(format(round(100 * num_table$Prop, digits = 1), nsmall = 1), "%")
    names(num_table) <- c("Majors Awarded", "Students", "Proportion")
    num_table
  })
  

  ### Rendering Graphics - Proportion of Multiple Majors over Time ------------


  over_time <- eventReactive(input$GOBUTTON, {
    degree_per_academic_year <- data_reactive()$d2
    degree_per_academic_year <- degree_per_academic_year %>% 
      group_by(year_leading_summer_7)  %>% 
      count(major_perc_corrected) %>% 
      mutate(n = n * major_perc_corrected) %>% 
      mutate(freq = n / sum(n)) %>% 
      summarise(
        `One Major` = sum(freq[major_perc_corrected == 1], na.rm = TRUE),
        `Multiple Majors` = sum(freq[major_perc_corrected < 1], na.rm = TRUE),
      ) 
    degree_per_academic_year
  })
  
  output$Multiple_Majors_Over_Time_Plot <- renderPlot({
    validate(need(nrow(over_time()) > 0, "No data found!"))
    data <- pivot_longer(over_time(), 
                         cols = c(`One Major`, `Multiple Majors`), 
                         names_to = "Major Type", values_to = "Proportion")
    ggplot(data, aes(x = year_leading_summer_7, y = Proportion, fill = fct_inorder(`Major Type`))) +
      geom_bar(stat = "identity", position = "stack") +
      scale_fill_manual(values = c(`Multiple Majors` = primarycol2, `One Major` = primarycol1)) +
      scale_y_continuous(limits = c(0, 1)) +
      labs(x = "Academic Year", y = "Proportion", fill = "") +
      theme(legend.position = "bottom") +
      ggtitle("Number of Majors Awarded Over Time")
    
  })
  
  output$Multiple_Majors_Over_Time_Table <- renderTable({
    validate(need(nrow(over_time()) > 0, "No data found!"))
    over_time <- over_time()
    names(over_time)[1] <- "Academic Year"
    over_time$`One Major` <- paste0(format(round(100 * over_time$`One Major`, digits = 1), nsmall = 1), "%")
    over_time$`Multiple Majors` <- paste0(format(round(100 * over_time$`Multiple Majors`, digits = 1), nsmall = 1), "%")
    over_time
  })

  ### Rendering Graphics - Retention Rate -------------------------------------

  
  
  retention_table <- eventReactive(input$GOBUTTON, {
    mult_enrl_env <- new.env()
    mult_enrl_env$count <- 0
    

    ## "All Majors" Case for retention_table() ---------------------------------

    
    if (data_reactive()$SELECTED_MAJOR == "All Majors") {
      double_majors_awarded_index <-
        vapply((data_reactive()$dretention)$majors_awarded,
               function(x) {
                 length(x) > 1
               }, logical(1))
      
      double_majors_enrolled <- lapply((data_reactive()$dretention)$majors_enrolled,
                                       function(x) {
                                         if (is.null(x)) {
                                           return(character(0))
                                         } else {
                                           mult_enrl_env$count <- mult_enrl_env$count + 1
                                           return(unique(x$major_name))
                                         }
                                       }) %>% 
        unlist() %>% 
        table() %>% 
        as.data.frame()
    } 

    ## Specific Majors Case for retention_table() ------------------------------

    
    else {
      double_majors_awarded_index <-
        vapply((data_reactive()$dretention)$majors_awarded,
               function(x) {
                 length(x) > 1 & any(x == data_reactive()$SELECTED_MAJOR)
               }, logical(1))

      double_majors_enrolled <- lapply((data_reactive()$dretention)$majors_enrolled,
                                       function(x) {
                                         if(is.null(x)) {
                                           return(character(0))
                                         } else {
                                           mult_enrl_env$count <- mult_enrl_env$count + 1
                                           x <- group_by(x, term_order_4_apb) %>% 
                                             filter(any(major_name == data_reactive()$SELECTED_MAJOR)) %>% 
                                             ungroup() %>% 
                                             filter(major_name != data_reactive()$SELECTED_MAJOR)
                                           return(unique(x$major_name))
                                         }
                                       }) %>% 
        unlist() %>% 
        table() %>% 
        as.data.frame()
    }
    

    ## Finish retention_table() ------------------------------------------------

  
    mult_grad_all <- sum(double_majors_awarded_index)
    double_majors_awarded <- data_reactive()$dretention[double_majors_awarded_index, "majors_awarded"]
    double_majors_awarded <- as.data.frame(table(unlist(double_majors_awarded)))
    
    if (nrow(double_majors_awarded) == 0) {
      double_majors_awarded <- data.frame(Major = "balls :(", NumberDoubleMajorGraduated = 0)
    } else {
      names(double_majors_awarded) <- c("Major", "NumberDoubleMajorGraduated")
    }  
    
    double_majors_awarded <- filter(double_majors_awarded, Major != data_reactive()$SELECTED_MAJOR)
    
    if (nrow(double_majors_enrolled) == 0) {
      double_majors_enrolled <- data.frame(Major = "balls :(", NumberDoubleMajorEnrolled = 0)
    } else {
      names(double_majors_enrolled) <- c("Major", "NumberDoubleMajorEnrolled")
    }
   
    output <- left_join(double_majors_awarded, double_majors_enrolled, by = "Major") %>% 
      rbind(data.frame(Major = "Any", 
                       NumberDoubleMajorEnrolled = mult_enrl_env$count,
                       NumberDoubleMajorGraduated = mult_grad_all
      )) %>% 
      mutate(RetentionRate = NumberDoubleMajorGraduated/NumberDoubleMajorEnrolled)
    return(output)
  })
  

  ### Rendering Graphics - Major Combos ---------------------------------------

  
  combos_table <- eventReactive(input$GOBUTTON, {
    mult_grad_all <- sum(num_table()[num_table()$num_majors != 1, "Freq"])
    tot_grad_all <- round(mult_grad_all / sum(num_table()[num_table()$num_majors !=1, "Prop"]))
    

    ## "All Majors" Case for combos_table() ------------------------------------


    
    if (data_reactive()$SELECTED_MAJOR == "All Majors") {
      majors_freq <- as.data.frame(table((data_reactive()$d2)$major_name))
      if (nrow(majors_freq) == 0) return (data.frame())
      names(majors_freq) <- c("Major", "TotalGradFreq")
      
      combos_freq <- filter(data_reactive()$d2, major_perc_corrected < 1) %>% 
        select(major_name) %>% 
        table() %>% 
        as.data.frame()
      names(combos_freq) <- c("Major", "Freq")
      
      combos_table <- left_join(combos_freq, majors_freq, by = "Major") %>% 
        rbind(data.frame(Major = "Any", Freq = mult_grad_all, TotalGradFreq = tot_grad_all)) %>% 
        mutate(Prop = `Freq` / TotalGradFreq) %>% 
        arrange(Freq)
      
      output <- left_join(combos_table, retention_table(), by = "Major")
      names(output) <- c("Major", "Freq", "Total Grad Freq", "Prop", "Students Awarded Multiple Majors (Subset)", "Graduating Students Enrolled in Multiple Majors (Subset)", "Retention Rate (Estimate)")
      
      return(output)
    }
    

    ## Specific Major Case for combos_table() ----------------------------------

    
    else {
      n <- nrow(data_reactive()$d2n)
    
      combos <- unlist(lapply((data_reactive()$d2n)$Awarded, 
                              function(x) {x$major_name[x$major_name != data_reactive()$SELECTED_MAJOR]}
      ))
      
      if (length(combos) == 0) return(data.frame())
      
      combos_table <- as.data.frame(table(combos)) %>% 
        rbind(data.frame(combos = "Any", Freq = mult_grad_all)) %>% 
        mutate(Prop = Freq / n) %>% 
        arrange(Freq)
      names(combos_table) <- c("Major", "Freq", "Prop")

      output <- left_join(combos_table, retention_table(), by = "Major")
      names(output) <- c("Major", "Freq", "Prop", "Students Awarded Both Majors (Subset)", "Graduating Students Enrolled in Both Majors (Subset)", "Retention Rate (Estimate)")
      return(output)
      
    }  
  }) 
  
  output$MAJOR_COMBOS_PLOT <- renderPlot({
    validate(need(nrow(combos_table()) > 0, "No data found!"))
    combos_table <- filter(combos_table(), Major != "Any")
    

    ## "All Majors" Case for Major Combos Plot ---------------------------------

    
    if (data_reactive()$SELECTED_MAJOR == "All Majors") {
      combos_table$Major <- str_trunc(as.character(combos_table$Major), 40, "center") # for long majors strings
      
      ggplot(data = tail(combos_table, 20),
             mapping = aes(
               x = Freq,
               y = fct_inorder(Major)
             )) +
        geom_col(fill = primarycol1) +
        ggtitle(
          "Most Common Majors Awarded Multiple Majors"
        ) +
        xlab("Students Awarded Multiple Majors") +
        ylab("")
    } 
    

    ## Specific Major Case for Major Combos Plot -------------------------------

  
    else {
      combos_table$Major <- str_trunc(as.character(combos_table$Major), 40, "center") # for long major names
      
      ggplot(data = tail(combos_table, 20),
             mapping = aes(
               x = Freq,
               y = fct_inorder(Major)
             )) +
        geom_col(fill = rgb(39/255, 116/255, 174/255)) +
        ggtitle("Most Common Awarded Major Combinations",
                subtitle = paste(
                              "For",
                              data_reactive()$SELECTED_MAJOR,
                              "Majors:"
                              )
                ) +
        ylab("") +
        xlab("Students Awarded Both Majors")
    }
  })
  
  output$MAJOR_COMBOS_TABLE <- DT::renderDataTable({
    combos_table <- combos_table()
    validate(need(nrow(combos_table) > 0, "No data found!"))
    
    combos_table <- arrange(combos_table, desc(Freq)) 
    combos_table$Prop <- paste0(format(round(100 * combos_table$Prop, digits = 1), nsmall = 1), "%")
    combos_table$`Retention Rate (Estimate)` <- paste0(format(round(100 * combos_table$`Retention Rate (Estimate)`, digits = 1), nsmall = 1), "%")
    

    ## "All Majors" Case for Major Combos Table --------------------------------

    
    if (data_reactive()$SELECTED_MAJOR == "All Majors") {
      names(combos_table)[1:4] <- c(
        "Major",
        "Students Awarded Multiple Majors",
        "Total Students Awarded Majors",
        "Multiple Major Rate"
      )
      combos_table
    }
    

    ## Specific Major Case for Major Combos Table ------------------------------


    else {
      names(combos_table)[1:3] <- c(
        "Other Major",
        "Students Awarded Both Majors",
        paste(
          "Proportion of All",
          data_reactive()$SELECTED_MAJOR,
          "Degrees Awarded"
        )
      )
      combos_table
    }
  }, options = list(scrollX = TRUE), rownames = FALSE)
  

  ### Rendering Graphics - Time to Degree -------------------------------------


  
  output$Time_to_Degree <- renderPlot({
    validate(need(nrow(num_table()) > 0, "No data found!"))
    if (data_reactive()$SELECTED_STATUS == "All Admitted Students") {
      ttd_table <- ttd_df
    } else {
      if (data_reactive()$SELECTED_STATUS == "Freshmen Only") {
        ttd_table <- filter(ttd_df, admit_status == "Freshmen")
      } else {
        ttd_table <- filter(ttd_df, admit_status == "Transfers")
      }
    }
    ttd_table <- filter(ttd_table, year_leading_summer_7 %in% data_reactive()$SELECTED_YEAR)
    if (data_reactive()$SELECTED_MAJOR != "All Majors") {
      ttd_table <- filter(ttd_table, major_name %in% data_reactive()$SELECTED_MAJOR)
    }

    p <- ttd_table %>% 
      ggplot(aes(x=admit_status, y=avg_terms, fill=major_bin)) +
      scale_fill_manual(values=c(primarycol1, primarycol2)) +
      geom_bar(stat="summary", fun="mean", position="dodge") +
      xlab("Admit Status") +
      ylab("Average Terms") +
      labs(fill = "") + 
      theme(legend.position = "bottom")
    if (data_reactive()$SELECTED_MAJOR == "All Majors") {
      p +
        ggtitle("Average Terms to Graduation",
        subtitle = "(across all Majors and Years)")
    } else {
      p +
        ggtitle("Average Terms to Graduation",
                subtitle = "(across all Years)")
    }

    
  })
  
}


#### Run Shiny App -----------------------------------------------------------


shinyApp(ui = ui, server = server)




