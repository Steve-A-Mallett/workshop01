library(dplyr)
library(shiny)
library(ggplot2)
library(MASS)
library(ggtext)
library(emojifont)
library(tidyr)
library(reshape2)
# Added some code in RStudio..

ui <- fluidPage(
  
  tags$style("h1 { color: #0570b0;}"),
  tags$style("h2 { color: #74a9cf;}"),
  tags$style("h3 { color: black;}"),
  tags$style("h4 { color: black;}"),
  
  tabsetPanel(
    id = "tabset",
    tabPanel(h1("Introduction"),
             fluidRow(
               column(6,
               h2("Make a New Asthma Medicine"),
               h4("Have you ever wondered how statistics could be useful in the real world?"),
               h4("Imagine that scientists have produced a new type of asthma inhaler. 
                  Any new medicine first needs to be tested in a clinical trial to be sure it works and hasn't any serious side effects."),
               h4("In this app we're going to learn how stats can be used to analyse the results of a clinical trial, and make a decision on whether or not the medicine should be given a licence."),
               h4("Are you ready to be a pharmaceutical statistician? Click on 'Lets Go' to continue..."),
               ),
               column(3,
                      imageOutput("photo"))
             )),
    tabPanel(h1("Let's Go!"),
             fluidRow(
               column(3,
                      sliderInput(inputId = "TE",
                                  label = h4("How Well Does the Medicine Work?"),
                                  min = 0,
                                  max = 25,
                                  value = 10,
                                  step=5)),
               column(3,
                      sliderInput(inputId = "N",
                                  label = h4("How Many Patients?"),
                                  min = 50,
                                  max = 500,
                                  value = 100,
                                  step=10)),
               column(3,
                      sliderInput(inputId = "SD",
                                  label = h4("Variability"),
                                  min = 5,
                                  max = 45,
                                  value = 10,
                                  step=5)),
               column(3,
                      actionButton("simulate", h4("Refresh"), class = "btn-block"))
               
             ),
             fluidRow(
               navlistPanel(well = TRUE,
                 id = "panel",
                 tabPanel(h4("Step 1: Get Some Patients"),
                          fluidRow(
                              h4("The first step is to recruit some asthma patients into the study,
                              and randomly assign each patient to one of two groups: the new medicine (orange) or a control group (green).
                                 Patients further to the right on the below figure had more of an improvement in their asthma,"),
                              h4("Q: Do you think there's any difference between patients given the new treatment compared to the control group?
                                 Do all patients in the same group improve by the same amount?"),
    
                              h4("Feel free to experiment with the sliders, to see the effect on the results. 
                                 You can change how well the medicine works (in a real trial, we obviously don't know this, which is why we need to do the study!).
                                 You can also change the number of patients (an important part of a statistician's job is to get this number right)."), 
                              h4(""), 
                              h4("Now let's go on to Step 2 to take a closer look..."),                             
                              
                          plotOutput(outputId = "myPlot1"
                                     , width = "800px", height = "450px"
                                     ))
                          ),
                 tabPanel(h4("Step 2: Look at the Data"),
                          fluidRow(
                            h4("This figure provides allows us to compare the results for the two treatment groups. If, for example, the orange curve is shifted
                               to the right compared to the green curve, it provides evidence that the medicine is effective compared to the control group."),
                            h4(""),
                            h4("Try moving the first slider to the right, simulating a more effective medicine. Do you notice any difference in the curves?"),
                            h4("Now move the slider labelled 'Variability' to the right. What effect does it have on the curves? "),
                            
                           plotOutput(outputId = "myPlot2", width = "800px"
                                     ))
                          ),
                 tabPanel(h4("Step 3: What Does It Tell Us?"), 
                          fluidRow(
                            h4("In this figure, the coloured circle represents the effectiveness of the new medicine in the study. Try hitting the 'refresh' button to
                               simulate re-running the study. Do we always get the same result? You'll notice we get a slightly different result each time."),
                            h4("So what is the 'real' treatment effect (i.e. if we were to give the medicine to all asthma patients in the real world)? In practice we don't know this,
                            but the horizontal coloured line (confidence interval) represents the likely values. So if this line is short, we're more confident in our estimate of the treatment effect,
                            compared to a wider line."),
                            h4("Try experimenting with the three sliders. What happens to the position and width of the confidence interval if we change each slider. 
                               If the confidence interval turns red, it means we've made a false conclusion - which we discuss in Step 4..."),
                          plotOutput(outputId = "myPlot3", width = "800px", height = "150px"
                                     )),
                 ),
                 tabPanel(h4("Step 4:Can We Trust the Results?"),
                          tags$div(
                   h3("Here are some instructions for this page.")
                 ),
                          plotOutput(outputId = "myPlot4", width = "800px", height = "800px"))
                 )
               )
             )
          )
        )
  
server <- function(input, output, session) {
  
  my_data1 <- reactive({
    input$simulate
    N <- as.numeric(input$N)
    sd <- as.numeric(input$SD)
    m_p <- 10
    m_a <- 10 + as.numeric(input$TE)
    sub <- 1:N
    trt <- as.factor(rbinom(N, 1, 0.5))

    data.frame(sub, trt) %>%
      mutate(m = if_else(trt==0, m_p, m_a)) %>%
      mutate(chg = rnorm(N, m, sd))
    })

  my_data2 <- reactive({
    input$simulate
    nsim <- 20
    N <- as.numeric(input$N)
    sd <- rep(as.numeric(input$SD), N*nsim)
    m_p <- 10
    m_a <- 10 + as.numeric(input$TE)
    trt <- as.factor(rbinom(N*nsim, 1, 0.5))
    sim <- crossing(sim=1:nsim, sub=1:N) 
    m = if_else(trt==0, m_p, m_a)
    chg = rnorm(n=sim$sim, mean=m, sd=sd)
    df <- cbind(sim, trt, chg, m)
    summary_stats <- df %>%
      group_by(sim, trt) %>%
      summarise(mean_value = mean(chg),
                sd_value = sd(chg),
                n = n())
    
    ss1 <- dcast(summary_stats, sim ~ trt, value.var="mean_value") %>%
      rename(pla_mn="0", act_mn="1") %>%
      data.frame()
    
    ss2 <- dcast(summary_stats, sim ~ trt, value.var="sd_value") %>%
      rename(pla_sd="0", act_sd="1") %>%
      data.frame()
    
    ss3 <- dcast(summary_stats, sim ~ trt, value.var="n") %>%
      rename(pla_n="0", act_n="1") %>%
      data.frame()
    
    ss4 <- full_join(ss1, ss2, by="sim")
    
    full_join(ss4, ss3, by="sim") %>%
      mutate(diff_mean = act_mn - pla_mn) %>%
      mutate(se_diff = sqrt((act_sd^2/act_n) + (pla_sd^2/pla_n))) %>%
      mutate(ci_lower = diff_mean - qt(0.975, df = act_n + pla_n - 2) * se_diff) %>%
      mutate(ci_upper = diff_mean + qt(0.975, df = act_n + pla_n - 2) * se_diff) %>%
      mutate(error_flag = (input$TE > 0 & ci_lower < 0 & ci_upper > 0 | (input$TE == 0 & (ci_lower > 0 | ci_upper < 0)))) %>%
      mutate(c = if_else(error_flag, "0", "1"))
      
  })

  output$photo <- renderImage({
    list(
      src = "photos/asthma.jpg",
      contentType = "image/jpeg",
      width = 250,
      height = 200
    )
  }, deleteFile = FALSE)
  
     output$myPlot1 <- renderPlot({

    df <- my_data1()
    
     ggplot() +
      scale_x_continuous("Health Score",
                         limits=c(0,60)) +
      scale_y_continuous(" ") +
      scale_color_manual(values=c("#1b9e77", "#d95f02")) +
      theme(plot.title = element_markdown(colour = "#636363",
                                          size = 20),
            panel.background=element_rect(fill="#f0f0f0"),
            panel.grid.major.x=element_line(colour = "#bdbdbd",
                                          linewidth = 0.5,
                                          linetype = 1),
            panel.grid.major.y=element_blank(),
            axis.line.x=element_line(colour = "#bdbdbd",
                                   linewidth = 1,
                                   linetype = 1),
            axis.line.y=element_blank(),
            axis.text.x=element_text(
              colour = "#636363",
              size = 20),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            axis.title=element_text(
              colour = "#636363",
              size = 20)) +
       geom_fontawesome(
         alias = "fa-child",
         x = df$chg,
         y = df$sub,
         size = 10, 
         alpha = 0.6,
         color = ifelse(df$trt == 0, "#1b9e77", "#d95f02")
       ) 
       # labs(title="<b>Disease Outcome in <span style='color:#1b9e77'>Placebo</span> and <span style='color:#d95f02'>Treated</span> Patients</b>") 
       
     }) 
    
    output$myPlot2 <- renderPlot({
      
      ggplot(my_data1(), aes(x = chg, fill = trt, group = trt)) +
        geom_density(show.legend = FALSE,
                   size=0.25, shape=1, alpha=0.7) +
        scale_x_continuous("Health Score",
                           limits=c(0,60)) +
        # scale_y_continuous("",
        #                    limits=c(0, 0.1)) +
        scale_fill_manual(values=c("#1b9e77", "#d95f02")) +
        theme(plot.title = element_markdown(colour = "#636363",
                                            size = 20),
              panel.background=element_rect(fill="#f0f0f0"),
              panel.grid.major.x=element_line(colour = "#636363",
                                            linewidth = 0.5,
                                            linetype = 1),
              panel.grid.major.y=element_blank(),
              axis.line.x=element_line(colour = "#636363",
                                       linewidth = 1,
                                       linetype = 1),
              axis.line.y=element_blank(),
              axis.text.x=element_text(
                colour = "#636363",
                size = 20),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank(),
              axis.title=element_text(
                colour = "#636363",
                size = 20)) 
        # labs(title="<b>Density Plot (<span style='color:#1b9e77'>Placebo</span> and <span style='color:#d95f02'>Treated</span> Groups)</b>") 
        # 
    })

    output$myPlot3 <- renderPlot({
      
      df <- my_data2() %>%
        filter(sim == 1) 

      ggplot(data=df) +
        geom_errorbarh(show.legend = FALSE, aes(xmin = ci_lower, xmax = ci_upper, y = 1, color = c), size = 3, alpha = 0.7) +
        geom_point(show.legend = FALSE, aes(x = diff_mean, y = 1, color = c, fill = c), shape = 21, size = 12, alpha = 0.7) +
        # geom_point(aes(x = as.numeric(input$TE), y = 20), shape=5, size = 8, color = "red") +
        # geom_vline(show.legend = FALSE, aes(xintercept = as.numeric(input$TE)), color = "red", linetype = "dotted", size = 0.5) +
        # geom_vline(aes(xintercept = as.numeric(input$TE)), color = "red", linetype = "dotted", size = 1) +
        geom_vline(aes(xintercept = 0), color = "#636363", size = 1) +       scale_x_continuous("Effect of Treatment", limits=c(-50,70)) +
        scale_y_continuous("",
                           limits=c(0,2)) +
        scale_color_manual(values = c("0" = "red", "1" = "blue")) +
        scale_fill_manual(values = c("0" = "red", "1" = "blue")) +
        # annotate("text", label = "Confidence Interval", x = df$diff_mean, y = 8, color = "#636363", size=5) +
        theme(plot.title = element_markdown(colour = "#636363",
                                            size = 20),
              panel.background=element_rect(fill="#f0f0f0"),
              panel.grid.major.x=element_line(colour = "#f0f0f0",
                                              linewidth = 0.5,
                                              linetype = 1),
              panel.grid.major.y=element_blank(),
              axis.line.x=element_line(colour = "#f0f0f0",
                                       linewidth = 1,
                                       linetype = 1),
              axis.line.y=element_blank(),
              axis.text.x=element_text(
                colour = "#636363",
                size = 20),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank(),
              axis.title=element_text(
                colour = "#636363",
                size = 20)) 
      
    })
    
    output$myPlot4 <- renderPlot({
  
        ggplot(data=my_data2()) +
        geom_errorbarh(show.legend = FALSE, aes(xmin = ci_lower, xmax = ci_upper, y = sim, color = c)) +
        geom_point(show.legend = FALSE, aes(x = diff_mean, y = sim, color = c, fill = c), shape = 21, size = 2, alpha = 0.7) +
        geom_vline(show.legend = FALSE, aes(xintercept = as.numeric(input$TE)), color = "red", linetype = "dotted", size = 0.5) +
        geom_vline(show.legend = FALSE, aes(xintercept = 0), color = "#636363", size = 1) +
        scale_x_continuous("Effect of Treatment", limits=c(-50,70)
        ) +
        scale_y_continuous("",
                           limits=c(0,21)) +
        scale_color_manual(values = c("0" = "red", "1" = "blue")) +
        scale_fill_manual(values = c("0" = "red", "1" = "blue")) +
        theme(plot.title = element_markdown(colour = "#636363",
                                            size = 20),
              panel.background=element_rect(fill="#f0f0f0"),
              panel.grid.major.x=element_line(colour = "#f0f0f0",
                                              linewidth = 0.5,
                                              linetype = 1),
              panel.grid.major.y=element_blank(),
              axis.line.x=element_line(colour = "#f0f0f0",
                                       linewidth = 1,
                                       linetype = 1),
              axis.line.y=element_blank(),
              axis.text.x=element_text(
                colour = "#636363",
                size = 20),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank(),
              axis.title=element_text(
                colour = "#636363",
                size = 20)) 
      
    })
}

shinyApp(ui, server)