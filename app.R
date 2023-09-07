# Load Packages -----------------------------------------------------------
library(shiny)
library(tidyverse)
library(RColorBrewer)
library(ggrepel)
library(ggpol)

list_parties <- c("A","B","C","D","E","F","G","H","I")
list_colors <- brewer.pal(n = 9, name = "Set1")

rand_votes <- function(N,M){
  x <- runif(N, 5, M)
  (x/sum(x))*100
}

# App UI ------------------------------------------------------------------

ui <- fluidPage(
  titlePanel("Coalition Formation"),
  
  fluidRow(
    column(2,
      sliderInput("num_parties",
                  label = "Number of parties:",
                  min = 3, max = 9, value = 5),
      radioButtons("num_dimensions",
                   label = "Political space",
                   choices = list("one-dimensional" = 1,
                                  "two-dimensional" = 2)),
      h5("Status Quo"),
      checkboxInput("statusquo",
                    label = "Include",
                    value = FALSE),
      actionButton("submitButton", "Submit/Refresh")
      
    ),
    
    column(10,
      plotOutput("positions"),
      plotOutput("parliament")
    )
  ),
  hr(),
  div(
    class = "footer",
    includeHTML("footer.Rhtml")
  ),
  p("Parliament diagram:", a("https://erocoar.github.io/ggpol/", href="https://erocoar.github.io/ggpol/")),
  p("Text labels:", a("https://ggrepel.slowkow.com", href="https://ggrepel.slowkow.com")),
  p(a("View Source", href="https://github.com/robertwelz/CoalitionGame"))
)


# Server logic ------------------------------------------------------------

server <- function(input, output, session) {
  
  sq_x <- reactive(sample(2:10, input$num_parties/input$num_parties))
  sq_y <- reactive(sample(2:10, input$num_parties/input$num_parties))
  
  df <- 
    eventReactive(list(input$submitButton, input$num_parties) , {
      tibble(party = head(c("A","B","C","D","E","F","G","H","I"), input$num_parties),
               colors = brewer.pal(n = input$num_parties, name = "Set1"),
               percent = round(rand_votes(input$num_parties,100),0), 
               seats = percent*5,
               dim_one = sample(1:11, input$num_parties, replace=F),
               dim_two = sample(1:11, input$num_parties, replace=F),
               partylabel = paste(str_c(party, percent, sep = ": "),"%")
  ) %>% slice_head(n = input$num_parties) %>% 
    arrange(desc(dim_one))
    }
    )
  
  output$positions <- renderPlot({
    num_parties <- input$num_parties

    one_dim_plot_1 <- 
      ggplot(data = df()) +
      geom_hline(yintercept = 1, linetype = 1, alpha = .3, color = "black")  +
      geom_text_repel(mapping = aes(x = dim_one, y = 1, label = party), 
                      nudge_y = .3, na.rm = T, min.segment.length = Inf, size = 8,
                      direction = "x",
                      box.padding = .2) +
      geom_point(mapping = aes(x = dim_one, y = 1, fill = party), size = 7, pch = 21, na.rm = T) +
      scale_fill_manual(values = brewer.pal(n = num_parties, name = "Set1")) +
      theme_minimal() +
      labs(title = expression(paste(underline(
        "Party Positions"
      )))) +
      theme(axis.text.y = element_text(color = "black"),
            axis.text.x = element_text(color = "black", size = 20),
            legend.position = "none",
            plot.title = element_text(size = 20, hjust = 0.5)) +
      ylab("") + xlab("") +
      scale_y_discrete(breaks = 2) +
      scale_x_continuous(limits = c(1, 11), breaks = seq(1:11)) +
      coord_fixed(ratio = 1)
    
    one_dim_plot_2 <- 
      ggplot(data = df()) +
      geom_hline(yintercept = 1, linetype = 1, alpha = .3, color = "black")  +
      geom_text_repel(mapping = aes(x = dim_one, y = 1, label = party), 
                      nudge_y = .3, na.rm = T, min.segment.length = Inf, size = 8,
                      direction = "x",
                      box.padding = .2) +
      geom_point(mapping = aes(x = dim_one, y = 1, fill = party), size = 7, pch = 21, na.rm = T) +
      scale_fill_manual(values = brewer.pal(n = num_parties, name = "Set1")) +
      theme_minimal() +
      labs(title = expression(paste(underline(
        "Party Positions"
      )))) +
      theme(axis.text.y = element_text(color = "black"),
            axis.text.x = element_text(color = "black", size = 20),
            legend.position = "none",
            plot.title = element_text(size = 20, hjust = 0.5)) +
      ylab("") + xlab("") +
      scale_y_discrete(breaks = 2) +
      scale_x_continuous(limits = c(1, 11), breaks = seq(1:11)) +
      geom_point(
        mapping = aes(x = sq_x(), y = 1),
        size = 5 , pch = 4
      ) +
      annotate(
        "text", x = sq_x(), y = .7, size = 7, 
        label = "SQ") +
      coord_fixed(ratio = 1)
    
    two_dim_plot_1 <- 
      ggplot(data = df()) +
      geom_hline(yintercept = 6, linetype = 1, alpha = .3, color = "black")  +
      geom_vline(xintercept = 6, linetype = 1, alpha = .3, color = "black")  +
      geom_text_repel(mapping = aes(x = dim_one, y = dim_two, label = party), 
                      na.rm = T, min.segment.length = Inf, size=7,
                      box.padding = .5) +
      geom_point(mapping = aes(x = dim_one, y = dim_two, fill = party), size = 6, pch = 21, na.rm = T) +
      scale_fill_manual(values = brewer.pal(n = num_parties, name = "Set1")) +
      theme_linedraw() +
      labs(title = expression(paste(underline(
        "Party Positions"
      )))) +
      theme(axis.text.y = element_text(color = "black", size = 20),
            axis.text.x = element_text(color = "black", size = 20),
            legend.position = "none",
            plot.title = element_text(size = 20, hjust = 0.5)) +
      ylab("") + xlab("") +
      scale_x_continuous(limits = c(1, 11), breaks = seq(1:11)) +  
      scale_y_continuous(limits = c(1, 11), breaks = seq(1:11)) +
      coord_fixed() 
    
    two_dim_plot_2 <- 
      ggplot(data = df()) +
      geom_hline(yintercept = 6, linetype = 1, alpha = .3, color = "black")  +
      geom_vline(xintercept = 6, linetype = 1, alpha = .3, color = "black")  +
      geom_text_repel(mapping = aes(x = dim_one, y = dim_two, label = party), 
                      na.rm = T, min.segment.length = Inf, size=7,
                      box.padding = .5) +
      geom_point(mapping = aes(x = dim_one, y = dim_two, fill = party), size = 6, pch = 21, na.rm = T) +
      scale_fill_manual(values = brewer.pal(n = num_parties, name = "Set1")) +
      theme_linedraw() +
      labs(title = expression(paste(underline(
        "Party Positions"
      )))) +
      theme(axis.text.y = element_text(color = "black", size = 20),
            axis.text.x = element_text(color = "black", size = 20),
            legend.position = "none",
            plot.title = element_text(size = 20, hjust = 0.5)) +
      ylab("") + xlab("") +
      scale_x_continuous(limits = c(1, 11), breaks = seq(1:11)) +  
      scale_y_continuous(limits = c(1, 11), breaks = seq(1:11)) +
      coord_fixed() +
      geom_point(
        mapping = aes(x = sq_x(), y = sq_y()),
        size = 5 , pch = 4
      ) +
      annotate(
        "text", x = sq_x(), y = sq_y()+0.5, size = 7, 
        label = "SQ")
    
    if (input$num_dimensions == 1 & input$statusquo == FALSE) {print(one_dim_plot_1)}
    if (input$num_dimensions == 1 & input$statusquo == TRUE) {print(one_dim_plot_2)}
    if (input$num_dimensions == 2 & input$statusquo == FALSE) {print(two_dim_plot_1)}
    if (input$num_dimensions == 2 & input$statusquo == TRUE)  {print(two_dim_plot_2)}
    
  })
  output$parliament <- renderPlot({
    num_parties <- input$num_parties
    
    ggplot(data = df()) +
      stat_parliament(mapping = aes(seats = seats, fill = colors), color = "white") +
      scale_fill_manual(values = df()$colors, labels = df()$partylabel) +
      coord_fixed() + 
      theme_void() +
      labs(title = expression(paste(underline(
        "Seats in Parliament"
      )))) +
      theme(plot.title = element_text(size = 20, hjust = 0.5),
            legend.position = 'bottom',
            legend.direction = "horizontal",
            legend.key.size = unit(1.5, 'lines'),
            legend.text = element_text(margin = margin(r = 1, unit = 'cm'), size=20),
            legend.text.align = 0)+
      guides(fill=guide_legend(byrow=TRUE,reverse = TRUE,title=NULL))
    
  })
}


# Run the application 
shinyApp(ui = ui, server = server)
