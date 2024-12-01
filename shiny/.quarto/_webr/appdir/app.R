library(shiny)
library(ggplot2)
library(cowplot)
library(latex2exp)

# Define UI for the app
ui <- fluidPage(
    titlePanel("Power Analysis Visualization"),
    
    sidebarLayout(
        sidebarPanel(
            sliderInput("h0", "Null Hypothesis (H0):", 
                        min = 0, max = 5, value = 0, step = 0.1),
            sliderInput("d", "Effect size:", 
                        min = 0, max = 2, value = 0.5, step = 0.1),
            sliderInput("n", "Sample size:", 
                        min = 1, max = 1000, value = 30, step = 1),
            sliderInput("alpha", "Significance Level (α):", 
                        min = 0.01, max = 1, value = 0.05, step = 0.01),
            checkboxInput("fixAxis", "Fix x-axis limits?", value = FALSE),
            conditionalPanel(
                condition = "input.fixAxis == true",
                numericInput("xMin", "X-axis Minimum:", value = -1, step = 0.1),
                numericInput("xMax", "X-axis Maximum:", value = 1, step = 0.1)
            ),
        ),
        
        mainPanel(
            h3("Power Analysis Plot"),
            plotOutput("powerPlot")
        )
    )
)

# Define server logic
server <- function(input, output) {
    # Function to create the power plot
    plot_inference <- function(d = 0,
                               n,
                               h0 = 0,
                               alpha = 0.05,
                               p.value = FALSE,
                               do = NULL,
                               lb = NULL,
                               ub = NULL){
        h1 <- h0 + d
        s <- 1
        se0 <- sqrt(s/n + s/n)
        se1 <- sqrt(s/n + s/n)
        zc <- abs(qnorm(alpha/2, h0, se0))
        md <- (h0 + h1) / 2
        if(is.null(lb)) lb <- md - se0*5
        if(is.null(ub)) ub <- md + se0*5
        
        quiet <- function(x) {
            suppressMessages(
                suppressWarnings(
                    x
                )
            )
        }
        
        dnorm0 <- function(x, mean = h0, sd = se0){
            dnorm(x, mean, sd)
        }
        
        # null hypothesis plot
        ph0 <- ggplot(data = data.frame(x = c(lb, ub))) +
            stat_function(aes(x = x),
                          fun = dnorm0,
            ) +
            geom_area(stat = "function",
                      aes(fill = "Type-1 Error α"),
                      fun = dnorm0,
                      xlim = c(zc, ub),
                      alpha = 0.5) +
            geom_area(stat = "function",
                      fun = dnorm0,
                      xlim = c(lb, -zc),
                      fill = scales::alpha("red", 0.5),
                      alpha = 0.5) +
            ggtitle(latex2exp::TeX(sprintf("$H_0: \\; d = %s$", h0))) +
            theme_minimal(base_size = 15) +
            theme(legend.title = element_blank(),
                  axis.title.y = element_blank(),
                  axis.text.y = element_blank(),
                  axis.ticks.y = element_blank(),
                  legend.position = c(0.85, 0.90),
                  plot.title = element_text(hjust = 0.5)) +
            xlab(latex2exp::TeX("$t(x)_o$")) +
            scale_fill_manual(values = c(scales::alpha("red", 0.5)),
                              labels = latex2exp::TeX("Type-1 error $\\alpha$")) +
            geom_vline(xintercept = c(-zc, zc), lty = "dashed") +
            geom_segment(aes(x = h0, xend = h0, y = 0, yend = dnorm0(h0)))
        # alternative hypothesis plot
        
        dnorm1 <- function(x, mean = h1, sd = se1){
            dnorm(x, mean, sd)
        }
        
        ph1 <- ggplot(data = data.frame(x = c(lb, ub))) +
            stat_function(aes(x = x),
                          fun = dnorm1) +
            geom_area(stat = "function",
                      aes(fill = "Type-2 Error (β)"),
                      args = list(mean = h1),
                      fun = dnorm1,
                      xlim = c(-zc, zc),
                      alpha = 0.5) +
            geom_area(stat = "function",
                      args = list(mean = h1),
                      fun = dnorm1,
                      aes(fill = "Power (1 - β)"),
                      xlim = c(zc, ub),
                      alpha = 0.5) +
            geom_area(stat = "function",
                      args = list(mean = h1),
                      fun = dnorm1,
                      xlim = c(lb, -zc),
                      fill = "dodgerblue",
                      alpha = 0.5) +
            scale_fill_manual(values = c("dodgerblue", "black"),
                              labels = c(latex2exp::TeX("Power ($1 - \\beta$)"),
                                         latex2exp::TeX("Type-2 Error ($\\beta$)"))) +
            geom_vline(xintercept = c(-zc, zc), linetype = "dashed") +
            theme_minimal(base_size = 15) +
            theme(legend.title = element_blank(),
                  plot.title = element_text(hjust = 0.5),
                  axis.title.y = element_blank(),
                  axis.text.y = element_blank(),
                  legend.position = c(0.85, 0.90)) +
            ggtitle(latex2exp::TeX(sprintf("$H_1: \\; d \\neq 0$ ($d = %s$)", d))) +
            geom_segment(aes(x = h1, xend = h1, y = 0, yend = dnorm1(h1))) +
            xlab("t(x)")
        if(p.value){
            if(is.null(do)){
                stop("When p.value is TRUE, the observed statistics to need to be provided!")
            }
            ph0 <- ph0 +
                geom_area(stat = "function",
                          fun = dnorm0,
                          aes(fill = "p value"),
                          xlim = c(do, ub),
                          alpha = 1) +
                scale_fill_manual(values = c("purple", scales::alpha("red", 0.5))) +
                geom_vline(xintercept = do) +
                annotate("label", x = do, y = dnorm(0)/2, label = latex2exp::TeX("$t(x)_o$"))
            quiet(print(ph0))
        } else{
            ph0 <- ph0 +
                theme(axis.text.x = element_blank(),
                      axis.title.x = element_blank())
            quiet(
                cowplot::plot_grid(ph0, ph1, nrow = 2, align = "hv")
            )
        }
    }
    
    # Render the plot
    output$powerPlot <- renderPlot({
        if(input$fixAxis){
            lb <- input$xMin
            ub <- input$xMax
        }else{
            lb <- ub <- NULL
        }
        plot_inference(h0 = input$h0, d = input$d, n = input$n, alpha = input$alpha,
                       lb = lb, ub = ub)
    },
    height = 500, width = 700)
}

# Run the application
shinyApp(ui = ui, server = server)
