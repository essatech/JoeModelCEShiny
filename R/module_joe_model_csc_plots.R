#' Joe Model Cumulative System Capacity Plots UI
#'
#' Define parameters and run the Joe Model
#' 
#' @param id The id for this module
#'
#' @return a tagList containing UI elements
#'
module_joe_model_csc_plots_ui <- function(id) {
  
  ns <- NS(id)
  # Single action button to call modal
  actionButton(ns("open_joe_modal_csc_plots_all"),
                  tags$b("All waterseds"),
                  class="chart-line",
                  width = "100%")

}



#' Joe Model Cumulative System Capacity Plots Server
#'
#' Server and modal content for the Joe model server
#'
#' @param none
#'
#' @return None
#'
module_joe_model_csc_plots_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      ns <- session$ns
      
      print("Calling module_joe_model_csc_plots_server")
      
      #-------------------------------------------------------
      # DISABLE AND ENABLE 
      #------------------------------------------------------- 
      # this module is disabled if the Joe Model rsults are empty
      observe({
        sims <- rv_joe_model_results$sims
        if(length(sims) > 0) {
          shinyjs::enable("open_joe_modal_csc_plots_all")
        } else {
          shinyjs::disable("open_joe_modal_csc_plots_all")
        }
      })



      #-------------------------------------------------------
      # START OF INPUT MODAL UI
      #-------------------------------------------------------      
      # Display the CSC plots for all watersheds
      observeEvent(input$open_joe_modal_csc_plots_all, {
        print("Joe model form click to open ...")
        # Gather a list of all the stessors to build the checkbox list
        showModal(modalDialog(
          title = "Cumulative System Capacity Plots",
          tagList(
            fluidRow(
              shinydashboard::box(
                width = 12,
                plotOutput(ns("csc_plots"),
                    height = "1100px")
              )
            ),
          ),
          easyClose = TRUE,
          size = 'l',
          footer = NULL
        ))
      }) # END OF INPUT MODAL UI
      #-------------------------------------------------------




      #-------------------------------------------------------
      # Generate CSC Joe Model Plots
      #-------------------------------------------------------
      # Generate plots for the latest 
      output$csc_plots <- renderPlot({


     #Script for plotting function 
        #of the Joe model

        library(ggplot2)
        library(plotly)
        library(plyr)

        #Need TruncatedDistributions package developed by 
        #Rob Carnell <bertcarnell@gmail.com>
        #found on R-Forge and not CRAN
        if(!require("TruncatedDistributions",character.only = TRUE)) {
        install.packages("TruncatedDistributions", repos="http://R-Forge.R-project.org")
        }else library(TruncatedDistributions)

        #theme for all plots
        andy_theme<-theme(axis.text.y   = element_text(size=12),
                        axis.text.x   = element_text(size=12),
                        axis.title.y  = element_text(size=14),
                        axis.title.x  = element_text(size=14),
                        panel.background = element_blank(),
                        panel.grid.major = element_blank(), 
                        panel.grid.minor = element_blank(),
                        axis.line = element_line(colour = "black"),
                        panel.border = element_rect(colour = "black", fill=NA, size=0.5))


        ##function to plot a stressor-response relationship
        stres.resp.plot<-function(func.list,min=0,max=100,seq.num=1000,
                                log.boo=F,sims=1,stress.name=""){
        #set alpha to 1 if no Monte Carlo simulations
        if (sims==1) alpha.set=1 else alpha.set=0.1
        #determine x variables in natural and transformed scale
        if (log.boo) {
            x.tran<-seq(min,max,length.out = seq.num)
            x<-exp(x.tran)
        } else{
            x<-seq(min,max,length.out=seq.num)
            x.tran<-x
        }
        #create data.frame to store values
        df<-data.frame(x=as.numeric(),x.tran=as.numeric(),y=as.numeric())
        for (i in 1:length(x.tran)){
            y<-replicate(sims,tbeta_rnd(mn_est=func.list[[1]](x.tran[i]),
                                        sd_est=func.list[[2]](x.tran[i]),
                                        low.limit=func.list[[3]](x.tran[i]),
                                        up.limit=func.list[[4]](x.tran[i])),
                        simplify = "vector")
            df<-rbind(data.frame(x=rep(x[i],sims),x.tran=rep(x.tran[i],sims),y=y),
                    df)
        }
        ggplotly(
            ggplot(data=df,aes(x=x,y=y))+
            geom_point(alpha=alpha.set, 
                        position=position_jitter(width=0,height=0))+
            andy_theme+
            xlab("Stressor value")+
            ylab("System capacity")+
            ggtitle(stress.name)+
            if (log.boo) scale_x_continuous(tran="log") else scale_x_continuous(tran="identity") 
        )
        
        }#end stres.resp.plot()


        #Plot cumulative effect for each HUC
        cum.eff.plot<-function(df){
        #standardise densities to 1 (..scaled..)
        ggplot(data=df)+
            geom_freqpoly(aes(x=CE,y=..ndensity..),size=1,binwidth=0.01)+
            geom_vline(data = ddply(.data = df,.variables = "HUC", summarize, median = median(CE)),mapping = aes(xintercept = median, color = "median"), linetype = "dashed")+
            geom_vline(data = ddply(.data = df,.variables = "HUC", summarize, mean = mean(CE)),mapping = aes(xintercept = mean, color = "mean"), linetype = "dashed")+
            scale_color_manual(name = "statistics", values = c(median = "blue", mean = "red"))+
            #geom_density(aes(y=..scaled..),fill="red",alpha=0.5)+
            scale_x_continuous(limits=c(0,1))+
            facet_wrap(~HUC,ncol = 8)+
            geom_rect(data=data.frame(xmin = 0, xmax = 0.2, ymin = 0, ymax = Inf),
                    aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="red", alpha=0.2)+
            geom_rect(data=data.frame(xmin = 0.2, xmax = 0.5, ymin = 0, ymax = Inf),
                    aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="orange", alpha=0.2)+
            geom_rect(data=data.frame(xmin = 0.5, xmax = 0.7, ymin = 0, ymax = Inf),
                    aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="yellow", alpha=0.2)+
            geom_rect(data=data.frame(xmin = 0.7, xmax = 1, ymin = 0, ymax = Inf),
                    aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill="light green", alpha=0.2)+
            xlab("Cumulative system capacity")+
            ylab("Scaled probability")+
            andy_theme
        }#end cumulative effect plot

        #Plot system capacity for each stressor in a HUC
        huc.plot<-function(df,huc.name){
        #standardise densities to 1 
        
        ggplotly(
            ggplot(data=df,aes(x=sys.cap))+
            geom_freqpoly(aes(y=..ndensity..),size=1,binwidth=0.01)+
            scale_x_continuous(limits=c(0,1))+
            facet_wrap(~stressor)+
            ggtitle(huc.name)+
            xlab("System capacity")+
            ylab("Scaled probability")+
            andy_theme
        )
        }#end huc plot

        #Plot system capacity for each stressor in a HUC
        huc.plot_dash<-function(df,huc.name){
        #standardise densities to 1 
        
        ggplotly(
            ggplot(data=df,aes(x=sys.cap))+
            geom_freqpoly(aes(y=..ndensity..),size=1,binwidth=0.01)+
            scale_x_continuous(limits=c(0,1))+
            facet_wrap(~stressor,ncol = 1)+
            ggtitle(huc.name)+
            xlab("System capacity")+
            ylab("Scaled probability")+
            andy_theme
        )
        }#end huc plot


        ## Stressor summary stat plot (dashboard tab)

        huc.str.boxplot_dash <- function(df){
        ggplotly(
            df %>% 
            ggplot(aes(x = stressor,y = sys.cap, color = `Geo Spatial Code`,fill = `Geo Spatial Code`))+
            geom_boxplot(alpha = 0.6)+
            andy_theme+
            theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
            ylab("System Capacity")
        )
        }


        ##function to plot stressor distributions for given stressors and HUCs
        dose.plot<-function(dose.input,n.val=100){
        df<-data.frame(HUC=as.character(),Stressor=as.character(),
                        Dose=as.numeric(),Density=as.numeric())
        num<-length(dose.input$HUC_ID)
        for (i in 1:num){
            if (dose.input[i,]$Distribution=="lognormal") {
            #if mean < 0 change mean to minimum limit (can't have zero or negative on log scale)
            mn.val<-ifelse(dose.input$Mean[i]<=0,
                            log(dose.input$Low_Limit[i]),log(dose.input$Mean[i]))
            x<-exp(c(mn.val,seq(log(dose.input[i,]$Low_Limit),
                                log(dose.input[i,]$Up_Limit),length.out = n.val)))
            #sD is provided on the log scale, if zero no variance
            if (dose.input[i,]$SD==0){
                dens.val<-rep(0,length(x))
                dens.val[exp(mn.val)==x]<-1 #replace density with 1 at mean value
            }else {
                dens.val<-dtlnorm(x,mn.val,dose.input[i,]$SD,
                                a=dose.input[i,]$Low_Limit,b=dose.input[i,]$Up_Limit)
                dens.val<-dens.val/max(dens.val)
            }
            }else if (dose.input[i,]$Distribution=="normal"){
            mn.val<-dose.input[i,]$Mean
            x<-c(mn.val,seq(dose.input[i,]$Low_Limit,dose.input[i,]$Up_Limit,length.out = n.val))
            #If SD zero no variance
            if (dose.input[i,]$SD==0){
                dens.val<-rep(0,length(x))
                dens.val[mn.val==x]<-1 #replace density with 1 at mean value
            }else {
                dens.val<-dtnorm(x,mn.val,dose.input[i,]$SD,
                                a=dose.input[i,]$Low_Limit,b=dose.input[i,]$Up_Limit)
                dens.val<-dens.val/max(dens.val)
            }
            }
            #use sub.type name if available
            stressor.name<-ifelse(dose.input[i,]$Sub_Type=="NA",dose.input[i,]$Stressor,
                                dose.input[i,]$Sub_Type)
            temp.df<-data.frame(HUC=rep(dose.input[i,]$HUC_ID,n.val+1),
                                Stressor=rep(stressor.name,n.val+1),
                                Dose=x,Density=dens.val)
            
            df<-rbind(df,temp.df)
        }
        
        ggplotly(
            ggplot(data=df,aes(x=Dose,y=Density))+
            geom_area(position="identity", aes(fill = Stressor))+
            geom_line(aes(color = Stressor))+
            geom_point(aes(color = Stressor),size=1)+
            #geom_segment(aes(xend=Dose),yend=0)+
            facet_wrap(~HUC,dir="v")+
            scale_x_continuous(trans = "log",breaks=10^(-4:4))+
            ylab("Scaled probability")+
            xlab("Stressor value")+
            andy_theme
        ) %>% hide_legend()
        } #end dose.plot()


         # Otherwise look at system capacity from Joe mode
          res <- rv_joe_model_results$sims
          res <- res[[length(res)]] # Get the latest result (if multiple)
          # Take the average by each watershed
          df_vals <- res$ce.df


        cum.eff.plot(df_vals)


          
      })



      
   
    }
  )
}