#------------------------------------------------------------------------
# Run the population model time series projection for a target watershed
#------------------------------------------------------------------------

# Inputs n_years

#------------------------------------------------------------------------
# Part 1: Gather Inputs (Start)

# Gather function inputs
n_years       # Number of years to simulate in the population model
n_replicates  # Number of batch replicates for the population model
dat           # Life history parameter data frame for population model

# Subset the stressor magnitude dataframe to include only the target HUC unit
# (population model is run seperatly for each HUC)



    # Limit user to 500 years
    test_n_years <-
    ifelse(test_n_years > 500, 500, test_n_years)
    test_n_replicates <- input$test_n_replicates
    # Limit user to 100 replicates
    test_n_replicates <-
    ifelse(test_n_replicates > 100, 100, test_n_replicates)
    
    dat <- rv_life_stages$dat
    
    # Gather the environmental stressors for selected HUCs
    click_hucs <- rv_clickedIds$ids
    splits <- lapply(click_hucs, strsplit, "\\|")
    splits <- sapply(splits, head, 1)
    splits <- sapply(splits, head, 1)
    HUC_ids <- as.numeric(splits)
    
    # Get the target HUCs
    CE_df <- rv_stressor_magnitude$sm_dat
    CE_df <-
    CE_df[which(CE_df$HUC_ID %in% HUC_ids), ]
    
    # Thin down stressors to target...
    sr <- list()
    sr$main_sheet <-
    rv_stressor_response$main_sheet
    sr$stressor_names <-
    rv_stressor_response$stressor_names
    sr$sr_dat <- rv_stressor_response$sr_dat
    # Thin down...
    sr$main_sheet <-
    sr$main_sheet[which(sr$main_sheet$Stressors %in% CE_df$Stressor),]
    sr$stressor_names <-
    sr$stressor_names[sr$stressor_names %in% CE_df$Stressor]
    sr$sr_dat <-
    sr$sr_dat[which(names(sr$sr_dat) %in%  CE_df$Stressor)]
    
    # Merge main sheet data
    CE_df$Stressor_cat <- NULL
    CE_df <-
    merge(
        CE_df,
        sr$main_sheet,
        by.x = "Stressor",
        by.y = "Stressors",
        all.x = TRUE
    )
    
    # Stressor Magnitude...
    smw_sample <-
    data.frame(
        HUC_ID = CE_df$HUC_ID,
        NAME = CE_df$NAME,
        Stressor = CE_df$Stressor,
        Stressor_cat = CE_df$Stressor_cat,
        Mean = CE_df$Mean,
        SD = CE_df$SD,
        Distribution = CE_df$Distribution,
        Low_Limit = CE_df$Low_Limit,
        Up_Limit = CE_df$Up_Limit
    )
    
    # browser()
    jm <- JoeModelCE::JoeModel_Run(
    dose = smw_sample,
    sr_wb_dat = sr,
    MC_sims = test_n_replicates,
    adult_sys_cap = FALSE
    )
    
    nrow(jm$sc.dose.df)
    nrow(smw_sample)
    
    # Gather summary at stressor level
    dobj <- jm$sc.dose.df
    
    # add on missing attr columns
    merge_cols <-
    CE_df[, c("Stressor",
                "Life_stages",
                "Parameters",
                "Stressor_cat")]
    
    merge_cols <-
    merge_cols[!(duplicated(merge_cols)),]
    
    m_all <-
    merge(
        merge_cols,
        dobj,
        by.x = "Stressor",
        by.y = "Stressor",
        all.x = TRUE,
        all.y = TRUE
    )
    
    # Fix col names
    colnames(m_all)[colnames(m_all) == "Stressors"] <-
    "Stressor"
    colnames(m_all)[colnames(m_all) == "Life_stages"] <-
    "life_stage"
    colnames(m_all)[colnames(m_all) == "Parameters"] <-
    "parameter"
    colnames(m_all)[colnames(m_all) == "Stressor_cat"] <-
    "Stressor_cat"
    
    # Return cleaned object
    CE_df <- m_all
    
    

# end of isolate
# -------------------------------------------