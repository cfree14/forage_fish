
# Setup
rm(list = ls())
codedir1 <- "code"
codedir2 <- "code/figures"
new.environ <- new.env()

# Build data
sys.source(file.path(codedir1, "Step1_build_fish_data.R"), envir=new.environ)
sys.source(file.path(codedir1, "Step2a_trim_fish_prey_data.R"), envir=new.environ)
sys.source(file.path(codedir1, "Step2b_trim_fish_pred_data.R"), envir=new.environ)
sys.source(file.path(codedir1, "Step3_build_bird_mammal_data.R"), envir=new.environ)
sys.source(file.path(codedir1, "Step4a_link_fish_pred_prey_data.R"), envir=new.environ)
sys.source(file.path(codedir1, "Step4b_link_nonfish_pred_prey_data.R"), envir=new.environ)
sys.source(file.path(codedir1, "Step4c_merge_linked_population_data.R"), envir=new.environ)

# Fit model
sys.source(file.path(codedir1, "Step5a_fit_pella.R"), envir=new.environ)
sys.source(file.path(codedir1, "Step5b_fit_pella_prey_fixed.R"), envir=new.environ)
sys.source(file.path(codedir1, "Step6_assemble_model_results.R"), envir=new.environ)

# Plot results
sys.source(file.path(codedir2, "AppendixD_surplus_production_curves.R"), envir=new.environ)
sys.source(file.path(codedir2, "Table1_aic_values.R"), envir=new.environ)
sys.source(file.path(codedir2, "Fig1_thetas.R"), envir=new.environ)
sys.source(file.path(codedir2, "Fig2_thetas_explanatory_variable.R"), envir=new.environ)
sys.source(file.path(codedir2, "STable1_prey_species_and_stocks.R"), envir=new.environ)
sys.source(file.path(codedir2, "STable2_predator_species_and_stocks.R"), envir=new.environ)




