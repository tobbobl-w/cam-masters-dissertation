# Other set up bits 

data.table::update_dev_pkg() # I program on datatable which is only available in the devpkg. 

packages <- utils::installed.packages() # Write out current packages
utils::write.csv(packages, "current_packages.csv")

