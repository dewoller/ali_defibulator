
config = config::get( config='one_loc', file='deploy.yml')

"R_files" = fs::dir_ls('R', regexp='\\.R$')
"targets_files" = fs::dir_ls('_targets', recurse=TRUE)


main_file = 'one_row_shiny.R'
root_files=c(  '_targets.R'  )

appFiles = c(R_files, targets_files, main_file, root_files)

rsconnect::deployApp( 
	appPrimaryDoc=main_file,
	appDir=getwd(), 
	appFiles=appFiles,
	appName=config$appName, 
	# appId=config$appId,
	account=config$account,
	server='shinyapps.io',
	lint=FALSE,
	forceUpdate=TRUE)



config = config::get( config='one_loc', file='deploy.yml')
rsconnect::showLogs( 	
	appName=config$appname, 
	account=config$account,
	server="shinyapps.io", entries=500 )




