GetFonts <- function()
{
fonts <- c('sans','serif','mono')
return(fonts)
}

GetThemes <- function()
{
	require(ggthemes)
	themelist <- ls("package:ggthemes")[grepl("theme_", ls("package:ggthemes"))]
	if(length(themelist) < 1)
	{
		themelist <- c("theme_base", "theme_calc", "theme_economist", "theme_excel", "theme_solarized", "theme_pander", "theme_solid", "theme_stata", "theme_tufte","theme_wsj")
	}
	return(themelist)
}