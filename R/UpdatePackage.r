
BSkyInstall.packages <- function(pkgs, lib, repos, method)
{
	if(method == 'libcurl')
	{
		install.packages(pkgs =pkgs, lib = lib, repos=repos, method="libcurl")
	}
	else
	{
		install.packages(pkgs =pkgs, lib = lib, repos=repos)
	}
}


