{
  lib,
  buildNpmPackage,
  fetchurl,
  nodejs,
}:

buildNpmPackage rec {
  pname = "task-master-ai";
  version = "0.16.2";

  src = fetchurl {
    url = "https://github.com/eyaltoledano/claude-task-master/archive/refs/tags/v${version}.tar.gz";
    hash = "sha256-hYXIvhXx1qJefnEbsllwm7TATPU8ihVV6XchaMjTACQ=";
  };

  npmDepsHash = "sha256-WjPFg/jYTbxrKNzTyqb6e0Z+PLPg6O2k8LBIELwozo8=";

  dontNpmBuild = true;

  # buildInputs = [ nodejs ]; # buildNpmPackage usually brings in nodejs

  meta = with lib; {
    description = "Claude Task Master AI - An intelligent task management and project breakdown tool";
    homepage = "https://github.com/eyaltoledano/claude-task-master";
    license = licenses.mit;
    maintainers = [ ]; # Add your GitHub username if you want
    platforms = platforms.all;
    mainProgram = "task-master-ai";
  };
}
