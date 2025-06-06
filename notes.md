# Notes to be use to write blog post 

deployment script: rsync -av --delete /home/geir/Home-lab/ sma@sleeper-service:/tmp/home-lab-config/ and ssh sma@sleeper-service "cd /tmp/home-lab-config && sudo nixos-rebuild boot --flake .#sleeper-service"

like the best approach maye we should add a todo for making scripts or research deploy-rs