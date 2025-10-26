
# Just list all recipes by default
default:
  just --list


run:
  stack run expenses-manager-server -- --user test --verbose

run-user-dir:
  stack run expenses-manager-server -- --user test --verbose --app-dir /home/dc/.local/share/expenses-manager

test:
  stack test --fast

test-filter filter:
  stack test --fast --test-arguments='--pattern "{{filter}}"' --file-watch

# Run stack tests and accept any changes to golden files.
test-accept:
  stack test --fast --test-arguments='--accept'

nix-show:
  nix flake show --allow-import-from-derivation

# Build and run the application
nix-run:
   nix run .#expenses-manager-service

backup:
  sudo anacron -fnd expenses-backup

copy-db-to-repo:
  cp /home/dc/.local/share/expenses-manager/expenses.db db/db

update-haskell-nix:
  nix flake update haskellNix

# Usage: just run-migration 99 true
[confirm]
run-migration idx run_backup:
  echo {{idx}}

  if [ "{{run_backup}}" = "true" ]; then \
    ./scripts/backup.sh "_before_migration_{{idx}}"; \
  fi

  stack run expenses:exe:db-migrations -- {{idx}}


# Setup a cloudflare quick tunnel to test the local server
quick-tunnel:
  cloudflared tunnel --url http://localhost:8081

# Bank access must be renewed every 3 months
renew-logins:
  ./scripts/expenses-reset.sh

##############################################################
# Raspberry Pi
##############################################################

# Address of the Raspberry Pi machine
remote := "192.168.1.244"

# Cross-compile to 64-bit ARM
rpi-build:
  nix build .#expenses-manager-bundle-rpi

# Deploy the built package to the Raspberry Pi,
# updates the nix profile,
# and restarts the systemd service.
rpi-deploy:
  #!/usr/bin/env bash
  set -euxo pipefail
  # Use a shebang to be able to set variables
  # See: https://just.systems/man/en/setting-variables-in-a-recipe.html

  # NOTE:
  #   * For reproducibility, nix sets the `modified` timestamp of all files in the store to the epoch (1970-01-01).
  #   * This means the browser will cache index.html and other assets forever.
  #     The server will first serve the file with: 'last-modified: Thu, 01 Jan 1970 00:00:01 GMT',
  #     The browser will then issue requests with 'If-Modified-Since: Thu, 01 Jan 1970 00:00:01 GMT',
  #     and the server will always respond with '304 Not Modified'.
  #   * To prevent these files from being cached forever, we need to "touch" the files
  #     so that they have the current timestamp.
  #   * Links:
  #       https://nix.dev/manual/nix/2.18/language/derivations
  #       https://github.com/NixOS/nixpkgs/issues/196804
  #       https://www.reddit.com/r/NixOS/comments/10p446h/file_date_of_111970_in_nix_store_folder/
  BUNDLE_PATH=$(nix build .#expenses-manager-bundle-rpi --print-out-paths --no-link)
  nix copy $BUNDLE_PATH --to ssh://{{remote}}
  ssh {{remote}} -- "\
    nix profile remove expenses-manager-bundle-rpi ;\
    nix profile add $BUNDLE_PATH ;\
    sudo find $BUNDLE_PATH/bin/ -type f -exec touch {} + ;\
    sudo systemctl restart expenses-manager ;\
    sudo systemctl restart expenses-manager-demo ;\
    "

rpi-setup-service env_file:
  # `scp` can't use sudo, so we have to copy the file to a location where we have permission,
  # and then use `ssh` to move it to the correct directory.`
  scp ./resources/prod/expenses-manager-rpi.service     dc@{{remote}}:/home/dc/.local/share/expenses-manager/expenses-manager.service
  scp {{env_file}}                                      dc@{{remote}}:/home/dc/.local/share/expenses-manager/override.conf
  scp ./resources/prod/config.yaml                      dc@{{remote}}:/home/dc/.local/share/expenses-manager/config.yaml
  ssh {{remote}} -- " \
    sudo mv /home/dc/.local/share/expenses-manager/expenses-manager.service   /etc/systemd/system/expenses-manager.service;  \
    sudo mkdir -p                                                             /etc/systemd/system/expenses-manager.service.d ; \
    sudo mv /home/dc/.local/share/expenses-manager/override.conf              /etc/systemd/system/expenses-manager.service.d/override.conf ; \
    sudo systemctl daemon-reload ; \
    sudo systemctl enable expenses-manager ; \
    sudo systemctl restart expenses-manager ; "

rpi-logs:
  ssh {{remote}} -- journalctl -u expenses-manager --follow --lines 100

rpi-event-log:
  ssh {{remote}} -- "tail -n 50 -F /home/dc/.local/share/expenses-manager/eventlog.jsonl"

# Overwrite the database on the Raspberry Pi with the local database
[confirm]
rpi-overwrite-data:
  ssh {{remote}} -- mkdir -p /home/dc/.local/share/expenses-manager
  scp ~/.local/share/expenses-manager/expenses.db dc@{{remote}}:/home/dc/.local/share/expenses-manager/expenses.db
  scp ~/.local/share/expenses-manager/eventlog.jsonl dc@{{remote}}:/home/dc/.local/share/expenses-manager/eventlog.jsonl

rpi-sync:
  curl -v -X POST "http://{{remote}}:8082/sync" -H "Cf-Access-Authenticated-User-Email: diogo.filipe.acastro@gmail.com"

##############################################################
# Raspberry Pi - Public demo page
##############################################################
demo-app-dir := "/home/dc/.local/share/expenses-manager-demo"

rpi-setup-demo-service env_file:
  ssh {{remote}} -- mkdir -p {{demo-app-dir}}
  scp ./resources/test-app-dir/expenses.db              dc@{{remote}}:{{demo-app-dir}}/expenses.db
  scp ./resources/test-app-dir/config.yaml              dc@{{remote}}:{{demo-app-dir}}/config.yaml
  scp ./resources/demo/expenses-manager-demo.service    dc@{{remote}}:{{demo-app-dir}}/expenses-manager-demo.service
  scp {{env_file}}                                      dc@{{remote}}:{{demo-app-dir}}/override.conf
  ssh {{remote}} -- " \
    sudo mv {{demo-app-dir}}/expenses-manager-demo.service    /etc/systemd/system/expenses-manager-demo.service;  \
    sudo mkdir -p                                             /etc/systemd/system/expenses-manager-demo.service.d ; \
    sudo mv {{demo-app-dir}}/override.conf                    /etc/systemd/system/expenses-manager-demo.service.d/override.conf ; \
    sudo systemctl daemon-reload ; \
    sudo systemctl enable expenses-manager-demo ; \
    sudo systemctl restart expenses-manager-demo ; "

rpi-demo-logs:
  ssh {{remote}} -- journalctl -u expenses-manager-demo --follow --lines 100

##############################################################
# Systemd
##############################################################

# Install the application in the nix store
install:
  #!/usr/bin/env bash
  set -euxo pipefail
  # Use a shebang to be able to set variables
  # See: https://just.systems/man/en/setting-variables-in-a-recipe.html

  BUNDLE_PATH=$(nix build .#expenses-manager-bundle-native --print-out-paths --no-link)
  nix profile remove expenses-manager-bundle-native
  nix profile install $BUNDLE_PATH
  sudo find $BUNDLE_PATH/bin/ -type f -exec touch {} +
  sudo systemctl restart expenses-manager

setup-service env_file:
  sudo cp ./resources/prod/expenses-manager-localhost.service   /etc/systemd/system/expenses-manager.service
  sudo mkdir -p                                                 /etc/systemd/system/expenses-manager.service.d
  sudo cp {{env_file}}                                          /etc/systemd/system/expenses-manager.service.d/override.conf
  cp ./resources/prod/config.yaml                               /home/dc/.local/share/expenses-manager/config.yaml
  sudo systemctl daemon-reload
  sudo systemctl enable expenses-manager
  sudo systemctl restart expenses-manager

logs:
  # journalctl -xefu expenses-manager --no-pager
  # journalctl -u expenses-manager --no-pager
  journalctl -u expenses-manager --follow --lines 100

event-log:
  tail -n 50 -F /home/dc/.local/share/expenses-manager/eventlog.jsonl
