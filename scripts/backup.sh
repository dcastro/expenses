#!/bin/bash

# Rsync:
#   * https://www.digitalocean.com/community/tutorials/how-to-use-rsync-to-sync-local-and-remote-directories
#   * https://linux.die.net/man/1/rsync
#   * https://superuser.com/a/1644264/298837


# -e: exit on error
# -u: error on undefined var
# -x: print command before execution
# -o pipefail: exit on command pipe failure
set -e
set -u
set -x
set -o pipefail


REMOTE_DIR="dc@192.168.1.244:/home/dc/.local/share/expenses-manager/"
LOCALHOST_DIR="/home/dc/.local/share/expenses-manager/"
TIMESTAMP=$(date +"%Y-%m-%d_%H-%M-%S")

NEXTCLOUD_DIR="/home/dc/Nextcloud/expenses-manager/"
DROPBOX_DIR="/home/dc/Dropbox/expenses-manager/"
MEGA_DIR="/home/dc/mega/expenses-manager/"

# Optional SUFFIX argument
SUFFIX="${1:-}"

# Sync RPI -> Localhost
rsync --archive --verbose --progress --partial \
    "${REMOTE_DIR}/" \
    "${LOCALHOST_DIR}"


# Sync with cloud storage
DIRS=(
  "/home/dc/Nextcloud/expenses-manager/"
  "/home/dc/Dropbox/expenses-manager/"
  "/home/dc/mega/expenses-manager/"
)
for CS_DIR in "${DIRS[@]}"; do
  # NOTE:Use a trailing slash on the source path to copy the contents of the directory
  rsync -avP "${LOCALHOST_DIR}/logs/"              "${CS_DIR}/logs/"

  mkdir -p                                         "${CS_DIR}/backups/"
  rsync -avP "${LOCALHOST_DIR}/expenses.db"        "${CS_DIR}/backups/${TIMESTAMP}${SUFFIX}/"
  rsync -avP "${LOCALHOST_DIR}/eventlog.jsonl"     "${CS_DIR}/backups/${TIMESTAMP}${SUFFIX}/"
done
