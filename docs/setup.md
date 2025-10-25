- [Setup](#setup)
  - [Setup static IPv4 addresses in the local machine and the raspberry pi.](#setup-static-ipv4-addresses-in-the-local-machine-and-the-raspberry-pi)
  - [Provision RPI](#provision-rpi)
    - [Nix](#nix)
    - [Cloudflare](#cloudflare)
  - [Install app in the RPI](#install-app-in-the-rpi)
  - [Add anacron job](#add-anacron-job)
  - [Configure Raspberry Pi connect](#configure-raspberry-pi-connect)
  - [Renew bank logins](#renew-bank-logins)


# Setup

## Setup static IPv4 addresses in the local machine and the raspberry pi.

```sh
# Lookup MAC address for this machine
# Look for `ether`, under `wlo1`
$ ifconfig

# Then go to the router's admin panel and map the MAC address to a static IP like 192.168.1.233
```

## Provision RPI

* Transfer SSH keys to the raspberry pi, to allow
  * running `ssh` and `scp` commands without having to type the password all the time
  * running the backup script as a cron job
```sh
ssh-copy-id dc@192.168.1.244
```

### Nix

* Install nix in both machines
* Enable nix flakes

* Make nix available during non-interactive SSH sessions.
  * Problem: when you run `ssh <ip> <command>`, and `.bashrc` is sourced, it'll exit early because there's a line that says:
    "If not running interactively, don't do anything"
    `nix copy` uses a non-interactive SSH login, so this is a problem.
    To verify it's a problem:
    ```
    $ ssh 192.168.1.244 -- nix --version
    bash: line 1: nix: command not found
    ```
    To fix it, we must add this line to `.bashrc` ABOVE the line with the early return:
    ```
    source /nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh
    ```
    Verify it now works:
    ```
    $ ssh 192.168.1.244 -- nix --version
    nix (Nix) 2.30.2
    ```

* Allow copying packages from host to target machines
```sh
# Open nix.conf in the raspberry pi
sudo vim /etc/nix/nix.conf
# Add this line:
# NOTE: `= ["dc"]` will NOT work
trusted-users = dc
# Restart nix daemon
sudo systemctl restart nix-daemon

# Then, on the host machine, try copying the nix store path:
nix copy /nix/store/v7vjfr5s586kky2jqnyshbblzi8qhjbz-expenses-bundle --to ssh://dc@192.168.1.244
```

### Cloudflare

* Install `cloudflared` on the RPI following the instructions for Debian:
  https://developers.cloudflare.com/cloudflare-one/connections/connect-networks/downloads/

* Setup a remotely-managed tunnel using Zero Trust
  https://developers.cloudflare.com/cloudflare-one/connections/connect-networks/get-started/create-remote-tunnel/
  * In "Published Application Routes" > "Additional application settings" > "Access", enable "Enforce Access JSON Web Token (JWT) validation"

* Setup a login method via Google OAuth2.0
  https://developers.cloudflare.com/cloudflare-one/identity/idp-integration/
  https://developers.cloudflare.com/cloudflare-one/identity/idp-integration/google/

* Go to "Access" > "Policies" to allow specific users to login
  https://developers.cloudflare.com/cloudflare-one/policies/access/policy-management/
  https://one.dash.cloudflare.com/8d4b450054bebd727ed6830294482a73/access/policies?tab=reusable

* Setup a public web application in Cloudflare using Google for authentication
  https://developers.cloudflare.com/cloudflare-one/applications/configure-apps/self-hosted-public-app/
  * Select the previously configured Login method
  * Select the previously configured Access policy
  * In Advanced > Cookie Settings, set `SameSite = lax`, to avoid redirect loops after logging in.
    * See: https://community.cloudflare.com/t/cloudflareaccess-com-redirect-loop/468169
    * See: https://developers.cloudflare.com/cloudflare-one/identity/authorization-cookie/#samesite-attribute

FAQ:

* How to logout?
  Go to: https://dfacastro.cloudflareaccess.com/cdn-cgi/access/logout
  https://developers.cloudflare.com/cloudflare-one/identity/users/session-management/#log-out-as-a-user

## Install app in the RPI

```sh
just rpi-overwrite-data
just rpi-deploy
just rpi-send-service
just rpi-install-env-vars /home/dc/Dropbox/dotfiles/expenses-manager-override.conf

# Setup db and event log
ssh {{remote}} -- mkdir -p /home/dc/.local/share/expenses-manager
scp ~/.local/share/expenses-manager/expenses.db dc@{{remote}}:/home/dc/.local/share/expenses-manager/expenses.db
scp ~/.local/share/expenses-manager/eventlog.jsonl dc@{{remote}}:/home/dc/.local/share/expenses-manager/eventlog.jsonl
```

## Add anacron job

```sh
# NOTE: this will backup to Dropbox, Mega, and Nextcloud, so I need to set those up first
sudo bash -c "echo '3	10	expenses-backup	sudo -u dc /home/dc/Dropbox/Projects/Haskell/expenses/scripts/backup.sh' >> /etc/anacrontab"
```

Run the anacron job manually:

```sh
sudo anacron -fnd expenses-backup
```

## Configure Raspberry Pi connect

* https://www.raspberrypi.com/documentation/services/connect.html
* https://connect.raspberrypi.com/devices

## Renew bank logins

Bank access must be renewed every 3 months.

```sh
just renew-logins
```
