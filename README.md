# expenses


This is an application I wrote for managing my family's expenses and budget.

It syncs with your bank accounts using the [GoCardless API](https://developer.gocardless.com/bank-account-data/quick-start-guide).

You can find a sample demo page here: <https://expenses-demo.diogocastro.com/>

## Tech stack

 * The backend is written in Haskell, using Servant. We use [`effectful`'s effect system](https://hackage.haskell.org/package/effectful-2.6.1.0#readme).
 * The frontend is written in Purescript, using Halogen.
 * The project is cross-compiled to AArch64 / ARM64 using `nix` and `haskell.nix`.
 * We deploy it to a Raspberry Pi Zero 2, where it is run as a `systemd` service.
 * We use a Cloudflare tunnel for exposing the service and Cloudflate Zero Trust for handling authentication.
 * We use `anacron` and `rsync` for performing regular backups.
 * We use `ntfy.sh` to send push notifications to our phones with budget updates.
