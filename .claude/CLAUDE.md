# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Commands

```bash
just run                       # Run server in dev mode (port 8081, hot-reload)
just test                      # Run all tests
just test-filter <pattern>     # Run tests matching pattern, with file watch
just test-accept               # Accept golden file changes
```

For Stack directly:
```bash
stack build --fast             # Build library
stack test --fast              # Run tests
stack test --fast --ta '-p "some test name"'  # Run a single test
```

For deployment (cross-compiled to AArch64 for RPi Zero 2):
```bash
just rpi-build    # Cross-compile via Nix
just rpi-deploy   # Deploy and restart systemd service
```

## Architecture

Full-stack expense manager that syncs with bank accounts via the GoCardless Nordigen API. Backend is Haskell/Servant + SQLite; frontend is PureScript/Halogen. Deployed on a Raspberry Pi Zero 2.

### Effect System

The application uses `effectful` for effect management. All handlers run in `AppM`, a stack defined in `src/Expenses/Effects/Effects.hs`. Effects are layered bottom-up: `NextUUID → ErrorHandler → Db → Time → EventLog → Nordigen → Reader → FileSystem → Concurrent → Log → IOE`.

Each effect has its own module under `src/Expenses/Effects/` (SQLite, EventLog, NextUUID, Nordigen). When adding new functionality, prefer adding to an existing effect interface rather than creating new ones.

### API Layer

Servant routes are defined with the Named Routes pattern in `src/Expenses/Server/Routes.hs`.

Each route has its own handler module under `src/Expenses/Server/Routes/`.
Ensure each new route handler has its own dedicated module.

Authentication uses Cloudflare Zero Trust JWT (`AuthProtect "cloudflare-auth"`); admin routes additionally check the user's email against regexes in the config.

### Currency Amounts — Important Invariant

There are two amount types that must not be mixed:
- **`FECents`** (frontend): expenses are **positive**, refunds **negative** — used in JSON serialization
- **`BECents`** (backend): expenses are **negative**, refunds **positive** — used in SQLite

Conversions between `FECents` and `BECents` **MUST** use the `toBE` and `toFE` functions.

The frontend **MUST NOT** use `Int` or similar to model amounts of cents, it must use `FECents`.
The database **MUST NOT** use `Int` or similar to model amounts of cents, it must use `BECents`.

### Configuration

Runtime config is a YAML file loaded from `--app-dir` (default `~/.local/share/expenses-manager/`). The directory also contains `expenses.db` and `eventlog.jsonl`. For dev, use `--app-dir ./resources/test-app-dir/`.

Required environment variables for Nordigen sync: `EXPENSES_NORDIGEN_SECRET_ID`, `EXPENSES_NORDIGEN_SECRET_KEY`.
Treat these env vars as secrets.

Use `--demo-mode` in dev to skip real API calls.

### Database Migrations

Migrations are standalone executables in `db-migrations/` named `M01`–`M09`. Each migration is a new module.

When modifying the db schema, also update `schema.sql`

### Testing

Tests use Tasty (orchestration) with HSpec and golden files. Golden file outputs live in `test/golden/`. When a golden test fails due to intentional output changes, run `just test-accept` to update the snapshots.


### Domain

* An institution (e.g. a bank) supports many accounts
* Nordigen allows us to create "requisitions", which connects us to an institution.
* While approving the "requisition", the user tells Nordigen which accounts from that institution they want to enable.
* Therefore, our app keeps track of the institutions the user is connected to, each institution's requisition status, and the accounts for each institution.

## Instructions

* ALWAYS make sure the code compiles without warnings.
* Ensure the tests compile and pass after non-trivial changes.
