---
name: run-expenses
description: Build, run, and drive the expenses manager web app (Haskell server + PureScript frontend). Use when asked to start or run the app, take a screenshot of its UI, click through a page (Review, Budget, Search, ...), or check that a frontend or API change works in the real app, not only in the tests.
---

Start the server against a **copy** of an app dir with `start.sh`, then drive headless Chrome with
`driver.mjs`. The driver reads one command per line on stdin. All paths are relative to the repo
root.

## Prerequisites

These were already on the machine: `stack`, `just`, `node`/`npm`, `sqlite3`, `lsof`, and Google
Chrome at `/usr/bin/google-chrome`. `chromium-cli` is not installed. The driver uses
`playwright-core` with the system Chrome instead.

Install `playwright-core` once, outside the repo:

```bash
export RUN_DIR=<your scratch dir>/run            # app dir copy, server.log, shots/
export PLAYWRIGHT_DIR=<your scratch dir>/pw
npm i --silent --prefix "$PLAYWRIGHT_DIR" playwright-core@1.48.2
```

## Build

The server serves the frontend bundle from `frontend/prod/` (git-ignored). Rebuild it after each
frontend change:

```bash
(cd frontend && just build-prod)
```

`start.sh` uses `stack run`, which builds the backend.

## Run (agent path)

```bash
.claude/skills/run-expenses/start.sh
```

- It copies `resources/dev-app-dir` to `$RUN_DIR/app-dir`, once. To start from fresh data, delete
  that copy.
  - If the dev dir has no `expenses.db`, set `APP_SRC=resources/test-app-dir`.
- It starts the server on `PORT` (default 8091) with `--user test`, so no Cloudflare header is
  necessary. With the prod config, `test` is an admin.
- It waits until the port is open.

Drive it:

```bash
node .claude/skills/run-expenses/driver.mjs <<'EOF'
nav /
click a.navbar-item:has-text("Review")
wait-for text=to review
text p:has-text("to review")
text [title^="Suggestion"] >> visible=true
screenshot review
click [title^="Suggestion"] >> visible=true
type  (edited)
press Tab
wait 1000
api transactions/review
console
EOF
```

Then look at the screenshot: `$RUN_DIR/shots/review.png`.

| Command | Does |
|---|---|
| `nav <path>` | Opens `BASE_URL` + path (e.g. `nav #/review`), waits for network idle. |
| `click <selector>` | Clicks a Playwright selector. |
| `fill <selector> \|\|\| <text>` | Fills an input. |
| `type <text>` / `press <key>` | Keyboard input on the focused element. |
| `wait-for <selector>` / `wait <ms>` | Waits. |
| `text <selector>` | Prints the `innerText` of each match. |
| `eval <js>` | Evaluates in the page, prints JSON. |
| `api <path>` | GETs a backend route, prints the JSON (cut to 2000 chars). |
| `screenshot <name>` | Saves `$RUN_DIR/shots/<name>.png`. |
| `console` | Prints the browser console errors collected so far. |

The driver exits with 1 if any command fails.

### Test data

To add test data, change the **copy** with `sqlite3`. Never change `resources/dev-app-dir`.
Example: give the newest untagged item a suggestion:

```bash
sqlite3 "$RUN_DIR/app-dir/expenses.db" "UPDATE transaction_items SET details_suggestion = 'Calças de ganga Slim Fit' WHERE transaction_id = (SELECT t.id FROM transactions t JOIN transaction_items ti ON t.id = ti.transaction_id WHERE ti.tag IS NULL AND COALESCE(ti.details, '') = '' ORDER BY t.date DESC LIMIT 1); SELECT changes();"
```

The UI changes the copy too, for example when you approve or edit details. To run a flow again,
undo those changes with `sqlite3`, or delete `$RUN_DIR/app-dir`, then restart.

### Stop

```bash
lsof -ti:8091 -sTCP:LISTEN | xargs -r kill
```

## Run (human path)

See `just run` in `.claude/CLAUDE.md` (port 8081, file watch). It uses the real dev app dir and
needs the real `EXPENSES_*` env vars, so it changes the real dev data. Do not use it for agent
checks. (Not verified by this skill.)

## Test

```bash
stack test --fast
(cd frontend && spago test)
```

## Gotchas

- **Hidden pages have the same rows.** Root keeps "Single month" and "Multiple months" mounted
  with `display: none`. So `tr:has-text("...")` also finds hidden copies, and `.click()` waits
  forever ("element is not visible"). Add `>> visible=true` to row and cell selectors. The same
  applies to inputs: the first `input` on the page is the hidden month picker.
- **Search filters come from the URL.** Use `nav #/search?desc=CONTINENTE` instead of filling the
  hidden-input-prone search form.
- **Expected console errors.** `console` always shows these two, and they are not bugs:
  - A 500 from `GET /sync-status`: Nordigen rejects the dummy secrets with a 401 (see
    `server.log`).
  - The ZoomCharts license warnings.

  Once, a 404 also showed. Its URL is unknown, and it did not occur again.
- **Blur saves the details.** `press Tab` (or a click elsewhere) triggers the save. Tab can move
  the focus to the next row's "Approve" button. That is harmless.
- **Navbar screenshots just after a click** can show the old tab still half-highlighted, because
  of the Bulma transition. To check the active tab, use
  `eval [...document.querySelectorAll('a.navbar-item.is-active')].map(a => a.innerText)`.
- **`config.yaml` in the dev dir is a symlink** (`../prod/config.yaml`). `start.sh` copies it with
  `cp -L`. A plain copy gives a broken link.

## Troubleshooting

| Symptom | Fix |
|---|---|
| `start.sh`: "Port 8091 is already in use" | A previous server still runs. Stop it (see "Stop"), or set `PORT`. |
| `driver.mjs`: `Cannot find module 'playwright-core'` | `PLAYWRIGHT_DIR` is not the `--prefix` that you installed into. |
| `TimeoutError ... element is not visible` | The selector found a hidden copy. Add `>> visible=true`. |
