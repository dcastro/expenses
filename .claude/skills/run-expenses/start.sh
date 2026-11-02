#!/usr/bin/env bash
# Starts the expenses server in the background, against a COPY of an app dir,
# so that driving the UI never changes the real dev/test data.
#
# Env:
#   RUN_DIR  (required) scratch directory. The app dir copy, server.log and shots/ go here.
#   PORT     (default 8091) port for the server. `just run` uses 8081, so this doesn't clash.
#   APP_SRC  (default resources/dev-app-dir) app dir to copy. Use resources/test-app-dir
#            when the dev dir has no db (populate it with `just restore-dev-app-dir`).
#
# The copy is made only once. Delete "$RUN_DIR/app-dir" to start from fresh data.
set -euo pipefail

: "${RUN_DIR:?Set RUN_DIR to a scratch directory}"
PORT="${PORT:-8091}"
repo="$(git -C "$(dirname "$0")" rev-parse --show-toplevel)"
APP_SRC="${APP_SRC:-$repo/resources/dev-app-dir}"

mkdir -p "$RUN_DIR/app-dir" "$RUN_DIR/shots"
if [ ! -f "$RUN_DIR/app-dir/expenses.db" ]; then
  # `-L`: in the dev dir, config.yaml is a symlink to ../prod/config.yaml.
  cp -L "$APP_SRC/config.yaml" "$APP_SRC/expenses.db" "$RUN_DIR/app-dir/"
  touch "$RUN_DIR/app-dir/eventlog.jsonl"
  echo "Copied $APP_SRC to $RUN_DIR/app-dir"
fi

if (exec 3<>"/dev/tcp/127.0.0.1/$PORT") 2>/dev/null; then
  echo "Port $PORT is already in use. Stop it with: lsof -ti:$PORT -sTCP:LISTEN | xargs -r kill"
  exit 1
fi

cd "$repo"
# The server refuses to start without these. Dummy values are fine without `--cron`;
# only the sync-status call (and manual syncs) fail, with a 401 from Nordigen.
EXPENSES_NORDIGEN_SECRET_ID=dummy \
EXPENSES_NORDIGEN_SECRET_KEY=dummy \
EXPENSES_NTFY_TOPIC=dummy \
  nohup stack run expenses-manager-server -- \
    --user test \
    --port "$PORT" \
    --app-dir "$RUN_DIR/app-dir" \
    > "$RUN_DIR/server.log" 2>&1 &

for _ in $(seq 1 300); do
  if (exec 3<>"/dev/tcp/127.0.0.1/$PORT") 2>/dev/null; then
    echo "Server is up on http://localhost:$PORT/ (log: $RUN_DIR/server.log)"
    exit 0
  fi
  sleep 1
done
echo "The server did not start. See $RUN_DIR/server.log"
exit 1
