// Drives the expenses web app in headless Chrome.
// Reads one command per line from stdin (see SKILL.md for the list), prints the results.
//
// Env:
//   RUN_DIR         (required) screenshots go to "$RUN_DIR/shots/".
//   PLAYWRIGHT_DIR  (required) directory where `playwright-core` is installed (`npm i --prefix`).
//   BASE_URL        (default http://localhost:8091/)
//   CHROME          (default /usr/bin/google-chrome)
import { createRequire } from 'node:module';
import { createInterface } from 'node:readline';
import { mkdirSync } from 'node:fs';
import path from 'node:path';

const need = name => process.env[name] ?? (() => { throw new Error(`Set ${name}`); })();
const runDir = need('RUN_DIR');
const require = createRequire(path.join(need('PLAYWRIGHT_DIR'), 'node_modules', 'noop.js'));
const { chromium } = require('playwright-core');
const base = process.env.BASE_URL ?? 'http://localhost:8091/';
const shots = path.join(runDir, 'shots');
mkdirSync(shots, { recursive: true });

const browser = await chromium.launch({
  executablePath: process.env.CHROME ?? '/usr/bin/google-chrome',
  args: ['--no-sandbox'],
});
const page = await (await browser.newContext({ viewport: { width: 1600, height: 1000 } })).newPage();
const errors = [];
page.on('pageerror', e => errors.push(e.message));
page.on('console', m => { if (m.type() === 'error') errors.push(m.text()); });

// The first word is the command. The rest of the line is its argument(s).
const commands = {
  // nav <path>              e.g. `nav #/review`
  nav: async arg => { await page.goto(new URL(arg, base).href); await page.waitForLoadState('networkidle'); },
  // click <selector>
  click: async arg => page.locator(arg).click(),
  // fill <selector> ||| <text>
  fill: async arg => { const [sel, text] = arg.split(' ||| '); await page.locator(sel).fill(text); },
  // type <text>             types into the focused element
  type: async arg => page.keyboard.type(arg),
  // press <key>             e.g. `press Tab`
  press: async arg => page.keyboard.press(arg),
  // wait-for <selector>
  'wait-for': async arg => page.locator(arg).first().waitFor(),
  // wait <ms>
  wait: async arg => page.waitForTimeout(Number(arg)),
  // text <selector>         prints the innerText of each match
  text: async arg => console.log(JSON.stringify(await page.locator(arg).allInnerTexts())),
  // eval <js expression>    evaluated in the page, prints the JSON result
  eval: async arg => console.log(JSON.stringify(await page.evaluate(arg))),
  // api <path>              GET from the backend, prints the JSON (first 2000 chars)
  api: async arg => console.log(JSON.stringify(await (await fetch(new URL(arg, base))).json()).slice(0, 2000)),
  // screenshot <name>       saves "$RUN_DIR/shots/<name>.png"
  screenshot: async arg => {
    const file = path.join(shots, `${arg || 'screenshot'}.png`);
    await page.screenshot({ path: file, fullPage: false });
    console.log(file);
  },
  // console                 prints the console errors collected so far
  console: async () => console.log(errors.length ? errors.join('\n') : '(no console errors)'),
};

let failed = false;
for await (const raw of createInterface({ input: process.stdin })) {
  const line = raw.trim();
  if (!line || line.startsWith('#')) continue;
  const [cmd, ...rest] = line.split(' ');
  const run = commands[cmd];
  console.log(`> ${line}`);
  if (!run) { console.log(`Unknown command: ${cmd}`); failed = true; continue; }
  try {
    await run(rest.join(' '));
  } catch (e) {
    console.log(`ERROR: ${e.message.split('\n')[0]}`);
    failed = true;
  }
}
await browser.close();
process.exit(failed ? 1 : 0);
