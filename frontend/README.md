# Expenses Manager

## Development

Requires Node.js and `npm`.

```sh
npm install -g purescript spago parcel esbuild
```

Most commands below use [justfile](https://github.com/casey/just).

To launch the app in the browser:

```sh
just run
```

## Test

```sh
just test
```

## Production

Run `just build-prod` to build the minified production bundle, you can find it in the `/docs` folder.

To test the production output locally, run:

```sh
npm install -g http-server
http-server docs -o
```
