

export const _log = msg => () => {
  console.log(msg);
}

export const apiBaseUrl = (() => {
  let url = new URL(window.location.href);

  url.pathname = "/";
  url.hash = "";

  // The window.location.href could be:
  //  * http://localhost:1234/ if we're running in live reload mode with `just run`
  //  * http://localhost:8081/ if the static files are being served by the servant server
  //  * http://192.168.1.244:8082/ if the static files are being served by the systemd service, from the raspberry pi's nix store
  //
  // In the 1st case, we want to replace the port with 8081, and use that URL to send requests to the backend.
  // In all other cases, we want to use the same base URL.
  if (url.port === '1234') {
    url.port = '8081';
  }

  const urlStr = url.toString();
  console.log("Base URL: ", urlStr);
  return urlStr;
})();
