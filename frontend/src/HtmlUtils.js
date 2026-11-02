

export const _log = msg => () => {
  console.log(msg);
}

export const moveCursorToEnd = element => () => {
  const range = document.createRange();
  range.selectNodeContents(element);
  range.collapse(false);
  const selection = window.getSelection();
  selection.removeAllRanges();
  selection.addRange(range);
}
