function copy_to_clipboard(element) {
  var doc = document, text = doc.getElementById(element), range, selection;

  selection = window.getSelection();
  range = doc.createRange();
  range.selectNodeContents(text);
  selection.removeAllRanges();
  selection.addRange(range);

  console.log(this);

  document.execCommand('copy');
  window.getSelection().removeAllRanges();
}