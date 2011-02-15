load("FunctionalTools.js");

function inPlacePrinter() {
  var div = __ENV.parent.DIV();
  var first = true;
  __ENV.output(div);
  return function(text) {
    text = String(text);
    if (__ENV.parent.preNewline != "\n")
      text = text.replace(/\n/g, __ENV.parent.preNewline);
    __ENV.parent.replaceChildNodes(div, __ENV.parent.document.createTextNode(String(text)));
    if (first) {
      __ENV.parent.scrollToBottom(div.parentNode.parentNode);
      first = false;
    }
  };
}
