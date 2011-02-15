function getParams() {
  var question = document.URL.indexOf("?");
  var result = {};
  if (question > -1) {
    var params = document.URL.slice(question);
    while (params) {
      var match = params.match(/^.([^=]+)=([^&]*)(&.*)?$/);
      result[match[1]] = unescape(match[2]);
      params = match[3];
    }
  }
  return result;
}
