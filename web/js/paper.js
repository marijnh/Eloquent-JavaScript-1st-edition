//- bookutil.js

function stripPath(url){
  var slash = url.lastIndexOf("/");
  return (slash == -1) ? url : url.slice(slash + 1);
}

function addPoint(a, b) {
  return {x: a.x + b.x, y: a.y + b.y};
}

function placeElement(e, p) {
  setElementPosition(e, p);
  setElementDimensions(e, p);
}

var dimMode = function(){
  var mode = undefined;
  return function(){
    if (mode == undefined) {
      var test = DIV(null, "q");
      test.style.width = "100px";
      test.style.borderWidth = "5px";
      test.style.borderStyle = "solid";
      test.style.visibility = "hidden";
      document.body.appendChild(test);
      mode = test.offsetWidth == 100 ? "precise" : "standard";
      removeElement(test);
    }
    return mode;
  }
}();

function growElement(node) {
  setElementDimensions(node, {w: node.parentNode.clientWidth, h: node.parentNode.clientHeight});
}

function centerElement(node, pos) {
  setElementPosition(node, {x: pos.x - node.offsetWidth / 2, y: pos.y - node.offsetHeight / 2});
}

function attach(event, func) {
  return function(element){connect(element, event, func);};
}

function scrollToBottom(element) {
  element.scrollTop = element.scrollHeight - element.clientHeight;
}

function disconnectTree(root) {
  disconnectAll(root);
  if (root.childNodes)
    forEach(root.childNodes, disconnectTree);
}

var browserIsIE = document.all && window.ActiveXObject;
var preNewline = browserIsIE ? "\r" : "\n";

function probablyAnArray(value) {
  try {
    return value && typeof value == "object" &&
      typeof value.length == "number" && typeof value.splice == "function";
  } catch (e) {return false;}
}

function probablyARegexp(value) {
  try {
    return value && typeof value == "object" &&
      typeof value.ignoreCase == "boolean" && typeof value.compile == "function";
  } catch (e) {return false;}
}

function probablyADOMNode(value) {
  try {
    return value && typeof value == "object" &&
      value.previousSibling !== undefined &&
      (value.nodeType == 3 || value.nodeType == 1);
  } catch (e) {return false;}
}

function isAccessibleWindow(win) {
  try {
    return win && typeof win == "object" && win.document && win.document.nodeType == 9;
  } catch (e) {return false;}
}

function trim(string) {
  return string.match(/^\s*(.*)\s*$/)[1];
}

function getCookie(name, def) {
  var cookies = document.cookie.split(";");
  for (var i = 0; i < cookies.length; i++) {
    var parts = cookies[i].split("=");
    if (trim(parts[0]) == name)
      return parts[1];
  }
  return def;
}

function setCookie(name, value) {
  document.cookie = name + "=" + value + "; expires=" + (new Date(2030, 0, 1)).toGMTString();
}

//- initenv.js

var htmlTable = [];

function initEnvironment(win, output, callback) {
  var feed = (win.execScript ? function(code) {win.execScript(code);} : function(code) {win.__setTimeout(code, 0);});

  function wrapCode(before, after, code) {
    env.code = code;
    return "__setTimeout(__ENV.callback, 0); try{" + before + "__ENV.code" + after +"}catch(e){__ENV.error(e);}";
  }

  function run(code, showResult) {
    if (showResult) 
      feed(wrapCode("$r = eval(", "); if ($r !== undefined) " + showResult + "($r);", code));
    else
      feed(wrapCode("eval(", ");", code));
  }

  function error(err) {
    env.output(DIV(null, "Exception: ", env.format(err)));
    if (err.stack) {
      var stack = err.stack.split("\n");
      for (var i = 0; i < env.maxStackTrace && i < stack.length; i++) {
        var part = stack[i], at = part.indexOf("@");
        if (at > 1 && part.slice(0, 5) != "eval(")
          win.print("  in function " + part.slice(0, at));
      };
      if (stack.length > env.maxStackTrace)
        win.print("  [...]")
    }
  };

  var env = {
    output: method(output, "append"),
    format: method(output, "summarize"),
    parent: window,
    maxStackTrace: 10,
    error: error,
    run: run,
    callback: callback,
    code: null
  };
  win.__ENV = env;

  win.load = function(file) {
    win.document.body.appendChild(withDocument(win.document, function(){
      return createDOM("SCRIPT", {type: "text/javascript", src: /$http:\/\//.test(file) ? file : "js/" + file});
    }));
  };

  win.print = function() {
    var accum = [];
    for (var i = 0; i != arguments.length; i++)
      accum.push(String(arguments[i]));
    var joined = accum.join("");
    env.output(DIV(null, preNewline != "\n" ? joined.replace(/\n/g, preNewline) : joined));
  };

  win.show = function(x) {
    env.output(DIV(null, env.format(x)));
  };

  win.viewHTML = function(html) {
    htmlTable.push(String(html));
    var result = window.open("view.html?id=" + (htmlTable.length - 1));
    if (!result)
      alert("There seems to be a popup-blocker stopping this page from opening new windows. Try turning it off first.");
    return result;
  };

  function wrapAction(action) {
    if (typeof action == "string")
      return "try{" + action + "}catch(e){__ENV.error(e);}";
    else
      return function(){try{action();}catch(e){env.error(e);}};
  }

  // Apparantly, the .call and .apply methods of setTimeout and
  // setInterval don't quite work on IE, so we rename them and call
  // them directly.
  win.__setTimeout = win.setTimeout;
  win.__setInterval = win.setInterval;

  win.setTimeout = function(action, interval) {
    return win.__setTimeout(wrapAction(action), interval);
  };

  win.setInterval = function(action, interval) {
    return win.__setInterval(wrapAction(action), interval);
  };

  // For some strange reason, this *has* to be executed from the
  // window itself, or creating the Error object fails (in IE).
  win.__setTimeout("if (/^\\[object/.test((new Error(\"...\")).toString())) " +
                   "Error.prototype.toString = function(){return this.name + \": \" + this.message;};", 0);
}

//- env.js

function makeFrame(place, init) {
  var frame = createDOM("IFRAME", {"style": "border-width: 0; display: none;"});
  (place || document.body).appendChild(frame);

  var fdoc = frame.contentWindow.document;
  fdoc.open();
  fdoc.write("<html><head><title>Default</title></head><body style=\"border-width: 0\"></body></html>");
  fdoc.close();

  if (init) {
    if (fdoc.body)
      init(frame);
    else
      connect(frame, "onload", function(){disconnectAll(frame, "onload"); init(frame);});
  }
  return frame;
}

function checkAppVersion(minimum){
  if (typeof navigator.appVersion != "string") return false;
  var parts = navigator.appVersion.split(" ");
  return parts.length > 0 && Number(parts[0]) >= minimum;
}
var useJSEditor = true;
// No CodeMirror for iOS
if (/AppleWebKit/.test(navigator.userAgent) && /iP[oa]d|iPhone/.test(navigator.userAgent)) useJSEditor = false;

function Buffer(name, content, where){
  if (useJSEditor){
    var self = this;
    this.editor = new CodeMirror(function(node){
      self.node = node;
      where.appendChild(node);
    }, {value: content, matchBrackets: true, lineWrapping: true});
  }
  else {
    this.node = TEXTAREA({spellcheck: false}, content);
    where.appendChild(this.node);
  }
  growElement(this.node);
  this.name = name;
}

Buffer.prototype = {
  show: function() {
    showElement(this.node);
  },
  hide: function() {
    hideElement(this.node);
  },
  remove: function() {
    removeElement(this.node);
  },
  getCode: function() {
    if (useJSEditor)
      return this.editor.getValue();
    else
      return this.node.value;
  }
};

function History(){
  this.history = [];
  this.pos = 0;
  this.current = "";
}

History.prototype = {
  push: function(line) {
    this.history.push(line);
    if (this.history.length > 150)
      this.history = this.history.slice(0, 100);
    this.pos = this.history.length;
  },
  move: function(dir, from) {
    if (this.pos == this.history.length)
      this.current = from;
    else
      this.history[this.pos] = from;

    this.pos = (this.pos + dir) % (this.history.length + 1);
    if (this.pos < 0) // JS' modulo is a bit impractical when dealing with negative numbers
      this.pos += (this.history.length + 1);

    if (this.pos == this.history.length)
      return this.current;
    else
      return this.history[this.pos];
  }
};

var dotdotdot = "\u2026";

function summarize(element, depth) {
  depth = depth || 0;
  var maxLength = depth == 0 ? 50 : 10;
  var self = this;

  function nodeLength(node) {
    if (node.nodeType == 3)
      return node.nodeValue.length;
    else
      return sum(map(nodeLength, node.childNodes));
  }
  function span(className, content, onclick) {
    var result = SPAN({"class": className}, content);
    if (onclick) {
      connect(result, "onclick", function(event) {
        onclick();
        event.stop();
      });
    }
    return result;
  }
  function objectHasProperties(object) {
    for (var x in object)
      return true;
    return false;
  }
  // Some mystery objects in IE throw an exception when you try to
  // enumerate them.
  function objectIsEnumerable(object) {
    try {
      for (var x in object)
        return true;
      return true;
    }
    catch (e) {
      return false;
    }
  }

  function formatFunc(value) {
    var regexp = /^\s*function ?([^\(]*)?\(([^\(]*)\)/;
    var match = String(value).match(regexp);
    return span("functionvalue", match ? "<function " + (match[1] || "") + "(" + match[2] + ")>" : "<function>");
  }
  function formatArray(value) {
    var content = ["["], length = 2;
    if (depth > 1) {
      if (value.length > 0)
        content.push(dotdotdot);
    }
    else {
      for (var i = 0; i < value.length; i++) {
        var last = i == value.length - 1;
        var summary = self.summarize(value[i], depth + 1);
        var summaryLength = nodeLength(summary);
        if (length + summaryLength + (last ? 0 : 3) <= maxLength) {
          content.push(summary);
          length += summaryLength;
        }
        else {
          content.push(dotdotdot);
          break;
        }
        if (!last)
          content.push(", ");
      }
    }
    content.push("]");
    return span("arrayvalue", content, bind(self.expand, self, value));
  }
  function formatDOMNode(value) {
    if (value.nodeType == 3)
      return value.nodeValue.replace("\n", "\\n");
    var accum = ["<", value.nodeName.toLowerCase()];
    if (value.attributes) {
      forEach(value.attributes, function(attr) {
        if (attr.specified && typeof attr.nodeValue == "string") {
          accum.push(" " + attr.nodeName.toLowerCase() + "=" + serializeJSON(attr.nodeValue));
        }
      });
    }
    accum.push(">");
    return accum.join("");
  }
  function formatObject(value) {
    var asString = (probablyADOMNode(value) ? formatDOMNode(value) : value + "");
    if (/^\[object.*\]$/.test(asString))
      return formatPlainObject(value);
    else
      return span("objectvalue", asString, bind(self.expand, self, value));
  }
  function formatPlainObject(value) {
    var content = ["{"], length = 2, elements = [], first = true;
    if (depth > 1) {
      if (objectIsEnumerable(value) && objectHasProperties(value))
        content.push(dotdotdot);
    }
    else if (objectIsEnumerable(value)) {
      for (var name in value) {
        var skip = true;
        // Firefox has a nasty habit of throwing 'not implemented' or 'security'
        // exceptions when accessing certain properties in window and document.
        try {
          var element = value[name];
          skip = false;
        }
        catch (e) {}
        if (!skip) {
          if (first) {
            first = false;
          }
          else {
            content.push(", ");
            length += 2;
          }
          var summary = self.summarize(element, depth + 1);
          var elementLength = nodeLength(summary) + 2 + name.length;
          if (length + elementLength <= maxLength) {
            content.push(name + ": ");
            content.push(summary);
            length += elementLength;
          }
          else {
            content.push(dotdotdot);
            break;
          }
        }
      }
    }
    content.push("}");
    return span("objectvalue", content, bind(self.expand, self, value));
  }
  function formatString(value){
    return span("stringvalue", serializeJSON(value));
  }
  function formatAtom(value) {
    return span("atomvalue", String(value));
  }

  var type = typeof element;
  // Regexps report their type as function, but that is a lousy way to
  // display them.
  if (probablyARegexp(element))
    type = "object";
  if (type == "function")
    return formatFunc(element);
  else if (type == "object" && element != null) {
    // Some IE built-in functions report their type as "object"
    // Also, the navigator object in IE can not be passed to the String function. Gah.
    if (browserIsIE && ("" + element).match(/function .*/))
      return formatFunc(element);
    else if (probablyAnArray(element))
      return formatArray(element);
    else
      return formatObject(element);
  }
  else if (type == "string")
    return formatString(element);
  else
    return formatAtom(element);
}

function inspect(value) {
  function cutOff(name) {
    if (name.length > 22)
      return name.slice(0, 21) + dotdotdot;
    else
      return name;
  }
  
  var tbody = TBODY();
  if (probablyAnArray(value)) {
    for (var i = 0; i < value.length; i++)
      tbody.appendChild(TR(null, TH(null, i + ":"), TD(null, this.summarize(value[i], 1))));
  }
  else {
    var elements = [];
    for (var name in value) {
      var skip = true;
      try {
        var element = value[name];
        skip = false;
      }
      catch(e) {}
      if (!skip)
        tbody.appendChild(TR(null, TH(null, cutOff(name) + ":"), TD(null, this.summarize(element, 1))));
    }
  }
  return TABLE({"class": "objecttable"}, tbody);
}

function Output(place, parent) {
  this.place = place;
  this.scrollPos = 0;
  this.stack = [];
  this.parent = parent;

  this.outhead = DIV({"class": "outputhead"},
                     DIV({"class": "outputbutton", "title": "Clear output"}, "\u263C",
                         attach("onclick", method(this, "clear"))),
                     "Output:");
  this.out = PRE(), this.show = PRE();
  this.scroll = DIV({"class": "outputinner", "id": "outputinner"}, this.out);
  this.showhead = DIV({"class": "outputhead"},
                      DIV({"class": "outputbutton", "title": "Store this value in $i"}, "$",
                          attach("onclick", method(this, "copy"))),
                      DIV({"class": "outputbutton", "title": "Close inspect view"}, "\u00D7",
                          attach("onclick", method(this, "close"))),
                      DIV({"class": "outputbutton", "title": "Back"}, "\u2190",
                          attach("onclick", method(this, "back"))),
                     "View object");
  replaceChildNodes(this.place, this.outhead, this.scroll);
}

Output.prototype = {
  append: function(node) {
    this.out.appendChild(node);
    if (this.stack.length == 0)
      scrollToBottom(this.scroll);
  },
  clear: function() {
    disconnectTree(this.out);
    replaceChildNodes(this.out);
  },
  expand: function(value) {
    if (this.stack.length == 0) {
      this.scrollPos = this.scroll.scrollTop;
      this.place.replaceChild(this.showhead, this.outhead);
      this.scroll.replaceChild(this.show, this.out);
    }
    this.stack.push(value);
    this.display(value);
  },
  display: function(value) {
    disconnectTree(this.show);
    this.scroll.scrollTop = 0;
    replaceChildNodes(this.show, this.inspect(value));
  },
  close: function() {
    this.place.replaceChild(this.outhead, this.showhead);
    this.scroll.replaceChild(this.out, this.show);
    this.scroll.scrollTop = this.scrollPos;
    this.stack = [];
  },
  back: function() {
    this.stack.pop();
    if (this.stack.length == 0)
      this.close();
    else
      this.display(this.stack[this.stack.length - 1]);
  },
  copy: function() {
    if (this.parent.env)
      this.parent.env.$i = this.stack[this.stack.length - 1];
  },
  summarize: summarize,
  inspect: inspect
};

function Console(param) {
  var active, self = this, frame = null;
  var history = new History();
  var out = new Output(param.output, this);
  var baseEnv, codeStream = [], streaming = false;
  resetEnvironment();

  function showBuffer(buffer) {
    if (active)
      active.hide();
    active = buffer;
    active.show();
    return active;
  }

  function runCode(code, showResult) {
    if (self.env && !self.env.__ENV) {
      self.env = baseEnv;
      self.env.print("Lost attached window, detaching.");
    }

    if (streaming || !self.env) {
      codeStream.push({code: code, show: showResult});
    }
    else {
      streaming = true;
      self.env.__ENV.run(code, showResult);
    }
  }
  // Because the window that code must be sent to can change, code
  // must only be sent when the code before it has finished running.
  // Hence this callback.
  function runCallback() {
    if (codeStream.length > 0 && self.env) {
      var code = codeStream.shift();
      self.env.__ENV.run(code.code, code.show);
    }
    else {
      streaming = false;
    }
  }
  function setEnvironment(win) {
    self.env = win;
    if (win && !streaming && codeStream.length > 0)
      runCallback();
  }

  var buffers = SELECT({"class": "buffers"});
  replaceChildNodes(
    param.controls,
    BUTTON({title: "Run the code in this buffer", "type": "button"},
           "Run", attach("onclick", function(){runCode(active.getCode(), false);})),
    buffers,
    BUTTON({title: "New buffer", "type": "button"}, "New", attach("onclick", createBuffer)),
//    BUTTON({title: "Load a file as a new buffer", "type": "button"}, "Load", attach("onclick", loadFile)),
    BUTTON({title: "Close this buffer", "type": "button"}, "Close", attach("onclick", closeBuffer)),
    BUTTON({title: "Reset the console environment", "type": "button"}, "Reset", attach("onclick", resetEnvironment)));
  connect(buffers, "onchange", function(){
    showBuffer(buffers.options[buffers.selectedIndex].buffer);
  });
  var repl = INPUT({"type": "text"});
  replaceChildNodes(param.repl, repl);
  connect(repl, "onkeydown", lineKey);

  function bufferName(name){
    function exists(name) {
      return some(buffers.childNodes, function(option){return option.text == name;});
    }
    if (!exists(name)) return name;
    for (var i = 2; ; i++) {
      var newName = name + "(" + i + ")";
      if (!exists(newName)) return newName;
    }
  }

  function addBuffer(name, content){
    var option = OPTION(null, bufferName(name));
    buffers.appendChild(option);
    option.selected = true;
    option.buffer = new Buffer(name, content || "", param.editor);
    return showBuffer(option.buffer);
  }
  function gotoBuffer(name) {
    var res = false;
    forEach(buffers.options, function(opt) {
      if (opt.value == name) {
        opt.selected = true;
        showBuffer(opt.buffer);
        res = true;
        throw StopIteration;
      }
    });
    return res;
  }
  function createBuffer(){
    var name = prompt("Enter a name for the new buffer", "");
    if (name)
      addBuffer(name);
  }
  function closeBuffer(){
    if (buffers.selectedIndex != -1) {
      buffers.removeChild(buffers.childNodes[buffers.selectedIndex]);
      active.remove();
      if (buffers.firstChild){
        active = buffers.firstChild.buffer;
        buffers.firstChild.selected = true;
        active.show();
      }
      else {
        active = null;
      }
    }
  }
  function loadFile(){
    var filename = prompt("Enter a filename or URL", "");
    if (filename) {
      var simplename = stripPath(filename);
      if (!/^http:\/\//.test(filename))
        filename = "js/" + filename;
      var defer = doXHR(filename);
      defer.addCallback(function(xhr){addBuffer(simplename, xhr.responseText);});
      defer.addErrback(function(){alert("File '" + simplename + "' could not be loaded.");});
    }
  }

  function attachEnvironment(win) {
    function detach() {
      if (self.env == win) {
        var title = self.env.document.title;
        setEnvironment(baseEnv);
        self.env.print("Detaching from window '", title || "[unnamed]", "'.");
      }
    }
    function attach() {
      if (!win.__ENV)
        initEnvironment(win, out, runCallback);
      win.detach = detach;
      var unload = connect(win, "onunload", detach);
      connect(window, "onunload", function(){disconnect(unload);});
      setEnvironment(win);
      self.env.print("Attaching to window '", win.document.title || "[unnamed]", "'.");
    }

    if (isAccessibleWindow(win)) {
      connect(win, "onload", attach);
      // When immediately attaching to a newly created window, wait
      // until onload, or strange things happen.
      self.env = null;
      if (win.document.body && (win.document.body.childNodes.length > 0 || win.document.title != ""))
        attach();
    }
    else {
      self.env.print("Not an accessible window.");
    }
  }

  function resetEnvironment(){
    if (frame)
      removeElement(frame);
    frame = makeFrame(param.framePlace, function(frame){
      baseEnv = frame.contentWindow;
      initEnvironment(baseEnv, out, runCallback);
      baseEnv.attach = attachEnvironment;
      if (param.initEnv)
        param.initEnv(baseEnv);
      setEnvironment(baseEnv);
    });
  }

  function evalLine(){
    var line = repl.value;
    repl.value = "";
    history.push(line);
    runCode(line, "show");
  }
  function getHistory(dir){
    repl.value = history.move(dir, repl.value);
  }

  // Opera generates events for the '(' and '&' keys that are pretty
  // much the same as those for arrow down and up. So, to
  // disambiguate, we disallow shift when those are pressed.
  function lineKey(event){
    var key = event.key().string;
    var shift = event.modifier().shift;
    if (key == "KEY_ENTER")
      evalLine();
    else if (key == "KEY_ARROW_UP" && !shift)
      getHistory(-1);
    else if (key == "KEY_ARROW_DOWN" && !shift)
      getHistory(1);
    else
      return;
    event.stop();
  }

  addBuffer("*scratch*");

  this.loadCode = addBuffer;
  this.showCode = gotoBuffer;
  this.runCode = function(code){runCode(code, false);};
  this.evalCode = function(code){runCode(code, "show");};
  this.printCode = function(code){runCode(code, "print");};
}

var dimMode = function(){
  var mode = undefined;
  return function(){
    if (mode == undefined) {
      var test = DIV(null, "q");
      test.style.width = "100px";
      test.style.borderWidth = "5px";
      test.style.borderStyle = "solid";
      test.style.visibility = "hidden";
      document.body.appendChild(test);
      mode = test.offsetWidth == 100 ? "precise" : "standard";
      removeElement(test);
    }
    return mode;
  }
}();
var sizeCorrection = null;

function resizeConsole() {
  var cn = $("console");
  var margin = 6;
  var leftRatio = .4;
  var width = cn.clientWidth, height = cn.clientHeight;

  var bottomHeight = Math.max($("repl").offsetHeight, $("controls").offsetHeight);
  var topHeight = height - 3 * margin - bottomHeight;
  var innerWidth = width - 3 * margin;
  var leftWidth = Math.round(leftRatio * innerWidth);
  var rightWidth = innerWidth - leftWidth;

  var output = $("output"), editor = $("editor");

  placeElement(output, {x: margin, y: margin, w: leftWidth + sizeCorrection, h: topHeight + sizeCorrection});
  placeElement($("repl"), {x: margin, y: margin + margin + topHeight, w: leftWidth + sizeCorrection});
  setElementDimensions($("repl").firstChild, {w: $("repl").clientWidth});

  placeElement(editor, {x: 2 * margin + leftWidth, y: margin, w: rightWidth + sizeCorrection, h: topHeight + sizeCorrection});
  placeElement($("controls"), {x: 2 * margin + leftWidth, y: margin + margin + topHeight, w: rightWidth});

  forEach(editor.childNodes, growElement);
  setElementDimensions($("outputinner"), {w: output.clientWidth, h: output.clientHeight - output.firstChild.offsetHeight});
}

function initConsole(where) {
  var output = DIV({"class": "output", "id": "output"}),
    controls = DIV({"id": "controls"}),
    editor = DIV({"class": "editor", "id": "editor"}),
    repl = DIV({"class": "editor", "id": "repl"});
  replaceChildNodes(where, output, controls, editor, repl);

  function initFrame(env) {
    env.load("base-env.js");
    st = $("frameplace").firstChild.style;
    st.display = ""; st.width = "100%"; st.height = "50em";
  }
  connect(window, "onresize", resizeConsole);
  var cn = new Console({output: output, controls: controls, editor: editor, repl: repl, initEnv: initFrame,
    framePlace: $("frameplace")});
  resizeConsole();
  return cn;
}

var _console;

connect(window, "onload", function() {
  if (/^\[object/.test((new Error("...")).toString()))
    Error.prototype.toString = function(){return this.name + ": " + this.message;};
  sizeCorrection = dimMode() == "standard" ? -2 : 0;
  _console = initConsole($("console"));
  initCodeList();
});

var code = {
  1: ["// variable\nvar caught = 5 * 5;\nprint(caught);\ncaught = 4 * 4;\nprint(caught);\n",
      "// variable 2\nvar luigisDebt = 140;\nluigisDebt = luigisDebt - 35;\nprint(luigisDebt);\n",
      "// alert\nalert(\"Good morning!\");\n",
      "// confirm\nprint(confirm(\"Shall we, then?\"));\n",
      "// prompt\nprint(prompt(\"Tell me everything you know.\", \"...\"));\n",
      "// conditionals\nvar theNumber = Number(prompt(\"Pick a number\", \"\"));\nif (!isNaN(theNumber))\n  alert(\"Your number is the square root of \" + (theNumber * theNumber));\n",
      "// if/else\nif (true == false)\n  print(\"How confusing!\");\nelse\n  print(\"True still isn't false.\");\n",
      "// if/else 2\nvar num = prompt(\"Pick a number:\", \"0\");\n\nif (num < 10)\n  print(\"Small\");\nelse if (num < 100)\n  print(\"Medium\");\nelse\n  print(\"Large\");\n",
      "// while\nvar currentNumber = 0;\nwhile (currentNumber <= 12) {\n  print(currentNumber);\n  currentNumber = currentNumber + 2;\n}\n",
      "// power of 2\nvar result = 1;\nvar counter = 0;\nwhile (counter < 10) {\n  result = result * 2;\n  counter = counter + 1;\n}\nprint(result);\n",
      "// do/while\ndo {\n  var input = prompt(\"Who are you?\");\n} while (!input);\n",
      "// for\nfor (var number = 0; number <= 12; number = number + 2)\n  print(number);\n",
      "// break\nfor (var current = 20; ; current++) {\n  if (current % 7 == 0)\n    break;\n}\nprint(current);\n",
      "// switch\nswitch(prompt(\"What is the weather like?\")) {\n  case \"rainy\":\n    print(\"Remember to bring an umbrella.\");\n    break;\n  case \"sunny\":\n    print(\"Dress lightly.\");\n  case \"cloudy\":\n    print(\"Go outside.\");\n    break;\n  default:\n    print(\"Unknown weather type!\");\n    break;\n}\n",
      "// comments\n// The variable counter, which is about to be defined, is going\n// to start with a value of 0, which is zero.\nvar counter = 0;\n// Next, we loop. Hold on to your hat.\nwhile (counter < 100 /* counter is less than one hundred */)\n/* Every time we loop, we INCREMENT the value of counter,\n   You could say we just add one to it. */\n  counter++;\n// And here, we are done.\n",
      "// type conversion\nprint(\"Apollo\" + 5);\nprint(null + \"ify\");\nprint(\"5\" * 5);\nprint(\"strawberry\" * 5);\n",
      "// or-defaulting\nvar input = prompt(\"What is your name?\", \"Kilgore Trout\");\nprint(\"Well hello \" + (input || \"dear\"));\n",
      "// shortcut evaluation\nfalse || alert(\"I'm happening!\");\nfalse && alert(\"Not me.\");\n"],

  2: ["// square\nfunction square(x) {\n  return x * x;\n}\n\nprint(square(12));\n",
      "// power\nfunction power(base, exponent) {\n  var result = 1;\n  for (var count = 0; count < exponent; count++)\n    result *= base;\n  return result;\n}\n\nprint(power(2, 10));\n",
      "// evaluation order\nprint(\"The future says: \", future());\n\nfunction future() {\n  return \"We STILL have no flying cars.\";\n}\n",
      "// local variables\nvar x = \"A\";\n\nfunction setVarToB() {\n  x = \"B\";\n}\nsetVarToB();\nprint(x);\n\nfunction setVarToC() {\n  var x;\n  x = \"C\";\n}\nsetVarToC();\nprint(x);\n",
      "// inner functions\nfunction multiplyAbsolute(number, factor) {\n  function multiply(number) {\n    return number * factor;\n  }\n  if (number < 0)\n    return multiply(-number);\n  else\n    return multiply(number);\n}\n",
      "// block\nvar something = 1;\n{\n  var something = 2;\n  // Do stuff with variable something...\n}\n// Outside of the block again...\n",
      "// infinite recursion\nfunction chicken() {\n  return egg();\n}\nfunction egg() {\n  return chicken();\n}\nprint(chicken() + \" came first.\");\n",
      "// functions as values\nvar a = null;\nprint((a || function(){return \"B\";})());\n",
      "// makeAdder\nfunction makeAdder(amount) {\n  return function(number) {\n    return number + amount;\n  };\n}\nvar addTwo = makeAdder(2);\nprint(addTwo(3));\n",
      "// optional argument\nfunction power(base, exponent) {\n  var result = 1;\n  if (exponent === undefined)\n    exponent = 2;\n  for (var count = 0; count < exponent; count++)\n    result *= base;\n  return result;\n}\n",
      "// zeroPad\nfunction zeroPad(number, width) {\n  var string = String(Math.round(number));\n  while (string.length < width)\n    string = \"0\" + string;\n  return string;\n}\n",
      "// recursive power\nfunction power(base, exponent) {\n  if (exponent == 0)\n    return 1;\n  else\n    return base * power(base, exponent - 1);\n}\n",
      "// recursive search\nfunction findSequence(goal) {\n  function find(start, history) {\n    if (start == goal)\n      return history;\n    else if (start > goal)\n      return null;\n    else\n      return find(start + 5, \"(\" + history + \" + 5)\") ||\n             find(start * 3, \"(\" + history + \" * 3)\");\n  }\n  return find(1, \"1\");\n}\n\nprint(findSequence(24));\n"],

  3: ["// object-as-set\nvar set = {\"Spot\": true};\nset[\"White Fang\"] = true;\ndelete set[\"Spot\"];\nprint(\"Asoka\" in set);\n",
      "// object identity\nvar object1 = {value: 10};\nvar object2 = object1;\nvar object3 = {value: 10};\n\nprint(object1 == object2);\nprint(object1 == object3);\n\nobject1.value = 15;\nprint(object2.value);\nprint(object3.value);\n",
      "// mail array\nvar mailArchive = [\"mail one\", \"mail two\", \"mail three\"];\n\nfor (var current = 0; current < mailArchive.length; current++)\n  print(\"Processing email #\", current, \": \", mailArchive[current]);\n",
      "// range\nfunction range(upto) {\n  var result = [];\n  for (var i = 0; i <= upto; i++)\n    result[i] = i;\n  return result;\n}\nprint(range(4));\n",
      "// method call\nvar doh = \"Doh\";\nprint(typeof doh.toUpperCase);\nprint(doh.toUpperCase());\n",
      "// array methods\nvar mack = [];\nmack.push(\"Mack\");\nmack.push(\"the\");\nmack.push(\"Knife\");\nprint(mack);\nprint(mack.join(\" \"));\nprint(mack.pop());\nprint(mack);\n",
      "// startsWith\nfunction startsWith(string, pattern) {\n  return string.slice(0, pattern.length) == pattern;\n}\n\nprint(startsWith(\"rotation\", \"rot\"));\n",
      "// catNames\nfunction catNames(paragraph) {\n  var colon = paragraph.indexOf(\":\");\n  return paragraph.slice(colon + 2).split(\", \");\n}\n\nprint(catNames(\"born 20/09/2004 (mother Yellow Bess): Doctor Hobbles the 2nd, Noog\"));\n",
      "// algorithm, take 1\nvar livingCats = {\"Spot\": true};\n\nfor (var mail = 0; mail < ARCHIVE.length; mail++) {\n  var paragraphs = ARCHIVE[mail].split(\"\\n\");\n  for (var i = 0; i < paragraphs.length; i++) {\n    var paragraph = paragraphs[i];\n    if (startsWith(paragraph, \"born\")) {\n      var names = catNames(paragraph);\n      for (var name = 0; name < names.length; name++)\n        livingCats[names[name]] = true;\n    }\n    else if (startsWith(paragraph, \"died\")) {\n      var names = catNames(paragraph);\n      for (var name = 0; name < names.length; name++)\n        delete livingCats[names[name]];\n    }\n  }\n}\n",
      "// RIP spot\nif (\"Spot\" in livingCats)\n  print(\"Spot lives!\");\nelse\n  print(\"Good old Spot, may she rest in peace.\");\n",
      "// list the cats\nfor (var cat in livingCats)\n  print(cat);\n",
      "// addToSet/removeFromSet\nfunction addToSet(set, values) {\n  for (var i = 0; i < values.length; i++)\n    set[values[i]] = true;\n}\n\nfunction removeFromSet(set, values) {\n  for (var i = 0; i < values.length; i++)\n    delete set[values[i]];\n}\n",
      "// algorithm, take 2\nvar livingCats = {Spot: true};\n\nfor (var mail = 0; mail < ARCHIVE.length; mail++) {\n  var paragraphs = ARCHIVE[mail].split(\"\\n\");\n  for (var i = 0; i < paragraphs.length; i++) {\n    var paragraph = paragraphs[i];\n    if (startsWith(paragraph, \"born\"))\n      addToSet(livingCats, catNames(paragraph));\n    else if (startsWith(paragraph, \"died\"))\n      removeFromSet(livingCats, catNames(paragraph));\n  }\n}\n",
      "// algorithm, take 3\nfunction findLivingCats() {\n  var livingCats = {\"Spot\": true};\n\n  function handleParagraph(paragraph) {\n    if (startsWith(paragraph, \"born\"))\n      addToSet(livingCats, catNames(paragraph));\n    else if (startsWith(paragraph, \"died\"))\n      removeFromSet(livingCats, catNames(paragraph));\n  }\n\n  for (var mail = 0; mail < ARCHIVE.length; mail++) {\n    var paragraphs = ARCHIVE[mail].split(\"\\n\");\n    for (var i = 0; i < paragraphs.length; i++)\n      handleParagraph(paragraphs[i]);\n  }\n  return livingCats;\n}\n",
      "// extractDate\nfunction extractDate(paragraph) {\n  function numberAt(start, length) {\n    return Number(paragraph.slice(start, start + length));\n  }\n  return new Date(numberAt(11, 4), numberAt(8, 2) - 1, numberAt(5, 2));\n}\n",
      "// addCats/deadCats\nfunction catRecord(name, birthdate, mother) {\n  return {name: name, birth: birthdate, mother: mother};\n}\n\nfunction addCats(set, names, birthdate, mother) {\n  for (var i = 0; i < names.length; i++)\n    set[names[i]] = catRecord(names[i], birthdate, mother);\n}\nfunction deadCats(set, names, deathdate) {\n  for (var i = 0; i < names.length; i++)\n    set[names[i]].death = deathdate;\n}\n",
      "// extractMother\nfunction extractMother(paragraph) {\n  var start = paragraph.indexOf(\"(mother \") + \"(mother \".length;\n  var end = paragraph.indexOf(\")\");\n  return paragraph.slice(start, end);\n}\n\nprint(extractMother(\"born 15/11/2003 (mother Spot): White Fang\"));\n",
      "// algorithm, take 4\nfunction findCats() {\n  var cats = {\"Spot\": catRecord(\"Spot\", new Date(1997, 2, 5), \"unknown\")};\n\n  function handleParagraph(paragraph) {\n    if (startsWith(paragraph, \"born\"))\n      addCats(cats, catNames(paragraph), extractDate(paragraph),\n              extractMother(paragraph));\n    else if (startsWith(paragraph, \"died\"))\n      deadCats(cats, catNames(paragraph), extractDate(paragraph));\n  }\n\n  for (var mail = 0; mail < ARCHIVE.length; mail++) {\n    var paragraphs = ARCHIVE[mail].split(\"\\n\");\n    for (var i = 0; i < paragraphs.length; i++)\n      handleParagraph(paragraphs[i]);\n  }\n  return cats;\n}\n\nvar catData = findCats();\n",
      "// catInfo\nfunction formatDate(date) {\n  return date.getDate() + \"/\" + (date.getMonth() + 1) + \"/\" + date.getFullYear();\n}\n\nfunction catInfo(data, name) {\n  var cat = data[name];\n  if (cat == undefined)\n    return \"No cat by the name of \" + name + \" is known.\";\n\n  var message = name + \", born \" + formatDate(cat.birth) +\n                \" from mother \" + cat.mother;\n  if (\"death\" in cat)\n    message += \", died \" + formatDate(cat.death);\n  return message + \".\";\n}\n\n// For example...\nprint(catInfo(catData, \"Fat Igor\"));\n",
      "// oldestCat\nfunction oldestCat(data) {\n  var oldest = null;\n\n  for (var name in data) {\n    var cat = data[name];\n    if (!(\"death\" in cat) && (oldest == null || oldest.birth > cat.birth))\n      oldest = cat;\n  }\n\n  if (oldest == null)\n    return null;\n  else\n    return oldest.name;\n}\n",
      "// argumentCounter\nfunction argumentCounter() {\n  return \"You gave me \" + arguments.length + \" arguments.\";\n}\nprint(argumentCounter(\"Straw man\", \"Tautology\", \"Ad hominem\"));\n",
      "// optional argument\nfunction add(number, howmuch) {\n  if (arguments.length < 2)\n    howmuch = 1;\n  return number + howmuch;\n}\n\nprint(add(6));\nprint(add(6, 4));\n",
      "// range, again\nfunction range(start, end) {\n  if (arguments.length < 2) {\n    end = start;\n    start = 0;\n  }\n  var result = [];\n  for (var i = start; i <= end; i++)\n    result.push(i);\n  return result;\n}\n\nprint(range(4));\nprint(range(2, 4));\n",
      "// sum-of-range\nfunction sum(numbers) {\n  var total = 0;\n  for (var i = 0; i < numbers.length; i++)\n    total += numbers[i];\n  return total;\n}\n\nprint(sum(range(1, 10)));\n"],

  4: ["// between\nfunction between(string, start, end) {\n  var startAt = string.indexOf(start) + start.length;\n  var endAt = string.indexOf(end, startAt);\n  return string.slice(startAt, endAt);\n}\n\nprint(between(\"Louis 'Pops' Armstrong\", \"'\", \"'\"));\n",
      "// between, checking for errors\nfunction between(string, start, end) {\n  var startAt = string.indexOf(start);\n  if (startAt == -1)\n    return undefined;\n  startAt += start.length;\n  var endAt = string.indexOf(end, startAt);\n  if (endAt == -1)\n    return undefined;\n\n  return string.slice(startAt, endAt);\n}\n\nvar input = prompt(\"Tell me something\", \"\");\nvar parenthesized = between(input, \"(\", \")\");\nif (parenthesized != undefined)\n  print(\"You parenthesized '\", parenthesized, \"'.\");\n",
      "// lastElement\nfunction lastElement(array) {\n  if (array.length > 0)\n    return array[array.length - 1];\n  else\n    return undefined;\n}\n\nprint(lastElement([1, 2, undefined]));\n",
      "// lastElement, with exception\nfunction lastElement(array) {\n  if (array.length > 0)\n    return array[array.length - 1];\n  else\n    throw \"Cannot take the last element of an empty array.\";\n}\n\nfunction lastElementPlusTen(array) {\n  return lastElement(array) + 10;\n}\n\ntry {\n  print(lastElementPlusTen([]));\n}\ncatch (error) {\n  print(\"Something went wrong: \", error);\n}\n",
      "// program error\ntry {\n  print(Sasquatch);\n}\ncatch (error) {\n  print(\"Caught: \" + error.message);\n}\n",
      "// throwing inputNumber\nvar InvalidInputError = new Error(\"Invalid numeric input\");\n\nfunction inputNumber() {\n  var input = Number(prompt(\"Give me a number\", \"\"));\n  if (isNaN(input))\n    throw InvalidInputError;\n  return input;\n}\n",
      "// test exception\nfor (;;) {\n  try {\n    alert(inputNumber() + 5);\n    break;\n  }\n  catch (e) {\n    if (e != InvalidInputError)\n      throw e;\n    alert(\"You did not input a number. Try again.\");\n  }\n}\n",
      "// automatic test\nfunction testBetween() {\n  function assert(name, x) {\n    if (!x)\n      throw \"Assertion failed: \" + name;\n  }\n\n  assert(\"identical delimiters\", between(\"a |b| c\", \"|\", \"|\") == \"b\");\n  assert(\"whole string\", between(\"[[n]]\", \"[[\", \"]]\") == \"n\");\n  assert(\"reversed\", between(\"]x[\", \"[\", \"]\") == undefined);\n  assert(\"missing end\", between(\" -->d \", \"-->\", \"<--\") == undefined);\n  /* and so on */\n}\n\ntestBetween();\n"],

  5: ["// forEach\nfunction forEach(array, action) {\n  for (var i = 0; i < array.length; i++)\n    action(array[i]);\n}\n\nforEach([\"Wampeter\", \"Foma\", \"Granfalloon\"], print);\n",
      "// sum\nfunction sum(numbers) {\n  var total = 0;\n  forEach(numbers, function (number) {\n    total += number;\n  });\n  return total;\n}\n\nprint(sum([1, 2, 3]));\n",
      "// negate, version 1\nfunction negate(func) {\n  return function() {\n    return !func.apply(null, arguments);\n  };\n}\n",
      "// reduce\nfunction reduce(combine, base, array) {\n  forEach(array, function (element) {\n    base = combine(base, element);\n  });\n  return base;\n}\n\nfunction add(a, b) {\n  return a + b;\n}\n\nfunction sum(numbers) {\n  return reduce(add, 0, numbers);\n}\n",
      "// countZeroes\nfunction count(test, array) {\n  var counted = 0;\n  forEach(array, function(element) {\n    if (test(element)) counted++;\n  });\n  return counted;\n}\n\nfunction countZeroes(array) {\n  function isZero(x) {return x === 0;}\n  return count(isZero, array);\n}\n",
      "// map\nfunction map(func, array) {\n  var result = [];\n  forEach(array, function (element) {\n    result.push(func(element));\n  });\n  return result;\n}\n\nprint(map(Math.round, [0.01, 2, 9.89, Math.PI]));\n",
      "// split into paragraphs\nvar paragraphs = RECLUSEFILE.split(\"\\n\\n\");\nprint(paragraphs.length);\n",
      "// processParagraph\nfunction processParagraph(paragraph) {\n  var header = 0;\n  while (paragraph.charAt(header) == \"%\")\n    header++;\n  if (header > 0)\n    return {type: \"h\" + header, content: paragraph.slice(header + 1)};\n  else\n    return {type: \"p\", content: paragraph};\n}\n\nshow(processParagraph(paragraphs[0]));\n",
      "// splitParagraph, functional\nfunction splitParagraph(text) {\n  function split(pos) {\n    if (pos == text.length) {\n       return [];\n    }\n    else if (text.charAt(pos) == \"*\") {\n      var end = findClosing(\"*\", pos + 1),\n          frag = {type: \"emphasized\", content: text.slice(pos + 1, end)};\n      return [frag].concat(split(end + 1));\n    }\n    else if (text.charAt(pos) == \"{\") {\n      var end = findClosing(\"}\", pos + 1),\n          frag = {type: \"footnote\", content: text.slice(pos + 1, end)};\n      return [frag].concat(split(end + 1));\n    }\n    else {\n      var end = findOpeningOrEnd(pos),\n          frag = {type: \"normal\", content: text.slice(pos, end)};\n      return [frag].concat(split(end));\n    }\n  }\n\n  function findClosing(character, from) {\n    var end = text.indexOf(character, from);\n    if (end == -1) throw new Error(\"Missing closing '\" + character + \"'\");\n    else return end;\n  }\n\n  function findOpeningOrEnd(from) {\n    function indexOrEnd(character) {\n      var index = text.indexOf(character, from);\n      return index == -1 ? text.length : index;\n    }\n    return Math.min(indexOrEnd(\"*\"), indexOrEnd(\"{\"));\n  }\n\n  return split(0);\n}\n",
      "// splitParagraph, idiomatic\nfunction splitParagraph(text) {\n  function split() {\n  var pos = 0, fragments = [];\n  while (pos < text.length) {\n    if (text.charAt(pos) == \"*\") {\n      var end = findClosing(\"*\", pos + 1);\n      fragments.push({type: \"emphasized\", content: text.slice(pos + 1, end)});\n      pos = end + 1;\n    }\n    else if (text.charAt(pos) == \"{\") {\n      var end = findClosing(\"}\", pos + 1);\n      fragments.push({type: \"footnote\", content: text.slice(pos + 1, end)});\n      pos = end + 1;\n    }\n    else {\n      var end = findOpeningOrEnd(pos);\n      fragments.push({type: \"normal\", content: text.slice(pos, end)});\n      pos = end;\n    }\n  }\n  return fragments;\n}\n\n  function findClosing(character, from) {\n    var end = text.indexOf(character, from);\n    if (end == -1) throw new Error(\"Missing closing '\" + character + \"'\");\n    else return end;\n  }\n\n  function findOpeningOrEnd(from) {\n    function indexOrEnd(character) {\n      var index = text.indexOf(character, from);\n      return index == -1 ? text.length : index;\n    }\n    return Math.min(indexOrEnd(\"*\"), indexOrEnd(\"{\"));\n  }\n\n  return split(0);\n}\n",
      "// processParagraph\nfunction processParagraph(paragraph) {\n  var header = 0;\n  while (paragraph.charAt(header) == \"%\")\n    header++;\n  if (header > 0)\n    return {type: \"h\" + header, content: splitParagraph(paragraph.slice(header + 1))};\n  else\n    return {type: \"p\", content: splitParagraph(paragraph)};\n}\n",
      "// extractFootnotes\nfunction extractFootnotes(paragraphs) {\n  var footnotes = [];\n  var currentNote = 0;\n\n  function replaceFootnote(fragment) {\n    if (fragment.type == \"footnote\") {\n      currentNote++;\n      footnotes.push(fragment);\n      fragment.number = currentNote;\n      return {type: \"reference\", number: currentNote};\n    }\n    else {\n      return fragment;\n    }\n  }\n\n  forEach(paragraphs, function(paragraph) {\n    paragraph.content = map(replaceFootnote, paragraph.content);\n  });\n\n  return footnotes;\n}     \n",
      "// tag\nfunction tag(name, content, attributes) {\n  return {name: name, attributes: attributes, content: content};\n}\n\nfunction link(target, text) {\n  return tag(\"a\", [text], {href: target});\n}\n\nfunction htmlDoc(title, bodyContent) {\n  return tag(\"html\", [tag(\"head\", [tag(\"title\", [title])]),\n                      tag(\"body\", bodyContent)]);\n}\n",
      "// HTML renderer\nfunction escapeHTML(text) {\n  var replacements = [[/&/g, \"&amp;\"], [/\"/g, \"&quot;\"],\n                      [/</g, \"&lt;\"], [/>/g, \"&gt;\"]];\n  forEach(replacements, function(replace) {\n    text = text.replace(replace[0], replace[1]);\n  });\n  return text;\n}\n\nfunction renderAttributes(attributes) {\n  if (attributes == null) return \"\"\n\n  var result = [];\n  for (var name in attributes)\n    if (attributes.hasOwnProperty(name))\n      result.push(\" \" + name + \"=\\\"\" + escapeHTML(attributes[name]) + \"\\\"\");\n  return result.join(\"\");\n}\n\nfunction renderHTML(element) {\n  var pieces = [];\n\n  function render(element) {\n    // Text node\n    if (typeof element == \"string\") {\n      pieces.push(escapeHTML(element));\n    }\n    // Empty tag\n    else if (!element.content || element.content.length == 0) {\n      pieces.push(\"<\" + element.name +\n                  renderAttributes(element.attributes) + \">\");\n    }\n    // Tag with content\n    else {\n      pieces.push(\"<\" + element.name +\n                  renderAttributes(element.attributes) + \">\");\n      forEach(element.content, render);\n      pieces.push(\"</\" + element.name + \">\");\n    }\n  }\n\n  render(element);\n  return pieces.join(\"\");\n}\n\nprint(renderHTML(link(\"http://www.nedroid.com\", \"Drawings!\")));\n",
      "// book render functions\nfunction renderFragment(fragment) {\n  if (fragment.type == \"reference\")\n    return tag(\"sup\", [link(\"#footnote\" + fragment.number, String(fragment.number))]);\n  else if (fragment.type == \"emphasized\")\n    return tag(\"em\", [fragment.content]);\n  else if (fragment.type == \"normal\")\n    return fragment.content;\n}\n\nfunction renderParagraph(paragraph) {\n  return tag(paragraph.type, map(renderFragment, paragraph.content));\n}\n\nfunction renderFootnote(footnote) {\n  var anchor = tag(\"a\", [], {name: \"footnote\" + footnote.number});\n  var number = \"[\" + footnote.number + \"] \";\n  return tag(\"p\", [tag(\"small\", [anchor, number, footnote.content])]);\n}\n\nfunction renderFile(file, title) {\n  var paragraphs = map(processParagraph, file.split(\"\\n\\n\"));\n  var footnotes = map(renderFootnote, extractFootnotes(paragraphs));\n  var body = map(renderParagraph, paragraphs).concat(footnotes);\n  return renderHTML(htmlDoc(title, body));\n}\n\ndocument.body.innerHTML = renderFile(RECLUSEFILE, \"The Book of Programming\");\n",
      "// op object\nvar op = {\n  \"+\": function(a, b){return a + b;},\n  \"==\": function(a, b){return a == b;},\n  \"===\": function(a, b){return a === b;},\n  \"!\": function(a){return !a;}\n  /* and so on */\n};\n",
      "// partial application\nfunction partial(func) {\n  var knownArgs = arguments;\n  return function() {\n    var realArgs = [];\n    for (var i = 1; i < knownArgs.length; i++)\n      realArgs.push(knownArgs[i]);\n    for (var i = 0; i < arguments.length; i++)\n      realArgs.push(arguments[i]);\n    return func.apply(null, realArgs);\n  };\n}\n\nprint(map(partial(op[\"+\"], 1), [0, 2, 4, 6, 8, 10]));\n",
      "// sum/reduce\nvar sum = partial(reduce, op[\"+\"], 0);\n",
      "// negate, version 2\nfunction negate(func) {\n  return function() {\n    return !func.apply(null, arguments);\n  };\n}\n",
      "// compose\nfunction compose(f1, f2) {\n  return function() {\n    return f1(f2.apply(null, arguments));\n  };\n}\n\nvar isNotNaN = compose(op[\"!\"], isNaN);\nprint(isNotNaN(5));\n"],

  6: ["// the first method\nvar rabbit = {};\nrabbit.speak = function(line) {\n  print(\"The rabbit says '\", line, \"'\");\n};\n\nrabbit.speak(\"I'm alive.\");\n",
      "// method sharing\nfunction speak(line) {\n  print(\"The \", this.adjective, \" rabbit says '\", line, \"'\");\n}\nvar whiteRabbit = {adjective: \"white\", speak: speak};\nvar fatRabbit = {adjective: \"fat\", speak: speak};\n\nwhiteRabbit.speak(\"Oh my ears and whiskers, how late it's getting!\");\nfatRabbit.speak(\"I could sure use a carrot right now.\");\n",
      "// call and apply\nspeak.apply(fatRabbit, [\"Yum.\"]);\n\nspeak.call(fatRabbit, \"Burp.\");\n\nfunction run(from, to) {\n  print(\"The \", this.adjective, \" rabbit runs from \", from, \" to \", to, \".\");\n}\nrun.apply(whiteRabbit, [\"A\", \"B\"]);\nrun.call(fatRabbit, \"the cupboard\", \"the fridge\");\n",
      "// the first constructor\nfunction Rabbit(adjective) {\n  this.adjective = adjective;\n  this.speak = function(line) {\n    print(\"The \", this.adjective, \" rabbit says '\", line, \"'\");\n  };\n}\n\nvar killerRabbit = new Rabbit(\"killer\");\nkillerRabbit.speak(\"GRAAAAAAAAAH!\");\n",
      "// prototypes\nRabbit.prototype.teeth = \"small\";\nprint(killerRabbit.teeth);\nkillerRabbit.teeth = \"long, sharp, and bloody\";\nprint(killerRabbit.teeth);\nprint(Rabbit.prototype.teeth);\n",
      "// methods in prototype\nRabbit.prototype.dance = function() {\n  print(\"The \", this.adjective, \" rabbit dances a jig.\");\n};\n\nkillerRabbit.dance();",
      "// constructor 2\nfunction Rabbit(adjective) {\n  this.adjective = adjective;\n}\nRabbit.prototype.speak = function(line) {\n  print(\"The \", this.adjective, \" rabbit says '\", line, \"'\");\n};\n",
      "// automatic properties\nvar noCatsAtAll = {};\nif (\"constructor\" in noCatsAtAll)\n  print(\"Yes, there definitely is a cat called 'constructor'.\");\n",
      "// properties method, take 1\nObject.prototype.properties = function() {\n  var result = [];\n  for (var property in this)\n    result.push(property);\n  return result;\n};\n\nvar test = {x: 10, y: 3};\nprint(test.properties());\n",
      "// properties method, take 2\nObject.prototype.properties = function() {\n  var result = [];\n  for (var property in this) {\n    if (this.hasOwnProperty(property))\n      result.push(property);\n  }\n  return result;\n};\n\nvar test = {\"Fat Igor\": true, \"Fireball\": true};\nprint(test.properties());\n",
      "// forEachIn\nfunction forEachIn(object, action) {\n  for (var property in object) {\n    if (Object.prototype.hasOwnProperty.call(object, property))\n      action(property, object[property]);\n  }\n}\n",
      "// propertyIsEnumerable\nvar object = {foo: \"bar\"};\nprint(Object.prototype.hasOwnProperty.call(object, \"foo\") &&\n      Object.prototype.propertyIsEnumerable.call(object, \"foo\"));\n",
      "// Dictionary type\nfunction Dictionary(startValues) {\n  this.values = startValues || {};\n}\nDictionary.prototype.store = function(name, value) {\n  this.values[name] = value;\n};\nDictionary.prototype.lookup = function(name) {\n  return this.values[name];\n};\nDictionary.prototype.contains = function(name) {\n  return Object.prototype.hasOwnProperty.call(this.values, name) &&\n    Object.prototype.propertyIsEnumerable.call(this.values, name);\n};\nDictionary.prototype.each = function(action) {\n  forEachIn(this.values, action);\n};\n\nvar colors = new Dictionary({Grover: \"blue\",\n                             Elmo: \"red\",\n                             Bert: \"yellow\"});  \nprint(colors.contains(\"Grover\"));\nprint(colors.contains(\"constructor\"));\n\ncolors.store(\"Ernie\", \"orange\");\ncolors.each(function(name, color) {\n  print(name, \" is \", color);\n});\n",
      "// terrarium plan\nvar thePlan =\n  [\"############################\",\n   \"#      #    #      o      ##\",\n   \"#                          #\",\n   \"#          #####           #\",\n   \"##         #   #    ##     #\",\n   \"###           ##     #     #\",\n   \"#           ###      #     #\",\n   \"#   ####                   #\",\n   \"#   ##       o             #\",\n   \"# o  #         o       ### #\",\n   \"#    #                     #\",\n   \"############################\"];\n",
      "// Point type\nfunction Point(x, y) {\n  this.x = x;\n  this.y = y;\n}\nPoint.prototype.add = function(other) {\n  return new Point(this.x + other.x, this.y + other.y);\n};\n",
      "// Grid type\nfunction Grid(width, height) {\n  this.width = width;\n  this.height = height;\n  this.cells = new Array(width * height);\n}\nGrid.prototype.valueAt = function(point) {\n  return this.cells[point.y * this.width + point.x];\n};\nGrid.prototype.setValueAt = function(point, value) {\n  this.cells[point.y * this.width + point.x] = value;\n};\nGrid.prototype.isInside = function(point) {\n  return point.x >= 0 && point.y >= 0 &&\n         point.x < this.width && point.y < this.height;\n};\nGrid.prototype.moveValue = function(from, to) {\n  this.setValueAt(to, this.valueAt(from));\n  this.setValueAt(from, undefined);\n};\n\nGrid.prototype.each = function(action) {\n  for (var y = 0; y < this.height; y++) {\n    for (var x = 0; x < this.width; x++) {\n      var point = new Point(x, y);\n      action(point, this.valueAt(point));\n    }\n  }\n};\n",
      "// directions object\nvar directions = new Dictionary(\n  {\"n\":  new Point( 0, -1),\n   \"ne\": new Point( 1, -1),\n   \"e\":  new Point( 1,  0),\n   \"se\": new Point( 1,  1),\n   \"s\":  new Point( 0,  1),\n   \"sw\": new Point(-1,  1),\n   \"w\":  new Point(-1,  0),\n   \"nw\": new Point(-1, -1)});\n",
      "// StupidBug\nfunction StupidBug() {};\nStupidBug.prototype.act = function(surroundings) {\n  return {type: \"move\", direction: \"s\"};\n};\n",
      "// Terrarium\nvar wall = {};\n\nfunction elementFromCharacter(character) {\n  if (character == \" \")\n    return undefined;\n  else if (character == \"#\")\n    return wall;\n  else if (character == \"o\")\n    return new StupidBug();\n}\n\nfunction Terrarium(plan) {\n  var grid = new Grid(plan[0].length, plan.length);\n  for (var y = 0; y < plan.length; y++) {\n    var line = plan[y];\n    for (var x = 0; x < line.length; x++) {\n      grid.setValueAt(new Point(x, y), elementFromCharacter(line.charAt(x)));\n    }\n  }\n  this.grid = grid;\n}\n",
      "// characterFromElement\nwall.character = \"#\";\nStupidBug.prototype.character = \"o\";\n\nfunction characterFromElement(element) {\n  if (element == undefined)\n    return \" \";\n  else\n    return element.character;\n}\n",
      "// Terrarium.prototype.toString\nTerrarium.prototype.toString = function() {\n  var characters = [];\n  var endOfLine = this.grid.width - 1;\n  this.grid.each(function(point, value) {\n    characters.push(characterFromElement(value));\n    if (point.x == endOfLine)\n      characters.push(\"\\n\");\n  });\n  return characters.join(\"\");\n};\n",
      "// bind and method\nfunction bind(func, object) {\n  return function(){\n    return func.apply(object, arguments);\n  };\n}\n\nvar x = [];\nvar pushX = bind(x.push, x);\npushX(\"A\");\npushX(\"B\");\nprint(x);\n\nfunction method(object, name) {\n  return function() {\n    object[name].apply(object, arguments);\n  };\n}\nvar pushX = method(x, \"push\");\n",
      "// Terrarium.prototype.step\nTerrarium.prototype.listActingCreatures = function() {\n  var found = [];\n  this.grid.each(function(point, value) {\n    if (value != undefined && value.act)\n      found.push({object: value, point: point});\n  });\n  return found;\n};\n\nTerrarium.prototype.listSurroundings = function(center) {\n  var result = {};\n  var grid = this.grid;\n  directions.each(function(name, direction) {\n    var place = center.add(direction);\n    if (grid.isInside(place))\n      result[name] = characterFromElement(grid.valueAt(place));\n    else\n      result[name] = \"#\";\n  });\n  return result;\n};\n\nTerrarium.prototype.processCreature = function(creature) {\n  var action = creature.object.act(this.listSurroundings(creature.point));\n\n  if (action.type == \"move\" && directions.contains(action.direction)) {\n    var to = creature.point.add(directions.lookup(action.direction));\n    if (this.grid.isInside(to) && this.grid.valueAt(to) == undefined)\n      this.grid.moveValue(creature.point, to);\n  }\n  else {\n    throw new Error(\"Unsupported action: \" + action.type);\n  }\n};\n\nTerrarium.prototype.step = function() {\n  forEach(this.listActingCreatures(), bind(this.processCreature, this));\n};\n",
      "// boring demo\n\nvar terrarium = new Terrarium(thePlan);\nvar anim1 = animateTerrarium(terrarium);\n\n// stop the animation with clearInterval(anim1)\n",
      "// Point.prototype.toString\nPoint.prototype.toString = function() {\n  return \"(\" + this.x + \",\" + this.y + \")\";\n};\n",
      "// creatureTypes\nvar creatureTypes = new Dictionary();\ncreatureTypes.register = function(constructor, character) {\n  constructor.prototype.character = character;\n  this.store(character, constructor);\n};\n\nfunction elementFromCharacter(character) {\n  if (character == \" \")\n    return undefined;\n  else if (character == \"#\")\n    return wall;\n  else if (creatureTypes.contains(character))\n    return new (creatureTypes.lookup(character))();\n  else\n    throw new Error(\"Unknown character: \" + character);\n}\n",
      "// BouncingBug\nfunction BouncingBug() {\n  this.direction = \"ne\";\n}\nBouncingBug.prototype.act = function(surroundings) {\n  if (surroundings[this.direction] != \" \")\n    this.direction = (this.direction == \"ne\" ? \"sw\" : \"ne\");\n  return {type: \"move\", direction: this.direction};\n};\ncreatureTypes.register(BouncingBug, \"%\");\n",
      "// Dictionary.prototype.names\nDictionary.prototype.names = function() {\n  var names = [];\n  this.each(function(name, value) {names.push(name);});\n  return names;\n};\n\nprint(directions.names());\n",
      "// randomElement\nfunction randomInteger(below) {\n  return Math.floor(Math.random() * below);\n}\n\nfunction randomElement(array) {\n  if (array.length == 0)\n    throw new Error(\"The array is empty.\");\n  return array[Math.floor(Math.random() * array.length)];\n}\n\nprint(randomElement([\"heads\", \"tails\"]));\n",
      "// DrunkBug\nfunction DrunkBug() {};\nDrunkBug.prototype.act = function(surroundings) {\n  return {type: \"move\", direction: randomElement(directions.names())};\n};\ncreatureTypes.register(DrunkBug, \"~\");\n",
      "// better demo\nvar anim2 = animateTerrarium(new Terrarium(\n  [\"############################\",\n   \"#      #    #             ##\",\n   \"#     ~            ~       #\",\n   \"#        % #####         % #\",\n   \"##         #   #    ##     #\",\n   \"###           ##     #     #\",\n   \"#           ###      #     #\",\n   \"#   ####                   #\",\n   \"#   ##                     #\",\n   \"#    #     %         ~ ### #\",\n   \"#    #                     #\",\n   \"############################\"]));\n\n// stop with clearInterval(anim2)",
      "// clone\nfunction clone(object) {\n  function OneShotConstructor(){}\n  OneShotConstructor.prototype = object;\n  return new OneShotConstructor();\n}\n",
      "// LifeLikeTerrarium\nfunction LifeLikeTerrarium(plan) {\n  Terrarium.call(this, plan);\n}\nLifeLikeTerrarium.prototype = clone(Terrarium.prototype);\nLifeLikeTerrarium.prototype.constructor = LifeLikeTerrarium;\n\nLifeLikeTerrarium.prototype.processCreature = function(creature) {\n  var energy, action, self = this;\n  function dir() {\n    if (!directions.contains(action.direction)) return null;\n    var target = creature.point.add(directions.lookup(action.direction));\n    if (!self.grid.isInside(target)) return null;\n    return target;\n  }\n  \n  action = creature.object.act(this.listSurroundings(creature.point));\n\n  if (action.type == \"move\")\n    energy = this.creatureMove(creature.object, creature.point, dir());\n  else if (action.type == \"eat\")\n    energy = this.creatureEat(creature.object, dir());\n  else if (action.type == \"photosynthesize\")\n    energy = -1;\n  else if (action.type == \"reproduce\")\n    energy = this.creatureReproduce(creature.object, dir());\n  else if (action.type == \"wait\")\n    energy = 0.2;\n  else\n    throw new Error(\"Unsupported action: \" + action.type);\n\n  creature.object.energy -= energy;\n  if (creature.object.energy <= 0)\n    this.grid.setValueAt(creature.point, undefined);\n};\n\nLifeLikeTerrarium.prototype.creatureMove = function(creature, from, to) {\n  if (to != null || this.grid.valueAt(to) == undefined) {\n    this.grid.moveValue(from, to);\n    from.x = to.x; from.y = to.y;\n  }\n  return 1;\n};\n\nLifeLikeTerrarium.prototype.creatureEat = function(creature, source) {\n  var energy = 1;\n  if (source != null) {\n    var meal = this.grid.valueAt(source);\n    if (meal != undefined && meal.energy) {\n      this.grid.setValueAt(source, undefined);\n      energy -= meal.energy;\n    }\n  }\n  return energy;\n};\n\nLifeLikeTerrarium.prototype.creatureReproduce = function(creature, target) {\n  var energy = 1;\n  if (target != null && this.grid.valueAt(target) == undefined) {\n    var species = characterFromElement(creature);\n    var baby = elementFromCharacter(species);\n    energy = baby.energy * 2;\n    if (creature.energy >= energy)\n      this.grid.setValueAt(target, baby);\n  }\n  return energy;\n};\n",
      "// Lichen\nfunction findDirections(surroundings, wanted) {\n  var found = [];\n  directions.each(function(name) {\n    if (surroundings[name] == wanted)\n      found.push(name);\n  });\n  return found;\n}\n\nfunction Lichen() {\n  this.energy = 5;\n}\nLichen.prototype.act = function(surroundings) {\n  var emptySpace = findDirections(surroundings, \" \");\n  if (this.energy >= 13 && emptySpace.length > 0)\n    return {type: \"reproduce\", direction: randomElement(emptySpace)};\n  else if (this.energy < 20)\n    return {type: \"photosynthesize\"};\n  else\n    return {type: \"wait\"};\n};\ncreatureTypes.register(Lichen, \"*\");\n",
      "// LichenEater\nfunction LichenEater() {\n  this.energy = 10;\n}\nLichenEater.prototype.act = function(surroundings) {\n  var emptySpace = findDirections(surroundings, \" \");\n  var lichen = findDirections(surroundings, \"*\");\n\n  if (this.energy >= 30 && emptySpace.length > 0)\n    return {type: \"reproduce\", direction: randomElement(emptySpace)};\n  else if (lichen.length > 0)\n    return {type: \"eat\", direction: randomElement(lichen)};\n  else if (emptySpace.length > 0)\n    return {type: \"move\", direction: randomElement(emptySpace)};\n  else\n    return {type: \"wait\"};\n};\ncreatureTypes.register(LichenEater, \"c\");\n",
      "// awesome demo\nvar moodyCave =\n  [\"############################\",\n   \"#                     ######\",\n   \"#    ***                **##\",\n   \"#   *##**         **  c  *##\",\n   \"#    ***     c    ##**    *#\",\n   \"#       c         ##***   *#\",\n   \"#                 ##**    *#\",\n   \"#   c       #*            *#\",\n   \"#*          #**       c   *#\",\n   \"#***        ##**    c    **#\",\n   \"#*****     ###***       *###\",\n   \"############################\"];\n\nvar anim3 = animateTerrarium(new LifeLikeTerrarium(moodyCave));\n\n// stop with clearInterval(anim3)\n",
      "// CleverLichenEater\nfunction CleverLichenEater() {\n  this.energy = 10;\n  this.direction = \"ne\";\n}\nCleverLichenEater.prototype.act = function(surroundings) {\n  var emptySpace = findDirections(surroundings, \" \");\n  var lichen = findDirections(surroundings, \"*\");\n\n  if (surroundings[this.direction] != \" \" && emptySpace.length > 0)\n    this.direction = randomElement(emptySpace);\n\n  if (this.energy >= 30 && emptySpace.length > 0)\n    return {type: \"reproduce\", direction: randomElement(emptySpace)};\n  else if (lichen.length > 1)\n    return {type: \"eat\", direction: randomElement(lichen)};\n  else if (emptySpace.length > 0)\n    return {type: \"move\", direction: this.direction};\n  else\n    return {type: \"wait\"};\n};\ncreatureTypes.register(CleverLichenEater, \"c\");\n\nvar anim4 = animateTerrarium(new LifeLikeTerrarium(moodyCave));\n\n// clearInterval(anim4), you know the drill\n",
      "// inherit\nObject.prototype.inherit = function(baseConstructor) {\n  this.prototype = clone(baseConstructor.prototype);\n  this.prototype.constructor = this;\n};\nObject.prototype.method = function(name, func) {\n  this.prototype[name] = func;\n};\n\nfunction StrangeArray(){}\nStrangeArray.inherit(Array);\nStrangeArray.method(\"push\", function(value) {\n  Array.prototype.push.call(this, value);\n  Array.prototype.push.call(this, value);\n});\n\nvar strange = new StrangeArray();\nprint(strange.push(4));\n",
      "// create\nObject.prototype.create = function() {\n  var object = clone(this);\n  if (object.construct != undefined)\n    object.construct.apply(object, arguments);\n  return object;\n};\n",
      "// extend\nObject.prototype.extend = function(properties) {\n  var result = clone(this);\n  forEachIn(properties, function(name, value) {\n    result[name] = value;\n  });\n  return result;\n};\n",
      "// text-adventure Item\nvar Item = {\n  construct: function(name) {\n    this.name = name;\n  },\n  inspect: function() {\n    print(\"it is \", this.name, \".\");\n  },\n  kick: function() {\n    print(\"klunk!\");\n  },\n  take: function() {\n    print(\"you cannot lift \", this.name, \".\");\n  }\n};\n\nvar lantern = Item.create(\"the brass lantern\");\nlantern.kick();\n\nvar DetailedItem = Item.extend({\n  construct: function(name, details) {\n    Item.construct.call(this, name);\n    this.details = details;\n  },\n  inspect: function() {\n    print(\"you see \", this.name, \", \", this.details, \".\");\n  }\n});\n\nvar giantSloth =\n  DetailedItem.create(\"the giant sloth\",\n                      \"it is quietly hanging from a tree, munching leaves\");\ngiantSloth.inspect();\n\nvar SmallItem = Item.extend({\n  kick: function() {\n    print(this.name, \" flies across the room.\");\n  },\n  take: function() {\n    // (imagine some code that moves the item to your pocket here)\n    print(\"you take \", this.name, \".\");\n  }\n});\n\nvar pencil = SmallItem.create(\"the red pencil\");\npencil.take();\n",
      "// isA\nObject.prototype.isA = function(prototype) {\n  function DummyConstructor() {}\n  DummyConstructor.prototype = prototype;\n  return this instanceof DummyConstructor;\n};\n\nprint(pencil.isA(Item));\nprint(pencil.isA(DetailedItem));\n",
      "// mixInto\nfunction mixInto(object, mixIn) {\n  forEachIn(mixIn, function(name, value) {\n    object[name] = value;\n  });\n};\n\nvar SmallDetailedItem = clone(DetailedItem);\nmixInto(SmallDetailedItem, SmallItem);\n\nvar deadMouse = SmallDetailedItem.create(\"Fred the mouse\", \"he is dead\");\ndeadMouse.inspect();\ndeadMouse.kick();\n",
      "// Monster\nvar Monster = Item.extend({\n  construct: function(name, dangerous) {\n    Item.construct.call(this, name);\n    this.dangerous = dangerous;\n  },\n  kick: function() {\n    if (this.dangerous)\n      print(this.name, \" bites your head off.\");\n    else\n      print(this.name, \" squeaks and runs away.\");\n  }\n});\n\nvar DetailedMonster = DetailedItem.extend({\n  construct: function(name, description, dangerous) {\n    DetailedItem.construct.call(this, name, description);\n    Monster.construct.call(this, name, dangerous);\n  },\n  kick: Monster.kick\n});\n\nvar giantSloth = DetailedMonster.create(\n  \"the giant sloth\",\n  \"it is quietly hanging from a tree, munching leaves\",\n  true);\ngiantSloth.kick();\n"],

  7: ["// months, version 1\nvar names = [\"January\", \"February\", \"March\", \"April\", \"May\", \"June\", \"July\",\n             \"August\", \"September\", \"October\", \"November\", \"December\"];\nfunction getMonthName(number) {return names[number];}\nfunction getMonthNumber(name) {\n  for (var number = 0; number < names.length; number++) {\n    if (names[number] == name) return number;\n  }\n}\n\nprint(getMonthNumber(\"February\"));\n",
      "// months, version 2\nfunction buildMonthNameModule() {\n  var names = [\"January\", \"February\", \"March\", \"April\", \"May\", \"June\", \"July\",\n               \"August\", \"September\", \"October\", \"November\", \"December\"];\n  function getMonthName(number) {return names[number];}\n  function getMonthNumber(name) {\n    for (var number = 0; number < names.length; number++) {\n      if (names[number] == name) return number;\n    }\n  }\n\n  window.getMonthName = getMonthName;\n  window.getMonthNumber = getMonthNumber;\n}\nbuildMonthNameModule();\n",
      "// provide\nfunction provide(values) {\n  forEachIn(values, function(name, value) {\n    window[name] = value;\n  });\n}\n",
      "// months, version 3\n(function() {\n  var names = [\"January\", \"February\", \"March\", \"April\", \"May\", \"June\", \"July\",\n               \"August\", \"September\", \"October\", \"November\", \"December\"];\n  provide({\n    getMonthName: function(number) {return names[number];},\n    getMonthNumber: function(name) {\n      for (var number = 0; number < names.length; number++) {\n        if (names[number] == name) return number;\n      }\n    }\n  });\n})();\n\nprint(getMonthName(2));\n",
      "// HTML\nvar HTML = {\n  tag: function(name, content, properties) {\n    return {name: name, properties: properties, content: content};\n  },\n  link: function(target, text) {\n    return HTML.tag(\"a\", [text], {href: target});\n  }\n  /* ... many, many more HTML-producing functions ... */\n};\n",
      "// days\nvar days = (function() {\n  var names = [\"Sunday\", \"Monday\", \"Tuesday\", \"Wednesday\",\n               \"Thursday\", \"Friday\", \"Saturday\"];\n  return {\n    getDayName: function(number) {return names[number];},\n    getDayNumber: function(name) {\n      for (var number = 0; number < names.length; number++) {\n        if (names[number] == name) return number;\n      }\n    }\n  };\n})();\n\nprint(days.getDayNumber(\"Wednesday\"));\n",
      "// positionOf, version 1\nfunction positionOf(element, array, compare, start, end) {\n  if (start == null) start = 0;\n  if (end == null) end = array.length;\n  for (; start < end; start++) {\n    var current = array[start];\n    if (compare ? compare(element, current) : element == current) return start;\n  }\n}\n\nprint(positionOf(2, [1, 2, 3, 4, 3, 2, 1], null, 3, 6));\n",
      "// positionOf, version 2\n// optional arguments in args: {compare, start, end}\nfunction positionOf(element, array, args) {\n  args = args || {};\n  var start = (args.start == null ? 0 : args.start),\n      end = (args.end == null) ? array.length : args.end,\n      compare = args.compare;\n  for (; start < end; start++) {\n    var current = array[start];\n    if (compare ? compare(element, current) : element == current) return start;\n  }\n}\n\nprint(positionOf(2, [1, 2, 3, 4, 3, 2, 1], {start: 3, end: 6}));\n"],

  8: ["// asterisk or brace\nvar asteriskOrBrace = /[\\{\\*]/;\nvar story = \"We noticed the *giant sloth*, hanging from a giant branch.\";\nprint(story.search(asteriskOrBrace));\n",
      "// digit surrounded by space\nvar digitSurroundedBySpace = /\\s\\d\\s/;\nprint(\"1a 2 3d\".search(digitSurroundedBySpace));\n",
      "// not\nvar notABC = /[^ABC]/;\nprint(\"ABCBACCBBADABC\".search(notABC));\n",
      "// date, version 1\nvar datePattern = /\\d\\d\\/\\d\\d\\/\\d\\d\\d\\d/;\nprint(\"born 15/11/2003 (mother Spot): White Fang\".search(datePattern));\n",
      "// word-break\nprint(/cat/.test(\"concatenate\"));\nprint(/\\bcat\\b/.test(\"concatenate\"));\n",
      "// repeat\nvar parenthethicText = /\\(.*\\)/;\nprint(\"Its (the sloth's) claws were gigantic!\".search(parenthethicText));\n",
      "// date, version 2\nvar datePattern = /\\d{1,2}\\/\\d\\d?\\/\\d{4}/;\nprint(\"born 15/11/2003 (mother Spot): White Fang\".search(datePattern));\n",
      "// grouping\nvar cartoonCrying = /boo(hoo+)+/i;\nprint(cartoonCrying.test(\"Boohoooohoohooo\"));\n",
      "// alternatives\nvar holyCow = /\\b(sacred|holy) (cow|bovine|bull|taurus)\\b/i;\nprint(holyCow.test(\"Sacred bovine!\"));\n",
      "// match\nprint(\"No\".match(/yes/i));\n\nprint(\"... yes\".match(/yes/i));\n\nprint(\"Giant Ape\".match(/giant (\\w+)/i));\n",
      "// date, version 3\nfunction extractDate(string) {\n  var found = string.match(/\\b(\\d\\d?)\\/(\\d\\d?)\\/(\\d{4})\\b/);\n  if (found == null)\n    throw new Error(\"No date found in '\" + string + \"'.\");\n  return new Date(Number(found[3]), Number(found[2]) - 1, Number(found[1]));\n}\n",
      "// replace\nprint(\"Borobudur\".replace(/[ou]/g, \"a\"));\n",
      "// replace groups\nvar names = \"Picasso, Pablo\\nGauguin, Paul\\nVan Gogh, Vincent\";\nprint(names.replace(/([\\w ]+), ([\\w ]+)/g, \"$2 $1\"));\n",
      "// replace function\nprint(\"the cia and fbi\".replace(/\\bfbi|cia\\b/g, function(str) {\n  return str.toUpperCase();\n}));\n",
      "// replace function 2\nvar stock = \"1 lemon, 2 cabbages, and 101 eggs\";\nfunction minusOne(match, amount, unit) {\n  amount = Number(amount) - 1;\n  if (amount == 1) // only one left, remove the 's'\n    unit = unit.slice(0, unit.length - 1);\n  else if (amount == 0)\n    amount = \"no\";\n  return amount + \" \" + unit;\n}\nprint(stock.replace(/(\\d+) (\\w+)/g, minusOne));\n",
      "// escapeHTML\nfunction escapeHTML(text) {\n  var replacements = {\"<\": \"&lt;\", \">\": \"&gt;\",\n                      \"&\": \"&amp;\", \"\\\"\": \"&quot;\"};\n  return text.replace(/[<>&\"]/g, function(character) {\n    return replacements[character];\n  });\n}\n",
      "// wordfilter\nvar badWords = [\"ape\", \"monkey\", \"simian\", \"gorilla\", \"evolution\"];\nvar pattern = new RegExp(badWords.join(\"|\"), \"i\");\nfunction isAcceptable(text) {\n  return !pattern.test(text);\n}\n\nprint(isAcceptable(\"The quick brown fox...\"));\nprint(isAcceptable(\"Cut that monkeybusiness out.\"));\nprint(isAcceptable(\"Mmmm, grapes.\"));\n",
      "// parseINI\nfunction parseINI(string) {\n  var lines = splitLines(string);\n  var categories = [];\n\n  function newCategory(name) {\n    var cat = {name: name, fields: []};\n    categories.push(cat);\n    return cat;\n  }\n  var currentCategory = newCategory(\"TOP\");\n\n  forEach(lines, function(line) {\n    var match;\n    if (/^\\s*(;.*)?$/.test(line))\n      return;\n    else if (match = line.match(/^\\[(.*)\\]$/))\n      currentCategory = newCategory(match[1]);\n    else if (match = line.match(/^(\\w+)=(.*)$/))\n      currentCategory.fields.push({name: match[1], value: match[2]});\n    else\n      throw new Error(\"Line '\" + line + \"' is invalid.\");\n  });\n\n  return categories;\n}\n"],

  9: ["// encodeURIComponent\nvar encoded = encodeURIComponent(\"aztec empire\");\nprint(encoded);\nprint(decodeURIComponent(encoded));\n",
      "// navigator object\nprint(navigator.userAgent);\nprint(navigator.vendor);\nprint(navigator.platform);\n"],

 10: ["// load HTML\n// this sets the body of the frame below to a\n// document similar to the one shown in the book\ndocument.body.innerHTML =\n  \"<h1>Chapter 1: Equipment</h1><p>This is what an <em>alchemists' bottle</em> looks like:</p><img src=\\\"img/florence_flask.png\\\" alt=\\\"a fat bottle\\\" id=\\\"picture\\\">\";",
      "// isTextNode\nfunction isTextNode(node) {\n  return node.nodeType == 3;\n}\n\nprint(isTextNode(document.body));\nprint(isTextNode(document.body.firstChild.firstChild));\n",
      "// nodeName, nodeValue\nprint(document.body.firstChild.nodeName);\nprint(document.body.firstChild.firstChild.nodeValue);\n",
      "// isImage\nfunction isImage(node) {\n  return !isTextNode(node) && node.nodeName == \"IMG\";\n}\n\nprint(isImage(document.body.lastChild));\n",
      "// picture.src\nvar picture = document.getElementById(\"picture\");\nprint(picture.src);\npicture.src = \"img/ostrich.png\";\n",
      "// add header\nvar secondHeader = document.createElement(\"H1\");\nvar secondTitle = document.createTextNode(\"Chapter 2: Deep magic\");\nsecondHeader.appendChild(secondTitle);\ndocument.body.appendChild(secondHeader);\n",
      "// add image\nvar newImage = document.createElement(\"IMG\");\nnewImage.setAttribute(\"src\", \"img/yinyang.png\");\ndocument.body.appendChild(newImage);\nprint(newImage.getAttribute(\"src\"));\n",
      "// dom function\nfunction dom(name, attributes) {\n  var node = document.createElement(name);\n  if (attributes) {\n    forEachIn(attributes, function(name, value) {\n      node.setAttribute(name, value);\n    });\n  }\n  for (var i = 2; i < arguments.length; i++) {\n    var child = arguments[i];\n    if (typeof child == \"string\")\n      child = document.createTextNode(child);\n    node.appendChild(child);\n  }\n  return node;\n}\ndocument.body.appendChild(\n  dom(\"P\", null, \"A paragraph with a \",\n      dom(\"A\", {href: \"http://en.wikipedia.org/wiki/Alchemy\"}, \"link\"),\n      \" inside of it.\"));\n",
      "// removeNode, insertBefore\nfunction removeNode(node) {\n  node.parentNode.removeChild(node);\n}\nfunction insertBefore(newNode, node) {\n  node.parentNode.insertBefore(newNode, node);\n}\n",
      "var output = dom(\"DIV\", {id: \"printOutput\"}, dom(\"H1\", null, \"Print output:\"));\ndocument.body.appendChild(output);\n\nfunction print2() {\n  var result = [];\n  forEach(arguments, function(arg){result.push(String(arg));});\n  output.appendChild(dom(\"PRE\", null, result.join(\"\")));    \n}\n\n// we use a different name here, don't want to clobber the default\n// print function\n",
      "picture.style.borderWidth = \"4px\";\n",
      "picture.style.display = \"none\";\n// picture gone\npicture.style.display = \"\";\n// picture visible again\n",
      "picture.style.position = \"absolute\";\nvar angle = 0;\nvar spin = setInterval(function() {\n  angle += 0.1;\n  picture.style.left = (100 + 100 * Math.cos(angle)) + \"px\";\n  picture.style.top = (100 + 100 * Math.sin(angle)) + \"px\";\n}, 100);\n\n// stop with clearInterval(spin)\n",
      "picture.style.width = \"400px\";\npicture.style.height = \"200px\";\n"],

 11: ["// registerEventHandler\nfunction registerEventHandler(node, event, handler) {\n  if (typeof node.addEventListener == \"function\")\n    node.addEventListener(event, handler, false);\n  else\n    node.attachEvent(\"on\" + event, handler);\n}\n\nfunction unregisterEventHandler(node, event, handler) {\n  if (typeof node.removeEventListener == \"function\")\n    node.removeEventListener(event, handler, false);\n  else\n    node.detachEvent(\"on\" + event, handler);\n}\n\n// note that you have to click on the bottom right of the screen\n// to hit the frame that this script runs in\nregisterEventHandler(document.body, \"click\", function(event) {\n  event = event || window.event;\n  print(event.clientX, \",\", event.clientY);\n});\n",
      "// key event\nregisterEventHandler(document.body, \"keypress\", function(event) {\n  event = event || window.event;\n  var charCode = event.charCode || event.keyCode;\n  if (charCode)\n    print(\"Character '\", String.fromCharCode(charCode), \"' was typed.\");\n});\n",
      "// normalizeEvent\nfunction normalizeEvent(event) {\n  if (!event.stopPropagation) {\n    event.stopPropagation = function() {this.cancelBubble = true;};\n    event.preventDefault = function() {this.returnValue = false;};\n  }\n  if (!event.stop)\n    event.stop = function() {\n      this.stopPropagation();\n      this.preventDefault();\n    };\n\n  if (event.srcElement && !event.target)\n    event.target = event.srcElement;\n  if ((event.toElement || event.fromElement) && !event.relatedTarget)\n    event.relatedTarget = event.toElement || event.fromElement;\n  if (event.clientX != undefined && event.pageX == undefined) {\n    event.pageX = event.clientX + document.body.scrollLeft;\n    event.pageY = event.clientY + document.body.scrollTop;\n  }\n  if (event.type == \"keypress\")\n    event.character = String.fromCharCode(event.charCode || event.keyCode);\n  return event;\n}\n",
      "// addHandler\nfunction addHandler(node, type, handler) {\n  function wrapHandler(event) {\n    handler(normalizeEvent(event || window.event));\n  }\n  registerEventHandler(node, type, wrapHandler);\n  return {node: node, type: type, handler: wrapHandler};\n}\n\nfunction removeHandler(object) {\n  unregisterEventHandler(object.node, object.type, object.handler);\n}\n",
      "// blockQ\nvar textfield = document.body.appendChild(document.createElement(\"TEXTAREA\"));\nvar blockQ = addHandler(textfield, \"keypress\", function(event) {\n  if (event.character.toLowerCase() == \"q\")\n    event.stop();\n});\n",
      "// focusedstyle\naddHandler(textfield, \"focus\", function(event) {\n  event.target.style.backgroundColor = \"yellow\";\n});\naddHandler(textfield, \"blur\", function(event) {\n  event.target.style.backgroundColor = \"\";\n});\n",
      "// change event\naddHandler(textfield, \"change\", function(event) {\n  print(\"Content of text field changed to '\", event.target.value, \"'.\");\n});\n",
      "// Square type\nfunction Square(character, img) {\n  this.img = img;\n  var content = {\"@\": \"player\", \"#\": \"wall\", \"*\": \"exit\",\n                 \" \": \"empty\", \"0\": \"boulder\"}[character];\n  if (content == null)\n    throw new Error(\"Unrecognized character: '\" + character + \"'\");\n  this.setContent(content);\n}\n\nSquare.prototype.setContent = function(content) {\n  this.content = content;\n  this.img.src = \"img/sokoban/\" + content + \".png\";\n}\n",
      "// SokobanField type\nfunction SokobanField(level) {\n  this.fieldDiv = dom(\"DIV\");\n  this.squares = [];\n  this.bouldersToGo = level.boulders;\n\n  for (var y = 0; y < level.field.length; y++) {\n    var line = level.field[y], squareRow = [];\n    for (var x = 0; x < line.length; x++) {\n      var img = dom(\"IMG\");\n      this.fieldDiv.appendChild(img);\n      squareRow.push(new Square(line.charAt(x), img));\n      if (line.charAt(x) == \"@\")\n        this.playerPos = new Point(x, y);\n    }\n    this.fieldDiv.appendChild(dom(\"BR\"));\n    this.squares.push(squareRow);\n  }\n}\n\nSokobanField.prototype.status = function() {\n  return this.bouldersToGo + \" boulder\" +\n    (this.bouldersToGo == 1 ? \"\" : \"s\") + \" to go.\";\n};\nSokobanField.prototype.won = function() {\n  return this.bouldersToGo <= 0;\n};\n\nSokobanField.prototype.place = function(where) {\n  where.appendChild(this.fieldDiv);\n};\nSokobanField.prototype.remove = function() {\n  this.fieldDiv.parentNode.removeChild(this.fieldDiv);\n};\n\nSokobanField.prototype.move = function(direction) {\n  var playerSquare = this.squares[this.playerPos.y][this.playerPos.x],\n      targetPos = this.playerPos.add(direction),\n      targetSquare = this.squares[targetPos.y][targetPos.x];\n\n  // First, see if the player can push a boulder...\n  if (targetSquare.content == \"boulder\") {\n    var pushPos = targetPos.add(direction),\n        pushSquare = this.squares[pushPos.y][pushPos.x];\n    if (pushSquare.content == \"empty\") {\n      targetSquare.setContent(\"empty\");\n      pushSquare.setContent(\"boulder\");\n    }\n    else if (pushSquare.content == \"exit\") {\n      targetSquare.setContent(\"empty\");\n      this.bouldersToGo--;\n    }\n  }\n  // Then, try to move...\n  if (targetSquare.content == \"empty\") {\n    playerSquare.setContent(\"empty\");\n    targetSquare.setContent(\"player\");\n    this.playerPos = targetPos;\n  }\n};\n",
      "// SokobanGame type\nfunction SokobanGame(levels, place) {\n  this.levels = levels;\n  var newGame = dom(\"BUTTON\", null, \"New game\");\n  addHandler(newGame, \"click\", method(this, \"newGame\"));\n  var reset = dom(\"BUTTON\", null, \"Reset level\");\n  addHandler(reset, \"click\", method(this, \"resetLevel\"));\n  this.status = dom(\"DIV\");\n  this.container = dom(\"DIV\", null, dom(\"H1\", null, \"Sokoban\"),\n                       dom(\"DIV\", null, newGame, \" \", reset), this.status);\n  place.appendChild(this.container);\n  addHandler(document, \"keydown\", method(this, \"keyDown\"));\n  this.newGame();\n}\n\nSokobanGame.prototype.newGame = function() {\n  this.level = 0;\n  this.resetLevel();\n};\nSokobanGame.prototype.resetLevel = function() {\n  if (this.field)\n    this.field.remove();\n  this.field = new SokobanField(this.levels[this.level]);\n  this.field.place(this.container);\n  this.updateStatus();\n};\nSokobanGame.prototype.updateStatus = function() {\n  this.status.innerHTML = \"Level \" + (1 + this.level) + \": \" +\n    this.field.status();\n};\n\nvar arrowKeyCodes = {\n  37: new Point(-1, 0), // left\n  38: new Point(0, -1), // up\n  39: new Point(1, 0),  // right\n  40: new Point(0, 1)   // down\n};\n\nSokobanGame.prototype.keyDown = function(event) {\n  if (arrowKeyCodes.hasOwnProperty(event.keyCode)) {\n    event.stop();\n    this.field.move(arrowKeyCodes[event.keyCode]);\n    this.updateStatus();\n    if (this.field.won()) {\n      if (this.level < this.levels.length - 1) {\n        alert(\"Excellent! Going to the next level.\");\n        this.level++;\n        this.resetLevel();\n      }\n      else {\n        alert(\"You win! Game over.\");\n        this.newGame();\n      }\n    }\n  }\n};\n",
      "// demo\ndocument.body.innerHTML = \"\";\nvar game = new SokobanGame(SOKOBANLEVELS, document.body);\n\n// click the frame in the bottom-left of the page to play\n"],

 12: ["// requestObject\nfunction requestObject() {\n  if (window.XMLHttpRequest)\n    return new XMLHttpRequest();\n  else if (window.ActiveXObject)\n    return new ActiveXObject(\"Msxml2.XMLHTTP\");\n  else\n    throw new Error(\"Could not create HTTP request object.\");\n}\n",
      "// simple request\nvar request = requestObject();\nrequest.open(\"GET\", \"files/data.txt\", false);\nrequest.send(null);\n\nprint(request.responseText);\nprint(request.getResponseHeader(\"Content-Type\"));\nprint(request.status);\nprint(request.statusText);\n",
      "// asynchronous request\nrequest.open(\"GET\", \"files/data.txt\", true);\nrequest.onreadystatechange = function() {\n  if (request.readyState == 4)\n    print(request.status + \" \" + request.statusText);\n};\nrequest.send(null);\n",
      "// xml request\nrequest.open(\"GET\", \"files/fruit2.xml\", false);\nrequest.send(null);\nprint(request.responseXML.documentElement.childNodes.length);\n",
      "// json request\nrequest.open(\"GET\", \"files/fruit2.json\", true);\nrequest.onreadystatechange = function() {\n  if (request.readyState == 4) {\n    var data = eval(\"(\" + request.responseText + \")\");\n    print(data[\"lemon\"]);\n  }\n};\nrequest.send(null);\n",
      "// wrapper\nfunction simpleHttpRequest(url, success, failure) {\n  var request = requestObject();\n  request.open(\"GET\", url, true);\n  request.onreadystatechange = function() {\n    if (request.readyState == 4) {\n      if (request.status == 200 || !failure)\n        success(request.responseText);\n      else if (failure)\n        failure(request.status, request.statusText);\n    }\n  };\n  request.send(null);\n}\n"]
};

function initCodeList() {
  var div = $("code");
  forEach(keys(code), function(ch) {
    var expand = SPAN({style: "cursor: pointer; font-size: 80%;"}, "\u25b6"),
        runAll = SPAN({style: "cursor: pointer; font-size: 70%; font-weight: normal;"}, "[run all]");
    div.appendChild(H1(null, "Chapter " + ch + " ", runAll, " ", expand));
    var block = div.appendChild(DIV({"class": "chapterblock", style: "display: none;"}));
    connect(expand, "onclick", function() {
      if (expand.firstChild.nodeValue == "\u25b6") { // expand
        expand.innerHTML = "\u25bc";
        block.style.display = "";
      }
      else {
        expand.innerHTML = "\u25b6";
        block.style.display = "none";
      }
    });
    connect(runAll, "onclick", function() {
      forEach(code[ch], function(frag) {_console.runCode(frag);});
    });
    forEach(code[ch], function(frag) {
      var name = frag.match(/^(?:\/\/ )?(.*)/)[1]
      var tag = name + " [chapter " + ch + "]";
      var node = DIV(null, name);
      connect(node, "onclick", function(){
        node.style.color = "#666";
        if (!_console.showCode(tag))
          _console.loadCode(tag, frag);
      });
      block.appendChild(node);
    });
  });
}
