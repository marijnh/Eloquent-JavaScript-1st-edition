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

function growEditor(node) {
  var cm = node.CodeMirror, parent = node.parentNode;
  setElementDimensions(cm.getWrapperElement(), {w: parent.clientWidth, h: parent.clientHeight});
  cm.refresh();
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

var ie = document.selection && window.ActiveXObject && /MSIE \d+/.test(navigator.userAgent);

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

function Buffer(name, content, where){
  if (useJSEditor){
    var self = this;
    this.editor = CodeMirror(function(node){
      self.node = node;
      where.appendChild(node);
    }, {value: content,
        matchBrackets: true,
        lineWrapping: true
       });
  }
  else {
    this.node = TEXTAREA({spellcheck: false}, content);
    where.appendChild(this.node);
  }
  growEditor(this.node);
  this.name = name;
}

Buffer.prototype = {
  show: function() {
    showElement(this.node);
    if (useJSEditor) this.editor.refresh();
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
    var keyObj = event.key(), key = keyObj.string;
    var shift = event.modifier().shift;
    if (key == "KEY_ENTER" || keyObj.code == 10)
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
  this.runCode = function(code){runCode(code, false);};
  this.evalCode = function(code){runCode(code, "show");};
  this.printCode = function(code){runCode(code, "print");};
}

connect(window, "onload", function() {
  if (/^\[object/.test((new Error("...")).toString()))
    Error.prototype.toString = function(){return this.name + ": " + this.message;};
});

//- book.js

// This code assumes a 1px border around the console, repl, output,
// and editor elements.
var _console = null;

function hasClass(node, cls) {
  return (new RegExp("\\b" + cls + "\\b")).test(node.className);
}

var processPage = function(){
  function hideSolutions() {
    forEach(getElementsByTagAndClassName("div", "solution"), function(solution) {
      solution.style.display = "none";

      var showToggle = DIV({"class": "toggle"}, "[show solution]");
      connect(showToggle, "onclick", function() {
	showToggle.style.display = "none";
	solution.style.display = "";
      });
      solution.parentNode.insertBefore(showToggle, solution);

      var hideToggle = DIV({"class": "solutionarrow", title: "Hide the solution."}, "\u00D7");
      connect(hideToggle, "onclick", function() {
	showToggle.style.display = "";
	solution.style.display = "none";
      });
      solution.insertBefore(hideToggle, solution.firstChild);
    });
  }

  function positionFloater(element, pos) {
    var minWidth = 600;
    var winWidth = getViewportDimensions().w;
    pos.x = Math.min(pos.x, winWidth - minWidth);
    setElementDimensions(element, {w: winWidth - pos.x - 35});
    setElementPosition(element, pos);
  }

  function moveFootnotes() {
    var notelist = getFirstElementByTagAndClassName("ol", "footnotes");
    if (!notelist)
      return;

    function moveNote(note, ref) {
      var floater = DIV({"class": "floater footnotefloat"}, note.childNodes);
      floater.style.display = "none";
      document.body.appendChild(floater);
      var newRef = SPAN({"class": "footref"}, ref.firstChild);
      ref.parentNode.replaceChild(newRef, ref);
      connect(newRef, "onmouseover", function(event) {
        positionFloater(floater, addPoint(event.mouse().page, {x: 5, y: 10}));
        floater.style.display = "block";
      });
      connect(newRef, "onmouseout", function() {
        floater.style.display = "none";
      });
    }

    var notes = notelist.childNodes;
    var refs = getElementsByTagAndClassName("a", "footref");
    for (var i = 0; i < notes.length; i++)
      moveNote(notes[i], refs[i]);
    removeElement(notelist);
  }

  function getCode(code){
    function flattenNode(node){
      if (node.nodeType == 3)
        return node.nodeValue;
      else if (node.nodeName == "SPAN")
        return node.firstChild.nodeValue;
      else
        return "";
    }
    return map(flattenNode, code.childNodes).join("");
  }

  function runCodeUpto(code) {
    var fragments = [];
    while (code) {
      removeElementClass(code, "not-run");
      fragments.push(getCode(code));
      code = code.prev;
    }
    for (var i = fragments.length - 1; i >= 0; i--)
      _console.runCode(fragments[i]);
  }

  function addCodeButtons(){
    var prev = null;
    forEach(getElementsByTagAndClassName("pre", "code"), function(code) {
      addElementClass(code, "not-run");
      var expr = hasClass(code, "expression");
      var runUpto = !(expr || hasClass(code, "invalid"));
      if (runUpto) {
        code.prev = prev;
        prev = code;
      }

      if (!expr) {
          var load = code.insertBefore(BUTTON({"class": "codebutton load", "type": "button", "title": "Load this code into the console"}), code.firstChild);
        connect(load, "onclick", function(){
          setOpen(true);
          _console.loadCode("example", getCode(code));
        });
      }

      var run = code.insertBefore(BUTTON({"class": "codebutton run", "title": "Run this code", "type": "button"}), code.firstChild);
      connect(run, "onclick", function(event){
        removeElementClass(code, "not-run");
        setOpen(true);
        if (runUpto && event.modifier().shift)
          runCodeUpto(code);
        else
          _console[expr ? "printCode" : "runCode"](getCode(code));
      });
    });
  }

  var popup = null;
  function react() {
    if (popup) {
      closeReaction();
      return;
    }

    var name = INPUT({value: getCookie("name", ""), type: "text"}),
        email = INPUT({value: getCookie("email", ""), type: "text"}),
        subject = INPUT({value: "", type: "text"}),
        message = TEXTAREA(null, "");
    popup = DIV({"class": "reactpopup"},
                P(null, "Send me a message..."),
                P(null, SPAN(null, "Your name:"), name),
                P(null, SPAN(null, "Your e-mail:"), email),
                P(null, SPAN(null, "Subject:"), subject),
                P(null, message),
                P(null,
                  BUTTON({type: "button"}, "Send", attach("onclick", sendReaction)), " ",
                  BUTTON({type: "button"}, "Cancel", attach("onclick", closeReaction))));
    if (fixedConsole) popup.style.position = "fixed";
    document.body.appendChild(popup);
    name.focus();

    function sendByXHR() {
      var data = queryString({name: name.value, email: email.value, message: message.value,
                              subject: subject.value, chapter: chapterTag});
      request = doXHR("/contact", {method: "POST", sendContent: data,
                                  headers: {"Content-type": "application/x-www-form-urlencoded; charset=utf-8",
                                            "Content-length": data.length,
                                            "Connection": "close"}});
      function fail(reason) {
        alert("Could not deliver your message. (" + reason + ")");
      }
      request.addErrback(function(xhr){
        fail((err.xhr && err.xhr.statusText) || err.message);
      });
      request.addCallback(function(xhr) {
        if (xhr.responseText != "ok")
          fail(xhr.responseText);
      });
    }
    function sendByIFrame() {
      var frame = createDOM("IFRAME", {style: "border-width: 0; position: absolute; width: 1px; height: 1px; top: 0px;",
                                       src: "js/sendreaction.html"});
      connect(frame, "onload", function() {
        var win = frame.contentWindow;
        var form = win.document.getElementById("form");
        form.elements.name.value = name.value;
        form.elements.email.value = email.value;
        form.elements.chapter.value = chapterTag;
        form.elements.message.value = message.value;
        form.elements.subject.value = subject.value;
        form.submit();
        setTimeout(partial(removeElement, frame), 10000);
      });
      document.body.appendChild(frame);
    }

    function sendReaction() {
      if (message.value == "") {
        alert("You did not enter a message.");
        return;
      }

      if (!/\b[\w\.-]+@[\w\.-]+\.\w{2,4}\b/.test(email.value) && !confirm("If you do not enter a valid e-mail address, I will probably ignore you. Send anyway?"))
        return;

      setCookie("name", name.value);
      setCookie("email", email.value);

      if (/eloquentjavascript\.net/.test(document.domain))
        sendByXHR();
      else
        sendByIFrame();
      closeReaction();
    }
    function closeReaction() {
      removeElement(popup);
      popup = null
    }
  }

  function addReactButton() {
    var b = BUTTON({type: "button", "class": "react",
                    title: "Send me a message"},
                    attach("onclick", react));
    document.body.appendChild(b);
    if (fixedConsole) b.style.position = "fixed";
  }

  var fixedConsole = !ie;
  var fakeFixed = false;
  var open = true;
  var contentRatio = Number(getCookie("contentRatio", .75));
  var topBar = 13;
  var sizeCorrection = dimMode() == "standard" ? -2 : 0;
  var minContentWidth = 700;

  function setContentRatio(consoleHeight) {
    var winHeight = getViewportDimensions().h;
    contentRatio = Math.min(.96, Math.max(.02, (winHeight - consoleHeight) / winHeight));
    setCookie("contentRatio", String(contentRatio));
  }

  function alignConsole() {
    if (fakeFixed) {
      $("console").style.bottom = "-" + (iphone ? window.pageYOffset : document.body.scrollTop) + "px";
    }
    else {
      $("console").style.bottom = "0px";
    }
  }

  function resizeFrames() {
    var winSize = getViewportDimensions();
    if (open) {
      var consoleSize = Math.max(minSize, Math.round(winSize.h * (1.0 - contentRatio)));
      if (fixedConsole)
        $("consoleCompensation").style.height = consoleSize + "px";
      else
        setElementDimensions($("content"), {h: Math.max(0, winSize.h - 1 - consoleSize)});
      var width = fixedConsole ? document.body.clientWidth : winSize.w + sizeCorrection;
      if (fixedConsole && fakeFixed) width += sizeCorrection;
        setElementDimensions($("console"), {h: consoleSize + sizeCorrection, w: width});
      resizeConsole();
    }
    else {
      if (fixedConsole) {
        $("consoleCompensation").style.height = "0px";
        $("console").style.height = topBar + "px";
      }
      else {
        setElementDimensions($("content"), {h: winSize.h - topBar + sizeCorrection});
      }
      var width = fixedConsole ? document.body.clientWidth : winSize.w + sizeCorrection;
      if (fixedConsole && fakeFixed) width += sizeCorrection;
      setElementDimensions($("console"), {h: topBar, w: width});
    }
    if (fixedConsole) alignConsole();
  }

  function addConsole() {
    if (fixedConsole) {
      document.body.appendChild(DIV({"class": "frame console" + (open ? " open" : ""), "id": "console"}));
      document.getElementsByClassName("content")[0].id = "content";
      $("console").style.position = fakeFixed ? "absolute" : "fixed";
      alignConsole();
      document.body.appendChild(DIV({id: "consoleCompensation"}));
    }
    else {
      document.body.appendChild(DIV({"class": "frame", "id": "content"}, document.body.childNodes[1]));
      document.body.appendChild(DIV({"class": "frame console" + (open ? " open" : ""), "id": "console"}));
      document.body.style.overflow = "hidden"; 
    }
    initConsole($("console"));
    setOpen(false);
    connect(window, "onresize", resizeFrames);

    if (!fixedConsole) {
      window.onscroll = function() {
        if (document.body.scrollTop > 0) {
	  $("content").scrollTop += document.body.scrollTop;
          document.body.scrollTop = 0;
	  $("content").focus();
        }
      };
    }
    else if (fakeFixed) {
      var timeout = null;
      window.onscroll = function() {
        $("console").style.display = "none";
        clearTimeout(timeout);
        timeout = setTimeout(function() {
          $("console").style.display = "";
          alignConsole();
        }, 100);
      };
    }
  }

  var minSize = 120;

  function resizeConsole() {
    var margin = 6;
    var leftRatio = .4;
    var width = fixedConsole ? document.body.clientWidth : $("console").clientWidth;
    var height = $("console").clientHeight;

    var bottomHeight = Math.max($("repl").offsetHeight, $("controls").offsetHeight);
    var topHeight = height - topBar - 2 * margin - bottomHeight;

    var innerWidth = width - 3 * margin;
    var leftWidth = Math.round(leftRatio * innerWidth);
    var rightWidth = innerWidth - leftWidth;

    var output = $("output"), editor = $("editor");

    placeElement(output, {x: margin, y: topBar, w: leftWidth + sizeCorrection, h: topHeight + sizeCorrection});
    placeElement($("repl"), {x: margin, y: topBar + margin + topHeight, w: leftWidth + sizeCorrection});
    setElementDimensions($("repl").firstChild, {w: $("repl").clientWidth});

    placeElement(editor, {x: 2 * margin + leftWidth, y: topBar, w: rightWidth + sizeCorrection, h: topHeight + sizeCorrection});
    placeElement($("controls"), {x: 2 * margin + leftWidth, y: topBar + margin + topHeight, w: rightWidth});

    forEach(editor.childNodes, growEditor);
    setElementDimensions($("outputinner"), {w: output.clientWidth, h: output.clientHeight - output.firstChild.offsetHeight});
  }

  function setOpen(nowOpen){
    if (open == nowOpen)
      return;
    open = nowOpen;
    $("editor").style.display = $("repl").style.display = $("output").style.display =
	 $("controls").style.display = $("resize").style.display = (open ? "" : "none");
    resizeFrames();
    if (open)
      addElementClass($("console"), "open");
    else
      removeElementClass($("console"), "open");
  }

  function dragResize(event) {
    var size = $("console").offsetHeight, startSize = size;
    var startY = event.mouse().page.y;

    document.body.style.cursor = "n-resize";

    var marker = DIV({"class": "resizemarker"});
    marker.style.bottom = size + "px";
    $("console").appendChild(marker);

    var tracker = connect(document.body, "onmousemove", function(event) {
      size = Math.max(minSize, startSize - (event.mouse().page.y - startY));
      marker.style.bottom = size + "px";
    });
    var finish = connect(document.body, "onmouseup", function(event) {
      disconnect(tracker);
      disconnect(finish);
      document.body.style.cursor = "";
      setContentRatio(size);
      resizeFrames();
      removeElement(marker);
    });
  }

  function initConsole(where) {
    function toggle(event){
      setOpen(!open);
    }

    var showHide = BUTTON({"class": "showhide", "type": "button", "title": "Open or close the console"},
      attach("onclick", toggle));
    var resize = BUTTON({"class": "resize", "type": "button", "title": "Resize the console", "id": "resize"},
      attach("onmousedown", dragResize));

    var output = DIV({"class": "output", "id": "output"}),
        controls = DIV({"id": "controls"}),
        editor = DIV({"class": "editor", "id": "editor"}),
        repl = DIV({"class": "editor", "id": "repl"}),
	header = DIV({"class": "header"}, SPAN(null, "CONSOLE", attach("onclick", toggle)), resize, showHide);
    replaceChildNodes(where, header, output, controls, editor, repl);

    function initFrame(env) {
      if (window.chapterTag)
        env.load("chapter/" + chapterTag + ".js");
    }
    _console = new Console({output: output, controls: controls, editor: editor, repl: repl,
                            initEnv: initFrame});
  }

  function restoreBookmark(){
    if (/#/.test(location.href))
      location.href = location.href;
  }

  setTimeout(function(){document.body.style.visibility = "";}, 0);
  if (/Safari\//.test(navigator.userAgent) && /Mobile\//.test(navigator.userAgent)) {
      // Doesn't work on mobile safari
  } else if (/Version\/2/.test(navigator.userAgent) && /Safari\//.test(navigator.userAgent)) {
    if (!getCookie("safariwarning", false)) {
      setCookie("safariwarning", "1");
      alert("Safari 2 unfortunately does not support the JavaScript used by this book. Extra functionality will be disabled. Upgrade to version 3 (still beta), or use Firefox to read the book with full functionality.");
    }
  } else if (window.opera && Number(window.opera.version()) < 9.52) {
    if (!getCookie("operawarning", false)) {
      setCookie("operawarning", "1");
      alert("Your version of Opera is not supported by this site. The 'active' components of this book will be disabled. Use version 9.52+, Firefox, or a recent Safari if you want full functionality.");
    }
  } else {
    hideSolutions();
    moveFootnotes();
    addCodeButtons();
//    addReactButton();
    addConsole();
    restoreBookmark();
  }
};

connect(window, "onload", processPage);
