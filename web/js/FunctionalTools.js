var Break = {name: "Break"};

function forEach(array, action) {
  var len = array.length;
  try {
    for (var i = 0; i < len; i++)
      action(array[i]);
  }
  catch(e) {
    if (e != Break)
      throw e;
  }
}

function forEachIn(object, action) {
  try {
    for (var property in object) {
      if (Object.prototype.hasOwnProperty.call(object, property))
        action(property, object[property]);
    }
  }
  catch(e) {
    if (e != Break)
      throw e;
  }
}
  
function map(func, array) {
  var len = array.length;
  var result = new Array(len);
  for (var i = 0; i < len; i++)
    result[i] = func(array[i]);
  return result;
}

function reduce(func, start, array) {
  var len = array.length;
  for (var i = 0; i < len; i++)
    start = func(start, array[i]);
  return start;
}

function filter(test, array) {
  var result = [], len = array.length;
  for (var i = 0; i < len; i++) {
    var current = array[i];
    if (test(current))
      result.push(current);
  }
  return result;
}

function any(test, array) {
  for (var i = 0; i < array.length; i++) {
    var found = test(array[i]);
    if (found)
      return found;
  }
  return false;
}

function partial(func) {
  function asArray(quasiArray, start) {
    var result = [], len = quasiArray.length;
    for (var i = (start || 0); i < len; i++)
      result.push(quasiArray[i]);
    return result;
  }
  var fixedArgs = asArray(arguments, 1);
  return function(){
    return func.apply(null, fixedArgs.concat(asArray(arguments)));
  };
}

function bind(func, object) {
  return function(){
    return func.apply(object, arguments);
  };
}

function method(object, name) {
  return function() {
    object[name].apply(object, arguments);
  };
}

function compose(func1, func2) {
  return function() {
    return func1(func2.apply(null, arguments));
  };
}

var op = function(){
  var result = {
    "-": function(a, b) {
      if (arguments.length < 2)
        return -a;
      else
        return a - b;
    },
    "!": function(a) {return !a;},
    "typeof": function(a) {return typeof a;},
    "?": function(a, b, c) {return a ? b : c;}
  };
  var ops = ["+", "*", "/", "%", "&&", "||", "==", "!=", "===",
             "!==", "<", ">", ">=", "<=", "in", "instanceof"];
  forEach(ops, function(op){
    result[op] = eval("[function(a, b){return a " + op + " b;}][0]");
  });
  return result;
}();
