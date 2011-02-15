function test() {
  var tests = [["FunctionalTools", "map(partial(op[\"*\"], 5), [3, 6, 15])"],
               ["ObjectTools", "clone({x: 10, y: 5}).x"],
               ["Dictionary", "(new Dictionary({left: true})).contains(\"right\")"]];
  for (var i = 0; i < tests.length; i++) {
    var test = tests[i];
    print("Testing " , test[0], ": ", test[1]);
    show(eval(test[1]));
  }
}
