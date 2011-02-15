function escapeHTML(text) {
  var replacements = {"<": "&lt;", ">": "&gt;", "&": "&amp;", "\"": "&quot;"};
  return text.replace(/[<>&\"]/g, function(character) {
    return replacements[character];
  });
}

function makeTable(data, columns) {
  var thead = dom("THEAD", null, dom("TR")), tbody = dom("TBODY");
  forEach(columns, function(name) {
    var cell = dom("TD", null, name);
    thead.firstChild.appendChild(cell);
    cell.onclick = partial(reSort, name);
  });
  forEach(data, function(element) {
    element._row = dom("TR");
    tbody.appendChild(element._row);
    forEach(columns, function(name) {
      element._row.appendChild(dom("TD", null, element[name]));
    });
  });

  function compare(a, b) {
    if (a < b) return -1;
    else if (b > a) return 1;
    else return 0;
  }

  function reSort(name) {
    data.sort(function(a, b) {return compare(a[name], b[name]);});
    forEach(data, function(element) {
      tbody.appendChild(element._row);
    });
  }

  return dom("TABLE", null, thead, tbody);
}

function test() {
  document.body.appendChild(makeTable([{foo: 1, bar: "flub"}, {foo: 8, bar: "aap"}, {foo: 0, bar: "zruty"}], ["foo", "bar"]));
}
