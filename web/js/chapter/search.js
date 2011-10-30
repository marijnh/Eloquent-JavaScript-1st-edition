load("FunctionalTools.js");

var heightAt = function(){
  var heights = [[111,111,122,137,226,192,246,275,285,333,328,264,202,175,151,222,250,222,219,146],
                 [205,186,160,218,217,233,268,300,316,357,276,240,240,253,215,201,256,312,224,200],
                 [228,176,232,258,246,289,306,351,374,388,319,333,299,307,261,286,291,355,277,258],
                 [228,207,263,264,284,348,368,358,391,387,320,344,366,382,372,394,360,314,259,207],
                 [238,237,275,315,353,355,341,332,350,315,283,310,355,350,336,405,361,273,264,228],
                 [245,264,289,340,359,349,336,303,267,259,285,340,315,290,333,372,306,254,220,220],
                 [264,287,331,365,382,381,386,360,299,258,254,284,264,276,295,323,281,233,202,160],
                 [300,327,360,355,365,402,393,343,307,274,232,226,221,262,289,250,252,228,160,160],
                 [343,379,373,337,309,336,378,352,303,290,294,241,176,204,235,205,203,206,169,132],
                 [348,348,364,369,337,276,321,390,347,354,309,259,208,147,158,165,169,169,200,147],
                 [320,328,334,348,354,316,254,315,303,297,283,238,229,207,156,129,128,161,174,165],
                 [297,331,304,283,283,279,250,243,264,251,226,204,155,144,154,147,120,111,129,138],
                 [302,347,332,326,314,286,223,205,202,178,160,172,171,132,118,116,114, 96, 80, 75],
                 [287,317,310,293,284,235,217,305,286,229,211,234,227,243,188,160,152,129,138,101],
                 [260,277,269,243,236,255,343,312,280,220,252,280,298,288,252,210,176,163,133,112],
                 [266,255,254,254,265,307,350,311,267,276,292,355,305,250,223,200,197,193,166,158],
                 [306,312,328,279,287,320,377,359,289,328,367,355,271,250,198,163,139,155,153,190],
                 [367,357,339,330,290,323,363,374,330,331,415,446,385,308,241,190,145, 99, 88,145],
                 [342,362,381,359,353,353,369,391,384,372,408,448,382,358,256,178,143,125, 85,109],
                 [311,337,358,376,330,341,342,374,411,408,421,382,271,311,246,166,132,116,108, 72]];
  return function(point) {
    return heights[point.y][point.x];
  };
}();

function BinaryHeap(score){
  this.content = [];
  this.score = score;
}

BinaryHeap.prototype = {
  push: function(element) {
    this.content.push(element);
    this.cascadeDown(this.content.length - 1);
  },
  pop: function() {
    var result = this.content[0];
    var end = this.content.pop();
    if (this.content.length > 0) {
      this.content[0] = end;
      this.cascadeUp(0);
    }
    return result;
  },
  remove: function(node) {
    var len = this.content.length;
    for (var i = 0; i < len; i++) {
      if (this.content[i] == node) {
        var end = this.content.pop();
        if (this.content.length > 0) {
          this.content[i] = end;
          this.cascadeUp(i);
        }
        return;
      }
    }
//    throw new Error("Node not found.");
  },
  size: function() {
    return this.content.length;
  },
  cascadeDown: function(n) {
    var element = this.content[n];
    while (n > 0) {
      var parentN = ((n + 1) >> 1) - 1;
      var parent = this.content[parentN];
      if (this.score(element) < this.score(parent)) {
        this.content[parentN] = element;
        this.content[n] = parent;
        n = parentN;
      }
      else {
        break;
      }
    }
  },
  cascadeUp: function(n) {
    var element = this.content[n], elemScore = this.score(element);
    var length = this.content.length;
    while(true) {
      var child2N = (n + 1) << 1, child1N = child2N - 1;
      var swap = n;
      if (child1N < length) {
        var child1 = this.content[child1N], child1Score = this.score(child1);
        if (child1Score < elemScore)
          swap = child1N;
      }
      if (child2N < length) {
        var child2 = this.content[child2N], child2Score = this.score(child2);
        if (child2Score < (swap == n ? elemScore : child1Score))
          swap = child2N;
      }

      if (swap != n) {
        this.content[n] = this.content[swap];
        this.content[swap] = element;
        n = swap;
      }
      else {
        break;
      }
    }
  }
};

function showRoute() {
  var routes = arguments;
  parent.withDocument(document, function() {
    var img = parent.DIV({"class": "outputimage"}, parent.IMG({src: "img/height.png"}));
    __ENV.output(img);
    forEach(routes, function(route) {
      while(route) {
        var marker = parent.DIV({"class": "marker"});
        img.appendChild(marker);
        parent.centerElement(marker, {x: route.point.x * 20 + 12, y: route.point.y * 20 + 12});
        route = route.from;
      }
    });
  });
}
