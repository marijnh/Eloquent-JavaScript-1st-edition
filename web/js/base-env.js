var ARCHIVE =
    ["Nephew,\n\nI bought a computer as soon as I received your letter. It took me two days to make it do 'internet', but I just kept calling the nice man at the computer shop, and in the end he came down to help personally. Send me something back if you receive this, so I know whether it actually works.\n\nLove,\nAunt Emily",
    "Dear Nephew,\n\nVery good! I feel quite proud about being so technologically minded, having a computer and all. I bet Mrs. Goor down the street wouldn't even know how to plug it in, that witch.\n\nAnyway, thanks for sending me that game, it was great fun. After three days, I beat it. My friend Mrs. Johnson was quite worried when I didn't come outside or answer the phone for three days, but I explained to her that I was working with my computer.\n\nMy cat had two kittens yesterday! I didn't even realize the thing was pregnant. I've listed the names at the bottom of my letter, so that you will know how to greet them the next time you come over.\n\nSincerely,\nAunt Emily\n\nborn 15/02/1999 (mother Spot): Clementine, Fireball",
    "[... and so on ...]\n\nborn 21/09/2000 (mother Spot): Yellow Emperor, Black Leclère",
    "...\n\nborn 02/04/2001 (mother Clementine): Bugeye, Wolverine, Miss Bushtail",
    "...\n\ndied 12/12/2002: Clementine\n\ndied 15/12/2002: Wolverine",
    "...\n\nborn 15/11/2003 (mother Spot): White Fang",
    "...\n\nborn 10/04/2003 (mother Miss Bushtail): Yellow Bess",
    "...\n\ndied 30/05/2004: Yellow Emperor",
    "...\n\nborn 01/06/2004 (mother Miss Bushtail): Catharina, Fat Igor",
    "...\n\nborn 20/09/2004 (mother Yellow Bess): Doctor Hobbles the 2nd, Noog",
    "...\n\nborn 15/01/2005 (mother Yellow Bess): The Moose, Liger\n\ndied 17/01/2005: Liger",
    "Dear nephew,\n\nYour mother told me you have taken up skydiving. Is this true? You watch yourself, young man! Remember what happened to my husband? And that was only from the second floor!\n\nAnyway, things are very exciting here. I have spent all week trying to get the attention of Mr. Drake, the nice gentleman who moved in next\ndoor, but I think he is afraid of cats. Or allergic to them? I am\ngoing to try putting Fat Igor on his shoulder next time I see him, very curious what will happen.\n\nAlso, the scam I told you about is going better than expected. I have already gotten back five 'payments', and only one complaint. It is starting to make me feel a bit bad though. And you are right that it is probably illegal in some way.\n\n(... etc ...)\n\nMuch love,\nAunt Emily\n\ndied 27/04/2006: Black Leclère\n\nborn 05/04/2006 (mother Lady Penelope): Red Lion, Doctor Hobbles the 3rd, Little Iroquois",
    "...\n\nborn 22/07/2006 (mother Noog): Goblin, Reginald, Little Maggie",
    "...\n\ndied 13/02/2007: Spot\n\ndied 21/02/2007: Fireball",
    "...\n\nborn 05/02/2007 (mother Noog): Long-ear Johnson",
    "...\n\nborn 03/03/2007 (mother Catharina): Asoka, Dark Empress, Rabbitface"];

var RECLUSEFILE = "% The Book of Programming\n\n%% The Two Aspects\n\nBelow the surface of the machine, the program moves. Without effort,\nit expands and contracts. In great harmony, electrons scatter and\nregroup. The forms on the monitor are but ripples on the water. The\nessence stays invisibly below.\n\nWhen the creators built the machine, they put in the processor and the\nmemory. From these arise the two aspects of the program.\n\nThe aspect of the processor is the active substance. It is called\nControl. The aspect of the memory is the passive substance. It is\ncalled Data.\n\nData is made of merely bits, yet it takes complex forms. Control\nconsists only of simple instructions, yet it performs difficult\ntasks. From the small and trivial, the large and complex arise.\n\nThe program source is Data. Control arises from it. The Control\nproceeds to create new Data. The one is born from the other, the\nother is useless without the one. This is the harmonious cycle of\nData and Control.\n\nOf themselves, Data and Control are without structure. The programmers\nof old molded their programs out of this raw substance. Over time,\nthe amorphous Data has crystallized into data types, and the chaotic\nControl was wrung into control structures and functions.\n\n%% Short Sayings\n\nWhen a student asked Fu-Tzu about the nature of the cycle of Data and\nControl, Fu-Tzu replied 'Think of a compiler, compiling itself.'\n\nA student asked, 'The programmers of old used only simple machines and\nno programming languages, yet they made beautiful programs. Why do we\nuse complicated machines and programming languages?' Fu-Tzu replied\n'The builders of old used only sticks and clay, yet they made\nbeautiful huts.'\n\nA hermit spent ten years writing a program. 'My program can compute\nthe motion of the stars on a 286-computer running MS DOS,' he proudly\nannounced. 'Nobody owns a 286-computer or uses MS DOS anymore,'\nFu-Tzu responded.\n\nFu-Tzu had written a small program that was full of global state and\ndubious shortcuts. Reading it, a student asked 'You warned us against\nthese techniques, yet I find them in your program. How can this be?'\nFu-Tzu said, 'There is no need to fetch a water hose when the house is\nnot on fire.'{This is not to be read as an encouragement of sloppy\nprogramming, but rather as a warning against neurotic adherence to\nrules of thumb.}\n\n%% Wisdom\n\nA student was complaining about digital numbers. 'When I take the root\nof two and then square it again, the result is already inaccurate!'\nOverhearing him, Fu-Tzu laughed. 'Here is a sheet of paper. Write down\nthe precise value of the square root of two for me.'\n\nFu-Tzu said, 'When you cut against the grain of the wood, much strength\nis needed. When you program against the grain of a problem, much code\nis needed.'\n\nTzu-li and Tzu-ssu were boasting about the size of their latest\nprograms. 'Two-hundred thousand lines,' said Tzu-li, 'not counting\ncomments!' Tzu-ssu responded, 'Psah, mine is almost a *million* lines\nalready.' Fu-Tzu said, 'My best program has five hundred lines.'\nHearing this, Tzu-li and Tzu-ssu were enlightened.\n\nA student had been sitting motionless behind his computer for hours,\nfrowning darkly. He was trying to write a beautiful solution to a\ndifficult problem but could not find the right approach. Fu-Tzu hit\nhim on the back of his head and shouted, '*Type something!*' The student\nstarted writing an ugly solution. After he had finished, he suddenly\nunderstood the beautiful solution.\n\n%% Progression\n\nA beginning programmer writes his programs like an ant builds her\nhill, one piece at a time, without thought for the bigger structure.\nHis programs will be like loose sand. They may stand for a while, but\ngrowing too big they fall apart{Referring to the danger of internal\ninconsistency and duplicated structure in unorganized code.}.\n\nRealizing this problem, the programmer will start to spend a lot of\ntime thinking about structure. His programs will be rigidly\nstructured, like rock sculptures. They are solid, but when they must\nchange, violence must be done to them{Referring to the fact that\nstructure tends to put restrictions on the evolution of a program.}.\n\nThe master programmer knows when to apply structure and when to leave\nthings in their simple form. His programs are like clay, solid yet\nmalleable.\n";

var animateTerrarium = function() {
  var animating = null, ie = /MSIE \d+/.test(navigator.userAgent);
  return function(t) {
    function stop() {clearTimeout(animating); animating = null;}
    if (animating) stop();
    document.body.innerHTML = "";
    var pre = document.body.appendChild(document.createElement("PRE"));
    animating = setInterval(function() {
      if (!pre.parentNode) return stop();
      t.step();
      pre.innerHTML = "";
      var text = t.toString();
      if (ie) text = text.replace(/\n/g, "\r");
      pre.appendChild(document.createTextNode(text));
    }, 500);
    return animating;
  };
}();

function forEach(array, action) {
  for (var i = 0, l = array.length; i < l; i++)
    action(array[i]);
}

function forEachIn(object, action) {
  for (var property in object)
    if (Object.prototype.hasOwnProperty.call(object, property))
      action(property, object[property]);
}

function map(func, array) {
  var l = array.length, result = new Array(l);
  for (var i = 0; i < l; i++)
    result[i] = func(array[i]);
  return result;
}

// Sokoban levels based on those in the Nethack game
// (http://www.nethack.org).

var SOKOBANLEVELS = [
  {field: ["######  ##### ",
           "#    #  #   # ",
           "# 0  #### 0 # ",
           "# 0 @    0  # ",
           "#  #######0 # ",
           "####   ### ###",
           "       #     #",
           "       #0    #",
           "       # 0   #",
           "      ## 0   #",
           "      #*0 0  #",
           "      ########"],
   boulders: 10},
  
  {field: ["###########   ",
           "#    #    #   ",
           "#  00#00 @#   ",
           "#     0   #   ",
           "#    #    #   ",
           "## #########  ",
           "#  0 #     #  ",
           "# 00 #0 0 0#  ",
           "#  0     0 #  ",
           "# 000#0  0 ###",
           "#    #  0 0 *#",
           "##############"],
   boulders: 20},
                                         
  {field: ["##########    ",
           "#@      *#    ",
           "#       ##    ",
           "####### ######",
           " #           #",
           " # 0 0 0 0 0 #",
           "######## #####",
           "#   0 0  0 0 #",
           "#   0        #",
           "##### ########",
           " #  0 0 0   # ",
           " #     0    # ",
           " # 0 0   0 ## ",
           "####### ####  ",
           "#  0     #    ",
           "#        #    ",
           "#   ######    ",
           "#####         "],
   boulders: 16},

  {field: [" ####         ",
           "## @########  ",
           "#          #  ",
           "# 0#####0# #  ",
           "#  #   # 0 #  ",
           "# 0 0    0##  ",
           "# 0  0  #  #  ",
           "# ####0 ## #  ",
           "#  0   0 # ## ",
           "# ###0#   0 ##",
           "#   #  0# 0 *#",
           "#  0      ####",
           "#####  #  #   ",
           "    #######   "],
   boulders: 12},

  {field: ["   ###         ",
           "####*#    #####",
           "#  #0##  ##   #",
           "#     #### 0  #",
           "# 00  #  #  0 #",
           "##  00#   00 ##",
           "#0  0   #0  #  ",
           "# 00 #  #  0#  ",
           "# 0 0#### 0 #  ",
           "#       #  ##  ",
           "#### 0  ####   ",
           " ### ## #      ",
           "  # 0   #      ",
           "  #@ #  #      ",
           "  #######      "],
   boulders: 18}];

function Point(x, y) {
  this.x = x;
  this.y = y;
}
Point.prototype.add = function(other) {
  return new Point(this.x + other.x, this.y + other.y);
};
Point.prototype.isEqualTo = function(other) {
  return this.x == other.x && this.y == other.y;
};

function dom(name, attributes) {
  var node = document.createElement(name);
  if (attributes) {
    forEachIn(attributes, function(name, value) {
      node.setAttribute(name, value);
    });
  }
  for (var i = 2; i < arguments.length; i++) {
    var child = arguments[i];
    if (typeof child == "string")
      child = document.createTextNode(child);
    node.appendChild(child);
  }
  return node;
}

function method(object, name) {
  return function() {
    object[name].apply(object, arguments);
  };
}
