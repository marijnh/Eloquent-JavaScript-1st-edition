load("FunctionalTools.js");

var timeWriter = "<html>\n  <head><title>The time</title></head>\n  <body>\n    <h1>The time</h1>\n    <p>The time is\n      <script type=\"text/javascript\">\n        var time = new Date();\n        document.write(time.getHours() + \":\" + time.getMinutes());\n      </script>\n    </p>\n  </body>\n</html>";
