<html xmlns="http://www.w3.org/1999/xhtml" lang="en-US" xml:lang="en-US">
  <head>
    <title>OpenWordnet-PT</title>
    <meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1">
    <link rel="stylesheet" type="text/css" href="static/common.css" />
  </head>

  <body>
    <div id="header">
      <h2>Welcome to the openWordnet-PT</h2>
    </div>

    <div id="search-box">
      <form method="get" action="search">
	<strong>Lookup Word (or Synset)</strong>: 
	<input type="text" name="term" value="<clp_value name="term" query/>" size="15" maxlength="50">
	<input type="submit" name="query" value="Search Wordnet">
      </form>
    </div>

    <div id="result">
      <display_result />
    </div>
    
    <div id="footer">
      <p><a href="http://github.com/arademaker/openWordnet-PT/">More detail about the project</a>,
	including the full data for download.</p>
    
      <p>Maintainer: Alexandre Rademaker
      &lt;<a href="mailto:arademaker@gmail.com">arademaker@gmail.com</a>&gt;</p>
    </div>
  </body>
</html>
