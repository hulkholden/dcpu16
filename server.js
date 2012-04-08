var http = require('http');
var fs = require('fs');
var path = require('path');

http.createServer(function (request, response) {
  console.log(request.url);

  serveFile(request, response);

}).listen(8124);

console.log('Server running at http://127.0.0.1:8124/');

function serveFile(request, response) {

  var filePath = '.' + request.url;
    
  var extname = path.extname(filePath);
  var contentType;
  switch (extname) {
    case '.html':
      contentType = 'text/html';
      break;
    case '.js':
      contentType = 'application/javascript';
      break;
    case '.css':
      contentType = 'text/css';
      break;
    case '.png':
      contentType = 'image/png';
      break;      
  }

  // Only serve filetype we know about
  if(!contentType) {
      response.writeHead(404);
      response.end();    
  }
  
  path.exists(filePath, function(exists) {
  
    if (exists) {
      fs.readFile(filePath, function(error, content) {
        if (error) {
          response.writeHead(500);
          response.end();
        }
        else {
          response.writeHead(200, { 'Content-Type': contentType });
          response.end(content, 'utf-8');
        }
      });
    }
    else {
      response.writeHead(404);
      response.end();
    }
  });
  
}
