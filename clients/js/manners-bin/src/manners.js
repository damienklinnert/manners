'use strict';

var path = require('path');
var BinWrapper = require('bin-wrapper');

var BASE = 'https://github.com/mannersio/manners/releases/download/v0.1.0.0/';

var filePathForPlatform = function (platform) {
  var p = { darwin: '-osx', linux: '-linux' }[platform];
  return 'manners-0.1.0.0-x86_64' + p;
};

module.exports = new BinWrapper()
  .src(BASE + filePathForPlatform('darwin'), 'darwin')
  .src(BASE + filePathForPlatform('linux'), 'linux', 'x64')
  .dest(path.join('vendor'))
  .use(filePathForPlatform(process.platform));
