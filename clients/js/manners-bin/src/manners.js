'use strict';

var path = require('path');
var BinWrapper = require('bin-wrapper');

var VERSION = '0.5.0.1';
var BASE = 'https://github.com/mannersio/manners/releases/download/v' + VERSION + '/';

var filePathForPlatform = function (platform) {
  var p = { darwin: '-osx', linux: '-linux' }[platform];
  return 'manners-' + VERSION + '-x86_64' + p;
};

module.exports = new BinWrapper()
  .src(BASE + filePathForPlatform('darwin') + '.tar.gz', 'darwin')
  .src(BASE + filePathForPlatform('linux') + '.tar.gz', 'linux', 'x64')
  .dest(path.join('vendor'))
  .use(filePathForPlatform(process.platform));
