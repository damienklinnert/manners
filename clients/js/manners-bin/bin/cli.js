#!/usr/bin/env node
'use strict';

var path = require('path');
var spawn = require('child_process').spawn;
var mannersPath = require('../index.js');

spawn(mannersPath, process.argv.slice(2), { stdio: 'inherit' })
  .on('exit', process.exit);
