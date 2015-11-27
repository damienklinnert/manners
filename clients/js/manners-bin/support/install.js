'use strict';

var mannersBin = require('../src/manners.js');

mannersBin.run(['--version'], function (err) {
  if (err) {
    return console.error(err);
  }

  console.log('manners installed');
});
