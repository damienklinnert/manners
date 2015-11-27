'use strict';

var spawn = require('child_process').spawn;
var mannersBin = require('manners-bin');

var FakeProvider = module.exports.FakeProvider = function () {
  this._process = null;
};

FakeProvider.prototype.start = function (done) {
  this._process = spawn(mannersBin, ['fake-provider'], { cwd: __dirname });
  this._process.stdout.on('data', function (d) {
    if (d.toString().indexOf('listen') !== -1) {
      done();
    }
  });
};

FakeProvider.prototype.stop = function (done) {
  this._process.on('exit', function () {
    done();
  });
  this._process.kill();
};