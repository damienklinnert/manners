'use strict';

var spawn = require('child_process').spawn;
var mannersBin = require('manners-bin');

var FakeProvider = module.exports.FakeProvider = function () {
  this._process = null;
};

FakeProvider.prototype.start = function (done) {
  this._process = spawn(mannersBin, ['fake-provider']);
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

var FakeConsumer = module.exports.FakeConsumer = function (contractPath, providerUrl) {
  this._contractPath = contractPath;
  this._providerUrl = providerUrl;
};

FakeConsumer.prototype.run = function (done) {
  var process = spawn(mannersBin, ['fake-consumer', this._contractPath, this._providerUrl], { stdio: 'inherit' });
  process.on('exit', done);
};