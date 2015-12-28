'use strict';

var spawn = require('child_process').spawn;
var axios = require('axios');
var mannersBin = require('manners-bin');

var ProviderError = module.exports.ProviderError = function (message) {
  Error.call(this);
  Error.captureStackTrace(this, ProviderError);
  this.name = 'ProviderError';
  this.message = message || '';
};

ProviderError.prototype = Object.create(Error.prototype);
ProviderError.prototype.constructor = ProviderError;

var FakeProvider = module.exports.FakeProvider = function (opts) {
  this._process = null;
  this._opts = opts;
};

FakeProvider.prototype.start = function (done) {
  return new Promise(function (resolve, reject) {
    this._process = spawn(mannersBin, ['fake-provider']);
    this._process.stdout.on('data', function (d) {
      if (d.toString().indexOf('listen') !== -1) {
        resolve();
      }
    });
  }.bind(this));
};

FakeProvider.prototype.stop = function () {
  return new Promise(function (resolve, reject) {
    this._process.on('exit', resolve);
    this._process.kill();
  }.bind(this));
};

var FakeConsumer = module.exports.FakeConsumer = function (contractPath, providerUrl) {
  this._contractPath = contractPath;
  this._providerUrl = providerUrl;
};

FakeConsumer.prototype.run = function () {
  return new Promise(function (resolve, reject) {
    var process = spawn(mannersBin, ['fake-consumer', this._contractPath, this._providerUrl], { stdio: 'inherit' });
    process.on('exit', resolve);
  }.bind(this));
};

FakeProvider.prototype.interactions = function () {
  return new InteractionGroup({
    baseUrl: 'http://localhost:1234',
    consumer: this._opts.consumer,
    provider: this._opts.provider
  });
};

var InteractionGroup = module.exports.InteractionGroup = function (cfg, interactions) {
  this._cfg = cfg;
  this._interactions = interactions || [];
};

InteractionGroup.prototype.add = function (interaction) {
  return new InteractionGroup(this._cfg, this._interactions.concat(interaction));
};

InteractionGroup.prototype.setup = function (readyFn) {
  var cfg = this._cfg;
  var axiosCfg = { headers: { 'X-Pact-Mock-Service': 'True' } };
  var rethrow = function (fn) {
    return function (err) {
      if (err instanceof ProviderError) { throw err; }
      fn(err);
    }
  };

  return axios
      .put(cfg.baseUrl + '/interactions', { interactions: this._interactions }, axiosCfg)
      .catch(function (err) {
        throw new ProviderError('Could not setup interactions');
      })
      .then(function () {
        return readyFn();
      })
      .catch(rethrow(function (err) {
        var msg = 'Promise returned from readyFn did not resolve:\n' + err;
        throw new ProviderError(msg);
      }))
      .then(function () {
        return axios.get(cfg.baseUrl + '/interactions/verification', axiosCfg);
      })
      .catch(rethrow(function (err) {
        var msg = 'Verification failed with error:\n' + JSON.stringify(err.data.error, null, 2);
        throw new ProviderError(msg);
      }))
      .then(function () {
        return axios.delete(cfg.baseUrl + '/interactions', axiosCfg);
      })
      .catch(rethrow(function (err) {
        throw new ProviderError('Cleaning up interactions failed');
      }))
      .then(function () {
        var contract = {
          consumer: { name: cfg.consumer },
          provider: { name: cfg.provider }
        };
        return axios.post(cfg.baseUrl + '/pact', contract, axiosCfg);
      })
      .catch(rethrow(function (err) {
        throw new ProviderError('Could not write contract');
      }));
};