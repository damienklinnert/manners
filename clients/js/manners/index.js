'use strict';

var spawn = require('child_process').spawn;
var util = require('util');
var http = require('http');
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
  var axiosCfg = {
    timeout: 500,
    headers: { 'X-Pact-Mock-Service': 'True' }
  };
  var rethrow = function (fn) {
    return function (err) {
      if (err instanceof ProviderError) { throw err; }
      fn(err);
    }
  };
  var cleanup = function () {
    return axios.delete(cfg.baseUrl + '/interactions', axiosCfg);
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
    .then(
      function () {
        return cleanup();
      }, function (err) {
        return cleanup().then(function () {
          throw err;
        });
      }
    )
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
      throw new ProviderError('Could not write contract:\n' + err);
    }));
};

var FakeConsumer = module.exports.FakeConsumer = function (contractPath, providerUrl) {
  this._cfg = {
    contractPath: contractPath,
    providerUrl: providerUrl
  };
};

FakeConsumer.prototype.states = function () {
  return new StateGroup(this._cfg, []);
};

var StateGroup = module.exports.StateGroup = function (cfg, states) {
  this._cfg = cfg;
  this._states = states;
}

StateGroup.prototype.whenDefault = function (setupFn, teardownFn) {
  var states = this._states.concat({ state: '__MANNERS_DEFAULT_STATE__', setupFn: setupFn, teardownFn: teardownFn });
  return new StateGroup(this._cfg, states);
};

StateGroup.prototype.when = function (state, setupFn, teardownFn) {
  var states = this._states.concat({ state: state, setupFn: setupFn, teardownFn: teardownFn });
  return new StateGroup(this._cfg, states);
};

StateGroup.prototype.run = function () {
  var service = new StateHelperService(this._states);

  var spawnClient = function () {
    return new Promise(function (resolve, reject) {
      var process = spawn(mannersBin, ['fake-consumer', this._cfg.contractPath, this._cfg.providerUrl], { stdio: 'inherit' });
      process.on('exit', function (statusCode) {
        if (statusCode !== 0) { return reject(new Error('Failed to run fake-consumer')); }
        resolve();
      });
      process.on('error', reject);
    }.bind(this));
  }.bind(this);

  return service.start(2345)
    .then(spawnClient)
    .then(function () {
      console.log('SUCCESS');
      process.exit();
    })
    .catch(function (err) {
      console.log('FAILURE', err);
      process.exit(1);
    });
};

var StateHelperService = function (states) {
  this._stateMap = states.reduce(function (mem, cur) {
    mem[cur.state] = cur;
    return mem;
  }, {});
};

StateHelperService.prototype.start = function (port) {
  return new Promise(function (resolve, reject) {
    http
      .createServer(this._handleRequest.bind(this))
      .listen(port, function (err) {
        if (err) { return reject(err); }
        resolve();
      });
  }.bind(this));
};

StateHelperService.prototype._handleRequest = function (req, res) {
  if (req.method !== 'POST' && req.url !== '/state') {
    res.statusCode = 500;
    return res.end();
  }

  var data = '';
  req.on('data', function (d) { data += d.toString(); });
  req.on('end', function () {
    var payload = JSON.parse(data);
    var stateCfg = this._stateMap[payload.state];

    if (!stateCfg) {
      res.statusCode = 500;
      res.end(JSON.stringify({ error: util.format('could not find state "%s" in config', payload.state)}));
      return;
    }

    var fnName = (payload.type === 'setup') ? 'setupFn' : 'teardownFn';
    var stateFn = stateCfg[fnName] || function () {};
    var promise = stateFn() || Promise.resolve();
    promise.then(function () {
      res.statusCode = 200;
      res.end();
    }, function () {
      res.statusCode = 500;
      res.end();
    });
  }.bind(this));
};
