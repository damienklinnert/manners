'use strict';

const spawn = require('child_process').spawn;
const util = require('util');
const http = require('http');
const axios = require('axios');
const mannersBin = require('manners-bin');

class ProviderError extends Error {
  constructor(message) {
    super(message);
    this.name = this.constructor.name;
    this.message = message || '';

    Error.call(this);
    Error.captureStackTrace(this, ProviderError);
  }
}
exports.ProviderError = ProviderError;

class InteractionGroup {
  constructor(cfg, interactions) {
    this._cfg = cfg;
    this._interactions = interactions || [];
  }

  add(interaction) {
    return new InteractionGroup(this._cfg, this._interactions.concat(interaction));
  }

  setup(readyFn) {
    const cfg = this._cfg;
    const axiosCfg = {
      timeout: 500,
      headers: {'X-Pact-Mock-Service': 'True'}
    };
    const rethrow = (fn) => {
      return (err) => {
        if (err instanceof ProviderError) {
          throw err;
        }
        fn(err);
      };
    };
    const cleanup = () => {
      return axios.delete(cfg.baseUrl + '/interactions', axiosCfg);
    };

    return axios
      .put(cfg.baseUrl + '/interactions', {interactions: this._interactions}, axiosCfg)
      .catch((err) => {
        throw new ProviderError('Could not setup interactions:' + err);
      })
      .then(() => {
        return readyFn();
      })
      .catch(rethrow((err) => {
        const msg = 'Promise returned from readyFn did not resolve: ' + err;
        throw new ProviderError(msg);
      }))
      .then(() => {
        return axios.get(cfg.baseUrl + '/interactions/verification', axiosCfg);
      })
      .catch(rethrow((err) => {
        const msg = 'Verification failed with error: ' + JSON.stringify(err.data.error, null, 2);
        throw new ProviderError(msg);
      }))
      .then(
        () => {
          return cleanup();
        }, (err) => {
          return cleanup().then(() => {
            throw err;
          });
        }
      )
      .catch(rethrow((err) => {
        throw new ProviderError('Cleaning up interactions failed: ' + err);
      }))
      .then(() => {
        const contract = {
          consumer: {name: cfg.consumer},
          provider: {name: cfg.provider}
        };
        return axios.post(cfg.baseUrl + '/pact', contract, axiosCfg);
      })
      .catch(rethrow((err) => {
        throw new ProviderError('Could not write contract: ' + err);
      }));
  }
}
exports.InteractionGroup = InteractionGroup;

class StateHelperService {
  constructor(states) {
    this._stateMap = states.reduce((mem, cur) => {
      mem[cur.state] = cur;
      return mem;
    }, {});
  }

  start(port) {
    return new Promise((resolve, reject) => {
      http
        .createServer(this._handleRequest)
        .listen(port, (err) => {
          if (err) {
            return reject(err);
          }
          resolve();
        });
    });
  }

  _handleRequest(req, res) {
    if (req.method !== 'POST' && req.url !== '/state') {
      res.statusCode = 500;
      return res.end();
    }

    let data = '';
    req.on('data', (d) => {
      data += d.toString();
    });
    req.on('end', () => {
      const payload = JSON.parse(data);
      const stateCfg = this._stateMap[payload.state];

      if (!stateCfg) {
        res.statusCode = 500;
        res.end(JSON.stringify({
          error: util.format('could not find state "%s" in config', payload.state)
        }));
        return;
      }

      const fnName = (payload.type === 'setup') ? 'setupFn' : 'teardownFn';
      const stateFn = stateCfg[fnName] || () => {
        };
      const promise = stateFn() || Promise.resolve();
      promise.then(() => {
        res.statusCode = 200;
        res.end();
      }, () => {
        res.statusCode = 500;
        res.end();
      });
    });
  }
}

class StateGroup {
  constructor(cfg, states) {
    this._cfg = cfg;
    this._states = states;
  }

  whenDefault(setupFn, teardownFn) {
    const states = this._states.concat({
      state: '__MANNERS_DEFAULT_STATE__',
      setupFn: setupFn,
      teardownFn: teardownFn
    });
    return new StateGroup(this._cfg, states);
  }

  when(state, setupFn, teardownFn) {
    const states = this._states.concat({state: state, setupFn: setupFn, teardownFn: teardownFn});
    return new StateGroup(this._cfg, states);
  }

  run() {
    const service = new StateHelperService(this._states);

    const spawnClient = () => {
      return new Promise((resolve, reject) => {
        const process = spawn(
          mannersBin,
          ['fake-consumer', this._cfg.contractPath, this._cfg.providerUrl],
          {stdio: 'inherit'});

        process.on('exit', (statusCode) => {
          if (statusCode !== 0) {
            return reject(new Error('Failed to run fake-consumer'));
          }
          resolve();
        });
        process.on('error', reject);
      });
    };

    return service.start(2345)
      .then(spawnClient)
      .then(() => {
        console.log('SUCCESS');
        process.exit();
      })
      .catch((err) => {
        console.log('FAILURE', err);
        process.exit(1);
      });
  }
}
exports.StateGroup = StateGroup;

class FakeProvider {
  constructor(opts) {
    this._process = null;
    this._opts = opts;
  }

  start(done) {
    return new Promise((resolve, reject) => {
        const manners = spawn(mannersBin, ['fake-provider']);
        const checkMannersServerIsListening = (data) => {
          if (data.toString().indexOf('listen') !== -1) {
            resolve();
          }
        };

        manners.stdout.on('data', checkMannersServerIsListening);

        if (this._opts.debug || process.env.MANNERS_DEBUG) {
          console.log('Debug mode enabled. Outputting Manners server to console.');

          manners.stdout.on('data', (data) => {
            console.log(data.toString());
          });
        }

        this._process = manners;
      }
    );
  }

  stop() {
    return new Promise((resolve, reject) => {
      this._process.on('exit', resolve);
      this._process.kill();
    });
  }

  interactions() {
    return new InteractionGroup({
      baseUrl: 'http://localhost:1234',
      consumer: this._opts.consumer,
      provider: this._opts.provider
    });
  }
}
exports.FakeProvider = FakeProvider;

class FakeConsumer {
  constructor(contractPath, providerUrl) {
    this._cfg = {
      contractPath: contractPath,
      providerUrl: providerUrl
    };
  }

  states() {
    return new StateGroup(this._cfg, []);
  }
}
exports.FakeConsumer = FakeConsumer;