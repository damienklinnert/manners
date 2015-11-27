# manners-bin

Binary wrapper for manners.

## Installation

    $ npm install manners-bin --save-dev

## CLI

    $ node_modules/.bin/manners --help

## API

    var spawn = require('child_process').spawn;
    var mannersBin = require('manners-bin');

    spawn(mannersBin, ['--help'], { stdio: 'inherit' })
      .on('exit', process.exit);
