# manners

Get your services behaved.

## Installation

    $ npm install manners --save-dev

## Usage

    var manners = require('manners');
    var fakeProvider = new manners.FakeProvider();

    fakeProvider.start(function () {

      // run tests ..

      fakeProvider.stop();
    });