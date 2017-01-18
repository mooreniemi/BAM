'use strict';

var expect = require('chai').expect;

describe('Kth Element in Linear Time', function() {
  it('should exist', function() {
    var KthElement = require('./kth-element.js');
    expect(KthElement).to.not.be.undefined;
  });
  it('provides a working O(n log n) implementation using sort', function() {
    var KthElement = require('./kth-element.js');
    expect(KthElement.cheat([2,4,1,0,9], 1)).to.equal(0);
  });
});
