'use strict';

var expect = require('chai').expect;
var jsc = require("jsverify");
var KthElement = require('./kth-element.js');

describe('Kth Element in Linear Time', function() {
  it('should exist', function() {
    expect(KthElement).to.not.be.undefined;
  });
  it('provides a working O(n log n) implementation using sort', function() {
    expect(KthElement.cheat([2,4,1,0,9], 1)).to.equal(0);
  });
  it('provides a working O(n) implementation without sort', function() {
    expect(KthElement.find([2,4,1,0,9], 1)).to.equal(0);
  });
  describe("find has cheat's correctness", function () {
    jsc.property("idempotent", "array nat", function (arr) {
      return KthElement.find(arr, 2) === KthElement.cheat(arr, 2);
    });
  });
});
